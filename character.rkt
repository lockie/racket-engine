#lang racket/base

(require racket/function racket/math "tiled.rkt" "sprite.rkt")

(provide
 (all-defined-out))


(define (make-character sprite tiled-map)
    (define speed 200)  ;; TODO : unhardcode

    (define screen-width 0)
    (define screen-height 0)

    ;; NOTE : coords are isometric
    (define target-x 0)
    (define target-y 0)
    ;; sprite pixel offset relative to tile it's in
    (define sprite-offset-x 0)
    (define sprite-offset-y 0)

    (define (conf config)
        (set! screen-width (hash-ref config 'window-width 0))
        (set! screen-height (hash-ref config 'window-height 0)))

    (define (load renderer)
        ;; XXX these are somewhat hardcoded formulas for Flare character sprites
        (set! sprite-offset-x
            (exact-floor
             (* 1/2
                (- (sprite-get-width sprite)
                   (tiled-map-renderer-get-tile-width tiled-map)))))
        (set! sprite-offset-y
            (exact-floor
             (-
              (sprite-get-height sprite)
              (* (tiled-map-renderer-get-tile-height tiled-map) 3/2))))
        (let-values ([(x y) (get-pos)])
            (set! target-x x)
            (set! target-y y)))

    (define (switch-stance stance)
        (unless (eq? (sprite-get-stance sprite) stance)
            (sprite-set-stance sprite stance)))

    (define (sprite-with-offset-x)
        (+ (sprite-get-x sprite) sprite-offset-x))

    (define (sprite-with-offset-y)
        (+ (sprite-get-y sprite) sprite-offset-y))

    (define (sprite-pos-x)
        (- (sprite-with-offset-x)
           (tiled-map-renderer-get-tx tiled-map)))

    (define (sprite-pos-y)
        (- (sprite-with-offset-y)
           (tiled-map-renderer-get-ty tiled-map)))

    (define (move dt)
        ;; TODO : walking sound
        (switch-stance 'move)
        (let-values ([(x y)
                      (tiled-map-renderer-map-to-screen
                       tiled-map target-x target-y)])
            (define direction (atan (- (sprite-with-offset-y) y)
                                    (- (sprite-with-offset-x) x)))
            ;; TODO : turn speed
            ;; TODO : diagonal movement feels different than straight rectilinear.
            ;;  Probably it has something to do with tile width/height ratio.
            (sprite-set-angle sprite direction)
            (define distance (* speed dt))
            (sprite-set-x
             sprite
             (- (sprite-get-x sprite) (* distance (cos direction))))
            (sprite-set-y
             sprite
             (- (sprite-get-y sprite) (* distance (sin direction))))))

    (define (update dt)
        (define (approx-equal x y)
            (< (abs (- x y)) 0.5))
        (let-values ([(x y)
                      (tiled-map-renderer-map-to-screen
                       tiled-map target-x target-y)])
            (if (and (approx-equal (sprite-with-offset-x) x)
                     (approx-equal (sprite-with-offset-y) y))
                (switch-stance 'idle)
                (move dt))))

    (define (get-pos)
        (tiled-map-renderer-screen-to-map
         tiled-map
         (sprite-with-offset-x)
         (sprite-with-offset-y)))

    (define (get-target-x)
        target-x)

    (define (get-target-y)
        target-y)

    (define (set-target-x x)
        (set! target-x x))

    (define (set-target-y y)
        (set! target-y y))

    (define (center-map)
        (define x-diff (- (/ screen-width 2) (sprite-with-offset-x)))
        (define y-diff (- (/ screen-height 2) (sprite-with-offset-y)))
        (when (or (> (abs x-diff) 1) (> (abs y-diff) 1))
            (tiled-map-renderer-set-tx
             tiled-map
             (exact-floor
              (+ (tiled-map-renderer-get-tx tiled-map)
                 x-diff)))
            (tiled-map-renderer-set-ty
             tiled-map
             (exact-floor
              (+ (tiled-map-renderer-get-ty tiled-map)
                 y-diff)))
            (sprite-set-x
             sprite
             (+ (sprite-get-x sprite)
                x-diff))
            (sprite-set-y
             sprite
             (+ (sprite-get-y sprite)
                y-diff))))

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(update) update]
            [(get-pos) get-pos]
            [(get-target-x) get-target-x]
            [(get-target-y) get-target-y]
            [(set-target-x) set-target-x]
            [(set-target-y) set-target-y]
            [(center-map) center-map]
            [else (const #t)])))

(define (character-get-pos c)
    ((c 'get-pos)))

(define (character-get-target-x c)
    ((c 'get-target-x)))

(define (character-get-target-y c)
    ((c 'get-target-y)))

(define (character-set-target-x c x)
    ((c 'set-target-x) x))

(define (character-set-target-y c y)
    ((c 'set-target-y) y))

(define (character-center-map c)
    ((c 'center-map)))

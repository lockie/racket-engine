#lang racket/base

(require racket/function racket/math "tiled.rkt" "sprite.rkt")

(provide
 (all-defined-out))


(define (make-character sprite tiled-map)
    (define speed 200)  ;; TODO : unhardcode

    ;; NOTE : coords are isometric
    (define target-x 0)
    (define target-y 0)
    ;; sprite pixel offset relative to tile it's in
    (define sprite-offset-x 0)
    (define sprite-offset-y 0)

    (define (load renderer)
        ;; XXX these are somewhat hardcoded formulas for Flare character sprites
        (set! sprite-offset-x
            (exact-floor
             (/
              (- (sprite-get-width sprite)
                 (tiled-map-renderer-get-tile-width tiled-map))
              2)))
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

    (define (sprite-pos-x)
        (+ (sprite-get-x sprite) sprite-offset-x))

    (define (sprite-pos-y)
        (+ (sprite-get-y sprite) sprite-offset-y))

    (define (move dt)
        ;; TODO : walking sound
        (switch-stance 'move)
        (let-values ([(x y)
                      (tiled-map-renderer-map-to-screen
                       tiled-map target-x target-y)])
            (define direction (atan (- (sprite-pos-y) y) (- (sprite-pos-x) x)))
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
            (if (and (approx-equal (sprite-pos-x) x)
                     (approx-equal (sprite-pos-y) y))
                (switch-stance 'idle)
                (move dt))))

    (define (get-pos)
        (tiled-map-renderer-screen-to-map
         tiled-map (sprite-pos-x) (sprite-pos-y)))

    (define (get-target-x)
        target-x)

    (define (get-target-y)
        target-y)

    (define (set-target-x x)
        (set! target-x x))

    (define (set-target-y y)
        (set! target-y y))

    ;; TODO : center-map method

    (lambda (msg)
        (case msg
            [(load) load]
            [(update) update]
            [(get-pos) get-pos]
            [(get-target-x) get-target-x]
            [(get-target-y) get-target-y]
            [(set-target-x) set-target-x]
            [(set-target-y) set-target-y]
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

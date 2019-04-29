#lang racket/base

(require racket/function racket/math racket/list racket/set data/heap "sdl-mixer.rkt" "tiled.rkt" "sprite.rkt")

(provide
 (all-defined-out))


(define (make-character sprite tiled-map #:player [player #f])
    (define speed 200)  ;; TODO : unhardcode
    (define defence 10)
    (define offence 10)
    (define crit-chance 1)
    (define health 100)

    (define screen-width 0)
    (define screen-height 0)

    ;; NOTE : coords are isometric
    (define target-x 0)
    (define target-y 0)
    ;; sprite pixel offset relative to tile it's in
    (define sprite-offset-x 0)
    (define sprite-offset-y 0)

    (define attack-target #f)

    (define hit-sound #f)

    (define (astar start-x start-y dest-x dest-y)
        (define (f path)
            (let* ([end (last path)]
                   [end-x (car end)]
                   [end-y (cdr end)])
                (+ (length path)
                   ;; distance to destination heuristic is euclidean
                   (sqrt (+ (sqr (- end-x dest-x))
                            (sqr (- end-y dest-y)))))))
        (define closed (mutable-set))
        (define open
            (make-heap
             (lambda (x y)
                 (<= (f x) (f y)))))
        (heap-add! open (list (cons start-x start-y)))
        (let loop ()
            (if (zero? (heap-count open))
                #f
                (let ()
                    (define p (heap-min open))
                    (heap-remove-min! open)
                    (define x (last p))
                    (cond
                        [(set-member? closed x) (loop)]
                        [(equal? x (cons dest-x dest-y)) p]
                        [else
                         (set-add! closed x)
                         (for ([dir (in-range 8)])
                             (let-values ([(y-col y-row)
                                           (tiled-map-renderer-adjacent-tile
                                            tiled-map (car x) (cdr x) dir)])
                                 (when (tiled-map-renderer-tile-traversable?
                                             tiled-map y-col y-row)
                                     (heap-add!
                                      open
                                      (append p (list (cons y-col y-row)))))))
                         (loop)])))))

    (define (conf config)
        (set! screen-width (hash-ref config 'window-width 0))
        (set! screen-height (hash-ref config 'window-height 0)))

    (define (load renderer)
        (set! hit-sound (Mix_LoadWAV "hit15.ogg"))
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
        (reset-target))

    (define (approx-equal? x y)
        (< (abs (- x y)) 2))

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
        (let-values ([(x y)
                      (tiled-map-renderer-map-to-screen
                       tiled-map target-x target-y)]
                     [(curr-x curr-y) (get-pos)])
            (define direction
                (atan (- (sprite-with-offset-y) y)
                      (- (sprite-with-offset-x) x)))
            (define distance (* speed dt))
            ;; TODO : turn speed
            ;; TODO : diagonal movement feels different than straight rectilinear.
            ;;  Probably it has something to do with tile width/height ratio.
            (let-values([(next-tile-x next-tile-y)
                         (tiled-map-renderer-adjacent-tile
                          tiled-map curr-x curr-y direction)])
                ;; TODO : this algorithm certainly can use some polishing
                (if (tiled-map-renderer-tile-traversable?
                     tiled-map next-tile-x next-tile-y)
                    (begin
                        (switch-stance 'move)
                        (sprite-set-angle sprite direction)
                        (sprite-set-x
                         sprite
                         (- (sprite-get-x sprite)
                            (* distance (cos direction))))
                        (sprite-set-y
                         sprite
                         (- (sprite-get-y sprite)
                            (* distance (sin direction)))))
                    (begin
                        (switch-stance 'idle)
                        (set! target-x curr-x)
                        (set! target-y curr-y)
                        (let-values ([(new-curr-x new-curr-y)
                                      (tiled-map-renderer-map-to-screen
                                       tiled-map curr-x curr-y)])
                            (unless (and (approx-equal?
                                          new-curr-x
                                          (sprite-with-offset-x))
                                         (approx-equal?
                                          new-curr-y
                                          (sprite-with-offset-y)))
                                ;; smoothly slow down
                                (define direction
                                    (atan (- (sprite-with-offset-y) new-curr-y)
                                          (- (sprite-with-offset-x) new-curr-x)))
                                (sprite-set-angle sprite direction)
                                (sprite-set-x
                                 sprite
                                 (- (sprite-get-x sprite)
                                    (* distance (cos direction))))
                                (sprite-set-y
                                 sprite
                                 (- (sprite-get-y sprite)
                                    (* distance (sin direction)))))))))))

    (define (update dt)
        (when (and (eq? (sprite-get-stance sprite) 'hit)
                   (sprite-stance-finished? sprite))
            (if (dead?)
                (let-values ([(x y) (get-pos)])
                    (set! target-x x)
                    (set! target-y y)
                    (sprite-set-stance sprite 'die))
                (sprite-set-stance sprite 'idle)))
        (cond
            [(eq? (sprite-get-stance sprite) 'die)
             (let-values ([(x y)
                           (tiled-map-renderer-map-to-screen
                            tiled-map target-x target-y)])
                 (sprite-set-x sprite (- x sprite-offset-x))
                 (sprite-set-y sprite (- y sprite-offset-y)))]
            [(not (eq? (sprite-get-stance sprite) 'hit))
             (when (and attack-target
                        (not (character-dead? attack-target)))
                 (let-values ([(attack-target-x attack-target-y)
                               (character-get-pos attack-target)]
                              [(x y) (get-pos)])
                     (define diff-x (- (sprite-get-x
                                        (character-get-sprite attack-target))
                                       (sprite-get-x sprite)))
                     (define diff-y (- (sprite-get-y
                                        (character-get-sprite attack-target))
                                       (sprite-get-y sprite)))
                     (when (and (> (abs diff-x) 1) (> (abs diff-y) 1))
                         (define direction (atan diff-y diff-x))
                         (sprite-set-angle sprite (- pi direction))
                         (let-values ([(dest-x dest-y)
                                       (tiled-map-renderer-adjacent-tile
                                        tiled-map attack-target-x attack-target-y
                                        direction)])
                             (if player
                                 (begin
                                     (set! target-x dest-x)
                                     (set! target-y dest-y))
                                 (begin
                                     (let* ([path (astar x y dest-x dest-y)]
                                            [first-step
                                             (if (and path (> (length path) 1))
                                                 (second path)
                                                 #f)])
                                         (when first-step
                                             (set! target-x (car first-step))
                                             (set! target-y (cdr first-step))))))))))
             (let-values ([(x y)
                           (tiled-map-renderer-map-to-screen
                            tiled-map target-x target-y)])
                 (if (and (approx-equal? (sprite-with-offset-x) x)
                          (approx-equal? (sprite-with-offset-y) y))
                     (begin
                         (when (or
                                (eq? (sprite-get-stance sprite) 'move)
                                (and
                                 (eq? (sprite-get-stance sprite) 'swing)
                                 attack-target
                                 (character-dead? attack-target)))
                             (switch-stance 'idle))
                         (sprite-set-x sprite (- x sprite-offset-x))
                         (sprite-set-y sprite (- y sprite-offset-y))
                         (if (and attack-target
                                  (not (character-dead? attack-target)))
                             (let-values ([(attack-target-x attack-target-y)
                                           (character-get-pos attack-target)]
                                          [(x y) (get-pos)])
                                 (when (and (< (abs (- x attack-target-x)) 2)
                                            (< (abs (- y attack-target-y)) 2))
                                     (do-attack)))
                             (switch-stance 'idle)))
                     (move dt)))]))


    (define (get-sprite)
        sprite)

    (define (get-sprite-offset-x)
        sprite-offset-x)

    (define (get-sprite-offset-y)
        sprite-offset-y)

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
        (unless (dead?)
            (set! target-x x)))

    (define (set-target-y y)
        (unless (dead?)
            (set! target-y y)))

    (define (reset-target)
        (let-values ([(x y) (get-pos)])
            (set! target-x x)
            (set! target-y y)))

    (define (center-map)
        (define x-diff (- (/ screen-width 2) (sprite-with-offset-x)))
        (define y-diff (- (/ screen-height 2) (sprite-with-offset-y)))
        (when (or (> (abs x-diff) 1) (> (abs y-diff) 1))
            (tiled-map-renderer-set-tx
             tiled-map
              (+ (tiled-map-renderer-get-tx tiled-map)
                 x-diff))
            (tiled-map-renderer-set-ty
             tiled-map
              (+ (tiled-map-renderer-get-ty tiled-map)
                 y-diff))
            (sprite-set-x
             sprite
             (+ (sprite-get-x sprite)
                x-diff))
            (sprite-set-y
             sprite
             (+ (sprite-get-y sprite)
                y-diff)))
        (values x-diff y-diff))

    (define (set-attack-target char)
        (set! attack-target char))

    (define (do-attack)
        (define (roll-dice n s)
            (for/sum ([i (in-range n)])
                (random (add1 s))))
        (define (roll-damage offence-value)
            (define range (/ offence-value 2))
            (define roll1 (roll-dice 1 range))
            (define roll2 (roll-dice 1 range))
            (define roll3 (roll-dice 1 range))
            (+ roll1 roll2 roll3 (- (min roll1 roll2 roll3))))
        (switch-stance 'swing)
        (when (sprite-stance-finished? sprite)
            ;; do actual attack
            (define attack-connected?
                (> (+ offence
                      (* 75 (/ offence (character-get-defence attack-target))))
                   (roll-dice 1 100)))
            (when attack-connected?
                (Mix_PlayChannel -1 hit-sound 0)
                (define damage (roll-damage offence))
                (define critical? (< (roll-dice 1 100) crit-chance))
                (when critical?
                    (set! damage (+ damage (roll-damage (* 2 offence)))))
                (character-set-health
                 attack-target
                 (- (character-get-health attack-target)
                    damage))
                (sprite-set-stance sprite 'idle))))

    (define (get-health)
        health)

    (define (set-health h)
        (when (< h health)
            (switch-stance 'hit))
        (set! health h))

    (define (dead?)
        (not (positive? health)))

    (define (get-defence)
        defence)

    (define (get-offence)
        offence)

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(update) update]
            [(switch-stance) switch-stance]
            [(get-sprite) get-sprite]
            [(get-sprite-offset-x) get-sprite-offset-x]
            [(get-sprite-offset-y) get-sprite-offset-y]
            [(get-pos) get-pos]
            [(get-target-x) get-target-x]
            [(get-target-y) get-target-y]
            [(set-target-x) set-target-x]
            [(set-target-y) set-target-y]
            [(reset-target) reset-target]
            [(center-map) center-map]
            [(set-attack-target) set-attack-target]
            [(get-health) get-health]
            [(set-health) set-health]
            [(dead?) dead?]
            [(get-defence) get-defence]
            [(get-offence) get-offence]
            [else (const #t)])))

(define (character-switch-stance c s)
    ((c 'switch-stance) s))

(define (character-get-sprite c)
    ((c 'get-sprite)))

(define (character-get-sprite-offset-x c)
    ((c 'get-sprite-offset-x)))

(define (character-get-sprite-offset-y c)
    ((c 'get-sprite-offset-x)))

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

(define (character-reset-target c)
    ((c 'reset-target)))

(define (character-center-map c)
    ((c 'center-map)))

(define (character-set-attack-target c t)
    ((c 'set-attack-target) t))

(define (character-get-health c)
    ((c 'get-health)))

(define (character-set-health c h)
    ((c 'set-health) h))

(define (character-dead? c)
    ((c 'dead?)))

(define (character-get-defence c)
    ((c 'get-defence)))

(define (character-get-offence c)
    ((c 'get-offence)))

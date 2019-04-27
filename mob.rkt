#lang racket/base

(require racket/function racket/list racket/math racket/set data/heap "tiled.rkt" "character.rkt")

(provide
 (all-defined-out))


(define (make-mob char player-char tiled-map)
    (define destination-col 0)
    (define destination-row 0)

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
                                            tiled-map (car x) (cdr x) dir)]
                                          [(player-col player-row)
                                           (character-get-pos player-char)])
                                 (when (tiled-map-renderer-tile-traversable?
                                             tiled-map y-col y-row)
                                     (heap-add!
                                      open
                                      (append p (list (cons y-col y-row)))))))
                         (loop)])))))

    (define (load renderer)
        (let-values ([(x y) (character-get-pos char)])
            (set! destination-col x)
            (set! destination-row y)))

    (define (update dt)
        (let-values ([(x y) (character-get-pos char)])
            (unless (and (= destination-col x)
                         (= destination-row y))
                (define path (astar x y destination-col destination-row))
                (when path
                    (define first-step (second path))
                    (character-set-target-x char (car first-step))
                    (character-set-target-y char (cdr first-step))))))

    (define (set-destination col row)
        (set! destination-col col)
        (set! destination-row row))

    (lambda (msg)
        (case msg
            [(load) load]
            [(update) update]
            [(set-destination) set-destination]
            [else (const #t)])))

(define (mob-set-destination m x y)
    ((m 'set-destination) x y))

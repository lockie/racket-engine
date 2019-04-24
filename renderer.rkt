#lang racket/base

(require data/heap)

(provide
 (all-defined-out))


;; Internal renderer wrapper with painter's algorithm inside
(define (make-renderer sdl-renderer)
    (define make-render-queue-item cons)
    (define render-queue-item-order car)
    (define render-queue-item-proc cdr)
    (define (render-queue-item-comparator x y)
        (<= (render-queue-item-order x) (render-queue-item-order y)))
    (define render-queue (make-heap render-queue-item-comparator))

    (define (render order render-proc)
        (heap-add! render-queue (make-render-queue-item order render-proc)))

    (define (do-draw)
        (for ([item (in-heap/consume! render-queue)])
            ((render-queue-item-proc item) sdl-renderer)))

    (lambda (msg)
        (case msg
            [(render) render]
            [(do-draw) do-draw]
            [else (error 'renderer "Unknown message: ~a" msg)])))

(define (renderer-render r o p)
    ((r 'render) o p))

(define (renderer-do-draw r)
    ((r 'do-draw)))

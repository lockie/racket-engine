#lang racket/base

(require racket/list racket/function racket/math racket/path sdl "sdl-image.rkt")

(provide
 (all-defined-out))


;; sprite file example:
;; (
;;  (width . 128)
;;  (height . 128)
;;  (speed . 5)
;;  (layers
;;   (default . "minotaur_alpha.png"))
;;  (stances
;;   (idle . 4)
;;   (rush . 8)
;;   (swing . 4)
;;   (block . 2)
;;   (hit . 2)
;;   (die . 4))
;;  )

(define (make-sprite path)
    (define width 0)
    (define height 0)
    (define time-delta 0)
    (define layers #f)
    (define stances #f)

    (define angle 0)
    (define pos-x 0)
    (define pos-y 0)
    (define current-stance #f)
    (define current-frame 0)
    (define current-direction 0)
    (define time-counter 0.0)
    ;; TODO : turn layers on/off

    (define (load renderer)
        (call-with-input-file path
            (lambda (port)
                (let* ([data (read port)]
                       [data-width (assq 'width data)]
                       [data-height (assq 'height data)]
                       [data-speed (assq 'speed data)]
                       [data-layers (assq 'layers data)]
                       [data-stances (assq 'stances data)])
                    (unless (and data-width data-height data-layers data-stances)
                        (error 'sprite "invalid sprite file: ~a" path))
                    (set! width (cdr data-width))
                    (set! height (cdr data-height))
                    (set! time-delta (/ 1.0 (cdr data-speed)))
                    (set! layers
                        (make-immutable-hasheq
                         (let* ([layers (cdr data-layers)]
                                [layer-names (map car layers)]
                                [layer-file-paths (map cdr layers)])
                             (map cons layer-names
                                  (map (lambda (layer-path)
                                           (IMG_LoadTexture
                                            renderer
                                            (build-path
                                             (path-only path)
                                             layer-path)))
                                       layer-file-paths)))))
                    (set! stances
                        (make-immutable-hasheq
                         (let* ([stances (cdr data-stances)]
                                [stance-names (map car stances)]
                                [stance-lengths (map cdr stances)])
                             (map cons stance-names
                                  (map
                                   (let ([index 0])
                                       (lambda (len)
                                           (set! index (+ index len))
                                           (range (- index len) index)))
                                   stance-lengths)))))
                    (set-stance 'idle)

                    ))))

    (define (draw renderer)
        (hash-for-each
         layers
         (lambda (name texture)
             (SDL_RenderCopy
              renderer texture
              (make-SDL_Rect
               (* current-frame width)
               (* current-direction height)
               width height)
              (make-SDL_Rect pos-x pos-y width height)))))

    (define (update dt)
        (set! time-counter (+ time-counter dt))
        (when (> time-counter time-delta)
            (set! time-counter (- time-counter time-delta))
            (define all-frames (hash-ref stances current-stance))
            (define remaining-frames
                (cdr (memq current-frame all-frames)))
            (set! current-frame
                (if (null? remaining-frames)
                    (first all-frames)
                    (first remaining-frames)))))

    (define (get-x)
        pos-x)

    (define (get-y)
        pos-y)

    (define (set-x x)
        (set! pos-x x))

    (define (set-y y)
        (set! pos-y y))

    (define (get-angle)
        angle)

    (define (set-angle a)
        (set! angle a)
        ;; NOTE : west direction is 0 degree angle; counted clockwise
        (set! current-direction (exact-round (/ a (/ pi 4)))))

    (define (get-stance)
        current-stance)

    (define (set-stance s)
        (set! current-stance s)
        (set! current-frame (first (hash-ref stances current-stance))))

    (define (quit)
        (hash-for-each
         layers
         (lambda (name texture)
             (SDL_DestroyTexture texture))))

    (lambda (msg)
        (case msg
            [(load) load]
            [(draw) draw]
            [(update) update]
            [(get-x) get-x]
            [(get-y) get-y]
            [(set-x) set-x]
            [(set-y) set-y]
            [(get-angle) get-angle]
            [(set-angle) set-angle]
            [(get-stance) get-stance]
            [(set-stance) set-stance]
            [(quit) quit]
            [else (const #t)])))

(define (sprite-get-x s)
    ((s 'get-x)))

(define (sprite-get-y s)
    ((s 'get-y)))

(define (sprite-set-x s x)
    ((s 'set-x) x))

(define (sprite-set-y s y)
    ((s 'set-y) y))

(define (sprite-get-angle s)
    ((s 'get-angle)))

(define (sprite-set-angle s a)
    ((s 'set-angle) a))

(define (sprite-get-stance s)
    ((s 'get-stance)))

(define (sprite-set-stance s st)
    ((s 'set-stance) st))
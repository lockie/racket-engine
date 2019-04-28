#lang racket/base

(require racket/list racket/function racket/math racket/path sdl "sdl-image.rkt" "sdl-mixer.rkt" "renderer.rkt")

(provide
 (all-defined-out))


;; sprite file example:
;; (
;;  (width . 128)
;;  (height . 128)
;;  (speed
;;   (idle . 2)
;;   (move . 5)
;;   (swing . 5)
;;   (block . 5)
;;   (hit . 5)
;;   (die . 5))
;;  (sounds
;;   (4 . "sounds/stomp_01.ogg")
;;   (8 . "sounds/stomp_01.ogg")
;;   (13 . "sounds/mino-attack.ogg")
;;   (18 . "sounds/mino-hit.ogg")
;;   (20 . "sounds/mino-death.ogg"))
;;  (layers
;;   (default . "minotaur_alpha.png"))
;;  (stances
;;   (idle . 4)
;;   (move . 8)
;;   (swing . 4)
;;   (block . 2)
;;   (hit . 2)
;;   (die . 4))
;;  )

(define (make-sprite path #:player [player #f])
    (define debug? #f)

    (define width 0)
    (define height 0)
    (define stance-time-deltas #f)
    (define layers #f)
    (define stances #f)
    (define sounds #f)
    (define last-sounded-frame -1)

    (define angle 0)
    ;; NOTE : screen coords
    (define pos-x 0)
    (define pos-y 0)
    (define current-stance #f)
    (define current-frame 0)
    (define current-direction 0)
    (define time-counter 0.0)
    (define layers-toggled #f)

    (define (conf config)
        (set! debug? (hash-ref config 'debug #f)))

    (define (load renderer)
        (call-with-input-file path
            (lambda (port)
                (let* ([data (read port)]
                       [data-width (assq 'width data)]
                       [data-height (assq 'height data)]
                       [data-speed (assq 'speed data)]
                       [data-layers (assq 'layers data)]
                       [data-stances (assq 'stances data)]
                       [data-sounds (assq 'sounds data)])
                    (unless (and data-width data-height data-layers data-stances
                                 data-sounds)
                        (error 'sprite "invalid sprite file: ~a" path))
                    (set! width (cdr data-width))
                    (set! height (cdr data-height))
                    (let* ([layers-list (cdr data-layers)]
                           [layer-names (map car layers-list)]
                           [layer-file-paths (map cdr layers-list)])
                        (set! layers
                            (make-immutable-hasheq
                             (map cons layer-names
                                  (map (lambda (layer-path)
                                           (IMG_LoadTexture
                                            renderer
                                            (build-path
                                             (path-only path)
                                             layer-path)))
                                       layer-file-paths))))
                        (set! layers-toggled
                            (make-hasheq
                             (map cons layer-names
                                  (make-list (length layer-names) #t)))))
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
                    (set! stance-time-deltas
                        (make-immutable-hasheq
                         (map
                          (lambda (speed)
                              (cons (car speed) (/ 1.0 (cdr speed))))
                          (cdr data-speed))))
                    (set! sounds
                        (make-immutable-hasheq
                         (map
                          (lambda (sound)
                              (cons
                               (car sound)
                               (Mix_LoadWAV
                                (build-path
                                 (path-only path)
                                 (cdr sound)))))
                          (cdr data-sounds))))
                    (set-stance 'idle)))))

    (define (time-delta)
        (hash-ref stance-time-deltas current-stance))

    (define (draw renderer)
        (hash-for-each
         layers
         (lambda (name texture)
             (when (hash-ref layers-toggled name #f)
                 (renderer-render
                  renderer
                  (cond
                      [player 30]
                      [(eq? current-stance 'die) 10]
                      [else 50])
                  (lambda (sdl-renderer)
                      (SDL_RenderCopy
                       sdl-renderer texture
                       (make-SDL_Rect
                        (* current-frame width)
                        (* current-direction height)
                        width height)
                       (make-SDL_Rect
                        (exact-round pos-x)
                        (exact-round pos-y)
                        width height)))))))
        (when debug?
            (renderer-render
             renderer
             1010
             (lambda (sdl-renderer)
                 (SDL_SetRenderDrawColor sdl-renderer 255 15 192 255)
                 (SDL_RenderDrawRect
                  sdl-renderer
                  (make-SDL_Rect
                   (exact-round pos-x)
                   (exact-round pos-y)
                   width height))))))

    (define (update dt)
        (unless (= last-sounded-frame current-frame)
            (set! last-sounded-frame current-frame)
            (define sound (hash-ref sounds current-frame #f))
            (when sound
                (Mix_PlayChannel -1 sound 0)))
        (set! time-counter (+ time-counter dt))
        (when (> time-counter (time-delta))
            (set! time-counter (- time-counter (time-delta)))
            (define all-frames (hash-ref stances current-stance))
            (define remaining-frames
                (cdr (memq current-frame all-frames)))
            (set! current-frame
                (if (null? remaining-frames)
                    (if (eq? current-stance 'die)
                        current-frame
                        (first all-frames))
                    (first remaining-frames)))))

    (define (get-width)
        width)

    (define (get-height)
        height)

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
        (when (negative? angle)
            (set! angle (+ angle (* 2 pi))))
        ;; NOTE : west direction is 0 degree angle; counted clockwise
        (set! current-direction (remainder (exact-round (/ angle (/ pi 4))) 8)))

    (define (get-stance)
        current-stance)

    (define (set-stance s)
        (set! current-stance s)
        (set! current-frame (first (hash-ref stances current-stance)))
        (set! time-counter 0))

    (define (get-layer-toggled l)
        (hash-ref layers-toggled l))

    (define (set-layer-toggled l t)
        (hash-set! layers-toggled l t))

    (define (player?)
        player)

    (define (stance-finished?)
        (= current-frame (last (hash-ref stances current-stance))))

    (define (quit)
        (hash-for-each
         layers
         (lambda (name texture)
             (SDL_DestroyTexture texture))))

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(draw) draw]
            [(update) update]
            [(get-width) get-width]
            [(get-height) get-height]
            [(get-x) get-x]
            [(get-y) get-y]
            [(set-x) set-x]
            [(set-y) set-y]
            [(get-angle) get-angle]
            [(set-angle) set-angle]
            [(get-stance) get-stance]
            [(set-stance) set-stance]
            [(get-layer-toggled) get-layer-toggled]
            [(set-layer-toggled) set-layer-toggled]
            [(player?) player?]
            [(stance-finished?) stance-finished?]
            [(quit) quit]
            [else (const #t)])))

(define (sprite-get-width s)
    ((s 'get-width)))

(define (sprite-get-height s)
    ((s 'get-height)))

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

(define (sprite-get-layer-toggled s l)
    ((s 'get-layer-toggled) l))

(define (sprite-set-layer-toggled s l t)
    ((s 'set-layer-toggled) l t))

(define (sprite-player? s)
    ((s 'player?)))

(define (sprite-stance-finished? s)
    ((s 'stance-finished?)))

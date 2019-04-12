#lang racket/base

(require racket/function ffi/unsafe sdl)


(define (event-type event)
    (cast (union-ref (ptr-ref event _SDL_Event) 0) _uint32 _SDL_EventType))

(define (game-thread components)
    (define config
        (make-hasheq
         '(
           (window-width . 600)
           (window-height . 400)
           (window-title . "Generic game")
           )))
    (for-each (lambda (c) ((c 'conf) config)) components)
    (define (config-ref key) (hash-ref config key))

    (SDL_SetMainReady)
    (SDL_Init (bitwise-ior SDL_INIT_VIDEO))

    (define window
        (SDL_CreateWindow
         (config-ref 'window-title)
         SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
         (config-ref 'window-width)
         (config-ref 'window-height)
         0))
    (define renderer (SDL_CreateRenderer window -1 0))
    (SDL_SetRenderDrawColor renderer 0 0 0 255)

    (define tick-period (/ 1.0 (SDL_GetPerformanceFrequency)))
    (define event (cast (malloc _SDL_Event) _pointer _SDL_Event*))

    (define (cleanup)
        (SDL_DestroyRenderer renderer)
        (SDL_DestroyWindow window)
        (SDL_Quit))

    (with-handlers ([exn:fail? (lambda (e) (cleanup) (raise e))])
        (unless (andmap (lambda (c) ((c 'load) renderer)) components)
            (error 'engine "loading failed"))

        (let game-loop ([last-tick (SDL_GetPerformanceCounter)])
            (when (let event-loop ()
                      (cond
                          [(zero? (SDL_PollEvent event)) #t]
                          [(and (eq? (event-type event) 'SDL_QUIT)
                                (andmap (lambda (c) ((c 'quit))) components))
                           #f]
                          [(not
                            (andmap (lambda (c) ((c 'event) event)) components))
                           #f]
                          [else (event-loop)]))
                (define current-tick (SDL_GetPerformanceCounter))
                (for-each
                 (lambda (c) ((c 'update)
                              (* tick-period (- current-tick last-tick))))
                 components)
                (SDL_RenderClear renderer)
                (for-each (lambda (c) ((c 'draw) renderer)) components)
                (SDL_RenderPresent renderer)
                (SDL_Delay 1)
                (game-loop current-tick))))
    (cleanup))


;; NOTE : this is not the part of engine
(define (make-example-game)
    (define texture #f)
    (define rect (make-SDL_Rect 0 0 600 400))

    (define (load renderer)
        (define image (SDL_LoadBMP "image.bmp"))
        (set! texture (SDL_CreateTextureFromSurface renderer image))
        (SDL_FreeSurface image))

    (define (draw renderer)
        (SDL_RenderCopy renderer texture rect rect))

    (define (quit)
        (SDL_DestroyTexture texture)
        #t)

    (lambda (msg)
        (case msg
            [(load) load]
            [(draw) draw]
            [(quit) quit]
            [else identity])))

(define (run-game)
    (define game (make-example-game))
    (thread
     (lambda () (game-thread (list game)))))

(thread-wait (run-game))

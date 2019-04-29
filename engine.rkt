#lang racket/base

(require
 ffi/unsafe
 racket/function
 "sdl.rkt"
 "sdl-image.rkt"
 "sdl-mixer.rkt"
 "sdl-ttf.rkt"
 "character.rkt"
 "renderer.rkt"
 "sprite.rkt"
 "tiled.rkt")

(provide
 (all-from-out "sdl.rkt")
 (all-from-out "sdl-image.rkt")
 (all-from-out "sdl-mixer.rkt")
 (all-from-out "sdl-ttf.rkt")
 (all-from-out "character.rkt")
 (all-from-out "renderer.rkt")
 (all-from-out "sprite.rkt")
 (all-from-out "tiled.rkt")
 (all-defined-out))


;; Helper functions

(define (event-type event)
    (cast (union-ref (ptr-ref event _SDL_Event) 0) _uint32 _SDL_EventType))

(define (event-keysym event)
    (SDL_KeyboardEvent-keysym (union-ref (ptr-ref event _SDL_Event) 3)))

(define (event-mouse-button-x event)
    (SDL_MouseButtonEvent-x (union-ref (ptr-ref event _SDL_Event) 7)))

(define (event-mouse-button-y event)
    (SDL_MouseButtonEvent-y (union-ref (ptr-ref event _SDL_Event) 7)))

(define (event-mouse-motion-state event)
    (SDL_MouseMotionEvent-state (union-ref (ptr-ref event _SDL_Event) 6)))

(define (event-mouse-motion-x event)
    (SDL_MouseMotionEvent-x (union-ref (ptr-ref event _SDL_Event) 6)))

(define (event-mouse-motion-y event)
    (SDL_MouseMotionEvent-y (union-ref (ptr-ref event _SDL_Event) 6)))

;; Engine entrypoint

(define (game-thread components)
    (define config
        (make-hasheq
         '(
           (window-width . 800)
           (window-height . 600)
           (window-title . "Generic game")
           (debug . #f)
           )))
    (for-each (lambda (c) ((c 'conf) config)) components)
    (define (config-ref key) (hash-ref config key))

    (SDL_SetMainReady)
    (SDL_Init (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
    (IMG_Init 'IMG_INIT_PNG)
    (TTF_Init)
    (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT MIX_DEFAULT_CHANNELS 4096)

    (define window
        (SDL_CreateWindow
         (config-ref 'window-title)
         SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
         (config-ref 'window-width)
         (config-ref 'window-height)
         0))
    (define sdl-renderer (SDL_CreateRenderer window -1 0))
    (SDL_SetRenderDrawBlendMode sdl-renderer 'SDL_BLENDMODE_BLEND)
    (define renderer (make-renderer sdl-renderer))

    (define tick-period (/ 1.0 (SDL_GetPerformanceFrequency)))
    (define event (cast (malloc _SDL_Event) _pointer _SDL_Event*))

    (define (cleanup)
        (SDL_DestroyRenderer sdl-renderer)
        (SDL_DestroyWindow window)
        (Mix_CloseAudio)
        (Mix_Quit)
        (IMG_Quit)
        (SDL_Quit))

    (with-handlers ([exn:fail? (lambda (e) (cleanup) (raise e))])
        (unless (andmap (lambda (c) ((c 'load) sdl-renderer)) components)
            (error 'engine "loading failed"))

        (random-seed (current-seconds))
        (collect-garbage)

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
                (for-each (lambda (c) ((c 'draw) renderer)) components)
                (SDL_RenderClear sdl-renderer)
                (renderer-do-draw renderer)
                (SDL_RenderPresent sdl-renderer)
                (SDL_Delay 1)
                (game-loop current-tick))))
    (cleanup))

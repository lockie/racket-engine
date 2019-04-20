#lang racket/base

(require racket/function ffi/unsafe sdl "sdl-image.rkt" "tiled.rkt")


;; racket-sdl fixups

(define-cstruct _SDL_KeyboardEvent
    ([type _uint32]
     [timestamp _uint32]
     [windowID _uint32]
     [state _uint8]
     [repeat _uint8]
     [padding2 _uint8]
     [padding3 _uint8]
     [keysym _SDL_Keysym]))

(define SDLK_ESCAPE  27)


;; Helper functions

(define (event-type event)
    (cast (union-ref (ptr-ref event _SDL_Event) 0) _uint32 _SDL_EventType))

(define (event-keysym event)
    (SDL_KeyboardEvent-keysym (union-ref (ptr-ref event _SDL_Event) 3)))


;; Engine entrypoint

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
    (IMG_Init 'IMG_INIT_PNG)

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
        (IMG_Quit)
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
(define (make-example-game map-renderer)
    (define (load renderer)
        #t)

    (define (draw renderer)
        #t)

    (define (event e)
        (when (eq? (event-type e) 'SDL_KEYDOWN)
            (let ([sym (SDL_Keysym-sym (event-keysym e))])
                (cond
                    [(eq? sym SDLK_ESCAPE) #f]
                    [(eq? sym SDLK_UP)
                     (tiled-map-renderer-set-ty
                      map-renderer
                      (+ (tiled-map-renderer-get-ty map-renderer) 5))]
                    [(eq? sym SDLK_DOWN)
                     (tiled-map-renderer-set-ty
                      map-renderer
                      (- (tiled-map-renderer-get-ty map-renderer) 5))]
                    [(eq? sym SDLK_LEFT)
                     (tiled-map-renderer-set-tx
                      map-renderer
                      (+ (tiled-map-renderer-get-tx map-renderer) 5))]
                    [(eq? sym SDLK_RIGHT)
                     (tiled-map-renderer-set-tx
                      map-renderer
                      (- (tiled-map-renderer-get-tx map-renderer) 5))]
                    ))))

    (define (quit)
        #t)

    (lambda (msg)
        (case msg
            [(load) load]
            [(draw) draw]
            [(event) event]
            [(quit) quit]
            [else (const #t)])))

(define (run-game)
    (define map-file "map.tmx")
    (define tiled-map-renderer
        (make-tiled-map-renderer
         (parse-tiled-map map-file)))
    (define game (make-example-game tiled-map-renderer))
    (thread
     (lambda () (game-thread (list game tiled-map-renderer)))))

(thread-wait (run-game))

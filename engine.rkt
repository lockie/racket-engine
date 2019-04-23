#lang racket/base

(require racket/function ffi/unsafe sdl "sdl-image.rkt" "tiled.rkt" "sprite.rkt" "character.rkt")


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
(define SDLK_d 100)


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
           (window-width . 600)
           (window-height . 400)
           (window-title . "Generic game")
           (debug . #f)
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
                (SDL_SetRenderDrawColor renderer 0 0 0 255)
                (SDL_RenderClear renderer)
                (for-each (lambda (c) ((c 'draw) renderer)) components)
                (SDL_RenderPresent renderer)
                (SDL_Delay 1)
                (game-loop current-tick))))
    (cleanup))


;; NOTE : this is not the part of engine
(define (make-example-game map-renderer player-sprite player-character)
    (define (load renderer)
        (sprite-set-layer-toggled player-sprite 'buckler #f)
        ;; (sprite-set-layer-toggled player-sprite 'clothes #f)
        (sprite-set-layer-toggled player-sprite 'dagger #f)
        (sprite-set-layer-toggled player-sprite 'greatbow #f)
        (sprite-set-layer-toggled player-sprite 'greatstaff #f)
        (sprite-set-layer-toggled player-sprite 'greatsword #f)
        (sprite-set-layer-toggled player-sprite 'leather-armor #f)
        (sprite-set-layer-toggled player-sprite 'longbow #f)
        (sprite-set-layer-toggled player-sprite 'longsword #f)
        (sprite-set-layer-toggled player-sprite 'rod #f)
        (sprite-set-layer-toggled player-sprite 'shield #f)
        (sprite-set-layer-toggled player-sprite 'shortbow #f)
        (sprite-set-layer-toggled player-sprite 'slingshot #f)
        (sprite-set-layer-toggled player-sprite 'staff #f)
        (sprite-set-layer-toggled player-sprite 'steel-armor #f)
        (sprite-set-layer-toggled player-sprite 'wand #f))

    (define (draw renderer)
        #t)

    (define (event e)
        (when (eq? (event-type e) 'SDL_MOUSEBUTTONDOWN)
            (let-values ([(x y)
                          (tiled-map-renderer-screen-to-map
                           map-renderer
                           (event-mouse-button-x e)
                           (event-mouse-button-y e))])
                (character-set-target-x player-character x)
                (character-set-target-y player-character y)))
        (when (eq? (event-type e) 'SDL_MOUSEMOTION)
            (when (bitwise-bit-set? (event-mouse-motion-state e)
                                    (sub1 SDL_BUTTON_LEFT))
                (let-values ([(x y)
                              (tiled-map-renderer-screen-to-map
                               map-renderer
                               (event-mouse-motion-x e)
                               (event-mouse-motion-y e))])
                    (character-set-target-x player-character x)
                    (character-set-target-y player-character y))))
        (when (eq? (event-type e) 'SDL_KEYDOWN)
            (let ([sym (SDL_Keysym-sym (event-keysym e))])
                (cond
                    [(eq? sym SDLK_ESCAPE) #f]
                    [(eq? sym SDLK_d)
                     ;; some debugging
                     (define mouse-x (cast (malloc _int) _pointer _int*))
                     (define mouse-y (cast (malloc _int) _pointer _int*))
                     (SDL_GetMouseState mouse-x mouse-y)
                     (define mouse-coord-x (ptr-ref mouse-x _int))
                     (define mouse-coord-y (ptr-ref mouse-y _int))
                     (let-values ([(x y)
                                   (tiled-map-renderer-screen-to-map
                                    map-renderer mouse-coord-x mouse-coord-y)])
                         (displayln
                          (list 'mouse-pos mouse-coord-x mouse-coord-y x y)))
                     (displayln (append (list 'char-pos)
                                        (call-with-values
                                         (lambda () (character-get-pos
                                                     player-character))
                                         list)))
                     (displayln (list 'char-target
                                      (character-get-target-x player-character)
                                      (character-get-target-y player-character)))
                     (displayln (list 'sprite-pos
                                      (sprite-get-x player-sprite)
                                      (sprite-get-y player-sprite)))
                     (newline)
                     ]))))

    (define (update dt)
        (character-center-map player-character))

    (define (quit)
        #t)

    (lambda (msg)
        (case msg
            [(load) load]
            [(draw) draw]
            [(event) event]
            [(update) update]
            [(quit) quit]
            [else (const #t)])))

(define (run-game)
    (define map-file "map.tmx")
    (define tiled-map-renderer
        (make-tiled-map-renderer
         (parse-tiled-map map-file)))
    (define sprite-path "heroine.ss")
    (define player-sprite (make-sprite sprite-path))
    (define player-character (make-character player-sprite tiled-map-renderer))
    (define game (make-example-game tiled-map-renderer player-sprite player-character))
    (thread
     (lambda () (game-thread (list tiled-map-renderer player-sprite game player-character)))))

(thread-wait (run-game))

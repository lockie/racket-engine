#lang racket/base

(require racket/function ffi/unsafe sdl "sdl-image.rkt" "sdl-mixer.rkt" "renderer.rkt" "tiled.rkt" "sprite.rkt" "character.rkt")


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

(define _SDL_EventType
  (_enum
   '(SDL_FIRSTEVENT     = 0
    SDL_QUIT           = #x100
    SDL_APP_TERMINATING
    SDL_APP_LOWMEMORY
    SDL_APP_WILLENTERBACKGROUND
    SDL_APP_DIDENTERBACKGROUND
    SDL_APP_WILLENTERFOREGROUND
    SDL_APP_DIDENTERFOREGROUND
    SDL_DISPLAYEVENT   = #x150
    SDL_WINDOWEVENT    = #x200
    SDL_SYSWMEVENT
    SDL_KEYDOWN        = #x300
    SDL_KEYUP
    SDL_TEXTEDITING
    SDL_TEXTINPUT
    SDL_KEYMAPCHANGED
    SDL_MOUSEMOTION    = #x400
    SDL_MOUSEBUTTONDOWN
    SDL_MOUSEBUTTONUP
    SDL_MOUSEWHEEL
    SDL_JOYAXISMOTION  = #x600
    SDL_JOYBALLMOTION
    SDL_JOYHATMOTION
    SDL_JOYBUTTONDOWN
    SDL_JOYBUTTONUP
    SDL_JOYDEVICEADDED
    SDL_JOYDEVICEREMOVED
    SDL_CONTROLLERAXISMOTION  = #x650
    SDL_CONTROLLERBUTTONDOWN
    SDL_CONTROLLERBUTTONUP
    SDL_CONTROLLERDEVICEADDED
    SDL_CONTROLLERDEVICEREMOVED
    SDL_CONTROLLERDEVICEREMAPPED
    SDL_FINGERDOWN      = #x700
    SDL_FINGERUP
    SDL_FINGERMOTION
    SDL_DOLLARGESTURE   = #x800
    SDL_DOLLARRECORD
    SDL_MULTIGESTURE
    SDL_CLIPBOARDUPDATE = #x900
    SDL_DROPFILE        = #x1000
    SDL_DROPTEXT
    SDL_DROPBEGIN
    SDL_DROPCOMPLETE
    SDL_AUDIODEVICEADDED = #x1100
    SDL_AUDIODEVICEREMOVED
    SDL_SENSORUPDATE = #x1200
    SDL_RENDER_TARGETS_RESET = #x2000
    SDL_RENDER_DEVICE_RESET
    SDL_USEREVENT    = #x8000
    SDL_LASTEVENT    = #xFFFF)))


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
    (SDL_Init (bitwise-ior SDL_INIT_VIDEO SDL_INIT_AUDIO))
    (IMG_Init 'IMG_INIT_PNG)
    (Mix_OpenAudio 44100 MIX_DEFAULT_FORMAT MIX_DEFAULT_CHANNELS 4096)

    (define window
        (SDL_CreateWindow
         (config-ref 'window-title)
         SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
         (config-ref 'window-width)
         (config-ref 'window-height)
         0))
    (define sdl-renderer (SDL_CreateRenderer window -1 0))
    (SDL_SetRenderDrawColor sdl-renderer 0 0 0 255)
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
                (SDL_SetRenderDrawColor sdl-renderer 0 0 0 255)
                (SDL_RenderClear sdl-renderer)
                (renderer-do-draw renderer)
                (SDL_RenderPresent sdl-renderer)
                (SDL_Delay 1)
                (game-loop current-tick))))
    (cleanup))


;; NOTE : this is not the part of engine
(define (make-example-game map-renderer player-sprite player-character mob-sprite mob-character)
    (define music #f)

    (define (load renderer)
        (set! music (Mix_LoadMUS "music.mp3"))
        (Mix_VolumeMusic 32)
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
        (when (zero? (Mix_PlayingMusic))
            (Mix_PlayMusic music -1))
        (character-center-map player-character))

    (define (quit)
        (Mix_FreeMusic music))

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
    (define player-sprite-path "heroine.ss")
    (define player-sprite (make-sprite player-sprite-path #:player #t))
    (define player-character (make-character player-sprite tiled-map-renderer #:player #t))
    (tiled-map-renderer-set-player-sprite tiled-map-renderer player-sprite)
    (define mob-sprite-path "minotaur.ss")
    (define mob-sprite (make-sprite mob-sprite-path))
    (sprite-set-x mob-sprite 600)
    (sprite-set-y mob-sprite 80)
    (define mob-character (make-character mob-sprite tiled-map-renderer))
    (character-set-attack-target mob-character player-character)
    (define game (make-example-game tiled-map-renderer player-sprite player-character mob-sprite mob-character))
    (thread
     (lambda ()
         (game-thread
          (list
           tiled-map-renderer
           player-sprite
           game
           player-character
           mob-sprite
           mob-character
           )))))

(thread-wait (run-game))

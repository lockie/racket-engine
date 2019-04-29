#lang racket/base

(require racket/function ffi/unsafe sdl "sdl-image.rkt" "sdl-mixer.rkt" "sdl-ttf.rkt" "renderer.rkt" "tiled.rkt" "sprite.rkt" "character.rkt")


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

(define SDL_PIXELFORMAT_RGBA8888 #x16462004)
(define SDL_TEXTUREACCESS_TARGET 2)

(define-cstruct _SDL_Color
    ([r _uint8]
     [g _uint8]
     [b _uint8]
     [a _uint8]))

(define-sdl SDL_RenderCopy (_fun _SDL_Renderer _SDL_Texture _SDL_Rect-pointer/null _SDL_Rect-pointer/null -> _int))


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
(require racket/math racket/string racket/bool racket/list racket/path)

(define (position-character sprite character tiled-object map-renderer)
    ;; TODO : this arithmetic is correct for player, but slightly off for mobs
    (sprite-set-x
     sprite
     (- (tiled-object-x tiled-object)
        (tiled-map-renderer-get-tile-width map-renderer)))
    (sprite-set-y
     sprite
     (- (tiled-object-y tiled-object)
        (* 2 (tiled-map-renderer-get-tile-height map-renderer))
        (character-get-sprite-offset-y character)))
    (character-reset-target character))

(define (make-example-game tiled-map map-renderer player-sprite player-character mob-sprites mob-characters mob-objects text-objects)
    (define window-width 0)
    (define window-height 0)

    (define font #f)
    (define music #f)

    (define popup-text #f)
    (define popup #f)

    (define (conf config)
        (set! window-width (hash-ref config 'window-width 0))
        (set! window-height (hash-ref config 'window-height 0)))

    (define (load renderer)
        (set! font (TTF_OpenFont "font.ttf" 18))
        (TTF_SetFontHinting font TTF_HINTING_NONE)
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
        (sprite-set-layer-toggled player-sprite 'wand #f)
        (let ([player-object
               (findf
                (lambda (object)
                    (string=?
                     "player"
                     (hash-ref (tiled-object-properties object) 'type "")))
                (tiled-map-objects tiled-map))])
            (position-character
             player-sprite player-character player-object map-renderer))
        (for/list ([object mob-objects]
                   [mob-sprite mob-sprites]
                   [mob-character mob-characters])
            (position-character
             mob-sprite mob-character object map-renderer)))

    (define (close-text-popup)
        (when popup
            (SDL_DestroyTexture popup))
        (set! popup #f)
        (set! popup-text #f))

    (define (draw-text-popup sdl-renderer popup-text
                             [color (make-SDL_Color 255 255 255 255)])
        (define popup
            (SDL_CreateTexture
             sdl-renderer
             SDL_PIXELFORMAT_RGBA8888
             SDL_TEXTUREACCESS_TARGET
             window-width
             window-height))
        (SDL_SetRenderTarget sdl-renderer popup)
        (SDL_SetRenderDrawColor sdl-renderer 0 0 0 128)
        (SDL_RenderClear sdl-renderer)
        (define surfaces
            (map
             (lambda (text)
                 (TTF_RenderText_Blended font text color))
             (string-split popup-text "\n")))
        (define h (for/sum ([s (in-list surfaces)]) (SDL_Surface-h s)))
        (define w (apply max (map SDL_Surface-w surfaces)))
        (define y (exact-round (* 1/2 (- window-height h))))
        (for ([surface (in-list surfaces)])
            (define texture (SDL_CreateTextureFromSurface sdl-renderer surface))
            (SDL_RenderCopy
             sdl-renderer texture #f
             (make-SDL_Rect
              (exact-round (* 1/2 (- window-width w)))
              y (SDL_Surface-w surface) (SDL_Surface-h surface)))
            (set! y (+ y (SDL_Surface-h surface)))
            (SDL_FreeSurface surface)
            (SDL_DestroyTexture texture))
        (SDL_SetRenderTarget sdl-renderer #f)
        (SDL_SetTextureBlendMode popup 'SDL_BLENDMODE_BLEND)
        popup)

    (define (draw renderer)
        (renderer-render
         renderer
         10000
         (lambda (sdl-renderer)
             (when popup-text
                 (unless popup
                     (set! popup (draw-text-popup sdl-renderer popup-text)))
                 (SDL_RenderCopy sdl-renderer popup #f #f)))))

    (define (event e)
        (define (text-point text-object screen-x screen-y)
            (let ([object-x (+ (tiled-object-x text-object)
                               (tiled-map-renderer-get-tx map-renderer))]
                  [object-y (+ (tiled-object-y text-object)
                               (tiled-map-renderer-get-ty map-renderer))])
                (and (>= screen-x object-x)
                     (>= screen-y object-y)
                     (<= screen-x
                         (+ object-x
                            (tiled-object-width text-object)))
                     (<= screen-y
                         (+ object-y
                            (tiled-object-height text-object))))))
        (define (mob-point mob-character screen-x screen-y)
            (let* ([mob-sprite (character-get-sprite mob-character)]
                   [mob-x (sprite-get-x mob-sprite)]
                   [mob-y (sprite-get-y mob-sprite)]
                   [mob-w (sprite-get-width mob-sprite)]
                   [mob-h (sprite-get-height mob-sprite)])
                (and (not (character-dead? mob-character))
                     (>= screen-x mob-x)
                     (>= screen-y mob-y)
                     (<= screen-x (+ mob-x mob-w))
                     (<= screen-y (+ mob-y mob-h)))))
        (define (do-click screen-x screen-y)
            (if popup-text
                (close-text-popup)
                (let-values ([(x y)
                              (tiled-map-renderer-screen-to-map
                               map-renderer screen-x screen-y)]
                             [(pointed-text)
                              (findf
                               (lambda (text-object)
                                   (text-point text-object screen-x screen-y))
                               text-objects)]
                             [(pointed-mob)
                              (findf
                               (lambda (mob-character)
                                   (mob-point mob-character screen-x screen-y))
                               mob-characters)])
                    (cond
                        [pointed-mob
                         (character-set-attack-target
                          player-character pointed-mob)]
                        [pointed-text
                         (set! popup-text (tiled-object-text pointed-text))]
                        [else
                         (character-set-attack-target player-character #f)
                         (character-set-target-x player-character x)
                         (character-set-target-y player-character y)]))))
        (character-set-attack-target player-character #f)
        (case (event-type e)
            [(SDL_MOUSEBUTTONDOWN)
             (do-click (event-mouse-button-x e) (event-mouse-button-y e))]
            [(SDL_MOUSEMOTION)
             (when (bitwise-bit-set? (event-mouse-motion-state e)
                                     (sub1 SDL_BUTTON_LEFT))
                 (do-click (event-mouse-motion-x e) (event-mouse-motion-y e)))]
            [(SDL_KEYDOWN)
             (close-text-popup)]))

    (define (update dt)
        (when (zero? (Mix_PlayingMusic))
            (Mix_PlayMusic music -1))
        (define-values (x-diff y-diff) (character-center-map player-character))
        (for/list ([mob-sprite mob-sprites])
            (sprite-set-x
             mob-sprite
             (+ (sprite-get-x mob-sprite)
                x-diff))
            (sprite-set-y
             mob-sprite
             (+ (sprite-get-y mob-sprite)
                y-diff))))

    (define (quit)
        (Mix_FreeMusic music)
        (TTF_CloseFont font))

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(draw) draw]
            [(event) event]
            [(update) update]
            [(quit) quit]
            [else (const #t)])))

(define (run-game)
    (define map-file "map.tmx")
    (define player-sprite-path "heroine.ss")
    (define tiled-map (parse-tiled-map map-file))
    (define tiled-map-renderer (make-tiled-map-renderer tiled-map))
    (define player-sprite (make-sprite player-sprite-path #:player #t))
    (define player-character (make-character player-sprite tiled-map-renderer #:player #t))
    (tiled-map-renderer-set-player-sprite tiled-map-renderer player-sprite)
    (define text-objects
        (filter
         (lambda (object)
             (string=?
              "text"
              (hash-ref (tiled-object-properties object) 'type "")))
         (tiled-map-objects tiled-map)))
    (define mob-objects
        (filter
         (lambda (object)
             (string=?
              "mob"
              (hash-ref (tiled-object-properties object) 'type "")))
         (tiled-map-objects tiled-map)))
    (define mob-sprites
        (for/list ([object mob-objects])
            (make-sprite
             (build-path
              (path-only (tiled-map-file-path tiled-map))
              (hash-ref (tiled-object-properties object) 'sprite)))))
    (define mob-characters
        (for/list ([sprite mob-sprites])
            (define mob-character
                (make-character sprite tiled-map-renderer))
            (character-set-attack-target mob-character player-character)
            mob-character))
    (define game (make-example-game tiled-map tiled-map-renderer player-sprite player-character mob-sprites mob-characters mob-objects text-objects))
    (thread
     (lambda ()
         (game-thread
          (append
           (list
            tiled-map-renderer
            player-sprite
            player-character)
           mob-sprites
           mob-characters
           (list game))))))

#lang racket/base

;; NOTE : this is not the part of engine

(require
 racket/bool
 racket/file
 racket/function
 racket/list
 racket/math
 racket/path
 racket/string
 "engine.rkt")

(define (position-character sprite character tiled-object map-renderer)
    ;; TODO : this arithmetic is correct for player, but slightly off for mobs
    (sprite-set-x
     sprite
     (+ (tiled-map-renderer-get-tx map-renderer)
        (- (tiled-object-x tiled-object)
           (tiled-map-renderer-get-tile-width map-renderer))))
    (sprite-set-y
     sprite
     (+ (tiled-map-renderer-get-ty map-renderer)
        (- (tiled-object-y tiled-object)
           (* 2 (tiled-map-renderer-get-tile-height map-renderer))
           (character-get-sprite-offset-y character))))
    (character-reset-target character))

(define (make-example-game
         tiled-map map-renderer
         player-sprite player-character
         mob-sprites mob-characters mob-objects
         text-objects
         npc-object npc-sprite npc-character)
    (define window-width 0)
    (define window-height 0)

    (define font #f)
    (define music #f)
    (define win-music #f)
    (define pot-sound #f)

    (define popup-text #f)
    (define popup #f)

    (define orb-texture #f)
    (define orb-fill-texture #f)
    (define orb-health-texture #f)

    (define (conf config)
        (set! window-width (hash-ref config 'window-width 0))
        (set! window-height (hash-ref config 'window-height 0)))

    (define (load-player)
        (sprite-set-stance player-sprite 'idle)
        (sprite-set-layer-toggled player-sprite 'buckler #f)
        (sprite-set-layer-toggled player-sprite 'clothes #f)
        (sprite-set-layer-toggled player-sprite 'dagger #f)
        (sprite-set-layer-toggled player-sprite 'greatbow #f)
        (sprite-set-layer-toggled player-sprite 'greatstaff #f)
        (sprite-set-layer-toggled player-sprite 'greatsword #f)
        (sprite-set-layer-toggled player-sprite 'leather-armor #t)
        (sprite-set-layer-toggled player-sprite 'longbow #f)
        (sprite-set-layer-toggled player-sprite 'longsword #f)
        (sprite-set-layer-toggled player-sprite 'rod #f)
        (sprite-set-layer-toggled player-sprite 'shield #f)
        (sprite-set-layer-toggled player-sprite 'shortbow #f)
        (sprite-set-layer-toggled player-sprite 'slingshot #f)
        (sprite-set-layer-toggled player-sprite 'staff #f)
        (sprite-set-layer-toggled player-sprite 'steel-armor #f)
        (sprite-set-layer-toggled player-sprite 'wand #f)
        (character-set-health!
         player-character (character-get-max-health player-character))
        (let ([player-object
               (findf
                (lambda (object)
                    (string=?
                     "player"
                     (hash-ref (tiled-object-properties object) 'type "")))
                (tiled-map-objects tiled-map))])
            (position-character
             player-sprite player-character player-object map-renderer)))

    (define (load renderer)
        (set! font (TTF_OpenFont "assets/font.ttf" 18))
        (TTF_SetFontHinting font TTF_HINTING_NONE)
        (set! music (Mix_LoadMUS "assets/sounds/music.mp3"))
        (set! win-music (Mix_LoadMUS "assets/sounds/Victory1.mp3"))
        (set! pot-sound (Mix_LoadWAV "assets/sounds/metalPot1.ogg"))
        (Mix_VolumeMusic 32)
        (set! orb-texture (IMG_LoadTexture renderer "assets/images/orb/orb.png"))
        (set! orb-fill-texture (IMG_LoadTexture renderer "assets/images/orb/orb-fill.png"))
        (set! orb-health-texture (IMG_LoadTexture renderer "assets/images/orb/orb-health.png"))
        (SDL_SetTextureBlendMode orb-health-texture 'SDL_BLENDMODE_ADD)
        (load-player)
        (position-character npc-sprite npc-character npc-object map-renderer)
        (for/list ([object mob-objects]
                   [mob-sprite mob-sprites]
                   [mob-character mob-characters])
            (position-character
             mob-sprite mob-character object map-renderer)))

    (define (close-text-popup)
        (when (and popup-text (string=? popup-text "YOU DIED"))
            (load-player))
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
             (string-split
              (string-append popup-text "\n \n(press any key)")
              "\n")))
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
         9000
         (lambda (sdl-renderer)
             (define orb (SDL_CreateTexture
                          sdl-renderer
                          SDL_PIXELFORMAT_RGBA8888
                          SDL_TEXTUREACCESS_TARGET
                          105 105))
             (SDL_SetRenderTarget sdl-renderer orb)
             (define h
                 (exact-round
                  (* 105
                     (/ (character-get-health player-character)
                        (character-get-max-health player-character)))))
             (when (negative? h)
                 (set! h 0))
             (SDL_RenderCopy sdl-renderer orb-fill-texture #f #f)
             (SDL_RenderCopy
              sdl-renderer orb-health-texture #f
              (make-SDL_Rect 0 (- 105 h) 105 105))
             (SDL_SetRenderTarget sdl-renderer #f)
             (SDL_RenderCopy
              sdl-renderer orb-texture #f
              (make-SDL_Rect 343 485 115 115))
             (SDL_SetTextureBlendMode orb 'SDL_BLENDMODE_BLEND)
             (SDL_RenderCopy
              sdl-renderer orb #f
              (make-SDL_Rect 348 490 105 105))
             (SDL_DestroyTexture orb)))
        (define target-character (character-get-attack-target player-character))
        (when target-character
            (renderer-render
             renderer
             9000
             (lambda (sdl-renderer)
                 (SDL_SetRenderDrawColor sdl-renderer 0 0 0 192)
                 (SDL_RenderFillRect
                  sdl-renderer
                  (make-SDL_Rect 320 10 160 25))
                 (SDL_SetRenderDrawColor sdl-renderer 86 12 6 255)
                 (SDL_RenderFillRect
                  sdl-renderer
                  (make-SDL_Rect
                   320 10
                   (exact-round
                    (/
                     (* 160
                        (character-get-health target-character))
                     (character-get-max-health target-character)))
                   25)))))
        (when popup-text
            (renderer-render
             renderer
             10000
             (lambda (sdl-renderer)
                 (unless popup
                     (set! popup (draw-text-popup sdl-renderer popup-text)))
                 (SDL_RenderCopy sdl-renderer popup #f #f)))))

    (define (pickup-reward)
        (sprite-set-layer-toggled player-sprite 'greatsword #t)
        (sprite-set-layer-toggled player-sprite 'leather-armor #f)
        (sprite-set-layer-toggled player-sprite 'steel-armor #t)
        (character-set-speed player-character 120)
        (character-set-defence player-character 2400)
        (character-set-offence player-character 15)
        (character-set-crit-chance player-character 10))

    (define talked-with-npc #f)

    (define intro-text #<<EOT
You see some old monk near the road. You hail him.
- May your days be light, monk.
- May your days be light, soldier. What brings you to this forsaken land?
- I'm heading home.
- I've heard the news we've won the war with The Dark One. Unfortunately,
his spawn still inhabit the land. You won't be able to travel further inland.
The foul beast lurks in the forest, destroying everything that is light.
- I'll kill it.
- Many have tried, and many have failed. You may stand a chance before him
with the legendary combat equipment. It is hidden in the old graveyard,
which is infested with zombies. If the need arises, I can share some food
with you so you can regain your strength.
- Thanks for the heads up.
- May your days be light.
EOT
        )

    (define (npc-interact)
        (if talked-with-npc
            (set! popup-text "- May your days be light, soldier.")
            (begin
                (set! popup-text intro-text)
                (set! talked-with-npc #t)))
        (when (< (character-get-health player-character)
                 (character-get-max-health player-character))
            (Mix_PlayChannel -1 pot-sound 0)
            (character-set-health!
             player-character
             (character-get-max-health player-character))))

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
            (unless popup-text
                (let-values ([(x y)
                              (tiled-map-renderer-screen-to-map
                               map-renderer screen-x screen-y)]
                             [(pointed-text)
                              (findf
                               (lambda (text-object)
                                   (text-point text-object screen-x screen-y))
                               text-objects)]
                             [(pointed-npc)
                              (mob-point npc-character screen-x screen-y)]
                             [(pointed-mob)
                              (findf
                               (lambda (mob-character)
                                   (mob-point mob-character screen-x screen-y))
                               mob-characters)])
                    (cond
                        [pointed-npc
                         (npc-interact)]
                        [pointed-mob
                         (character-set-attack-target
                          player-character pointed-mob)]
                        [(and pointed-text
                              (let-values ([(map-x map-y)
                                            (tiled-map-renderer-screen-to-map
                                             map-renderer
                                             (+ (tiled-object-x pointed-text)
                                                (tiled-map-renderer-get-tx map-renderer))
                                             (+ (tiled-object-y pointed-text)
                                                (tiled-map-renderer-get-ty map-renderer)))]
                                           [(char-x char-y)
                                            (character-get-pos
                                             player-character)])
                                  (and (< (abs (- char-x map-x)) 4)
                                       (< (abs (- char-y map-y)) 4))))
                         (set! popup-text (tiled-object-text pointed-text))
                         (when (string=?
                                "true"
                                (hash-ref
                                 (tiled-object-properties pointed-text)
                                 'reward ""))
                             (pickup-reward))]
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
        (for/list ([mob-object mob-objects]
                   [mob-char mob-characters])
            (when (and
                   (string=?
                    (hash-ref (tiled-object-properties mob-object) 'boss "")
                    "true")
                   (character-dead? mob-char))
                (when (not popup-text)
                    (Mix_PlayMusic win-music -1))
                (set! popup-text
                    (string-trim (file->string "CREDITS.txt" #:mode 'text)))))
        (when (zero? (Mix_PlayingMusic))
            (Mix_PlayMusic music -1))
        (when (character-dead? player-character)
            (set! popup-text "YOU DIED"))
        (define-values (x-diff y-diff) (character-center-map player-character))
        (sprite-set-x npc-sprite (+ (sprite-get-x npc-sprite) x-diff))
        (sprite-set-y npc-sprite (+ (sprite-get-y npc-sprite) y-diff))
        (for/list ([mob-sprite mob-sprites])
            (sprite-set-x
             mob-sprite
             (+ (sprite-get-x mob-sprite)
                x-diff))
            (sprite-set-y
             mob-sprite
             (+ (sprite-get-y mob-sprite)
                y-diff)))
        (unless (character-dead? player-character)
            (let-values ([(player-x player-y)
                          (character-get-pos player-character)])
                (for/list ([mob-character mob-characters])
                    (let-values ([(mob-x mob-y) (character-get-pos mob-character)])
                        (unless (character-get-attack-target mob-character)
                            (when (< (sqrt (+ (sqr (- mob-x player-x))
                                              (sqr (* 0.5 (- mob-y player-y)))))
                                     6)
                                (character-set-attack-target
                                 mob-character player-character))))))))

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
    (define map-file "assets/map.tmx")
    (define tiled-map (parse-tiled-map map-file))
    (define tiled-map-renderer (make-tiled-map-renderer tiled-map))
    (define player-sprite-path "assets/heroine.ss")
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
        (for/list ([object mob-objects]
                   [sprite mob-sprites])
            (define char
                (make-character sprite tiled-map-renderer))
            (define (get-prop prop default)
                (string->number
                 (hash-ref (tiled-object-properties object) prop default)))
            (character-set-speed char (get-prop 'speed "200"))
            (character-set-defence char (get-prop 'defence "10"))
            (character-set-offence char (get-prop 'offence "10"))
            (character-set-crit-chance char (get-prop 'crit-chance "10"))
            (character-set-health! char (get-prop 'health "100"))
            (character-set-max-health char (get-prop 'max-health "100"))
            char))
    (define npc-object
        (findf
         (lambda (object)
             (string=?
              "npc"
              (hash-ref (tiled-object-properties object) 'type "")))
         (tiled-map-objects tiled-map)))
    (define npc-sprite
        (make-sprite
         (build-path
          (path-only (tiled-map-file-path tiled-map))
          (hash-ref (tiled-object-properties npc-object) 'sprite))))
    (define npc-character
        (make-character npc-sprite tiled-map-renderer))
    (define game
        (make-example-game
         tiled-map tiled-map-renderer
         player-sprite player-character
         mob-sprites mob-characters mob-objects
         text-objects
         npc-object npc-sprite npc-character))
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
           (list npc-sprite npc-character game))))))


(thread-wait (run-game))

#lang racket/base

(require ffi/unsafe sdl)

(provide
 (all-from-out sdl)
 (all-defined-out))


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
#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         sdl)

(provide
 (all-defined-out))


(define-ffi-definer define-sdl-image (ffi-lib "libSDL2_image"))

(define-sdl-image IMG_Linked_Version (_fun -> _SDL_version-pointer))

(define _IMG_InitFlags
    (_bitmask
     '(IMG_INIT_JPG = #x00000001
       IMG_INIT_PNG = #x00000002
       IMG_INIT_TIF = #x00000004
       IMG_INIT_WEBP = #x00000008)))

(define-sdl-image IMG_Init (_fun _IMG_InitFlags -> _int))

(define-sdl-image IMG_Quit (_fun -> _void))

(define-sdl-image IMG_LoadTexture (_fun _SDL_Renderer _string -> _SDL_Texture))

(define-sdl-image IMG_SavePNG (_fun _SDL_Surface _string -> _int))

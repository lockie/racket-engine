#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define
         sdl)

(provide
 (all-defined-out))


(define-ffi-definer define-sdl-mixer (ffi-lib "libSDL2_mixer"))

(define _SDL_bool _int)

(define-sdl-mixer Mix_Linked_Version (_fun -> _SDL_version-pointer))

(define _MIX_InitFlags
    (_bitmask
     '(MIX_INIT_FLAC = #x00000001
       MIX_INIT_MOD  = #x00000002
       MIX_INIT_MP3  = #x00000008
       MIX_INIT_OGG  = #x00000010
       MIX_INIT_MID  = #x00000020
       MIX_INIT_OPUS = #x00000040)))

(define-sdl-mixer Mix_Init (_fun _MIX_InitFlags -> _int))

(define-sdl-mixer Mix_Quit (_fun -> _void))

(define MIX_DEFAULT_FREQUENCY 22050)

(define MIX_DEFAULT_FORMAT
    (if (system-big-endian?)
        AUDIO_S16MSB
        AUDIO_S16LSB))

(define MIX_DEFAULT_CHANNELS 2)

(define-cstruct _Mix_Chunk
    ([allocated _int]
     [abuf (_cpointer _uint8)]
     [alen _uint32]
     [volume _uint8]))

(define-cpointer-type _Mix_Chunk*)

(define _Mix_Fading
    (_enum
     '(MIX_NO_FADING
       MIX_FADING_OUT
       MIX_FADING_IN)))

(define _Mix_MusicType
    (_enum
     '(MUS_NONE
       MUS_CMD
       MUS_WAV
       MUS_MOD
       MUS_MID
       MUS_OGG
       MUS_MP3
       MUS_MP3_MAD_UNUSED
       MUS_FLAC
       MUS_MODPLUG_UNUSED
       MUS_OPUS)))

(define-cpointer-type _Mix_Music*)

(define-sdl-mixer Mix_OpenAudio (_fun _int _uint16 _int _int -> _int))

(define-sdl-mixer Mix_OpenAudioDevice (_fun _int _uint16 _int _int _string _int -> _int))

(define-sdl-mixer Mix_AllocateChannels (_fun _int -> _int))

(define-sdl-mixer Mix_QuerySpec (_fun _int* _uint16* _int* -> _int))

(define-sdl-mixer Mix_LoadWAV_RW (_fun _pointer _int -> _Mix_Chunk*))
(define (Mix_LoadWAV file) (Mix_LoadWAV_RW (SDL_RWFromFile file "rb") 1))

(define-sdl-mixer Mix_LoadMUS (_fun _string -> _Mix_Music*))

(define-sdl-mixer Mix_LoadMUS_RW (_fun _pointer _int -> _Mix_Music*))

(define-sdl-mixer Mix_LoadMUSType_RW (_fun _pointer _Mix_MusicType _int -> _Mix_Music*))

(define-sdl-mixer Mix_QuickLoad_WAV (_fun _uint8* -> _Mix_Chunk*))

(define-sdl-mixer Mix_QuickLoad_RAW (_fun _uint8* _uint32 -> _Mix_Chunk*))

(define-sdl-mixer Mix_FreeChunk (_fun _Mix_Chunk* -> _void))

(define-sdl-mixer Mix_FreeMusic (_fun _Mix_Music* -> _void))

(define-sdl-mixer Mix_GetNumChunkDecoders (_fun -> _int))
(define-sdl-mixer Mix_GetChunkDecoder (_fun _int -> _string))
(define-sdl-mixer Mix_HasChunkDecoder (_fun _string -> _SDL_bool))
(define-sdl-mixer Mix_GetNumMusicDecoders (_fun -> _int))
(define-sdl-mixer Mix_GetMusicDecoder (_fun _int -> _string))
;; see https://discourse.libsdl.org/t/missing-mix-hasmusicdecoder-definition/23392
;;(define-sdl-mixer Mix_HasMusicDecoder (_fun _string -> _SDL_bool))

(define-sdl-mixer Mix_GetMusicType (_fun _Mix_Music* -> _Mix_MusicType))

(define-sdl-mixer Mix_SetPostMix (_fun (_fun _pointer _uint8* _int -> _void) _pointer -> _void))

(define-sdl-mixer Mix_HookMusic (_fun (_fun _pointer _uint8* _int -> _void) _pointer -> _void))

(define-sdl-mixer Mix_HookMusicFinished (_fun (_fun -> _void) -> _void))

(define-sdl-mixer Mix_GetMusicHookData (_fun -> _pointer))

(define-sdl-mixer Mix_ChannelFinished (_fun (_fun _int -> _void) -> _void))

(define MIX_CHANNEL_POST -2)

(define _Mix_EffectFunc_t* (_fun _int _pointer _int _pointer -> _void))

(define _Mix_EffectDone_t* (_fun _int _pointer -> _void))

(define-sdl-mixer Mix_RegisterEffect (_fun _int _Mix_EffectFunc_t* _Mix_EffectDone_t* _pointer -> _int))

(define-sdl-mixer Mix_UnregisterEffect (_fun _int _Mix_EffectFunc_t* -> _int))

(define-sdl-mixer Mix_UnregisterAllEffects (_fun _int -> _int))

(define MIX_EFFECTSMAXSPEED "MIX_EFFECTSMAXSPEED")

(define-sdl-mixer Mix_SetPanning (_fun _int _uint8 _uint8 -> _int))

(define-sdl-mixer Mix_SetPosition (_fun _int _sint16 _uint8 -> _int))

(define-sdl-mixer Mix_SetDistance (_fun _int _uint8 -> _int))

(define-sdl-mixer Mix_SetReverseStereo (_fun _int _int -> _int))

(define-sdl-mixer Mix_ReserveChannels (_fun _int -> _int))

(define-sdl-mixer Mix_GroupChannel (_fun _int _int -> _int))
(define-sdl-mixer Mix_GroupChannels (_fun _int _int _int -> _int))
(define-sdl-mixer Mix_GroupAvailable (_fun _int -> _int))
(define-sdl-mixer Mix_GroupCount (_fun _int -> _int))
(define-sdl-mixer Mix_GroupOldest (_fun _int -> _int))
(define-sdl-mixer Mix_GroupNewer (_fun _int -> _int))

(define (Mix_PlayChannel channel chunk loops) (Mix_PlayChannelTimed channel chunk loops -1))
(define-sdl-mixer Mix_PlayChannelTimed (_fun _int _Mix_Chunk* _int _int -> _int))
(define-sdl-mixer Mix_PlayMusic (_fun _Mix_Music* _int -> _int))

(define-sdl-mixer Mix_FadeInMusic (_fun _Mix_Music* _int _int -> _int))
(define-sdl-mixer Mix_FadeInMusicPos (_fun _Mix_Music* _int _int _double -> _int))
(define (Mix_FadeInChannel channel chunk loops ms) (Mix_FadeInChannelTimed channel chunk loops ms -1))
(define-sdl-mixer Mix_FadeInChannelTimed (_fun _int _Mix_Chunk* _int _int _int -> _int))

(define-sdl-mixer Mix_Volume(_fun _int _int -> _int))
(define-sdl-mixer Mix_VolumeChunk(_fun _Mix_Chunk* _int -> _int))
(define-sdl-mixer Mix_VolumeMusic(_fun _int -> _int))

(define-sdl-mixer Mix_HaltChannel(_fun _int -> _int))
(define-sdl-mixer Mix_HaltGroup(_fun _int -> _int))
(define-sdl-mixer Mix_HaltMusic(_fun -> _int))

(define-sdl-mixer Mix_ExpireChannel(_fun _int _int -> _int))

(define-sdl-mixer Mix_FadeOutChannel(_fun _int _int -> _int))
(define-sdl-mixer Mix_FadeOutGroup(_fun _int _int -> _int))
(define-sdl-mixer Mix_FadeOutMusic(_fun _int -> _int))

(define-sdl-mixer Mix_FadingMusic(_fun -> _Mix_Fading))
(define-sdl-mixer Mix_FadingChannel(_fun _int -> _Mix_Fading))

(define-sdl-mixer Mix_Pause(_fun _int -> _void))
(define-sdl-mixer Mix_Resume(_fun _int -> _void))
(define-sdl-mixer Mix_Paused(_fun _int -> _int))

(define-sdl-mixer Mix_PauseMusic(_fun -> _void))
(define-sdl-mixer Mix_ResumeMusic(_fun -> _void))
(define-sdl-mixer Mix_RewindMusic(_fun -> _void))
(define-sdl-mixer Mix_PausedMusic(_fun -> _int))

(define-sdl-mixer Mix_SetMusicPosition(_fun _double -> _int))

(define-sdl-mixer Mix_Playing(_fun _int -> _int))
(define-sdl-mixer Mix_PlayingMusic(_fun -> _int))

(define-sdl-mixer Mix_SetMusicCMD(_fun _string -> _int))

(define-sdl-mixer Mix_SetSynchroValue(_fun _int -> _int))
(define-sdl-mixer Mix_GetSynchroValue(_fun -> _int))

(define-sdl-mixer Mix_SetSoundFonts(_fun _string -> _int))
(define-sdl-mixer Mix_GetSoundFonts(_fun -> _string))

(define-sdl-mixer Mix_EachSoundFont(_fun (_fun _string _pointer -> _int) _pointer -> _int))

(define-sdl-mixer Mix_GetChunk(_fun _int -> _Mix_Chunk*))

(define-sdl-mixer Mix_CloseAudio(_fun -> _void))


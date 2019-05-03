$ErrorActionPreference = "Stop"

& "C:\Program Files (x86)\Racket\raco.exe" exe --gui darkness-looming.rkt
& "C:\Program Files (x86)\Racket\raco.exe" distribute DL darkness-looming.exe
Copy-Item assets -Destination DL\ -Recurse -Force

Invoke-WebRequest -OutFile SDL2.zip https://www.libsdl.org/release/SDL2-2.0.9-win32-x86.zip
expand-archive -path SDL2.zip -destinationpath DL -Force
Move-Item -Path DL\README-SDL.txt DL\LICENSE.SDL2.txt -Force

Invoke-WebRequest -OutFile SDL2_image.zip https://www.libsdl.org/projects/SDL_image/release/SDL2_image-2.0.4-win32-x86.zip
expand-archive -path SDL2_image.zip -destinationpath DL -Force
Move-Item -Path DL\SDL2_image.dll DL\libSDL2_image.dll -Force
Move-Item -Path DL\README.txt DL\LICENSE.SDL2_image.txt -Force

Invoke-WebRequest -OutFile SDL2_mixer.zip https://www.libsdl.org/projects/SDL_mixer/release/SDL2_mixer-2.0.4-win32-x86.zip
expand-archive -path SDL2_mixer.zip -destinationpath DL -Force
Move-Item -Path DL\SDL2_mixer.dll DL\libSDL2_mixer.dll -Force
Move-Item -Path DL\README.txt DL\LICENSE.SDL2_mixer.txt -Force

Invoke-WebRequest -OutFile SDL2_ttf.zip https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-2.0.15-win32-x86.zip
expand-archive -path SDL2_ttf.zip -destinationpath DL -Force
Move-Item -Path DL\SDL2_ttf.dll DL\libSDL2_ttf.dll -Force
Move-Item -Path DL\README.txt DL\LICENSE.SDL2_ttf.txt -Force

compress-archive -path DL -destinationpath darkness-looming.zip -compressionlevel optimal -Force

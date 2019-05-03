# racket-engine

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
[![Code size](https://img.shields.io/github/languages/code-size/lockie/racket-engine.svg)](https://github.com/lockie/racket-engine)
[![Downloads](https://img.shields.io/github/downloads/lockie/racket-engine/total.svg)](https://github.com/lockie/racket-engine/releases)
[![License](https://img.shields.io/github/license/lockie/racket-engine.svg)](LICENSE)

A simple SDL2-based game engine in [Racket language](https://racket-lang.org), loosely inspired by [LÖVE framework](https://love2d.org).
Used to create [Itch.io Lisp Game Jam 2019](https://itch.io/jam/lisp-game-jam-2019) entry called [Darkness Looming](https://awkravchuk.itch.io/darkness-looming).

![Screenshot](https://img.itch.zone/aW1nLzIwNjM3MTMucG5n/original/ShqNYz.png)

# Installation

## Windows
Use the provided binaries archive in [releases section](https://github.com/lockie/racket-engine/releases). Just run `darkness-looming.exe` from it.

## Ubuntu
Use the provided binaries archive in [releases section](https://github.com/lockie/racket-engine/releases).
You'll need SDL2, SDL2\_image, SDL2\_mixer and SDL2\_ttf libraries:
```
sudo apt-get install libsd2-2.0-0 libsdl2-image-2.0-0 libsdl2-mixer-2.0-0 libsdl2-ttf-2.0-0
sudo ln -s /usr/lib/x86_64-linux-gnu/libSDL2-2.0.so.0 /usr/lib/libSDL2.so
sudo ln -s /usr/lib/x86_64-linux-gnu/libSDL2_image-2.0.so.0 /usr/lib/libSDL2_image.so
sudo ln -s /usr/lib/x86_64-linux-gnu/libSDL2_mixer-2.0.so.0 /usr/lib/libSDL2_mixer.so
sudo ln -s /usr/lib/x86_64-linux-gnu/libSDL2_ttf-2.0.so.0 /usr/lib/libSDL2_ttf.so
```
To run the game in the unpacked archive directory:
```
cd DL/bin
./darkness-looming
```
If there's no sound, try running `export SDL_AUDIODRIVER=alsa` first.

## Other Linuces
Use the provided binaries archive in [releases section](https://github.com/lockie/racket-engine/releases).
You'll need SDL2, SDL2\_image, SDL2\_mixer and SDL2\_ttf libraries.
To run the game in the unpacked archive directory:
```
cd DL/bin
./darkness-looming
```
If there's no sound, try running `export SDL_AUDIODRIVER=alsa` first.

## From source
To run the game from source:
1. you'll need working [Racket](https://download.racket-lang.org) 7.0+ installation.
2. Install necessary packages: `raco pkg install csv-reading sxml sdl`
3. If you're using git, don't forget to pull all the assets from Git LFS: `git lfs fetch`
4. run `racket darkness-looming.rkt`

# Art
[List of art used in the game](CREDITS.md)

# License
[MIT](LICENSE)

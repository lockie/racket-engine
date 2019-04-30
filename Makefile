
all: darkness-looming.tar.gz

SOURCES := character.rkt darkness-looming.rkt engine.rkt renderer.rkt sdl-image.rkt sdl-mixer.rkt sdl.rkt sdl-ttf.rkt sprite.rkt tiled-layer.rkt tiled.rkt

darkness-looming: ${SOURCES}
	raco exe --gui darkness-looming.rkt

darkness-looming.tar.gz: darkness-looming
	raco distribute DL darkness-looming; cp -r assets DL/bin; tar -czvf $@ DL

clean:
	rm -fr DL darkness-looming *.tar.gz

#lang racket/base

;; NOTE : additionally requires sxml, csv-reading
(require racket/list racket/function racket/math racket/sequence racket/path sxml csv-reading sdl "sdl-image.rkt" "renderer.rkt" "tiled-layer.rkt" "sprite.rkt")

(provide
 (all-defined-out))


(define-struct tiled-map
    (file-path format-version tiled-version orientation render-order width height
               tile-width tile-height stagger-axis stagger-index
               background-color tilesets layers objects) #:transparent)

(define-struct tiled-tileset
    (name first-id tile-width tile-height tile-count columns
          spacing margin image-source) #:transparent)

(define-struct tiled-object
    (id x y width height properties text) #:transparent)

(define (parse-tiled-map path)
    (define (parse-tileset tag)
        (make-tiled-tileset
         (sxml:attr tag 'name)
         (sxml:num-attr tag 'firstgid)
         (sxml:num-attr tag 'tilewidth)
         (sxml:num-attr tag 'tileheight)
         (sxml:num-attr tag 'tilecount)
         (sxml:num-attr tag 'columns)
         (sxml:num-attr tag 'spacing)
         (sxml:num-attr tag 'margin)
         (sxml:attr ((if-car-sxpath '(image)) tag) 'source)))
    (define (symbolize val) (if val (string->symbol val) #f))
    (define (tag-properties tag)
        (make-immutable-hasheq
         (map
          (lambda (property-tag)
              (cons (symbolize (sxml:attr property-tag 'name))
                    (sxml:attr property-tag 'value)))
          ((sxpath '(properties property)) tag))))
    (define (parse-layer tag order)
        (define (csv-layer-parser data)
            (let ([array-data (rest (csv->list data))])
                (lambda (index)
                    (let ([row (vector-ref index 0)]
                          [col (vector-ref index 1)])
                        (string->number
                         (list-ref (list-ref array-data row) col))))))
        (let* ([data ((car-sxpath '(data)) tag)]
               [width (sxml:num-attr tag 'width)]
               [height (sxml:num-attr tag 'height)])
            (unless (and height width)
                (error 'tiled "invalid layer in map file: ~a" path))
            (build-tiled-layer
             (sxml:num-attr tag 'id)
             order
             (sxml:attr tag 'name)
             width
             height
             (tag-properties tag)
             (case (sxml:attr data 'encoding)
                 [("csv") (csv-layer-parser (sxml:text data))]
                 [else #f]))))
    (define (parse-object tag)
        (make-tiled-object
         (sxml:num-attr tag 'id)
         (sxml:num-attr tag 'x)
         (sxml:num-attr tag 'y)
         (sxml:num-attr tag 'width)
         (sxml:num-attr tag 'height)
         (tag-properties tag)
         ((if-car-sxpath '(text *text*)) tag)))
    (call-with-input-file path
        (lambda (port)
            (define document (ssax:xml->sxml port '()))
            (define map-tag ((if-car-sxpath '(map)) document))
            (unless map-tag
                (error 'tiled "invalid map file: ~a" path))
            (make-tiled-map
             path
             (sxml:attr map-tag 'version)
             (sxml:attr map-tag 'tiledversion)
             (symbolize (sxml:attr map-tag 'orientation))
             (symbolize (sxml:attr map-tag 'renderorder))
             (sxml:num-attr map-tag 'width)
             (sxml:num-attr map-tag 'height)
             (sxml:num-attr map-tag 'tilewidth)
             (sxml:num-attr map-tag 'tileheight)
             (symbolize (sxml:attr map-tag 'staggeraxis))
             (symbolize (sxml:attr map-tag 'staggerindex))
             (sxml:attr map-tag 'backgroundcolor)
             (map parse-tileset ((sxpath '(map tileset)) document))
             (map
              (let ([order 0])
                  (lambda (layer-tag)
                      (set! order (add1 order))
                      (parse-layer layer-tag order)))
              ((sxpath '(map layer)) document))
             (map parse-object ((sxpath '(map // object)) document))))))

(define (make-tiled-map-renderer tiled-map)
    (define window-width 0)
    (define window-height 0)
    (define debug? #f)
    (define tile-width 0)
    (define tile-height 0)
    (define transform-x 0)
    (define transform-y 0)
    (define pos-x (const 0))
    (define pos-y (const 0))
    (define tileset-map #f)

    (define player-sprite #f)

    (define (conf config)
        (set! window-width (hash-ref config 'window-width 0))
        (set! window-height (hash-ref config 'window-height 0))
        (set! debug? (hash-ref config 'debug #f)))

    (define (load renderer)
        (define first-tileset (first (tiled-map-tilesets tiled-map)))
        (set! tile-width (tiled-tileset-tile-width first-tileset))
        (set! tile-height (tiled-tileset-tile-height first-tileset))

        (case (tiled-map-orientation tiled-map)
            [(orthogonal)
             (begin
                 (set! pos-x
                       (lambda (row col) (* col tile-width)))
                 (set! pos-y
                       (lambda (row col) (* row tile-height))))]
            [(isometric)
             (begin
                 (set! pos-x
                     (lambda (row col)
                         (* (- col row)
                            (/ tile-width 2))))
                 (set! pos-y
                     (lambda (row col)
                         (* (+ row col)
                            (/ tile-height 2)))))]
            [(staggered)
             (begin
                 (set! pos-x
                       (lambda (row col)
                           (+ (* col tile-width)
                              (/ (* (remainder row 2) tile-width) 2))))
                 (set! pos-y
                       (lambda (row col)
                           (/ (* row tile-height) 2))))])

        (define last-tileset
            (argmax tiled-tileset-first-id (tiled-map-tilesets tiled-map)))
        (define temp-map
            (make-vector
             (+ (tiled-tileset-first-id last-tileset)
                (tiled-tileset-tile-count last-tileset))
             #f))
        (for-each
         (lambda (tileset)
             (define texture
                 (IMG_LoadTexture
                  renderer
                  (build-path
                   (path-only (tiled-map-file-path tiled-map))
                   (tiled-tileset-image-source tileset))))
             (SDL_SetTextureBlendMode texture 'SDL_BLENDMODE_BLEND)
             (define first-index (tiled-tileset-first-id tileset))
             (define columns (tiled-tileset-columns tileset))
             (for ([i (range
                       first-index
                       (+ first-index (tiled-tileset-tile-count tileset)))])
                 (define index (- i first-index))
                 (let-values ([(q r) (quotient/remainder index columns)])
                     (define x (* r tile-width))
                     (define y (* q tile-height))
                     (vector-set! temp-map i
                                  (cons texture
                                        (make-SDL_Rect
                                         x y tile-width tile-height))))))
         (tiled-map-tilesets tiled-map))
        (set! tileset-map (vector->immutable-vector temp-map)))

    (define (ground? layer)
        (string=?
         (hash-ref (tiled-layer-properties layer) 'ground "")
         "true"))

    (define (draw renderer)
        (renderer-render
         renderer
         0
         (lambda (sdl-renderer)
             (define background (tiled-map-background-color tiled-map))
             (define r (string->number (substring background 1 3) 16))
             (define g (string->number (substring background 3 5) 16))
             (define b (string->number (substring background 5 7) 16))
             (SDL_SetRenderDrawColor sdl-renderer r g b 255)
             (SDL_RenderClear sdl-renderer)))
        (define player-x (sprite-get-x player-sprite))
        (define player-y (sprite-get-y player-sprite))
        (for-each
         (lambda (layer)
             (define ground-layer? (ground? layer))
             (define layer-order
                 (+ (tiled-layer-order layer)
                    (if ground-layer? 0 100)))
             (define (obscures-player? x y)
                 (and
                  (not ground-layer?)
                  (> x (sub1 player-x))
                  (< x (+ player-x tile-width 1))
                  (> y (+ player-y 1))
                  (< y (+ player-y (* 3 tile-height) 1))))
             (for-each-tile
              layer
              (lambda (row col tile-index)
                  (let* ([x (exact-round (+ transform-x (pos-x row col)))]
                         [y (exact-round (+ transform-y (pos-y row col)))]
                         [visible? (and (> x (- tile-width))
                                        (> y (- tile-height))
                                        (< x window-width)
                                        (< y window-height))])
                      (when visible?
                          (let* ([tile (vector-ref tileset-map tile-index)]
                                 [texture (car tile)]
                                 [srcrect (cdr tile)])
                              (define opacity (if (obscures-player? x y) 100 255))
                              (renderer-render
                               renderer
                               layer-order
                               (lambda (sdl-renderer)
                                   (SDL_SetTextureAlphaMod texture opacity)
                                   (SDL_RenderCopy
                                    sdl-renderer texture
                                    srcrect
                                    (make-SDL_Rect
                                     x y tile-width tile-height))))
                              (void))))))
             (when debug?
                 (for-each-tile
                  layer
                  (lambda (row col tile-index)
                      (let* ([tile (vector-ref tileset-map tile-index)]
                             [srcrect (cdr tile)]
                             [x (+ transform-x (pos-x row col))]
                             [y (+ transform-y (pos-y row col))]
                             [visible? (and (> x (- tile-width))
                                            (> y (- tile-height))
                                            (< x window-width)
                                            (< y window-height))])
                          (when visible?
                              (renderer-render
                               renderer
                               1000
                               (lambda (sdl-renderer)
                                   (SDL_SetRenderDrawColor
                                    sdl-renderer
                                    (* 20 row) (* 20 col) (* 2 row col) 255)
                                   (SDL_RenderDrawRect
                                    sdl-renderer
                                    (make-SDL_Rect
                                     (exact-round x) (exact-round y)
                                     tile-width tile-height))
                                   (void)))))))))
         (tiled-map-layers tiled-map)))

    (define (get-tile-width)
        tile-width)

    (define (get-tile-height)
        tile-height)

    (define (get-tx)
        transform-x)

    (define (get-ty)
        transform-y)

    (define (set-tx tx)
        (set! transform-x tx))

    (define (set-ty ty)
        (set! transform-y ty))

    (define (screen-to-map screen-x screen-y)
        (let ([global-x (- screen-x transform-x)]
              [global-y (- screen-y transform-y)])
            ;; XXX hardcoded for staggered case
            (define row (exact-round (* 2 (/ global-y tile-height))))
            (define col (exact-round (- (/ global-x tile-width)
                                        (/ (remainder row 2) 2))))
            (values col row)))

    (define (map-to-screen col row)
        (values (+ transform-x (pos-x row col))
                (+ transform-y (pos-y row col))))

    (define (set-player-sprite sprite)
        (set! player-sprite sprite))

    (define (adjacent-tile col row angle)
        (when (negative? angle)
            (set! angle (+ angle (* 2 pi))))
        (define direction (remainder (exact-round (/ angle (/ pi 4))) 8))
        (case direction
            [(0) (values (sub1 col) row)]
            [(1) (values (- col (remainder (add1 row) 2)) (sub1 row))]
            [(2) (values col (- row 2))]
            [(3) (values (+ col (remainder row 2)) (sub1 row))]
            [(4) (values (add1 col) row)]
            [(5) (values (+ col (remainder row 2)) (add1 row))]
            [(6) (values col (+ row 2))]
            [(7) (values (- col (remainder (add1 row) 2)) (add1 row))]
            [else (error 'tiled "Wrong direction: ~a" direction)]))

    (define (tile-traversable? col row)
        (if (or (negative? row) (negative? col))
            #f
            (andmap
             (lambda (layer)
                 (cond
                     [(or (>= row (tiled-layer-height layer))
                          (>= col (tiled-layer-width layer)))
                      #f]
                     [(ground? layer)
                      #t]
                     ;; TODO : read "traversable" property of tiles
                     [else
                      (zero? (pick-tile layer row col))]))
             (tiled-map-layers tiled-map))))

    (define (quit)
        (sequence-for-each
         (lambda (tile)
             (when tile
                 (define texture (car tile))
                 (SDL_DestroyTexture texture)))
         tileset-map))

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(draw) draw]
            [(get-tile-width) get-tile-width]
            [(get-tile-height) get-tile-height]
            [(get-tx) get-tx]
            [(get-ty) get-ty]
            [(set-tx) set-tx]
            [(set-ty) set-ty]
            [(screen-to-map) screen-to-map]
            [(map-to-screen) map-to-screen]
            [(set-player-sprite) set-player-sprite]
            [(adjacent-tile) adjacent-tile]
            [(tile-traversable?) tile-traversable?]
            [(quit) quit]
            [else (const #t)])))

(define (tiled-map-renderer-get-tile-width r)
    ((r 'get-tile-width)))

(define (tiled-map-renderer-get-tile-height r)
    ((r 'get-tile-height)))

(define (tiled-map-renderer-get-tx r)
    ((r 'get-tx)))

(define (tiled-map-renderer-get-ty r)
    ((r 'get-ty)))

(define (tiled-map-renderer-set-tx r tx)
    ((r 'set-tx) tx))

(define (tiled-map-renderer-set-ty r ty)
    ((r 'set-ty) ty))

(define (tiled-map-renderer-screen-to-map r x y)
    ((r 'screen-to-map) x y))

(define (tiled-map-renderer-map-to-screen r row col)
    ((r 'map-to-screen) row col))

(define (tiled-map-renderer-set-player-sprite r s)
    ((r 'set-player-sprite) s))

(define (tiled-map-renderer-adjacent-tile r col row a)
    ((r 'adjacent-tile) col row a))

(define (tiled-map-renderer-tile-traversable? r col row)
    ((r 'tile-traversable?) col row))

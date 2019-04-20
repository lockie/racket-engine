#lang racket/base

;; NOTE : additionally requires sxml, csv-reading
(require racket/list racket/function racket/sequence racket/path sxml csv-reading sdl "sdl-image.rkt" "tiled-layer.rkt")

(provide
 (all-defined-out))


(define-struct tiled-map
    (file-path format-version tiled-version orientation render-order width height
               tile-width tile-height stagger-axis stagger-index
               background-color tilesets layers) #:transparent)

(define-struct tiled-tileset
    (name first-id tile-width tile-height tile-count columns
          spacing margin image-source) #:transparent)

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
    (define (parse-layer tag)
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
             (sxml:attr tag 'name)
             width
             height
             (case (sxml:attr data 'encoding)
                 [("csv") (csv-layer-parser (sxml:text data))]
                 [else #f]))))
    (define (symbolize val) (if val (string->symbol val) #f))
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
             (map parse-layer ((sxpath '(map layer)) document))))))

(define (make-tiled-map-renderer tiled-map)
    (define window-width 0)
    (define window-height 0)
    (define tile-width 0)
    (define tile-height 0)
    (define transform-x 0)
    (define transform-y 0)
    (define pos-x (const 0))
    (define pos-y (const 0))
    (define tileset-map #f)

    (define (conf config)
        (set! window-width (hash-ref config 'window-width 0))
        (set! window-height (hash-ref config 'window-height 0)))

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

    (define (draw renderer)
        (for-each
         (lambda (layer)
             (for-each-tile
              layer
              (lambda (row col tile-index)
                  (let* ([tile (vector-ref tileset-map tile-index)]
                         [texture (car tile)]
                         [srcrect (cdr tile)]
                         [x (+ transform-x (pos-x row col))]
                         [y (+ transform-y (pos-y row col))]
                         [visible? (and (> x (- tile-width))
                                        (> y (- tile-height))
                                        (< x window-width)
                                        (< y window-height))])
                      (when visible?
                          (void (SDL_RenderCopy
                                 renderer texture
                                 srcrect (make-SDL_Rect
                                          x y
                                          tile-width tile-height))))))))
         (tiled-map-layers tiled-map)))

    (define (get-tx)
        transform-x)

    (define (get-ty)
        transform-y)

    (define (set-tx tx)
        (set! transform-x tx))

    (define (set-ty ty)
        (set! transform-y ty))

    (define (quit)
        (sequence-for-each
         (lambda (tile)
             (define texture (car tile))
             (SDL_DestroyTexture texture))
         tileset-map))

    (lambda (msg)
        (case msg
            [(conf) conf]
            [(load) load]
            [(draw) draw]
            [(get-tx) get-tx]
            [(get-ty) get-ty]
            [(set-tx) set-tx]
            [(set-ty) set-ty]
            [(quit) quit]
            [else (const #t)])))

(define (tiled-map-renderer-get-tx r)
    ((r 'get-tx)))

(define (tiled-map-renderer-get-ty r)
    ((r 'get-ty)))

(define (tiled-map-renderer-set-tx r tx)
    ((r 'set-tx) tx))

(define (tiled-map-renderer-set-ty r ty)
    ((r 'set-ty) ty))

#lang typed/racket/base

(require math/array)

(provide
 build-tiled-layer
 for-each-tile)


(struct tiled-layer ([id : Positive-Integer]
                     [name : String]
                     [width : Positive-Integer]
                     [height : Positive-Integer]
                     [data : (Array Nonnegative-Integer)]) #:transparent)

(: build-tiled-layer
   (-> Positive-Integer String Positive-Integer Positive-Integer
       (-> Indexes Nonnegative-Integer) tiled-layer))
(define (build-tiled-layer id name width height data-proc)
    (tiled-layer
     id name width height
     (build-array
      (vector height width)
      data-proc)))

(: for-each-tile
   (-> tiled-layer
       (-> Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer Void)
       Void))
(define (for-each-tile layer proc)
    (for* ([row (tiled-layer-height layer)]
           [col (tiled-layer-width layer)])
        (define tile-index (array-ref (tiled-layer-data layer) (vector row col)))
        (unless (zero? tile-index)
            (proc row col tile-index))))

#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "vec-mat.rkt")
(provide (all-defined-out))
;intrip `(,1/2 ,1/2 0)
;triary res (map from 1) vec(of pixle(4bytes))
;(struct triarray (res vec)#:transparent)
(define test-triary (triarray 4
(vector (vector #"\1\1\1\1")
        (vector #"\1\0\1\1" #"\1\1\0\1")
        (vector #"\1\1\1\0" #"\0\0\1\1" #"\1\0\0\1")
        (vector #"\1\1\0\0" #"\0\1\0\1" #"\1\0\1\0" #"\0\0\0\1"))));singletriangel(4by4 pixles sq 1 2 3 4 pixles pat)
                   ;is alwased to right triangle in 2d (eqilateral in 3d pint dropd border=hypotnece '(1 0 0) to (0 1 0)

;get pixle
(define (ref-triary triary xi yi)
  ;take first 2 (make 2 dim comp data) make max =res to be reft to neerest pixle(only 2 mostly)
  (let* ((res (triarray-res triary))
         (x 0);(inexact->exact (truncate (- (* res xi) .00001))))
         (y 0);(inexact->exact (truncate (- (* res yi) .00001))))
         ;(a (begin (print x) (newline) (print y) (newline)(newline)))
         )
    (vector-ref (vector-ref (triarray-vec triary) y) x)))

;(ref-triary test-triary '(0 .9))
;(ref-triary test-triary '(0 0))
;(ref-triary test-triary '(1 1))
;(ref-triary test-triary '(.3 .8))



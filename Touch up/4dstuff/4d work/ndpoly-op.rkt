#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "opvec.rkt")
(require "ndpoly-rot.rkt")
(provide (all-defined-out))



;rotate ndpoly
(define (rotndpoly ndp di1 di2 deg)
  (let* ((axi (ndpoly-axis ndp))
        (cp (ndpoly-cp ndp))
        (rot-mat (build-rot-mat di1 di2 deg (matrix-rows cp))))
  (ndpoly cp (rot-axis axi rot-mat) (vector-map (λ (x) (rot-abt-pt x cp axi rot-mat)) (ndpoly-avop ndp)) (ndpoly-aloreftri ndp))))
(define pol (rotndpoly (ndpoly (l->mat '(10 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(10 0 1)) (l->mat '(11 0 1)) (l->mat '(10 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
           0 1 (degrees->radians 45)))
(mat->ll (ndpoly-cp pol))
(mat->ll (ndpoly-axis pol))
(vector-map mat->ll (ndpoly-avop pol)) 

;rotate ndpoly
(define (rotndpoly-abs ndp di1 di2 deg)
  (let*((cp (ndpoly-cp ndp))
        (rot-mat (build-rot-mat di1 di2 deg (matrix-rows cp))))
  (ndpoly cp (matrix-mul rot-mat (ndpoly-axis ndp)) (vector-map (λ (x) (rot-abt-pt-abs x cp rot-mat)) (ndpoly-avop ndp)) (ndpoly-aloreftri ndp))))
;(define pol (rotndpoly (ndpoly (l->mat '(0 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(0 0 1)) (l->mat '(1 0 1)) (l->mat '(0 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
 ;          0 1 (degrees->radians 45)))
;(mat->ll (ndpoly-cp pol))
;(mat->ll (ndpoly-axis pol))
;(vector-map mat->ll (ndpoly-avop pol)) 



;move ndpoly
(define (movndpoly ndp dvec amt)
  (let ((mov (matrix-scale dvec amt)))
  (ndpoly (matrix-add (ndpoly-cp ndp) mov) (ndpoly-axis ndp) (vector-map (λ (x) (matrix-add mov x)) (ndpoly-avop ndp)) (ndpoly-aloreftri ndp))))
;(define pol (movndpoly (ndpoly (l->mat '(0 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(0 0 1)) (l->mat '(1 0 1)) (l->mat '(0 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
 ;          (l->mat '(1 0 0)) 2))
;(mat->ll (ndpoly-cp pol))
;(vector-map mat->ll (ndpoly-avop pol))
#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "vec-mat.rkt")
(require "rot3dpoly.rkt")
(provide (all-defined-out))



;rotate 3dpoly
(define (rot3dpoly 3dp di1 di2 deg)
  (let* ((axi (3dpoly-axis 3dp))
        (cp (3dpoly-cp 3dp))
        (rot-mat (build-rot-mat di1 di2 deg (matrix-rows cp))))
  (3dpoly cp (rot-axis axi rot-mat) (vector-map (λ (x) (rot-abt-pt x cp axi rot-mat)) (3dpoly-avop 3dp)) (3dpoly-aloreftri 3dp))))
;(define pol (rot3dpoly (3dpoly (l->mat '(0 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(0 0 1)) (l->mat '(1 0 1)) (l->mat '(0 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
 ;          0 1 (degrees->radians 45)))
;(mat->ll (3dpoly-cp pol))
;(mat->ll (3dpoly-axis pol))
;(vector-map mat->ll (3dpoly-avop pol)) 

;rotate 3dpoly
(define (rot3dpoly-abs 3dp di1 di2 deg)
  (let*((cp (3dpoly-cp 3dp))
        (rot-mat (build-rot-mat di1 di2 deg (matrix-rows cp))))
  (3dpoly cp (matrix-mul rot-mat (3dpoly-axis 3dp)) (vector-map (λ (x) (rot-abt-pt-abs x cp rot-mat)) (3dpoly-avop 3dp)) (3dpoly-aloreftri 3dp))))
;(define pol (rot3dpoly (3dpoly (l->mat '(0 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(0 0 1)) (l->mat '(1 0 1)) (l->mat '(0 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
 ;          0 1 (degrees->radians 45)))
;(mat->ll (3dpoly-cp pol))
;(mat->ll (3dpoly-axis pol))
;(vector-map mat->ll (3dpoly-avop pol)) 



;move 3dpoly
(define (mov3dpoly 3dp dvec amt)
  (let ((mov (matrix-scale dvec amt)))
  (3dpoly (matrix-add (3dpoly-cp 3dp) mov) (3dpoly-axis 3dp) (vector-map (λ (x) (matrix-add mov x)) (3dpoly-avop 3dp)) (3dpoly-aloreftri 3dp))))
;(define pol (mov3dpoly (3dpoly (l->mat '(0 0 1)) (ll->mat '((1 0 0)(0 1 0)(0 0 1))) (vector (l->mat '(0 0 1)) (l->mat '(1 0 1)) (l->mat '(0 1 1))) (list (reftri 0 1 2 (triarray 1 (vector (vector #"\0\0\0\0"))))))
 ;          (l->mat '(1 0 0)) 2))
;(mat->ll (3dpoly-cp pol))
;(vector-map mat->ll (3dpoly-avop pol))
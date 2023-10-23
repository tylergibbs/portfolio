#lang racket
(require (planet wmfarr/plt-linalg:1:13/all))
(define (dot-product v v1)
  (foldr (λ (x y z) (+ (* x y) z)) 0 v v1))

(define (vec+ v v1)
  (map + v v1))

(define (vec- v v1)
  (map - v v1))

(define (vec-scale v n)
  (map (λ (x) (* x n)) v))

(define (len-vec vec)
  (sqrt (foldr (λ (x y) (+ (sqr x) y)) 0 vec)))

(define (norm vec)
  (let ((l (len-vec vec)))
    (map (λ (x) (/ x l)) vec)))

(define (orth-aux v u)
  (vec-scale u (dot-product u v)))

(define (orthonormal alovec acc)
  (cond
    ((null? alovec) (reverse acc))
    (else (orthonormal (rest alovec) (cons (norm (vec- (first alovec) 
                                                       (foldr (λ (x y) (vec+ (orth-aux (first alovec) x) y))
                                                              (make-list (length (first alovec)) 0)
                                                              acc)))
                                           acc)
                       ))))
;(orthonormal '((1 0 2 1) (2 2 3 1) (1 0 1 0)) null)
;(orthonormal '((1 2 1) (2 -1 2) (1 0 -5)) null)
(orthonormal '((1 2 1) (2 -1 2) (1 2 -5)) null)
(define (transpose mat)
  (apply map list mat))
;(transpose '((1 3) (2 4)))

(define (mat* m ma)
  (map (λ (x) (map (λ (y) (dot-product x y)) (transpose m))) ma))
;(mat* '((1 4)(2 5) (3 6)) '((7 9 11) (8 10 12)))

(define (inv-sing-val-dec  U W Vt)
  (mat* U (mat* W Vt)))
(inv-sing-val-dec `((,(/ 1 (sqrt 2)) ,(/ 1 (sqrt 2)))(,(/ 1 (sqrt 2)) ,(/ -1 (sqrt 2))))
                 `((,(sqrt 12) 0)(0 ,(sqrt 10)) (0 0))
               `((,(/ 1 (sqrt 6)) ,(/ 2 (sqrt 5)) ,(/ 1 (sqrt 30)))
                  (,(/ 2 (sqrt 6)) ,(/ -1 (sqrt 5)) ,(/ 2 (sqrt 30)))
                 (,(/ 1 (sqrt 6)) 0 ,(/ -5 (sqrt 30)))))




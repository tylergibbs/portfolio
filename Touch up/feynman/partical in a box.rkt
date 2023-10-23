#lang racket
(require (planet wmfarr/plt-linalg:1:13/all))
(require plot)
(require 2htdp/universe)

;Author: Tyler Gibbs
;2015
;Simulates the energy states of a single partical in a quantum mechanical system using a eigenvector based method


(define (mat->list mat)
  (build-list (matrix-cols mat) (λ (x) (build-list (matrix-rows mat) (λ (y) (matrix-ref mat y x))))))

(define (get-v vec)
  (let* ((l (vector-length vec))
         (m (make-matrix l l 0))
         )
    (begin 
      (build-list l (λ (x) (matrix-set! m x x (vector-ref vec x))))
      m)
    ))

;Testing
;(mat->list (get-v #(0 0 0 1 1 1 1 0)))

(define (get-2dir n)
  (let ((mat (make-matrix n n 0)))
    (begin
    (build-list n (λ (x) (build-list n (λ (y) (cond
                                                ((= x y) (matrix-set! mat x y -2))
                                                ((= x (add1 y)) (matrix-set! mat x y 1)) 
                                                ((= (add1 x) y) (matrix-set! mat x y 1))
                                                (else null))
                                         ))))
    mat)))

;Testing
;(mat->list (get-2dir 10))
;(mat->list (matrix-mul (get-2dir 5) (matrix 5 1 0 1 2 3 4)))
;(mat->list (matrix-mul (get-2dir 5) (matrix 5 1 0 1 4 9 16)))
;(mat->list (matrix-mul (get-2dir 5) (matrix 5 1 0 1 3 6 10)))
;(mat->list (matrix-mul (get-2dir 5) (matrix 5 1 10 6 3 1 0)))
;(mat->list (matrix-mul (get-2dir 5) (matrix 5 1 0 -1 -3 -6 -10)))

(define (get-H vec)
  (matrix-add (get-2dir (vector-length vec)) (get-v vec)))

;Testing
;(mat->list (get-H #(0 0 -1 -1 -1 0)))
;(mat->list (get-H #(0 0 -1 -1 -1 0 0 0)))

(define (get-lps H)
  (local ((define-values (x y z w) (eigensystem H)))
    (map (λ (x y) (list x y)) 
         (vector->list (eigenvalues->vector x y))
         (mat->list z))))

;Testing
;(get-lps (get-H #(0 -1 -1 -1 -1 -1 0)))
;(define well #(0 0 0 0 0 -10000 -10000 -10000 -10000 -10000 -10000 -10000 -10000
;                 -10000 -10000 -10000 -10000 -10000 -10000 -10000 -10000 0 0 0 0 0))
;(define d (foldr (λ (x y) (map (λ (z w) (+ (* (first x) z) w)) (second x) y)) (build-list (vector-length well) (λ (x)0))                      
;                        (sort (get-lps (get-H well))#:key car >)))

(define (disp-aux x wel)
  (plot
   (list (lines
    (map (λ (x y) (vector x y)) 
         (build-list (length (second (second x))) (λ(y)(+ y .2)))
         (second (list-ref (rest x) (first x))))
    #:color 'green)
    (points (map (λ (x y) (vector x y)) (build-list (vector-length wel) (λ(y)y)) (vector->list wel))
            #:color 'blue))
     #:y-max 1 #:y-min -1 #:title (number->string (first(list-ref (rest x) (first x))))))

(define (disp wel)
  (let ((lps (sort (get-lps (get-H wel)) #:key car >)))
  (build-list (length lps) (λ (x) (disp-aux (cons x lps) wel)))))

;Examples

;(get-lps (get-H well))
;(disp  #(0 0 0 0 0 -10 -10 -10 -10 -10 -10 -10 -10
 ;                -10 -10 -10 -10 -10 -10 -10 -10 0 0 0 0 0))
;(map cons (disp  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
 ;                1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;(disp  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 -1 -1 -1 -1 -1 -1 -1
 ;                -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;(map cons (disp  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10
 ;                10 10 10 10 10 10 10 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;(disp  #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -10 -10 -10 -10 -10 -10 -10 -10
 ;                -10 -10 -10 -10 -10 -10 -10 -10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
;(build-vector 41 (λ (x) (sqr (- x 10))))
(disp #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  
        -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1                       
        0 0 0 -1 -1 -1 -1 -1 -1 -1 -1 0 0 0 0 ))
(disp (build-vector 41 (λ (x) (* 1/10 (sqr (- x 20))))))

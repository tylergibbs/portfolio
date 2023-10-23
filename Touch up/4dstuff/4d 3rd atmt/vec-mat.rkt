#lang racket
(provide (all-defined-out))

(require (planet wmfarr/plt-linalg:1:13/matrix))

;vector+
(define (vecl+ v ve)
  (map + v ve))

;vector-
(define (vecl- v ve)
  (map - v ve))

;vector-scale
(define (vecl-scale ve n)
  (map (λ (x) (* n x)) ve))

(define (vecl-l vec)
  (sqrt (foldr (λ (x y) (+ (sqr x) y)) 0 vec)))

(define (norml vec n)
  (let ((l (vecl-l vec)))
    (map (λ (x) (* n (/ x l))) vec)))
;(norm '(1 1) 1)
;(norm '(0 0) 1) error


(define (mat-l vec)
  (local ((define mm (make-matrix (matrix-rows vec) (matrix-cols vec) +nan.0))
           ;row length colem length
          (define (aux c r mc mr)
            (cond
              ((= r mr) 0)
              ((= c mc) (aux 0 (add1 r) mc mr))
              (else (+ (sqr (matrix-ref vec c r)) (aux (add1 c) r mc mr))))))
    (sqrt (aux 0 0 (matrix-rows vec) (matrix-cols vec)))))

(define (mat-norm vec n)
  (let ((s (/ n (mat-l vec))))
    (local ((define mm (make-matrix (matrix-rows vec) (matrix-cols vec) +nan.0))
           ;row length colem length
          (define (aux c r mc mr)
            (cond
              ((= r mr) mm)
              ((= c mc) (aux 0 (add1 r) mc mr))
              (else (begin (matrix-set! mm c r (* (matrix-ref vec c r) s)) (aux (add1 c) r mc mr))))))
    (aux 0 0 (matrix-rows vec) (matrix-cols vec)))))

(define (mat-dot m)
  (matrix-ref (matrix-mul m (matrix-transpose  m)) 0 0))

(define (matrix-append alomat)
  (local ((define mar (matrix-rows (vector-ref alomat 0)))
          (define mm (make-matrix mar (vector-length alomat) 0))
          (define r# 0)
          (define (aux mat r mr)
            (if (= r mr) #t
                (begin 
                  (matrix-set! mm r r# (matrix-ref mat r 0))
                       (aux mat (add1 r) mr))))
        )
    (begin 
      (vector-map (λ (x) (begin (aux x 0 mar) (set! r# (add1 r#)))) alomat)
     mm)))



;ll->matrix
(define (ll->mat ll)
  (apply matrix (length (first ll)) (length ll) (apply append ll)))

;ll->matrix
(define (l->mat l)
  (apply matrix (length l) 1 l))

;matrix->ll
(define (mat->ll mat)
  (build-list (matrix-cols mat) (λ (m) (build-list (matrix-rows mat) (λ (n) (matrix-ref mat n m))))))

(define (mat->dir sqmat num)
  (local ((define mar (matrix-rows sqmat))
          (define mm (make-matrix mar 1 0))
          (define (aux r)
            (if (= r mar) mm
                (begin 
                  (matrix-set! mm r 0 (matrix-ref sqmat r num))
                       (aux (add1 r)))))
        )
    (aux 0)))

(define (dotp vec mec)
  (matrix-ref 
   (matrix-mul (matrix-transpose mec) vec)0 0))
;(dotp (matrix 3 1 9 2 3) (matrix 3 1 2 4 2))
;(dotp (l->mat '(1 2 3)) (l->mat '(1 2 3)))
;(mat->ll (mat->dir (ll->mat '((1 1 1)(2 2 2) (3 3 3))) 1))
;(mat->ll (mat->dir (ll->mat '((1 1 1)(2 2 2) (3 3 3))) 2))
#|
(mat->ll (l->mat '(1 1 0)))
(mat->ll (ll->mat '((1 1 0))))
(mat->ll (matrix-mul (ll->mat '((2 2 1)(1 0 2)(1 2 1))) (ll->mat '((2 0 1)(1 0 3)(1 0 1)))))
(mat->ll (matrix-mul (matrix-transpose (ll->mat '((2 2 1)(1 0 2)(1 2 1)))) (matrix-transpose (ll->mat '((2 0 1)(1 0 3)(1 0 1))))))
(matrix-ref (l->mat '(1 2 3)) 1 0)
(mat-l (matrix 1 2 4 3))
(mat->ll (mat-norm (matrix 1 2 1 -1) 1))|#
;(mat-dot (matrix 1 3 2 2 1))
;(mat->ll (matrix-append (list (l->mat '(1 2 3)) (l->mat '(4 5 6)))))
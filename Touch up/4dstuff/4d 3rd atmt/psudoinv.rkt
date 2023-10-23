#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "vec-mat.rkt")
(provide (all-defined-out))
;-D V1V0 V2V0 OV0 Gram–Schmidt process
(define (matrix-psudo-3c col1 col2 col3)
  (local ((define ref-cols-a (vector col1 col2 col3))
          (define ref-cols-e (vector ))
          (define l (matrix-rows col1))
          ;(define Q (make-matrix l 3 0))
          (define R (make-matrix 3 3 0))
          (define (QR-aux aref eref)
            (cond
              ((= aref eref) (matrix-scale (vector-ref ref-cols-a aref) 0)
                      )
              (else (let* ((E (vector-ref ref-cols-e eref))(dp (dotp (vector-ref ref-cols-a aref) E)))
                      (begin
                        (matrix-set! R eref aref dp)
                        (matrix-add (matrix-scale E dp) (QR-aux aref (add1 eref)))
                      )))
            ))
          (define (build-QR aref)
            (cond
              ((= 3 aref) #t)
              (else 
               (begin 
                 (set! ref-cols-e (vector-append ref-cols-e 
                                        (vector (mat-norm (matrix-sub (vector-ref ref-cols-a aref) (QR-aux aref 0))1))))
                 (matrix-set! R aref aref (dotp (vector-ref ref-cols-a aref) (vector-ref ref-cols-e aref)))
                 (build-QR (add1 aref))))
               ))
          )
    (build-QR 0)
    ;(print (mat->ll (matrix-inverse R)))
    ;(newline)
    ;(print (mat->ll (matrix-transpose (matrix-append ref-cols-e))))
    (matrix-mul (matrix-inverse R) (matrix-transpose (matrix-append ref-cols-e)))
    ))
;(mat->ll (matrix-psudo-3c (l->mat '(1 1 1 1)) (l->mat '(-1 4 4 -1)) (l->mat '(4 -2 2 0))))

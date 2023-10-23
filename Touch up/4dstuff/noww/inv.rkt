#lang racket
(require (planet wmfarr/plt-linalg:1:13/all))
;
(define (f64->mat f64 mat n mn)
  (cond
    ((= n mn) mat)
    (else (begin (matrix-set! mat n n (f64vector-ref f64 n))
                 (f64->mat f64 mat (add1 n) mn)))))

(define (disp-mat-aux mat m n mn)
  (cond 
    ((= n mn) null)
    (else (cons (matrix-ref mat m n) (disp-mat-aux mat m (add1 n) mn)))))

(define (disp-mat mat m)
  (cond
    ((= m (matrix-rows mat)) null)
    (else (cons (disp-mat-aux mat m 0 (matrix-cols mat)) (disp-mat mat (add1 m))))))

(disp-mat (matrix 2 2 1 0 0 1) 0)
(disp-mat (matrix-mul (matrix 2 2 1 0 0 1) (matrix 2 2 2 2 2 2)) 0)

(define (inv-eigen U S Vt)
  (matrix-mul Vt (matrix-mul (f64->mat S (make-matrix (matrix-rows Vt) (matrix-rows U) 0) 0 (f64vector-length S)) U)))
(disp-mat (inv-eigen (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30)))) 0)

(define (f64->matinv f64 mat n mn)
  (cond
    ((= n mn) mat)
    (else (let ((ns (f64vector-ref f64 n)))
                (if (< (abs ns) .1) (f64->matinv f64 mat (add1 n) mn)
                    (begin (matrix-set! mat n n (/ 1 (f64vector-ref f64 n)))
                 (f64->matinv f64 mat (add1 n) mn)))))))

(disp-mat (f64->matinv (f64vector (sqrt 12) (sqrt 10)) (make-matrix 3 2 0) 0 2) 0 )


(define (psudoinv U S Vt)
  (let* ((Ut (matrix-transpose U))
        (V (matrix-transpose Vt))
        (W (f64->matinv S (make-matrix (matrix-rows V) (matrix-rows Ut) 0) 0 (f64vector-length S))))
  (matrix-mul V (matrix-mul W Ut))))

(disp-mat (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30)))) 0)
"id"
(disp-mat (matrix-mul
           (matrix 2 3 3 -1 1 3 1 1)
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))) 0)
(disp-mat (matrix-mul
           
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))
           (matrix 2 3 3 -1 1 3 1 1)) 0)

(disp-mat (matrix-mul 
           (matrix-mul
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))
           (matrix 2 3 3 -1 1 3 1 1))
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))
          )
          0)
(disp-mat (matrix-mul 
           (matrix 2 3 3 -1 1 3 1 1)
           (matrix-mul
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))
           (matrix 2 3 3 -1 1 3 1 1))
           
          )
          0)
(disp-mat (matrix-mul 
           
           (matrix-mul
            (matrix 2 3 3 -1 1 3 1 1)
           (psudoinv (matrix 2 2 (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ 1 (sqrt 2)) (/ -1 (sqrt 2)))
                  (f64vector (sqrt 12) (sqrt 10))
                  (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30))
                    (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30))
                    (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))
           )
           (matrix 2 3 3 -1 1 3 1 1)
          )
          0)

(disp-mat (matrix-mul (matrix 3 2 1 2 3 4 5 6) (matrix 2 2 1 0 0 1))0)
(disp-mat (matrix-mul (matrix 3 2 1 2 3 4 5 6) (matrix 2 2 1.0 8.326672684688674e-17 8.326672684688674e-17 1.0))0)
"liner decomp"


"##"
(define (matrix-psudoinverse m)
  (local ((define mt (matrix-transpose m))
         (define I (matrix-mul m mt))
         (define O (matrix-mul mt m))
         (define-values (R1 I1 M1 U1) (eigensystem I))
         (define-values (R2 I2 M2 U2) (eigensystem O))
         (define diag (list->f64vector (map sqrt (f64vector->list R1))))
         (define A (begin (print (f64vector->list R1)) (newline) (print (f64vector->list R2)) (newline)))
         )
    (inv-eigen U2 diag U1)))
(disp-mat (matrix-transpose (matrix 3 3 (/ 1 (sqrt 6)) (/ 2 (sqrt 5)) (/ 1 (sqrt 30)) (/ 2 (sqrt 6)) (/ -1 (sqrt 5)) (/ 2 (sqrt 30)) (/ 1 (sqrt 6)) 0 (/ -5 (sqrt 30))))0)
(disp-mat (matrix-psudoinverse (matrix 3 2 3 -1 1 3 1 1)) 0)
(disp-mat (matrix 2 3 3 -1 1 3 1 1)0)

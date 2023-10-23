#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "opvec.rkt")
(require "ndpoly-op.rkt")
(require "worldop.rkt")
(provide (all-defined-out))

(define (cart->barac x y p1 p2 p3)
  (let*(
        (x1 (2dp-x p1))
        (y1 (2dp-y p1))
        (x2 (2dp-x p2))
        (y2 (2dp-y p2))
        (x3 (2dp-x p3))
        (y3 (2dp-y p3))
        (y2-y3 (- y2 y3))
        (x3-x2 (- x3 x2))
        (x1-x3 (- x1 x3))
        (y-y3 (- y y3))
        (x-x3 (- x x3))
        (det(+ (* y2-y3 x1-x3) (* x3-x2 (- y1 y3))))
        (la1(/ (+ (* y2-y3 x-x3) (* x3-x2 y-y3)) det))
        (la2(/ (+ (* (- y3 y1) x-x3) (* x1-x3 y-y3)) det))
        )
    (values la1 la2 (- 1 la1 la2))))

(define (in-tri? b1 b2 b3)
  (and (< b1 1.0001) (< b2 1.0001) (< b3 1.0001) (> b1 -0.0001) (> b2 -0.0001) (> b3 -0.0001)))

(define (triary-ref b1 b2 triary)
  (let((res (triarray-res triary)))
  (vector-ref (vector-ref triary (inexact->exact (round (* res b1))))
              (round (* res b2)))))

(define (get-dist b1 b2 b3 p1 p2 p3)
    (+ (* b1 (2dp-dist p1)) (* b2 (2dp-dist p2)) (* b3 (2dp-dist p3))))

(define (ref-triary triary xi yi)
  ;take first 2 (make 2 dim comp data) make max =res to be reft to neerest pixle(only 2 mostly)
  (let* ((res (triarray-res triary))
         (x (inexact->exact (truncate (- (* res xi) .01))))
         (y (inexact->exact (truncate (- (* res yi) .01))))
         ;(a (begin (print x) (newline) (print y) (newline)(newline)))
         )
    (vector-ref (vector-ref (triarray-vec triary) y) x)))


(define (get-pixl x y alotri accdist accbt)
  ;(print alotri)
  (cond 
    ((null? alotri) accbt)
    (else (local ((define tr (first alotri))
                  (define tp1 (2dtri-p1 tr))
                  (define tp2 (2dtri-p2 tr))
                  (define tp3 (2dtri-p3 tr))
                  (define-values (b1 b2 b3) (cart->barac x y tp1 tp2 tp3))
                  ;(define a (print (list b1 b2)))
                  ;(define b (newline))
                  )
            (if (in-tri? b1 b2 b3) 
                (let*((newdist (get-dist b1 b2 b3 tp1 tp2 tp3))
                      ;(A (begin (print b1)(newline)(print b2)(newline)(print x)(newline)(print y)(newline)(newline)))
                
                      )
                    (if (> accdist newdist)
                    (get-pixl x y (rest alotri) newdist (ref-triary (2dtri-triary tr) b1 b2))
                    (get-pixl x y (rest alotri) accdist accbt)))
                (get-pixl x y (rest alotri) accdist accbt))))))


(define (wxy->n x y w)
  (+ (* 4 x) (* y w 4)))

;bytestring of 4 (1pixl)
(define (add-pixl 4btst btstr acw w ach h)
  (let ((n (wxy->n acw ach w)))
    (begin
      ;(print acw )
      ;(newline)
      ;(print ach )
      ;(newline)
      ;(print w)
      ;(newline)
    (bytes-set! btstr n (bytes-ref 4btst 0))
    (bytes-set! btstr (+ n 1) (bytes-ref 4btst 1))
    (bytes-set! btstr (+ n 2) (bytes-ref 4btst 2))
    (bytes-set! btstr (+ n 3) (bytes-ref 4btst 3))
    btstr)))

         
         
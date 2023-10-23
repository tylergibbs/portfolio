#lang racket
(require 2htdp/universe)
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "ndpoly-rot.rkt")
(require "globalstruct.rkt")
(require "ndpoly-op.rkt")
(require "progwor.rkt")
(require "triop.rkt")
(require "worldop.rkt")
(require "opvec.rkt")
(provide (all-defined-out))

(define rel #f)
(define ref 1);0->

(define (keys wor str)
  (cond
    ((string=? str "o") (begin (set! rel #f) wor))
    ((string=? str "p") (begin (set! rel #t) wor))
    ((string->number str) (begin (set! ref (string->number str)) wor))
   (else (if rel
             (cond
    ((string=? str "q") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((0 0 1))) 1) 
                            (mov-ob-rel wor 2 1 (sub1 ref))))
    ((string=? str "e") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((0 0 1))) -1) 
                            (mov-ob-rel wor 2 -1 (sub1 ref))))
    ((string=? str "w") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((0 1 0))) 1) 
                            (mov-ob-rel wor 1 1 (sub1 ref))))
    ((string=? str "s") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((0 1 0))) -1)
                            (mov-ob-rel wor 1 -1 (sub1 ref))))
    ((string=? str "a") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((1 0 0))) 1)
                            (mov-ob-rel wor 0 1 (sub1 ref))))
    ((string=? str "d") (if (= 0 ref) (mov-cam-rel wor (ll->mat '((1 0 0))) -1)
                            (mov-ob-rel wor 0 -1 (sub1 ref))))
    ((string=? str "z") (if (= 0 ref) (rot-cam-rel wor 0 1 (/ pi 4)) 
                            (rot-ob-rel wor 0 1 (/ pi 4) (sub1 ref))))
    ((string=? str "x") (if (= 0 ref) (rot-cam-rel wor 0 2 (/ pi 4)) 
                            (rot-ob-rel wor 0 2 (/ pi 4) (sub1 ref))))
    ((string=? str "c") (if (= 0 ref) (rot-cam-rel wor 1 2 (/ pi 4))
                            (rot-ob-rel wor 0 3 (/ pi 4) (sub1 ref))))
    ((string=? str "v") (if (= 0 ref) (rot-cam-rel wor 0 1 (/ pi 4)) 
                            (rot-ob-rel wor 1 2 (/ pi 4) (sub1 ref))))
    ((string=? str "b") (if (= 0 ref) (rot-cam-rel wor 0 2 (/ pi 4)) 
                            (rot-ob-rel wor 1 3 (/ pi 4) (sub1 ref))))
    ((string=? str "n") (if (= 0 ref) (rot-cam-rel wor 1 2 (/ pi 4)) 
                            (rot-ob-rel wor 2 3 (/ pi 4) (sub1 ref))))
    (else wor)
    )
             (cond
    ((string=? str "q") (if (= 0 ref) (mov-cam-abs wor (ll->mat '((0 0 1))) 1) 
                            (mov-ob-abs wor (ll->mat '((0 0 1))) 1 (sub1 ref))))
    ((string=? str "e") (if (= 0 ref) (mov-cam-abs wor (ll->mat '((0 0 1))) -1) 
                            (mov-ob-abs wor (ll->mat '((0 0 1))) -1 (sub1 ref))))
    ((string=? str "w") (if (= 0 ref) 
                            (mov-cam-abs wor (ll->mat '((0 1 0))) 1) 
                            (mov-ob-abs wor (ll->mat '((0 1 0))) 1 (sub1 ref))))
    ((string=? str "s") (if (= 0 ref) (mov-cam-abs wor (ll->mat '((0 1 0))) -1)
                            (mov-ob-abs wor (ll->mat '((0 1 0))) -1 (sub1 ref))))
    ((string=? str "a") (if (= 0 ref) (mov-cam-abs wor (ll->mat '((1 0 0))) 1)
                            (mov-ob-abs wor (ll->mat '((1 0 0))) 1 (sub1 ref))))
    ((string=? str "d") (if (= 0 ref) (mov-cam-abs wor (ll->mat '((1 0 0))) -1)
                            (mov-ob-abs wor (ll->mat '((1 0 0))) -1 (sub1 ref))))
    ((string=? str "z") (if (= 0 ref) (rot-cam-abs wor 0 1 (/ pi 4)) 
                            (rot-ob-abs wor 0 1 (/ pi 4) (sub1 ref))))
    ((string=? str "x") (if (= 0 ref) (rot-cam-abs wor 0 2 (/ pi 4)) 
                            (rot-ob-abs wor 0 2 (/ pi 4) (sub1 ref))))
    ((string=? str "c") (if (= 0 ref) (rot-cam-abs wor 1 2 (/ pi 4)) 
                            (rot-ob-abs wor 0 3 (/ pi 4) (sub1 ref))))
    ((string=? str "v") (if (= 0 ref) (rot-cam-abs wor 0 1 (/ pi 4)) 
                            (rot-ob-abs wor 1 2 (/ pi 4) (sub1 ref))))
    ((string=? str "b") (if (= 0 ref) (rot-cam-abs wor 0 2 (/ pi 4)) 
                            (rot-ob-abs wor 1 3 (/ pi 4) (sub1 ref))))
    ((string=? str "n") (if (= 0 ref) (rot-cam-abs wor 1 2 (/ pi 4)) 
                            (rot-ob-abs wor 2 3 (/ pi 4) (sub1 ref))))
    (else wor)
    )))))


(define (change wor)
  wor)



;3d
;(define worl (world (cam-obj (l->mat '(0 0 0)) (matrix-identity 3))
 ;                   (vector
  ;                   (obj (ndpoly 
   ;                     (matrix 3 1 0 0 80) (matrix-identity 3) 
    ;                    (vector (l->mat '(-40 -40 40)) (l->mat '(40 -40 40)) (l->mat '(-40 40 40)) (l->mat '(40 40 40))
     ;                           (l->mat '(-40 -40 120)) (l->mat '(40 -40 120)) (l->mat '(-40 40 120)) (l->mat '(40 40 120)))
      ;               (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 1 2 3 (triarray 1 (vector (vector #"\100\100\100\100"))))
       ;                    (reftri 4 5 6 (triarray 1 (vector (vector #"\0\0\177\0")))) (reftri 5 6 7 (triarray 1 (vector (vector #"\0\0\177\0"))))
        ;                   (reftri 0 1 4 (triarray 1 (vector (vector #"\0\177\0\0")))) (reftri 1 4 5 (triarray 1 (vector (vector #"\0\177\0\0"))))
         ;                  (reftri 2 3 6 (triarray 1 (vector (vector #"\0\0\0\177")))) (reftri 3 6 7 (triarray 1 (vector (vector #"\0\0\0\177"))))
          ;                 (reftri 0 2 4 (triarray 1 (vector (vector #"\0\177\177\0")))) (reftri 2 4 6 (triarray 1 (vector (vector #"\0\177\177\0"))))
           ;                (reftri 1 3 5 (triarray 1 (vector (vector #"\0\177\0\177")))) (reftri 3 5 7 (triarray 1 (vector (vector #"\0\177\0\177"))))
            ;               )))
             ;        )))
(define worl (world (cam-obj (l->mat '(0 0 0 0)) (matrix-identity 4))
                    (vector
                     (obj (ndpoly 
                        (matrix 4 1 0 0 80 80) (matrix-identity 4) 
                        (vector (l->mat '(-40 -40 40 40)) (l->mat '(40 -40 40 40)) (l->mat '(-40 40 40 40)) (l->mat '(40 40 40 40))
                                (l->mat '(-40 -40 120 40)) (l->mat '(40 -40 120 40)) (l->mat '(-40 40 120 40)) (l->mat '(40 40 120 40))
                                
                                (l->mat '(-40 -40 40 120)) (l->mat '(40 -40 40 120)) (l->mat '(-40 40 40 120)) (l->mat '(40 40 40 120))
                                (l->mat '(-40 -40 120 120)) (l->mat '(40 -40 120 120)) (l->mat '(-40 40 120 120)) (l->mat '(40 40 120 120)))
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 1 2 3 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 0 4 5 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 5 1 0 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 2 6 7 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 7 3 2 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 1 5 7 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 7 3 1 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 0 4 6 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 6 2 0 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 4 5 6 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 5 6 7 (triarray 1 (vector (vector #"\100\100\100\100"))))
                          
                           ;low z to low z
                           (reftri 0 8 9 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 9 1 0 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 2 10 11 (triarray 1 (vector (vector #"\100\100\00\100")))) (reftri 11 3 2 (triarray 1 (vector (vector #"\100\100\00\100"))))
                           (reftri 0 8 10 (triarray 1 (vector (vector #"\100\00\00\100")))) (reftri 10 2 0 (triarray 1 (vector (vector #"\100\00\00\100"))))
                           (reftri 3 9 11 (triarray 1 (vector (vector #"\100\100\00\00")))) (reftri 9 3 1 (triarray 1 (vector (vector #"\100\100\00\00"))))
                          
                           ;high z to high z
                           (reftri 12 4 5 (triarray 1 (vector (vector #"\100\00\100\100")))) (reftri 5 13 12 (triarray 1 (vector (vector #"\100\00\100\100"))))
                           (reftri 14 6 7 (triarray 1 (vector (vector #"\100\00\100\100")))) (reftri 7 15 14 (triarray 1 (vector (vector #"\100\00\100\100"))))
                           (reftri 12 4 6 (triarray 1 (vector (vector #"\100\00\100\100")))) (reftri 6 14 12 (triarray 1 (vector (vector #"\100\00\100\100"))))
                           (reftri 13 5 7 (triarray 1 (vector (vector #"\100\00\100\100")))) (reftri 7 15 13 (triarray 1 (vector (vector #"\100\00\100\100"))))
                           
                           ;low y low y
                           (reftri 0 4 8 (triarray 1 (vector (vector #"\100\00\00\100")))) (reftri 8 12 4 (triarray 1 (vector (vector #"\100\00\00\100"))))
                           (reftri 1 5 9 (triarray 1 (vector (vector #"\100\00\00\100")))) (reftri 9 13 5 (triarray 1 (vector (vector #"\100\00\00\100"))))
                           
                           ;high y to high y
                           (reftri 2 6 10 (triarray 1 (vector (vector #"\100\00\100\00")))) (reftri 10 14 6 (triarray 1 (vector (vector #"\100\00\100\00"))))
                           (reftri 3 7 11 (triarray 1 (vector (vector #"\100\00\100\00")))) (reftri 11 15 7 (triarray 1 (vector (vector #"\100\00\100\00"))))
                           
                           (reftri 8 9 10 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 9 10 11 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           (reftri 8 12 13 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 13 9 8 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           (reftri 10 14 15 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 15 11 10 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           (reftri 9 13 15 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 15 11 9 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           (reftri 8 12 14 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 14 10 8 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           (reftri 12 13 14 (triarray 1 (vector (vector #"\100\100\100\00")))) (reftri 13 14 15 (triarray 1 (vector (vector #"\100\100\100\00"))))
                           
                           )))
                     )))

(define (big)
  (set! worl (change (change worl)))
  (place-scene worl))
;(place-scene worl)
(define (bang str)
  (set! worl (keys worl str))
  (place-scene worl))

(define (test)
  (let ((n (random 6)))
  (print (cond
                 ((= n 0) (bang "z"))
                 ((= n 1) (bang "x"))
                 ((= n 2) (bang "c"))
                 ((= n 3) (bang "v"))
                 ((= n 4) (bang "b"))
                 ((= n 5) (bang "n"))
                 )))
  (test))

(test)
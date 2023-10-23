#lang racket/gui
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "ndpoly-rot.rkt")
(require "globalstruct.rkt")
(require "ndpoly-op.rkt")
(require "progwor.rkt")
(require "triop.rkt")
(require "worldop.rkt")
(require "opvec.rkt")
(require "intface.rkt")
(define worl (world (cam-obj (l->mat '(0 0 0)) (matrix-identity 3))
                    (vector
                     (obj (ndpoly 
                        (matrix 3 1 0 0 80) (matrix-identity 3) 
                        (vector (l->mat '(-40 -40 40)) (l->mat '(40 -40 40)) (l->mat '(-40 40 40)) (l->mat '(40 40 40))
                                (l->mat '(-40 -40 120)) (l->mat '(40 -40 120)) (l->mat '(-40 40 120)) (l->mat '(40 40 120)))
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))) (reftri 1 2 3 (triarray 1 (vector (vector #"\100\100\100\100"))))
                           (reftri 4 5 6 (triarray 1 (vector (vector #"\0\0\177\0")))) (reftri 5 6 7 (triarray 1 (vector (vector #"\0\0\177\0"))))
                           (reftri 0 1 4 (triarray 1 (vector (vector #"\0\177\0\0")))) (reftri 1 4 5 (triarray 1 (vector (vector #"\0\177\0\0"))))
                           (reftri 2 3 6 (triarray 1 (vector (vector #"\0\0\0\177")))) (reftri 3 6 7 (triarray 1 (vector (vector #"\0\0\0\177"))))
                           (reftri 0 2 4 (triarray 1 (vector (vector #"\0\177\177\0")))) (reftri 2 4 6 (triarray 1 (vector (vector #"\0\177\177\0"))))
                           (reftri 1 3 5 (triarray 1 (vector (vector #"\0\177\0\177")))) (reftri 3 5 7 (triarray 1 (vector (vector #"\0\177\0\177"))))
                           )))
                     )))
(define frame (new frame%
                   [label "Hyper_Space"]
                   [width 120]
                   [height 100]))
(define cav%
  (class
        canvas%
    (define/override (on-char event)
      (let ((ch (send event get-key-code)))
        (if (char? ch)
      (begin (set! worl (keys worl (string ch))) (send this refresh-now))
      (void))))
    (define (tick)
      (begin 
        (set! worl (change worl))
        (send this refresh-now)))
    (public tick)
    (super-new)))
(define runing
  (new cav% [parent frame]
            
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap (place-scene worl) 0 0))]))

(send frame show #t)
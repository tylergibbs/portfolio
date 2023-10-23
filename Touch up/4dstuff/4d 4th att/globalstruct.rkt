#lang racket
;globalstruct
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "opvec.rkt")
(provide (all-defined-out))


(struct world (cam-ob avoob) #:transparent)
(struct cam-obj (p axis)#:transparent);will get vel/rotvel
(struct obj (ndpoly)#:transparent);will get vel/rotvel

(struct ndpoly (cp axis avop aloreftri rad avobp)#:transparent)
(struct reftri (rp1 rp2 rp3 triary)#:transparent)

(struct 2dtri (p1 p2 p3 triary)#:transparent)
(struct triarray (res vec)#:transparent)
(struct 2dp (x y dist)#:transparent)
 
(define (get-rad avop acc l acv)
  (if
    (= acc l) acv
    (let ((adist (disten (vector-ref avop acc) 0)))
      (if (> adist acv) (get-rad avop (add1 acc) l adist)
          (get-rad avop (add1 acc) l acv)))))
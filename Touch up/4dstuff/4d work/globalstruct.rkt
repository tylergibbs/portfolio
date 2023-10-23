#lang racket
;globalstruct
(require (planet wmfarr/plt-linalg:1:13/matrix))
(provide (all-defined-out))


(struct world (cam-ob avoob) #:transparent)
(struct cam-obj (p axis)#:transparent);will get vel/rotvel
(struct obj (ndpoly)#:transparent);will get vel/rotvel

(struct ndpoly (cp axis avop aloreftri)#:transparent)
(struct reftri (rp1 rp2 rp3 triary)#:transparent)

(struct 2dtri (p1 p2 p3 triary)#:transparent)
(struct triarray (res vec)#:transparent)
(struct 2dp (x y dist)#:transparent)
 
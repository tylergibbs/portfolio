#lang racket
;globalstruct
(require (planet wmfarr/plt-linalg:1:13/matrix))
(provide (all-defined-out))

;all points are help as 1bd matrixs

(struct world (cam-ob aloob) #:transparent)
(struct cam-obj (p axis vel rot-vel)#:transparent)
(struct obj (3dpoly vel rot-vel)#:transparent)

;avop cartetion
(struct 3dpoly (cp axis avop aloreftri)#:transparent)
(struct reftri (rp1 rp2 rp3 triary))

;cartiian cords '( 1 2 ...)
;barecentric '(1/2 1/2 0)
;cartition still
(struct tri (p1 p2 p3 triary) #:transparent)
(struct line (start vec) #:transparent);vec is normalised
(struct triarray (res vec)#:transparent)
;(define test-triary (triarray 4
;(vector #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1"
 ;       #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1" #"\1\1\1\1")));singletriangel(4by4 pixles sq 1 2 3 4 pixles pat)
                   ;is alwased to right triangle in 2d (eqilateral in 3d pint dropd border=hypotnece '(1 0 0) to (0 1 0)

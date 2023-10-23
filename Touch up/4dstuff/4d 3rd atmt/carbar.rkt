#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require (file "vec-mat.rkt"))
(require (file "psudoinv.rkt"))
(require "globalstruct.rkt")
(require "pixle-ref.rkt")
(provide (all-defined-out))

;cartiian cords '( 1 2 ...)
;barecentric '(1/2 1/2 0)
;
;(struct tri (p1 p2 p3) #:transparent)
;(struct line (start vec) #:transparent);vec is normalised

;first return is distence on line sencond are u v
(define (cartetian->barecentric tr lin)
  ;[t u v]
  (let ((V1 (tri-p1 tr));reversed to make uv by 12 not 23
        (V2 (tri-p2 tr))
        (V0 (tri-p3 tr))
        (O  (line-start lin))
        (-D (matrix-scale (line-vec lin) -1)))
  (matrix-mul (matrix-psudo-3c -D (matrix-sub V1 V0) (matrix-sub V2 V0)) (matrix-sub O V0))))

;(mat->ll (cartetian->barecentric (tri (matrix 3 1 1 0 1) (matrix 3 1 0 0 1) (matrix 3 1 0 1 1) test-triary) (line (matrix 3 1 0 0 0) (matrix 3 1 0 0 1))))
;(mat->ll (cartetian->barecentric (tri (matrix 3 1 0 0 5) (matrix 3 1 1 0 5) (matrix 3 1 0 1 5) test-triary) (line (matrix 3 1 0 0 0) (matrix 3 1 0 0 1))));1 is in 2nd corisponding to first
;(mat->ll (cartetian->barecentric (tri (matrix 3 1 0 0 5) (matrix 3 1 1 0 5) (matrix 3 1 0 1 5) test-triary) (line (matrix 3 1 1 0 0) (matrix 3 1 0 0 1))));1 is in third so cor to sec
;(mat->ll (cartetian->barecentric (tri (matrix 3 1 0 0 5) (matrix 3 1 1 0 5) (matrix 3 1 0 1 5) test-triary) (line (matrix 3 1 0 1 0) (matrix 3 1 0 0 1))));no 1 so 1 is in third(dirivable by append (1-u-v)

;(mat->ll (cartetian->barecentric (tri (matrix 4 1 1 0 1 1) (matrix 4 1 0 0 1 1) (matrix 4 1 0 1 1 1) test-triary) (line (matrix 4 1 0 0 0 0) (mat-norm (matrix 4 1 0 0 1 1) 1))))

(define (val-bar? mat)
  (let* ((1st(matrix-ref mat 0 0))
        (2nd(matrix-ref mat 1 0))
        (3rd(matrix-ref mat 2 0))
        (4th (- 1 2nd 3rd))
        )
  (and (>= 1st -0.001)
       (>= 2nd -0.001) (>= 3rd -0.001) (>= 4th -0.001)
       (<= 2nd 1.001) (<= 3rd 1.001) (<= 4th 1.001))))

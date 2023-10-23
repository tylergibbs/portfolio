#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "opvec.rkt")
(require "triop.rkt")
(require "ndpoly-op.rkt")
(require "worldop.rkt")
(require "ndpoly-rot.rkt")

(require racket/draw)
(provide (all-defined-out))

(define (d-1 ixp c di dd)
  (local((define vec (make-matrix dd 1 0))
         (define di-1 (sub1 di))
         (define (aux d)
           (if (= d di) (void) 
               (void (matrix-set! vec d 0 (* -1 (matrix-ref ixp d 0))) (aux (add1 d)))
               ))
         ;(define e(print (mat->ll ixp)))
         ;(define e1(print (mat->ll vec)))
         (define chvec1(aux 0))
         (define chvec2(matrix-set! vec di-1 0 (+ (matrix-ref ixp di-1 0) c)))
         ;(define e2(print (mat->ll vec)))
         (define n (/ (matrix-ref ixp di-1 0) (matrix-ref vec di-1 0)))
         (define proj (matrix-add ixp (matrix-scale vec n)))
         ;(define e3 (print n))
         ;(define e4 (print(mat->ll proj)))
         ;(define e5 (newline))
         )
    proj))
         

(define (d=2 ixp c di dd)
  (cond
    ((= 2 di) (values (matrix-ref ixp 0 0) (matrix-ref ixp 1 0)))
    (else (d=2 (d-1 ixp c di dd) c (- di 1) dd))))

(define (find-dist ixp di)
  (local ((define (aux n)
            (if (= n di) 0
            (+ (sqr (matrix-ref ixp n 0)) (aux (add1 n))))))
    (sqrt (aux 0))))

;2dpoint
(define (prog ixp c width hight di)
  (local ((define-values (x y) (d=2 ixp c di di))
          (define dist (find-dist ixp di))
          ;(define a (print (2dp (+ x (/ width 2)) (+ y (/ hight 2)) dist)))
          )
    (2dp x y dist)))

;(prog (l->mat '(1 1 2000)) -5 50 50 3)
;(prog (l->mat '(1 1 20 2)) -50 50 50 4)

(define (valp 2dpt width hight)
  (let* ((x (2dp-x 2dpt))
        (y (2dp-y 2dpt))
        ) 
  (and (>= x (/ width -2)) (<= x (/ width 2)) (>= y (/ hight -2))(<= y (/ hight 2)))))

(define (psrt->2dtris aloreftris ndpoints di)
  (if (null? aloreftris) null
    (let* ((c -21)
          (x (first aloreftris))
          (p1 (prog
              (vector-ref ndpoints (reftri-rp1 x)) c 100 100 di))
          (p2 (prog
              (vector-ref ndpoints (reftri-rp2 x)) c 100 100 di)) 
          (p3 (prog
              (vector-ref ndpoints (reftri-rp3 x)) c 100 100 di))
          ;(pp(begin (print p1) (print p2) (print p3)))
           )
      (if (or (valp p1 100 100) (valp p2 100 100) (valp p3 100 100))
          (cons (2dtri p1 p2 p3 (reftri-triary x)) (psrt->2dtris (rest aloreftris) ndpoints di))
          (psrt->2dtris (rest aloreftris) ndpoints di)))))

(define (get-2dtris avob ref l p invrotax di)
 (if (= l ref) null
     (let* ((ndp (obj-ndpoly(vector-ref avob ref)))
            ;(z (print ndp))
            (ndpoints (vector-map (λ (x) (matrix-mul invrotax (matrix-sub x p))) (ndpoly-avop ndp)))
            )
     (append (psrt->2dtris (ndpoly-aloreftri ndp) ndpoints di)
             (get-2dtris avob (add1 ref) l p invrotax di)))))

;###



(define (proj-tri-byt btstr acx ax/2 acy ay/2 alotri di)
  ;(print acx)
  ;(newline)
  ;(print acy)
  ;(newline)
  ;(newline)
  (cond
    ((= ay/2 acy) btstr)
    ((= ax/2 acx) (proj-tri-byt btstr (* -1  ax/2) ax/2 (add1 acy) ay/2 alotri di))
    (else (let* ((pixl (get-pixl acx acy alotri +inf.0 #f))
                )
            (if pixl (proj-tri-byt (add-pixl pixl btstr (+ acx ax/2) (* 2 ax/2) (+ ay/2 acy) (* 2 ay/2)) (add1 acx) ax/2 acy ay/2 alotri di)
                (proj-tri-byt btstr (add1 acx) ax/2 acy ay/2 alotri di))))))



;camera put interms to accoplish inv rot
(define (get-bytes btstr cam avob w h)
  (let* ((axis (cam-obj-axis cam))
        (p (cam-obj-p cam))
        (di (matrix-rows p))
        ;(o (print avob))
        (trs(get-2dtris avob 0 (vector-length avob) p (matrix-inverse axis) di))
        ;(z (print trs))
        )
  (proj-tri-byt btstr -50 50 -50 50 trs di)))

(define (place-scene wor)
  (local ((define bm(make-object bitmap% 100 100))
          (define byt (get-bytes (make-bytes (* 4 100 100) 0) (world-cam-ob wor) (world-avoob wor) 100 100))
          ;
          (define A (send bm set-argb-pixels 0 0 100 100 byt))
          )
    bm))

;(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (matrix-identity 3))
 ;                   (vector
  ;                   (obj (ndpoly (matrix 3 1 5 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(100 0 20)) (l->mat '(0 100 20))) 
   ;                  (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
    ;                 ))))
#|
(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (build-rot-mat 0 1 (/ pi 4) 3))
                    (vector
                     (obj (ndpoly (matrix 3 1 5 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(100 0 20)) (l->mat '(0 100 20))) 
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
                     ))))
(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (build-rot-mat 0 2 (/ pi 4) 3))
                    (vector
                     (obj (ndpoly (matrix 3 1 5 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(100 0 20)) (l->mat '(0 100 20))) 
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
                     ))))
|#
#|
(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (matrix-identity 3))
                    (vector
                     (obj (ndpoly (matrix 3 1 0 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(-10 0 20)) (l->mat '(0 10 20))) 
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
                     ))))
(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (matrix-identity 3))
                    (vector
                     (obj (ndpoly (matrix 3 1 0 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(10 0 20)) (l->mat '(0 -10 20))) 
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
                     ))))
(time (place-scene (world (cam-obj (matrix 3 1 0 0 0) (matrix-identity 3))
                    (vector
                     (obj (ndpoly (matrix 3 1 0 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(10 0 20)) (l->mat '(0 10 20))) 
                     (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
                     ))))
;(time (place-scene (world (cam-obj (matrix 3 1 18 18 0) (matrix-identity 3))
 ;                   (vector
  ;                  (obj (ndpoly (matrix 3 1 0 0 5) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(-10 0 20)) (l->mat '(0 -10 20))) 
   ;                  (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
    ;                 ))))|#
;(time (place-scene (world (cam-obj (matrix 3 1 0 0 10) (matrix-identity 3))
 ;                   (vector
  ;                   (obj (ndpoly (matrix 3 1 0 0 20) (matrix-identity 3) (vector (l->mat '(0 0 20)) (l->mat '(100 0 20)) (l->mat '(0 100 20))) 
   ;                  (list (reftri 0 1 2 (triarray 1 (vector (vector #"\100\100\100\100")))))))
    ;                 ))))

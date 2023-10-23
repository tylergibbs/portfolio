#lang racket
;globalstruct
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "opvec.rkt")
(require "ndpoly-rot.rkt")
(require rnrs/arithmetic/bitwise-6)
(provide (all-defined-out))


(struct world (cam-ob avoob) #:transparent)
(struct cam-obj (p axis)#:transparent);will get vel/rotvel
(struct obj (ndpoly)#:transparent);will get vel/rotvel
;siplexes are used to rough outline shap
(struct ndpoly (cp axis avop aloreftri avorefsimplex rad avobp)#:transparent)

(define (make-ndpoly axis avop aloreftri avorefsimplex)
  (let* ((li (vector->list avop))
         (l (vector-length avop))
        (cp (matrix-scale (foldl (λ (x y) (matrix-add x y)) (first li) (rest li)) (/ 1 l)))
        (ma (make-matrix (matrix-rows cp) (matrix-cols cp) 0)))
  (ndpoly cp axis (vector-map (λ (x) (matrix-sub x cp))avop) aloreftri 
          avorefsimplex
          (get-rad avop 0 l 0)
          (vector  (l->mat '(0 0 0 0)))
          ;(get-avobp avop 0 l ma ma)
          )))

(struct reftri (rp1 rp2 rp3 triary)#:transparent)
(struct refsimplex (avorp avoadjsmp))
(struct simplex (avop avoadjsmp))


(struct 2dtri (p1 p2 p3 triary)#:transparent)
(struct triarray (res vec)#:transparent)
(struct 2dp (x y dist)#:transparent)
 
(define (get-rad avop acc l acv)
  (if
    (= acc l) acv
    (let* ((adist (find-dist (vector-ref avop acc) (matrix-rows (vector-ref avop 0)))))
      (if (> adist acv) (get-rad avop (add1 acc) l adist)
          (get-rad avop (add1 acc) l acv)))))
;(get-rad (vector (l->mat '(3 1)) (l->mat '(2 1)) (l->mat '(7 6))) 0 3 0)


(define (get-avobp avop cp)
  '(0 0 0 0)
  ;  (get-angle (vector-map (λ (x) (matrix-sub x cp)) avop))
  )
;(gt-m (vector 1 2 -1 0) (matrix 1 4 2 1 0 1) min)
#|
(define (get-angle avop)
  (local ((define th1 (get-ang avop :number:))
         (define-values (bps1 bpl1) (get-bp-theta avop th1))
         (define sz1 (get-size bps1 bpl1 0))
         (define th2 (get-ang avop :number:))
         (define-values (bps2 bpl2)  (get-bp-theta avop th2))
         (define sz2 (get-size bps2 bpl2 0))
         (define th3 (get-ang avop :number:))
         (define-values (bps3 bpl3)  (get-bp-theta avop th3))
         (define sz3 (get-size bps3 bpl3 0))
         (define th4 (get-ang avop :number:))
         (define-values (bps4 bpl4)  (get-bp-theta avop th4))
         (define sz4 (get-size bps4 bpl4 0)))
    (if (and (< sz1 sz2) (< sz1 sz3) (< sz1 sz4))
        bps1 bpl1
        (if (and (< sz2 sz1) (< sz2 sz3) (< sz2 sz4))
            bps2 bpl2
            (if (and (< sz3 sz1) (< sz3 sz2) (< sz3 sz4))
                bps3 bpl3
                (if (and (< sz4 sz1) (< sz4 sz2) (< sz4 sz3))
                    bps4 bpl4
                    ))))))

(define (get-size bps bpl n)
  (if
   (= (length bps) n) 1
   (* (- (vector-ref bps n) (vector-ref bpl)) (get-size bps bpl (add1 n)))))

(define (get-bp-theta avop  th1)
  (let ((l (matrix-rows (vector-ref avop))))
  (intplt (make-vector 0 l) 
          (make-vector 0 l)
          avop
          dir-mat
          (build-rot-mat 

;V+ a vector + V- a vector -
(define (intplt v+ v- avop axis ang-mat n)
  (if
    (= n (length avop)) (values v+ v-)
    (local ((define np (matrix-rot (vector-ref avop n) axis ang-mat)))
      (intplt (gt-m v+ np max) (gt-m v- min) avop axis ang (add1 n)))))

(define (gt-m vec mat f)
  (gt-man vec mat f 0 (make-vector (vector-length vec) 0)))
(define (gt-man vec mat f n nv)
  (if (= (vector-length vec) n) nv
      (gt-man vec mat f (add1 n) 
              (begin (vector-set! nv n (f (vector-ref vec n) (matrix-ref mat 0 n))) nv))))|#
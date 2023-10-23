#lang racket

(define (rad-col? ob ob2)
  (let ((nd(obj-ndpoly ob))(nd2(obj-ndpoly ob2)))
  (<= (find-dist (ndpoly-cp nd) (ndpoly-cp nd2) 
                     (matrix-rows(ndpoly-cp nd)))
      (+ (ndpoly-rad nd) (ndpoly-rad nd2)))))

(define (bound-col? ob ob2)
  (let ((bp(ndpoly-avobp(obj-ndpoly ob)))(bp2(ndpoly-avobp(obj-ndpoly ob2))))
    
    
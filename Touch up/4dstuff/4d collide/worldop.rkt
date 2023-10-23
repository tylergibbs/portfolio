#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "opvec.rkt")
(require "ndpoly-rot.rkt")
(require "ndpoly-op.rkt")
(provide (all-defined-out))

(define (edit-sig-vec f v ref)
  (vector-set! v ref (f (vector-ref v ref)))
  v)

(define (rot-cam-abs wor or1 or2 deg)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (cam-obj-p cam) (matrix-mul (build-rot-mat or1 or2 deg (matrix-rows (cam-obj-p cam))) (cam-obj-axis cam)))
         (world-avoob wor))))

(define (mov-cam-abs wor dir amt)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (movndpoly (cam-obj-p cam) dir amt) (cam-obj-axis cam))
         (world-avoob wor))))
(define (rot-cam-rel wor or1 or2 deg)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (cam-obj-p cam) (rot-axis (cam-obj-axis cam) (build-rot-mat or1 or2 deg (matrix-rows (cam-obj-p cam)))))
         (world-avoob wor))))
(define (mov-cam-rel wor axn amt)  
  (let* ((cam (world-cam-ob wor))
        (ax (cam-obj-axis cam)))
  (world (cam-obj (movndpoly (cam-obj-p cam) (mat->dir ax axn) amt) ax)
         (world-avoob wor))))

(define (rot-ob-abs wor or1 or2 deg ref)
  (world (world-cam-ob wor) (edit-sig-vec (λ (x) (obj (rotndpoly-abs (obj-ndpoly x) or1 or2 deg))) (world-avoob wor) ref)))
  
(define (mov-ob-abs wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-vec (λ (x) (obj (movndpoly (obj-ndpoly x) dir amt))) (world-avoob wor) ref)))

(define (rot-ob-rel wor or1 or2 deg ref)
  (world (world-cam-ob wor) (edit-sig-vec (λ (x) (obj (rotndpoly (obj-ndpoly x) or1 or2 deg))) (world-avoob wor) ref)))
  
(define (mov-ob-rel wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-vec (λ (x) (obj (movndpoly (obj-ndpoly x) (mat->dir (ndpoly-axis (obj-ndpoly x)) dir) amt))) (world-avoob wor) ref)))

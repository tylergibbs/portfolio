#lang racket
(require (planet wmfarr/plt-linalg:1:13/matrix))
(require "globalstruct.rkt")
(require "vec-mat.rkt")
(require "rot3dpoly.rkt")
(require "3dpolyop.rkt")
(provide (all-defined-out))

;"once"
(define (edit-sig-list f l ref)
  (cond
    ((null? l) null)
    ((= ref 0) (cons (f (first l)) (edit-sig-list f (rest l) (sub1 ref))))
    (else (cons (first l) (edit-sig-list f (rest l) (sub1 ref))))))


(define (rot-cam-abs wor or1 or2 deg)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (cam-obj-p cam) (matrix-mul (build-rot-mat or1 or2 deg (matrix-rows (cam-obj-p cam))) (cam-obj-axis cam)) (cam-obj-vel cam) (cam-obj-rot-vel cam))
         (world-aloob wor))))
  
(define (mov-cam-abs wor dir amt)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (mov3dpoly (cam-obj-p cam) dir amt) (cam-obj-axis cam) (cam-obj-vel cam) (cam-obj-rot-vel cam))
         (world-aloob wor))))

(define (rot-ob-abs wor or1 or2 deg ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (rot3dpoly-abs (obj-3dpoly x) or1 or2 deg) (obj-vel x) (obj-rot-vel x) )) (world-aloob wor) ref)))
  
(define (mov-ob-abs wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (mov3dpoly (obj-3dpoly x) dir amt) (obj-vel x) (obj-rot-vel x) )) (world-aloob wor) ref)))


(define (rot-cam-rel wor or1 or2 deg)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (cam-obj-p cam) (rot-axis (cam-obj-axis cam) (build-rot-mat or1 or2 deg (matrix-rows (cam-obj-p cam)))) (cam-obj-vel cam) (cam-obj-rot-vel cam))
         (world-aloob wor))))
  
(define (mov-cam-rel wor axn amt)  
  (let* ((cam (world-cam-ob wor))
        (ax (cam-obj-axis cam)))
  (world (cam-obj (mov3dpoly (cam-obj-p cam) (mat->dir ax axn) amt) ax (cam-obj-vel cam) (cam-obj-rot-vel cam))
         (world-aloob wor))))

(define (rot-ob-rel wor or1 or2 deg ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (rot3dpoly (obj-3dpoly x) or1 or2 deg) (obj-vel x) (obj-rot-vel x))) (world-aloob wor) ref)))
  
(define (mov-ob-rel wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (mov3dpoly (obj-3dpoly x) (mat->dir (3dpoly-axis (obj-3dpoly x)) dir) amt) (obj-vel x) (obj-rot-vel x))) (world-aloob wor) ref)))

;"continuse"

(define (mov-cam-con wor amt)  
  (let* ((cam (world-cam-ob wor))
        (ax (cam-obj-axis cam)))
  (world (cam-obj (matrix-add (cam-obj-p cam) (matrix-scale (cam-obj-vel cam) amt)) ax (cam-obj-vel cam) (cam-obj-rot-vel cam))
         (map (λ (x) (obj (mov3dpoly (obj-3dpoly x) (obj-vel x) amt) (obj-vel x) (obj-rot-vel x))) (world-aloob wor)))))
  
;"edit"

(define (mov-cam-abs-ed wor dir amt)
  (let ((cam (world-cam-ob wor)))
  (world (cam-obj (cam-obj-p cam) (cam-obj-axis cam) (matrix-add (cam-obj-vel cam)  (matrix-scale dir amt)) (cam-obj-rot-vel cam))
         (world-aloob wor))))

(define (mov-ob-abs-ed wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (obj-3dpoly x) (matrix-add (obj-vel x) (matrix-scale dir amt)) (obj-rot-vel x) )) (world-aloob wor) ref)))


(define (mov-cam-rel-ed wor axn amt)  
  (let* ((cam (world-cam-ob wor))
        (ax (cam-obj-axis cam)))
  (world (cam-obj (cam-obj-p cam) ax (matrix-add (cam-obj-vel cam) (matrix-scale (mat->dir ax axn) amt)) (cam-obj-rot-vel cam))
         (world-aloob wor))))

(define (mov-ob-rel-ed wor dir amt ref)
  (world (world-cam-ob wor) (edit-sig-list (λ (x) (obj (obj-3dpoly x) (matrix-add (obj-vel x) (matrix-scale (mat->dir (3dpoly-axis (obj-3dpoly x)) dir) amt)) (obj-rot-vel x))) (world-aloob wor) ref)))


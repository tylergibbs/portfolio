#lang s-exp syntax/module-reader
(planet crewman_51/problem1/lang/language)
#:read red
#:read-syntax red-stx
(require "../parse.rkt")


(define (red in)
  (syntax->datum (red-stx #f in)))

(define (red-stx src in)
  (parse-expr src in))

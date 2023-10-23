#lang racket
(require "semantics.rkt")
(provide greater-than
         less-than
         plus
         minus
         period
         com
         brak
         (rename-out [my-module-begin #%module-begin]))
(define stat (make-parameter (new-state)))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
    (parameterize ([stat (new-state)])
       body ...)))

(define-syntax-rule (greater-than)
  (inc (stat)))
(define-syntax-rule (less-than)
  (dec (stat)))
(define-syntax-rule (minus)
  (sb (stat)))
(define-syntax-rule (plus)
  (ad (stat)))
(define-syntax-rule (period)
  (writ (stat)))
(define-syntax-rule (com)
  (red (stat)))

(define-syntax-rule (brak body ...)
  (loop (stat) body ...))

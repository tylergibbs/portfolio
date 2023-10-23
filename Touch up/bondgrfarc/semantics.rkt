#lang racket
(require "sym-math.rkt")
(struct junct (R C P alojc sames adds))

(struct arrow (a b v n))

(define (sym+n sym n)
  (if (symbol? sym)
  (string->symbol (string-append (symbol->string sym) "_" (number->string n)))
  sym))

(define (arrow->eq arw n)
  (equ (sym+n (arrow-a arw) n) (mull (sym+n (arrow-v arw) n) (sym+n (arrow-b arw) n))))


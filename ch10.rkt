#lang racket

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 4
(define (var name) (vector name))
(define (var? x) (vector? x))

; 5
(define u (var 'u))
(define v (var 'v))
(define w (var 'w))

(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

; 11
(define empty-s '())

(run-tests
 (test-suite "chapter 10"
  (test-equal? "6" (cdr `(,z . a)) 'a)
  (test-equal? "7" (cdr `(,z . b)) 'b)
  (test-equal? "8" (cdr `(,z . (,x e ,y))) `(,x e ,y))
  (test-equal? "10" `((,x . ,z)) '((#(x) . #(z))))
 ))
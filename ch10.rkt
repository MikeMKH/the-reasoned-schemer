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

; 18
(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
  (cond
    ((pair? a) (walk (cdr a) s))
    (else v))))

(run-tests
 (test-suite "chapter 10"
  (test-equal? "6" (cdr `(,z . a)) 'a)
  (test-equal? "7" (cdr `(,z . b)) 'b)
  (test-equal? "8" (cdr `(,z . (,x e ,y))) `(,x e ,y))
  (test-equal? "10" `((,x . ,z)) '((#(x) . #(z))))
  (test-equal? "13" (walk z `((,z . a) (,x . ,w) (,y . ,z))) 'a)
  (test-equal? "14" (walk y `((,z . a) (,x . ,w) (,y . ,z))) 'a)
  (test-equal? "15" (walk x `((,z . a) (,x . ,w) (,y . ,z))) w)
  (test-equal? "16 x" (walk x `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "16 v" (walk v `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "16 w" (walk w `((,x . ,y) (,v . ,x) (,w . ,x))) y)
  (test-equal? "17" (walk w `((,x . b) (,z . ,y) (,w . (,x e ,z)))) `(,x e ,z))
 ))
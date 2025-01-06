#lang racket
(require cKanren/miniKanren)

(define s (== #t #t))
(define u (== #t #f))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(run-tests
 (test-suite "chapter 1"
  (test-equal? "7" (run* (q) u) '())
  (test-equal? "10" (run* (q) (== 'pea 'pod)) '())
  (test-equal? "11" (run* (q) (== q 'pea)) '(pea))
  (test-equal? "12" (run* (q) (== 'pea q)) '(pea))
  (test-equal? "16" (run* (q) s) '(_.0)) ; q is still fresh
  (test-equal? "19" (run* (q) (== 'pea 'pod)) '()) ; q is still fresh
  (test-equal? "20" (run* (q) (== q q)) '(_.0)) ; q is still fresh
  (test-equal? "21" (run* (q) (fresh (x) (== 'pea q))) '(pea)) ; x is still fresh
  (test-equal? "24" (run* (q) (fresh (x) (== 'pea x))) '(_.0)) ; q is still fresh
  (test-equal? "25" (run* (q) (fresh (x) (== (cons x '()) q))) '((_.0))) ; x is still fresh
  (test-equal? "32" (run* (q) (== '(((pea)) pod) '(((pea)) pod))) '(_.0)) ; q is still fresh
  (test-equal? "33" (run* (q) (== '(((pea)) pod) '(((pea)) ,q))) '())
  (test-equal? "34" (run* (q) (== '(((,q)) pod) '(((pea)) pod))) '())
  (test-equal? "35" (run* (q) (== '(((,q)) pod) '(((,x)) pod))) '())
  (test-equal? "36" (run* (q) (== '(((,q)) ,x) '(((,x)) pod))) '())
  (test-equal? "37" (run* (q) (fresh (x) (== '(,x ,x) 'q))) '())
  (test-equal? "38" (run* (q) (fresh (x) (fresh (y) (== '(,q ,y) '((,x ,y) ,x))))) '())
 ))
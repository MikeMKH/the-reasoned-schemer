#lang racket
(require cKanren/miniKanren)

(define s (== #t #t))
(define u (== #t #f))

(require (prefix-in : rackunit))
(require rackunit/text-ui)

(run-tests
 (:test-suite "chapter 1"
  (:test-equal? "7" (run* (q) u) '())
  (:test-equal? "10" (run* (q) (== 'pea 'pod)) '())
  (:test-equal? "11" (run* (q) (== q 'pea)) '(pea))
 ))
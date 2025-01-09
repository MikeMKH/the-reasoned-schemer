#lang racket
(require cKanren/miniKanren)

(define s (== #t #t))
(define u (== #t #f))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(run-tests
 (test-suite "chapter 2"
  (test-equal? "hello testing" (run* (q) u) '())
 ))
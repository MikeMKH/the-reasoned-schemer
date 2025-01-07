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

  (test-equal? "50" (run* (q) (conj s s)) '(_.0))
  (test-equal? "51" (run* (q) (conj s (== 'corn q))) '(corn))
  (test-equal? "52" (run* (q) (conj u (== 'corn q))) '())
  (test-equal? "53" (run* (q) (conj (== 'corn q) (== 'meal q))) '())
  (test-equal? "54" (run* (q) (conj (== 'corn q) (== 'corn q))) '(corn))

  (test-equal? "55" (run* (q) (disj u u)) '())
  (test-equal? "56" (run* (q) (disj (== 'olive q) u)) '(olive))
  (test-equal? "57" (run* (q) (disj u (== 'oil q))) '(oil))
  (test-equal? "58" (run* (q) (disj (== 'olive q) (== 'oil q))) '(olive oil))
  (test-equal? "59" (run* (q) (fresh (x) (fresh (y) (disj (== '(,x ,y) q) (== '(,y ,x) q))))) '((,x ,y) (,y ,x))) ; not what I expected
  (test-equal? "61" (run* (q) (disj (== 'olive q) (== 'oil q))) (run* (q) (disj (== 'olive q) (== 'oil q)))) ; order matters to test-equal?
 ))
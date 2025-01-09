#lang racket
(require cKanren/miniKanren)

(define s (== #t #t))
(define u (== #t #f))

; https://github.com/rymaju/mykanren/blob/c56318625319b598c0e05cf993e8aafabee83187/mykanren.rkt#L360C1-L366C34
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(defrel (teacup t) (disj (== 'tea t) (== 'cup t)))

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
  (test-equal? "59" (run* (q) (fresh (x) (fresh (y) (disj (== (cons x y) q) (== (cons y x) q))))) '((_.0 . _.1) (_.0 . _.1))) ; not what I expected
  (test-equal? "61" (run* (q) (disj (== 'olive q) (== 'oil q))) (run* (q) (disj (== 'olive q) (== 'oil q)))) ; order matters to test-equal?

  (test-equal? "62" (run* (x) (disj (conj (== 'olive x) u) (== 'oil x))) '(oil))
  (test-equal? "63" (run* (x) (disj (conj (== 'olive x) s) (== 'oil x))) '(oil olive)) ; I expected '(olive oil) must be something with cKanren/miniKanren
  (test-equal? "64" (run* (x) (disj (== 'olive x) (conj (== 'oil x) s))) '(olive oil)) ; different than book, must be something with cKanren/miniKanren
  (test-equal? "65" (run* (x) (disj (conj (== 'virgin x) u) (disj (== 'olive x) (disj s (== 'oil x))))) '(_.0 olive oil)) ; again different than book

  (test-equal? "67" (run* (r) (fresh (x) (fresh (y) (conj (== 'split x) (conj (== 'pea y) (== (cons x y) r)))))) '((split . pea)))
  (test-equal? "70" (run* (r) (fresh (x y) (conj (== 'split x) (conj (== 'pea y) (== (cons x y) r))))) '((split . pea)))
  (test-equal? "72" (run* (r x y) (conj (conj (== 'split x) (== 'pea y)) (== (cons x y) r))) '(((split . pea) split pea))) ; ((r1 x1 y1) ...)
  (test-equal? "75" (run* (x y) (conj (== 'split x) (== 'pea y))) '((split pea)))
  (test-equal? "76" (run* (x y) (disj (conj (== 'split x) (== 'pea y)) (conj (== 'red x) (== 'bean y)))) '((split pea) (red bean)))
  (test-equal? "80" (run* (x y z) (disj (conj (== 'split x) (== 'pea y)) (conj (== 'red x) (== 'bean y))) (== 'soup z)) '((split pea soup) (red bean soup)))
  (test-equal? "81" (run* (x y) (== 'split x) (== 'pea y)) '((split pea)))

  ; teacup is defrel outside of run-tests
  (test-equal? "83" (run* (x) (teacup x)) '(tea cup))
  (test-equal? "85" (run* (x y) (teacup x) (teacup y)) '((tea tea) (cup tea) (tea cup) (cup cup)))
  (test-equal? "86" (run* (x y) (teacup x) (teacup x)) '((tea _.0) (cup _.0)))
  (test-equal? "87" (run* (x y) (disj (conj (teacup x) (teacup x)) (conj (== #f x) (teacup y)))) '((tea _.0) (cup _.0) (#f tea) (#f cup)))

  (test-equal? "88" (run* (x y) (conde ((== 'split x) (== 'pea y)) ((== 'red x) (== 'bean y)))) '((split pea) (red bean)))
  (test-equal? "89" (run* (x) (conde ((== 'olive x) u) ((== 'oil x)))) '(oil))
  (test-equal? "90" (run* (x y) (conde ((fresh (z) (== 'lentil z))) ((== x y)))) '((_.0 _.0) (_.0 _.1)))
  (test-equal? "91" (run* (x y) (conde ((== 'split x) (== 'pea y)) ((== 'red x) (== 'bean y)) ((== 'green x) (== 'lentil y)))) '((split pea) (red bean) (green lentil)))
 ))
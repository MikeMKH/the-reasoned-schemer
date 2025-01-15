#lang racket
(require cKanren/miniKanren)

; https://github.com/rymaju/mykanren/blob/c56318625319b598c0e05cf993e8aafabee83187/mykanren.rkt#L360C1-L366C34
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (s)
         (lambda ()
           ((conj g ...) s)))))))

(define s (== #t #t))
(define u (== #t #f))

(require (except-in rackunit fail))
(require rackunit/text-ui)

(defrel (caro p a)
  (fresh (d)
    (== (cons a d) p)))

; 13
(defrel (cdro p d)
  (fresh (a)
    (== (cons a d) p)))

; 25
(defrel (conso a d p)
  (caro p a)
  (cdro p d))

; 33
(defrel (nullo x)
  (== x '()))

; 46
(defrel (pairo p)
  (fresh (a d)
    (conso a d p)))

; 58
(define (singleton? l)
  (cond
    ((pair? l) (null? (cdr l)))
    (else #f)))

; 68
(defrel (singletono l)
  (fresh (d)
    (cdro l d)
    (nullo d)))

(run-tests
 (test-suite "chapter 2"
  (test-equal? "1" (car '(grape rasin pear)) 'grape)
  (test-equal? "2" (car '(a c o r n)) 'a)
  
  ; caro is defrel outside of run-tests
  (test-equal? "3" (run* (q) (caro '(a c o r n) q)) '(a))
  (test-equal? "4" (run* (q) (caro '(a c o r n) 'a)) '(_.0))
  (test-equal? "5" (run* (r) (fresh (x y) (caro (cons r y) x) (== 'pear x))) '(pear))
  (test-equal? "7" (cons (car '(grape rasin pear)) (car '((a) (b) (c)))) '(grape a))
  (test-equal? "8" (run* (r) (fresh (x y) (caro '(grape rasin pear) x) (caro '((a) (b) (c)) y) (== (cons x y) r))) '((grape a)))
  (test-equal? "10" (cdr '(grape rasin pear)) '(rasin pear))
  (test-equal? "11" (car (cdr (cdr '(a c o r n)))) 'o)
  
  ; cdro is defrel outside of run-tests
  (test-equal? "12" (run* (r) (fresh (v) (cdro '(a c o r n) v) (fresh (w) (cdro v w) (caro w r)))) '(o))
  (test-equal? "15" (run* (r) (fresh (x y) (cdro '(grape rasin pear) x) (caro '((a) (b) (c)) y) (== (cons x y) r))) '(((rasin pear) a)))
  (test-equal? "16" (run* (q) (cdro '(a c o r n) '(c o r n))) '(_.0))
  (test-equal? "17" (run* (x) (cdro '(c o r n) (cons x '(r n)))) '(o))
  (test-equal? "18" (run* (l) (fresh (x) (cdro l '(c o r n)) (caro l x) (== 'a x))) '((a c o r n)))

  ; conso is defrel outside of run-tests
  (test-equal? "19" (run* (l) (conso '(a b c) '(d e) l)) '(((a b c) d e)))
  (test-equal? "20" (run* (x) (conso x '(a b c) '(d a b c))) '(d))
  (test-equal? "22" (run* (x) (conso x (cons 'a (cons x '(c))) (append '(d a) (cons x '(c))))) '(d))

  (test-equal? "28" (null? '(grape rasin pear)) #f)
  (test-equal? "29" (null? '()) #t)

  ; nullo is defrel outside of run-tests
  (test-equal? "30" (run* (q) (nullo '(grape rasin pear))) '())
  (test-equal? "31" (run* (q) (nullo '())) '(_.0))
  (test-equal? "32" (run* (x) (nullo x)) '(()))

  (test-equal? "36" (pair? '((split) . pea)) #t)
  (test-equal? "37" (pair? '()) #f)
  (test-equal? "41" (car '(pear)) 'pear)
  (test-equal? "42" (cdr '(pear)) '())
  (test-equal? "43" (cons '(split) 'pea) '((split) . pea))
  (test-equal? "44" (run* (r) (fresh (x y) (== (cons x (cons y 'salad)) r))) '((_.0 _.1 . salad)))

  ; pairo is defrel outside of run-tests
  (test-equal? "47" (run* (q) (pairo (cons q q))) '(_.0))
  (test-equal? "48" (run* (q) (pairo '())) '())
  (test-equal? "49" (run* (q) (pairo 'pair)) '())
  (test-equal? "50" (run* (x) (pairo x)) '((_.0 . _.1)))
  (test-equal? "51" (run* (r) (pairo (cons r '()))) '(_.0))

  ; singleton? define outside of run-tests
  (test-equal? "60" (singleton? '()) #f)
  (test-equal? "61" (singleton? (cons 'pea '())) #t)
  (test-equal? "62" (singleton? '(vegan-yogurt)) #t)

  ; singletono defrel outside of run-tests
  (test-equal? "extra 60" (run* (q) (singletono '())) '())
  (test-equal? "extra 61" (run* (q) (singletono (cons 'pea '()))) '(_.0))
  (test-equal? "extra 62" (run* (q) (singletono '(vegan-yogurt))) '(_.0))
  (test-equal? "extra 32" (run* (x) (singletono x)) '((_.0)))
 ))
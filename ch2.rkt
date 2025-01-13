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
 ))
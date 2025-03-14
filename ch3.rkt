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

(defrel (cdro p d)
  (fresh (a)
    (== (cons a d) p)))

(defrel (conso a d p)
  (caro p a)
  (cdro p d))

(defrel (nullo x)
  (== x '()))

(defrel (pairo p)
  (fresh (a d)
    (conso a d p)))

; 1
(define (list? l)
  (cond
    ((null? l) #t)
    ((pair? l) (list? (cdr l)))
    (#t #f)))

; 8
(defrel (listo l)
  (conde
     ((nullo l) s)
     ((fresh (d)
      (cdro l d)
      (listo d)))))

; 21
(define (lol? l)
  (cond
    ((null? l) #t)
    ((list? car l) (lol? (cdr l)))
    (#t #f)))

; 22
(defrel (lolo l)
     (conde
      ((nullo l))
      ((fresh (a)
        (caro l a)
        (listo a)
        (fresh (d)
          (cdro l d)
          (lolo d))))))

; 32
#|(defrel (singletono l)
  (fresh (a)
    (== `(,a) l)))
|#
(defrel (singletono l)
  (fresh (d)
     (cdro l d)
     (nullo d)))
; 33
#| (defrel (lolo l)
     (conde
        ((nullo l))
        ((fresh (a)
           (caro l a)
           (singletono a))
         (fresh (d)
            (cdro l d)
            (lolo d)))))
 |#

; 44
(define (member? x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) #t)
    (#t (member? x (cdr l)))))

; 47
(defrel (membero x l)
  (conde
     ((caro l x))
     ((fresh (d)
       (cdro l d)
       (membero x d)))))

; 73
(defrel (proper-membero x l)
  (conde
   ((caro l x)
    (fresh (d)
      (cdro l d)
      (listo d)))
   ((fresh (d)
      (cdro l d)
      (proper-membero x d)))))

(run-tests
 (test-suite "chapter 3"
  (test-equal? "2" (list? '()) #t)
  (test-equal? "3" (list? 's) #f)
  (test-equal? "4" (list? '(date . s)) #f)

  ; listo is defrel outside of run-tests
  (test-equal? "9" (run* (x) (listo (append '(a b) (cons x '(d))))) '(_.0))
  ;(test-equal? "12" (run* (x) (listo (append '(a b) x))) '(lots-of-things)) ; will cause an endless loop
  (test-equal? "14" (run 1 (x) (listo (append '(a b) x))) '(()))
  (test-equal? "18" (run 5 (x) (listo (append '(a b) x))) '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3)))

  ; lolo is defrel outside of run-tests
  (test-equal? "23" (run* (q) (fresh (x y) (lolo '((a b) (,x c) (d ,y))))) '(_.0))
  (test-equal? "24" (run 1 (q) (lolo q)) '(()))
  (test-equal? "25" (run 1 (q) (fresh (x) (lolo (append '((a b)) x)))) '(_.0))
  (test-equal? "26" (run 1 (x) (lolo (append '((a b) (c d)) x))) '(()))
  (test-equal? "27" (run 5 (x) (lolo (append '((a b) (c d)) x))) '(() (()) ((_.0)) (() ()) ((_.0 _.1))))
  (test-equal? "29" (run 5 (x) (lolo x)) '(() (()) ((_.0)) (() ()) ((_.0 _.1))))
  (test-equal? "34" (run 1 (z) (lolo `((g) . ,z))) '(()))
  (test-equal? "38" (run 5 (z) (lolo `((g) . ,z))) '(() (()) ((_.0)) (() ()) ((_.0 _.1))))
  (test-equal? "41" (run 4 (r) (fresh (w x y z) (lolo `((g) (e . ,w) (,x . ,y) . ,z)) (== `(,w (,x . ,y) ,z) r))) '((() (_.0) ()) (() (_.0 _.1) ()) ((_.0) (_.1) ()) (() (_.0) (()))))

  ; membero is defrel outside of run-tests
  (test-equal? "44" (member? 'olive '(virgin olive oil)) #t)
  (test-equal? "48" (run* (q) (membero 'olive '(virgin olive oil))) '(_.0))
  (test-equal? "49" (run 1 (y) (membero y '(hummus with pita))) '(hummus))
  (test-equal? "50" (run 1 (y) (membero y '(with pita))) '(with))
  (test-equal? "51" (run 1 (y) (membero y '(pita))) '(pita))
  (test-equal? "52" (run 1 (y) (membero y '())) '())
  (test-equal? "53" (run* (y) (membero y '(hummus with pita))) '(hummus with pita))
  (test-equal? "56" (run* (x) (membero 'e `(pata ,x fagioli))) '(e))
  (test-equal? "59" (run 1 (x) (membero 'e `(pata e ,x fagioli))) '(_.0))
  (test-equal? "60" (run 1 (x) (membero 'e `(pata ,x fagioli))) '(e))
  (test-equal? "61" (run* (x y) (membero 'e `(pata ,x fagioli ,y))) '((e _.0) (_.0 e)))
  (test-equal? "63" (run* (q) (fresh (x y) (== `(pata ,x fagioli ,y) q) (membero 'e q))) '((pata e fagioli _.0) (pata _.0 fagioli e)))
  (test-equal? "64" (run 1 (l) (membero 'tofu l)) '((tofu . _.0)))
  ;(test-equal? "66" (run* (l) (membero 'tofu l)) '(lots-of-things)) ; will cause an endless loop
  (test-equal? "67" (run 5 (l) (membero 'tofu l)) '((tofu . _.0) (_.0 tofu . _.1) (_.0 _.1 tofu . _.2) (_.0 _.1 _.2 tofu . _.3) (_.0 _.1 _.2 _.3 tofu . _.4)))

  ; proper-membero is defrel outside of run-tests
  (test-equal? "74" (run 3 (l) (proper-membero 'tofu l)) '((tofu) (tofu _.0) (tofu _.0 _.1))) ; changed from run 12 in text, who needs that many results
 ))
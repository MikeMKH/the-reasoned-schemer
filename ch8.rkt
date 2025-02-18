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

(defrel (bit-xoro x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 1 r))
   ((== 1 x) (== 0 y) (== 1 r))
   ((== 1 x) (== 1 y) (== 0 r))))

(defrel (bit-ando x y r)
  (conde
   ((== 0 x) (== 0 y) (== 0 r))
   ((== 0 x) (== 1 y) (== 0 r))
   ((== 1 x) (== 0 y) (== 0 r))
   ((== 1 x) (== 1 y) (== 1 r))))

(defrel (half-addero x y r c)
  (bit-xoro x y r)
  (bit-ando x y c))

(defrel (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(defrel (addero b n m r)
  (conde
   ((== 0 b) (== '() m) (== n r))
   ((== 0 b) (== '() n) (== m r) (poso m))
   ((== 1 b) (== '() m) (addero 0 n '(1) r))
   ((== 1 b) (== '() n) (poso m) (addero 0 '(1) m r))
   ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero b 1 1 a c)))
   ((== '(1) n) (gen-addero b n m r))
   ((== '(1) m) (>1o n) (>1o r) (addero b '(1) n r))
   ((>1o n) (gen-addero b n m r))))

(defrel (gen-addero b n m r)
  (fresh (a c d e x y z)
    (== `(,a . ,x) n)
    (== `(,d . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero b a d c e)
    (addero e x y z)))

(defrel (+o n m k)
  (addero 0 n m k))

(require (except-in rackunit fail))
(require rackunit/text-ui)

; 9
(defrel (*o n m p)
  (conde
   ((== '() n) (== '() p))
   ((poso n) (== '() m) (== '() p))
   ((== '(1) n) (poso m) (== m p))
   ((>1o n) (== '(1) m) (== n p))
   ((fresh (x z)
      (== `(0 . ,x) n) (poso x)
      (== `(0 . ,z) p) (poso z)
      (>1o m)
      (*o x m z)))
   ((fresh (x y)
      (== `(1 . ,x) n) (poso x)
      (== `(0 . ,y) m) (poso y)
      (*o m n p)))
   ((fresh (x y)
      (== `(1 . ,x) n) (poso x)
      (== `(1 . ,y) m) (poso y)
      (odd-*o x n m p)))))

; 17
(defrel (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (+o `(0 . ,q) m p)))

; 24
(defrel (bound-*o q p n m)
  (conde
   ((== '() q) (poso p))
   ((fresh (a0 a1 a2 a3 x y z)
      (== `(,a0 . ,x) q)
      (== `(,a1 . ,y) p)
      (conde
       ((== '() n)
        (== `(,a2 . ,z) m)
        (bound-*o x y z'()))
       ((== `(,a3 . ,z) n)
        (bound-*o x y z m)))))))

; 28
(defrel (=lo n m)
  (conde
   ((== '() n) (== '() m))
   ((== '(1) n) (== '(1) m))
   ((fresh (a x b y)
      (== `(,a . ,x) n) (poso x)
      (== `(,b . ,y) m) (poso y)
      (=lo x y)))))

; 36
(defrel (<lo n m)
  (conde
   ((== '() n) (poso m))
   ((== '(1) n) (>1o m))
   ((fresh (a x b y)
      (== `(,a . ,x) n) (poso x)
      (== `(,b . ,y) m) (poso y)
      (<lo x y)))))

; 40
(defrel (<=lo n m)
  (conde
   ((=lo n m))
   ((<lo n m))))

; 46
(defrel (<o n m)
  (conde
   ((<lo n m))
   ((=lo n m)
    (fresh (x)
      (poso x)
      (+o n x m)))))

(defrel (<=o n m)
  (conde
   ((== n m))
   ((<o n m))))

; 54
(defrel (/o n m q r)
  (conde
   ; <o are needed for backtracking
   ((== '() q) (== n r) (<o n m))
   ((== '(1) q) (== '() r) (== n m) (<o r m))
   ((<o m n) (<o r m)
     (fresh (mq)
       (<=lo mq n)
       (*o m q mq)
       (+o mq r n)))))

(run-tests
 (test-suite "chapter 8"
  ; *o is defrel outside of run-tests
  (test-equal? "1" (run 10 (x y r) (*o x y r))
    '((() _.0 ())
      ((_.0 . _.1) () ())
      ((1) (_.0 . _.1) (_.0 . _.1))
      ((_.0 _.1 . _.2) (1) (_.0 _.1 . _.2))
      ((0 1) (_.0 _.1 . _.2) (0 _.0 _.1 . _.2))
      ((0 0 1) (_.0 _.1 . _.2) (0 0 _.0 _.1 . _.2))
      ((1 _.0 . _.1) (0 1) (0 1 _.0 . _.1))
      ((0 0 0 1) (_.0 _.1 . _.2) (0 0 0 _.0 _.1 . _.2))
      ((1 _.0 . _.1) (0 0 1) (0 0 1 _.0 . _.1))
      ((0 1 _.0 . _.1) (0 1) (0 0 1 _.0 . _.1))))
  (test-equal? "3" (run* (p) (*o '(0 1) '(0 0 1) p)) '((0 0 0 1))) ; related to 5th value above '((0 1) (_.0 _.1 . _.2) (0 _.0 _.1 . _.2))
  (test-equal? "8" (run 1 (x y r) (== `(,x ,y ,r) '((1 1) (1 1) (1 0 0 1))) (*o x y r)) '(((1 1) (1 1) (1 0 0 1))))
  (test-equal? "19" (run 1 (n m) (*o n m '(1))) '(((1) (1))))
  (test-equal? "20" (run 1 (n m) (>1o n) (>1o m) (*o n m '(1 1))) '())
  (test-equal? "25" (run 2 (n m) (*o n m '(1))) '(((1) (1))))
  (test-equal? "26" (run* (p) (*o '(1 1 1) '(1 1 1 1 1 1) p)) '((1 0 0 1 1 1 0 1 1)))

  ; =1o is defrel outside of run-tests
  (test-equal? "29" (run* (w x y) (=lo `(1 ,w ,x . ,y) '(0 1 1 0 1))) '((_.0 _.1 (_.2 1))))
  (test-equal? "30" (run* (b) (=lo '(1) `(,b))) '(1))
  (test-equal? "31" (run* (n) (=lo `(1 0 1 . ,n) '(0 1 1 0 1))) '((_.0 1)))
  (test-equal? "32" (run 5 (y z) (=lo `(1 . ,y) `(1 . ,z)))
    '((() ())
      ((1) (1))
      ((_.0 1) (_.1 1))
      ((_.0 _.1 1) (_.2 _.3 1))
      ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))))
  (test-equal? "33" (run 5 (y z) (=lo `(1 . ,y) `(0 . ,z)))
    '(((1) (1))
      ((_.0 1) (_.1 1))
      ((_.0 _.1 1) (_.2 _.3 1))
      ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))
      ((_.0 _.1 _.2 _.3 1) (_.4 _.5 _.6 _.7 1))))
  (test-equal? "35" (run 5 (y z) (=lo `(1 . ,y) `(0 1 1 0 1 . ,z)))
    '(((_.0 _.1 _.2 1) ())
      ((_.0 _.1 _.2 _.3 1) (1))
      ((_.0 _.1 _.2 _.3 _.4 1) (_.5 1))
      ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 1))
      ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 1) (_.7 _.8 _.9 1))))

  ; <1o is defrel outside of run-tests
  (test-equal? "37" (run 8 (y z) (<lo `(1 . ,y) `(0 1 1 0 1 . ,z)))
    '((() _.0)
      ((1) _.0)
      ((_.0 1) _.1)
      ((_.0 _.1 1) _.2)
      ((_.0 _.1 _.2 1) (_.3 . _.4))
      ((_.0 _.1 _.2 _.3 1) (_.4 _.5 . _.6))
      ((_.0 _.1 _.2 _.3 _.4 1) (_.5 _.6 _.7 . _.8))
      ((_.0 _.1 _.2 _.3 _.4 _.5 1) (_.6 _.7 _.8 _.9 . _.10))))
  ;(test-equal? "39" (run 1 (n) (<lo n n)) '(endless-nothing)) ; no value can meet this condition

  ; <=lo is defrel outside of run-tests
  (test-equal? "41" (run 8 (n m) (<=lo n m))
    '((() ())
      ((1) (1))
      (() (_.0 . _.1))
      ((1) (_.0 _.1 . _.2))
      ((_.0 1) (_.1 1))
      ((_.0 1) (_.1 _.2 _.3 . _.4))
      ((_.0 _.1 1) (_.2 _.3 1))
      ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6))))
  (test-equal? "42" (run 1 (n m) (<=lo n m) (*o n '(0 1) m)) '((() ())))
  (test-equal? "43" (run 10 (n m) (<=lo n m) (*o n '(0 1) m))
    '((() ())
      ((1) (0 1))
      ((0 1) (0 0 1))
      ((1 1) (0 1 1))
      ((1 _.0 1) (0 1 _.0 1))
      ((0 0 1) (0 0 0 1))
      ((0 1 1) (0 0 1 1))
      ((1 _.0 _.1 1) (0 1 _.0 _.1 1))
      ((0 1 _.0 1) (0 0 1 _.0 1))
      ((0 0 0 1) (0 0 0 0 1))))
  (test-equal? "44" (run 9 (n m) (<=lo n m))
    '((() ())
      ((1) (1))
      (() (_.0 . _.1))
      ((1) (_.0 _.1 . _.2))
      ((_.0 1) (_.1 1))
      ((_.0 1) (_.1 _.2 _.3 . _.4))
      ((_.0 _.1 1) (_.2 _.3 1))
      ((_.0 _.1 1) (_.2 _.3 _.4 _.5 . _.6))
      ((_.0 _.1 _.2 1) (_.3 _.4 _.5 1))))

  ; <o is defrel outside of run-tests
  (test-equal? "47" (run* (q) (<o '(1 0 1) '(1 1 1))) '(_.0))
  (test-equal? "48" (run* (q) (<o '(1 1 1) '(1 0 1))) '())
  (test-equal? "49" (run* (q) (<o '(1 0 1) '(1 0 1))) '())
  (test-equal? "50" (run 6 (n) (<o n '(1 0 1))) '(() (1) (_.0 1) (0 0 1)))
  (test-equal? "51" (run 6 (m) (<o '(1 0 1) m)) '((_.0 _.1 _.2 _.3 . _.4) (0 1 1) (1 1 1)))
  ;(test-equal? "52" (run* (n) (<o n n)) '(endless-nothing)) ; no value can meet this condition, same as 39

  ; /o is defrel outside of run-tests
  (test-equal? "53" (run 4 (n m q r) (/o n m q r))
    '((() (_.0 . _.1) () ())
      ((_.0 . _.1) (_.0 . _.1) (1) ())
      ((1) (_.0 _.1 . _.2) () (1))
      ((_.0 1) (_.1 _.2 _.3 . _.4) () (_.0 1))))
  (test-equal? "62" (run* (m) (fresh (r) (/o '(1 0 1) m '(1 1 1) r))) '())
 ))
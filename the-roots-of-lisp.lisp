; ---- quote

(quote a)
;A

'a
;A

(quote (a b c))
;(A B C)

; Cambridge is a town in Massachisetts that contains about 90k people.
; "Cambridge" is a word that contains 9 letters.
; code and data are made out of the same data structures, and the quote operator is the way we distinguish between them.

; -

'(a b c)
;(A B C)

; ---- atom

(atom 'a)
;T

(atom '(a b c))
;NIL

(atom '())
;T


(atom (atom 'a))
;T

(atom '(atom 'a))
;NIL


; -

(atom NIL)
;T

(atom (quote a))
;T

(atom ())
;T

()
;NIL

'()
;NIL

; ---- eq

(eq 'a 'a)
;T

(eq 'a 'b)
;NIL

(eq '() '())
;T

; -

(eq '() ())
;T

(eq '() NIL)
;T

;(eq '() NIL ())
;*** - EVAL: too many arguments given to EQ:
;      (EQ 'NIL NIL NIL)


; ---- car

(car '(a b c))
;A

; ---- cdr

(cdr '(a b c))
;(B C)

; ---- cons

(cons 'a '(b c))
;(A B C)

(cons 'a (cons 'b (cons 'c '())))
;(A B C)

(car (cons 'a '(b c)))
;A

(cdr (cons 'a '(b c)))
;(B C)

; -

(cons 'a 'b)
;(A . B)
; 🤔️???


;(cons 'a (b c))
;*** - EVAL: undefined function B

; ---- cond

(cond ((eq 'a 'b) 'first)
      ((atom 'a) 'second))
;SECOND

; -

(cond ((eq 'a 'b) 'first ))
;NIL

; -

(cond ((eq 'a 'b) 1) ('t 2))
;2

; In five of our seven primitive operators, the arguments are always evaluated when an expression beginning with that operator is evaluated. We will call an operator of that type a 'function'.

; primitive operators that are functions: atom, eq, car, cdr, cons
; primitive operators that ARE NOT functions:
; - quote: arguments are not evaluated but returned as a value,
; - cond: "only an L-shaped path of subexpressions will be evaluated". (WAT 🤔️???)

; ====

; ---- lambda

;function
; (lambda (p1 ... pn) e)
;where
; p1...pn are atoms called parameters
; e is an expression

;function call
; ((lambda (p1 ... pn) e) a1...an)
; each expression ai is evaluated
; then e is evaluated
; during the evaluation of e, the value of pi is the value of the corresponding ai

((lambda (x) (cons x '(b))) 'a)
;(A B)

((lambda (x y) (cons x (cdr y))) 'z '(a b c))
;(Z B C)

;((lambda (f) (f '(b c))) '(lambda (x) (cons 'a x)))
;*** - EVAL: undefined function F

; -

;(label square (lambda (x) (* x x)))
;*** - EVAL: undefined function LABEL

(defun square (x) (* x x))
;SQUARE

(square 2)
;4

(defun subst (x y z)
  (cond ((atom z)
    (cond ((eq z y) x)
          ('t z)))
    ('t (cons (subst x y (car z))
              (subst x y (cdr z))))))
;** - Continuable Error
;DEFUN/DEFMACRO(SUBST): #<PACKAGE COMMON-LISP> is
;     locked


(defun subst. (x y z)
  (cond ((atom z)
    (cond ((eq z y) x)
          ('t z)))
    ('t (cons (subst. x y (car z))
              (subst. x y (cdr z))))))
;SUBST.

;'t is a convention (everything that is not NIL is truthy)

(subst. 'm 'b '(a b (a b c) d))
;(A M (A M C) D)

; ---- cxr

;(cadr e)  <=> (car (cdr e))
;(caddr e) <=> (car (cdr (cdr e)))
;(cdar e)  <=> (cdr (car e))

(cadr '((a b) (c d) e))
;(C D)

(cdr '((a b) (c d) e))
;((C D) E)

(car '((c d) e))
;(C D)



(caddr '((a b) (c d) e))
;E

(cdr '((a b) (c d) e))
;((C D) E)

(cdr '((c d) e))
;(E)


(car '(e))
;E



(cdar '((a b) (c d) e))
;(B)

(car '((a b) (c d) e))
;(A B)

(cdr '(a b))
;(B)

; ---- list

(cons 'a (cons 'b (cons 'c '())))
;(A B C)

(list 'a 'b 'c)
;(A B C)

; ---- null

(defun null. (x)
  (eq x '()))
;NULL.

(null. 'a)
;NIL

(null. '())
;T

(null. NIL)
;T


; ---- and

(defun and. (x y)
  (cond (x
          (cond (y 't)
                ('t '())))
        ('t '())))
;AND.

(and. (atom 'a) (eq 'a 'a))
;T

(and. (atom 'a) (eq 'a 'b))
;NIL

(and. (atom '(a b)) (eq 'a 'a))
;NIL

; ---- not

(defun not. (x)
  (cond (x '())
  ('t 't)))
;NOT.

(not (eq 'a 'a))
;NIL

(not (eq 'a 'b))
;T

; ---- append

(defun append. (x y)
  (cond ((null. x) y)
        ('t (cons (car x) (append. (cdr x) y)))))
;APPEND.

(append. '() '(c d))
;(C D)

(append. '(a b) '(c d))
;(A B C D)

(append. '(a) '(b c d))
;(A B C D)

; ---- pair

(defun pair. (x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
          (cons (list (car x) (car y))
                (pair. (cdr x) (cdr y))))))
;PAIR.

(pair. '(x y z) '(a b c))
;((X A) (Y B) (Z C))

; ---- assoc

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y
;ASSOC.

(assoc. 'x '((x a) (y b)))
;A


(assoc. 'x '((x new) (x a) (y b)))
;NEW

; -

(assoc. 'x '((k a) (m b) (x c)))
;C

;(assoc. 'x '((k a) (m b) (n c)))
;*** - Lisp stack overflow. RESET

; ---- eval

(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
      (cond
        ((eq (car e) 'quote) (cadr e))
        ((eq (car e) 'atom) (atom (eval. (cadr e) a)))
        ((eq (car e) 'eq) (eq (eval. (cadr e) a)
                              (eval. (caddr e) a)))
        ((eq (car e) 'car) (car (eval. (cadr e) a)))
        ((eq (car e) 'cdr) (cdr (eval. (cadr e) a)))
        ((eq (car e) 'cons) (cons (eval. (cadr e) a)
                                  (eval. (caddr e) a)))
        ((eq (car e) 'cond) (evcon. (cdr e) a))
        ('t (eval. (cons (assoc. (car e) a)
                         (cdr e))
                   a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))
;EVAL.

(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))
;EVCON.

(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval. (car m) a)
                  (evlis. (cdr m) a)))))
;EVLIS.

(eval. 'x '((x a) (y b)))
;A

(eval. 'x '((x a) (y b)))
;A

(eval. '(eq 'a 'a) '())
;T

(eval. '(cons x '(b c)) '((x a) (y b)))
;(A B C)

(eval. '(cond ((atom x) 'atom)
              ('t 'list))
       '((x '(a b))))
;LIST

(eval. '(f '(b c))
       '((f (lambda (x) (cons 'a x)))))
;(A B C)

(eval. '((lambda (x) (cons 'a x)) '(b c))
       '((f (lambda (x) (cons 'a x)))))
;(A B C)

(eval. '((label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  ('t (firstatom (car x))))))
          y)
         '((y ((a b) (c d)))))
;A

(eval. '((lambda (x)
          (cond ((atom x) x)
                ('t (firstatom (car x)))))
        y)
       '((firstatom 
          (label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  ('t (firstatom (car x)))))))
      (y ((a b) (c d)))))
;A

(eval. '((lambda (x y) (cons x (cdr y))) 'a '(b c d)) '())
;(A C D)


(eval. '(cons x (cdr y)) '((x a) (y (b c d))))
;(A C D)



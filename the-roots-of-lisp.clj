; ---- quote

(quote a)
;=> a

'a
;=> a

(quote (a b c))
;=> (a b c)

; ---- atom (not defined in Clojure)

(defn atom* [x]
  (or (= x '()) (not (seq? x))))

(atom* 'a)
; => true

(atom* '(a b c))
; => false

(atom* '())
; => true

(atom* (atom* 'a))
; => true

(atom* '(atom* 'a))
; => false

; -

(atom* true)
; => true

; ---- eq <=> =

(= 'a 'a)
; => true

(= 'a 'b)
; => false

(= '() '())
; => true

; ---- car <=> first

(first '(a b c))
; => a

; ---- cdr <=> rest

(rest '(a b c))
; => (b c)

; ---- cons

(cons 'a '(b c))
; => (a b c)

(cons 'a (cons 'b (cons 'c '())))
; => (a b c)

(first (cons 'a '(b c)))
; => a

(rest (cons 'a '(b c)))
; => (b c)

; ---- cond (Clojure's syntax is different)
(cond (= 'a 'b) 'first
      (atom* 'a) 'second)
; => second

; ---- lambda <=> fn
;      defun <=> defn

((fn [x] (cons x '(b))) 'a)
; => (a b)

((fn [x y] (cons x (rest y)))
 'z
 '(a b c))
; => (z b c)

((fn [f] (f '(b c)))
 (fn [x] (cons 'a x))) ; no need for quoting
; => (a b c)

(defn subst [x y z]
  (cond (atom* z)
        (cond (= z y) x
              :default z) ; default clause's syntax is different in Clojure
        :default (cons (subst x y (first z))
                       (subst x y (rest z)))))

(subst 'm 'b '(a b (a b c) d))
; => (a m (a m c) d)

; ---- cxr
; Don't know if it exists...

; caar <=> ffirst

; ---- list

(cons 'a (cons 'b (cons 'c '())))
; => (a b c)

(list 'a 'b 'c)
; => (a b c)

; ---- null

(defn null* [x]
  (= x '()))

(null* 'a)
; => false

(null* '())
; => true

; ---- and

(defn and* [x y]
  (cond x
        (cond y true
              :default false)
        :default false))

(and* (atom 'a) (= 'a 'a))
; => true

(and* (atom 'a) (= 'a 'b))
; => false

; ---- not

(defn not* [x]
  (cond x false
        :default true))

(not* (= 'a 'a))
; => false

(not* (= 'a 'b))
; => true

; ---- append

(defn append* [x y]
  (cond (null* x) y
        :default (cons (first x) (append* (rest x) y))))

(append* '(a b) '(c d))
; => (a b c d)

; ---- pair

(defn pair* [x y]
  (cond (and* (null* x) (null* y)) '()
        (and* (not* (atom* x)) (not* (atom* y)))
        (cons (list (first x) (first y))
              (pair* (rest x) (rest y)))))

(pair* '(x y z) '(a b c))
; => ((x a) (y b) (z c))

; ---- assoc

(defn assoc* [x y]
  (cond (= (first (first y)) x)
        (first (rest (first y)))
        :default (assoc* x (rest y))))

(assoc* 'x '((x a) (y b)))
; => a

(assoc* 'x '((x new) (x a) (y b)))
; => new

; The Surprise

(defn eval* [e a]
  (defn evcon* [c a]
    (cond (eval* (first (first c)) a) (eval* (first (rest (first c))) a)
          :default (evcon* (rest c) a)))
  (defn evlis* [m a]
    (cond (null* m) '()
          :default (cons (eval* (first m) a)
                         (evlis* (rest m) a))))
  (cond
    (atom* e) (assoc* e a)
    (atom* (first e))
      (cond
        (= (first e) 'quote) (first (rest e))
        (= (first e) 'atom) (atom* (eval* (first (rest e)) a))
        (= (first e) 'eq) (= (eval* (first (rest e)) a)
                             (eval* (first (rest (rest e))) a))
        (= (first e) 'car) (first (eval* (first (rest e)) a))
        (= (first e) 'cdr) (rest (eval* (first (rest e)) a))
        (= (first e) 'cons) (cons (eval* (first (rest e)) a)
                                  (eval* (first (rest (rest e))) a))
        (= (first e) 'cond) (evcon* (rest e) a)
        :default (eval* (cons (assoc* (first e) a)
                              (rest e))
                        a))
    (= (first (first e)) 'label)
      (eval* (cons (first (rest (rest (first e)))) (rest e))
             (cons (list (first (rest (first e))) (first e)) a))
    (= (first (first e)) 'lambda)
      (eval* (first (rest (rest (first e))))
             (append* (pair* (first (rest (first e)))
                             (evlis* (rest e) a))
                      a))
    ))

(eval* 'x '((x a) (y b)))
; => a

(eval* '(eq 'a 'a) '())
; => true

(eval* '(eq 'a 'b) '())
; => false

(eval* '(cons x '(b c))
       '((x a) (y b)))
; => (a b c)

(eval* '(cond ((atom x) 'atom)
              ('t 'list))
       '((x '(a b))))
; => list

(eval* '(f '(b c))
       '((f (lambda (x) (cons 'a x)))))
; => (a b c)

(eval* '((lambda (x) (cons 'a x)) '(b c))
       '((f (lambda (x) (cons 'a x)))))
; => (a b c)

(eval* '((label firstatom (lambda (x)
                                  (cond ((atom x) x)
                                        ('t (firstatom (car x))))))
         y)
       '((y ((a b) (c d)))))
; => a

(eval* '((lambda (x)
                 (cond ((atom x) x)
                       ('t (firstatom (car x)))))
         y)
       '((firstatom
           (label firstatom (lambda (x)
                                    (cond ((atom x) x)
                                          ('t (firstatom (car x)))))))
         (y ((a b) (c d)))))
; => a

(eval* '((lambda (x y) (cons x (cdr y))) 'a '(b c d)) '())
; => (a c d)


(eval* '(cons x (cdr y)) '((x a) (y (b c d))))
; => (a c d)


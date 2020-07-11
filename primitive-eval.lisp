;eval only with primitives: quote, atom, eq,  car, cdr, cons, cond


(defun null. (x)
       (eq x '()))

(defun and. (x y)
       (cond (x
               (cond (y 't)
                     ('t '())))
             ('t '())))

(defun not. (x)
       (cond (x '())
             ('t 't)))

(defun append. (x y)
       (cond ((null. x) y)
             ('t (cons (car x) (append. (cdr x) y)))))

(defun pair. (x y)
       (cond ((and. (null. x) (null. y)) '())
             ((and. (not. (atom x)) (not. (atom y)))
              (cons (cons (car x) (cons (car y) '()))
                          (pair. (cdr x) (cdr y))))))

       (defun assoc. (x y)
              (cond ((eq (car (car y)) x) (car (cdr (car y))))
                    ('t (assoc. x (cdr y)))))

       (defun eval. (e a)
              (cond
                ((atom e) (assoc. e a))
                ((atom (car e))
                 (cond
                   ((eq (car e) 'quote) (car (cdr e)))
                   ((eq (car e) 'atom) (atom (eval. (car (cdr e)) a)))
                   ((eq (car e) 'eq) (eq (eval. (car (cdr e)) a)
                                         (eval. (car (cdr (cdr e))) a)))
                   ((eq (car e) 'car) (car (eval. (car (cdr e)) a)))
                   ((eq (car e) 'cdr) (cdr (eval. (car (cdr e)) a)))
                   ((eq (car e) 'cons) (cons (eval. (car (cdr e)) a)
                                             (eval. (car (cdr (cdr e))) a)))
                   ((eq (car e) 'cond) (evcon. (cdr e) a))
                   ('t (eval. (cons (assoc. (car e) a)
                                    (cdr e))
                              a))))
                ((eq (car (car e)) 'label)
                 (eval. (cons (car (cdr (cdr (car e)))) (cdr e))
                        (cons (cons (car (cdr (car e))) (cons (car e) '())) a)))
                 ((eq (car (car e)) 'lambda)
                  (eval. (car (cdr (cdr (car e))))
                         (append. (pair. (car (cdr (car e))) (evlis. (cdr e) a))
                                  a)))))

              (defun evcon. (c a)
                     (cond ((eval. (car (car c)) a)
                            (eval. (car (cdr (car c))) a))
                           ('t (evcon. (cdr c) a))))

              (defun evlis. (m a)
                     (cond ((null. m) '())
                           ('t (cons (eval. (car m) a)
                                     (evlis. (cdr m) a)))))

; ---- testing

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



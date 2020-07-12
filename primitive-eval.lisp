;eval only with primitives: quote, atom, eq,  car, cdr, cons, cond with no special syntax besides parentheses


(defun null. (x)
       (eq x (quote ())))

(defun and. (x y)
       (cond (x
               (cond (y (quote t))
                     ((quote t) (quote ()))))
             ((quote t) (quote ()))))

(defun not. (x)
       (cond (x (quote ()))
             ((quote t) (quote t))))

(defun append. (x y)
       (cond ((null. x) y)
             ((quote t) (cons (car x) (append. (cdr x) y)))))

(defun pair. (x y)
       (cond ((and. (null. x) (null. y)) (quote ()))
             ((and. (not. (atom x)) (not. (atom y)))
              (cons (cons (car x) (cons (car y) (quote ())))
                          (pair. (cdr x) (cdr y))))))

       (defun assoc. (x y)
              (cond ((eq (car (car y)) x) (car (cdr (car y))))
                    ((quote t) (assoc. x (cdr y)))))

       (defun eval. (e a)
              (cond
                ((atom e) (assoc. e a))
                ((atom (car e))
                 (cond
                   ((eq (car e) (quote quote)) (car (cdr e)))
                   ((eq (car e) (quote atom)) (atom (eval. (car (cdr e)) a)))
                   ((eq (car e) (quote eq)) (eq (eval. (car (cdr e)) a)
                                         (eval. (car (cdr (cdr e))) a)))
                   ((eq (car e) (quote car)) (car (eval. (car (cdr e)) a)))
                   ((eq (car e) (quote cdr)) (cdr (eval. (car (cdr e)) a)))
                   ((eq (car e) (quote cons)) (cons (eval. (car (cdr e)) a)
                                             (eval. (car (cdr (cdr e))) a)))
                   ((eq (car e) (quote cond)) (evcon. (cdr e) a))
                   ((quote t) (eval. (cons (assoc. (car e) a)
                                    (cdr e))
                              a))))
                ((eq (car (car e)) (quote label))
                 (eval. (cons (car (cdr (cdr (car e)))) (cdr e))
                        (cons (cons (car (cdr (car e))) (cons (car e) (quote ()))) a)))
                 ((eq (car (car e)) (quote lambda))
                  (eval. (car (cdr (cdr (car e))))
                         (append. (pair. (car (cdr (car e))) (evlis. (cdr e) a))
                                  a)))))

              (defun evcon. (c a)
                     (cond ((eval. (car (car c)) a)
                            (eval. (car (cdr (car c))) a))
                           ((quote t) (evcon. (cdr c) a))))

              (defun evlis. (m a)
                     (cond ((null. m) (quote ()))
                           ((quote t) (cons (eval. (car m) a)
                                     (evlis. (cdr m) a)))))

; ---- testing

(eval. (quote x) (quote ((x a) (y b))))
;A

(eval. (quote x) (quote ((x a) (y b))))
;A

(eval. (quote (eq (quote a) (quote a))) (quote ()))
;T

(eval. (quote (cons x (quote (b c)))) (quote ((x a) (y b))))
;(A B C)

(eval. (quote (cond ((atom x) (quote atom))
              ((quote t) (quote list))))
       (quote ((x (quote (a b))))))
;LIST

(eval. (quote (f (quote (b c))))
       (quote ((f (lambda (x) (cons (quote a) x))))))
;(A B C)

(eval. (quote ((lambda (x) (cons (quote a) x)) (quote (b c))))
       (quote ((f (lambda (x) (cons (quote a) x))))))
;(A B C)

(eval. (quote ((label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  ((quote t) (firstatom (car x))))))
          y))
         (quote ((y ((a b) (c d))))))
;A

(eval. (quote ((lambda (x)
          (cond ((atom x) x)
                ((quote t) (firstatom (car x)))))
        y))
       (quote ((firstatom 
          (label firstatom (lambda (x)
                            (cond ((atom x) x)
                                  ((quote t) (firstatom (car x)))))))
      (y ((a b) (c d))))))
;A

(eval. (quote ((lambda (x y) (cons x (cdr y))) (quote a) (quote (b c d)))) (quote ()))
;(A C D)


(eval. (quote (cons x (cdr y))) (quote ((x a) (y (b c d)))))
;(A C D)



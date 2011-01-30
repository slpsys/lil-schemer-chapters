#lang scheme

(provide atom?)
(provide lat?)
(provide member?)

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat? 
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (member? x (cdr l))))))
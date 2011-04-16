#lang scheme

(require "little_schemer_ch2.ss")

(provide remberAll)
(provide rember)
(provide firsts)
(provide insertR)
(provide insertL)
(provide subst)
(provide subst2)
(provide multirember)
(provide multiinsertR)
(provide multiinsertL)
(provide multisubst)

(define remberAll
  (lambda (x l)
    (cond
      ((null? l) (list ))
      ((eq? x (car l)) (rember x (cdr l)))
      (else (cons (car l) (rember x (cdr l)))))))

(define rember
  (lambda (x l)
    (cond
      ((null? l) (list ))
      ((eq? x (car l)) (cdr l))
      (else (cons (car l) (rember x (cdr l)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (list))
      ((or (atom? l) (atom? (car l))) (firsts (cdr l)))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR 
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (cons old (cons new (cdr l))))
      (else (cons (car l) (insertR new old (cdr l)))))))

(define insertL
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? old (car l)) (cons new l))
      (else (cons (car l) (insertL new old (cdr l)))))))

(define subst
  (lambda (new old l)
    (cond 
      ((null? l) (list))
      ((eq? old (car l)) (cons new (cdr l)))
      (else (cons (car l) (subst new old (cdr l)))))))

(define subst2
  (lambda (new o1 o2 l)
    (cond
      ((null? l) (list l))
      ((or 
        (eq? o1 (car l))
        (eq? o2 (car l)))
       (cons new (cdr l))
       )
      (else (cons (car l) (subst2 new o1 o2 (cdr l)))))))

(define multirember
  (lambda (a l)
    (cond
      ((null? l) (list))
      ((eq? a (car l)) (multirember a (cdr l)))
      (else (cons (car l) (multirember a (cdr l)))))))
  
(define multiinsertR
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? (car l) old) (cons old (cons new (multiinsertR new old (cdr l)))))
      (else (cons (car l) (multiinsertR new old (cdr l)))))))

(define multiinsertL
  (lambda (new old l)
    (cond
      ((null? l) (list))
      ((eq? (car l) old) (cons new (cons old (multiinsertL new old (cdr l)))))
      (else (cons (car l) (multiinsertL new old (cdr l)))))))

(define multisubst
  (lambda (new old l)
    (cond 
      ((null? l) (list))
      ((eq? old (car l)) (cons new (multisubst new old (cdr l))))
      (else (cons (car l) (multisubst new old (cdr l)))))))
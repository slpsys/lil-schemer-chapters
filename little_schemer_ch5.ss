#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")
(require "little_schemer_ch4.ss")

(provide rember*)
(provide insertR*)
(provide occur*)
(provide subst*)
(provide insertL*)
(provide member*)
(provide leftmost)
(provide eqlist?)

(define rember* 
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eqan? a (car l)) (rember* a (cdr l)))
      ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((eqan? o (car l)) (cons o (cons n (insertR* n o (cdr l)))))
      ((atom? (car l)) (cons (car l) (insertR* n o (cdr l))))
      (else (cons (insertR* n o (car l)) (insertR* n o (cdr l)))))))

(define occur* 
  (lambda (a l)
    (cond 
      ((null? l) 0)
      ((eqan? a (car l)) (add1 (occur* a (cdr l))))
      ((atom? (car l)) (occur a (cdr l)))
      (else (plus (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((eqan? o (car l)) (cons n (subst* n o (cdr l))))
      ((atom? (car l)) (cons (car l) (subst* n o (cdr l))))
      (else (cons (subst* n o (car l)) (subst* n o (cdr l)))))))

(define insertL*
  (lambda (n o l)
    (cond
      ((null? l) '())
      ((eqan? o (car l)) (cons n (cons o (insertL* n o (cdr l)))))
      ((atom? (car l)) (cons (car l) (insertL* n o (cdr l))))
      (else (cons (insertL* n o (car l)) (insertL* n o (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eqan? a (car l)) #t)
      ((atom? (car l)) (member* a (cdr l)))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (a b)
    (cond
      ((null? a) (null? b))
      ((null? b) #f)
      ((atom? a) (and (atom? b) (eqan? a b)))
      ((atom? b) #f)
      ((null? (car a)) (null? (car b)))
      ((null? (car b)) #f)
      ((null? (cdr a)) (and (null? (cdr b)) (eqlist? (car a) (car b))))
      ((null? (cdr b)) #f)
      (else (and (eqlist? (car a) (car b)) (eqlist? (cdr a) (cdr b)))))))

(define equal?
  (lambda (a b)
    (cond
      ((and (atom? a) (atom? b) (eqan? a b)))
      ((or (atom? a) (atom? b)) #f)
      (else (eqlist2? a b)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))))))


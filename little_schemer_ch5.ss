#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")
(require "little_schemer_ch4.ss")

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
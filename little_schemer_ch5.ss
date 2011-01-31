#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")
(require "little_schemer_ch4.ss")

(define rember* 
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eqan? a (car l)) (rember* a (cdr l)))
      ((not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l))))
      (else (cons (car l) (rember* a (cdr l)))))))

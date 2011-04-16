#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")
(require "little_schemer_ch4.ss")
(require "little_schemer_ch5.ss")

(define numbered? 
  (lambda (aexp) 
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      )
    ))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? '+ (operator nexp)) (+ (value (1st-subexp nexp)) (value (2nd-subexp nexp))))
      ((eq? '* (operator nexp)) (* (value (1st-subexp nexp)) (value (2nd-subexp nexp))))
      ((eq? '^ (operator nexp)) (^ (value (1st-subexp nexp)) (value (2nd-subexp nexp))))
    )))
    
(define operator
  (lambda (op)
    (car (cdr op))
    ))
(define 1st-subexp
  (lambda (aexp)
    (car aexp)
    ))
(define 2nd-subexp
  (lambda (aexp)
    (car (cdr (cdr aexp)))
    ))

(define sero?
  (lambda (n)
      (null? n)
      ))

(define edd1
  (lambda (n)
    (cons '() n)
    ))

(define zub1
  (lambda (n)
    (cdr n)
    ))

(define pluz
  (lambda (a b)
    (cond
      ((sero? b) a)
      (else
       (edd1 a)
       (zub1 b)
       ))))
#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")
(require "little_schemer_ch4.ss")
(require "little_schemer_ch5.ss")

(define numbered? 
  (lambda (aexp) 
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? '+ (car (cdr aexp))) (and (numbered? (cdr (cdr aexp))) (numbered? (car aexp))))
      ((eq? 'x (car (cdr aexp))) (and (numbered? (cdr (cdr aexp))) (numbered? (car aexp))))
      ((eq? '^ (car (cdr aexp))) (and (numbered? (cdr (cdr aexp))) (numbered? (car aexp))))
      (else #f)
      )
    ))

(numbered? '(3 * 2))
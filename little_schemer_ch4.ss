#lang scheme

(require "little_schemer_ch2.ss")
(require "little_schemer_ch3.ss")

(provide add1)
(provide sub1)
(provide plus)
(provide minus)
(provide addtup)
(provide times)
(provide tupPlus)
(provide gt)
(provide lt)
(provide eq)
(provide pow)
(provide div)
(provide length)
(provide pick)
(provide rempick)
(provide no-nums)
(provide all-nums)
(provide eqan?)
(provide occur)
(provide one?)
(provide rempick2)
(provide ^)

(define add1
  (lambda (a)
    (+ a 1)))

(define sub1
  (lambda (a)
    (- a 1)))

(define plus 
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (plus (add1 a) (sub1 b))))))

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (minus (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (a b)
    (cond 
      ((zero? b) 0)
      (else (plus a (times a (sub1 b)))))))

(define tupPlus
  (lambda (a b)
    (cond
      ((null? a) b)
      ((null? b) a)
      (else (cons (plus (car a) (car b)) (tupPlus (cdr a) (cdr b)))))))

(define gt
  (lambda (a b)
    (cond 
      ((zero? a) #f)
      ((zero? b) #t)
      (else (gt (sub1 a) (sub1 b))))))
(define lt
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (lt (sub1 a) (sub1 b))))))

(define eq
  (lambda (a b)
    (cond
      ((lt a b) #f)
      ((gt a b) #f)
      (else #t))))

(define pow
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (times a (pow a (sub1 b)))))))
  
(define div
  (lambda (a b)
    (cond 
      ((eq b 0) 'inf)
      ((lt a b) 0)
      (else (add1 (div (minus a b) b))))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))
      )
    ))

(define pick
  (lambda (a l)
    (cond 
      ((null? l) 'unknown)
      ((zero? (sub1 a)) (car l))
      (else (pick (sub1 a) (cdr l))))))

(define rempick
  (lambda (a l)
    (cond
      ((null? l) l)
      ((zero? (sub1 a)) (cdr l))
      (else (cons (car l) (rempick (sub1 a) (cdr l)))))))

(define no-nums
  (lambda (l)
    (cond 
      ((null? l) '())
      ((number? (car l)) (no-nums (cdr l)))
      (else (cons (car l) (no-nums (cdr l)))))))

(define all-nums
  (lambda (l)
    (cond 
      ((null? l) '())
      ((number? (car l)) (cons (car l) (all-nums (cdr l))))
      (else (all-nums (cdr l))))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (atom? a) (atom? b))
       (cond
         ((and (number? a) (eq a b)) #t)
         ((eq? a b) #t)
         (else #f)))
       (else #f)
       )
    )
  )

(define occur
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((eqan? a (car l)) (add1 (occur a (cdr l))))
      (else (occur a (cdr l))))))

(define one? 
  (lambda (a)
    (eqan? a 1)))

(define rempick2
  (lambda (a l)
    (cond 
      ((null? l) '())
      ((one? a) (cdr l))
      (else (cons (car l) (rempick2 (sub1 a) (cdr l))))
      )
    )
  )

;; Helper
(define ^
  (lambda (a b)
    (pow a b)))
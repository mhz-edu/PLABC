#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file


;; Problem 1
;; Produce list of numbers from low to high with the given step (stride).
;; ASSUME: low, high - numbers; stride - positive number
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;; Problem 2
;; Produce list of strings from a given list of strings and suffix
;; by appending suffix to each string in the list
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


;; Problem 3
;; Return i-th element of the given list (starting from 0)
;; where i is the remainder of division given n by the length of the list
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


;; Problem 4
;; Produce list with the first n elements of the given stream
;;  ASSUME: n is non-negative
(define (stream-for-n-steps strm n)
  (letrec ([helper (lambda (strm k)
                     (if (= k n)
                         null
                         (cons (car (strm)) (helper (cdr (strm)) (+ k 1)))))])
    (helper strm 0)))


;; Problem 5
;; Produce a stream of natural numbers, but if number is divisible by 5 it is negated
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= 0 (remainder x 5))
                              (cons (* x -1) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))


;; Problem 6
;; Produce a stream that alternates between strings "dan.jpg" and "dog.jpg"
(define dan-then-dog
  (letrec ([f (lambda (s) (if (string=? s "dan.jpg")
                              (cons "dan.jpg" (lambda () (f "dog.jpg")))
                              (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))


;; Problem 7
;; Produce a new stream, where each i-th element is a pair (0 . v)
;; where v is the i-th element of the given stream s
(define (stream-add-zero strm)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f strm))))


;; Problem 8
;; Produce a stream of pairs where first element is from the given list xs
;; and the second element is from the givent list ys.
;; ASSUME: both lists are non-empty
(define (cycle-lists xs ys)
  (letrec ([f (lambda (as bs n) (cons (cons (list-nth-mod as n)
                                            (list-nth-mod bs n))
                                      (lambda () (f as bs (+ n 1)))))])
    (lambda () (f xs ys 0))))


;; Problem 9
;; Locates fisrt element of the given vector whose car is equal to the given value
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(> n (- (vector-length vec) 1)) #f]
                      [(let ([elem (vector-ref vec n)])
                         (if (pair? elem)
                             (if (equal? v (car elem))
                                 elem
                                 (f (+ n 1)))
                             (f (+ n 1))))]
                      [#t (f (+ n 1))]))])
    (f 0)))


;; Problem 10
;; Cached assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [next-slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo next-slot (cons v new-ans))
                              (if (= next-slot (- n 1))
                                  (set! next-slot 0)
                                  (set! next-slot (+ next-slot 1)))
                              new-ans)
                            new-ans)))))])
    f))


;; Problem 11
;; Define a while-like macro
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do body)
     (let ([condition e1]
           [first-run body])
       (letrec ([loop (lambda (x)
                        (if (< x condition)
                            (begin
                              (loop body))
                            #t))])
         (loop first-run)))]))

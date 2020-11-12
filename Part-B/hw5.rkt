;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; Problem 1a
;; Produce MUPL list from the given Racket list of MUPL values
(define (racketlist->mupllist l)
  (if (null? l)
      (aunit)
      (apair (car l) (racketlist->mupllist (cdr l)))))


;; Problem 1b
;; Produce Racket list from the given MUPL list of MUPL values
(define (mupllist->racketlist ml)
  (if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(fun? e)
         (closure env (if (fun-nameopt e)
                          (fun (fun-nameopt e) (fun-formal e) (mlet (fun-nameopt e) e (fun-body e)))
                          e))]
        [(closure? e)
         e]
        [(call? e)
         (let ([clsr (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? clsr)
               (let ([ext-env (cons (cons (fun-formal (closure-fun clsr)) arg) (closure-env clsr))])      
                 (eval-under-env (fun-body (closure-fun clsr)) ext-env))
               (error "MUPL function is expected, got" (call-funexp e))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y") e4
                         (ifgreater (var "_y") (var "_x") e4 e3)))))

;; Problem 4

(define mupl-map
  (fun #f "in-func"
       (fun "helper" "ml" (ifaunit (var "ml")
                                   (aunit)
                                   (apair (call (var "in-func") (fst (var "ml")))
                                          (call (var "helper") (snd (var "ml"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "i") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([helper (lambda (in-e fvars)
                     (cond [(var? in-e) (set-add fvars (var-string in-e))]
                           [(add? in-e) (set-union (helper (add-e1 in-e) fvars) (helper (add-e2 in-e) fvars))]
                           [(ifgreater? in-e) (set-union (helper (ifgreater-e1 in-e) fvars)
                                                         (helper (ifgreater-e2 in-e) fvars)
                                                         (helper (ifgreater-e3 in-e) fvars)
                                                         (helper (ifgreater-e4 in-e) fvars))]
                           [(apair? in-e) (set-union (helper (apair-e1 in-e) fvars)
                                                     (helper (apair-e2 in-e) fvars))]
                           [(fst? in-e) (helper (fst-e in-e) fvars)]
                           [(snd? in-e) (helper (snd-e in-e) fvars)]
                           [(isaunit? in-e) (helper (isaunit-e in-e) fvars)]
                           [(mlet? in-e) (set-remove (helper (mlet-body in-e) fvars) (mlet-var in-e))]
                           [(call? in-e) (set-union (helper (call-funexp in-e) fvars)
                                                    (helper (call-actual in-e) fvars))]
                           [(fun? in-e) (set-subtract (helper (fun-body in-e) fvars) (if (fun-nameopt in-e)
                                                                                         (set (fun-nameopt in-e) (fun-formal in-e))
                                                                                         (set (fun-formal in-e))))]
                           [#t fvars]
                           ))])
    (cond [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
          [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                     (compute-free-vars (ifgreater-e2 e))
                                     (compute-free-vars (ifgreater-e3 e))
                                     (compute-free-vars (ifgreater-e4 e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e))
                             (compute-free-vars (apair-e2 e)))]
          [(fst? e) (compute-free-vars (fst-e e))]
          [(snd? e) (compute-free-vars (snd-e e))]
          [(isaunit? e) (compute-free-vars (isaunit-e e))]
          [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
          [(call? e) (call (compute-free-vars (call-funexp e))
                           (compute-free-vars (call-actual e)))]
          [(fun? e)
           (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (helper e (set)))]
          [#t e])))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e)
         (let ([e1 (eval-under-env-c (apair-e1 e) env)]
               [e2 (eval-under-env-c (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-pair")))]
        [(mlet? e)
         (let ([v (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(fun-challenge? e)
         (let ([filter-env (lambda (l s) (filter (lambda (x) (set-member? s (car x))) l))])
           (closure (filter-env env (fun-challenge-freevars e)) (if (fun-challenge-nameopt e)
                                                                    (fun-challenge (fun-challenge-nameopt e)
                                                                                   (fun-challenge-formal e)
                                                                                   (mlet (fun-challenge-nameopt e) e (fun-challenge-body e))
                                                                                   (fun-challenge-freevars e))
                                                                    e)))]
        [(closure? e)
         e]
        [(call? e)
         (let ([clsr (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? clsr)
               (let ([ext-env (cons (cons (fun-challenge-formal (closure-fun clsr)) arg) (closure-env clsr))])      
                 (eval-under-env-c (fun-challenge-body (closure-fun clsr)) ext-env))
               (error "MUPL function is expected, got" (call-funexp e))))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

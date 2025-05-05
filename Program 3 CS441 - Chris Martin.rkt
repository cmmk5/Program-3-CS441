#lang racket

(require data/either)

(define (my-div x y)
  
  (if (= y 0)
      (failure "Division by zero")
      (success (/ x y))))

(define (id-veri? sym) #| Verify if variable is valid |#
  
  (and (symbol? sym)
       (regexp-match? #rx"^[a-zA-Z][a-zA-Z0-9_-]*$" (symbol->string sym))))

(define (id-search id state) #| Search for id |#
  (cond
    
    [(assoc id state) => (λ (pair)
                           (let ([val (cdr pair)])
                             (if (equal? val 'undefined)
                                 (failure (format "Undefined variable: ~a" id))
                                 (success val))))]
    [else (failure (format "Variable ~a not located" id))]))

(define (upd-state id val state) #| Update the state of variable |#
  (cond
    
    [(assoc id state)
     (cons (cons id val)
           (remove* (list id) state (λ (a b) (equal? a (car b)))))]
    [else state]))

(define (def-id id val state) #| New variable state |#
  
  (if (assoc id state)
      (failure (format "Variable ~a is defined" id))
      (success (cons (cons id val) state))))

(define (rem-id id state)
  
  (if (assoc id state)
      (remove* (list id) state (λ (a b) (equal? a (car b))))
      (begin (printf "Error: remove ~a: ,variable not defined" id)
             state)))

(define (evl expr state) #| Evaluator |#
  (cond
    
    [(and (list? expr) (equal? (first expr) 'num))
     (success (list (second expr) state))]
    [(and (list? expr) (equal? (first expr) 'id))
     (match (id-search (second expr) state)
       [(success v) (success (list v state))]
       [(failure msg) (failure msg)])]
    [(and (list? expr) (member (first expr) '(add sub mult div)))
     (match (evl (second expr) state)
       [(failure msg) (failure msg)]
       [(success (list x state1))
        (match (evl (third expr) state1)
          [(failure msg2) (failure msg2)]
          [(success (list y state2))
           (case (first expr)
             [(add) (success (list (+ x y) state2))]
             [(sub) (success (list (- x y) state2))]
             [(mult) (success (list (* x y) state2))]
             [(div) (match (my-div x y)
                      [(success z) (success (list z state2))]
                      [(failure msg3) (failure msg3)])]
             [else (failure "Unknown operation??")])])])]

    [(and (list? expr) (equal? (first expr) 'define) (= (length expr) 2))
     (let ([id (second expr)])
       (if (id-veri? id)
           (match (def-id id 'undefined state)
             [(success state-new) (success (list 'ok state-new))]
             [(failure msg) (failure msg)])
           (failure (format "Invalid identifier: ~a" id))))]

    [(and (list? expr) (equal? (first expr) 'define) (= (length expr) 3))
     (let ([id (second expr)])
       (if (id-veri? id)
           (match (evl (third expr) state)
             [(failure msg) (failure msg)]
             [(success (list val s1))
              (match (def-id id val s1)
                [(success state-new) (success (list 'ok state-new))]
                [(failure msg2) (failure msg2)])])
           (failure (format "Invalid identifier: ~a" id))))]

    [(and (list? expr) (equal? (first expr) 'assign) (= (length expr) 3))
     (let ([id (second expr)])
       (if (assoc id state)
           (match (evl (third expr) state)
             [(failure msg) (failure msg)]
             [(success (list val state-new))
              (success (list 'ok (upd-state id val state-new)))])
           (failure (format "Assignment error: variable ~a not defined" id))))]

    [(and (list? expr) (equal? (first expr) 'remove) (= (length expr) 2))
     (let ([id (second expr)])
       (success (list 'ok (rem-id id state))))]

    [else (failure (format "Invalid expression: ~a" expr))]))

(define (loop-run state) #| repl loop |#
  
  (display "Enter expression here (or 'Quit): ")
  (flush-output)
  (define input (read))
  
  (if (equal? input 'Quit)
      (printf "Thanks for using my Evaluator!\n")
      (match (evl input state)
        [(failure msg)
         (printf "Failure: ~a\n\n" msg)
         (loop-run state)]
        [(success (list val utd-state))
         (printf "Success: ~a\n\n" val)
         (loop-run utd-state)])))

(loop-run '()) #| runs loop |#

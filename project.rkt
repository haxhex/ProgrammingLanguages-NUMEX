;; PL Project - Fall 2023
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions

;; newly added structs 
(struct div  (e1 e2)  #:transparent)
(struct mult  (e1 e2)  #:transparent)
(struct minus  (e1 e2)  #:transparent)
(struct bool (b) #:transparent)
(struct iseq  (e1 e2)  #:transparent)
(struct isls (e1 e2) #:transparent)
(struct isgt (e1 e2) #:transparent)  ;; greater-than two expressions
(struct cnd  (e1 e2 e3)  #:transparent)
(struct orelse  (e1 e2)  #:transparent)
(struct andalso  (e1 e2)  #:transparent)
(struct neg  (e1)  #:transparent)
(struct apair  (e1 e2) #:transparent)
(struct 1st  (e1) #:transparent)
(struct 2nd  (e1) #:transparent)
(struct with  (s e1 e2) #:transparent)

(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct tlam  (nameopt formal arg-type body) #:transparent) ;; a typed argument, recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r

(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5) #:transparent) ;; a letrec expression for recursive definitions

;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

(define (racketlist->numexlist xs)
  (cond ((equal? xs null) (munit))                    ; If the input list is empty, return the empty NUMEX list (munit).
        ((list? xs) (apair (car xs) (racketlist->numexlist (cdr xs))))  ; If the input is a non-empty list, create a NUMEX pair with the first element and recursively convert the rest.
        (#t (error "Error in racketlist->numexlist: argument type is not valid"))))

(define (numexlist->racketlist xs)
  (cond ((munit? xs) null)                            ; If the input NUMEX list is empty (munit), return an empty Racket list (null).
        ((apair? xs) (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs))))  ; If the input is a NUMEX pair, cons the first element and recursively convert the rest.
        (#t (error "Error in numexlist->racketlist: argument type is not valid"))))

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]   ; If the environment is empty, raise an error.
        [(list? env)                                                     ; If the environment is a non-empty list,
         (cond ((equal? str (car (car env))) (cdr (car env)))            ; Check if the variable matches the name of the first binding in the environment.
               (#t (envlookup (cdr env) str)))]                          ; If not, recursively search the rest of the environment.
        [#t (error "Error in envlookup: invalid argument type")]))       ; If the environment is not a list, raise an error.


;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.


;; this function gets record and string and returns the value of string
(define (getVal rec str)
  (cond ((munit? rec) rec)                             ; Base case: If the record is empty, return the empty record.
        ((equal? (key-s (record-k rec)) str)           ; If the current record's key matches the target string,
         (key-e (record-k rec)))                       ; return the value associated with that key.
        (#t (getVal (record-r rec) str))))             ; Recursive case: Move to the next record in the chain.

(define (eval-under-env e env)
  (cond [(var? e)                        ; Check if e is a variable using var? predicate
         (envlookup env (var-string e))] ; If true, look up the value in the environment using envlookup
        ; A variable evaluates to the value associated with it in the given environment.
        
        [(num? e) (cond ((integer? (num-int e)) e) (#t (error "NUMEX num does not contain a numeral value")))]
        [(bool? e) (cond ((boolean? (bool-b e)) e) (#t (error "NUMEX bool does not contain a boolean value")))]
        
        [(munit? e) e]
        [(closure? e) e]
        
        ; An arithmetic operation (addition, subtraction, multiplication, and division)
        ; evaluates to the result of what its operands evaluate to. Note: the operands must be numbers.          
        [(plus? e)                                      ; Check if e is a plus expression using plus? predicate
         (let ([v1 (eval-under-env (plus-e1 e) env)]    ; Evaluate the first operand of the plus expression
               [v2 (eval-under-env (plus-e2 e) env)])   ; Evaluate the second operand of the plus expression
           (if (and (num? v1)                           ; Check if both operands are numbers
                    (num? v2))
               (num (+ (num-int v1) (num-int v2)))      ; If true, perform addition and construct a new NUMEX number
               (error "NUMEX addition applied to non-number")))]   ; If operands are not numbers, raise an error


        [(minus? e) 
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]

        [(mult? e) 
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]

        [(div? e)                                      ; Check if e is a division expression using div? predicate
         (let ([v1 (eval-under-env (div-e1 e) env)]    ; Evaluate the numerator of the division expression
               [v2 (eval-under-env (div-e2 e) env)])   ; Evaluate the denominator of the division expression
           (if (and (num? v1)                          ; Check if both numerator and denominator are numbers
                    (num? v2))
               (cond ((equal? (num-int v2) 0)          ; Check if the denominator is zero
                      (error "NUMEX division by zero"))  ; If true, raise an error for division by zero
                     (#t (num (quotient (num-int v1) (num-int v2)))))  ; If denominator is non-zero, perform division and construct a new NUMEX number
               (error "NUMEX division applied to non-number")))]   ; If either numerator or denominator is not a number, raise an error

        ; A logical operation (andalso and orelse) evaluates to the result of what its
        ; operands evaluate to. Note: short-circuit evaluations are desired, and the operands must be booleans
        [(andalso? e)                                      ; Check if e is an andalso expression using andalso? predicate
         (let ([v1 (eval-under-env (andalso-e1 e) env)])   ; Evaluate the first operand of the andalso expression
           (if (bool? v1)                                  ; Check if v1 is a boolean
               (cond ((equal? v1 (bool #f)) (bool #f))     ; If v1 is #f, short-circuit and return #f
                     (#t (let ([v2 (eval-under-env (andalso-e2 e) env)])  ; Otherwise, evaluate the second operand
                           (cond ((bool? v2) (bool (and (bool-b v1) (bool-b v2))))  ; If v2 is a boolean, perform andalso
                                 (#t error "NUMEX andalso applied to non-boolean")))))  ; If v2 is not a boolean, raise an error
               (error "NUMEX andalso applied to non-boolean")))]  ; If v1 is not a boolean, raise an error


        [(orelse? e)                                      ; Check if e is an orelse expression using orelse? predicate
         (let ([v1 (eval-under-env (orelse-e1 e) env)])   ; Evaluate the first operand of the orelse expression
           (if (bool? v1)                                 ; Check if v1 is a boolean
               (cond ((equal? v1 (bool #t)) (bool #t))    ; If v1 is #t, short-circuit and return #t
                     (#t (let ([v2 (eval-under-env (orelse-e2 e) env)])  ; Otherwise, evaluate the second operand
                           (cond ((bool? v2) (bool (or (bool-b v1) (bool-b v2))))  ; If v2 is a boolean, perform orelse
                                 (#t error "NUMEX orelse applied to non-boolean")))))  ; If v2 is not a boolean, raise an error
               (error "NUMEX orelse applied to non-boolean")))]  ; If v1 is not a boolean, raise an error
        
        ; A negation (neg e) evaluates to the opposite (negation) of what e evaluates to.
        ; Note: e can be a number or a boolean.
        [(neg? e) 
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond  ((bool? v1) (bool (not (bool-b v1))))
                  ((num? v1) (num (* -1 (num-int v1))))  
               (#t (error "NUMEX neg applied to non-boolean or non-number"))))]

        ; For the expression (cnd e1 e2 e3), e1 is first evaluated to obtain a boolean value.
        ; If the resulting value is (bool #t), the entire expression evaluates to the value of e2.
        ; Otherwise, the expression evaluates to the value of e3.
        [(cnd? e)                                      ; Check if e is a conditional expression using cnd? predicate
         (let ([v1 (eval-under-env (cnd-e1 e) env)])   ; Evaluate the condition of the conditional expression
           (if (bool? v1)                              ; Check if v1 is a boolean
               (cond ((equal? v1 (bool #t)) (eval-under-env (cnd-e2 e) env))     ; If true, evaluate the true branch
                     ((equal? v1 (bool #f)) (eval-under-env (cnd-e3 e) env)))    ; If false, evaluate the false branch
               (error "NUMEX conditional's condition applied to non-binary")))]  ; If v1 is not a boolean, raise an error
        
        ; The evaluation of (iseq e1 e2) involves the evaluation of e1 and e2.
        ; The resulting value is (bool #t) if the value of e1 equals the value of e2.
        ; Otherwise, the expression evaluates to (bool #f). Note: e1 and e2 can be numbers or booleans.
        [(iseq? e)                                     ; Check if e is an iseq expression using iseq? predicate
         (let ([v1 (eval-under-env (iseq-e1 e) env)]   ; Evaluate the first operand of the iseq expression
               [v2 (eval-under-env (iseq-e2 e) env)])  ; Evaluate the second operand of the iseq expression
           (if (and (or (bool? v1) (num? v1))          ; Check if v1 is a boolean or a number
                    (or (bool? v2) (num? v2)))         ; Check if v2 is a boolean or a number
               (cond ((equal? v1 v2) (bool #t))        ; If v1 equals v2, return (bool #t)
                     (#t (bool #f)))                   ; Otherwise, return (bool #f)
               (cond ((not (and (or (bool? v1) (num? v1))
                                (or (bool? v2) (num? v2)))) (error "NUMEX iseq applied to neither boolean nor num"))
                     ((not (or (and (bool? v1) (bool? v2))
                               (and (num? v1) (num? v2)))) (error "NUMEX iseq applied to different types")))))]   ; Raise errors for invalid types

        ; The evaluation of (isls e1 e2) involves the evaluation of e1 and e2.
        ; The resulting value is (bool #t) if the value of e1 is less than the value of e2.
        ; Otherwise, the expression evaluates to (bool #f). Note: e1 and e2 can only be numbers.
        [(isls? e)                                     ; Check if e is an isls expression using isls? predicate
         (let ([v1 (eval-under-env (isls-e1 e) env)]   ; Evaluate the first operand of the isls expression
               [v2 (eval-under-env (isls-e2 e) env)])  ; Evaluate the second operand of the isls expression
           (if (and (num? v1) (num? v2))               ; Check if both v1 and v2 are numbers
               (bool (< (num-int v1) (num-int v2)))    ; If true, construct a bool expression based on the numeric comparison
               (error "NUMEX 'isls' condition applied to non-numerics")))]  ; Raise an error if either v1 or v2 is not a number

        ; The evaluation of (isgt e1 e2) involves the evaluation of e1 and e2.
        ; The resulting value is (bool #t) if the value of e1 is greater than the value of e2.
        ; Otherwise, the expression evaluates to (bool #f). Note: e1 and e2 can only be numbers.
        [(isgt? e)                                     ; Check if e is an isgt expression using isgt? predicate
         (let ([v1 (eval-under-env (isgt-e1 e) env)]   ; Evaluate the first operand of the isgt expression
               [v2 (eval-under-env (isgt-e2 e) env)])  ; Evaluate the second operand of the isgt expression
           (if (and (num? v1) (num? v2))               ; Check if both v1 and v2 are numbers
               (bool (> (num-int v1) (num-int v2)))    ; If true, construct a bool expression based on the numeric comparison
               (error "NUMEX 'isgt' condition applied to non-numerics")))]  ; Raise an error if either v1 or v2 is not a number

        ; For (with s e1 e2), the expression e2 evaluates to a value in an environment extended to map the name s to the evaluated value of e1.
        ; Extend the current environment by adding a binding for the symbol s with the value obtained from evaluating e1.
        [(with? e)                                      ; Check if e is a with expression using with? predicate
         (let ([v1 (eval-under-env (with-e1 e) env)])   ; Evaluate the first operand of the with expression
           (if (string? (with-s e))                     ; Check if the second operand of with is a string
               (eval-under-env (with-e2 e) (cons (cons (with-s e) v1) env))   
               (error "NUMEX with applied to non-string")))]   ; Raise an error if s operand is not a string


        [(lam? e)
           (if (and (or (string? (lam-nameopt e)) (equal? (lam-nameopt e) null)) (string? (lam-formal e)))
               (closure env e)
               (error "NUMEX lam applied to non-string"))]

        [(tlam? e)
           (if (and (or (string? (tlam-nameopt e)) (equal? (tlam-nameopt e) null)) (string? (tlam-arg-type e)) (string? (tlam-formal e)))
               (closure env e)
               (error "NUMEX tlam applied to non-string"))]

        ; For (apply e1 e2), the expression e1 first evaluates to a value.
        ; If the resulting value is not a closure, an error should be raised.
        ; Otherwise, it evaluates the closure's function's body in the closure's environment,
        ; which is extended to map the function's name to the closure (unless the name field is null),
        ; and the function's argument to the result of the evaluation of e2.
        ; This condition checks if the nameopt of the closure v1 is null.
        ; If it is, it indicates that the closure does not have a name.
        ; In this case, the code evaluates the closure's body with the formal parameter bound to the result of evaluating the actual parameter.
        ; On the other hand, if the nameopt is not null, it means that the closure has a name.
        ; In this case, the code evaluates the closure's body with the nameopt bound to the closure,
        ; and the formal parameter is bound to the result of evaluating the actual parameter.
        ; in lambda calculus or functional programming languages, closures (lambda functions) have a name, and nameopt refers to an optional name associated with the closure.
        [(apply? e) 
         (let ([v1 (eval-under-env (apply-funexp e) env)])         ; Evaluate the function expression in the apply expression
           (if (closure? v1)                                       ; Check if the result is a closure
               (cond ((equal? null (lam-nameopt (closure-f v1)))   ; Check if the closure has a null (nameopt)
                      (eval-under-env (lam-body (closure-f v1))    ; If true, evaluate the closure's body with the formal parameter bound to the result of evaluating the actual parameter
                                      (cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env)) (closure-env v1))))
                     (#t (eval-under-env (lam-body (closure-f v1))   ; If the closure has a non-null nameopt,
                                         ; evaluate the closure's body with the nameopt bound to the closure and the formal parameter bound to the result of evaluating the actual parameter
                                         (cons (cons (lam-nameopt (closure-f v1)) v1)
                                               (cons (cons (lam-formal (closure-f v1)) (eval-under-env (apply-actual e) env))  (closure-env v1))))))
               (cond ((lam? v1) (eval-under-env (apply v1 (apply-actual e)) env))   ; If the result is a lambda function, apply it to the actual parameters
                     (#t (error "NUMEX ~v not a function" (apply-funexp e))))))]     ; Raise an error if the result is neither a closure nor a lambda function

        ; The (apair e1 e2) construct creates a new pair holding the results of the evaluations of e1 and e2. 
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]   ; Evaluate the first component of the pair
               [v2 (eval-under-env (apair-e2 e) env)])  ; Evaluate the second component of the pair
           (apair v1 v2))]   ; Create a new pair with the evaluated values of the components

        ; If the evaluation result of e1 in (1st e1) is a pair, the first component of the pair is returned.
        ; Otherwise, an error is returned. Similarly, the evaluation result of (2nd e1) is the second component of the given pair.
        ; Expression of the form (1st e)
        [(1st? e) 
         (let ([v1 (eval-under-env (1st-e1 e) env)])       ; Evaluate the subexpression (1st-e1 e) to get v1
           (if (apair? v1)                                 ; Check if v1 is a pair
               (apair-e1 v1)                               ; If true, return the first component of the pair
               (error "NUMEX 1st applied to non-apair")))] ; If v1 is not a pair, raise an error

        ; Expression of the form (2nd e)
        [(2nd? e) 
         (let ([v1 (eval-under-env (2nd-e1 e) env)])       ; Evaluate the subexpression (2nd-e1 e) to get v1
           (if (apair? v1)                                 ; Check if v1 is a pair
               (apair-e2 v1)                               ; If true, return the second component of the pair
               (error "NUMEX 2nd applied to non-apair")))] ; If v1 is not a pair, raise an error

        [(ismunit? e) 
         (let ([v1 (eval-under-env (ismunit-e e) env)])
           (cond ((munit? v1) (bool #t))
                 (#t (bool #f))))]

        ; expression e5 evaluates to a value in an environment extended to map the name s1 to the evaluated value of e1 
        ; and the name s2 to the evaluated value of e2 and the name s3 to the evaluated value of e3 and the name s4 to the evaluated value of e4.
        [(letrec? e)
         (cond
           ((and (string? (letrec-s1 e)) (string? (letrec-s2 e)) (string? (letrec-s3 e)) (string? (letrec-s4 e)))
            (eval-under-env (letrec-e5 e) (cons (cons (letrec-s1 e) (letrec-e1 e)) (cons (cons (letrec-s2 e) (letrec-e2 e))
                                                                                         (cons (cons (letrec-s3 e) (letrec-e3 e))
                                                                                               (cons (cons (letrec-s4 e) (letrec-e4 e)) env))))))   
           (#t (error "NUMEX letrec applied to non-string")))]

        ; if s is a Racket string and the result of evaluating e is a NUMEX expression, the
        ; (key s e) construct makes a key holding the corresponding value of s which is e. Otherwise, it returns an error
        [(key? e) 
         (let ([v1 (eval-under-env (key-e e) env)])
           (cond ((string? (key-s e)) (key (key-s e) v1))
                 (#t (error "NUMEX key applied to non-string"))))]

        ; If the result of evaluating k is a key and the result of evaluating m is a munit, 
        ; the (record k m) construct makes a record holding the result of the evaluation of k
        ; and the result of the evaluation of m. Otherwise, it returns an error
        ; If the result of evaluating k is a key and the result of evaluating r is a record, the
        ; (record k r) construct makes a record holding the results of the evaluation of k
        ; and the evaluation of r. Otherwise, it returns an error
         [(record? e) 
          (let ([v1 (eval-under-env (record-k e) env)]
                [v2 (eval-under-env (record-r e) env)])
           (if (key? v1)
               (cond ((or (munit? v2) (record? v2)) (record v1 v2))
                     (#t (error "NUMEX record input should be a record or munit")))
                     (error "NUMEX record input is not a key")))]

         ; if the result of evaluating s is a string and the result of evaluating r is a record, the
         ; (value s r) returns the corresponding value of s in r. If s does not exist in r, the
         ; (value s r) returns (munit). Otherwise, it returns an error.
         [(value? e) 
           (let ([v1 (eval-under-env (value-r e) env)])
             (if (string? (value-s e))
                 (cond ((record? v1) (getVal v1 (value-s e)))
                       (#t (error "NUMEX value input is not a record")))
               (error "NUMEX value input is not a string")))]
         
     
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3 - Implementing a simple type system for NUMEX
; A simple type system for NUMEX. It should be a Racket function inferexp that takes a NUMEX expression e and either returns the NUMEX type that
; e evaluates to, under the empty environment or calls Racket's error if evaluation encounters a type error.
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]
        
        ; The type of an arithmetic operation (addition, subtraction, multiplication,
        ; and division) is inferred as integer if both operands are inferred as integers,
        ; otherwise, an error should be raised (implement this inference rule just for addition)
        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]

        
        ; Integer: a type for numeric values which is represented as a Racket string “int”
        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        ; Boolean: a type for boolean values (true and false) which is represented as a Racket string “bool”.
        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        ; The type of a logical operation (andalso and orelse) is inferred as boolean if
        ; both operands are inferred as booleans, otherwise, an error should be raised
        ;(implement this inference rule just for andalso).
        [(andalso? e) 
         (let ([t1 (infer-under-env (andalso-e1 e) env)]
               [t2 (infer-under-env (andalso-e2 e) env)])
           (if (and (equal? "bool" t1)
                    (equal? "bool" t2))
               "bool"
               (error "NUMEX TYPE ERROR: andalso applied to non-boolean")))]

        ; The type of a negation (neg e) is inferred as the type of its argument expression (e).
        [(neg? e)
         (cond
           [(equal? (infer-under-env (neg-e1 e) env) "bool") "bool"]
           [(equal? (infer-under-env (neg-e1 e) env) "int") "int"]
           [#t (error "NUMEX TYPE ERROR: negation should be applied to boolean or integer type")])]

        ; For (cnd e1 e2 e3), if e1 is inferred as a boolean and e2 and e3 are both inferred
        ; as the same type, the whole expression’s type is inferred as the type of e2 (or e3). 
        ; Otherwise, an error should be raised.
        [(cnd? e) 
         (let ([t (infer-under-env (cnd-e1 e) env)])
           (if (equal? t "bool")
               (cond ((equal? (infer-under-env (cnd-e2 e) env) (infer-under-env (cnd-e3 e) env)) (infer-under-env (cnd-e2 e) env) )
                     (#t (error "NUMEX TYPE ERROR: cnd branches should have the same type")))                    
               (error "NUMEX TYPE ERROR: condition should be a boolean value")))]

        ; The type of (iseq e1 e2) is inferred as boolean if e1 and e2 are both inferred as
        ; the same type. Otherwise, an error should be raised.
         [(iseq? e)
             (cond ((equal? (infer-under-env (iseq-e1 e) env) (infer-under-env (iseq-e2 e) env)) "bool" )
                     (#t (error "NUMEX TYPE ERROR: iseq inputs should have the same type")))]

         ; The type of (isls e1 e2) is inferred as boolean if e1 and e2 are both inferred as
         ; integers. Otherwise, an error should be raised.
         [(isls? e)
          (let ([t1 (infer-under-env (isls-e1 e) env)]
                [t2 (infer-under-env (isls-e2 e) env)])
            (if (and (equal? t1 "int") (equal? t2 "int"))
                "bool"
                (error "NUMEX TYPE ERROR: 'isls' applied to non-integers")))]

         ; The type of (isgt e1 e2) is inferred as boolean if e1 and e2 are both inferred as
         ; integers. Otherwise, an error should be raised.
         [(isgt? e)
          (let ([t1 (infer-under-env (isgt-e1 e) env)]
                [t2 (infer-under-env (isgt-e2 e) env)])
            (if (and (equal? t1 "int") (equal? t2 "int"))
                "bool"
                (error "NUMEX TYPE ERROR: 'isgt' applied to non-integers")))]

         ; For (with s e1 e2), the expression e2 is inferred as a type in an
         ; environment extended to map the name s to the inferred type of e1.
         [(with? e) 
           (if (string? (with-s e))
               (infer-under-env (with-e2 e) (cons (cons (with-s e) (infer-under-env (with-e1 e) env)) env))               
               (error "NUMEX TYPE ERROR: with should be applied to a string"))]

         ; For functions, we consider two assumptions:
         ;   1. The function expression is constructed with tlam instead of lam, which contains the function argument type in it.
         ;   2. There is no use of apply in the function’s body expression.
         ; The function’s type is inferred as (function T1 T2), where T1 is the type of the function’s
         ; argument, and T2 is the type of the function’s body expression. T2 is inferred as a type
         ; in an environment extended to map the function’s argument to its type that is given in the tlam structure.
         ; Check if the expression is a tlam (typed lambda) expression
         [(tlam? e)
          ; Check if the argument type of tlam is "int", "bool", or "null"
          (if (or (equal? (tlam-arg-type e) "int") (equal? (tlam-arg-type e) "bool") (equal? (tlam-arg-type e) "null")) 
              ; Check if tlam has a valid structure with optional name, formal parameter, and argument type
              (if (and (or (string? (tlam-nameopt e)) (equal? (tlam-nameopt e) null)) (string? (tlam-formal e)) (string? (tlam-arg-type e)))
                  ; If the structure is valid, infer the type as (function T1 T2), where T1 is the argument type, and T2 is the body type
                  (function  (tlam-arg-type e) (infer-under-env (tlam-body e) (cons (cons (tlam-formal e) (tlam-arg-type e)) env)))  
                  ; Raise an error if tlam has an invalid structure
                  (error "NUMEX TYPE ERROR: tlam applied to non-string"))
              ; Raise an error if the argument type of tlam is not recognized
              (error "NUMEX TYPE ERROR: tlam arg-type not recognized"))]

         ; For (apply e1 e2), the expression e1 first evaluates to a value.
         ; If the resulting value is not a closure, an error should be raised.
         ; Otherwise, it evaluates the closure's function's body in the closure's environment,
         ; which is extended to map the function's name to the closure (unless the name field is null),
         ; and the function's argument to the result of the evaluation of e2.
         [(apply? e) 
          (let ([t1 (infer-under-env (apply-funexp e) env)] ; Infer the type of the function expression in the apply expression
                [t2 (infer-under-env (apply-actual e) env)]) ; Infer the type of the actual parameter expression in the apply expression
            (if (function? t1) ; Check if the result is a function
                ; checking if the type of the actual parameter matches the expected input type of the function
                (cond ((equal? (function-input-type t1) t2) (function-output-type t1)) ; Check if the types match
                      (#t (error "NUMEX TYPE ERROR: apply argument type mismatch")))
                (error "NUMEX TYPE ERROR: apply input is not a function")))] ; Raise an error if the result is not a function


         ; Null: a type for null value in NUMEX (munit) which is represented as a Racket string “null”.
         [(munit? e)
         "null"]

         ; The type of (apair e1 e2) is (collection T) if e1 is inferred as a T and e2 is inferred as either (collection T) or null.
         ; Otherwise, an error should be raised.
        [(apair? e) 
         (let ([t1 (infer-under-env (apair-e1 e) env)]
               [t2 (infer-under-env (apair-e2 e) env)])
           (if (or (equal? t2 (collection t1)) (equal? t2 "null"))
                    (collection t1)
               (error "NUMEX TYPE ERROR: apair's second input should be null or a collection with the correct type")))]

        ; if an expression like e is inferred as a (collection T), (1st e ) is inferred as a T.
        ; Otherwise, an error should be raised.
        [(1st? e) 
         (let ([t1 (infer-under-env (1st-e1 e) env)])
           (if (collection? t1)
                    (collection-type t1)
               (error "NUMEX TYPE ERROR: 1st input is not apairs")))]

        ;  If an expression like e is inferred as a (collection T), (2nd e ) is inferred as a (collection T).
        ; Otherwise, an error should be raised.
        [(2nd? e) 
         (let ([t1 (infer-under-env (2nd-e1 e) env)])
           (if (collection? t1)
                    t1
               (error "NUMEX TYPE ERROR: 2nd input is not apairs")))]

        ; The type of (ismunit e1) is inferred as boolean if the expression e1 is inferred
        ; as either (collection T) or null. Otherwise, an error should be raised.    
        [(ismunit? e) 
         (let ([t1 (infer-under-env (ismunit-e e) env)])
           (if (or (collection? t1) (equal? t1 "null"))
                    "bool"
               (error "NUMEX TYPE ERROR: ismunit input has an invalid type")))]

        

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4 - Extending the Language

; Takes three NUMEX expressions e1, e2, and e3.
; Returns a NUMEX expression that first evaluates e1.
; If the resulting value is NUMEX's munit, then it evaluates e2 and that is the overall result.
; Otherwise, e3 must be evaluated.
(define (ifmunit e1 e2 e3)
  (cnd (ismunit e1) e2 e3))

; Takes a Racket list of Racket pairs '((s1 . e1)... (si . ei)... (sn . en))
; and a final NUMEX expression en+1.
; Returns a NUMEX expression whose value is en+1 evaluated in an environment
; where each si is a variable bound to the result of evaluating the corresponding ei
; for 1<=i<=n. The bindings are done sequentially.
(define (with* bs e2)
  (cond ((null? bs) e2) ; Base case: if the list is empty, return the final expression e2
        (#t (with (car (car bs)) (cdr (car bs)) (with* (cdr bs) e2))))) ; Bind variables sequentially and recursively process the rest of the list.

; Takes four NUMEX expressions e1, e2, e3, and e4.
; Returns a NUMEX expression where e3 is evaluated if and only if e1 and e2 are not
; equal numbers/booleans. Otherwise, the whole expression evaluates to what e4 evaluates to.
; Assumes none of the arguments use the NUMEX variables _x or _y.
; Uses this assumption so that when an expression returned from ifneq is evaluated,
; e1 and e2 are evaluated exactly once each.
(define (ifneq e1 e2 e3 e4)
 (cnd (iseq e1 e2) e4 e3))



;; Problem 5 - Using the Language

; A NUMEX function that acts like the foldr function in Racket.
; Takes a binary function "f", an initial value "base", and a NUMEX list "lst".
; If the list is empty, returns the initial value.
; Otherwise, applies the binary function to the first element of the list and the result
; of folding the rest of the list using the same function and initial value.
(define numex-fold
  (lam null "f"
    (lam "recursion" "base"
      (lam "nfold" "lst"
        (ifmunit (var "lst") ; If the list is empty
            (var "base")     ; Return the initial value
            (apply (apply (var "f") (1st (var "lst"))) ; Apply the binary function to the first element of the list
                   (apply (apply (var "recursion") (var "base")) ; Folding the rest
                          (2nd (var "lst"))))))))) ; Recursive call on the rest of the list

; Using numex-fold, binds to the Racket variable numex-concat a
; NUMEX function that takes a NUMEX list xs and returns a NUMEX function that
; takes a NUMEX list ys and returns a new NUMEX list that is the concatenation of xs and ys.
(define numex-concat
  (with "nfold" numex-fold
    (lam null "xs"
      (lam null "ys"
        (apply (apply (apply (var "nfold") (lam null "x" (lam null "r" (apair (var "x") (var "r"))))) (var "ys")) (var "xs"))))))


(define sum-up-to-n
  (lam "sum-up-to-n" "n"
       (cnd (iseq (var "n") (num 0))
            (num 0)
            (plus (var "n") (apply (var "sum-up-to-n") (minus (var "n") (num 1)))))))


; Using numex-concatenate, binds to the Racket variable numex-bind. 
; numex-bind is a function that takes a non-deterministic value (a NUMEX list) 
; and a non-deterministic function and returns a new non-deterministic value (a NUMEX list) 
; by applying the non-deterministic function to the non-deterministic value.
; For example, calling numex-bind with [3,4,5] and λx -> [x, -x] must return [3,-3,4,-4,5,-5].
; Note that the function is curried.
(define numex-bind
  (with "nconcat" numex-concat
    (with "nfold" numex-fold
      (lam null "f"       ; Take a non-deterministic function f
        (lam null "xs"    ; Take a non-deterministic value xs (a NUMEX list)
          (apply (apply (apply (var "nfold") ; Use numex-fold to fold over xs
                               (lam null "x"
                                 (lam null "r"
                                   ; Concatenate the result of applying f to x with the current accumulator r
                                   (apply (apply (var "nconcat") (apply (var "f") (var "x")))
                                          (var "r"))))) ; This lambda now concatenates.
                       (munit)) ; An empty NUMEX list as the starting accumulator value.
              (var "xs")))))))

;; Problem 6 - Exploring the type system

; Two parts of condition's types are not identical but evaluates without error.
(define type-error-but-evaluates-ok
  (cnd (bool #t)
       (bool #t)
       (num 6)))
; (eval-exp type-error-but-evaluates-ok)
; (infer-exp type-error-but-evaluates-ok)


(define type-ok-but-evaluates-error
  (plus (num 6) (1st (2nd (apair (num 6) (munit))))))
; (infer-exp type-ok-but-evaluates-error)
; (eval-exp type-ok-but-evaluates-error)

(define type-ok-but-evaluates-error-2
  (iseq (tlam null "h" "int" (plus (num 6) (var "h")))
        (tlam null "h" "int" (plus (num 6) (var "h")))))
; (infer-exp type-ok-but-evaluates-error-2)
; (eval-exp type-ok-but-evaluates-error-2)

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent)

;; We will test this function directly, so it must do
;; as described in the assignment

; This function takes a NUMEX expression e and computes a new expression that replaces functions with fun-challenge and sets their free variable
; Uses the compute-free-variables-set function to calculate the set of free variables for each function and constructs a fun-challenge with the calculated free variables.
(define (compute-free-vars e)
  (cond[(var? e) e]
       [(lam? e) (fun-challenge (lam-nameopt e) (lam-formal e) (compute-free-vars (lam-body e)) (compute-free-variables-set e))]
       [(with? e) (with (with-s e) (compute-free-vars (with-e1 e)) (compute-free-vars (with-e2 e)))]
       [(apply? e) (apply (compute-free-vars (apply-funexp e)) (compute-free-vars (apply-actual e)))]
       [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
       [(1st? e) (1st (compute-free-vars (1st-e1 e)))]
       [(2nd? e) (2nd (compute-free-vars (2nd-e1 e)))]
       [(cnd? e) (cnd (compute-free-vars (cnd-e1 e)) (compute-free-vars (cnd-e2 e)) (compute-free-vars (cnd-e3 e)))]
       [(iseq? e) (iseq (compute-free-vars (iseq-e1 e)) (compute-free-vars (iseq-e2 e)))]
       [(ismunit? e) (ismunit (compute-free-vars (ismunit-e e)))]
       [(isls? e) (isls (compute-free-vars (isls-e1 e)) (compute-free-vars (isls-e2 e)))]
       [(isgt? e) (isgt (compute-free-vars (isgt-e1 e)) (compute-free-vars (isgt-e2 e)))]
       [(andalso? e) (andalso (compute-free-vars (andalso-e1 e)) (compute-free-vars (andalso-e2 e)))]
       [(orelse? e) (orelse (compute-free-vars (orelse-e1 e)) (compute-free-vars (orelse-e2 e)))]
       [(plus? e) (plus (compute-free-vars (plus-e1 e)) (compute-free-vars (plus-e2 e)))]
       [(minus? e) (minus (compute-free-vars (minus-e1 e)) (compute-free-vars (minus-e2 e)))]
       [(mult? e) (mult (compute-free-vars (mult-e1 e)) (compute-free-vars (mult-e2 e)))]
       [(div? e) (div (compute-free-vars (div-e1 e)) (compute-free-vars (div-e2 e)))]
       [(neg? e) (neg (compute-free-vars (neg-e1 e)))]
       [(num? e) e]
       [(bool? e) e]
       [(closure? e) e]
       [(string? e) e]
       [(munit? e) e]
       [#t (error "NUMEX Compute free vars wrong type" e)]))
    
; This function recursively calculates the set of free variables for a given expression e.
; It returns a set containing the free variables found in the expression.
(define (compute-free-variables-set e)
  (cond
    [(var? e) (set (var-string e))] ; If the expression is a variable, create a set containing its string representation.
    ; For a lambda expression, calculate free variables by removing the lambda's name and formals from the set of free variables in its body.
    [(lam? e) (set-remove (set-remove (compute-free-variables-set (lam-body e)) (lam-nameopt e)) (lam-formal e))]
    ; For a 'with' expression, union the set of free variables in its first and second parts, removing the shadowed variable if necessary.
    ; Removes the variables specified by the shadow set (with s e1 e2)
    ; When a variable is shadowed, it means that a new variable with the same name is introduced in a nested scope,
    ; and it temporarily hides or shadows the outer variable with the same name.
    [(with? e) (set-union (set-remove (compute-free-variables-set (with-e2 e)) (with-s e)) (compute-free-variables-set (with-e1 e)))]
    ; For an 'apply' expression, union the set of free variables in its function and actual arguments.
    [(apply? e) (set-union (compute-free-variables-set (apply-funexp e)) (compute-free-variables-set (apply-actual e)))]
    [(apair? e) (set-union (compute-free-variables-set (apair-e1 e)) (compute-free-variables-set (apair-e2 e)))] 
    [(1st? e) (compute-free-variables-set (1st-e1 e))] 
    [(2nd? e) (compute-free-variables-set (2nd-e1 e))]
    [(cnd? e) (set-union (compute-free-variables-set (cnd-e1 e)) (compute-free-variables-set (cnd-e2 e)) (compute-free-variables-set (cnd-e3 e)))]
    ; Union the sets of free variables in its two parts.
    [(iseq? e) (set-union (compute-free-variables-set (iseq-e1 e)) (compute-free-variables-set (iseq-e2 e)))] 
    [(isls? e) (set-union (compute-free-variables-set (isls-e1 e)) (compute-free-variables-set (isls-e2 e)))] 
    [(isgt? e) (set-union (compute-free-variables-set (isgt-e1 e)) (compute-free-variables-set (isgt-e2 e)))]
    [(ismunit? e) (compute-free-variables-set (ismunit-e e))]
    [(andalso? e) (set-union (compute-free-variables-set (andalso-e1 e)) (compute-free-variables-set (andalso-e2 e)))] 
    [(orelse? e) (set-union (compute-free-variables-set (orelse-e1 e)) (compute-free-variables-set (orelse-e2 e)))]
    [(plus? e) (set-union (compute-free-variables-set (plus-e1 e)) (compute-free-variables-set (plus-e2 e)))]
    [(minus? e) (set-union (compute-free-variables-set (minus-e1 e)) (compute-free-variables-set (minus-e2 e)))]
    [(mult? e) (set-union (compute-free-variables-set (mult-e1 e)) (compute-free-variables-set (mult-e2 e)))] 
    [(div? e) (set-union (compute-free-variables-set (div-e1 e)) (compute-free-variables-set (div-e2 e)))]
    [(neg? e) (compute-free-variables-set (neg-e1 e))] 
    [(num? e) (set)] ; For a 'num' expression, return an empty set as there are no free variables.
    [(bool? e) (set)]
    [(string? e) (set)]
    [(closure? e) (set)]
    [(munit? e) (set)] 
    [#t (error (format "bad NUMEX expression: ~v" e))]))

; Given the free variables of a function and an environment, returns a new environment for a closure env f
; Create a new environment that only contains variables from the original environment that are free in the current function.
(define (create-new-env env free)
  ; If the environment is empty, return an empty environment.
  (cond[(null? env) null]
       ; If the first variable in the environment is a free variable,
       ; include it in the new environment and recursively call create-new-env for the rest of the environment.
       ; Update the set of free variables, removing the current variable
       [(set-member? free (car (car env))) (cons (car env) (create-new-env (cdr env) (set-remove free (car (car env)))))]
       ; If the first variable is not a free variable, skip it and continue with the rest of the environment.
       [#t (create-new-env (cdr env) free)]))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
; This function is similar to the original eval-under-env, but it uses the computed free variables when building closures.
; It constructs closures with smaller environments that include only the free variables of the function.
(define (eval-under-env-c e env)
  (cond [(var? e)
         ; If the expression is a variable, evaluate it by looking up its value in the environment.
         (let ([v1 (eval-under-env-c (var-string e) env)])
         (if (string? (var-string e))
             (envlookup env (var-string e))
             (error "NUMEX var applied to non-string")))]

       [(fun-challenge? e)
        ; Check function name and parameter name are string
        (if (and (or (string? (fun-challenge-nameopt e)) (null? (fun-challenge-nameopt e))) (string? (fun-challenge-formal e)))
            ; Create a closure with a new environment that includes only the free variables of the function.
            (closure (create-new-env env (fun-challenge-freevars e)) e)
            (error "NUMEX function name and parameter name must be string"))]
        
       [(with? e)
        (eval-under-env-c (with-e2 e) (cons (cons (with-s e) (eval-under-env-c (with-e1 e) env)) env))]

       [(apply? e)
        (let ([v1 (eval-under-env-c (apply-funexp e) env)]
              [v2 (eval-under-env-c (apply-actual e) env)])
          (if (closure? v1)
              (if (null? (fun-challenge-nameopt (closure-f v1)))
                  (eval-under-env-c (fun-challenge-body (closure-f v1)) (cons (cons (fun-challenge-formal (closure-f v1)) v2) (closure-env v1)))
                  (eval-under-env-c (fun-challenge-body (closure-f v1)) (cons (cons (fun-challenge-nameopt (closure-f v1)) v1) (cons (cons (fun-challenge-formal (closure-f v1)) v2) (closure-env v1)))))
              (error  "NUMUX apply applied to non-closure" v1 (apply-funexp e))))]

       [(apair? e)
        (let ([v1 (eval-under-env-c (apair-e1 e) env)]
              [v2 (eval-under-env-c (apair-e2 e) env)])
              (apair v1 v2))]

       [(1st? e)
        (let ([v1 (eval-under-env-c (1st-e1 e) env)])
          (if (apair? v1)
              (apair-e1 v1)
              (error "NUMUX 1st applied to non-apair")))]

       [(2nd? e)
        (let ([v1 (eval-under-env-c (2nd-e1 e) env)])
          (if (apair? v1)
              (apair-e2 v1)
              (error "NUMUX 1st applied to non-apair")))]

       [(cnd? e)
        (let ([v1 (eval-under-env-c (cnd-e1 e) env)])
              (if (bool? v1)
                  (if (bool-b v1)
                      (eval-under-env-c (cnd-e2 e) env)
                      (eval-under-env-c (cnd-e3 e) env))
                  (error "NUMUX cnd guard applied to non-boolean")))]

       [(iseq? e)
        (let ([v1 (eval-under-env-c (iseq-e1 e) env)]
              [v2 (eval-under-env-c (iseq-e2 e) env)])
              (cond
                [(and (num? v1)(num? v2))
                 (bool (eq? (num-int v1) (num-int v2)))]
                [(and (bool? v1)(bool? v2))
                 (bool (eq? (bool-b v1)(bool-b v2)))]
                [#t (bool #f)]))]

       
          [(isls? e)
         (let ([v1 (eval-under-env-c (isls-e1 e) env)]
               [v2 (eval-under-env-c (isls-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (bool (< (num-int v1) (num-int v2)))
               (error "NUMEX 'isls' condition applied to non-numerics")))]

         [(isgt? e)
         (let ([v1 (eval-under-env-c (isgt-e1 e) env)]
               [v2 (eval-under-env-c (isgt-e2 e) env)])
           (if (and (num? v1) (num? v2))
               (bool (> (num-int v1) (num-int v2)))
               (error "NUMEX 'is' condition applied to non-numerics")))]


       [(ismunit? e)
        (let ([v1 (eval-under-env-c (ismunit-e e) env)])
              (bool (munit? v1)))]

        [(andalso? e)
          ; Evaluate the first operand and store the result in v1
         (let ([v1 (eval-under-env-c (andalso-e1 e) env)])
            ; Determine the truthiness of v1 and store it in v3 (bool or number(if 0 set flase else true))
           (let ([v3 (cond[(bool? v1) v1]
                          [(num? v1) (if (eq? (num-int v1) 0) (bool #f) (bool #t))]
                          [#t error "NUMEX and-also applied to non-number or non-boolean"])])
             ; If v3 is a boolean and is false, short-circuit to return false
             (if (and (bool? v3) (eq? (bool-b v3) #f))
               (bool #f)
               (let ([v2 (eval-under-env-c (andalso-e2 e) env)])
           (let ([v4 (cond[(bool? v2) v2]
                          [(num? v2) (if (eq? (num-int v2) 0) (bool #f) (bool #t))]
                          [#t error "NUMEX and-also applied to non-number or non-boolean"])]) v4)))))]
        
        [(orelse? e)
         (let ([v1 (eval-under-env-c (orelse-e1 e) env)])
           (let ([v3 (cond[(bool? v1) v1]
                          [(num? v1) (if (eq? (num-int v1) 0) (bool #f) (bool #t))]
                          [#t error "NUMEX orelse applied to non-number or non-boolean"])])
           (if (and (bool? v3) (eq? (bool-b v3) #t))
               (bool #t)
               (let ([v2 (eval-under-env-c (orelse-e2 e) env)])
           (let ([v4 (cond[(bool? v2) v2]
                          [(num? v2) (if (eq? (num-int v2) 0) (bool #f) (bool #t))]
                          [#t error "NUMEX orelse applied to non-number or non-boolean"])]) v4)))))]

        [(plus? e) 
         (let ([v1 (eval-under-env-c (plus-e1 e) env)]
               [v2 (eval-under-env-c (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        
        [(minus? e) 
         (let ([v1 (eval-under-env-c (minus-e1 e) env)]
               [v2 (eval-under-env-c (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        
        [(mult? e) 
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiply applied to non-number")))]
        [(div? e) 
         (let ([v1 (eval-under-env-c (div-e1 e) env)]
               [v2 (eval-under-env-c (div-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (if (eq? (num-int v2) 0)
                   (error "NUMEX division applied to Zero" v2)
                   (num (quotient (num-int v1) 
                       (num-int v2))))
               (error "NUMEX division applied to non-number")))]
        
        [(neg? e) 
         (let ([v1 (eval-under-env-c (neg-e1 e) env)])
           (if (num? v1)
               (num (- (num-int v1)))
               (if (bool? v1)
                   (bool (if (bool-b v1) #f #t))
                   (error "NUMEX Nagation applied to non-number or non-boolean"))))]
        
        [(num? e)
         (let ([v1 (eval-under-env-c (num-int e) env)])
         (if (integer? v1) (num v1) (error "NUMEX num applied to non-integer")))]
        [(bool? e)
         (let ([v1 (eval-under-env-c (bool-b e) env)])
         (if (boolean? v1) (bool v1) (error "NUMEX bool appllied to non-boolean")))]
        [(closure? e) e]
        [(number? e) e]
        [(boolean? e) e]
        [(string? e) e]
        [(munit? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

; It first computes the free variables for the entire expression
; and then evaluates the expression using an empty environment.
;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

; These modifications aim to improve efficiency by avoiding redundant calculations of free variables
; and constructing closures with smaller environments containing only the necessary variables.

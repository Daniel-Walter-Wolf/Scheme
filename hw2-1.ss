;; Daniel W. Wolf

;; Programming languages

;;9-26-14



;; Global variable for storing a list of tests, initially empty.
(define my-tests! '())

(define clear-tests! (lambda () (set! my-tests! '())))




;; (add-my-test! name-str qe1 qe2)
;; Function which takes a string name-str naming a test, and two
;; quoted S-expressions.  This function combines the name and the two
;; quoted expressions into a list and adds it to the head of the
;; global variable my-tests!  
;; MAKE SURE USE QUOTED EXPRESSIONS!


; (define add-my-test!
;   (lambda (name-str qe1 qe2)
;     (set! my-tests! (cons (list name-str qe1 qe2) my-tests!))))


;; part 1

(define add-my-test!
	(lambda (test-name-str ex-name-str ptval qe1 qe2)
		(set! my-tests! (cons (list test-name-str ex-name-str ptval qe1 qe2) my-tests!))))




;; (display-result! val1 val2)
;; Takes two values and displays them.
(define display-result!
  (lambda (val1 val2)
    (display val1)
    (display " => ")
    (display val2)))






;; (display-test-success! name-str qe1 qe2 val1 val2)
;; Displays text to indicate test success.
(define display-test-success!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Success -- ")
    (display-result! qe1 qe2)
    (display "\n")))





;; (display-test-failure! name-str qe1 qe2 val1 val2)
;; Displays text to indicate failure.
(define display-test-failure!
  (lambda (name-str qe1 qe2 val1 val2)
    (display name-str)
    (display " -- Failure\n")
    (display "  Expected: ")
    (display-result! qe1 qe2)
    (display "\n    Actual: ")
    (display-result! qe1 val1)
    (display "\n            ")
    (display-result! qe2 val2)
    (display "\n")))








;; (run-one-test! name-str qe1 qe2)
;; Runs a test with the given name two quoted expressions
(define run-one-test!
  (lambda (name-str qe1 qe2)
    (let 
	([val1 (eval qe1)]  ;; This is why the quote are necessary.
	 [val2 (eval qe2)])
      (cond
       [(equal? val1 val2) (display-test-success! name-str qe1 qe2 val1 val2)]
       [else (display-test-failure! name-str qe1 qe2 val1 val2)]))))






;;part 2

(define run-one-exercise!
	(lambda (ex-name-str test-ls) (define exercise-ls (group-exercises* ex-name-str test-ls)) (let 
    ( 
      [total (point-total* exercise-ls 0)]
      [earned (points-earned* exercise-ls 0)])

      (display earned) (display " points earned out of ") 
      (display total) (display " total"))))



;; helper functions for part 2


(define points-earned* (lambda (exercise-ls acc)
   (cond 
    [(null? exercise-ls) acc]
    [else (points-earned* 
      (cdr exercise-ls) (+ acc 
        (run-one-exercise* 
          (car (car exercise-ls)) 
          (caddr (car exercise-ls)) 
          (cadddr (car exercise-ls)) 

          (cadr (cdddr (car exercise-ls)))))) ])));;I wanted to do caddddr but it wouldnt let me



(define run-one-exercise*
  (lambda (name-str ptval qe1 qe2)
    (let 
  ([val1 (eval qe1)]  ;; This is why the quote are necessary.
   [val2 (eval qe2)])
      (cond
       [(equal? val1 val2) ptval]
       [else  0]))))


(define group-exercises* 
  (lambda (ex-name-str test-ls) (filter
    (lambda (test-ls) 
      (equal? ex-name-str (cadr test-ls))
      ) test-ls)))


(define point-total*
  (lambda (exercise-ls acc) (cond
    [(null? exercise-ls) acc]
    [else (point-total* (cdr exercise-ls) (+ acc (caddr (car exercise-ls))))])))


;; end of part 2 helper functions

;TEMPLATE
;(add-batch-tests! "Exercise 1 (2pts)" '((times10 '(1 2 3 4 5))  =>  '(10 20 30 40 50) (times10 '(26))  =>  '(250)))
;test-name-str ex-name-str ptval qe1 qe2




;; part 3

(define add-batch-tests! (lambda (ex-name-str q-tests)  
  (cond
    [(null? q-tests)]
    [else (add-batch-tests! ex-name-str (cdddr q-tests)) (add-my-test! "" ex-name-str 1 (car q-tests) (caddr q-tests))])))










;; (run-all-tests!)  
;; Runs all tests.  Note this is a 0-ary function, i.e., it takes no
;; arguments.



;;part 4


(define run-all-tests!
  (lambda ()
    (display (run-all-tests!* my-tests! 0)) (display " points earned out of ")
    (display (point-total* my-tests! 0)) (display " total")
    ))




;; (run-all-tests!* ls)
;; Recursive function to recurse through tests running each one
; (define run-all-tests!* 
;   (lambda (ls)
;     (if (not (null? ls))
; 	(let
; 	    ([test (car ls)])
; 	  (let ([name-str (car test)]
; 		[qe1 (cadr test)]
; 		[qe2 (caddr test)])
; 	    (run-one-test! name-str qe1 qe2)
; 	    (run-all-tests!* (cdr ls)))))))


;; helper function for run-all-tests!



(define run-all-tests!* 
  (lambda (ls acc) 
    (cond
      [(null? ls) acc]
      [else (run-all-tests!* (remove-fetched* ls) (+ acc (exercise-earned* (cadr (car ls)) ls))) 
      ])))


;;part 4 helper functions


(define fetch-tests* 
  (lambda (ls) (let ([first (cadr (car ls))])    
    (group-exercises* first ls))))

(define remove-fetched* 
  (lambda (ls) (let ([first (cadr (car ls))])    
    (remove-extra* first ls))))


(define remove-extra*
  (lambda (ex-name-str test-ls) (filter
    (lambda (test-ls) 
      (not(equal? ex-name-str (cadr test-ls)))
      ) test-ls)))


(define exercise-earned*
  (lambda (ex-name-str test-ls) (define exercise-ls (fetch-tests* test-ls)) (let 
    ( 
      [earned (points-earned* exercise-ls 0)])
    earned)))


;;end of part 4 helper functions


  
;; Sample tests for functions we wrote above
;(add-my-test! "Reverse test" '(reverse '(1 2 3)) ''(3 2 1))
;(add-my-test! "Fib test" '(fib 4) '3)
;(add-my-test! "Fib test *SHOULD FAIL*" '(fib 5) ''(1 3 4)) ;; should fail


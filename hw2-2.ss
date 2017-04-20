;; Daniel W. Wolf

;; Programming languages

;;9-26-14

;; part 1 GRADE

(define times10 (lambda (nums)  
	(map (lambda (nums) (* nums 10)) nums)))

;; part 2 GRADE

(define pair-up (lambda (elt ls)
	(map (lambda (ls) (cons elt ls)) ls)))

;; part 3 GRADE

(define x-odds (lambda (nums) 
	(map (lambda (nums) (cond
		[(odd? nums) 'x]
		[else nums])) nums)))

;; part 4 GRADE

(define replace (lambda (old new syms) 
	(map (lambda (syms) (cond
		[(equal? syms old) new]
		[else syms])) syms)))

;; part 5 GRADE

(define remove (lambda (elt ls) (filter
	(lambda (ls) (cond
		[(equal? elt ls) #f]
		[else #t])) ls)))

;; part 6

(define listoflists? (lambda (lls) (let ([newlist (listoflists-acc* lls '())])
	(cond
		[(null? newlist) #t]
		[(equal? (car newlist) #f) #f]
		[else (listoflists? (cdr lls))]
		))))


(define listoflists-acc*
	(lambda (lls acc)  
		(cond
			[(null? lls) acc]
			[else
			(let 
				([first (car lls)]
				 [rest (cdr lls)])
				(listoflists-acc* rest (cons (list? first) acc)))])))


;; part 7 GRADE

(define length (lambda (ls) (length-acc* ls 0)))


(define length-acc* (lambda (ls acc) (cond
	[(null? ls) acc]
	[else (length-acc* (cdr ls) (+ acc 1))])))


;; part 8 GRADE

(define average (lambda (nums) (/ (average-acc* nums 0) (length nums))))

(define average-acc* (lambda (nums acc) (cond
	[(null? nums) acc]
	[else  (average-acc* (cdr nums) (+ acc (car nums)))])))


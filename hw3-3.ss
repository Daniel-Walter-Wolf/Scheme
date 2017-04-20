; Daniel W. Wolf

; Programming languages

; hw 3-3

;10-03-14



;; part 1

(define zero
	(lambda ()
		'(0)))

(define is-Zero?
	(lambda (n)
		(cond
			[(or (equal? n '(0)) (equal? n 0)) #t]
			[else #f])))


(define successor 
	(lambda (n) 
		(number->binary (+ n 1))))

(define predecessor
	(lambda (n) 
		(cond
			[(< (- n 1) 0) (display "please enter a valid number")]
			[else (number->binary (- n 1))])))

;;part 2


(define number->binary
	(lambda (n) (reverse-ls* (number->binary* n) '() )))

(define number->binary*
	(lambda (n) 
		(cond
			[(<= n 1) (cons n '()) ]
			[else (cons (remainder n 2) (number->binary* (floor (/ n 2))))]
				)))



(define reverse-ls* 
	(lambda (ls acc)
		(cond
			[(null? ls) acc]
			[else (reverse-ls* (cdr ls) (cons (car ls) acc))  ])))


(define binary->number
	(lambda (n) (binary->number* n 0)))


(define binary->number*
	(lambda (n acc) (let ([count (- (length n) 1)]) 
		(cond
		[(null? n) acc]
		[else (binary->number* (cdr n) (+ acc (cond
			[(equal? 0 (car n)) 0]
			[else (expt 2 count)])))]))))



;;part 3

(define equals? 
	(lambda (a b)
		(cond
			[(and (is-Zero? a) (is-Zero? b)) #t]
			[(or (is-Zero? a) (is-Zero? b)) #f]
			[else (equals? (binary->number (predecessor a)) (binary->number (predecessor b))])))


(define less-than? 
	(lambda (a b) 
		(cond
			[(and (is-Zero? a) (not (is-Zero? b))) #t]
			[(or (equals? a b) (and (not (is-Zero? a)) (is-Zero? b))) #f]
			[else (less-than? (binary->number (predecessor a)) (binary->number (predecessor b))]))))


(define sum 
	(lambda (a b)
		))

(define prod
	(lambda (a b)))
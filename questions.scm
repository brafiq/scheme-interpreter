(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (s) (append (list first) s)) rests))

(define (zip pairs)
  (list (map (lambda (x) (car x)) pairs) (map (lambda (x) (cadr x)) pairs)))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (help s i)
    (if (null? s)
        nil
        (cons (cons i (cons (car s) nil)) (help (cdr s) (+ i 1)))))
  (help s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (cond
    ((null? denoms) nil)
    ((= total 0) (list (list (car denoms))))
    ((> (car denoms) total) (list-change total (cdr denoms)))
    ((= (car denoms) total) (append (list-change (- total (car denoms)) denoms)
                                    (list-change total (cdr denoms))))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                            (list-change total (cdr denoms)))))
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (append (list 'lambda params) (let-to-lambda body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (if (or (atom? values) (not (pair? (car values)))) ; making sure let expression is properly formed before proceeding
                  (cons (car expr) (let-to-lambda (cdr expr)))
                 (let ((symbols (car (zip values))) ; separate symbols and values
                        (vals (let-to-lambda (cadr (zip values)))))
                    (append (list (append (list 'lambda (map (lambda (x) x) symbols)) ; begin constructing the lambda expression
                                          (list (let-to-lambda (car body)))))
                            vals))
            )
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         (cons (let-to-lambda (car expr)) (let-to-lambda (cdr expr))) 
         ; END PROBLEM 19
         )))

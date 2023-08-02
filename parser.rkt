;Straub, Samuel
;CS 441-0001
;Problems: Was able to handle all test files, however, if the first line is empty the program will not work.
#lang racket
(define (parse file) ;Parser logic
(define inn (open-input-file file))
(define (scanner currChar) ;Scanner logic
  (cond
    [(member currChar (list #\( #\) #\+ #\- #\* )) (string currChar)] ;If token is an operator returns it.
    [(equal? currChar #\/) ;If token is part of a comment, removes token and comments.
     (if (member (peek-char inn 0) (list #\/ #\*))
         (let fac ([x (read-char inn)]) ;Recursively loops to remove all input from buffer that is part of comment.
           (cond
             [(equal? x #\newline) (scanner (peek-char inn 0))]
             [(equal? x #\*) (if (equal? (read-char inn) #\/) (scanner (read-char inn)) (fac (read-char inn)))]
             [else (fac (read-char inn))]
             )
           )
         (string currChar) ;Returns token as divisor.
         )
     ]
    [(equal? currChar #\:)
     (cond
       [(char=? #\: (peek-char inn 0)) (read-char inn)]
       )
     (if (char=? #\= (read-char inn)) ":=" (error "syntax"))

     ] ;Checks if the token is assign operator. 
    [(equal? currChar #\.) ;Checks if token is a decimal. Still-to-do. Please ignore
     (if (char-numeric? (read-char inn))
         (let fac ([x (peek-char inn 0)])
           (cond
             [(char-numeric? x) (fac (read-char inn))]                                        
             [else "number"]
             )
           )
         (error "scanning error")
         )
     ] ;Okay, start reading again
    [(char-numeric? currChar) ;Checks to see if token is a number
     (if (char-numeric? (peek-char inn 1)) ;Checks if the number is more than one digit.
         (let fac ([x (peek-char inn 0)]) ;Recursively loops through all the digits in a number
           (cond
             [(char-numeric? (peek-char inn 1)) (fac (read-char inn))]                                        
             [else "number"] ;Returns number as our token
             )
           )
         "number" ;Return subject to change
         )]
    [(char-alphabetic? currChar) ;checks if the input is read, write, or an ID
     (cond
       [(string=? (peek-string 5 0 inn) "write")  "write"] 
       [(string=? (peek-string 4 0 inn) "read")   "read"]  
       [else "id"] ;Need a more expansive definition to handle this case.
      )
     ]
    [(char=? #\space currChar) (if (char=? (peek-char inn 0) #\space) (scanner (read-char inn)) (scanner(peek-char inn 0)))] ;Handles reading empty spaces.
    [(char=? #\return currChar) (scanner (read-char inn))] ;Removes returns, which are unimportant tokens.
    [(char=? #\newline currChar)"newline"] ;Returns a newline which will act as an epsilon production. Needs to be polished. Cannot handle if first line is empty
    [(char=? #\$ currChar) (if (char=? #\$ (peek-char inn 0)) "$$" (error "syntax error")) ] ;if the scanner detects the eof
    [else (string currChar)]
    )
  ) 
  (define (match expected) ;Partially responsible for consuming characters.
    (cond
      [(string=? expected (scanner (peek-char inn))) (if (or (string=? "read" expected) (string=? "write" expected)) (file-position inn (+ (file-position inn) (string-length expected))) (string (read-char inn)))] ;If the current token is read or write, skips the length. Otherwise, if it is a number or ID all but the last token has already been consumed by the scanner
      [else (error (string-append "Match error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
    )
    )
  (define (statement_list) ;Starts the program off.
    (cond
      [(member (scanner(peek-char inn)) (list "id" "read" "write")) (statement) (statement_list) ]
      [(string=? "$$" (peek-string 2 0 inn)) "Finished"]
      [else (error (string-append "Statement list error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )  
    )
  (define (statement) ;Handles statements that are either read, write, or an id.
    (define peek_ahead (scanner (peek-char inn))) ;Slightly better than re-scanning every single time.
    (cond
      [(string=? "id" peek_ahead) (match "id") (match ":=") (expr)] 
      [(string=? "read" peek_ahead) (match "read") (match "id") ]
      [(string=? "write" peek_ahead)(match "write") (expr)] 
      [else (error (string-append "Statement error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (expr) ;Handles expressions.
    (cond
      [(member (scanner(peek-char inn)) (list "id" "number" "(" )) (term) (term_tail)] 
      [else (error (string-append "Expression error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (term_tail) ;Handles term_tails.
    (define peek_ahead (scanner (peek-char inn)))
    (cond
      [(member peek_ahead (list "+" "-")) (add_op) (term) (term_tail)]
      [(member peek_ahead (list ")" "id" "read" "write" "$$"))] 
      [else (error (string-append "Term tail error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )  
  (define (term) ;Handles terms
    (cond
      [(member (scanner(peek-char inn)) (list "id" "number" "(" )) (factor) (factor_tail)]
      [else (error (string-append "Term error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (factor_tail) ;Handles factor_tails
    (define peek_ahead (scanner (peek-char inn 0)))
    (cond
      [(member peek_ahead (list "*" "/")) (mult_op) (factor) (factor_tail)] 
      [(member peek_ahead (list "+" "-" ")" "id" "read" "write" "$$" "newline" ))]
      [else (error (string-append "Factor tail error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (factor) ;Handles factors
    (define peek_ahead (scanner (peek-char inn)))
    (cond
      [(string=? "id" peek_ahead) (match "id")]
      [(string=? "number" peek_ahead) (match "number")]
      [(string=? "(" peek_ahead) (match "(") (expr) (match ")")]
      [else (error (string-append "Factor error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (add_op) ;Handles additions/subtractions
    (define peek_ahead (scanner (peek-char inn)))
    (cond
      [(string=? "+" peek_ahead) (match "+")]
      [(string=? "-" peek_ahead) (match "-")]
      [else (error (string-append "Add operation error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
  (define (mult_op) ;Handles multiplications/divisions
    (define peek_ahead (scanner (peek-char inn)))
    (cond
      [(string=? "*" peek_ahead) (match "*")]
      [(string=? "/" peek_ahead) (match "/")]
      [else (error (string-append "Multiplication error at character number: "(number->string (car(reverse (call-with-values (lambda () (port-next-location inn)) list))))))]
      )
    )
    (statement_list) ;Starts the program
    (close-input-port inn) ;Closes the file, as it's no longer needed.
  "Input Accepted" ;Only reached if input was accepted.
    )
  
 (display (parse "input01.txt")) ;Please input your file here

    


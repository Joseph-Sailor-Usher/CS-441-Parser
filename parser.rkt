#lang racket
;tokenize a list of strings
(define (tokenize string)
  (begin
    (cond
      [(equal? "=" string) 'equals]
      [(equal? "if" string) 'if]
      [(equal? "then" string) 'then]
      [(equal? "read" string) 'read]
      [(equal? "write" string) 'write]
      [(equal? "goto" string) 'goto]
      [(equal? "gosub" string) 'gosub]
      [(equal? "return" string) 'return]
      [(equal? "(" string) 'l-parens]
      [(equal? ")" string) 'r-parens]
      [(equal? "+" string) 'plus]
      [(equal? "-" string) 'minus]
      [(equal? ":" string) 'colon]
      [(id? string) 'id]
      [(num? string) 'num]
      [else
       'UNKNOWN-SYMBOL])))

;program	:    linelist $$
(define (program? input)
  (and (line-list? input)
    (equal? (car (last input)) "$$")))

;linelist		:    line linelist | epsilon
(define (line-list? input)
  (or
   (equal? (car (car input)) "$$")
   (and
    (line? (car input))
    (line-list? (cdr input)))))

;line		:    idx stmt linetail* [EOL]
(define (line? line)
  (define tokens (append '(idx) (map tokenize (cdr line))))
  ;(displayln tokens)
  (cond
    [(eof-object? line)
     (displayln "No $$ at the end of the file.")]
    [(member 'UNKNOWN-SYMBOL tokens)
     (display (format "Unknown operator on line: ~a\n" (car line))) #f]
    [(not (idx? (car line)))
     (displayln "Missing line number, or line number beginning with zero.") #f]
    [(and (idx? (car line)) (stmt? (cdr line) (car line)))
     #t]
    [else
     #f]))

;linetail		:    stmt | epsilon
(define (line-tail? input)
  #t)

;stmt		:    id = expr | if expr then stmt | read id | write expr | goto idx | gosub idx | return
(define (stmt? input line-num)
  (begin
    ;(displayln (format "stmt ~a" input))
    (cond
      [(and (equal? (length input) 1) (equal? (car input) "return"))
       #t]
      [(and (> (length input) 1) (equal? (car input) "read") (id? (cadr input)))
       #t]
      [(and (> (length input) 1) (equal? (car input) "goto") (idx? (cadr input)))
       #t]
      [(and (> (length input) 1) (equal? (car input) "gosub") (idx? (cadr input)))
       #t]
      [(and (equal? (car input) "write") (expr? (cdr input) line-num))
       #t]
      [(and (> (length input) 1) (id? (car input)) (equal? (cadr input) "=") (expr? (cdr (cdr input)) line-num))
       #t]
      [(and (equal? (car input) "if") (expr-then-stmt? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper statement on line ~a" line-num))
       #f])))

;expr		:    id etail | num etail | ( expr )
(define (expr? input line-num)
  (begin
    ;(displayln (format "expr ~a" input))
    (cond
      [(empty? (car input))
       #t]
      [(and (id? (car input)) (etail? (cdr input) line-num))
       #t]
      [(and (num? (car input)) (etail? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "(") (expr-then-parens? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper expression on line ~a" line-num))
       #f])))

;etail		:    + expr | - expr | = expr | epsilon
(define (etail? input line-num)
  (begin
    ;(displayln (format "etail ~a" input))
    (cond
      [(empty? input)
       #t]
      [(and (equal? (car input) "+") (expr? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "-") (expr? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "=") (expr? (cdr input) line-num))
       #t]
      [(and (equal? (car input) ":") (expr? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper etail on line ~a" line-num))
       #f])))

;I couldn't figure out another way to do handle this case.
(define (expr-then-parens? input line-num)
  (begin
    ;(displayln (format "expr ~a" input))
    (cond
      [(empty? (car input))
       #t]
      [(and (id? (car input)) (etail-then-parens? (cdr input) line-num))
       #t]
      [(and (num? (car input)) (etail-then-parens? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "(") (expr-then-parens? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper expression on line ~a" line-num))
       #f])))

(define (etail-then-parens? input line-num)
  (begin
    ;(displayln (format "etail ~a" input))
    (cond
      [(empty? input)
       #t]
      [(equal? (car input) ")")
       #t]
      [(and (equal? (car input) "+") (expr-then-parens? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "-") (expr-then-parens? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "=") (expr-then-parens? (cdr input) line-num))
       #t]
      [(and (equal? (car input) ":") (expr-then-parens? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper etail on line ~a" line-num))
       #f])))

;Again, disgusted with myself for writing three duplicate functions, but it must function.
(define (expr-then-stmt? input line-num)
  (begin
    ;(displayln (format "expr-then-stmt ~a" input))
    (cond
      [(empty? (car input))
       #t]
      [(and (id? (car input)) (etail-then-stmt? (cdr input) line-num))
       #t]
      [(and (num? (car input)) (etail-then-stmt? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper expression-then-statement on line ~a" line-num))
       #f])))

(define (etail-then-stmt? input line-num)
  (begin
    ;(displayln (format "etail-then-stmt ~a" input))
    (cond
      [(empty? input)
       #t]
      [(and (equal? (car input) "+") (expr-then-stmt? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "-") (expr-then-stmt? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "=") (expr-then-stmt? (cdr input) line-num))
       #t]
      [(and (equal? (car input) ":") (expr-then-stmt? (cdr input) line-num))
       #t]
      [(and (equal? (car input) "then") (stmt? (cdr input) line-num))
       #t]
      [else
       (displayln (format "Improper etail on line ~a" line-num))
       #f])))

;idx		:    nonzero_digit digit* 
(define (idx? word)
  (and (not (equal? (string-ref word 0) #\0))
       (num? word)))

;id		:    [a-zA-Z]+
(define (id? word)
  (match word
    [(regexp #rx"^([a-zA-Z]+)$") #t]
    [else #f]))

;num		:    numsign digit digit*
(define (num? word)
  (or (regexp-match? #rx"^[1-9][0-9]*$" word)
      (equal? word "0")))

;numsign	:    + | - | epsilon 
(define (num-sign? character)
  (regexp-match? #rx"[+-]" (string character)))

;nonzero_digit	:    1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(define (non-zero-digit? character)
  (regexp-match? #rx"^[1-9]$" (string character)))

;Digit		:    0 | nonzero_digit
(define (digit? character)
  (or (char=?  character) #\0) (non-zero-digit? character))

;Split the lines of a file into a list of strings
(define (split-lines filename)
  (map (lambda (line) (string-split line))
       (file->lines filename)))

(define (parse filename)
  (begin
    ; Read and split the lines on white spaces into lists of strings
    (define lines (split-lines filename))
    ; Send the processed lines to be evaluated by our parser functions
    (program? lines)))


#|
;Original Parser function
(define (parse filename)
  (begin
    ;Read and split the lines on white spaces into lists of strings
    (define split-lines
      (map(lambda (line) (string-split line))
       (file->lines filename)))
    ;send the processed input file to be evaluated by our parser functions
    (program? split-lines)))
|#

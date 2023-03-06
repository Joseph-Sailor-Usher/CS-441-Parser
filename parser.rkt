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
     (display (format "Unknown symbol on line: ~a\n" (car line))) #f]
    [(not (idx? (car line)))
     (displayln "Missing line number.") #f]
    [(and (idx? (car line)) (stmt? (cdr line)))
     #t]
    [else
     #f]))

;idx		:    nonzero_digit digit* 
(define (idx? word)
  (and (not (equal? (string-ref word 0) #\0))
       (num? word)))

;linetail		:    stmt | epsilon
(define (line-tail? input)
  #t)

;stmt		:    id = expr | if expr then stmt | read id | write expr | goto idx | gosub idx | return
(define (stmt? input)
  #t)

;expr		:    id etail | num etail | ( expr )
(define (expr? input)
  #t)

;etail		:    + expr | - expr | = expr | epsilon
(define (etail? input)
  (displayln input))

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

(define (parse filename)
  (begin
    ;Read and split the lines on white spaces into lists of strings
    (define split-lines
      (map(lambda (line) (string-split line))
       (file->lines filename)))
    ;send the processed input file to be evaluated by our parser functions
    (program? split-lines)))

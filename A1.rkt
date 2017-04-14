#lang racket
(require racket/cmdline)

; Prints out a list of comma-separated values
(define (comma-separate in)
  (match in
    [(cons f r)(begin (translate f)
                      (if (< 0 (length r))
                          (display ", ")
                          (display ""))
                      (comma-separate r))]
    ['() (display "")]))

; Prints out a lambda application in Python3
(define (display-app args body vals)
  (begin (display-lam args body)
         (display "(")(comma-separate vals)(display ")")))

; Prints out a lambda definition in Python3
(define (display-lam args body)
  (display "(lambda ")(comma-separate args)(display ": ")
  (translate body)(display ")"))

; Translates an S-expression to Python3
(define (translate in)
  (match in
    [(? real? n) (display n)]
    [(? symbol? s) (display s)]
    [(list (list 'lambda (list (? symbol? args) ...) body) vals ...) (display-app args body vals)]
    [(list 'lambda (list (? symbol? args) ...) body) (display-lam args body)]
    [(list '+ a b) (translate a) (display " + ")(translate b)]
    [(list '* a b) (translate a) (display " * ")(translate b)]
    [(list 'ifleq0 a b c) (translate b)(display " if ")
                          (translate a)(display " <= 0 else ")(translate c)]
    [(list 'println x)(display "_____pr_____(")(translate x)(display ")" )]
    [(list x y) (translate x)(display "(")(translate y)(display ")")]
    [(cons f r) (cons (translate f) (translate r))]
    [else (display "")]))

; Translates a full lambda-calculus program to Python3
(define (top-translate in)
  (begin (display "# I'm being Pythonic!\n")
         (display "def _____pr_____(x):\n")
         (display "\tprint(x)\n")
         (display "\treturn 0\n\n")
         (translate in)(display "\n")))

; Reads from a file specified from the command line, translates it to Python3
; and outputs it to [input file].py
(define (driver)
  (local ([define fname (command-line #:args (filename) filename)])
    (with-output-to-file (string-append fname ".py") #:exists 'replace
      (lambda () (top-translate (file->value fname))))))

(driver)
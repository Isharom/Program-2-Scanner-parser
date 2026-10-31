
#lang racket

(struct token (type value line) #:transparent)

(define (show-tokens tokens)
  (for ([current-token tokens])
    (printf "Type: ~a | Value: ~a | Line: ~a\n"
            (token-type current-token)
            (token-value current-token)
            (token-line current-token))))


(define (string-index text character)
  (for/first ([i (in-range (string-length text))]
              #:when (char=? (string-ref text i) character))
    i))


(define (scan source)
  (define pattern
    #px"^[0-9]+\\.[0-9]+|^[0-9]+|^[a-zA-Z][a-zA-Z0-9]*|^(=>|>=|<=|==|/=)|^[+*/;{}()\\[\\],:<>~=-]")

  (let loop ([remaining source]
             [line 1]
             [tokens '()])
    (cond
      [(string=? remaining "")
       (reverse tokens)]

      [(char-whitespace? (string-ref remaining 0))
       (loop (substring remaining 1)
             (if (char=? (string-ref remaining 0) #\newline)
                 (add1 line)
                 line)
             tokens)]

      [(string-prefix? remaining "--")
       (define end (or (string-index remaining #\newline)
                       (string-length remaining)))
       (loop (substring remaining end) line tokens)]

      [(char=? (string-ref remaining 0) #\`)
       (define end
         (string-index (substring remaining 1) #\`))
       (if end
           (let ([value (substring remaining 0 (+ end 2))])
             (loop (substring remaining (+ end 2))
                   line
                   (cons (token 'string value line) tokens)))
           (error 'scan
                  (format "Syntax error on line ~a" line)))]

      [else
       (define found (regexp-match pattern remaining))
       (if found
           (let* ([value (first found)]
                  [type
                   (cond
                     [(regexp-match? #px"^[0-9]+\\.[0-9]+$" value)
                      'real]
                     [(regexp-match? #px"^[0-9]+$" value)
                      'integer]
                     [(regexp-match? #px"^[a-zA-Z]" value)
                      'identifier]
                     [else 'symbol])])
             (loop (substring remaining (string-length value))
                   line
                   (cons (token type value line) tokens)))
           (error 'scan
                  (format "Syntax error on line ~a" line)))])))




(define tokens '())
(define position 0)

(define (current-token)
  (if (< position (length tokens))
      (list-ref tokens position)
      (token 'eof "EOF"
             (if (null? tokens)
                 1
                 (token-line (last tokens))))))

(define (consume expected)
  (define current (current-token))
  (if (string=? (token-value current) expected)
      (begin
        (set! position (add1 position))
        current)
      (error 'parse
             (format "Syntax error on line ~a: expected ~a"
                     (token-line current)
                     expected))))




(define (parse-identifier)
  (define current (current-token))
  (if (eq? (token-type current) 'identifier)
      (begin
        (set! position (add1 position))
        (token-value current))
      (error 'parse
             (format "Syntax error on line ~a: expected identifier"
                     (token-line current)))))





(define (parse-type)
  (define value (token-value (current-token)))
  (if (member value '("int" "real" "bool" "string"))
      (begin
        (consume value)
        value)
      (error 'parse
             (format "Syntax error on line ~a: expected type"
                     (token-line (current-token))))))




(define (parse-identifier-list)
  (define name (parse-identifier))
  (if (string=? (token-value (current-token)) ",")
      (begin
        (consume ",")
        (cons name (parse-identifier-list)))
      (list name)))



(define (parse-var-declaration)
  (consume "decl")
  (define names (parse-identifier-list))
  (consume "as")
  (define type (parse-type))
  (consume ";")
  (list 'decl names type))




(define (parse-var-declarations)
  (if (string=? (token-value (current-token)) "decl")
      (cons (parse-var-declaration)
            (parse-var-declarations))
      '()))





(define (parse-factor)
  (define current (current-token))
  (define value (token-value current))

  (cond
    [(string=? value "(")
     (consume "(")
     (define result (parse-expression))
     (consume ")")
     result]

    
    [(and (string=? value "invoke")
          (< (+ position 2) (length tokens))
          (eq? (token-type (list-ref tokens (+ position 1)))
               'identifier)
          (string=? (token-value (list-ref tokens (+ position 2)))
                    "("))
     (parse-invoke)]


    [(member (token-type current) '(integer real string))
     (set! position (add1 position))
     (list 'literal value)]

    
    
    [(member value '("yes" "no" "maybe"))
     (set! position (add1 position))
     (list 'literal value)]



    [(eq? (token-type current) 'identifier)
     (set! position (add1 position))
     (list 'identifier value)]

    [else
     (error 'parse
            (format "Syntax error on line ~a: expected expression"
                    (token-line current)))]))




(define (parse-unary)
  (if (and (string=? (token-value (current-token)) "not")
           (< (+ position 1) (length tokens))
           (not (member
                 (token-value (list-ref tokens (+ position 1)))
                 '("+" "-" "*" "/" "and" "or" "~"
                   "==" "/=" "<" "<=" ">" ">="
                   "=>" ";" ")" "]" ","))))
      (begin
        (consume "not")
        (list 'not (parse-unary)))
      (parse-factor)))






(define (parse-term)
  (let loop ([left (parse-unary)])
    (define op (token-value (current-token)))
    (if (member op '("*" "/" "and"))
        (let ([operator (consume op)]
              [right (parse-unary)])
          (loop (list (token-value operator) left right)))
        left)))


(define (parse-simple-expr)
  (let loop ([left (parse-term)])
    (define op (token-value (current-token)))
    (if (member op '("+" "-" "or" "~"))
        (let ([operator (consume op)]
              [right (parse-term)])
          (loop (list (token-value operator) left right)))
        left)))




(define (parse-expression)
  (define left (parse-simple-expr))
  (define op (token-value (current-token)))

  (if (member op '("==" "/=" "<" "<=" ">" ">="))
      (let ()
        (consume op)
        (define right (parse-simple-expr))
        (list op left right))
      left))



(define (parse-assignment)
  (define value (parse-expression))
  (consume "=>")
  (define name (parse-identifier))
  (consume ";")
  (list 'assign name value))


(define (parse-yield)
  (consume "yield")
  (define value (parse-expression))
  (consume ";")
  (list 'yield value))




(define (parse-empty)
  (consume ";")
  (list 'empty))



(define (parse-loop)
  (consume "loop")
  (consume "until")
  (consume "[")

  (define condition (parse-expression))

  (consume "]")
  (consume "do")
  (consume "{")

  (define statements (parse-statement-list))

  (consume "}")
  (consume ";")

  (list 'loop condition statements))





(define (parse-choose)
  (consume "choose")
  (consume "[")

  (define condition (parse-expression))

  (consume "]")
  (consume "match")
  (consume "{")

  (define match-statements (parse-statement-list))
  (consume "}")

  (define otherwise-statements
    (if (string=? (token-value (current-token)) "otherwise")
        (begin
          (consume "otherwise")
          (consume "{")
          (let ([statements (parse-statement-list)])
            (consume "}")
            statements))
        '()))

  (consume ";")

  (list 'choose condition match-statements otherwise-statements))




(define (parse-parameter)
  (define name (parse-identifier))
  (consume ":")
  (define type (parse-type))
  (list 'param name type))



(define (parse-parameters)
  (if (string=? (token-value (current-token)) ")")
      '()
      (let loop ([params (list (parse-parameter))])
        (if (string=? (token-value (current-token)) ",")
            (begin
              (consume ",")
              (loop (append params (list (parse-parameter)))))
            params))))




(define (parse-routine)
  (consume "routine")
  (define name (parse-identifier))

  (consume "(")
  (define params (parse-parameters))
  (consume ")")

  (consume "returns")
  (define return-type (parse-type))
  (consume "{")

  

  (define declarations (parse-var-declarations))
  (define routines (parse-routines))
  (define statements (parse-statement-list))

  (consume "}")

  (list 'routine name params return-type declarations routines statements))







(define (parse-routines)
  (if (string=? (token-value (current-token)) "routine")
      (cons (parse-routine)
            (parse-routines))
      '()))



(define (parse-arguments)
  (if (string=? (token-value (current-token)) ")")
      '()
      (let loop ([args (list (parse-expression))])
        (if (string=? (token-value (current-token)) ",")
            (begin
              (consume ",")
              (loop (append args (list (parse-expression)))))
            args))))




(define (parse-invoke)
  (consume "invoke")
  (define name (parse-identifier))
  (consume "(")
  (define args (parse-arguments))
  (consume ")")
  (list 'invoke name args))



(define (parse-call-statement)
  (define call (parse-invoke))
  (consume ";")
  (list 'call call))





(define (parse-statement-list)
  (define value (token-value (current-token)))

  (cond
    [(string=? value "}")
     '()]

    [(string=? value ";")
     (cons (parse-empty)
           (parse-statement-list))]

    
    [(eq? (token-type (current-token)) 'eof)
     (error 'parse
            (format "Syntax error on line ~a: unexpected end of file"
                    (token-line (current-token))))]


    
    [(and (string=? value "loop")
          (< (+ position 1) (length tokens))
          (string=? (token-value (list-ref tokens (+ position 1)))
                    "until"))
     (cons (parse-loop)
           (parse-statement-list))]


    [(and (string=? value "choose")
      (< (+ position 1) (length tokens))
      (string=? (token-value (list-ref tokens (+ position 1))) "["))
     (cons (parse-choose)
       (parse-statement-list))]


    
    [(and (string=? value "yield")
          (not (member
                (token-value
                 (if (< (+ position 1) (length tokens))
                     (list-ref tokens (+ position 1))
                     (token 'eof "EOF" 0)))
                '("+" "-" "*" "/" "and" "or" "~"
                      "==" "/=" "<" "<=" ">" ">=" "=>"))))
     (cons (parse-yield)
           (parse-statement-list))]

    
    [(and (string=? value "invoke")
          (< (+ position 2) (length tokens))
          (eq? (token-type (list-ref tokens (+ position 1)))
               'identifier)
          (string=? (token-value (list-ref tokens (+ position 2)))
                    "("))
     (if (let ([saved-position position])
           (define result (parse-invoke))
           (define standalone?
             (string=? (token-value (current-token)) ";"))
           (set! position saved-position)
           standalone?)
         (cons (parse-call-statement)
               (parse-statement-list))
         (cons (parse-assignment)
               (parse-statement-list)))]




    [else
     (cons (parse-assignment)
           (parse-statement-list))]))




(define (parse-program)
  (consume "program")
  (define name (parse-identifier))
  (consume "{")

  (define declarations (parse-var-declarations))
  (define routines (parse-routines))
  (define statements (parse-statement-list))

  (consume "}")

  (unless (eq? (token-type (current-token)) 'eof)
    (error 'parse
           (format "Syntax error on line ~a: unexpected token"
                   (token-line (current-token)))))

  (list 'program name declarations routines statements))






(define (test-file filename)
  (displayln (string-append "\nTesting: " filename))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (exn-message e)))])
    (set! tokens (scan (file->string filename)))
    (set! position 0)
    (displayln (parse-program))))






(for ([i (in-range 1 6)])
  (test-file (format "Testfile-~a.txt" i)))


(displayln "\nTesting: EmptyRoutine")

(set! tokens
      (scan "program EmptyRoutine {
               routine doNothing() returns int {
               }
               ;
             }"))

(set! position 0)
(displayln (parse-program))



(displayln "\nTesting: KeywordRoutine")

(set! tokens
      (scan "program KeywordRoutine {
               routine routine() returns int {
                 yield 5;
               }
               invoke routine();
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: DeclAsVariable")

(set! tokens
      (scan "program DeclAsVariable {
               decl decl, result as int;
               5 => decl;
               decl + 2 => result;
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: Comments")

(set! tokens
      (scan "program CommentTest {
               -- This is a comment
               decl x as int;
               5 => x; -- Another comment
               x + 2 => x;
             }"))

(set! position 0)
(displayln (parse-program))





(displayln "\nTesting: OperatorPrecedence")

(set! tokens
      (scan "program OperatorPrecedence {
               decl result as int;
               2 + 3 * 4 => result;
             }"))

(set! position 0)
(displayln (parse-program))





(displayln "\nTesting: OperatorAssociativity")

(set! tokens
      (scan "program OperatorAssociativity {
               decl result as int;
               10 - 3 - 2 => result;
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: YieldAsVariable")

(set! tokens
      (scan "program YieldAsVariable {
               decl yield, result as int;
               5 => yield;
               yield + 2 => result;
               yield => result;
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: YieldReturn")

(set! tokens
      (scan "program YieldReturn {
               routine calculate() returns int {
                 decl x as int;
                 10 => x;
                 yield x;
               }
               ;
             }"))

(set! position 0)
(displayln (parse-program))





(displayln "\nTesting: RequiredNames")

(set! tokens
      (scan "program kenobi {
               decl skywalker, vader, organa, palpatine as int;
               10 => skywalker;
               skywalker + 5 => vader;
               vader => organa;
               organa => palpatine;
             }"))

(set! position 0)
(displayln (parse-program))





(displayln "\nTesting: UntilAndDo")

(set! tokens
      (scan "program UntilAndDo {
               decl until, do as int;
               10 => until;
               20 => do;
               loop until [until < do] do {
                 until + 1 => until;
               };
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: ReturnsAndProgram")

(set! tokens
      (scan "program ContextKeywords {
               decl returns, program as int;

               routine check() returns int {
                 5 => returns;
                 yield returns;
               }

               10 => program;
               invoke check() => returns;
               program + returns => program;
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: InvokeInExpression")

(set! tokens
      (scan "program InvokeInExpression {
               decl result as int;

               routine calculate() returns int {
                 yield 5;
               }

               invoke calculate() + 10 => result;
             }"))

(set! position 0)
(displayln (parse-program))




(displayln "\nTesting: InvalidInvoke")

(with-handlers ([exn:fail?
                 (lambda (e)
                   (displayln (exn-message e)))])
  (set! tokens
        (scan "program InvalidInvoke {
                 routine calculate() returns int {
                   yield 5;
                 }
                 invoke calculate(;
               }"))
  (set! position 0)
  (displayln (parse-program)))

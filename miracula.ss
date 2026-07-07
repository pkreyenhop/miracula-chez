#!/usr/bin/env scheme
(import (chezscheme))

;; =============================================================================
;; 1. SYNTAX & GRAPH STRUCTURAL RECORDS
;; =============================================================================

(define-record-type unevaluated (fields expr env))
(define-record-type evaluating)
(define-record-type evaluated (fields node))

(define-record-type int-node (fields val))
(define-record-type var-node (fields name))
(define-record-type lam-node (fields var body))
(define-record-type app-node (fields e1 e2))
(define-record-type sub-node (fields e1 e2))
(define-record-type add-node (fields e1 e2))
(define-record-type ifzero-node (fields cond t-branch f-branch))
(define-record-type cons-node (fields hd tl))
(define-record-type nil-node)
(define-record-type thunk-node (fields state-box))

(define-record-type script-bind (fields fname pats body))
(define-record-type repl-eval (fields expr))
(define-record-type pat-int (fields val))
(define-record-type pat-var (fields name))

;; =============================================================================
;; 2. LEXER IMPLEMENTATION
;; =============================================================================

(define (tokenize str)
  (let ([len (string-length str)]
        [is-digit? (lambda (c) (char-numeric? c))]
        [is-alpha? (lambda (c) (char-alphabetic? c))])
    (letrec ([loop 
              (lambda (i acc)
                (if (>= i len)
                    (reverse (cons 'TOK_EOF acc))
                    (let ([c (string-ref str i)])
                      (cond
                        [(char-whitespace? c) (loop (+ i 1) acc)]
                        [(char=? c #\\) (loop (+ i 1) (cons 'TOK_LAMBDA acc))]
                        [(char=? c #\.) (loop (+ i 1) (cons 'TOK_DOT acc))]
                        [(char=? c #\() (loop (+ i 1) (cons 'TOK_LPAREN acc))]
                        [(char=? c #\)) (loop (+ i 1) (cons 'TOK_RPAREN acc))]
                        [(char=? c #\[) (loop (+ i 1) (cons 'TOK_LBRACK acc))]
                        [(char=? c #\]) (loop (+ i 1) (cons 'TOK_RBRACK acc))]
                        [(char=? c #\,) (loop (+ i 1) (cons 'TOK_COMMA acc))]
                        [(char=? c #\=) (loop (+ i 1) (cons 'TOK_ASSIGN acc))]
                        [(char=? c #\+) (loop (+ i 1) (cons 'TOK_ADD acc))]
                        [(char=? c #\-)
                         (if (and (< (+ i 1) len) (char=? (string-ref str (+ i 1)) #\>))
                             (loop (+ i 2) (cons 'TOK_ARROW acc))
                             (loop (+ i 1) (cons 'TOK_SUB acc)))]
                        [(is-digit? c)
                         (letrec ([read-num 
                                   (lambda (j num-acc)
                                     (if (and (< j len) (is-digit? (string-ref str j)))
                                         (read-num (+ j 1) (string-append num-acc (string (string-ref str j))))
                                         (list j (string->number num-acc))))])
                           (let ([res (read-num (+ i 1) (string c))])
                             (loop (car res) (cons (list 'TOK_INT (cadr res)) acc))))]
                        [(is-alpha? c)
                         (letrec ([read-var 
                                   (lambda (j var-acc)
                                     (if (and (< j len)
                                              (or (is-alpha? (string-ref str j))
                                                  (is-digit? (string-ref str j))
                                                  (char=? (string-ref str j) #\_)))
                                         (read-var (+ j 1) (string-append var-acc (string (string-ref str j))))
                                         (list j var-acc)))])
                           (let* ([res (read-var (+ i 1) (string c))]
                                  [next-j (car res)]
                                  [s (cadr res)]
                                  [tok (cond
                                         [(string=? s "ifzero") 'TOK_IFZERO]
                                         [(string=? s "then")   'TOK_THEN]
                                         [(string=? s "else")   'TOK_ELSE]
                                         [else                  (list 'TOK_VAR s)])])
                             (loop next-j (cons tok acc))))]
                        [else (error 'lexer (format "Unexpected character: ~a" c))]))))])
      (loop 0 '()))))

;; =============================================================================
;; 3. PARSER MECHANICS
;; =============================================================================

(define (parse tokens)
  (let* ([cur-toks tokens]
         [peek (lambda () (car cur-toks))]
         [consume (lambda () (set! cur-toks (cdr cur-toks)))])
    
    (letrec 
        ([parse-expr
          (lambda ()
            (let ([t (peek)])
              (cond
                [(eq? t 'TOK_LAMBDA)
                 (consume)
                 (let ([v (peek)])
                   (if (and (pair? v) (eq? (car v) 'TOK_VAR))
                       (let ([x (cadr v)])
                         (consume)
                         (unless (eq? (peek) 'TOK_DOT) (error 'parser "Expected '.' after lambda variable"))
                         (consume)
                         (make-lam-node x (parse-expr)))
                       (error 'parser "Expected variable after lambda '\\'")))]
                [(eq? t 'TOK_IFZERO)
                 (consume)
                 (let ([cond-node (parse-expr)])
                   (unless (eq? (peek) 'TOK_THEN) (error 'parser "Expected 'then'"))
                   (consume)
                   (let ([t-branch (parse-expr)])
                     (unless (eq? (peek) 'TOK_ELSE) (error 'parser "Expected 'else'"))
                     (consume)
                     (make-ifzero-node cond-node t-branch (parse-expr))))]
                [else (parse-add-sub)])))]

         [parse-add-sub
          (lambda ()
            (letrec ([loop (lambda (left)
                             (let ([t (peek)])
                               (cond
                                 [(eq? t 'TOK_ADD) (consume) (loop (make-add-node left (parse-app)))]
                                 [(eq? t 'TOK_SUB) (consume) (loop (make-sub-node left (parse-app)))]
                                 [else left])))])
              (loop (parse-app))))]

         [parse-app
          (lambda ()
            (letrec ([loop (lambda (left)
                             (let ([t (peek)])
                               (if (or (and (pair? t) (or (eq? (car t) 'TOK_INT) (eq? (car t) 'TOK_VAR)))
                                       (eq? t 'TOK_LPAREN) (eq? t 'TOK_LBRACK))
                                   (loop (make-app-node left (parse-atom)))
                                   left)))])
              (loop (parse-atom))))]

         [parse-atom
          (lambda ()
            (let ([t (peek)])
              (cond
                [(and (pair? t) (eq? (car t) 'TOK_INT)) (consume) (make-int-node (cadr t))]
                [(and (pair? t) (eq? (car t) 'TOK_VAR)) (consume) (make-var-node (cadr t))]
                [(eq? t 'TOK_LPAREN)
                 (consume)
                 (let ([e (parse-expr)])
                   (unless (eq? (peek) 'TOK_RPAREN) (error 'parser "Expected ')'"))
                   (consume)
                   e)]
                [(eq? t 'TOK_LBRACK) (consume) (parse-list-elements)]
                [else (error 'parser (format "Unexpected token inside atom expression: ~a" t))])))]

         [parse-list-elements
          (lambda ()
            (if (eq? (peek) 'TOK_RBRACK)
                (begin (consume) (make-nil-node))
                (let ([head (parse-expr)])
                  (cond
                    [(eq? (peek) 'TOK_COMMA) (consume) (make-cons-node head (parse-list-elements))]
                    [(eq? (peek) 'TOK_RBRACK) (consume) (make-cons-node head (make-nil-node))]
                    [else (error 'parser "Expected ',' or ']' in list literal")]))))])

      (if (member 'TOK_ASSIGN tokens)
          (let ([t (peek)])
            (if (and (pair? t) (eq? (car t) 'TOK_VAR))
                (let* ([name (cadr t)]
                       [_ (consume)]
                       [collect-patterns
                        (lambda ()
                          (let loop ()
                            (let ([pt (peek)])
                              (cond
                                [(and (pair? pt) (eq? (car pt) 'TOK_INT)) (consume) (cons (make-pat-int (cadr pt)) (loop))]
                                [(and (pair? pt) (eq? (car pt) 'TOK_VAR)) (consume) (cons (make-pat-var (cadr pt)) (loop))]
                                [(eq? pt 'TOK_ASSIGN) (consume) '()]
                                [else (error 'parser "Malformed equation left hand side")]))))])
                  (make-script-bind name (collect-patterns) (parse-expr)))
                (error 'parser "Left hand side of binding must start with an identifier")))
          (let ([e (parse-expr)])
            (unless (eq? (peek) 'TOK_EOF) (error 'parser "Trailing tokens left unparsed"))
            (make-repl-eval e))))))

;; =============================================================================
;; 4. ENVIRONMENT RUNTIME WORKSPACE
;; =============================================================================

(define (env-lookup env key)
  (let ([pair (assoc key env)])
    (if pair (cdr pair) #f)))

(define (whnf env n)
  (cond
    [(int-node? n) n]
    [(lam-node? n) n]
    [(cons-node? n) n]
    [(nil-node? n) n]
    [(var-node? n)
     (let ([x (var-node-name n)])
       (if (or (string=? x "hd") (string=? x "tl"))
           n
           (let ([val (env-lookup env x)])
             (cond
               [(and val (thunk-node? val))
                (let* ([sb (thunk-node-state-box val)]
                       [state (unbox sb)])
                  (cond
                    [(evaluated? state) (evaluated-node state)]
                    [(evaluating? state) (error 'runtime (format "Infinite loop on identifier: ~a" x))]
                    [(unevaluated? state)
                     (set-box! sb (make-evaluating))
                     (let ([result (whnf (unevaluated-env state) (unevaluated-expr state))])
                       (set-box! sb (make-evaluated result))
                       result)]))]
               [val (whnf env val)]
               [else (error 'runtime (format "Unbound variable: ~a" x))]))))]
    [(app-node? n)
     (let ([target (whnf env (app-node-e1 n))]
           [arg (app-node-e2 n)])
       (cond
         [(and (var-node? target) (string=? (var-node-name target) "hd"))
          (let ([forced-arg (whnf env arg)])
            (if (cons-node? forced-arg)
                (whnf env (cons-node-hd forced-arg))
                (error 'runtime "hd expects a list")))]
         [(and (var-node? target) (string=? (var-node-name target) "tl"))
          (let ([forced-arg (whnf env arg)])
            (if (cons-node? forced-arg)
                (whnf env (cons-node-tl forced-arg))
                (error 'runtime "tl expects a list")))]
         [(lam-node? target)
          (let* ([x (lam-node-var target)]
                 [body (lam-node-body target)]
                 [shared-thunk (make-thunk-node (box (make-unevaluated arg env)))]
                 [extended-env (cons (cons x shared-thunk) env)])
            (whnf extended-env body))]
         [else (error 'runtime "Non-functional application")]))]
    [(sub-node? n)
     (let ([v1 (whnf env (sub-node-e1 n))]
           [v2 (whnf env (sub-node-e2 n))])
       (if (and (int-node? v1) (int-node? v2))
           (make-int-node (- (int-node-val v1) (int-node-val v2)))
           (error 'runtime "Subtraction expects integers")))]
    [(add-node? n)
     (let ([v1 (whnf env (add-node-e1 n))]
           [v2 (whnf env (add-node-e2 n))])
       (if (and (int-node? v1) (int-node? v2))
           (make-int-node (+ (int-node-val v1) (int-node-val v2)))
           (error 'runtime "Addition expects integers")))]
    [(ifzero-node? n)
     (let ([cond-val (whnf env (ifzero-node-cond n))])
       (if (int-node? cond-val)
           (if (= (int-node-val cond-val) 0)
               (whnf env (ifzero-node-t-branch n))
               (whnf env (ifzero-node-f-branch n)))
           (error 'runtime "Condition must resolve to an integer")))]
    [(thunk-node? n)
     (let* ([sb (thunk-node-state-box n)]
            [state (unbox sb)])
       (cond
         [(evaluated? state) (evaluated-node state)]
         [(evaluating? state) (error 'runtime "Infinite loop inside thunk")]
         [(unevaluated? state)
          (set-box! sb (make-evaluating))
          (let ([result (whnf (unevaluated-env state) (unevaluated-expr state))])
            (set-box! sb (make-evaluated result))
            result)]))]
    [else (error 'runtime "Unknown node mapping type")]))

(define (print-node env node)
  (cond
    [(int-node? node) (number->string (int-node-val node))]
    [(lam-node? node) (format "\\~a. <closure>" (lam-node-var node))]
    [(var-node? node) (var-node-name node)]
    [(app-node? node) (format "(~a ~a)" (print-node env (app-node-e1 node)) (print-node env (app-node-e2 node)))]
    [(sub-node? node) (format "(~a - ~a)" (print-node env (sub-node-e1 node)) (print-node env (sub-node-e2 node)))]
    [(add-node? node) (format "(~a + ~a)" (print-node env (add-node-e1 node)) (print-node env (add-node-e2 node)))]
    [(ifzero-node? node) "<conditional>"]
    [(thunk-node? node) "<thunk>"]
    [(nil-node? node) "[]"]
    [(cons-node? node)
     (letrec ([collect (lambda (current)
                         (let ([forced (whnf env current)])
                           (cond
                             [(cons-node? forced) (cons (print-node env (cons-node-hd forced)) (collect (cons-node-tl forced)))]
                             [(nil-node? forced) '()]
                             [else (list (print-node env forced))])))]
              [elements (collect node)])
       (if (null? elements)
           "[]"
           (let ([joined (car elements)])
             (for-each (lambda (el) (set! joined (string-append joined "," el))) (cdr elements))
             (format "[~a]" joined))))]
    [else "<unknown>"]))

;; =============================================================================
;; 5. SCRIPT DESUGARER FOR RECURSIVE PATTERNS
;; =============================================================================

(define (build-list n proc)
  (let loop ([i 0]) (if (= i n) '() (cons (proc i) (loop (+ i 1))))))

(define (foldr proc init lst)
  (if (null? lst) init (proc (car lst) (foldr proc init (cdr lst)))))

(define (desugar-equations eqs)
  (cond
    [(null? eqs) (error 'desugar "Empty equation sequence")]
    [(and (null? (cdr eqs)) (null? (script-bind-pats (car eqs))))
     (script-bind-body (car eqs))]
    [(and (null? (cdr eqs)) (= (length (script-bind-pats (car eqs))) 1) (pat-var? (car (script-bind-pats (car eqs)))))
     (make-lam-node (pat-var-name (car (script-bind-pats (car eqs)))) (script-bind-body (car eqs)))]
    [else
     (let* ([first-eq (car eqs)]
            [arity (length (script-bind-pats first-eq))])
       (for-each (lambda (e)
                   (unless (= (length (script-bind-pats e)) arity)
                     (error 'desugar "Equations have mismatched parameter arities")))
                 eqs)
       (letrec* ([param-names (build-list arity (lambda (idx) (format "p~a" idx)))]
              [build-decision-tree
               (lambda (remaining-eqs)
                 (if (null? remaining-eqs)
                     (error 'desugar "Pattern matching exhausted without catch-all")
                     (letrec* ([eq (car remaining-eqs)]
                            [rest (cdr remaining-eqs)]
                            [check-pats
                             (lambda (params patterns tree-body)
                               (cond
                                 [(and (null? params) (null? patterns)) tree-body]
                                 [(and (pair? params) (pair? patterns))
                                  (let ([p (car params)]
                                        [pat (car patterns)])
                                    (cond
                                      [(pat-int? pat)
                                       (make-ifzero-node (make-sub-node (make-var-node p) (make-int-node (pat-int-val pat)))
                                                         (check-pats (cdr params) (cdr patterns) tree-body)
                                                         (build-decision-tree rest))]
                                      [(pat-var? pat)
                                       (let ([substituted-body
                                              (if (string=? (pat-var-name pat) p)
                                                  tree-body
                                                  (make-app-node (make-lam-node (pat-var-name pat) tree-body) (make-var-node p)))])
                                         (check-pats (cdr params) (cdr patterns) substituted-body))]))]))])
                       (check-pats param-names (script-bind-pats eq) (script-bind-body eq)))))])
          (foldr make-lam-node (build-decision-tree eqs) param-names)))]))

(define (displayln x)
  (display x)
  (newline))

(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                      (loop (+ i 1))
                      i))]
         [end (let loop ([i (- len 1)])
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                    (loop (- i 1))
                    i))])
    (substring str start (+ end 1))))

(define (read-lines filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ([lines '()])
        (let ([line (get-line (current-input-port))])
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (group-bindings bindings)
  (let fold-loop ([bs bindings] [acc '()])
    (if (null? bs)
        (reverse acc)
        (let* ([b (car bs)]
               [fname (script-bind-fname b)]
               [pair (assoc fname acc)])
          (if pair
              (let ([new-acc (map (lambda (p)
                                    (if (string=? (car p) fname)
                                        (cons fname (append (cdr p) (list b)))
                                        p))
                                  acc)])
                (fold-loop (cdr bs) new-acc))
              (fold-loop (cdr bs) (cons (list fname b) acc)))))))

(define (load-script-file filename env)
  (if (not (file-exists? filename))
      (begin
        (printf "Script file '~a' not found. Starting with empty space.\n" filename)
        env)
      (let* ([lines (read-lines filename)]
             [trimmed-lines (map string-trim lines)]
             [raw-lines (filter (lambda (l)
                                  (and (not (string=? l ""))
                                       (not (and (>= (string-length l) 2)
                                                 (string=? (substring l 0 2) "||")))))
                                trimmed-lines)]
             [bindings (map (lambda (l)
                              (let ([parsed (parse (tokenize l))])
                                (if (script-bind? parsed)
                                    parsed
                                    (error 'load-script "Invalid expression structure in script file"))))
                            raw-lines)]
             [grouped (group-bindings bindings)])
        (let loop ([g grouped] [acc-env env])
          (if (null? g)
              acc-env
              (let* ([item (car g)]
                     [fname (car item)]
                     [eq-list (cdr item)]
                     [desugared (desugar-equations eq-list)])
                (loop (cdr g) (cons (cons fname desugared) acc-env))))))))

(define (repl env)
  (display "miranda> ")
  (flush-output-port (current-output-port))
  (let ([line (get-line (current-input-port))])
    (cond
      [(eof-object? line) (displayln "\nGoodbye.")]
      [(or (string=? line "/q") (string=? line "exit") (string=? line "quit"))
       (displayln "Goodbye.")]
      [(string=? line "/e")
       (displayln "Opening vi script.m ...")
       (system "vi script.m")
       (displayln "Reloading environment profiles from script.m...")
       (repl (load-script-file "script.m" '()))]
      [(string=? (string-trim line) "") (repl env)]
      [else
       (guard (exn [else (display "Error: ") (display-condition exn) (newline) (repl env)])
         (let* ([tokens (tokenize line)]
                [parsed (parse tokens)])
           (cond
             [(script-bind? parsed)
              (let* ([fname (script-bind-fname parsed)]
                     [pats (script-bind-pats parsed)]
                     [body (script-bind-body parsed)]
                     [final-lambda (desugar-equations (list parsed))]
                     [updated-env (cons (cons fname final-lambda) env)])
                (printf "Defined variable: ~a\n" fname)
                (repl updated-env))]
             [(repl-eval? parsed)
              (let* ([expr (repl-eval-expr parsed)]
                     [start-time (real-time)]
                     [result (whnf env expr)]
                     [end-time (real-time)]
                     [duration (- end-time start-time)])
                (printf "Result: ~a (Evaluated in ~d ms)\n" (print-node env result) duration)
                (repl env))]
             [else (error 'repl "Unknown parsed type")])))])))

(define (main)
  (displayln "==================================================")
  (displayln " Chez Scheme Version: Miranda REPL with Box Sharing")
  (displayln " Use '/e' to edit script.m, '/q' to exit          ")
  (displayln "==================================================")
  (let ([initial-env (load-script-file "script.m" '())])
    (repl initial-env)))

(main)

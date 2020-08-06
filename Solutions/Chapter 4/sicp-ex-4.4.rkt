#lang sicp

(define (eval exp env)
  (define (analyze exp)
    (define (self-evaluating? exp) (or (number? exp) (string? exp)))
    (define (analyze-self-evaluating exp) (lambda (env) exp))
    
    (define (quoted? exp) (tagged-list? exp 'quote))
    (define (text-of-quotation exp) (cadr exp))
    (define (analyze-quoted exp)
      (let ((qval (text-of-quotation exp)))
        (lambda (env) qval)))
    
    (define (variable? exp) (symbol? exp))
    (define (analyze-variable exp)
      (lambda (env) (lookup-variable-value exp env)))
    
    (define (assignment? exp) (tagged-list? exp 'set!))
    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))
    (define (analyze-assignment exp)
      (let ((var (assignment-variable exp))
            (vproc (analyze (assignment-value exp))))
        (lambda (env)
          (set-variable-value! var (vproc env) env)
          'ok)))
    
    (define (definition? exp) (tagged-list? exp 'define))
    (define (make-definition variable value)
      (list 'define variable value))
    (define (definition-variable exp)
      (let ((interface (cadr exp)))
        (if (symbol? interface)
            interface
            (car interface))))
    (define (definition-value exp)
      (let* ((operands (cdr exp))
             (interface (car operands)))
        (if (symbol? interface)
            (cadr operands)
            (make-lambda (cdr interface)    ; formal parameters
                         (cdr operands))))) ; body
    (define (analyze-definition exp)
      (let ((var (definition-variable exp))
            (vproc (analyze (definition-value exp))))
        (lambda (env)
          (define-variable! var (vproc env) env)
          'ok)))
    
    (define (if? exp) (tagged-list? exp 'if))
    (define (make-if predicate consequent alternative)
      (list 'if predicate consequent alternative))
    (define (if-predicate exp) (cadr exp))
    (define (if-consequent exp) (caddr exp))
    (define (if-alternative exp)
      (let ((alternative (cdddr exp)))
        (if (null? alternative)
            'false
            (car alternative))))
    (define (analyze-if exp)
      (let ((pproc (analyze (if-predicate exp)))
            (cproc (analyze (if-consequent exp)))
            (aproc (analyze (if-alternative exp))))
        (lambda (env)
          (if (true? (pproc env))
              (cproc env)
              (if (eq? 'false aproc)
                  'false
                  (aproc env))))))

    (define (and? exp) (tagged-list? exp 'and))
    (define (or? exp) (tagged-list? exp 'or))
    (define (and->if exp)
      (define result 'true)
      (define (transform exps)
        (if (null? exps)
            result
            (begin (set! result (car exps))
                   (list 'if result (transform (cdr exps))))))
      (transform (cdr exp)))
    (define (or->if exp)
      (define (transform exps)
        (if (null? exps)
            'false
            (let ((value (car exps)))
              (list 'if
                    value
                    value
                    (transform (cdr exps))))))
      (transform (cdr exp)))
    
    (define (lambda? exp) (tagged-list? exp 'lambda))
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))
    (define (analyze-lambda exp)
      (let ((vars (lambda-parameters exp))
            (bproc (analyze-sequence (lambda-body exp))))
        (lambda (env) (make-procedure vars bproc env))))
    
    (define (begin? exp) (tagged-list? exp 'begin))
    (define (make-begin seq) (cons 'begin seq))
    (define (begin-actions exp) (cdr exp))
    (define (last-exp? seq) (null? (cdr seq)))
    (define (first-exp seq) (car seq))
    (define (rest-exps seq) (cdr seq))

    (define (sequence->exp seq)
      (cond ((null? seq) seq)
            ((last-exp? seq) (first-exp seq))
            (else (make-begin seq))))
    
    (define (analyze-sequence exps)
      (define (execute-sequence procs env)
        (cond ((null? (cdr procs)) ((car procs) env))
              (else ((car procs) env)
                    (execute-sequence (cdr procs) env))))
      (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence -- ANALYZE"))
        (lambda (env) (execute-sequence procs env))))

    (define (cond? exp) (tagged-list? exp 'cond))
    (define (cond-clauses exp) (cdr exp))
    (define (cond-else-clause? clause)
      (eq? (cond-predicate clause) 'else))
    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (cdr clause))
    (define (cond->if exp)
      (define (expand-clauses clauses)
        (if (null? clauses)
            'false                          ; no else clause
            (let ((first (car clauses))
                  (rest (cdr clauses)))
              (if (cond-else-clause? first)
                  (if (null? rest)
                      (sequence->exp (cond-actions first))
                      (error
                       "ELSE clause isn't last -- COND->IF"
                       clauses))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions
                                           first))
                           (expand-clauses rest))))))
      (expand-clauses (cond-clauses exp)))
    
    (define (application? exp) (pair? exp))
    (define (operator exp) (car exp))
    (define (operands exp) (cdr exp))
    (define (no-operands? ops) (null? ops))
    (define (first-operand ops) (car ops))
    (define (rest-operands ops) (cdr ops))
    (define (analyze-application exp)
      (let ((fproc (analyze (operator exp)))
            (aprocs (map analyze (operands exp))))
        (lambda (env)
          (execute-application (fproc env)
                               (map (lambda (aproc) (aproc env))
                                    aprocs)))))
    (define (execute-application proc args)
      (cond ((primitive-procedure? proc)
             (apply-primitive-procedure proc args))
            ((compound-procedure? proc)
             ((procedure-body proc)
              (extend-environment
               (map make-binding
                    (procedure-parameters proc) args)
               (procedure-environment proc))))
            (else (error
                   "Unknown procedure type -- EXECUTE-APPLICATION"
                   proc))))
    
    (cond ((self-evaluating? exp) 
           (analyze-self-evaluating exp))
          ((quoted? exp) (analyze-quoted exp))
          ((variable? exp) (analyze-variable exp))
          ((assignment? exp) (analyze-assignment exp))
          ((definition? exp) (analyze-definition exp))
          ((if? exp) (analyze-if exp))
          ((and? exp) (analyze (and->if exp)))
          ((or? exp) (analyze (or->if exp)))
          ((lambda? exp) (analyze-lambda exp))
          ((begin? exp)
           (analyze-sequence (begin-actions exp)))
          ((cond? exp) (analyze (cond->if exp)))
          ((application? exp) (analyze-application exp))
          (else
           (error "Unknown expression type -- ANALYZE"
                  exp))))
  ((analyze exp) env))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))
    
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
    
(define apply-in-underlying-scheme apply)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedure-names)
  (frame-variables primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive proc))
       (frame-values primitive-procedures)))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (make-binding var val) (list var val))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cadr binding))

(define (empty-frame? frame) (null? (cdr frame)))
(define (make-frame bindings) (cons 'table bindings))
(define (frame-variables frame) (map binding-variable (cdr frame)))
(define (frame-values frame) (map binding-value (cdr frame)))
(define (add-binding-to-frame! binding frame)
  (set-cdr! frame (cons binding (cdr frame))))
(define (first-binding frame) (cadr frame))
(define (last-bindings frame) (cddr frame))
(define (lookup-variable-in-frame var frame)
  (if (empty-frame? frame)
      'Unbound-variable
      (let ((binding (first-binding frame)))
        (if (eq? var (binding-variable binding))
            binding
            (lookup-variable-in-frame var 
                                      (make-frame (last-bindings frame)))))))

(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (extend-environment frame base-env)
  (cons frame base-env))

(define (lookup-variable var env)
  (if (eq? env the-empty-environment)
      'Unbound-variable
      (let ((binding (lookup-variable-in-frame
                      var (first-frame env))))
        (if (eq? 'Unbound-variable binding)
            (lookup-variable
             var (enclosing-environment env))
            binding))))

(define (lookup-variable-value var env)
  (let ((binding (lookup-variable var env)))
    (if (eq? 'Unbound-variable binding)
        (error "Unbound variable" var)
        (binding-value binding))))

(define (set-variable-value! var val env)
  (let ((binding (lookup-variable var env)))
    (if (eq? 'Unbound-variable binding)
        (error "Unbound variable -- SET!" var)
        (set-cdr! binding val))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (lookup-variable-in-frame var frame)))
    (if (eq? 'Unbound-variable binding)
        (add-binding-to-frame! (make-binding var val) frame)
        (set-cdr! binding val))))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define primitive-procedures
  (make-frame (list (make-binding 'car car)
                    (make-binding 'cdr cdr)
                    (make-binding 'cons cons)
                    (make-binding 'null? null?)
                    (make-binding '= =)
                    (make-binding '< <)
                    (make-binding '> >)
                    (make-binding '+ +)
                    (make-binding '- -)
                    (make-binding '* *)
                    (make-binding '/ /)
                    ; (make-binding ')
                    ; <more primitives>
                    )))

(define (setup-environment)
  (let ((initial-env
         (extend-environment
          (make-frame
           (map make-binding
                (primitive-procedure-names)
                (primitive-procedure-objects)))
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (driver-loop)
  (display ";;; M-Eval input:") (newline)
  (let* ((input (read))
         (output (eval input the-global-environment)))
    (display ";;; M-Eval value:") (newline)
    (display output) (newline)
    (newline))
  (driver-loop))

(driver-loop)

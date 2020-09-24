#lang racket
(require "../main.rkt"
         (for-syntax syntax/name syntax/parse
                     syntax/id-set)
         racket)

(define-syntax (module-begin stx)
  (define defined-variables (make-hasheqv))
  (define used-variables (make-hasheqv))
    
  (define (mark-defined! idt)
    (unless (or (eq? '_ (syntax-e idt))
                (not (syntax-original? (syntax-local-introduce idt))))
      (let* ([phase (phase)]
             [idt (syntax-shift-phase-level idt phase)])
        (unless (hash-ref defined-variables phase #f)
          (hash-set! defined-variables phase (mutable-free-id-set #:phase phase)))
        (free-id-set-add! (hash-ref defined-variables phase)
                          idt))))
  
  (define (mark-used! idt)
    (let* ([phase (phase)]
           [idt (syntax-shift-phase-level idt phase)])
      (unless (hash-ref used-variables phase #f)
        (hash-set! used-variables phase (mutable-free-id-set #:phase phase)))
      (free-id-set-add! (hash-ref used-variables phase)
                        idt)))

  (define phase (make-parameter 0))

  (define-syntax-class formals
    (pattern ())
    (pattern x:id #:do [(mark-defined! #'x)])
    (pattern (x:id . r:formals) #:do [(mark-defined! #'x)]))

  (define result 
    (syntax-case stx ()
      [(_ mtl ...) (local-expand #'(#%module-begin mtl ...) 'module-begin '())]))
  
  (transform
   result 'module-begin
   #:expr
   (syntax-parser
     #:literal-sets (kernel-literals)
     [(~or* x:id
            (#%top . x:id)
            (#%variable-reference x:id)
            (#%variable-reference (#%top . x:id)))
      (mark-used! #'x)
      this-syntax]
     [(#%plain-lambda ~! _:formals _:Expr ...)
      this-syntax]
     [(case-lambda ~! [_:formals _:Expr ...] ...)
      this-syntax]
     [((~or* letrec-values let-values) ~! ([f:formals _:Expr] ...) _:Expr ...)
      this-syntax]
     [_ #f])
   
   #:module-level-form
   (syntax-parser
     #:literal-sets (kernel-literals)
     [(begin-for-syntax mtl ...)
      (parameterize ([phase (add1 (phase))])
        (syntax-parse (syntax-shift-phase-level #'(mtl ...) -1)
          [(shifted:Module-Level-Form ...) (void)]))
      this-syntax]
     [((~or* module module*) _ _ plain)
      (let ([stx (syntax-shift-phase-level #'plain (phase))])
        (parameterize ([phase 0])
          (syntax-parse stx
            [_:Module-Begin-Form (void)])))
      this-syntax]
     [_ #f])

   #:general-top-level-form
   (syntax-parser
     #:literal-sets (kernel-literals)
     [(define-syntaxes _ e)
      (parameterize ([phase (add1 (phase))])
        (syntax-parse (syntax-shift-phase-level #'e -1)
          [_:Expr (void)]))
      this-syntax]
     [_ #f])
   )
  
  (define unused-set (apply append
                            (for/list
                                ([(ph vars) (in-hash defined-variables)])
                              (define same-phase-used
                                (hash-ref used-variables ph #f))
                              (when same-phase-used
                                (free-id-set-subtract! vars same-phase-used))
                              (free-id-set->list vars))))

  (unless (null? unused-set)
    ((error-display-handler)
     "warning : unused variables"
     (make-exn:fail:syntax "" (current-continuation-marks) unused-set)))
  result)

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket) #%module-begin))
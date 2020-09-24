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
      (let* ([phase (current-phase)])
        (unless (hash-ref defined-variables phase #f)
          (hash-set! defined-variables phase (mutable-free-id-set #:phase phase)))
        (free-id-set-add! (hash-ref defined-variables phase)
                          idt))))
  
  (define (mark-used! idt)
    (let* ([phase (current-phase)])
      (unless (hash-ref used-variables phase #f)
        (hash-set! used-variables phase (mutable-free-id-set #:phase phase)))
      (free-id-set-add! (hash-ref used-variables phase)
                        idt)))

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
     [(~or* x:id
            (_:/#%top . x:id)
            (_:/#%variable-reference x:id)
            (_:/#%variable-reference (#%top . x:id)))
      (mark-used! #'x)
      this-syntax]
     [(_:/#%plain-lambda ~! _:formals _:Expr ...)
      this-syntax]
     [(_:/case-lambda ~! [_:formals _:Expr ...] ...)
      this-syntax]
     [((~or* _:/letrec-values _:/let-values) ~! ([_:formals _:Expr] ...) _:Expr ...)
      this-syntax]
     [_ #f])
   
   #:module-level-form
   (syntax-parser
     [(_:/begin-for-syntax (~phase (~seq _:Module-Level-Form ...)))
      this-syntax]
     [((~or* _:/module _:/module*) _ _ (~phase _:Module-Begin-Form 0))
      this-syntax]
     [_ #f])

   #:general-top-level-form
   (syntax-parser
     [(_:/define-syntaxes _ (~phase _:Expr))
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
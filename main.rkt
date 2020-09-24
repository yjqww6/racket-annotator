#lang racket/base

(module current-transformer racket/base
  (define-syntax-rule (define-trans id ...)
    (begin (define id (make-parameter #f)) ...))
  (define-trans
    current-expr-transformer
    current-general-top-level-form-transformer
    current-module-level-form-transformer
    current-top-level-form-transformer)
  (provide (all-defined-out)))

(module utils racket/base
  (require syntax/parse)
  (provide setup)
  (define-splicing-syntax-class setup
    (pattern (~seq (~alt (~optional (~seq #:expr expr))
                         (~optional (~seq #:general-top-level-form gtl))
                         (~optional (~seq #:module-level-form mtl))
                         (~optional (~seq #:top-level-form tl))) ...)
             #:with (args ...) #'((~? (~@ #:expr expr))
                                  (~? (~@ #:general-top-level-form gtl))
                                  (~? (~@ #:module-level-form mtl))
                                  (~? (~@ #:top-level-form tl))))))

(module disarm racket/base
  ;;;from typed/racket
  (provide (protect-out disarm*))
  (define (disarm* stx)
    (let loop ([v stx])
      (cond
        [(syntax? v)
         (let* ([stx (syntax-disarm v orig-insp)]
                [r (loop (syntax-e stx))])
           (if (eq? r (syntax-e stx))
               stx
               (datum->syntax stx r stx stx)))]
        [(pair? v) (let ([a (loop (car v))]
                         [d (loop (cdr v))])
                     (if (and (eq? a (car v))
                              (eq? d (cdr v)))
                         v
                         (cons a d)))]
        [else v])))

  (define orig-insp (variable-reference->module-declaration-inspector
                     (#%variable-reference))))

(module class racket/base
  (require syntax/parse syntax/stx
           (submod ".." current-transformer))

  (provide (all-defined-out))

  (define-syntax-rule (syntax/track loc form)
    (let ([l loc] [f (syntax form)])
      (datum->syntax l (syntax-e f) l l)))

  (define-syntax-class (Transformed transformer)
    #:attributes (out)
    (pattern (~do (define o (transformer this-syntax)))
             #:when o
             #:with out (datum->syntax this-syntax (syntax-e o)
                                       this-syntax this-syntax)))
  
  (define-syntax-class Expr
    #:commit
    #:literal-sets (kernel-literals)
    #:attributes (out)
    (pattern (~var || (Transformed (current-expr-transformer))))
    (pattern out:id)
    (pattern ((~and head #%plain-lambda) formals expr:Expr ...+)
             #:with out (syntax/track this-syntax
                                      (head formals expr.out ...)))
    (pattern ((~and head case-lambda) [formals expr:Expr ...+] ...)
             #:with out (syntax/track this-syntax
                                      (head [formals expr.out ...] ...)))
    (pattern ((~and head (~or* if begin begin0 with-continuation-mark #%plain-app))
              expr:Expr ...)
             #:with out (syntax/track this-syntax
                                      (head expr.out ...)))
    (pattern ((~and head (~or* let-values letrec-values))
              ([(x:id ...) rhs:Expr] ...)
              expr:Expr ...)
             #:with out (syntax/track this-syntax
                                      (head ([(x ...) rhs.out] ...) expr.out ...)))
    (pattern ((~and head set!) x:id expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head x expr.out)))
    (pattern ((~and head #%expression) expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head expr.out)))
    (pattern out))

  (define-syntax-class General-Top-Level-Form
    #:commit
    #:literal-sets (kernel-literals)
    #:attributes (out)
    (pattern (~var || (Transformed (current-general-top-level-form-transformer))))
    (pattern ((~and head define-values) (x:id ...) expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head (x ...) expr.out)))
    (pattern (~and out ((~or* #%require define-syntaxes) . _)))
    (pattern :Expr))

  (define-syntax-class Module-Level-Form
    #:commit
    #:literal-sets (kernel-literals)
    #:attributes (out)
    (pattern (~var || (Transformed (current-module-level-form-transformer))))
    (pattern (~and out ((~or* #%provide begin-for-syntax #%declare module module*) . _)))
    (pattern :General-Top-Level-Form)
    )

  (define-syntax-class Top-Level-Form
    #:commit
    #:literal-sets (kernel-literals)
    #:attributes (out)
    (pattern (~var || (Transformed (current-top-level-form-transformer))))
    (pattern ((~and head #%expression) expr:Expr)
             #:with out (syntax/track this-syntax (head expr.out)))
    (pattern ((~or* module begin-for-syntax) . _)
             #:with out this-syntax)
    (pattern ((~and head begin) tl:Top-Level-Form ...)
             #:with out (syntax/track this-syntax (head tl.out ...)))
    (pattern :General-Top-Level-Form))
  )

(module transformer racket/base
  (require (submod ".." class)
           (submod ".." current-transformer)
           (submod ".." disarm)
           syntax/parse/define syntax/parse
           (for-syntax racket/base (submod ".." utils))
           (for-template racket/base))
  
  (define (transform stx ctx
                     #:expr [e (λ (_) #f)]
                     #:general-top-level-form [g (λ (_) #f)]
                     #:module-level-form [m (λ (_) #f)]
                     #:top-level-form [t (λ (_) #f)])
    (let ([stx (disarm* stx)])
      (parameterize ([current-expr-transformer e]
                     [current-general-top-level-form-transformer g]
                     [current-module-level-form-transformer m]
                     [current-top-level-form-transformer t])
        (case ctx
          [(top-level-form)
           (syntax-parse stx
             [m:Top-Level-Form #'m.out])]
          [(module-level-form)
           (syntax-parse stx
             [m:Module-Level-Form #'m.out])]
          [(general-top-level-form)
           (syntax-parse stx
             [m:General-Top-Level-Form #'m.out])]
          [(expr)
           (syntax-parse stx
             [m:Expr #'m.out])]
          [(module-begin)
           (syntax-parse stx #:literal-sets (kernel-literals)
             [(#%plain-module-begin m:Module-Level-Form ...)
              (syntax/track this-syntax (#%plain-module-begin m.out ...))])]))))
  
  (begin-for-syntax)
  
  (define-syntax-parser transformer
    [(_ s:setup)
     #'(λ (stx ctx)
         (transform stx ctx s.args ...))])
  
  (define-syntax-parser define-transformer
    [(_ name:id s:setup)
     #'(define name
         (transformer s.args ...))])
  (provide (all-defined-out)))

(module modbeg racket/base
  (require syntax/parse/define
           (for-syntax racket/base (submod ".." utils)
                       (submod ".." transformer)))

  (define-syntax-parser define-module-begin
    [(_ name:id s:setup)
     #'(define-syntax name
         (let ([f (transformer s.args ...)])
           (syntax-parser
             [(_ . r)
              (f (local-expand #'(#%module-begin . r) 'module-begin '())
                 'module-begin)])))])
  
  (provide define-module-begin))

(require (for-syntax 'transformer 'class) 'modbeg)
(provide (for-syntax (all-from-out 'transformer 'class))
         (all-from-out 'modbeg))
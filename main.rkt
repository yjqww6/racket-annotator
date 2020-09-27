#lang racket/base

(module current-transformer racket/base
  (define-syntax-rule (define-trans id ...)
    (begin (define id (make-parameter #f)) ...))
  (define-trans
    current-expr-transformer
    current-general-top-level-form-transformer
    current-module-level-form-transformer
    current-top-level-form-transformer)
  (define current-phase (make-parameter 0))
  (provide (all-defined-out)))

(module literals racket/base
  (require syntax/parse/define syntax/parse
           (for-syntax racket/base racket/syntax syntax/stx)
           (submod ".." current-transformer)
           (prefix-in orig: racket/base))

  (define base-phase (variable-reference->module-base-phase (#%variable-reference)))

  (define-simple-macro (define-kernel-literal name:id ...)
    #:with (provided ...) (stx-map (λ (x) (format-id x "/~a" x)) #'(name ...))
    (begin
      (define-syntax-class provided
        (pattern _:id
                 #:when (free-identifier=? this-syntax #'name (current-phase) base-phase)))
      ...
      (provide provided ...)))
  (define-kernel-literal
    #%expression
    module module*
    #%plain-module-begin
    begin-for-syntax
    #%provide #%require #%declare
    define-values define-syntaxes
    #%plain-lambda case-lambda
    if begin begin0 let-values letrec-values
    set! quote quote-syntax with-continuation-mark #%plain-app #%top
    #%variable-reference))

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
           (submod ".." current-transformer)
           (submod ".." literals))

  (provide (all-defined-out) current-phase)

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
    #:attributes (out)
    (pattern (~var || (Transformed (current-expr-transformer))))
    (pattern out:id)
    (pattern (head:/#%plain-lambda formals expr:Expr ...+)
             #:with out (syntax/track this-syntax
                                      (head formals expr.out ...)))
    (pattern (head:/case-lambda [formals expr:Expr ...+] ...)
             #:with out (syntax/track this-syntax
                                      (head [formals expr.out ...] ...)))
    (pattern ((~and head (~or* _:/if _:/begin _:/begin0 _:/with-continuation-mark _:/#%plain-app))
              expr:Expr ...)
             #:with out (syntax/track this-syntax
                                      (head expr.out ...)))
    (pattern ((~and head (~or* _:/let-values _:/letrec-values))
              ([(x:id ...) rhs:Expr] ...)
              expr:Expr ...)
             #:with out (syntax/track this-syntax
                                      (head ([(x ...) rhs.out] ...) expr.out ...)))
    (pattern (head:/set! x:id expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head x expr.out)))
    (pattern (head:/#%expression expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head expr.out)))
    (pattern out))

  (define-syntax-class General-Top-Level-Form
    #:commit
    #:attributes (out)
    (pattern (~var || (Transformed (current-general-top-level-form-transformer))))
    (pattern (head:/define-values (x:id ...) expr:Expr)
             #:with out (syntax/track this-syntax
                                      (head (x ...) expr.out)))
    (pattern (~and out ((~or* _:/#%require _:/define-syntaxes) . _)))
    (pattern :Expr))

  (define-syntax-class Module-Level-Form
    #:commit
    #:attributes (out)
    (pattern (~var || (Transformed (current-module-level-form-transformer))))
    (pattern (~and out ((~or* _:/#%provide _:/begin-for-syntax _:/#%declare _:/module _:/module*) . _)))
    (pattern :General-Top-Level-Form)
    )

  (define-syntax-class Module-Begin-Form
    #:commit
    #:attributes (out)
    (pattern (head:/#%plain-module-begin mtl:Module-Level-Form ...)
             #:with out (syntax/track this-syntax (head mtl.out ...))))

  (define-syntax-class Top-Level-Form
    #:commit
    #:attributes (out)
    (pattern (~var || (Transformed (current-top-level-form-transformer))))
    (pattern (head:/#%expression expr:Expr)
             #:with out (syntax/track this-syntax (head expr.out)))
    (pattern ((~or* _:/module _:/begin-for-syntax) . _)
             #:with out this-syntax)
    (pattern (head:/begin tl:Top-Level-Form ...)
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
                     [current-top-level-form-transformer t]
                     [current-phase (syntax-local-phase-level)])
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
           (syntax-parse stx
             [m:Module-Begin-Form #'m.out])]))))
  
  (define-syntax-parser transformer
    [(_ s:setup)
     #'(λ (stx ctx)
         (transform stx ctx s.args ...))])
  
  (define-syntax-parser define-transformer
    [(_ name:id s:setup)
     #'(define name
         (transformer s.args ...))])
  (provide (all-defined-out)))

(module phase racket/base
  (require (for-syntax racket/base syntax/parse)
           syntax/parse (submod ".." current-transformer))
  (define-syntax ~phase
    (pattern-expander
     (syntax-parser
       [(_ form:expr (~optional delta:expr #:defaults ([delta #'(add1 (current-phase))])))
        #'(~and (~do (define old-phase (current-phase))
                     (define new-phase delta)
                     (current-phase new-phase))
                (~undo (current-phase old-phase))
                form
                (~do (current-phase old-phase))
                (~undo (current-phase new-phase)))])))
  (provide ~phase))

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
              (f (local-expand #'(#%module-begin . r) 'module-begin (list #'module*))
                 'module-begin)])))])
  
  (provide define-module-begin))

(require (for-syntax 'transformer 'class 'literals 'phase) 'modbeg)
(provide (for-syntax (all-from-out 'transformer 'class 'literals 'phase))
         (all-from-out 'modbeg))
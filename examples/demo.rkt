#lang racket/base
(require  "../main.rkt"
          (for-syntax racket/base syntax/name syntax/parse))

(provide (except-out (all-from-out racket/base) #%module-begin))

(begin-for-syntax
  (define-syntax-class formals
    #:attributes ([args 1])
    (pattern () #:with (args ...) #'())
    (pattern x:id #:with (args ...) #'(x))
    (pattern (x:id . r:formals) #:with (args ...) #'(x r.args ...)))
  )

(define-module-begin module-begin
  #:expr
  (syntax-parser
    [(head:/#%plain-lambda fmls:formals body:Expr ...)
     #:with name (datum->syntax #f (syntax-local-infer-name this-syntax #f))
     #'(head
        fmls
        (#%plain-app displayln (#%plain-app list 'name (~@ 'fmls.args fmls.args) ...))
        body.out ...)]
    [_ #f]))

(provide (rename-out [module-begin #%module-begin]))
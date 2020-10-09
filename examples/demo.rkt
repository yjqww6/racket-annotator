#lang racket/base
(require  "../main.rkt"
          (for-syntax racket/base syntax/name syntax/parse
                      syntax/parse/lib/function-header))

(provide (except-out (all-from-out racket/base) #%module-begin))

(define-module-begin module-begin
  #:expr
  (syntax-parser
    [(head:/#%plain-lambda fmls:formals body:Expr ...)
     #:with name (datum->syntax #f (syntax-local-infer-name this-syntax #f))
     #:with (args ...) #'fmls.params
     #'(head
        fmls
        (#%plain-app displayln (#%plain-app list 'name (~@ 'args args) ...))
        body.out ...)]
    [_ #f]))

(provide (rename-out [module-begin #%module-begin]))
#lang s-exp "demo-unused-variable-warner.rkt"
(module a racket/base (let ([x 1]
                            [y 1])
                        x))

(let ([x 1]
      [y 1])
  x)

(begin-for-syntax
  (let ([x 1]
        [y 1])
    x))


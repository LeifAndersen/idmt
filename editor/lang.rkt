#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     syntax/parse))

;; ===================================================================================================

(define-syntax for-editor
  (make-provide-pre-transformer
   (λ (stx mode)
     (syntax-parse stx
       [(_ name ...)
        (syntax-local-lift-module-end-declaration
         #`(module+ editor
             (provide name ...)))
        #'(only-meta-in 0)]))))

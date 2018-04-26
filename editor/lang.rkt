#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     racket/syntax
                     syntax/parse))

;; ===================================================================================================

(define-for-syntax editor-syntax-introduce (make-syntax-introducer))

;; ===================================================================================================

;; Since the editor submodule is a language detail, we want
;; a dedicated for-editor require subform.
(begin-for-syntax
  (struct for-editor-struct ()
    #:property prop:require-transformer
    (λ (str)
      (λ (stx)
        (syntax-parse stx
          [(_ name ...)
           #:with (marked-name ...) (editor-syntax-introduce #'(name ...))
           #:with r/b (editor-syntax-introduce (format-id stx "racket/base"))
           (syntax-local-lift-module-end-declaration
            #`(module+ editor
               (require r/b marked-name ...)))])
        (values '() '())))
    #:property prop:provide-pre-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           (syntax-local-lift-module-end-declaration
            #`(module+ editor
               (provide name ...)))
           #'(for-editor name ...)])))
    #:property prop:provide-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           '()])))))

(define-syntax for-editor (for-editor-struct))

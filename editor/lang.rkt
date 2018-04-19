#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/function
                     syntax/parse))

(define-for-syntax editor-syntax-introduce (make-syntax-introducer))

(define-syntax-parameter define-state
  (syntax-parser
    [(define-state marked-name:id body ...)
     (raise-syntax-error 'define-state "Use outside of define-editor is an error" this-syntax)]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id (~seq body ...))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(racket/base
                                     racket/class
                                     editor/lang))
     #`(begin
         (require marked-reqs ...)
         (splicing-syntax-parameterize ([define-state
                                          (syntax-parser
                                            [(_ st who) #'(begin)])])
           (define name
             (let ()
               (class/derived #,stx (name object% () #f) marked-body ...)))))]))

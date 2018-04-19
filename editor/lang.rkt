#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/function
                     syntax/parse))

;; ===================================================================================================

(define-for-syntax editor-syntax-introduce (make-syntax-introducer))

;; ===================================================================================================

(begin-for-syntax
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state marked-name:id body ...)
             #:attr name (editor-syntax-introduce (attribute marked-name)))))

(define-syntax-parameter defstate-parameter
  (syntax-parser
    [(_ stx who)
     (raise-syntax-error (syntax->datum #'who)
                         "Use outside of define-editor is an error"
                         this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     (quasisyntax/loc stx
       (defstate-parameter #,stx define-state))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id (~seq body ...))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(racket/base
                                     racket/class
                                     racket/serialize
                                     editor/lang))
     #`(begin
         (require marked-reqs ...)
         (splicing-syntax-parameterize ([defstate-parameter
                                          (syntax-parser
                                            [(_ st:defstate who) #'(begin)])])
           (define name
             (let ()
               (class/derived
                #,stx
                (name object% () #f)
                marked-body ...)))))]))

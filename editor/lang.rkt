#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/stxparam
         racket/splicing
         (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse
                     syntax/location))

;; ===================================================================================================

(define-for-syntax editor-syntax-introduce (make-syntax-introducer))

;; Creates a box for storing submodule syntax pieces.
;; Note that this box is newly instantiated for every module
;; that defines new editor types.
(define-for-syntax editor-submod-box (box '()))
(define-for-syntax (add-syntax-to-editor! stx)
  (define existing (unbox editor-submod-box))
  (when (null? existing)
    (syntax-local-lift-module-end-declaration
     #'(define-editor-submodule)))
  (set-box! editor-submod-box (append (reverse (syntax->list stx)) existing)))

(define-syntax (editor-submod stx)
  (syntax-parse stx
    [(_ body ...)
     (add-syntax-to-editor! (syntax-local-introduce #'(body ...)))
     #'(begin)]))

(define-syntax (define-editor-submodule stx)
  (syntax-parse stx
    [(_)
     #`(module* editor racket/base
         (require racket/serialize
                  racket/class)
         #,@(map syntax-local-introduce (reverse (unbox editor-submod-box))))]))

;; ===================================================================================================

(begin-for-syntax
  (define-syntax-class defstate
    #:literals (define-state)
    (pattern (define-state marked-name:id body ...)
             #:attr name (editor-syntax-introduce (attribute marked-name))
             #:attr getter (format-id this-syntax "get-~a" #'name)
             #:attr setter (format-id this-syntax "set-~a!" #'name))))

(define-syntax-parameter defstate-parameter
  (syntax-parser
    [(_ stx who)
     (raise-syntax-error (syntax->datum #'who) "Use outside of define-editor is an error" this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     (quasisyntax/loc stx
       (defstate-parameter #,stx define-state))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id
        (~and
         (~seq (~or plain-state:defstate
                    internal-body) ...)
         (~seq body ...)))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(racket/base
                                     racket/class
                                     racket/serialize
                                     editor/lang))
     #:with (state:defstate ...) (editor-syntax-introduce #'(plain-state ...))
     #`(begin
         (editor-submod
          (require marked-reqs ...)
          (#%require #,(quote-module-path))
          (splicing-syntax-parameterize ([defstate-parameter
                                           (syntax-parser
                                             [(_ st:defstate who)
                                              #'(begin
                                                  (define st.marked-name st.body (... ...)))])])
            (define name
              (let ()
                (class/derived
                 #,stx
                 (name object% () #f)
                 marked-body ...))))))]))

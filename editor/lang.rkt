#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/serialize
         racket/stxparam
         racket/splicing
         syntax/location
         syntax/parse/define
         (for-syntax racket/base
                     racket/match
                     racket/function
                     racket/require-transform
                     racket/provide-transform
                     racket/syntax
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/location
                     racket/serialize))

;; To be able to instantiate the found editors, we need each
;; module to be able to track the editors created in its
;; (partially defined) file.
(module key-submod racket/base
  ;(#%declare #:cross-phase-persistent)
  (provide editor-list-key editor-mixin-list-key)
  (define editor-list-key 'editor-list-cmark-key)
  (define editor-mixin-list-key 'editor-mixin-list-cmark-key))
(require (for-syntax 'key-submod))

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

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclass (interfaces ...)
        (~and
         (~seq (~or plain-state:defstate
                    internal-body) ...)
         (~seq body ...)))
     #:with (marked-interfaces ...) (editor-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(racket/base
                                     racket/class
                                     racket/serialize
                                     editor/lang))
     #:with marked-supclass (editor-syntax-introduce #'supclass)
     #:with (state:defstate ...) (editor-syntax-introduce #'(plain-state ...))
     (define state-methods (for/list ([i (in-list (attribute state.getter))])
                             (gensym (syntax->datum i))))
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
                 orig-stx
                 (name
                  marked-supclass
                  (marked-interfaces ...)
                  #f)
                 marked-body ...))))))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-editor #,stx name super (interfaces ...) body ...)]))

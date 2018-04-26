#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     racket/require-transform
                     racket/provide-transform
                     racket/syntax
                     syntax/parse))

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
            #`(editor-submod
               (require r/b marked-name ...)))])
        (values '() '())))
    #:property prop:provide-pre-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           (syntax-local-lift-module-end-declaration
            #`(editor-submod
               (provide name ...)))
           #'(for-editor name ...)])))
    #:property prop:provide-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           '()])))))

(define-syntax for-editor (for-editor-struct))

;; Just as for-editor is similar to for-syntax, for-elaborator
;; is similar to for-template. It lets helper modules bring in
;; editor components from another module.
;; XXX This NEEDS a proper from-editor implementation.
(begin-for-syntax
  (struct from-editor-struct ()
    #:property prop:require-transformer
    (λ (str)
      (λ (stx)
        (syntax-parse stx
          [(_ name ...)
           (for/fold ([i-list '()]
                      [is-list '()])
                     ([n (in-list (attribute name))])
             (define-values (i is)
               (expand-import #`(submod #,n editor)))
             (values (append i i-list)
                     (append is is-list)))])))
    #:property prop:provide-transformer
    (λ (str)
      (λ (stx mode)
        (syntax-parse stx
          [(_ name ...)
           (apply append (for/list ([i (in-list (attribute name))])
                           (expand-export i mode)))])))))

(define-syntax from-editor (from-editor-struct))

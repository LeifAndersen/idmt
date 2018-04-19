#lang racket/base

(provide (all-defined-out)
         (for-syntax current-editor-base-lang))

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

;; Because we use lang in building the stdlib, which is exported
;; as part of the lang, we want to use racket/base to bootstrap
;; that language.
(define-for-syntax current-editor-base-lang (make-parameter 'editor))
;(define-for-syntax current-editor-base-lang (make-parameter 'racket/base))

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

;; We want to require edit-time code into the modules editor submod.
(define-syntax (~require stx)
  (syntax-parse stx
    [(_ body ...)
     (for ([i (in-list (attribute body))])
       (define-values (imports import-sources) (expand-import i))
       (for ([s (in-list import-sources)])
         (match-define (struct* import-source ([mod-path-stx mod-path]
                                               [mode phase]))
           s)
         (define/syntax-parse maybe-require-submod
           ((make-syntax-introducer) (format-id #f "maybe-require-submod")))
         (syntax-local-lift-module-end-declaration
          #`(begin
              (define-syntax-parser maybe-require-submod
                [(_)
                 (when (module-declared? (convert-relative-module-path '(from-editor #,mod-path)) #t)
                   (syntax-local-lift-module-end-declaration
                    #'(~require (for-editor (for-meta #,phase (from-editor #,mod-path))))))
                 #'(begin)])
              (maybe-require-submod)))))
     #'(require body ...)]))

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
             ;; XXX This NEEDS a proper from-editor implementation.
             (define-values (i is)
               (expand-import #`(submod #,n editor)))
             (values (append i i-list)
                     (append is is-list)))])))))

(define-syntax from-editor (from-editor-struct))

(define-syntax (begin-for-editor stx)
  (syntax-parse stx
    [(_ code ...)
     #:with baselang (editor-syntax-introduce (datum->syntax stx (current-editor-base-lang)))
     #:with (marked-code ...) (editor-syntax-introduce #'(code ...))
     (syntax/loc stx
       (editor-submod
        (require baselang)
        marked-code ...))]))

(define-syntax (define-for-editor stx)
  (syntax-parse stx
    [(_ name:id body)
     (syntax/loc stx
       (begin-for-editor
         (define name body)))]
    [(_ name:function-header body)
     (syntax/loc stx
       (begin-for-editor
         (define name body)))]))

;; ===================================================================================================

;; Only introduced by #editor reader macro. Handles deserializing
;;  the editor.
(define-syntax (#%editor stx)
  (syntax-parse stx
    [(_ (elaborator-binding elaborator-name) body)
     (define/syntax-parse elaborator
       (syntax-local-lift-require (deserialize (syntax->datum #'elaborator-binding))
                                  (datum->syntax #f (syntax->datum #'elaborator-name))))
     #'(elaborator body)]))

(begin-for-syntax
  (define-syntax-class defelaborate
    #:literals (define-elaborate)
    (pattern (define-elaborate data body ...+)))
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

(define-syntax define-elaborate
  (syntax-parser
    [de:defelaborate
     (raise-syntax-error 'define-elaborate "Use outside of define-editor is an error" this-syntax)]))

(define-syntax (define-state stx)
  (syntax-parse stx
    [x:defstate
     (quasisyntax/loc stx
       (defstate-parameter #,stx define-state))]))

;; We don't want to get editor classes when
;; deserializing new editors.
(define deserialize-editor-classes?
  (make-parameter #t))

;; Each editor definition has three parts:
;; 1. A phase 1 elaboration
;; 2. A submodule with interaction code
;; 3. A deserializer submodule
(define-syntax (~define-editor stx)
  (syntax-parse stx
    [(_ orig-stx name:id supclass (interfaces ...)
        (~or (~optional (~seq #:base? b?) #:defaults ([b? #'#f]))
             (~optional (~seq #:direct-deserialize? dd?) #:defaults ([dd? #'#t])))
        ...
        (~and
         (~seq (~or plain-state:defstate
                    (~optional elaborator:defelaborate
                               #:defaults ([elaborator.data #'this]
                                           [(elaborator.body 1) (list #'this)]))
                    internal-body) ...)
         (~seq body ...)))
     #:with elaborator-name (format-id stx "~a:elaborate" #'name)
     #:with name-deserialize (format-id stx "~a:deserialize" #'name)
     #:with (marked-interfaces ...) (editor-syntax-introduce #'(interfaces ...))
     #:with (marked-body ...) (editor-syntax-introduce #'(body ...))
     #:with (marked-reqs ...) (map (compose editor-syntax-introduce (curry datum->syntax #'name))
                                   `(,(current-editor-base-lang)
                                     racket/class
                                     racket/serialize
                                     editor/lang))
     #:with marked-supclass (editor-syntax-introduce #'supclass)
     #:with (state:defstate ...) (editor-syntax-introduce #'(plain-state ...))
     (define dd?* (syntax-e #'dd?))
     (unless (or (not dd?*) (eq? 'module-begin (syntax-local-context)) (eq? 'module (syntax-local-context)))
       (raise-syntax-error #f "Must be defined at the module level" #'orig-stx))
     (define serialize-method (gensym 'serialize))
     (define deserialize-method (gensym 'deserialize))
     (define copy-method (gensym 'copy))
     (define elaborator-method (gensym 'elaborator))
     (define state-methods (for/list ([i (in-list (attribute state.getter))])
                             (gensym (syntax->datum i))))
     (define base? (syntax-e (attribute b?)))
     #`(begin
         #,@(if dd?*
                (list #'(provide elaborator-name)
                      #'(begin-for-syntax
                          (let ()
                            (define b (continuation-mark-set-first #f editor-list-key))
                            (when (and b (box? b))
                              (set-box! b (cons #'name (unbox b)))))))
                '())
         (define-syntax (elaborator-name stx)
           (syntax-parse stx
             [(_ data)
              #'(let ()
                  (define elaborator.data (deserialize 'data))
                  elaborator.body ...)]))
         (#,@(if dd?*
                 #`(editor-submod
                    (require marked-reqs ...)
                    (#%require #,(quote-module-path)))
                 #'(begin))
          (splicing-syntax-parameterize ([defstate-parameter
                                           (syntax-parser
                                             [(_ st:defstate who)
                                              #'(begin
                                                  (define st.marked-name st.body (... ...)))])])
            (define name
              (let ()
                #,@(for/list ([sm (in-list state-methods)])
                     #`(define-local-member-name #,sm))
                (class/derived
                 orig-stx
                 (name
                  marked-supclass
                  (marked-interfaces ...)
                  #f)
                 #,@(for/list ([i (in-list (attribute state.marked-name))]
                               [sm (in-list state-methods)])
                      #`(define/public (#,sm) #,i))
                 marked-body ...))))))]))

(define-syntax (define-base-editor* stx)
  (syntax-parse stx
    [(_ name:id super (interfaces ...) body ...)
     #`(~define-editor #,stx name super (interfaces ...) #:base? #t body ...)]))

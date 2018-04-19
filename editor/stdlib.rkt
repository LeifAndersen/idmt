#lang racket/base

(provide (all-defined-out))

(require "lang.rkt"
         (for-editor racket/match
                     racket/set
                     racket/list
                     racket/class
                     racket/serialize
                     racket/contract/base
                     racket/string
                     (except-in racket/gui/base
                                editor-snip%
                                editor-canvas%))
         racket/contract/base
         file/convertible
         racket/set
         racket/list
         racket/math
         racket/draw
         racket/class
         racket/serialize
         racket/match
         racket/string
         racket/format
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

;; Because this module is part of the editor language,
;;  its base lang needs to be something more like racket/base
;;  rather than editor
(begin-for-syntax
  (current-editor-base-lang 'racket/base))

(define-base-editor* base$ object% ()
  (super-new)
  (define-state content #f))

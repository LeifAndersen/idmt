#lang racket/base

(provide (all-defined-out))

(require "lang.rkt"
         (for-syntax racket/base))

(define-base-editor* base$ object% ()
  (super-new)
  (define-state content #f))

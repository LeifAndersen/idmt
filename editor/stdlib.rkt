#lang racket/base

(provide (all-defined-out))

(require "lang.rkt")

(define-base-editor* base$
  (super-new)
  (define-state content #f))

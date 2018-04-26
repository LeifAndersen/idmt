#lang racket/base

(provide (all-defined-out))

(require "lang.rkt"
         racket/class)

(module editor racket/base
  (provide base$)
  (define base$ 42))

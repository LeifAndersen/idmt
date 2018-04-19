#lang s-exp syntax/module-reader
editor
#:wrapper1 (λ (t)
             (parameterize ([current-readtable (make-editor-readtable)])
               (t)))
#:info (λ (key default defproc)
         (case key
           [(color-lexer) lex-editor]
           [else (defproc key default)]))

(require "read-editor.rkt")

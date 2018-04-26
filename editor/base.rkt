(module f racket/base
  (#%plain-module-begin
   (provide (for-editor (all-from-out (submod "stdlib.rkt" editor))))
   (require "lang.rkt"
            (for-editor "lang.rkt"
                        (submod "stdlib.rkt" editor)))))

(module f racket/base
  (#%plain-module-begin
   (provide (for-editor (all-from-out (submod "stdlib.rkt" editor))))
   (require "lang.rkt")

   (module+ editor
     (require "lang.rkt"
              (submod "stdlib.rkt" editor)))))

(module f racket/base
  (#%plain-module-begin
   (provide (for-editor (all-from-out "stdlib.rkt" (from-editor "stdlib.rkt"))))
   (require "lang.rkt"
            (for-editor "lang.rkt"
                        (from-editor "stdlib.rkt")))))

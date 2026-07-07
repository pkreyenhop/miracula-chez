(import (chezscheme))

(optimize-level 3)
(generate-wpo-files #t)

;; 1. Compile the source into an architecture-independent object file (.so)
(compile-program "miracula.ss" "miracula.so")

;; 2. Bundle the object code into a native standalone program binary
(compile-whole-program "miracula.wpo" "miracula")

(exit)

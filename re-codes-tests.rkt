#lang racket

(require redex/reduction-semantics
         "re-syntax.rkt"
         "re-codes.rkt")

;; code and decode are inverses.

(redex-check regexLCode
             #:satisfying (type (get-tree P) (get-regex P))
             (eq? (term (decode (code (get-tree P) (get-regex P))
                                (get-regex P)))
                  (term (get-tree P)))
             #:attempts 10000)

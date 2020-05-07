#lang racket

(require redex/reduction-semantics
         "re-codes.rkt"
         "re-syntax.rkt"
         "re-semantics.rkt"
         "re-small-step.rkt")

;; semantics soundness

(redex-check regexLState
             #:satisfying (in-regex (input-string Inp) (input-regex Inp))
             (not (eq? (term (interp (input-regex Inp) (input-string Inp)))
                       (term failure)))
             #:attempts 10000)

;; parsing result soundness

(redex-check regexLState
             #:satisfying (in-regex (input-string Inp) (input-regex Inp))
             (term (tc (decode (match (interp (input-regex Inp)
                                              (input-string Inp))
                                 [(Ok B) B]
                                 [failure cnil])
                               (input-regex Inp))
                       (input-regex Inp)))
             #:attempts 10000)

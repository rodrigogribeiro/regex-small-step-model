#lang racket

(require redex/reduction-semantics
         "re-syntax.rkt"
         "re-semantics.rkt")

  ;; simple test cases

  (build-derivations (in-regex (scons 1 (scons 2 snil)) epsilon (R snil s)))

  (build-derivations (in-regex (scons 1 (scons 2 snil))
                               (chr 1)
                               (R (scons 1 snil) (scons 2 snil))))

  (build-derivations (in-regex (scons 1 (scons 2 snil))
                               (cat (chr 1) (chr 2))
                               (R (scons 1 (scons 2 snil)) snil)))  

  (build-derivations (in-regex (scons 1 (scons 2 snil))
                               (choice (chr 1) (chr 2))
                               (R (scons 1 snil) (scons 2 snil))))

  (build-derivations (in-regex (scons 1 (scons 2 snil))
                               (choice (chr 2) (chr 1))
                               (R (scons 1 snil) (scons 2 snil))))

  (build-derivations (in-regex (scons 1 (scons 2 snil))
                               (star (chr 3))
                               (R snil (scons 1 (scons 2 snil)))))

  (build-derivations (in-regex (scons 1 (scons 1 (scons 1 snil)))
                     (star (chr 1))
                     (R (scons 1 (scons 1 snil))
                        (scons 1 snil))))

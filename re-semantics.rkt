#lang racket

(require redex/reduction-semantics
         "re-syntax.rkt")

;; standard RE semantics judgment


(define-judgment-form regexL
  #:contract (in-regex s e r)
  #:mode     (in-regex I I O)

  ;; semantics for epsilon

  [----------------------------"Eps"
   (in-regex s epsilon (R snil s))]

  ;; semantics for characters

  [-----------------------------"Char"
    (in-regex (scons c_1 s_1)
              (chr c_1)
              (R (scons c_1 snil) s_1))]

  ;; semantics for concatenation

  [(in-regex s_1  e_1 (R s_11 s_12))
   (in-regex s_12 e_2 (R s_21 s_22))
   (conc s_11 s_21 s_3)
   ----------------------------------------"Cat"
   (in-regex s_1 (cat e_1 e_2) (R s_3 s_22))]

  ;; semantics for choice 

  [(in-regex s_1 e_1 (R s_11 s_12))
   -------------------------------------------"Left"
   (in-regex s_1 (choice e_1 e_2) (R s_11 s_12))]


  [(in-regex s_2 e_2 (R s_21 s_22))
   -------------------------------------------"Right"
   (in-regex s_2 (choice e_1 e_2) (R s_21 s_22))]


  ;; semantics for star

  [---------------------------------------"Nil"
   (in-regex s_1 (star e_1) (R snil s_1))]

  [(in-regex s_1   e_1       (R s_11 s_12))
   (in-regex s_12 (star e_1) (R s_21 s_22))
   (conc s_11 s_21 s_3)
   ----------------------------------------"Cons"
   (in-regex s_1 (star e_1)  (R s_3 s_22))])


(provide in-regex)

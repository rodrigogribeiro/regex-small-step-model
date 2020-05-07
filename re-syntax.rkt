#lang racket

(require redex/reduction-semantics)

;; regular expression syntax

(define-language regexL
  ;; regular expressions

  (e ::= empty
     epsilon
     (chr c)
     (cat e e)
     (choice e e)
     (star e))
 
  ;; strings

  (c ::= natural)
  (s ::= snil
     (scons c s))
  
  ;; responses from parsing

  (r ::= (R s s)))

;; string concatenation as a judgment

(define-judgment-form regexL
  #:contract (conc s s s)
  #:mode     (conc I I O)
  [---------------------"Base"
    (conc snil s s)]

  [ (conc s_1 s_2 s_3)
    ----------------------------------------------- "Step"
    (conc (scons c_1 s_1) s_2 (scons c_1 s_3))])


;; exporting module definitions


(provide regexL conc)


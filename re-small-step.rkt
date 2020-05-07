#lang racket

(require redex/reduction-semantics
         "re-syntax.rkt"
         "re-codes.rkt")


;; extending syntax with semantics state

(define-extended-language regexLState regexLCode
  ;; direction

  (dir ::= start
           finish)

  ;; evaluation holes

  (E ::=
     Enil
     (catR e E)
     (catL E e)
     (choiceR e E)
     (choiceL E e)
     (starH E))

  ;; semantics state definition

  (state ::= (S dir e E B s))

  ;; input, for test generation

  (Inp ::= (inp e s)))

  ;; simple input projections
 
(define-metafunction regexLState
  input-regex : Inp -> e
  [(input-regex (inp e s)) e])

(define-metafunction regexLState
  input-string : Inp -> s
  [(input-string (inp e s)) s])

  ;; small step semantics

  (define ->
    (reduction-relation
     regexLState
     #:domain state

     ;; rules for epsilon

     (--> (S start epsilon E B s)
          (F finish epsilon E B s)
          "->Epsilon")

     ;; rules for character

     (--> (S start  (chr c_1) E B (scons c_1 s_2))
          (S finish (chr c_1) E B s_2)
          "->Chr")

     ;; rules for concatenation

     (--> (S start (cat e_1 e_2) E  B s)
          (S start e_1 (catL E e_2) B s)
          "->CatB")

     (--> (S finish e_1 (catL E e_2) B s)
          (S start  e_2 (catR e_1 E) B s)
          "->CatEL")

     (--> (S finish e_2 (catR e_1 E) B s)
          (S finish (cat e_1 e_2) E  B s)
          "->CatER")

     ;; rules for choice

     (--> (S start (choice e_1 e_2) E B s)
          (S start e_1 (choiceL E e_2) B s)
          "->LeftB")

     (--> (S start (choice e_1 e_2) E B s)
          (S start e_2 (choiceR e_1 E) B s)
          "->RightB")

     (--> (S finish e_1 (choiceL E e_2) B s)
          (S finish (choice e_1 e_2) E (ccons zero B) s)
          "->LeftE")

     (--> (S finish e_2 (choiceR e_1 E) B s)
          (S finish (choice e_1 e_2) E (ccons one B) s)
          "->RightE")

     ;; rules for star

     (--> (S start (star e_1) E B s)
          (S start e_1 (starH E) (ccons zero B) s)
          "->Star1")

     (--> (S finish e_1 (starH E) B s)
          (S start  e_1 (starH E) (ccons zero B) s)
          "->StarE1")

     (--> (S finish e_1 (starH E) B s)
          (S finish (star e_1) E (ccons one B) s)
          "->StarE2")))

  ;; reflexive-transitive closure of the semantics.

  (define ->* (compatible-closure -> regexLState state))

  ;; some functions for starting the semantics

  (define-metafunction regexLState
    initial : e s -> state
    [(initial e s) (S start e Enil cnil s)])


  (define-metafunction regexLState
    [(accepting (S finish e Enil b snil)) #t]
    [(accepting (S b e c b s))            #f])

  (define-metafunction regexLState
     bitcodes : state -> B
    [(bitcodes (S _ _ _ B _)) (reversecodes B)])

  ;; parsing function

  (define-metafunction regexLState
    interp : e s -> result
    [(interp e s) (match (apply-reduction-relation* ->* (initial e s))
                    [(cons x k) (if (accepting x) (Ok (bitcodes x)) failure)]
                    [x          failure])])



(provide -> ->* initial accepting interp bitcodes regexLState)

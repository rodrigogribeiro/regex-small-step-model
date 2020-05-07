#lang racket

(require redex/reduction-semantics
         "re-syntax.rkt")

(define-extended-language regexLCode regexL
  ;; codes

  (bit ::= zero
       one)
  (B ::= cnil
     (ccons bit B))

  ;; trees 

  (t ::= tunit
         (tchr char)
         (tpair t t)
         (tleft t)
         (tright t)
         tnil
         (tcons t t))

  (result ::= (Ok B)
          failure)

  (P ::= (p e t)))

(define-metafunction regexLCode
  get-regex : P -> e
  [(get-regex (p e t)) e])

(define-metafunction regexLCode
  get-tree : P -> t
  [(get-tree (p e t)) t])

;; functions on codes

(define-metafunction regexLCode
  revcodes : B B -> B
  [(revcodes cnil B) B]
  [(revcodes (ccons bit B) B_1) (revcodes B (ccons bit B_1))])

(define-metafunction regexLCode
  reversecodes : B -> B
  [(reversecodes B) (revcodes B cnil)])

(define-metafunction regexLCode
  codecat : B B -> B
  [(codecat cnil B) B]
  [(codecat (ccons bit_1 B_1) B_2) (ccons bit_1 (codecat B_1 B_2))])

;; type systems for trees

(define-judgment-form regexLCode
  #:contract (type t e)
  #:mode     (type I I)

  [-----------------------"T-unit"
    (type tunit epsilon) ]

  [-----------------------------"T-Chr"
    (type (tchr c_1) (chr c_1)) ]

  [         (type t_1 e_1)
            (type t_2 e_2)
   -------------------------------------"T-Pair"
   (type (tpair t_1 t_2) (cat e_1 e_2))]

  [         (type t_1 e_1)
   -------------------------------------"T-Left"
   (type (tleft t_1) (choice e_1 e_2))]

  [         (type t_2 e_2)
   -------------------------------------"T-Right"
    (type (tright t_2) (choice e_1 e_2))]


  [-------------------------------------"T-nil"
    (type tnil (star e_1))]

  [(type t_1 e_1)
   (type t_2 (star e_1))
   -------------------------------------"T-Cons"
   (type (tcons t_1 t_2) (star e_1))])


(define (typed? t)
  (not (null? (judgment-holds (type ,t e)
                              e))))

;; type checking trees

(define-metafunction regexLCode
  tc : t e -> boolean
  [(tc tunit epsilon) #t]
  [(tc (tchr c_1) (chr c_1)) #t]
  [(tc (tpair t_1 t_2) (cat e_1 e_2)) (and (tc t_1 e_1) (tc t_2 e_2))]
  [(tc (tleft t_1) (choice e_1 e_2)) (tc t_1 e_1)]
  [(tc (tright t_2) (choice e_1 e_2)) (tc t_2 e_2)]
  [(tc tnil (star e_1)) #t]
  [(tc (tcons t_1 t_2) (star e_1)) (and (tc t_1 e_1) (tc t_2 (star e_1)))]
  [(tc t e) #f])

;; coding trees

(define-metafunction regexLCode
  code : t e -> B
  [(code tunit epsilon) cnil]
  [(code (tchr c_1) (chr c_1)) cnil]
  [(code (tpair t_1 t_2) (cat e_1 e_2)) (codecat (code t_1 e_1) (code t_2 e_2))]
  [(code (tleft t_1) (choice e_1 e_2)) (ccons zero (code t_1 e_1))]
  [(code (tright t_2) (choice e_1 e_2)) (ccons one (code t_2 e_2))]
  [(code tnil (star e_1)) (ccons one cnil)]
  [(code (tcons t_1 t_2) (star e_1)) (ccons zero (codecat (code t_1 e_1)
                                                          (code t_2 (star e_1))))])

;; decoding

(define-metafunction regexLCode
  [(decode1 B epsilon) (cons tunit B)]
  [(decode1 B (chr c_1)) (cons (tchr c_1) B)]
  [(decode1 (ccons zero B) (choice e_1 e_2)) (match (decode1 B e_1)
                                               [(t_1 B_1) (cons (tleft t_1) B_1)])]
  [(decode1 (ccons one B) (choice e_1 e_2)) (match (decode1 B e_2)
                                               [(t_2 B_2) (cons (tright t_2) B_2)])]
  [(decode1 B (cat e_1 e_2)) (match (decode1 B e_1)
                               [(t_1 B_1) (match (decode B_1 e_2)
                                            [(t_2 B_2) (cons (tpair t_1 t_2) B_2)])])]
  [(decode1 (ccons one B_1) (star e_1)) (cons tnil B_1)]
  [(decode1 (ccons zero B_1) (star e_1))
   (match (decode1 B_1 e_1)
     [(t_2 B_2) (match (decode1 B_2 (star e_1))
                  [(t_3 B_3) (cons (tcons t_2 t_3) B_3)])])]
  [(decode1 B e) ()])


(define-metafunction regexLCode
  [(decode B e) (match (decode1 B e)
                  [(t_1 cnil) t_1]
                  [t          ()])])


(provide regexLCode code decode type typed? get-regex get-tree)

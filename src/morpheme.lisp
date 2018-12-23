(defpackage muddler.morpheme
  (:use :cl)
  (:export :morpheme
           :make-morpheme
           :morpheme-surface
           :morpheme-cost
           :morpheme-pos
           :morpheme-conj
           :morpheme-yomi
           :morpheme-hatsuon
           :+bos+
           :+eos+))
(in-package :muddler.morpheme)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct morpheme
    surface
    cost
    pos
    conj
    yomi
    hatsuon))


(defconstant +bos+
  (make-morpheme :surface ""
                 :cost 0
                 :pos '("文頭")
                 :conj nil
                 :yomi ""
                 :hatsuon ""))

(defconstant +eos+
  (make-morpheme :surface ""
                 :cost 0
                 :pos '("文末")
                 :conj nil
                 :yomi ""
                 :hatsuon ""))

(defpackage muddler.morpheme
  (:use :cl)
  (:export :morpheme
           :make-morpheme
           :morpheme-surface
           :morpheme-cost
           :morpheme-pos
           :morpheme-cform
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
    cform
    yomi
    hatsuon))


(defconstant +bos+
  (make-morpheme :surface ""
                 :cost 0
                 :pos '("文頭")
                 :cform nil
                 :yomi ""
                 :hatsuon ""))

(defconstant +eos+
  (make-morpheme :surface ""
                 :cost 0
                 :pos '("文末")
                 :cform nil
                 :yomi ""
                 :hatsuon ""))

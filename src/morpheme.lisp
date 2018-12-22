(defpackage muddler.morpheme
  (:use :cl)
  (:export :morpheme
           :make-morpheme
           :morpheme-surface
           :morpheme-cost
           :morpheme-pos
           :morpheme-cform
           :morpheme-yomi
           :morpheme-hatsuon))
(in-package :muddler.morpheme)


(defstruct morpheme
  surface
  cost
  pos
  cform
  yomi
  hatsuon)

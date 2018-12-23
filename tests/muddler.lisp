(defpackage muddler-test
  (:use :cl
        :muddler
        :prove))
(in-package :muddler-test)

;; NOTE: To run this test file, execute `(asdf:test-system :muddler)' in your Lisp.

(plan nil)

(princ "wait for load-tagger...")(fresh-line)
(defvar tagger (muddler:load-tagger #p"ipadic-2.7.0/"))
(princ "load-tagger done!")
(terpri)
(print (muddler:parse tagger "今日の天気は晴れです"))
(terpri)
(print (muddler:parse tagger "裏庭には二羽庭には二羽にわとりがいる"))
(terpri)
(print (muddler:parse tagger "これははにわです"))
(terpri)

(finalize)

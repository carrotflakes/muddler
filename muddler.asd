#|
  This file is a part of muddler project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "muddler"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("cl-double-array")
  :components ((:module "src"
                :components
                ((:file "muddler" :depends-on ("dictionary" "viterbi" "morpheme" "connect"))
                 (:file "viterbi" :depends-on ("morpheme" "connect"))
                 (:file "morpheme")
                 (:file "connect")
                 (:file "dictionary" :depends-on ("dic-parser"))
                 (:file "dic-parser"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "muddler-test"))))

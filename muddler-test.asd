#|
  This file is a part of muddler project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "muddler-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("muddler"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "muddler"))))
  :description "Test system for muddler"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))

(ql:quickload :muddler)

(defvar tagger (muddler::load-tagger #p"ipadic-2.7.0/"))
(print "done!")
(terpri)
(print (muddler::lattice tagger "今日の天気は晴れです"))

(defpackage muddler
  (:use :cl
        :muddler.dictionary
        :muddler.morpheme
        :muddler.lattice
        :cl-double-array)
  (:export :load-tagger
           :lattice))
(in-package :muddler)


(defstruct tagger
  words
  lattice-builder)


(defun cform-surface-suffix (cform)
  (if (string= (second cform) "*")
      ""
      (second cform)))

(defun cform-yomi-suffix (cform)
  (if (string= (third cform) "*")
      ""
      (or (third cform) (cform-surface-suffix cform))))

(defun cform-hatsuon-suffix (cform)
  (if (string= (fourth cform) "*")
      ""
      (or (fourth cform) (cform-yomi-suffix cform))))

(defun load-tagger (path)
  (let* ((dictionary (load-dictionary path))
         (dic (loop
                for (name . entry) in (dictionary-dics dictionary)
                append entry))
         (morphemes
           (loop
             for entry in dic
             for midashi = (cadr (assoc "見出し語" entry :test #'string=))
             for surface = (first midashi)
             for cost = (read-from-string (second midashi))
             for pos = (cadr (assoc "品詞" entry :test #'string=))
             for yomi = (cadr (assoc "読み" entry :test #'string=))
             for hatsuon = (cadr (assoc "発音" entry :test #'string=))
             for conj = (cadr (assoc "活用型" entry :test #'string=))
             if conj
             append (loop
                      with forms = (cadr (assoc conj
                                                (dictionary-cforms dictionary)
                                                :test #'string=))
                      with surface-base = (subseq surface
                                                  0
                                                  (- (length surface)
                                                     (length (cform-surface-suffix
                                                              (first forms)))))
                      with yomi-base = (subseq yomi
                                               0
                                               (- (length yomi)
                                                  (length (cform-yomi-suffix
                                                           (first forms)))))
                      with hatsuon-base = (subseq hatsuon
                                                  0
                                                  (- (length hatsuon)
                                                     (length (cform-hatsuon-suffix
                                                              (first forms)))))
                      for form in forms
                      do (assert (string= surface
                                          (cform-surface-suffix (first forms))
                                          :start1 (- (length surface)
                                                     (length (cform-surface-suffix
                                                              (first forms))))))
                      collect (make-morpheme :surface (format nil "~a~a"
                                                              surface-base
                                                              (cform-surface-suffix form))
                                             :cost cost
                                             :pos pos
                                             :cform (first form)
                                             :yomi (format nil "~a~a"
                                                              yomi-base
                                                              (cform-yomi-suffix form))
                                             :hatsuon (format nil "~a~a"
                                                              hatsuon-base
                                                              (cform-hatsuon-suffix form))))
             collect (make-morpheme :surface surface
                                    :cost cost
                                    :pos pos
                                    :cform nil
                                    :yomi yomi
                                    :hatsuon hatsuon))))
    (print (length morphemes))
    (print (first dic))
    (print (subseq morphemes 0 10))
    (let ((a (remove-duplicates (loop
                         for x in (dictionary-connect dictionary)
                         collect (caar x))
                       :test #'equal))
          (b (remove-duplicates (loop
                         for x in (dictionary-connect dictionary)
                         collect (cadar x))
                       :test #'equal)))
      (loop
        for x in a
        when (< 1 (length x))
        do (print x))
      (loop
        for x in b
        when (< 1 (length x))
        do (print x))
      '(print (set-difference b a :test #'equal)))
        
    (make-tagger :words dic
                 :lattice-builder (time (build-lattice-builder morphemes)))))

(defun node-result (node)
  (let ((result '()))
    (loop
      while node
      do (push node result)
      do (setf node (node-previous node)))
    result))

(defun lattice (tagger string)
  (with-slots (lattice-builder) tagger
    (let* ((nodes-list (build-lattice lattice-builder string))
           (end-node (first (aref nodes-list (1- (length nodes-list))))))
      (loop
        for node in (node-result end-node)
        do (print (node-cost node))
        do (print (node-morpheme node))))))

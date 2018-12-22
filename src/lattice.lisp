(defpackage muddler.lattice
  (:use :cl
        :cl-double-array
        :muddler.morpheme)
  (:export :build-lattice-builder
           :build-lattice))
(in-package :muddler.lattice)


(defstruct lattice-builder
  trie
  node-morpheme-table)

(defun build-lattice-builder (morphemes)
  (let* ((surfaces (mapcar #'morpheme-surface morphemes))
         (trie (build-double-array surfaces))
         (surface-morpheme-table (make-hash-table :test 'equal))
         (node-morpheme-table (make-hash-table :test 'eq)))
    (dolist (morpheme morphemes)
      (push morpheme (gethash (morpheme-surface morpheme) surface-morpheme-table)))
    (do-complete (trie "" :completed completed :node node)
      (setf (gethash node node-morpheme-table)
            (gethash completed surface-morpheme-table)))
    (make-lattice-builder :trie trie
                          :node-morpheme-table node-morpheme-table)))

(defun build-lattice (lattice-builder string)
  (with-slots (trie node-morpheme-table) lattice-builder
    (let ((array (make-array (length string) :initial-element 'nil)))
      (loop
        for start from 0 below (length string)
        do (do-common-prefix-search (trie string :start start :node node)
             (setf (aref array start)
                   (append (gethash node node-morpheme-table)
                           (aref array start)))))
      array)))

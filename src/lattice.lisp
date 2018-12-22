(defpackage muddler.lattice
  (:use :cl
        :cl-double-array
        :muddler.morpheme)
  (:export :build-lattice-builder
           :build-lattice
           :node-morpheme
           :node-cost
           :node-previous))
(in-package :muddler.lattice)


(defconstant +max-cost+ 1000000)

(defstruct node
  morpheme
  (cost +max-cost+)
  (previous nil))

(defun node-step-size (node)
  (max 1
       (length (morpheme-surface (node-morpheme node)))))

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

(defun connect-cost (node-left node-right)
  1)

; => vitavi
(defun build-lattice (lattice-builder string)
  (with-slots (trie node-morpheme-table) lattice-builder
    ; nodes?
    (let ((nodes-list (make-array (+ (length string) 2) :initial-element 'nil)))
      (setf (aref nodes-list 0)
            (list (make-node :morpheme +bos+ :cost 0))
            (aref nodes-list (1- (length nodes-list)))
            (list (make-node :morpheme +eos+)))
      (loop
        for start from 0 below (length string)
        for index from 1
        do (do-common-prefix-search (trie string :start start :node node)
             (dolist (morpheme (gethash node node-morpheme-table))
               (push (make-node :morpheme morpheme)
                     (aref nodes-list index)))))

      (dotimes (index (- (length nodes-list) 2))
        (dolist (node (aref nodes-list index))
          (loop
            with cost = (node-cost node)
            with next-index = (+ index (node-step-size node))
            for next-node in (aref nodes-list next-index)
            for next-cost = (+ cost
                               (connect-cost node next-node)
                               (morpheme-cost (node-morpheme next-node)))
            when (< next-cost (node-cost next-node))
            do (setf (node-cost next-node) next-cost
                     (node-previous next-node) node))))
                  
      nodes-list)))

 ; TODO: 未知語

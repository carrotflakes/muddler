(defpackage muddler.viterbi
  (:use :cl
        :cl-double-array
        :muddler.morpheme
        :muddler.connect)
  (:export :node-morpheme
           :node-cost
           :node-previous
           :build-viterbi
           :viterbi))
(in-package :muddler.viterbi)


(defconstant +max-cost+ most-positive-fixnum)
(defconstant +undefined-connect-cost+ 100000)

(defstruct node
  morpheme
  (cost +max-cost+)
  (previous nil))

(defun node-step-size (node)
  (let ((morpheme (node-morpheme node)))
    (if (or (eq morpheme +eos+) (eq morpheme +bos+))
        1
        (length (morpheme-surface morpheme)))))

(defstruct viterbi
  trie
  node-morpheme-table
  connect)

(defun build-viterbi (morphemes connect)
  (let* ((surfaces (mapcar #'morpheme-surface morphemes))
         (surface-morpheme-table (make-hash-table :test 'equal))
         (node-morpheme-table (make-hash-table :test 'eq))
         (complete-hook (lambda (node string)
                          (setf (gethash node node-morpheme-table)
                                (gethash string surface-morpheme-table)))))
    (dolist (morpheme morphemes)
      (push morpheme (gethash (morpheme-surface morpheme) surface-morpheme-table)))
    (make-viterbi :trie (build-double-array surfaces
                                            :complete-hook complete-hook)
                  :node-morpheme-table node-morpheme-table
                  :connect connect)))

(defun match (pattern morpheme)
  (let ((p-pos (pattern-pos pattern))
        (p-ctype (pattern-ctype pattern))
        (p-cform (pattern-cform pattern))
        (p-surface (pattern-surface pattern))
        (m-pos (morpheme-pos morpheme))
        (m-conj (morpheme-conj morpheme))
        (m-surface (morpheme-surface morpheme)))
    (and (equal p-pos m-pos)
         (if p-ctype (equal p-ctype (first m-conj)) t)
         (if p-cform (equal p-cform (second m-conj)) t)
         (if p-surface (equal p-surface m-surface) t))))

(defun connect-cost (connect node-left node-right)
  (loop
    with morpheme-left = (node-morpheme node-left)
    with morpheme-right = (node-morpheme node-right)
    for connect-entry in connect
    ;; do (when (match (connect-entry-left connect-entry) morpheme-left)
    ;;      (terpri)
    ;;      (print connect-entry)
    ;;      (print morpheme-right))
    when (and (match (connect-entry-left connect-entry) morpheme-left)
              (match (connect-entry-right connect-entry) morpheme-right))
    return (connect-entry-cost connect-entry)
    finally (return +undefined-connect-cost+)))

(defun viterbi (viterbi string)
  (with-slots (trie node-morpheme-table connect) viterbi
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
                               (connect-cost connect node next-node)
                               (morpheme-cost (node-morpheme next-node)))
            when (< next-cost (node-cost next-node))
            do (setf (node-cost next-node) next-cost
                     (node-previous next-node) node))))
                  
      nodes-list)))

 ; TODO: 未知語

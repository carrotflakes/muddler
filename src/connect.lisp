(defpackage muddler.connect
  (:use :cl)
  (:export :pattern-pos
           :pattern-ctype
           :pattern-cform
           :pattern-surface
           :connect-entry-left
           :connect-entry-right
           :connect-entry-cost
           :connect))
(in-package :muddler.connect)


(defstruct pattern
  pos
  ctype
  cform
  surface)

(defstruct connect-entry
  left
  right
  cost)

(defun build-pattern (pattern)
  '(when (and (equal (second pattern) "*") ; DELETEME
             (not (equal (third pattern) "*")))
    (print '!!!!)
    (print pattern))
  (make-pattern :pos (first pattern)
                :ctype (unless (equal (second pattern) "*") (second pattern))
                :cform (unless (equal (third pattern) "*") (third pattern))
                :surface (unless (equal (fourth pattern) "*") (fourth pattern))))

(defun connect (connect)
  (loop
    for (((left) (right)) cost) in connect
    collect (make-connect-entry
             :left (build-pattern left)
             :right (build-pattern right)
             :cost (read-from-string cost))))

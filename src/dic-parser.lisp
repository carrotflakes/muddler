(defpackage muddler.dic-parser
  (:use :cl)
  (:export :parse))
(in-package :muddler.dic-parser)

(defun skip-comment (stream)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (loop
    for char = (or (read-char stream nil nil)
                   (return))
    until (or (char= char #\CR) (char= char #\LF))))

(defun read-list (stream)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (assert (char= (read-char stream) #\())
  (loop
    for char = (peek-char t stream)
    until (char= char #\))
    if (char= char #\;)
    do (skip-comment stream)
    else
    collect (read-sexp stream)
    finally (read-char stream)))

(defun read-string (stream)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (coerce (if (char= (peek-char nil stream) #\")
              (progn
                (read-char stream)
                (loop
                  for char = (read-char stream)
                  while (char/= char #\")
                  collect (if (char= char #\\)
                              (read-char stream)
                              char)))
              (loop
                for char = (peek-char nil stream nil nil)
                while char
                until (member char '(#\( #\) #\SPACE #\TAB #\CR #\LF #\;) :test #'char=)
                collect (if (char= char #\\)
                            (and (read-char stream) (peek-char nil stream))
                            char)
                do (read-char stream)))
          'string))

(defun read-sexp (stream)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (if (char= (peek-char nil stream) #\()
      (read-list stream)
      (read-string stream)))

(defun parse (stream)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (loop
    for char = (peek-char t stream nil nil)
    while char
    if (char= char #\;)
    do (skip-comment stream)
    else
    collect (read-sexp stream)))

;(with-input-from-string (s " hoge 1 2 (3) \\\" \"hoge\" \"\\\"\" ") (print (parse s)))

;;;; -*- lexical-binding: t; -*-

(defconst axvr/macos?   (memq system-type '(darwin)))
(defconst axvr/linux?   (memq system-type '(gnu/linux gnu android)))
(defconst axvr/windows? (memq system-type '(windows-nt ms-dos cygwin)))

(defun axvr/flatten (lst)
  "Flatten a list of lists (of lists...)."
  (cond
   ((null lst) nil)
   ((atom lst) (list lst))
   (t
    (append (axvr/flatten (car lst))
            (axvr/flatten (cdr lst))))))

(defun axvr/current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(provide 'axvr-helpers)

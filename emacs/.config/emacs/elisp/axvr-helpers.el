;;;; -*- lexical-binding: t; -*-

(defconst axvr/macos?   (memq system-type '(darwin)))
(defconst axvr/linux?   (memq system-type '(gnu/linux gnu android)))
(defconst axvr/windows? (memq system-type '(windows-nt ms-dos cygwin)))

(defun axvr/flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (axvr/flatten (car mylist))
            (axvr/flatten (cdr mylist))))))

(defun axvr/current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(provide 'axvr-helpers)

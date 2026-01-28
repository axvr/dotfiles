;;;; -*- lexical-binding: t; -*-

(provide 'av-helpers)

(defun av/flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (av/flatten (car mylist))
            (av/flatten (cdr mylist))))))

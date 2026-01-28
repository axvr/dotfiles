;;;; -*- lexical-binding: t; -*-

(provide 'av-cua)

(cua-mode t)

(defun av/home-key ()
  (interactive "^")
    (if (= 0 (current-column))
        (beginning-of-line-text)
      (move-beginning-of-line nil)))

(global-set-key (kbd "<home>") 'av/home-key)

;;(global-set-key (kbd "C-a") 'mark-whole-buffer)
;;(global-set-key (kbd "C-s") 'save-buffer)
;;(global-set-key (kbd "C-S-s") 'write-file)
;;(global-unset-key (kbd "C-x C-w"))
;;(global-unset-key (kbd "C-x C-s"))
;;(global-set-key (kbd "C-o") 'find-file)

;;(global-unset-key (kbd "C-k"))
;;(global-set-key (kbd "C-k C-c") 'comment-region)
;;(global-set-key (kbd "C-k C-u") 'uncomment-region)

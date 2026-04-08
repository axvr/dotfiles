;;;; Override default key bindings. -*- lexical-binding: t; -*-

;; TODO: should these bindings be in a custom minor mode map?

;; UK keyboard has `#' as `M-3', but this isn't accessible from Emacs by default.
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))

;; Use Ibuffer instead of `list-buffers'.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Use regex with isearch by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

;; When using a trackpad and the control key is was very common to accidentally
;; increase/decrease the font size.  Let's remove those bindings.
(when (default-value 'mouse-wheel-mode)
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>")))

(defun axvr/home-key ()
  "Makes `home' key behave more like other editors and GUI apps."
  (interactive "^")
  (if (= 0 (current-column))
      (beginning-of-line-text)
    (move-beginning-of-line nil)))

;; Better `Home' and `End' key behaviours.
(global-set-key (kbd "<home>") 'axvr/home-key)
(global-set-key (kbd "<end>") 'move-end-of-line)

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun axvr/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p) (keyboard-quit))
   ((derived-mode-p 'completion-list-mode) (delete-completion-window))
   ((> (minibuffer-depth) 0) (abort-recursive-edit))
   (t (keyboard-quit))))

(define-key global-map (kbd "C-g") #'axvr/keyboard-quit-dwim)

(provide 'axvr-default-keys)

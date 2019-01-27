;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'

(menu-bar-mode -1)
(if (display-graphic-p)
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  (xterm-mouse-mode 1))

(show-paren-mode 1)

(column-number-mode 1)
(setq column-number-indicator-zero-based nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(setq ring-bell-function 'ignore)

(setq user-mail-address "av@axvr.io"
      user-full-name "Alex Vear")

;; TODO Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Behaviour similar to `textwidth' in Vim:
;; `auto-fill-mode' and `fill-column'

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq scroll-step 1
      scroll-conservatively 10000)
(put 'scroll-left 'disabled nil) ; Horizontal scrolling (`C-PgUp' & `C-PgDn')

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
;; (add-hook 'prog-mode-hook #'electric-pair-mode)

(fset 'yes-or-no-p 'y-or-n-p)

(define-minor-mode highlight-todos-mode
  "Highlight TODOs and other comment keywords"
  nil
  :lighter ""
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
          1 '((:foreground "#d78700") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'highlight-todos-mode)

(when (memq system-type '(cygwin windows-nt ms-dos))
  (set-face-attribute 'default nil :family "Consolas" :height 110))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      version-control t
      undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
      undo-tree-auto-save-history t
      save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)


;;; Packages
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "stable.melpa.org/packages/")) t))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 5)
        ("melpa-stable" . 0)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;;; TODO add undo-tree

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package markdown-mode :ensure t :defer t)

(use-package org
  :ensure t
  :defer t
  :hook (org-mode . org-indent-mode))
  ;; TODO set org directory for org-agenda

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package ledger-mode
  :ensure t
  :defer t)
  ;; FIXME `ledger-mode-clean-buffer' should sort in reverse order

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key restclient spacemacs-theme rainbow-delimiters ledger-mode ivy markdown-mode diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

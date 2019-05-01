;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'

;; Add custom elisp files to `load-path'
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; UI config
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  (xterm-mouse-mode 1))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq frame-title-format "GNU Emacs")

;; Set default fonts
(if (member "Inconsolata" (font-family-list))
    (set-face-attribute 'default nil :family "Inconsolata" :height 135)
  (when (memq system-type '(cygwin windows-nt ms-dos))
    (set-face-attribute 'default nil :family "Consolas" :height 110)))

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Keyboard scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Horizontal scrolling (`C-PgUp' & `C-PgDn')
(put 'scroll-left 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; Character encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Update buffers and `dired' when files change on disk
(global-auto-revert-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; Backups and save cursor position
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      version-control t
      save-place-file (concat user-emacs-directory "places")
      undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
      undo-tree-auto-save-history t)
(save-place-mode 1)

(setq user-mail-address "av@axvr.io"
      user-full-name "Alex Vear")


;;; Programming

;; Indentation settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; TODO configure indentation for each major mode

;; TODO Behaviour similar to `textwidth' in Vim:
;; `auto-fill-mode' and `fill-column'

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(show-paren-mode 1)
(column-number-mode 1)
(add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'prog-mode-hook 'prettify-symbols-mode) ; TODO only prettify `lambda'
;; (add-hook 'prog-mode-hook 'electric-pair-mode)

(define-minor-mode av/hl-todos-mode
  "Highlight TODOs and other common comment keywords"
  nil
  :lighter ""
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
          1 '((:foreground "#d78700") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'av/hl-todos-mode)


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

(setq use-package-always-ensure t)


(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode 1))


(use-package ivy
  :diminish ivy-mode counsel-mode
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1)
  (counsel-mode 1))

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode))

(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)

  (projectile-mode 1))

;;; TODO proof general

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; File types

(use-package markdown-mode :defer t)

(use-package org
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  (require 'org-man))

  ;; TODO set org directory for org-agenda

(use-package restclient
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package ledger-mode :defer t)
;; FIXME `ledger-mode-clean-buffer' should sort in reverse order

(load-theme 'tsdh-light t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (which-key use-package restclient rainbow-delimiters projectile markdown-mode ledger-mode ivy general diminish company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

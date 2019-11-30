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
           (scroll-bar-mode -1)
           (setq-default cursor-type 'bar))
  (xterm-mouse-mode 1))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(setq frame-title-format "GNU Emacs")


(defun current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(defun set-gtk-theme (variant)
  "Set the GTK theme variant for the current Emacs session."
  (interactive "sLight or dark? ")
  (call-process-shell-command
   (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"" variant "\" -name \"" (current-frame-name) "\"")))

(if (window-system)
    (set-gtk-theme "dark"))

(load-theme 'photon t)

;; Set default fonts
(if (member "Inconsolata" (font-family-list))
    (progn
      (set-face-attribute 'default nil :family "Inconsolata" :height 135)
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 135))
  (when (memq system-type '(cygwin windows-nt ms-dos))
    (progn (set-face-attribute 'default nil :family "Consolas" :height 110)
           (set-face-attribute 'fixed-pitch nil :family "Consolas" :height 110))))

(if (member "DejaVu Sans" (font-family-list))
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height 110))

;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Keyboard scrolling
(setq scroll-step 1
      scroll-conservatively 10000)

;; Horizontal scrolling (`C-PgUp' & `C-PgDn')
(put 'scroll-left 'disabled nil)

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

(define-minor-mode av/hl-todos-mode
  "Highlight TODOs and other common comment keywords"
  nil
  :lighter ""
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
          1 '((:foreground "#d75f5f") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'av/hl-todos-mode)


;;; Packages

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "stable.melpa.org/packages/")) t))

;; NOTE: Temporary until Fedora package GNU Emacs 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 5)
        ("melpa-stable" . 0)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; File types

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t
  :config
  (use-package julia-repl
    :ensure t
    :hook (julia-mode . julia-repl-mode)))

(use-package markdown-mode :ensure t :defer t)

(use-package org
  :ensure t
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  ;; TODO set org directory for org-agenda
  (require 'org-man))

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

(use-package ledger-mode :ensure t :defer t)
;; FIXME `ledger-mode-clean-buffer' should sort in reverse order

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (julia-shell clojure-mode julia-repl julia-mode use-package org restclient markdown-mode ledger-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; GNU Emacs -*- lexical-binding: t; -*-

;;; ----------------------------
;;; Core.

(setq user-mail-address "alex@vear.uk"
      user-full-name "Alex Vear")

;; File backups, save last cursor position and register persistence.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      save-place-file (concat user-emacs-directory "places")
      savehist-additional-variables '(register-alist))
(save-place-mode)
(savehist-mode)

(let ((default-directory (concat user-emacs-directory "elisp")))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(require 'av-helpers)

;;; ----------------------------
;;; Packages.

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 30) ("nongnu" . 20) ("melpa" . 10)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

;;; ----------------------------
;;; GUI.

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (context-menu-mode)
      (setq-default cursor-type 'bar)
      (global-set-key (kbd "<escape>") 'keyboard-escape-quit))
  (xterm-mouse-mode))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      frame-title-format "GNU Emacs")
;; (setq-default frame-title-format "%b %& emacs")

(defun av/current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(defun av/set-gtk-theme (variant)
  "Set the GTK theme variant for the current Emacs session."
  (when (and (display-graphic-p)
             (memq system-type '(gnu/linux))
             (not (null (executable-find "xprop"))))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \""
             variant "\" -name \"" (av/current-frame-name) "\""))))

(defun av/set-theme (theme &optional mode)
  (av/set-gtk-theme (if mode mode "dark"))
  (load-theme theme t))

(require-theme 'modus-themes)
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t)
;; (av/set-theme 'modus-operandi "light")

(use-package alabaster-themes
  :config (av/set-theme 'alabaster-themes-light-bg "light"))

;; TODO: evaluate if this is worth having.
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((projects  . 5)
			  (recents   . 5)
                          (bookmarks . 5))))

(pixel-scroll-precision-mode)

(mouse-wheel-mode)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      ;; Horizontal scroll on trackpad
      mouse-wheel-tilt-scroll 't
      mouse-wheel-flip-direction 't
      ;; Keyboard scrolling
      scroll-step 1
      scroll-conservatively 101)

;; Enable horizontal scroll (`C-PgUp' + `C-PgDn')
(put 'scroll-left 'disabled nil)

;;; ----------------------------
;;; Fonts.

(defun av/first-installed-font (fonts)
  (seq-find (lambda (x)
              (member (cdr (assoc :family x))
                      (font-family-list)))
            fonts))

(defun av/set-font (type fonts)
  (let ((font-attrs (av/first-installed-font fonts)))
    (when font-attrs
      (apply 'set-face-attribute type nil (av/flatten font-attrs)))))

(let ((monospace '(((:family . "IBM Plex Mono") (:height . 135))
                   ((:family . "JuliaMono")     (:height . 125))
                   ((:family . "Inconsolata")   (:height . 135))
                   ((:family . "Consolas")      (:height . 110))))
      (variable  '(((:family . "Cantarell")     (:height . 120))
                   ((:family . "DejaVu Sans")   (:height . 110)))))
  (av/set-font 'default monospace)
  (av/set-font 'fixed-pitch monospace)
  (av/set-font 'variable-pitch variable))

;;; ----------------------------
;;; Files.

;; Update buffers and directory listing when files change on disk.
(global-auto-revert-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; TODO
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq default-tab-width 4
      tab-width 4)

;; `display-fill-column-indicator-mode' & `fill-column'
;; (setq default-fill-column 120
;;       fill-column 120)

;; TODO
;; (global-visual-line-mode t)
;; (setq-default word-wrap t)
;; (setq-default truncate-lines t)

(setq version-control t
      vc-follow-symlinks t)

(setq uniquify-buffer-name-style 'forward)

(column-number-mode)
(electric-pair-mode)
(electric-indent-mode)
(delete-selection-mode)

(require 'project)

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default indicate-empty-lines t)

;; Highlight TODOs and more.
(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :init (add-hook 'prog-mode-hook #'hl-prog-extra-mode))

(use-package diff-hl :config (global-diff-hl-mode))

(use-package magit :defer t :functions magit-status)

;;; ----------------------------
;;; File types.

(use-package ledger-mode
  :mode ("\\.journal\\'" "\\.ledger\\'" "\\.hledger\\'")
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-report-auto-width nil
        ledger-report-links-in-register nil
        ledger-report-native-highlighting-arguments '("--color=always")))

(use-package markdown-mode :mode ("TODO\\'" "DOING\\'" "DONE\\'"))
(use-package org :hook (org-mode . org-indent-mode))

;; TODO
(setq scheme-program-name "csi -:c")
(use-package sly :config (setq inferior-lisp-program "sbcl"))
(use-package clojure-mode)

;;; ---------------------------
;;; Evil.

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu
	evil-want-C-u-scroll t
        evil-search-module 'evil-search
        evil-ex-search-case 'sensitive
        evil-search-wrap t
        evil-want-keybinding nil
	evil-shift-round nil
	evil-shift-width 4
	evil-indent-convert-tabs nil)
  :config
  (evil-mode)

  (evil-set-leader nil (kbd "<SPC>"))
  (evil-set-leader nil "\\" t)

  (evil-define-operator av/evil-commentary (beg end)
    "Emacs implementation of `tpope/vim-commentary'"
    :move-point nil
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  (evil-define-key 'normal 'global "gc" 'av/evil-commentary)

  (evil-define-key 'normal 'global "-" 'dired)

  (evil-define-command av/evil-retab (start end)
    "Emacs implementation of the `:retab' ex command in Vim"
    (interactive "<r>")
    (if indent-tabs-mode
      (tabify start end)
      (untabify start end)))

  (evil-ex-define-cmd "ret[ab]"    'av/evil-retab)
  (evil-ex-define-cmd "ter[minal]" 'ansi-term)
  (evil-ex-define-cmd "pa[ckadd]"  'package-list-packages))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

(use-package undo-fu)

(use-package undo-fu-session
  :after undo-fu
  :config
  ;; TODO: add other Git files.
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))


;; Prevent Emacs from appending "custom" stuff to this file.
;; (setq custom-file (make-temp-file "emacs-custom"))

;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings 'meta))

;; (require 'av-cua)

;; (av/package-install 'paren-face)
;; (setq paren-face-regexp "[][(){}]")
;; (global-paren-face-mode) ; TODO: only on prog mode.
;; (add-hook 'prog-mode-hook 'paren-face-mode)
;; (set-face-foreground 'parenthesis "#828282")

;; (fido-mode)

;; (av/package-install 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; (av/package-install 'company)
;; (global-company-mode)
;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;; (define-key company-active-map (kbd "M-.")  'company-show-location)

;; TODO find an alternative.
;; (av/package-install 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; serial-term for Replica 1

;; Control +/-/0 to zoom text.
(global-set-key (kbd "C-=") 'default-font-presets-scale-increase)
(global-set-key (kbd "C--") 'default-font-presets-scale-decrease)
(global-set-key (kbd "C-0") 'default-font-presets-scale-reset)

;; which-key?
;; some completion stuff is installed by default now.  enable it?

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(alabaster-themes clojure-mode dashboard diff-hl evil-collection
                      evil-numbers hl-prog-extra ledger-mode magit
                      markdown-mode sly undo-fu undo-fu-session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

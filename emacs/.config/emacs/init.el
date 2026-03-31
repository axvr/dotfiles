;;;; GNU Emacs configuration. -*- lexical-binding: t; -*-

;;; ----------------------------
;;; Core.

(setq user-full-name "Alex Vear" user-mail-address "alex@vear.uk")

;; File backups, save last cursor position and register persistence.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      save-place-file (concat user-emacs-directory "places")
      savehist-additional-variables '(register-alist))
(save-place-mode)
(savehist-mode)
(recentf-mode)

;; expand-file-name

(let ((default-directory (concat user-emacs-directory "elisp")))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(require 'axvr-helpers)
(require 'axvr-default-keys)

;; Prevent Emacs from appending "custom" stuff to this file.
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (setq custom-file (make-temp-file "emacs-custom"))
(load custom-file 'noerror 'nomessage)

;;; ----------------------------
;;; Packages.

;; TODO: package-vc, vc-use-package or alternatives.

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu" . 30) ("melpa" . 10) ("nongnu" . 5)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(when axvr/macos?
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

;;; ----------------------------
;;; GUI.

;; TODO: orderless.

(require 'axvr-style)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(setq ring-bell-function #'ignore)

(setq-default frame-title-format '("%n %b - %F"))
;; (project-mode-line project-mode-line-format)
(setq project-mode-line t)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;;; ----------------------------
;;; Files.

;; TODO: dired configuration.

(setq grep-command "rg -nS. --no-heading -g '!.git/*' "
      grep-use-null-device nil)

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
;; (word-wrap-whitespace-mode)

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

(use-package magit :defer t :functions magit-status)

(use-package diff-hl
  :config (global-diff-hl-mode)
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

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
(use-package sly :defer t :config (setq inferior-lisp-program "sbcl"))
(use-package clojure-mode) ; TODO: clojure-ts-mode?

(which-key-mode)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  ;; Override "super" binding on macOS.
  (when axvr/macos?
    (global-unset-key (kbd "s-z"))
    (global-set-key (kbd "s-z") 'undo-fu-only-undo)
    (global-set-key (kbd "s-Z") 'undo-fu-only-redo)))

(use-package undo-fu-session
  :after undo-fu
  :config
  ;; TODO: add other Git files.
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; (axvr/package-install 'paren-face)
;; (setq paren-face-regexp "[][(){}]")
;; (global-paren-face-mode) ; TODO: only on prog mode.
;; (add-hook 'prog-mode-hook 'paren-face-mode)
;; (set-face-foreground 'parenthesis "#828282")

;; (fido-mode)

;; (axvr/package-install 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; TODO find an alternative.
;; (axvr/package-install 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(setq reb-re-syntax 'string)

(use-package pdf-tools
  :mode "\\.pdf\\'"
  :hook
  (pdf-view-mode . pdf-view-roll-minor-mode) ; enable continuous scrolling
  :init
  (pdf-loader-install))

(require 'axvr-tools)

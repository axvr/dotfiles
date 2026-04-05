;;;; GNU Emacs configuration. -*- lexical-binding: t; -*-

;; TODO: move most of this file into the axvr directory.  `axvr-init.el' and symbolic link it.

;;; ----------------------------
;;; Core.

(setq user-full-name "Alex Vear"
      user-mail-address "alex@vear.uk")

;; File backups, save last cursor position and register persistence.
(setq backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory)))
      backup-by-copying t
      delete-old-versions t
      save-place-file (expand-file-name "places" user-emacs-directory)
      savehist-additional-variables '(register-alist))
(save-place-mode)
(savehist-mode)
(recentf-mode)

(let ((default-directory (expand-file-name "axvr" user-emacs-directory)))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(require 'axvr-helpers)
(require 'axvr-default-keys)

;; Prevent Emacs from appending "custom" stuff to this file.
(setq custom-file (make-temp-file "emacs-custom"))

;;; ----------------------------
;;; Packages.

(require 'axvr-packages)

(when axvr/macos?
  (use-package exec-path-from-shell
    :ensure (:wait t)
    :init
    (exec-path-from-shell-initialize)
    ;; Elpaca needs these values to be reset now that `PATH' is correct.
    (setq elpaca-makeinfo-executable (executable-find "makeinfo")
          elpaca-install-info-executable (executable-find "install-info"))
    ;; Rebuild Elpaca's docs if not built due to missing the above executables.
    (unless (file-exists-p (expand-file-name "elpaca/dir" elpaca-builds-directory))
      (elpaca-rebuild 'elpaca))))

;;; ----------------------------
;;; GUI.

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

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

(setq grep-command "rg -nS. --no-heading -g '!.git/*' "
      grep-use-null-device nil)

;; Update buffers and directory listing when files change on disk.
(global-auto-revert-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; TODO: dired configuration.
(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  (;; (dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; (use-package dired-subtree
;;   :after dired
;;   :bind
;;   ( :map dired-mode-map
;;     ("<tab>" . dired-subtree-toggle)
;;     ("TAB" . dired-subtree-toggle)
;;     ("<backtab>" . dired-subtree-remove)
;;     ("S-TAB" . dired-subtree-remove))
;;   :config
;;   (setq dired-subtree-use-backgrounds nil))

;; ;; https://codeberg.org/shinmera/.emacs/src/commit/43d39efc8d87a2c913b8e8c193c33582082af225/shinmera-general.el
;; (use-package server
;;   :demand t
;;   :config
;;   (define-hook after-init-hook ()
;;     (unless (or (server-running-p) axvr/windows?)
;;       (server-start))))

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

(global-prettify-symbols-mode)

(require 'project)

(use-package project
  :ensure nil
  :init
  (project-forget-zombie-projects)
  (project-remember-projects-under (expand-file-name "~/Projects"))
  (project-remember-projects-under (expand-file-name "~/Projects/Work"))
  ;; (project-remember-project (expand-file-name "~/Documents/Ledger"))
  ;; (project-remember-project (expand-file-name "~/Documents/Notes"))
  )

;; TODO: trim trailing whitespace on save.
(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default indicate-empty-lines t)

;; Highlight TODOs and more.
;; TODO: set colours.
(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :init (add-hook 'prog-mode-hook #'hl-prog-extra-mode))

(use-package transient)
(use-package magit
  :after (transient)
  :defer t
  :functions magit-status)

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

(use-package markdown-mode
  :mode ("TODO\\'" "DOING\\'" "DONE\\'"))

(use-package org
  :hook (org-mode . org-indent-mode))

;; TODO
(setq scheme-program-name "csi -:c")
(use-package sly :defer t :config (setq inferior-lisp-program "sbcl"))
(use-package clojure-mode) ; TODO: clojure-ts-mode?

(use-package execline)

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

;; (use-package erlang
;;   :vc (:url "https://github.com/erlang/otp" :lisp-dir "lib/tools/emacs" :make ""))

(require 'axvr-tools)

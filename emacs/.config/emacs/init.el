;;;; GNU Emacs configuration. -*- lexical-binding: t; -*-

;; TODO: move most of this file into the axvr directory.  `axvr-init.el' and symbolic link it.

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

(require 'axvr-packages)

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

(setq reb-re-syntax 'string)

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
  (setq project-mode-line t)
  (mapc (lambda (dir)
          (when (file-exists-p dir)
            (project-remember-projects-under dir)))
        '("~/Projects" "~/Projects/Work" "~/Documents")))

;; TODO: trim trailing whitespace on save.
(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default indicate-empty-lines t)

(which-key-mode)

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; (axvr/package-install 'paren-face)
;; (setq paren-face-regexp "[][(){}]")
;; (global-paren-face-mode) ; TODO: only on prog mode.
;; (add-hook 'prog-mode-hook 'paren-face-mode)
;; (set-face-foreground 'parenthesis "#828282")

;; (fido-mode)

(require 'axvr-tools)

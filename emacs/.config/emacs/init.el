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

(defvar av/use-evil-mode nil
  "When set to `t', Evil mode will be activated on start up.")

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
      initial-scratch-message "")

(setq-default frame-title-format '("%n %b - %F"))

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
      modus-themes-italic-constructs t
      modus-themes-mixed-fonts t)
;; (av/set-theme 'modus-operandi "light")

(use-package alabaster-themes
  :config (av/set-theme 'alabaster-themes-light-bg "light"))

;; (use-package ef-themes
;;   :init (ef-themes-take-over-modus-themes-mode 1)
;;   :config (modus-themes-load-theme 'ef-frost))

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
(use-package clojure-mode)

(if av/use-evil-mode
  (require 'av-evil)
  (progn
    (which-key-mode)
    (global-set-key (kbd "C-/") 'comment-or-uncomment-region)
    ;; (require 'av-cua)
    (when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings 'meta))))

(use-package undo-fu
  :config
  (unless av/use-evil-mode
    (global-unset-key (kbd "C-z"))
    (global-set-key (kbd "C-z")   'undo-fu-only-undo)
    (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)))

(use-package undo-fu-session
  :after undo-fu
  :config
  ;; TODO: add other Git files.
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; Prevent Emacs from appending "custom" stuff to this file.
;; (setq custom-file (make-temp-file "emacs-custom"))

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

(defun replica-1 ()
  (interactive)
  (serial-term (if (memq system-type '(darwin))
                 "/dev/tty.usbserial-AC00JRMK"
                 "/dev/ttyUSB")
               9600))

;; Control +/-/0 to zoom text.  (Do on non macOS only?)
;; (global-set-key (kbd "C-=") 'default-font-presets-scale-increase)
;; (global-set-key (kbd "C--") 'default-font-presets-scale-decrease)
;; (global-set-key (kbd "C-0") 'default-font-presets-scale-reset)

;; some completion stuff is installed by default now.  enable it?

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

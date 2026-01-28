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
(save-place-mode 1)
(savehist-mode 1)

(setq version-control t
      vc-follow-symlinks t)

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

;;; ----------------------------
;;; GUI.

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (context-menu-mode 1)
      (setq-default cursor-type 'bar)
      (global-set-key (kbd "<escape>") 'keyboard-escape-quit))
  (xterm-mouse-mode 1))

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
  :ensure t
  :config (av/set-theme 'alabaster-themes-light "light"))

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






;;;; Prevent Emacs from appending "custom" stuff to this file.
;;(setq custom-file (make-temp-file "emacs-custom"))

;;;;; ------------------------------------------------------------
;;;;; Packages

;;(defun av/package-install (&rest packages)
;;  (dolist (package packages)
;;    (unless (package-installed-p package)
;;      (package-install package))))

;;;;; ------------------------------------------------------------
;;;;; Essentials

;;;; (when (fboundp 'windmove-default-keybindings)
;;;;   (windmove-default-keybindings 'meta))

;;;; (require 'av-cua)

;;;;; ------------------------------------------------------------
;;;;; Scrolling

;;;; FIXME: this is slow!
;;(mouse-wheel-mode 1)
;;(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
;;      mouse-wheel-progressive-speed nil
;;      mouse-wheel-follow-mouse 't
;;      ;; Horizontal scroll on trackpad
;;      mouse-wheel-tilt-scroll 't
;;      mouse-wheel-flip-direction 't
;;      ;; Keyboard scrolling
;;      scroll-step 1
;;      scroll-conservatively 10000)
;;;; Enable horizontal scroll (`C-PgUp' + `C-PgDn')
;;(put 'scroll-left 'disabled nil)

;;;;; ------------------------------------------------------------
;;;;; Programming

;;;; Indentation.
;;(setq-default indent-tabs-mode nil
;;              tab-width 4)

;;(setq require-final-newline 'ask)
;;(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
;;(define-key global-map (kbd "RET") 'newline-and-indent)

;;(column-number-mode 1)
;;(electric-pair-mode 1)
;;(electric-indent-mode 1)
;;(delete-selection-mode 1)

;;;; (define-minor-mode av/hl-todos-mode
;;;;   "Highlight TODOs and other common comment keywords"
;;;;   nil
;;;;   :lighter ""
;;;;   (font-lock-add-keywords
;;;;    nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
;;;;           1 '((:foreground "#d75f5f") (:weight bold)) t))))
;;;; (add-hook 'prog-mode-hook 'av/hl-todos-mode)

;;(av/package-install 'paren-face)
;;(setq paren-face-regexp "[][(){}]")
;;(global-paren-face-mode 1) ; TODO: only on prog mode.
;;(add-hook 'prog-mode-hook 'paren-face-mode)
;;(set-face-foreground 'parenthesis "#828282")

;;;; (fido-mode 1)

;;(av/package-install 'expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)

;;;; (av/package-install 'company)
;;;; (global-company-mode 1)
;;;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
;;;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;;;; (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;;;; (define-key company-active-map (kbd "M-.")  'company-show-location)

;;(setq inferior-lisp-program "sbcl")
;;(av/package-install 'sly)

;;(setq scheme-program-name "csi -:c")

;;(av/package-install 'clojure-mode)

;;;;; ------------------------------------------------------------
;;;;; Tools

;;(av/package-install 'org 'markdown-mode)
;;(add-hook 'org-mode-hook 'org-indent-mode)

;;(av/package-install 'restclient)
;;(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;;(av/package-install 'ledger-mode)

;;;; (av/package-install 'circe)

;;(av/package-install 'magit)

;;(use-package evil
;;  :ensure t
;;  :init (setq evil-want-keybinding nil)
;;  :config
;;  (evil-mode 1)
;;  (evil-set-undo-system 'undo-fu))

;;(use-package evil-collection
;;  :after evil
;;  :ensure t
;;  :config
;;  (evil-collection-init))

;;(use-package undo-fu :ensure t)

;;(use-package undo-fu-session
;;  :ensure t
;;  :after undo-fu
;;  :config
;;  ;; TODO: add other Git files.
;;  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
;;  (undo-fu-session-global-mode))

;;;;(av/package-install 'evil 'evil-collection)
;;;;(setq evil-want-keybinding nil)
;;;;(require 'evil)
;;;;(evil-mode 1)
;;;;(evil-set-undo-system 'undo-fu)
;;;;(evil-collection-init)

(require 'project)

;;;; (global-visual-line-mode t)
;;(setq-default word-wrap t)

;; serial-term for Replica 1

;; Show empty lines.
(setq-default indicate-empty-lines t)

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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

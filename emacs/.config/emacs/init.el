;;;; GNU Emacs Configuration

;; Add custom elisp files to `load-path'.
(let ((default-directory (concat user-emacs-directory "elisp")))
  (when (file-directory-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; Prevent Emacs from appending "custom" stuff to this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; ------------------------------------------------------------
;;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu" . 10) ("melpa" . 5)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun av/package-install (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

;;; ------------------------------------------------------------
;;; Essentials

(setq user-mail-address "alex@vear.uk"
      user-full-name "Alex Vear")

;; Use UTF-8 character encoding.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; Update buffers and directory listing when files change on disk.
(global-auto-revert-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; File backups and remember last cursor position.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      version-control t
      save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p
      vc-follow-symlinks t)

;; (when (fboundp 'windmove-default-keybindings)
;;   (windmove-default-keybindings 'meta))

;; (require 'av-cua)

;;; ------------------------------------------------------------
;;; UI config

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (setq default-frame-alist '((height . 46) (width . 96)))
      (setq-default cursor-type 'bar)
      (global-set-key (kbd "<escape>") 'keyboard-escape-quit))
  (xterm-mouse-mode 1))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      frame-title-format "GNU Emacs")

(context-menu-mode 1)

(defun current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(defun av/executable-find (program)
  (not (null (executable-find program))))

(defun av/set-gtk-theme (variant)
  "Set the GTK theme variant for the current Emacs session."
  (when (and (display-graphic-p)
             (memq system-type '(gnu/linux))
             (av/executable-find "xprop"))
    (call-process-shell-command
     (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"" variant "\" -name \"" (current-frame-name) "\""))))

(defun av/set-theme (theme &optional mode)
  (av/set-gtk-theme (if mode mode "dark"))
  (load-theme theme t))

(av/package-install 'alabaster-themes)
(av/set-theme 'alabaster-themes-light "light")

;;; ------------------------------------------------------------
;;; Default fonts

(defun av/flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (av/flatten (car mylist))
            (av/flatten (cdr mylist))))))

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
                   ((:family . "Consolas")      (:height . 110)))))
  (av/set-font 'default monospace)
  (av/set-font 'fixed-pitch monospace))

(av/set-font 'variable-pitch
             '(((:family . "Cantarell")   (:height . 120))
               ((:family . "DejaVu Sans") (:height . 110))))

;;; ------------------------------------------------------------
;;; Scrolling

(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      ;; Horizontal scroll on trackpad
      mouse-wheel-tilt-scroll 't
      mouse-wheel-flip-direction 't
      ;; Keyboard scrolling
      scroll-step 1
      scroll-conservatively 10000)
;; Enable horizontal scroll (`C-PgUp' + `C-PgDn')
(put 'scroll-left 'disabled nil)

;;; ------------------------------------------------------------
;;; Programming

;; Indentation.
(setq-default indent-tabs-mode nil
              tab-width 4)

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(column-number-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)

;; (define-minor-mode av/hl-todos-mode
;;   "Highlight TODOs and other common comment keywords"
;;   nil
;;   :lighter ""
;;   (font-lock-add-keywords
;;    nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
;;           1 '((:foreground "#d75f5f") (:weight bold)) t))))
;; (add-hook 'prog-mode-hook 'av/hl-todos-mode)

(av/package-install 'paren-face)
(setq paren-face-regexp "[][(){}]")
(global-paren-face-mode 1) ; TODO: only on prog mode.
(add-hook 'prog-mode-hook 'paren-face-mode)
(set-face-foreground 'parenthesis "#828282")

;; (fido-mode 1)

(av/package-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; (av/package-install 'company)
;; (global-company-mode 1)
;; (define-key company-active-map (kbd "\C-n") 'company-select-next)
;; (define-key company-active-map (kbd "\C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
;; (define-key company-active-map (kbd "M-.")  'company-show-location)

(setq inferior-lisp-program "sbcl")
(av/package-install 'sly)

(setq scheme-program-name "csi -:c")

(av/package-install 'clojure-mode)

;;; ------------------------------------------------------------
;;; Tools

(av/package-install 'undo-fu 'undo-fu-session)
;; TODO: add other Git files.
(setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
(setq evil-undo-system 'undo-fu)

(av/package-install 'org 'markdown-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

(av/package-install 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(av/package-install 'ledger-mode)

;; (av/package-install 'circe)

(av/package-install 'magit)

(av/package-install 'evil 'evil-collection)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode 1)
(evil-set-undo-system 'undo-fu)
(evil-collection-init)

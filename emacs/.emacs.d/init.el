;;;; GNU Emacs Configuration
;;;; `~/.emacs.d/init.el'


;; Add custom elisp files to `load-path'.
(let ((default-directory (concat user-emacs-directory "elisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Prevent Emacs from appending "custom" stuff to this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)


;;; ------------------------------------------------------------
;;; Packages

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (protocol (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat protocol "melpa.org/packages/")) t))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 5)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun av/package-install (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))


;;; ------------------------------------------------------------
;;; Essentials

(setq user-mail-address "av@axvr.io"
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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(defalias 'yes-or-no-p 'y-or-n-p)

(when (display-graphic-p)
  (setq confirm-kill-emacs 'yes-or-no-p)
  ;; Remap escape key to "quit".
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(setq vc-follow-symlinks t)


;;; ------------------------------------------------------------
;;; UI config

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (setq default-frame-alist '((height . 46) (width . 96)))
      (setq-default cursor-type 'bar))
  (xterm-mouse-mode 1))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      frame-title-format "GNU Emacs")

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

(av/package-install 'doom-themes)

(if (and (av/executable-find "theme-variant")
         (string= (shell-command-to-string "theme-variant emacs") "light"))
    (progn
      (av/set-theme 'doom-opera-light "light")
      (set-face-foreground 'font-lock-comment-face "#949494")
      (set-face-attribute 'region nil :background "#e4e4e4"))
  (progn
    (av/set-theme 'doom-opera "dark")
    (set-face-background 'default "#262626")
    (set-face-foreground 'default "#C6C6C6")
    (set-face-background 'mode-line "#323334")
    (setq face-near-same-color-threshold 20000)))


;;; ------------------------------------------------------------
;;; Default fonts

(defun flatten (mylist)
  (cond
   ((null mylist) nil)
   ((atom mylist) (list mylist))
   (t
    (append (flatten (car mylist))
            (flatten (cdr mylist))))))

(defun av/top-installed-font (fonts)
    (seq-find (lambda (x)
                (member (cdr (assoc :family x))
                        (font-family-list)))
              fonts))

(defun av/set-font (type fonts)
  (let ((font-attrs (av/top-installed-font fonts)))
    (when font-attrs
        (apply 'set-face-attribute type nil (flatten font-attrs)))))

(let ((monospace '(((:family . "Inconsolata") (:height . 135))
                   ((:family . "Consolas")    (:height . 110)))))
  (av/set-font 'default monospace)
  (av/set-font 'fixed-pitch monospace))

(av/set-font 'variable-pitch
             '(((:family . "Cantarell")   (:height . 120))
               ((:family . "DejaVu Sans") (:height . 110))))


;;; ------------------------------------------------------------
;;; Scrolling

(mouse-wheel-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
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
;; TODO: configure indentation for each major mode.
;; TODO: behaviour similar to `textwidth' in Vim: `auto-fill-mode' and `fill-column'.

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)

;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; TODO: (defface todo ...)
(define-minor-mode av/hl-todos-mode
  "Highlight TODOs and other common comment keywords"
  nil
  :lighter ""
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
          1 '((:foreground "#d75f5f") (:weight bold)) t))))
;;(add-hook 'prog-mode-hook 'av/hl-todos-mode)

(av/package-install 'paren-face)
(setq paren-face-regexp "[][(){}]")
(global-paren-face-mode 1)
(set-face-foreground 'parenthesis "#767676")

(av/package-install 'undo-propose)
(global-set-key (kbd "C-c C-_") 'undo-propose)

(av/package-install 'popup-edit-menu)
(require 'popup-edit-menu)
(global-set-key [mouse-3] (popup-edit-menu-stub))

(fido-mode 1)

(av/package-install 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(av/package-install 'clojure-mode 'markdown-mode 'markless)
(av/package-install 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
(av/package-install 'ledger-mode)  ; FIXME: `ledger-mode-clean-buffer' sort in reverse.

(av/package-install 'org)
(add-hook 'org-mode-hook 'org-indent-mode)

(av/package-install 'company)
(global-company-mode 1)
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.")  'company-show-location)

(setq inferior-lisp-program "sbcl")
(av/package-install 'sly)

(setq scheme-program-name "csi -:c")

;; TODO: paredit, inf-clojure, magit

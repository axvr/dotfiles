;;;; GNU Emacs Configuration
;;;; `~/.emacs.d/init.el'


;; Add custom elisp files to `load-path'.
(let ((default-directory (concat user-emacs-directory "elisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


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


;;; UI config

(menu-bar-mode -1)
(if (display-graphic-p)
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1)
           (setq-default cursor-type 'bar))
  (xterm-mouse-mode 1))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      frame-title-format "GNU Emacs")

(defun current-frame-name ()
  "Return the name of the current GUI frame."
  (substring-no-properties
   (cdr (assoc 'name (frame-parameters)))))

(defun set-gtk-theme (variant)
  "Set the GTK theme variant for the current Emacs session."
  (interactive "sLight or dark? ")
  (call-process-shell-command
   (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"" variant "\" -name \"" (current-frame-name) "\"")))

(when (window-system)
  (set-gtk-theme "dark"))

(load-theme 'photon t)


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
      (if (eq type 'monospace)
          (progn
            (av/set-font 'default (list font-attrs))
            (av/set-font 'fixed-pitch (list font-attrs)))
        (apply 'set-face-attribute type nil (flatten font-attrs))))))

(av/set-font 'monospace
             '(((:family . "Roboto Mono") (:height . 110))
               ((:family . "Inconsolata") (:height . 135))
               ((:family . "Consolas")    (:height . 110))))

(av/set-font 'variable-pitch
             '(((:family . "Cantarell")   (:height . 120))
               ((:family . "DejaVu Sans") (:height . 110))))


;;; Scrolling

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      ;; Keyboard scrolling
      scroll-step 1
      scroll-conservatively 10000)
;; Enable horizontal scroll (`C-PgUp' + `C-PgDn')
(put 'scroll-left 'disabled nil)


;;; Programming

;; Indentation.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; TODO: configure indentation for each major mode.
;; TODO: behaviour similar to `textwidth' in Vim: `auto-fill-mode' and `fill-column'.

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; (define-minor-mode av/hl-todos-mode
;;   "Highlight TODOs and other common comment keywords"
;;   nil
;;   :lighter ""
;;   (font-lock-add-keywords
;;    nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
;;           1 '((:foreground "#d75f5f") (:weight bold)) t))))
;; (add-hook 'prog-mode-hook 'av/hl-todos-mode)

(setq scheme-program-name "csi -:c")


;;; Packages

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 5))
      package-enable-at-startup nil)

(when (< emacs-major-version 27)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package undo-propose
  :config (global-set-key (kbd "C-c u") 'undo-propose))

(use-package clojure-mode :defer t)
(use-package markdown-mode :defer t)
(use-package restclient :mode ("\\.http\\'" . restclient-mode))
(use-package ledger-mode :defer t)  ; FIXME: make `ledger-mode-clean-buffer' sort in reverse order.

(use-package org
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  ;; TODO set org directory for org-agenda
  (require 'org-man))

;; TODO: sly/slime, paredit, inf-clojure, gerbil, gambit, magit

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ledger-mode restclient markdown-mode clojure-mode undo-propose use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

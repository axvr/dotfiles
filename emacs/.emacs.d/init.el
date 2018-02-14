;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'


;;; TODO Install Org-mode, Magit, Ledger-mode and others (maybe restclient)
;;; TODO Set up for programming (e.g. C#, VimL, Perl, Python, C, C++, Clojure, etc.)
;;; TODO Sort out package archive priority (https://emacs.stackexchange.com/questions/2969/is-it-possible-to-use-both-melpa-and-melpa-stable-at-the-same-time)
;;; TODO Move this file to an org-mode file
;;; TODO Tidy up file & shorten comments
;;; TODO Set fonts (Consolas on Windows, maybe Source-Code-Pro or Monospace on Linux)
;;; TODO Set location to store backup files
;;; TODO Try and set up unlimited undo
;;; TODO Set up Vim-like folding (https://github.com/mrkkrp/vimish-fold, https://github.com/alexmurray/evil-vimish-fold)


;; Disable UI elements in GUI Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable various minor modes
(show-paren-mode 1)                           ; Highlight matching parens
(column-number-mode 1)                        ; Display column number
(setq column-number-indicator-zero-based nil) ; Start column number count at 1 (Emacs 26)
(hl-line-mode)                                ; Hightlight current line
(global-linum-mode 1)                         ; Enable line numbers
(setq display-line-numbers 'relative)         ; Relative line numbers (Emacs 26)
;; NOTE: May need: (setq display-line-numbers-current-absolute t) to enable hybrid line numbers

;; Auto-indent on new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Set `ESC' to `C-g' (no more `ESC-ESC-ESC')
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Allow using `y' or `n' instead of `yes' & `no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set UTF-8 encoding
(set-language-environment "UTF-8")

;; Highlight TODOs, NOTEs, FIXMEs etc. TODO: Hightlight only in comments
;; Space-vim-dark: #d78700 | Old: #d7a3ad
(defun axvr/highlight-todos () ; Source (modified): https://writequit.org/org/
  "Highlight FIXME, TODO and NOTE"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME:?\\|TODO:?\\|NOTE:?\\)\\>"
          1 '((:foreground "#d78700") (:weight bold)) t))))
(add-hook 'prog-mode-hook #'axvr/highlight-todos)

;; Vim-like tilde on empty lines.
;; Source: http://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)

;; Better scrolling
;; TODO Test and research: https://www.reddit.com/r/vim/comments/16w481/what_is_your_vimrc_and_why_do_you_like_it/c7zxdkl/
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-step 1)

;; Enable horizontal scrolling using C-PgUp and C-PgDn
(put 'scroll-left 'disabled nil)


;;; Packages Config
(require 'package)

;;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))
(eval-when-compile
  (require 'use-package))
(require 'diminish)


(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil) ; http://stackoverflow.com/questions/22878668/emacs-org-mode-evil-mode-tab-key-not-working
  :config

  (use-package evil-leader
    :ensure t
    :config
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "SPC" 'execute-extended-command
      "ff"  'find-file
      "fs"  'save-buffer
      "qq"  'save-buffers-kill-terminal)
    (global-evil-leader-mode))

  (use-package evil-escape
    :ensure t
    :diminish evil-escape-mode
    :init (setq evil-escape-key-sequence "jk")
    :config (evil-escape-mode))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

  (evil-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package org :ensure t)

(use-package magit :defer t)

; TODO enable only for Lisps
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diminish evil-numbers ivy use-package spacemacs-theme rainbow-delimiters evil-leader evil-escape))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

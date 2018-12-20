;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'

(menu-bar-mode -1)
(if (display-graphic-p)
    (progn (tool-bar-mode -1)
           (scroll-bar-mode -1))
  (xterm-mouse-mode 1))

(show-paren-mode 1)

(column-number-mode 1)
(setq column-number-indicator-zero-based nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

(setq ring-bell-function 'ignore)

(setq user-mail-address "av@axvr.io"
      user-full-name "Alex Vear")

;; TODO Indentation settings
(setq-default indent-tabs-mode nil)

(setq require-final-newline 'ask)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq scroll-step 1
      scroll-conservatively 10000)
(put 'scroll-left 'disabled nil) ; Horizontal scrolling (`C-PgUp' & `C-PgDn')

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
;; (add-hook 'prog-mode-hook #'electric-pair-mode)

(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight TODOs, NOTEs, FIXMEs etc.
(define-minor-mode highlight-todos-mode
  "Highlight TODO and other comment keywords"
  nil
  :lighter ""
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
          1 '((:foreground "#d78700") (:weight bold)) t))))
(add-hook 'prog-mode-hook 'highlight-todos-mode)

(when (memq system-type '(cygwin windows-nt ms-dos))
  (set-face-attribute 'default nil :family "Consolas" :height 110))

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      version-control t
      undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
      undo-tree-auto-save-history t
      save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)


;;; Packages
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "stable.melpa.org/packages/")) t))

(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa" . 5)
        ("melpa-stable" . 0)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)


;;; Evil-mode Configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config

  (evil-define-command av/evil-retab (start end)
    "Emacs implementation of the `:retab' ex command in Vim"
    (interactive "<r>")
    (if indent-tabs-mode
        (tabify start end)
      (untabify start end)))

  (evil-ex-define-cmd "ret[ab]"    'av/evil-retab)
  (evil-ex-define-cmd "ter[minal]" 'ansi-term)

  (use-package evil-collection
    :ensure t
    :init (evil-collection-init))

  (evil-define-operator av/evil-commentary (beg end)
    "Emacs implementation of `vim-commentary'"
    :move-point nil
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  (evil-define-key 'normal 'global "gc" 'av/evil-commentary)

  (use-package evil-lion
    :ensure t
    :config (evil-lion-mode))

  (evil-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode 1))

(use-package general
  :ensure t
  :after which-key
  :config
  (general-evil-setup t)

  (general-create-definer leader
    :prefix "SPC"
    :states '(normal visual))

  (general-create-definer local-leader
    :prefix "SPC m"
    :states '(normal visual))

  (leader "m" '(:ignore t :which-key "major-mode"))

  (general-define-key
    :prefix "SPC SPC"
    :states '(normal visual)
    "" 'execute-extended-command)

  ;; File
  (leader
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "ft" 'dired
    "fc" '((lambda () (interactive) (find-file (concat user-emacs-directory "init.el"))) :which-key "edit-config"))

  ;; Emacs Lisp
  (local-leader
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "e" '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "ee" 'eval-expression
    "es" 'eval-last-sexp
    "ep" 'eval-print-last-sexp
    "er" 'eval-region))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package markdown-mode :ensure t :defer t)

(use-package org
  :ensure t
  :defer t
  :hook (org-mode . org-indent-mode)
  :config
  ;; TODO set org directory for org-agenda

  (local-leader
    :keymaps 'org-mode-map
    "i" '(:ignore t :which-key "insert")
    "il" 'org-insert-link
    "o" 'org-open-at-point
    "a" 'org-agenda
    "c" 'org-capture
    "g" 'org-store-link
    "e" 'org-export-dispatch
    "v" 'org-eval
    "s" 'org-edit-src-code
    "t" 'org-todo))

(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode)
  :config

  (local-leader
    :keymaps 'restclient-mode-map
    "s" '(:ignore t :which-key "send")
    "sc" 'restclient-http-send-current
    "sr" 'restclient-http-send-current-raw
    "ss" 'restclient-http-send-current-stay-in-window
    "j" 'restclient-jump-prev
    "k" 'restclient-jump-next))

(use-package magit
  :ensure t
  :defer t
  :config

  (leader ; FIXME: doesn't work with `:defer'
    "g"  '(:ignore t :which-key "git/vcs")
    "gs" 'magit-status
    "gd" 'magit-diff
    "gb" 'magit-blame))

(use-package ledger-mode
  :ensure t
  :defer t
  :config
  ;; FIXME `ledger-mode-clean-buffer' should sort in reverse order

  (local-leader
    :keymaps 'ledger-mode-map
    "c" 'ledger-mode-clean-buffer
    "k" 'ledger-check-buffer
    "b" '(:ignore t :which-key "balance")
    "bb" 'ledger-display-balance
    "bp" 'ledger-display-balance-at-point
    "r" '(:ignore t :which-key "register")))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
    (general which-key magit restclient spacemacs-theme rainbow-delimiters ledger-mode ivy markdown-mode evil-lion evil-collection evil diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

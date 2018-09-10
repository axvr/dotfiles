;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'

;;; Initially this config will be primarily used for Lisp development.

;;; TODO Install and configure: magit, restclient, ivy, projectile, a paren package
;;; TODO Set fonts (Consolas on Windows, maybe Source-Code-Pro, Tamsyn or DejaVu Sans Mono on Linux)
;;; TODO Make this config work exactly the same in terminal as it does in GUI Emacs
;;; TODO Set up ctags and/or etags
;;; TODO Tab and indentation
;;; TODO Clean this file and remove unused packages and settings
;;; FIXME Esc in minibuffer needs to be pressed twice


(setq user-mail-address "av@axvr.io")

;; Disable UI elements in GUI Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable & configure various minor modes
(dolist (mode '(show-paren-mode     ; Highlight matching parens
		column-number-mode))  ; Display column number
		;; global-hl-line-mode)) ; Hightlight current line
		;; global-display-line-numbers-mode)) ; Enable line numbers
  (funcall mode 1))

(setq column-number-indicator-zero-based nil
      ring-bell-function 'ignore
      require-final-newline t
      show-trailing-whitespace t)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; Auto-indent on new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Allow using `y' or `n' instead of `yes' & `no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set UTF-8 encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Highlight TODOs, NOTEs, FIXMEs etc.
;; TODO: Allow separation characters in some of the keywords (e.g. `[-_ ]')
(define-minor-mode highlight-todos-mode
  "Highlight TODO and other comment keywords"
  nil
  :lighter " TODOs"
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIXME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\):?\\>"
	  1 '((:foreground "#d78700") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'highlight-todos-mode)

;; Vi-like tilde on empty lines.
;; Source: http://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/
(define-minor-mode vi-tilde-mode
  "Add vi-like tilde on empty lines"
  nil
  :lighter ""
  (setq-default indicate-empty-lines t)
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
  (set-fringe-bitmap-face 'tilde 'font-lock-function-name-face))

(vi-tilde-mode 1)

;; Better scrolling
;; TODO Test and improve this
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-step 1)

;; Enable horizontal scrolling using C-PgUp and C-PgDn
(put 'scroll-left 'disabled nil)


;; Improve the backup, undo and cursor placement defaults
(require 'saveplace) ; FIXME, this is not working
(setq-default save-place t)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      undo-tree-auto-save-history 1
      save-place-file (concat user-emacs-directory "places"))

(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))



;;; Packages Config
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "elpa.gnu.org/packages/")))))

(setq package-archive-priorities
      '(("gnu" . 10)
	("melpa-stable" . 5)
	("melpa" . 0)))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)



;; Evil-Mode Configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-integration nil
	evil-want-C-i-jump nil) ; https://stackoverflow.com/q/22878668
  :config

  (define-key evil-insert-state-map (kbd "C-U") 'insert-char)

  (evil-define-command vi-find-file (file)
    (interactive "<a>")
    (find-file (car (file-expand-wildcards file))))

  (evil-ex-define-cmd "fin[d]"     'vi-find-file)
  (evil-ex-define-cmd "ter[minal]" 'ansi-term)
  (evil-ex-define-cmd "pack[age]"  'package-list-packages)

  (use-package evil-collection
    :ensure t
    :custom (evil-collection-setup-minibuffer t)
    :init (evil-collection-init))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key
      "SPC" 'execute-extended-command
      "ff"  'find-file
      "ut"  'undo-tree-visualize))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

  ;; TODO: Is folding really needed?
  (use-package vimish-fold
    :ensure t
    :config

    (use-package evil-vimish-fold
      :ensure t
      :config (evil-vimish-fold-mode)))

  (use-package evil-commentary
    :ensure t
    :config (evil-commentary-mode))

  (use-package evil-lion
    :ensure t
    :config (evil-lion-mode))

  (use-package evil-goggles
    :ensure t
    :config (evil-goggles-mode))

  (evil-mode 1))



(use-package markdown-mode :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode 1))

(use-package org :ensure t)

(use-package restclient :ensure t)

(use-package magit
  :defer t
  :config

  (use-package evil-magit
    :ensure t
    :config
    (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; FIXME `ledger-mode-clean-buffer' should sort in reverse order
;; TODO automatically run `ledger-mode-clean-buffer' on save
(use-package ledger-mode :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; TODO improve C# set up
;; (use-package omnisharp
;;   :hook (csharp-mode . omnisharp-mode))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-collection-setup-minibuffer t)
 '(package-selected-packages
   (quote
    (company evil-vimish-fold vimish-fold restclient spacemacs-theme rainbow-delimiters ledger-mode ivy markdown-mode evil-goggles evil-lion evil-commentary evil-numbers evil-leader evil-collection evil diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

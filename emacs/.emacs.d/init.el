;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'

;;; TODO Set up ctags and/or etags
;;; TODO Configure Projectile

(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1))
  (xterm-mouse-mode 1))

(when (< emacs-major-version 25)
  (require 'saveplace)
  (setq-default save-place t))

;; Enable & configure various minor modes
(dolist (mode '(show-paren-mode    ; Highlight matching parens
                column-number-mode ; Display column number
                save-place-mode    ; Save position in file
                global-visual-line-mode)) ; Always soft-wrap lines
  (funcall mode 1))

(setq user-mail-address "av@axvr.io"
      user-full-name "Alex Vear"
      column-number-indicator-zero-based nil
      ring-bell-function 'ignore
      require-final-newline t
      show-trailing-whitespace t)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; TODO: improve default indentation settings
(setq-default indent-tabs-mode nil)
;; (setq tab-width 4)

;; Auto-indent on new line
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Allow using `y' or `n' instead of `yes' & `no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set UTF-8 encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Highlight TODOs, NOTEs, FIXMEs etc.
(define-minor-mode highlight-todos-mode
  "Highlight TODO and other comment keywords"
  nil
  :lighter " TODOs"
  (font-lock-add-keywords
   nil '(("\\<\\(TO[-_ ]?DO\\|FIX[-_ ]?ME\\|NOTE\\|XXX\\|BUG\\|HACK\\|UNDONE\\)\\>"
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

(when (display-graphic-p)
  (vi-tilde-mode 1))

;; Better scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-step 1)

;; Enable horizontal scrolling using C-PgUp and C-PgDn
(put 'scroll-left 'disabled nil)

;; Improve the backup, undo and cursor placement defaults
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo")))
      undo-tree-auto-save-history t
      save-place-file (concat user-emacs-directory "places"))

(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Set default fonts
(when (memq system-type '(cygwin windows-nt ms-dos))
  (set-face-attribute 'default nil :family "Consolas" :height 110))


;;; Packages Config
(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http://" "https://")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "elpa.gnu.org/packages/")))))

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


;; Evil-mode Configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-want-C-i-jump nil) ; https://stackoverflow.com/q/22878668
  :config

  (define-key evil-insert-state-map (kbd "C-U") 'insert-char)

  ;; TODO allow picking between found files
  (evil-define-command av/find-file (file)
    (interactive "<a>")
    (find-file (car (file-expand-wildcards file))))

  (evil-define-command av/retab (start end)
    (interactive "<r>")
    (if indent-tabs-mode
        (progn (tabify start end))
      (untabify start end)))

  ;; TODO Improve this
  (defun av/restclient-temp-open ()
    "Create a new REST Client window"
    (interactive)
    ;; (split-window-below)
    (switch-to-buffer "*REST Client*")
    (restclient-mode))

  (defun av/undo-tree-visualizer-toggle ()
    "Toggle the Undo-Tree"
    (interactive)
    (if (get-buffer undo-tree-visualizer-buffer-name)
        (undo-tree-visualizer-quit)
      (setq undo-tree-visualizer-diff t)
      (undo-tree-visualize)))

  (evil-ex-define-cmd "fin[d]"     'av/find-file)
  (evil-ex-define-cmd "ret[ab]"    'av/retab)
  (evil-ex-define-cmd "ter[minal]" 'ansi-term)
  (evil-ex-define-cmd "pack[age]"  'package-list-packages)

  (use-package evil-collection
    :ensure t
    :init (evil-collection-init))

  (use-package evil-numbers
    :ensure t
    :config
    (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

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


(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode 1)
  (define-key evil-normal-state-map (kbd "SPC g") '("git-prefix")))

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
   :states '(normal visual)
   :prefix "SPC SPC"
   "" 'execute-extended-command)

  ;; File
  (leader
    "f" '(:ignore t :which-key "file")
    "ff" 'find-file
    "ft" 'dired
    "fc" '(:ignore t :which-key "config")
    "fce" '((lambda () "" (interactive) (find-file (concat user-emacs-directory "init.el"))) :which-key "edit-config")
    "fcl" '((lambda () "" (interactive) (load-file (concat user-emacs-directory "init.el"))) :which-key "load-config"))

  ;; Tools
  (leader
    "t" '(:ignore t :which-key "tools")
    "tu" 'av/undo-tree-visualizer-toggle ; FIXME leader not available in undo-tree-mode
    "tr" 'av/restclient-temp-open)

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

(use-package markdown-mode
  :ensure t
  :defer t)

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
  :config

  (use-package evil-magit
    :ensure t
    :config
    (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward))

  (leader
    "g"  '(:ignore t :which-key "git/vcs")
    "gs" 'magit-status
    "gd" 'magit-diff
    "gb" 'magit-blame))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay nil
        company-selection-wrap-around t)

  ;; Add Vim insert mode completion to Emacs
  ;; (I find auto-complete to be annoying and distracting)
  ;; TODO add more insert mode completion options
  ;; FIXME when opening completion list auto-select the first item
  ;; FIXME `C-e' and `C-y' mappings
  (general-define-key
   :states 'insert
   :prefix "C-x"
   "C-o" 'company-complete-common-or-cycle
   "C-f" 'company-files))
  ;; (general-define-key
  ;;  :states 'insert
  ;;  :keymaps 'company-mode-map
  ;;  "C-e" 'undo
  ;;  "C-y" 'nil))

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
    (general which-key evil-surround evil-magit magit company restclient spacemacs-theme rainbow-delimiters ledger-mode ivy markdown-mode evil-goggles evil-lion evil-commentary evil-numbers evil-collection evil diminish use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

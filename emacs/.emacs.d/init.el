;;;; Emacs Configuration
;;;; `~/.emacs.d/init.el'


;;; Disable UI elements in GUI Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Enable line numbers
(global-linum-mode 1)
(setq display-line-numbers 'relative)


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
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(use-package evil
  :ensure t
  :config (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config (progn
	      (setq evil-leader/in-all-states t)
	      (evil-leader/set-leader "<SPC>")
	      (global-evil-leader-mode)))

  (use-package evil-escape
    :ensure t
    :init (setq evil-escape-key-sequence "jk")
    :config (evil-escape-mode)))

(use-package org
  :ensure t)

(use-package magit
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))


;;; Set `ESC' to `C-g' (no more `ESC-ESC-ESC')
;;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package spacemacs-theme rainbow-delimiters helm evil-leader evil-escape))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

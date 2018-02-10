

;; TODO improve line numbers (and set relative)
;; TODO highlight "TODO" and other parts of comments
;; TODO install Org-mode, Magit and others
;; TODO sort out package archives

;; Disable useless information in GUI Emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Line Numbers at right of buffer
(global-linum-mode 1)
;; Auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Emacs Package Configuration
(require 'package)

;(setq package-archives
;  '(("gnu"         . "https://elpa.gnu.org/packages/")
;    ("melpa"       . "http://melpa.milkbox.net/packages/")))
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                                        (not (gnutls-available-p))))
              (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
    (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; TODO simplify these
(when (not (package-installed-p 'evil))
    (package-refresh-contents)
      (package-install 'evil))

(when (not (package-installed-p 'spacemacs-theme))
    (package-refresh-contents)
      (package-install 'spacemacs-theme))


(evil-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages (quote (spacemacs-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(put 'scroll-left 'disabled nil)


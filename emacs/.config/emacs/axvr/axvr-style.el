;;;; Configure styling and GUI. -*- lexical-binding: t; -*-

(if (display-graphic-p)
    (progn
      (scroll-bar-mode)
      (tool-bar-mode -1)
      (unless axvr/macos? (menu-bar-mode -1))
      (context-menu-mode)
      (setq-default cursor-type 'bar))
  (progn
    (xterm-mouse-mode)
    (menu-bar-mode -1)))

(setq inhibit-startup-screen t)

(defun axvr/set-gtk-theme (variant)
  "Set the GTK theme variant for the current Emacs session."
  (when (and (display-graphic-p)
             axvr/linux?
             (not (null (executable-find "xprop"))))
    (call-process-shell-command
     (format "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"%s\" -name \"%s\""
             variant (axvr/current-frame-name)))))

(defun axvr/set-theme (theme &optional mode)
  (axvr/set-gtk-theme (or mode axvr/theme))
  (load-theme theme t))

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

(axvr/set-theme 'raider)

;; (use-package doric-themes
;;   :demand t
;;   :config
;;   (require-theme 'modus-themes)
;;   (setq modus-themes-bold-constructs t
;;         modus-themes-italic-constructs t
;;         modus-themes-mixed-fonts t)
;;   ;; (axvr/set-theme 'modus-operandi)
;;   (doric-themes-select 'doric-marble))

;; (use-package alabaster-themes
;;   :config (axvr/set-theme 'alabaster-themes-light))

;; (use-package spacemacs-theme
;;   :config (axvr/set-theme 'spacemacs-light))

(pixel-scroll-precision-mode)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      ;; Horizontal scroll on trackpad
      mouse-wheel-tilt-scroll 't
      mouse-wheel-flip-direction 't
      ;; Keyboard scrolling
      scroll-step 1
      scroll-conservatively 101)

;; Enable horizontal scroll (`C-PgUp' + `C-PgDn')
(put 'scroll-left 'disabled nil)

(use-package vertico
  :config
  (vertico-mode)
  (vertico-mouse-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(setopt mode-line-collapse-minor-modes t)

(defun axvr/first-installed-font (fonts)
  (seq-find (lambda (x)
              (member (cdr (assoc :family x))
                      (font-family-list)))
            fonts))

(defun axvr/set-font (type fonts)
  (let ((font-attrs (axvr/first-installed-font fonts)))
    (when font-attrs
      (apply 'set-face-attribute type nil (axvr/flatten font-attrs)))))

(let ((monospace '(((:family . "IBM Plex Mono") (:height . 135))
                   ((:family . "JuliaMono")     (:height . 125))
                   ((:family . "Inconsolata")   (:height . 135))
                   ((:family . "Consolas")      (:height . 110))))
      (variable  '(((:family . "Inter")         (:height . 135))
                   ((:family . "Cantarell")     (:height . 120))
                   ((:family . "DejaVu Sans")   (:height . 110)))))
  (axvr/set-font 'default monospace)
  (axvr/set-font 'fixed-pitch monospace)
  (axvr/set-font 'variable-pitch variable))

(provide 'axvr-style)

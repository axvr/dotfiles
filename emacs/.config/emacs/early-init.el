;;;; -*- lexical-binding: t; -*-

;; Having this in `early-init` stops the window opening off the edge of the screen.
(setq default-frame-alist '((height . 46) (width . 100)))

;; https://github.com/doomemacs/doomemacs/blob/fca8bd7f3fa697a91774c2ddedcd1a47cd7da01a/early-init.el#L70,L75
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Enable packages manually later.
(setq package-enable-at-startup nil)

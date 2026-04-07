;; -*- lexical-binding: t; -*-

(use-package ledger-mode
  :mode ("\\.journal\\'" "\\.ledger\\'" "\\.hledger\\'")
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-report-auto-width nil
        ledger-report-links-in-register nil
        ledger-report-native-highlighting-arguments '("--color=always")))

(use-package markdown-mode :mode ("TODO\\'" "DOING\\'" "DONE\\'"))

(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

;; TODO
(setq scheme-program-name "csi -:c")
(use-package sly :defer t :config (setq inferior-lisp-program "sbcl"))
(use-package clojure-ts-mode)
;; TODO: inf-clojure vs. cider.

(use-package execline)

(use-package erlang-ts :mode ("\\.erl\\'" . erlang-ts-mode))

(use-package typst-ts-mode
  :config
  ;; TODO: do not do this on start up.
  (typst-ts-mc-install-grammar))

(provide 'axvr-languages)

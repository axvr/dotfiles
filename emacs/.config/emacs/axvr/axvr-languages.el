;; -*- lexical-binding: t; -*-

(use-package ledger-mode
  :mode ("\\.journal\\'" "\\.ledger\\'" "\\.hledger\\'")
  :config
  (setq ledger-binary-path "hledger"
        ledger-mode-should-check-version nil
        ledger-report-auto-width nil
        ledger-report-links-in-register nil
        ledger-report-native-highlighting-arguments '("--color=always")))

(use-package markdown-mode
  :commands (markdown-mode)
  :mode
  ("TODO\\'" "DOING\\'" "DONE\\'" "\\.md\\'" "\\.mkd\\'" "\\.markdown\\'"))

(use-package org
  :hook (org-mode . org-indent-mode)
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE"))))

(setq scheme-program-name "csi -:c")

(use-package sly
  :commands (sly common-lisp-mode)
  :mode
  ("\\.sexp\\'" . common-lisp-mode)
  ("\\.lisp\\'" . common-lisp-mode)
  ("\\.asd\\'" . common-lisp-mode)
  :config (setq inferior-lisp-program "sbcl"))

(use-package clojure-ts-mode
  :mode
  ("\\.clj\\'" . clojure-ts-mode)
  ("\\.cljc\\'" . clojure-ts-mode)
  ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
  ("\\.edn\\'" . edn-mode)
  ("\\.bb\\'" . clojure-ts-mode))
;; TODO: inf-clojure vs. cider.

;; TODO: enable based on shebang.
(use-package execline :defer t)

(use-package erlang-ts
  :mode ("\\.erl\\'" . erlang-ts-mode))

(use-package typst-ts-mode
  :mode ("\\.typ\\'" . typst-ts-mode)
  :config (typst-ts-mc-install-grammar))

(use-package csv-mode
  :commands (csv-mode)
  :mode ("\\.csv\\'" . csv-mode))

(provide 'axvr-languages)

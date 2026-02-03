;;;; Evil mode configuration.  -*- lexical-binding: t; -*-

(provide 'av-evil)

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu
        evil-want-C-u-scroll t
        evil-search-module 'evil-search
        evil-ex-search-case 'sensitive
        evil-search-wrap t
        evil-want-keybinding nil
        evil-shift-round nil
        evil-shift-width 4
        evil-indent-convert-tabs nil)
  :config
  (evil-mode)

  (evil-set-leader nil (kbd "<SPC>"))
  (evil-set-leader nil "\\" t)

  (evil-define-operator av/evil-commentary (beg end)
    "Emacs implementation of `tpope/vim-commentary'"
    :move-point nil
    (interactive "<r>")
    (comment-or-uncomment-region beg end))

  (evil-define-key 'normal 'global "gc" 'av/evil-commentary)

  (evil-define-key 'normal 'global "-" 'dired)

  (evil-define-command av/evil-retab (start end)
    "Emacs implementation of the `:retab' ex command in Vim"
    (interactive "<r>")
    (if indent-tabs-mode
      (tabify start end)
      (untabify start end)))

  (evil-ex-define-cmd "ret[ab]"    'av/evil-retab)
  (evil-ex-define-cmd "ter[minal]" 'ansi-term)
  (evil-ex-define-cmd "pa[ckadd]"  'package-list-packages))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

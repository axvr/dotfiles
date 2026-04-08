;;;; -*- lexical-binding: t; -*-

(defun axvr/replica-1 ()
  "Start a serial term that connects to my Replica 1 Plus."
  (interactive)
  ;; TODO: create an axvr/upper-case-mode (minor mode) and enable it in this buffer.
  (serial-term (if axvr/macos?
                   "/dev/tty.usbserial-AC00JRMK"
                 "/dev/ttyUSB1")
               9600))

(defun axvr/do ()
  "Execute a `do' script."
  (interactive)
  (if-let* ((default-directory (locate-dominating-file "." "do/"))
            (script (expand-file-name
                     (read-file-name "Run do script: "
                                     (expand-file-name "do/")
                                     nil t nil
                                     #'f-executable?))))
      (if (f-executable? script)
          (async-shell-command script)
        (message "No executable selected."))
    (message "Not in a project with do scripts.")))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
  ;; Override "super" bindings on macOS.
  (when axvr/macos?
    (global-unset-key (kbd "s-z"))
    (global-set-key (kbd "s-z") 'undo-fu-only-undo)
    (global-set-key (kbd "s-Z") 'undo-fu-only-redo)))

(use-package undo-fu-session
  :after undo-fu
  :config
  ;; TODO: add other Git files.
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package pdf-tools
  :mode "\\.pdf\\'"
  :hook (pdf-view-mode . pdf-view-roll-minor-mode) ; enable continuous scrolling
  :init (pdf-loader-install))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Highlight TODOs and more.
;; TODO: set colours.
(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :init (add-hook 'prog-mode-hook #'hl-prog-extra-mode))

(use-package transient)
(use-package magit
  :after (transient)
  :defer t)

(use-package diff-hl
  :config (global-diff-hl-mode)
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; TODO find an alternative.
;; (axvr/package-install 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; TODO: replace with tree-sitter version?
;; (use-package expand-region
;;   :config (global-set-key (kbd "C-=") 'er/expand-region))

(provide 'axvr-tools)

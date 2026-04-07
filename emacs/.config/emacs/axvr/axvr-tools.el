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
  (when-let ((default-directory (locate-dominating-file "." "do/")))
    (async-shell-command
     (expand-file-name
      (read-file-name "Run do script: " (expand-file-name "do/" default-directory))
      default-directory))))

(provide 'axvr-tools)

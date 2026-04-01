;;;; -*- lexical-binding: t; -*-

(defun replica-1 ()
  "Start a serial term that connects to my Replica 1 Plus."
  (interactive)
  ;; TODO: create an axvr/upper-case-mode (minor mode) and enable it in this buffer.
  (serial-term (if axvr/macos?
                   "/dev/tty.usbserial-AC00JRMK"
                 "/dev/ttyUSB1")
               9600))

(provide 'axvr-tools)

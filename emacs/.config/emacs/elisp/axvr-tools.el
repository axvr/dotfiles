;;;; -*- lexical-binding: t; -*-

(defun replica-1 ()
  "Start a serial term that connects to my Replica 1 Plus."
  (interactive)
  (serial-term (if axvr/macos?
                   "/dev/tty.usbserial-AC00JRMK"
                 "/dev/ttyUSB1")
               9600))

(provide 'axvr-tools)

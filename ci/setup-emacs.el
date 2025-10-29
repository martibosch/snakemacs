;;; setup-emacs.el --- setup packages in CI -*- lexical-binding: t; -*-

;; 1. base env setup
(defvar url-show-status)               ; silence URL messages
(let ((debug-on-error t)               ; backtraces on error
      (url-show-status nil)            ; keep URL library quiet
      (user-emacs-directory default-directory)   ; treat repo root as $HOME
      (user-init-file (expand-file-name "init.el")) ; load your normal init
      (load-path (delq default-directory load-path))) ; avoid loading repo as lib

  ;; load the user init file
  (load-file user-init-file)

  ;; run the after-init hooks
  (run-hooks 'after-init-hook))

;; 2. build ZMQ
(defvar ci--original-recursive-edit (symbol-function 'recursive-edit))

(defun ci--wait-for-compilation ()
  "Block until the current `*compilation*` process finishes.
Used as a temporary replacement for `recursive-edit`."
  (let ((proc (get-buffer-process "*compilation*")))
    (when proc
      (while (process-live-p proc)
        (accept-process-output proc 0.1))))
  ;; Return nil so the original call simply falls through.
  nil)

;; install the temporary advice.
(advice-add #'recursive-edit :override #'ci--wait-for-compilation)

;; force a `y` answer for the prompt inside `zmq-load`.
(let ((y-or-n-p (lambda (&rest _ignore) t)))
  (require 'zmq)               ; make sure the library is known
  (zmq-load))

;; clean up
(advice-remove #'recursive-edit #'ci--wait-for-compilation)

;; 3. end
(message "✅ successfully setup emacs init and built ZMQ")

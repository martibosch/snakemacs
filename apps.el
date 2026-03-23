;; -*- lexical-binding: t; -*-
;;; applications


;; shell
(use-package
  vterm
  :custom (vterm-install t)
  (vterm-always-compile-module t)
  :config
  (defun my/vterm-buffer-name ()
    (let ((project (projectile-project-name)))
      (if (string= project "-")
          "*vterm*"
        (format "*vterm[%s]*" project))))
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; only rename generic vterm buffers; leave ai-code-named ones alone
              (when (string-match-p "\\`\\*vterm" (buffer-name))
                (rename-buffer (my/vterm-buffer-name) t)))))

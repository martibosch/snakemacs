;;; snakemacs
;;; description: emacs 28 setup for Python with conda/mamba and Jupyter
;;; author: Martí Bosch <marti.bosch@protonmail.com>
;;; -*- lexical-binding: t; -*-

;;; bootstrap straight and use-package
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; https://github.com/radian-software/straight.el#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; custom file
(setq custom-file (expand-file-name (concat user-emacs-directory "custom.el")))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;; config by sections
(setq config-files '(
		     "look.el"
		     "main.el"
		     ))
(dolist (file config-files)
  (load-file (expand-file-name (concat user-emacs-directory file))))

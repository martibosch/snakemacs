;; -*- lexical-binding: t; -*-

;;; early init

;; for emacs>=27
(setq package-enable-at-startup nil)

;; https://github.com/conda-forge/emacs-feedstock/issues/60
;; (if (eq system-type 'gnu/linux)
;;     (progn (setq configure-info-directory (string-replace "\0" "" configure-info-directory))
;; 	   (setf (car Info-default-directory-list) configure-info-directory)))

;; for lsp-booster
(setenv "LSP_USE_PLISTS" "true")

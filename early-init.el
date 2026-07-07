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

;; lsp-mode throughput: raise GC threshold and the max chunk read from the
;; language server process (default 4k is too small for lsp payloads)
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100 MB
(setq read-process-output-max (* 3 1024 1024)) ;; 3 MB

(load "~/.emacs.d/my-loadpackages.el")
(add-hook 'after-init-hook '(lambda ()
			      (load "~/.emacs.d/my-noexternals.el")))

;; BASIC CUSTOMIZATION
(add-to-list 'load-path "~/.emacs.d/custom") ;; custom scripts path
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'zenburn t)

;; LINUM
(global-linum-mode t) ;; enable line numbers globally
(require 'linum-off)


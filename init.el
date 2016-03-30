(load "~/.emacs.d/my-loadpackages.el")
(add-hook 'after-init-hook '(lambda ()
			      (load "~/.emacs.d/my-noexternals.el")))

;; BASIC CUSTOMIZATION
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'zenburn t)
(global-linum-mode t) ;; enable line numbers globally



(load "~/.emacs.d/my-loadpackages.el")
(add-hook 'after-init-hook '(lambda ()
			      (load "~/.emacs.d/my-noexternals.el")))

;; BASIC CUSTOMIZATION
(add-to-list 'load-path "~/.emacs.d/custom") ;; custom scripts path
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'white-sand t)

;; SCROLLING IN TERM
(if (eq window-system nil)
    (let ((map (make-sparse-keymap)))
      (define-key input-decode-map "\e[1;5A" [C-up])
      (define-key input-decode-map "\e[1;5B" [C-down])
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])))

;; LINUM
(global-linum-mode t) ;; enable line numbers globally
(require 'linum-off)


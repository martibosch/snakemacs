;;; .emacs.d --- emacs26 setup for Python, C/C++, Web, Latex...
;;; Commentary:
;;; author: Martí Bosch <marti.bosch@protonmail.com>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


;; configure packages
(load "~/.emacs.d/conf-packages.el")


;; basic customization
(add-to-list 'load-path "~/.emacs.d/custom") ;; custom scripts path
(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
(require 'linum-off) ;; disable line numbers in some modes (e.g., in notebooks)
(load-theme 'zenburn t)
(add-hook 'after-init-hook '(lambda () 
                              (load "~/.emacs.d/no-externals.el")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:output-area-inlined-images t)
 '(package-selected-packages
   (quote
    (markdown-mode magit auctex yasnippet-snippets jupyter ein py-isort yapfify exec-path-from-shell flycheck elpy zenburn-theme pallet better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

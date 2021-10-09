;;; .emacs.d --- emacs25 setup for Python, C/C++, Web, Latex...
;;; Commentary:
;;; author: Martí Bosch <marti.bosch.1992@gmail.com>

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(require 'cask "~/.cask/cask.el")
;; see https://github.com/cask/cask/issues/463
(setq warning-suppress-log-types '((package reinitialization)))
(cask-initialize)

(load "~/.emacs.d/conf-packages.el")

(add-hook 'after-init-hook '(lambda () 
                              (load "~/.emacs.d/noexternals.el")))

;; BASIC CUSTOMIZATION
(add-to-list 'load-path "~/.emacs.d/custom") ;; custom scripts path
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'zenburn t)

;; SCROLLING IN TERM
(if (eq window-system nil) 
    (let ((map (make-sparse-keymap))) 
      (define-key input-decode-map "\e[1;5A" [C-up]) 
      (define-key input-decode-map "\e[1;5B" [C-down]) 
      (define-key input-decode-map "\e[1;5C" [C-right]) 
      (define-key input-decode-map "\e[1;5D" [C-left])))

;; LINUM
;; (global-linum-mode t) ;; enable line numbers globally
;; (require 'linum-off)

(put 'downcase-region 'disabled nil)

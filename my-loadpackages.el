; my-loadpackages.el
; loading package
(load "~/.emacs.d/my-packages.el")

;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-auto-start 1)
;; (define-key ac-complete-mode-map "\t" 'ac-complete)
;; (define-key ac-complete-mode-map "\r" nil)

(require 'ein)
;; (setq ein:use-auto-complete t)

(elpy-enable)
(elpy-use-ipython)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))


;;; conf-packages.el --- configuration of packages
;;; Commentary:
;;; author: Martí Bosch <marti.bosch@protonmail.com>


;; package management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)


;; better defaults
(require 'better-defaults)


;; git
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)


;; Python
;; elpy
(exec-path-from-shell-copy-env "PATH") ;; exec-path-from-shell (must go before elpy for ipython)
(exec-path-from-shell-copy-env "WORKON_HOME")
(elpy-enable)
(setq python-shell-interpreter "jupyter" python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil) 
(add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

;; use flycheck instead of flymake
(when (load "flycheck" t t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; yapf
(add-hook 'python-mode-hook (lambda () 
                              (unless (eq major-mode 'ein:notebook-python-mode) 
                                (yapf-mode))))

;; isort
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

;; yasnippets
(require 'yasnippet)
(yas-global-mode 1)


;; LaTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;; Turn on RefTeX in AUCTeX
(setq reftex-plug-into-AUCTeX t) ;; Activate nice interface between RefTeX and AUCTeX
;; disable (annoying) indentation for tabular environments
(setq LaTeX-indent-environment-list (quote (("verbatim" current-indentation) 
                                            ("verbatim*" current-indentation) 
                                            ("align" LaTeX-indent-tabular) 
                                            ("align*" LaTeX-indent-tabular) 
                                            ("array" LaTeX-indent-tabular) 
                                            ("eqnarray" LaTeX-indent-tabular) 
                                            ("eqnarray*" LaTeX-indent-tabular) 
                                            ("displaymath") 
                                            ("equation") 
                                            ("equation*") 
                                            ("picture") 
                                            ("tabbing"))))

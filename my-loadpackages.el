; my-loadpackages.el
; loading package
(load "~/.emacs.d/my-packages.el")


;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(add-hook 'TeX-mode-hook (lambda()
                           (add-to-list 'TeX-command-list '("View" "(lambda () (let ((f \"%o\")) (find-file-other-window f) (doc-view-mode)))" TeX-run-function nil t))))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;; Turn on RefTeX in AUCTeX
(setq reftex-plug-into-AUCTeX t) ;; Activate nice interface between RefTeX and AUCTeX


;; better-defaults
(require 'better-defaults)


;; company
(add-hook 'after-init-hook 'global-company-mode)


;; emacs ipython notebook
(require 'ein)
(setq ein:use-auto-complete t)
(defun define-key-request-help ()
  (interactive)
  (define-key ein:notebook-mode-map (kbd "C-c C-f") 'ein:pytools-request-help)
  (define-key ein:notebook-multilang-mode-map (kbd "C-c C-f") 'ein:pytools-request-help))
(add-hook 'ein:notebook-mode-hook 'define-key-request-help)


;; { begin elpy }
(exec-path-from-shell-copy-env "PATH") ;; exec-path-from-shell (must go before elpy for ipython)
(setq elpy-remove-modeline-lighter nil) ;; do not hide modeline, call before elpy-enable
(elpy-enable)
(when (executable-find "ipython")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (elpy-use-ipython))
(setq elpy-rpc-backend "jedi")
(define-key elpy-mode-map (kbd "C-c C-f") 'elpy-doc)
(setenv "WORKON_HOME" "~/anaconda3/envs") ;; for conda venvs
(pyvenv-mode 1)

;; autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
;; { end elpy }


;; magit
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)


;; octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))


;; readline-complete
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
(require 'readline-complete)
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))


;; rvm
(require 'rvm)
(rvm-use-default)


;; scss
(require 'scss-mode) ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/folder-where-you-put-scss-mode-el"))
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)


;; sphinx
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))


;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(setq web-mode-engines-alist
      '(
        ("liquid" . "/\\(_includes\\|_layouts\\)/\\(.*/\\)*.*\\.html\\'")
        ("django" . "/templates/\\(.*/\\)*.*\\.html\\'")
        )
      )


;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))
(add-hook 'web-mode-hook (lambda ()
                           (yas-activate-extra-mode 'html-mode)))

1; my-loadpackages.el
; loading package
(load "~/.emacs.d/my-packages.el")

;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
;; (add-hook 'TeX-mode-hook (lambda()
;;                            (alist-set 'TeX-command-list "View"
;;                                       '("(lambda () (let ((f \"%o\")) (find-file-other-window f) (doc-view-mode)))" TeX-run-function nil t))))
;; (add-hook 'TeX-mode-hook (lambda()
;;                            (alist-set 'TeX-command-list "View" "firefox %o")))
(add-hook 'TeX-mode-hook (lambda()
                           (add-to-list 'TeX-command-list '("View" "(lambda () (let ((f \"%o\")) (find-file-other-window f) (doc-view-mode)))" TeX-run-function nil t))))
;; (setq TeX-view-program-selection
;;       '((output-pdf "Emacs")))
;; (setq TeX-view-program-list
;;       '(("Emacs" "emacsclient %o")))
;; Turn on RefTeX in AUCTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Activate nice interface between RefTeX and AUCTeX
(setq reftex-plug-into-AUCTeX t)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 1)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)
(defadvice auto-complete-mode (around disable-auto-complete-for-python)
  (unless (eq major-mode 'python-mode) ad-do-it))
(ad-activate 'auto-complete-mode) ;; disable auto-complete for python-mode (since using elpy)

;; emacs ipython notebook
(require 'ein)
(setq ein:use-auto-complete t)
(defun define-key-request-help ()
  (interactive)
  (define-key ein:notebook-mode-map (kbd "C-c C-f") 'ein:pytools-request-help)
  (define-key ein:notebook-multilang-mode-map (kbd "C-c C-f") 'ein:pytools-request-help))
(add-hook 'ein:notebook-mode-hook 'define-key-request-help)

;; elpy
(elpy-enable)
(elpy-use-ipython)
(setq elpy-rpc-backend "jedi")
(define-key elpy-mode-map (kbd "C-c C-f") 'elpy-doc)
(setenv "WORKON_HOME" "~/anaconda2/envs") ;; for conda venvs
(pyvenv-mode 1)

;; flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

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

;; autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; sphinx
(add-hook 'python-mode-hook (lambda ()
                              (require 'sphinx-doc)
                              (sphinx-doc-mode t)))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
    (setq yas-dont-activate t)))

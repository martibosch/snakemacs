;;; conf-packages.el --- configuration of packages
;;; Commentary:
;;; author: Martí Bosch <marti.bosch.1992@gmail.com>

;;;;;;;;;;;;;;;;;;;;;;
;; Common utilities

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; (setq ac-auto-start 1) do not enable ac, we are using company-mode!
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" nil)


;; bash-completion
(require 'bash-completion)
(bash-completion-setup)


;; better-defaults
(require 'better-defaults)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; elisp-format
(require 'elisp-format)
(defun elisp-format-before-save () 
  "Apply elisp-format to any elisp-buffer before saving." 
  (interactive) 
  (when (eq major-mode 'emacs-lisp-mode) 
    (elisp-format-buffer)))
(add-hook 'before-save-hook #'elisp-format-before-save)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; magit
(require 'magit)
(define-key global-map (kbd "C-x g") 'magit-status)

;; markdown
;; note that `markdown_github` is deprecated and should be replaced by `gfm` in pandoc 2.0
(setq markdown-command
      "pandoc --css ~/.emacs.d/pandoc-gfm.css -f markdown_github -t html5 --mathjax --highlight-style pygments --standalone")

;; pandoc
(add-hook 'markdown-mode-hook 'pandoc-mode)


;; rvm
(require 'rvm)
(rvm-use-default)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda() 
                            (setq yas-dont-activate t)))
(add-hook 'web-mode-hook (lambda () 
                           (yas-activate-extra-mode 'html-mode)))


;;;;;;;;;;;;;;;;;;;;;;
;; Python

;; emacs ipython notebook
(require 'ein)

;; { begin elpy }
(exec-path-from-shell-copy-env "PATH") ;; exec-path-from-shell (must go before elpy for ipython)
(exec-path-from-shell-copy-env "WORKON_HOME")
(setq elpy-remove-modeline-lighter nil) ;; do not hide modeline, call before elpy-enable
(elpy-enable)
(when (executable-find "jupyter") 
  (setq python-shell-interpreter "jupyter" python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil) 
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter"))
(setq safe-local-variable-values '((python-shell-interpreter-args . "-i manage.py shell"))) ;; to run django-shells
;; ACHTUNG: just related to a bug in emacs 25.1.x, so this way warning is ignored
;; (setq python-shell-prompt-detect-failure-warning nil)
(defun python-shell-completion-native-try () 
  "Return non-nil if can trigger native completion."
  (with-eval-after-load 'python '(let ((python-shell-completion-native-enable t) 
                                       (python-shell-completion-native-output-timeout
                                        python-shell-completion-native-try-output-timeout)) 
                                   (python-shell-completion-native-get-completions
                                    (get-buffer-process (current-buffer)) nil "_"))))
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-virtualenv-path "~/anaconda3/envs/emacs")
;; use black (needs python 3)
(setq elpy-rpc-python-command "python3")
(add-hook 'elpy-mode-hook (lambda () 
                            (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))
(define-key elpy-mode-map (kbd "C-c C-h") 'elpy-doc)
;; (setenv "WORKON_HOME" "~/anaconda3/envs") ;; for conda venvs
(pyvenv-mode 1)
;; { end elpy }

;; flycheck
(when 
    (require 'flycheck nil t) 
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)) 
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; py-isort
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save)

;; sphinx
(add-hook 'python-mode-hook (lambda () 
                              (require 'sphinx-doc) 
                              (sphinx-doc-mode t)))

;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

;; { begin cmake-ide }
(cmake-ide-setup)
(setq cmake-ide-flycheck-enabled nil)
(setq c-mode-hooks '(c++-mode-hook c-mode-hook objc-mode-hook))

(dolist (hook c-mode-hooks) 
  (add-hook hook 'irony-mode))

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(add-hook 'irony-mode-hook #'irony-eldoc)

;; clang-format
(require 'clang-format)
(dolist (hook c-mode-hooks) 
  (add-hook hook (lambda () 
                   (local-set-key (kbd "C-c i") 'clang-format-region) 
                   (local-set-key (kbd "C-c u") 'clang-format-buffer))))

(setq clang-format-style-option "llvm")
(defun clang-format-before-save () 
  "Apply clang-format to any c- buffer before saving." 
  (interactive) 
  (when (member major-mode '(c++-mode c-mode objc-mode)) 
    (clang-format-buffer)))
(add-hook 'before-save-hook #'clang-format-before-save)
;; { end cmake-ide }


;;;;;;;;;;;;;;;;;;;;;;
;; Java

;; meghanada
(require 'meghanada)
(add-hook 'java-mode-hook (lambda ()
                            ;; meghanada-mode on
                            (meghanada-mode t) 
                            (flycheck-mode +1) 
                            (setq c-basic-offset 2)
                            ;; use code format
                            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond ((eq system-type 'windows-nt) 
       (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))) 
       (setq meghanada-maven-path "mvn.cmd")) 
      (t 
       (setq meghanada-java-path "java") 
       (setq meghanada-maven-path "mvn")))


;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(add-hook 'TeX-mode-hook (lambda() 
                           (add-to-list 'TeX-command-list '("View"
                                                            "(lambda () (let ((f \"%o\")) (find-file-other-window f) (doc-view-mode)))"
                                                            TeX-run-function nil t))))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ;; Turn on RefTeX in AUCTeX
(setq reftex-plug-into-AUCTeX t) ;; Activate nice interface between RefTeX and AUCTeX
(require 'auctex-latexmk)
(auctex-latexmk-setup)
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

;;;;;;;;;;;;;;;;;;;;;;
;; Web

;; sass
(require 'sass-mode)

;; scss
(require 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(setq web-mode-engines-alist '(("django" .
                                "\\(/templates/\\(.*/\\)*.*\\.html\\'\\|/\\(_includes\\|_layouts\\)/\\(.*/\\)*.*\\.html\\'\\)")))


;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; octave
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(add-hook 'octave-mode-hook (lambda () 
                              (abbrev-mode 1) 
                              (auto-fill-mode 1) 
                              (if (eq window-system 'x) 
                                  (font-lock-mode 1))))

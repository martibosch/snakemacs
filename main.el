;;; main
;;; -*- lexical-binding: t; -*-

;;; general
;; misc
(use-package
  conda
  :config (conda-env-autoactivate-mode t)
  ;; TODO: we need to activate the envs for python files but not for, e.g., jupyter repl buffer
  :hook (python-mode . (lambda () (conda-env-activate-for-buffer))))

;; projet
(use-package
  projectile
  :config (projectile-mode +1))

;; completion
(use-package
  company
  :config (global-company-mode))

;; syntax checker
(use-package flycheck
  :config
  (flycheck-define-checker python-ruff
    "A Python syntax and style checker using the ruff utility.
    To override the path to the ruff executable, set
    `flycheck-python-ruff-executable'.
    See URL `http://pypi.python.org/pypi/ruff'."
    :command ("ruff"
              "--format=text"
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-ruff)
  :init (global-flycheck-mode))

;;; python
;; language server
(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 ((lambda () (unless (eq major-mode 'snakemake-mode)
				       (lsp))))))
  :config
  ;; these hooks can't go in the :hook section since lsp-restart-workspace
  ;; is not available if lsp isn't active
  ;; (add-hook 'conda-postactivate-hook (lambda () (lsp-restart-workspace)))
  ;; (add-hook 'conda-postdeactivate-hook (lambda () (lsp-restart-workspace)))
  )

;;; jupyter
(use-package
  jupyter)

;;; code cells
(use-package code-cells
  :config
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-c <up>") 'code-cells-backward-cell)
    (define-key map (kbd "C-c <down>") 'code-cells-forward-cell)
    (define-key map (kbd "M-<up>") 'code-cells-move-cell-up)
    (define-key map (kbd "M-<down>") 'code-cells-move-cell-down)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))

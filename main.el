;;; main
;;; -*- lexical-binding: t; -*-

;;; general
;; misc
(use-package
  conda
  :config (conda-env-autoactivate-mode t)
  (require 'conda-projectile)
  (conda-projectile-mode-line-setup)
  ;; TODO: we need to activate the envs for python files but not for, e.g., jupyter repl buffer
  :hook (python-mode . (lambda () (conda-env-activate-for-buffer))))

(use-package atomic-chrome
  :config (atomic-chrome-start-server))

;;  key bindings
(use-package
  which-key
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'frame)
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; text editing
(use-package aggressive-indent :commands (aggressive-indent-mode))

(use-package yasnippet-snippets)

(use-package
  yasnippet
  :config
  (setq yas-snippet-dirs
	`(,(concat (expand-file-name user-emacs-directory) "snippets")
          yasnippet-snippets-dir))
  (setq yas-triggers-in-field t) (yas-global-mode 1))

;; matching parentheses
(use-package smartparens :config (show-smartparens-global-mode t))

;; project management
(use-package magit)

(use-package projectile :config (projectile-mode +1))

(use-package counsel-projectile :after (counsel projectile))

;; completion
(use-package
  ivy
  :custom (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel :after ivy :config (counsel-mode))

(use-package swiper :defer t)

(use-package
  ivy-rich
  :after ivy
  :config (ivy-rich-mode 1)
  (setcdr
   (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package
  ivy-prescient
  :after counsel
  :custom (ivy-prescient-retain-classic-highlighting t)
  :config (ivy-prescient-mode +1) (prescient-persist-mode 1)
  ;; Do not use prescient in find-file
  (ivy--alist-set
   'ivy-sort-functions-alist
   #'read-file-name-internal
   #'ivy-sort-file-function-default))

(use-package company :config (global-company-mode))

(use-package
  copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after company
  :config
  (delq 'company-preview-if-just-one-frontend company-frontends)
  (define-key
   copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key
   copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  :hook
  ((prog-mode
    .
    (lambda ()
      (unless (equal (buffer-name) "*scratch*")
	copilot-mode)))))

;; syntax checker
(use-package
  flycheck
  :init (global-flycheck-mode)
  :config
  ;; we have to redefine the python-ruff checker to work on notebooks-as-scripts with code-cells
  (flycheck-define-checker
      python-ruff
    "A Python syntax and style checker using Ruff.

See URL `https://docs.astral.sh/ruff/'."
    :command
    ("ruff"
     "check"
     (config-file "--config" flycheck-python-ruff-config)
     "--output-format=concise"
     ;; we actually only change the commented line below
     ;; (option "--stdin-filename" buffer-file-name)
     ;; for this `eval`
     (eval
      (when (and buffer-file-name
		 (not (string-suffix-p ".ipynb" buffer-file-name)))
	`("--stdin-filename" ,buffer-file-name)))
     ;; end of changes
     "-")
    :standard-input t
    :error-filter
    (lambda (errors)
      (let* ((errors (flycheck-sanitize-errors errors))
             (errors-with-ids (seq-filter #'flycheck-error-id errors)))
	(seq-union
	 (seq-difference errors errors-with-ids)
	 (seq-map #'flycheck-flake8-fix-error-level errors-with-ids))))
    :error-patterns
    ((error
      line-start
      (or "-" (file-name))
      ":"
      line
      ":"
      (optional column ":")
      " "
      "SyntaxError: "
      (message (one-or-more not-newline))
      line-end)
     (warning
      line-start
      (or "-" (file-name))
      ":"
      line
      ":"
      (optional column ":")
      " "
      (id (one-or-more (any alpha)) (one-or-more digit) " ")
      (message (one-or-more not-newline))
      line-end))
    :working-directory flycheck-python-find-project-root
    :modes (python-mode python-ts-mode)
    :next-checkers ((warning . python-mypy))))

;; code parsing
(use-package
  tree-sitter
  :hook
  (python-mode
   .
   (lambda ()
     (unless (eq major-mode 'snakemake-mode)
       (tree-sitter-mode))))
  (python-mode
   .
   (lambda ()
     (unless (eq major-mode 'snakemake-mode)
       (tree-sitter-hl-mode)))))

(use-package tree-sitter-langs :after tree-sitter)

;;; YAML
(use-package
  yaml-mode
  :mode "\\.yml\\'"
  ;; :hook (yaml-mode . highlight-indent-guides-mode)
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;; web
(use-package
  web-mode
  :commands (web-mode)
  :init (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :hook (web-mode . my/engine-map-hook)
  :config
  (defun my/engine-map-hook ()
    (if (projectile-project-p)
	(if (or (file-exists-p
                 (concat (projectile-project-root) "manage.py"))
		(file-exists-p
                 (concat (projectile-project-root) "_config.yml")))
            (web-mode-set-engine "django")))))

;;; markdown
(use-package
  markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  ;; (setq markdown-command
  ;; 	(concat
  ;; 	 "pandoc"
  ;; 	 " --from=markdown --to=html"
  ;; 	 " --standalone --mathjax --highlight-style=pygments"
  ;; 	 " --css=pandoc.css"
  ;; 	 " --quiet"
  ;; 	 ))
  )

;;; LaTeX
(use-package
  tex
  :straight auctex
  :defer t
  :custom (TeX-auto-save t)
  :config
  ;; use XeLaTeX
  ;; (setq-default TeX-engine 'xetex)
  ;; (setq-default TeX-command-extra-options "-shell-escape")
  ;; synctex
  (setq-default TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-start-server t)
  (setq-default LaTeX-math-menu-unicode t)
  ;; (setq-default font-latex-fontify-sectioning 1.3)
  ;; use tectonic
  ;; https://tectonic-typesetting.github.io/book/latest/howto/auctex-setup/index.html
  ;; ACHTUNG: tectonic is built around XeLaTeX which is INCOMPATIBLE with arxiv, so use texlive until https://github.com/tectonic-typesetting/tectonic/discussions/956 is addressed
  ;; (add-to-list 'TeX-command-list '("tectonic" "%`tectonic -X compile --synctex --keep-logs %t"
  ;; 				   TeX-run-command nil t))
  ;; start: uncomment to use tectonic
  ;; (setq TeX-engine-alist '((default
  ;;                           "Tectonic"
  ;;                           "tectonic -X compile -f plain %T"
  ;;                           ;; "tectonic -X watch"
  ;; 			    "tectonic -X compile --synctex --keep-logs %T"
  ;;                           nil)))
  ;; (setq LaTeX-command-style '(("" "%(latex)")))
  ;; (setq TeX-process-asynchronous t
  ;; 	TeX-check-TeX nil
  ;; 	TeX-engine 'default)
  ;; (let ((tex-list (assoc "TeX" TeX-command-list))
  ;; 	(latex-list (assoc "LaTeX" TeX-command-list)))
  ;;   (setf (cadr tex-list) "%(tex)"
  ;;         (cadr latex-list) "%l"))
  ;; (add-hook 'after-change-major-mode-hook
  ;;           (lambda ()
  ;;             (when-let ((project (project-current))
  ;; 			 (proot (project-root project)))
  ;; 		(when (file-exists-p (expand-file-name "Tectonic.toml" proot))
  ;;                 (setq-local TeX-output-dir (expand-file-name "build/index" proot))))))
  ;; end: uncomment to use tectonic
  ;; ;; pdf view with eaf
  ;; (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
  ;; (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))

  ;; Do not run lsp within templated TeX files
  :hook
  (LaTeX-mode
   .
   (lambda ()
     (unless (string-match "\.hogan\.tex$" (buffer-name))
       (lsp))
     (setq-local lsp-diagnostic-package :none)
     (setq-local flycheck-checker 'tex-chktex)))
  (LaTeX-mode . turn-on-reftex))

;; (use-package
;;   ivy-bibtex
;;   :commands (ivy-bibtex)
;;   :bind ("C-c b" . ivy-bibtex)
;;   :hook (bibtex-mode . smartparens-mode))
(use-package
  reftex
  :commands turn-on-reftex
  :custom (reftex-plug-into-AUCTeX t))

(use-package gscholar-bibtex)

(use-package
  lsp-latex
  :disabled
  :hook ((TeX-mode bibtex-mode) . lsp-deferred)
  :commands (lsp-latex-build)
  :config (setq lsp-latex-build-executable "tectonic")
  (setq lsp-latex-build-args
	'("%f" "--synctex" "--keep-logs" "--keep-intermediates")))

;;; lisp
(use-package
  lispy
  :hook (emacs-lisp-mode . (lambda () (lispy-mode 1))))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(use-package
  elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;; python
;; language server
(use-package
  lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; TODO
  ;; (with-eval-after-load 'lsp-mode
  ;;   (lsp-register-client
  ;;    (make-lsp-client :new-connection (lsp-stdio-connection '("ruff" "server" "--preview"))
  ;; 		      :major-modes '(python-mode)
  ;; 		      :priority 1
  ;; 		      :add-on? t
  ;; 		      :multi-root t
  ;; 		      :server-id 'ruff-server)
  ;;    )
  ;;   )
  ;; (setq lsp-disabled-clients '(python-mode . (ruff-lsp)))
  :hook
  (python-mode
   .
   (lambda ()
     (unless (eq major-mode 'snakemake-mode)
       (lsp-deferred))))
  :config
  (setq lsp-pylsp-plugins-ruff-enabled t)
  :commands lsp lsp-deferred
  ;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgc88562e
  ;; https://github.com/blahgeek/emacs-lsp-booster/issues/19
  ;; :preface
  ;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
  ;;   "Try to parse bytecode instead of json."
  ;;   (or
  ;;    (when (equal (following-char) ?#)

  ;;      (let ((bytecode (read (current-buffer))))
  ;;        (when (byte-code-function-p bytecode)
  ;;          (funcall bytecode))))
  ;;    (apply old-fn args)))
  ;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  ;;   "Prepend emacs-lsp-booster command to lsp CMD."
  ;;   (let ((orig-result (funcall old-fn cmd test?)))
  ;;     (if (and (not test?) ;; for check lsp-server-present?
  ;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
  ;;              lsp-use-plists
  ;;              (not (functionp 'json-rpc-connection)) ;; native json-rpc
  ;;              (executable-find "emacs-lsp-booster"))
  ;;         (progn
  ;;           (message "Using emacs-lsp-booster for %s!" orig-result)
  ;;           (cons "emacs-lsp-booster" orig-result))
  ;;       orig-result)))
  ;; :init
  ;; (setq lsp-use-plists t)
  ;; ;; Initiate https://github.com/blahgeek/emacs-lsp-booster for performance
  ;; (advice-add (if (progn (require 'json)
  ;;                        (fboundp 'json-parse-buffer))
  ;;                 'json-parse-buffer
  ;;               'json-read)
  ;;             :around
  ;;             #'lsp-booster--advice-json-parse)
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; (use-package
;;   lsp-pyright
;;   :ensure t
;;   :custom
;;   (lsp-pyright-langserver-command
;;    (if (executable-find "basedpyright")
;;        "basedpyright"
;;      "pyright"))
;;   :hook
;;   (python-mode
;;    .
;;    (lambda ()
;;      (require 'lsp-pyright)
;;      (lsp))))

(use-package cython-mode)

;; formatting
(use-package
  reformatter
  :hook
  (python-mode . ruff-check-fix-on-save-mode)
  (python-mode . ruff-format-on-save-mode)
  :config
  ;; from https://www.reddit.com/r/emacs/comments/17gqjsy/using_ruff_format_with_emacs_to_reformat_python/
  ;; ACHTUNG: do NOT use `--stdin-filename` `buffer-file-name` because it will fail when
  ;; formatting notebooks (since in emacs buffers they are python scripts with percent
  ;; format using code-cells)
  (reformatter-define
    ruff-check-fix
    :program "ruff"
    :args `("check" "--fix" "--fix-only" "-"))
  (reformatter-define
    ruff-format
    :program "ruff"
    :args `("format" "-")))

(use-package
  python-docstring
  :hook (python-mode . python-docstring-mode)
  ;; this hook does not necessarily belong here as it applies to python-mode more broadly
  ;; but it is the most related use-package declaration
  (python-mode
   .
   (lambda ()
     (setq fill-column 88)
     ;; (auto-fill-mode t)
     (display-fill-column-indicator-mode 1))))

(use-package filladapt :hook (python-mode . filladapt-mode))

;;; Snakemake
(use-package snakemake-mode)
(use-package
  format-all
  ;; TODO: replace with reformatter?
  :commands format-all-mode
  :hook (snakemake-mode . format-all-mode))

;;; docker
(use-package
  dockerfile-mode
  :mode "Dockerfile\\'"
  ;; :config
  ;; (add-hook 'dockerfile-mode 'smartparens-mode)
  )

;;; terraform
(use-package
  terraform-mode
  ;; TODO: see the link below on how to connect terraform with lsp
  ;; https://www.reddit.com/r/emacs/comments/k6pp9r/debugging_lsp_and_terraform_config/
  ;; :hook (terraform-mode . lsp)
  :hook (terraform-mode . terraform-format-on-save-mode))

;;; org

;; base
(use-package
  org
  :straight (:type built-in)
  :defer t
  :init
  :config
  (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (setq org-confirm-babel-evaluate nil)
  ;; (add-hook 'org-mode-hook 'smartparens-mode)
  ;; (add-hook 'org-mode-hook (lambda ()
  ;; 			     (rainbow-delimiters-mode -1)))
  (require 'org-tempo)
  (add-to-list
   'org-structure-template-alist '("py" . "src jupyter-python")))

(use-package org-contrib :after (org))

;; (use-package poly-org)

;; python and jupyter
;;; custom zmq build - see https://github.com/alexmurray/emacs-snap/issues/66
(let* ((emacs-snap-dir
        (file-name-as-directory (getenv "EMACS_SNAP_DIR")))
       (process-environment
        (append
         process-environment
         `(,(concat "CC=" emacs-snap-dir "usr/bin/gcc-10")
           ,(concat "CXX=" emacs-snap-dir "usr/bin/g++-10")
           ,(concat "CFLAGS=--sysroot=" emacs-snap-dir)
           ,(concat "CPPFLAGS=--sysroot=" emacs-snap-dir)
           ,(concat
             "LDFLAGS=--sysroot="
             emacs-snap-dir
             " -L"
             emacs-snap-dir
             "/usr/lib")))))
  (use-package zmq))
;;; emacs-jupyter
(use-package jupyter :after (org))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (shell . t) (jupyter . t)))

(defun my/jupyter-refresh-kernelspecs ()
  "Refresh Jupyter kernelspecs"
  (interactive)
  (jupyter-available-kernelspecs t))

(defun my/jupyter-refesh-langs ()
  "Refresh Jupyter languages"
  (interactive)
  (org-babel-jupyter-aliases-from-kernelspecs t))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(org-babel-jupyter-override-src-block "python")
;; (add-hook 'org-src-mode-hook (lambda ()
;; 			       ;; (hs-minor-mode -1)
;; 			       ;; (electric-indent-local-mode -1)
;; 			       ;; (rainbow-delimiters-mode -1)
;; 			       (highlight-indent-guides-mode -1)))

(setq my/org-view-html-tmp-dir "/tmp/org-html-preview/")

(use-package f)

(defun my/org-view-html ()
  (interactive)
  (let ((elem (org-element-at-point))
        (temp-file-path
         (concat
          my/org-view-html-tmp-dir
          (number-to-string (random (expt 2 32)))
          ".html")))
    (cond
     ((not (eq 'export-block (car elem)))
      (message "Not in an export block!"))
     ((not (string-equal (plist-get (car (cdr elem)) :type) "HTML"))
      (message "Export block is not HTML!"))
     (t
      (progn
        (f-mkdir my/org-view-html-tmp-dir)
        (f-write
         (plist-get (car (cdr elem)) :value) 'utf-8 temp-file-path)
        (start-process "org-html-preview" nil "xdg-open"
                       temp-file-path))))))

(use-package
  ob-async
  :after (org)
  :config
  (setq ob-async-no-async-languages-alist
	'("python" "jupyter-python")))

(setq my/jupyter-runtime-folder
      (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  (mapcar
   #'string-to-number
   (split-string (shell-command-to-string
                  "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'")
                 "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar
   (lambda (file)
     (cons
      (car file)
      (cdr (assq 'shell_port (json-read-file (car file))))))
   (sort
    (directory-files-and-attributes my/jupyter-runtime-folder
                                    t ".*kernel.*json$")
    (lambda (x y) (not (time-less-p (nth 6 x) (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
        (files (my/list-jupyter-kernel-files)))
    (completing-read
     "Jupyter kernels: "
     (seq-filter (lambda (file) (member (cdr file) ports)) files))))

(defun my/insert-jupyter-kernel ()
  "Insert a path to an active Jupyter kernel into the buffer"
  (interactive)
  (insert (my/select-jupyter-kernel)))

(defun my/jupyter-connect-repl ()
  "Open an emacs-jupyter REPL, connected to a Jupyter kernel"
  (interactive)
  (jupyter-connect-repl (my/select-jupyter-kernel) nil nil nil t))

(defun my/jupyter-cleanup-kernels ()
  (interactive)
  (let* ((ports (my/get-open-ports))
         (files (my/list-jupyter-kernel-files))
         (to-delete
          (seq-filter
           (lambda (file) (not (member (cdr file) ports))) files)))
    (when (and (length> to-delete 0)
               (y-or-n-p
                (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
        (delete-file (car file))))))

;;; ein
(use-package ein)

;;; code cells
(use-package
  code-cells
  :config
  ;; (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
  ;; 					 ("pandoc" "--to" "org" "--from" "ipynb")
  ;; 					 org-mode))
  ;; see https://github.com/astoff/code-cells.el/issues/22
  ;; (defun gm/jupyter-eval-region (beg end)
  ;;   (jupyter-eval-region nil beg end))
  ;; (add-to-list 'code-cells-eval-region-commands '(jupyter-repl-interaction-mode . gm/jupyter-eval-region))
  (let ((map code-cells-mode-map))
    (define-key map (kbd "C-c <up>") 'code-cells-backward-cell)
    (define-key map (kbd "C-c <down>") 'code-cells-forward-cell)
    (define-key map (kbd "M-<up>") 'code-cells-move-cell-up)
    (define-key map (kbd "M-<down>") 'code-cells-move-cell-down)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key
     map [remap jupyter-eval-line-or-region] 'code-cells-eval)))
(defun my/new-notebook (notebook-name &optional kernel)
  "Creates an empty notebook in the current directory with an associated kernel."
  (interactive "sEnter the notebook name: ")
  (when (file-name-extension notebook-name)
    (setq notebook-name (file-name-sans-extension notebook-name)))
  (unless kernel
    (setq kernel
          (jupyter-kernelspec-name
           (jupyter-completing-read-kernelspec))))
  (unless (executable-find "jupytext")
    (error "Can't find \"jupytext\""))
  (let ((notebook-py (concat notebook-name ".py")))
    (shell-command (concat "touch " notebook-py))
    (shell-command
     (concat "jupytext --set-kernel " kernel " " notebook-py))
    (shell-command (concat "jupytext --to notebook " notebook-py))
    (shell-command (concat "rm " notebook-py))
    (message
     (concat
      "Notebook successfully created at " notebook-name ".ipynb"))))

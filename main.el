;;; main
;;; -*- lexical-binding: t; -*-

;;; general
;; misc
(use-package
  conda
  :config (conda-env-autoactivate-mode t)
  :hook (python-mode . (lambda () (conda-env-activate-for-buffer))))

;; text editing
(use-package aggressive-indent
  :commands (aggressive-indent-mode))

(use-package
  yasnippet-snippets)

(use-package
  yasnippet
  :config (setq yas-snippet-dirs `(,(concat (expand-file-name user-emacs-directory) "snippets")
				   yasnippet-snippets-dir))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

;; project management
(use-package
  magit)

(use-package
  projectile
  :config (projectile-mode +1))

(use-package
  counsel-projectile
  :after (counsel projectile))

;; completion
(use-package
  ivy
  :custom (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package
  counsel
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :defer t)

(use-package
  ivy-rich
  :after ivy
  :config (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package
  ivy-prescient
  :after counsel
  :custom (ivy-prescient-retain-classic-highlighting t)
  :config (ivy-prescient-mode +1)
  (prescient-persist-mode 1)
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal
		  #'ivy-sort-file-function-default))

(use-package
  company
  :config (global-company-mode))

;; syntax checker
(use-package flycheck
  :init (global-flycheck-mode))

;; code parsing
(use-package tree-sitter
  :hook ((python-mode . tree-sitter-mode)
	 (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

;;; YAML
(use-package
  yaml-mode
  :mode "\\.yml\\'"
  :hook (yaml-mode . highlight-indent-guides-mode)
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;; lisp
(use-package lispy
  :hook (emacs-lisp-mode . (lambda () (lispy-mode 1))))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
;; (add-hook 'emacs-lisp-mode-hook #'lispy-mode)

;;; python
;; language server
(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp)))
  :config
  ;; these hooks can't go in the :hook section since lsp-restart-workspace
  ;; is not available if lsp isn't active
  (add-hook 'conda-postactivate-hook (lambda () (lsp-restart-workspace)))
  (add-hook 'conda-postdeactivate-hook (lambda () (lsp-restart-workspace))))


;; formatting
(use-package
  py-isort
  :hook (before-save . py-isort-before-save))

(use-package
  blacken
  :hook (python-mode . blacken-mode))

;;; Snakemake
(use-package
  snakemake-mode)

;;; code cells
(use-package code-cells
  :commands (code-cells-mode)
  :config
  (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
					 ("pandoc" "--to" "org" "--from" "ipynb")
					 org-mode)))

;;; org

;; base
(use-package org
  :straight (:type built-in)
  :defer t
  :init
  :config (setq org-startup-indented t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively nil)
  (setq org-confirm-babel-evaluate nil)
  ;; (add-hook 'org-mode-hook 'smartparens-mode)
  ;; (add-hook 'org-mode-hook (lambda ()
  ;; 			     (rainbow-delimiters-mode -1)))
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("py" . "src jupyter-python")))

(use-package org-contrib
  :after (org))

;; python and jupyter
(use-package
  jupyter
  :after (org))
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)
							 (shell . t)
							 (jupyter . t)))

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
(add-hook 'org-src-mode-hook (lambda ()
			       ;; (hs-minor-mode -1)
			       ;; (electric-indent-local-mode -1)
			       ;; (rainbow-delimiters-mode -1)
			       (highlight-indent-guides-mode -1)))

(setq my/org-view-html-tmp-dir "/tmp/org-html-preview/")

(use-package
  f)

(defun my/org-view-html ()
  (interactive)
  (let ((elem (org-element-at-point))
	(temp-file-path (concat my/org-view-html-tmp-dir (number-to-string (random (expt 2 32)))
				".html")))
    (cond ((not (eq 'export-block (car elem)))
	   (message "Not in an export block!"))
	  ((not (string-equal (plist-get (car (cdr elem))
					 :type) "HTML"))
	   (message "Export block is not HTML!"))
	  (t (progn (f-mkdir my/org-view-html-tmp-dir)
		    (f-write (plist-get (car (cdr elem))
					:value) 'utf-8 temp-file-path)
		    (start-process "org-html-preview" nil "xdg-open" temp-file-path))))))

(use-package
  ob-async
  :after (org)
  :config (setq ob-async-no-async-languages-alist '("python" "jupyter-python")))

(setq my/jupyter-runtime-folder (expand-file-name "~/.local/share/jupyter/runtime"))

(defun my/get-open-ports ()
  (mapcar #'string-to-number (split-string (shell-command-to-string
					    "ss -tulpnH | awk '{print $5}' | sed -e 's/.*://'")
					   "\n")))

(defun my/list-jupyter-kernel-files ()
  (mapcar (lambda (file)
	    (cons (car file)
		  (cdr (assq 'shell_port (json-read-file (car file))))))
	  (sort (directory-files-and-attributes my/jupyter-runtime-folder t ".*kernel.*json$")
		(lambda (x y)
		  (not (time-less-p (nth 6 x)
				    (nth 6 y)))))))

(defun my/select-jupyter-kernel ()
  (let ((ports (my/get-open-ports))
	(files (my/list-jupyter-kernel-files)))
    (completing-read "Jupyter kernels: " (seq-filter (lambda (file)
						       (member (cdr file) ports)) files))))

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
	 (to-delete (seq-filter (lambda (file)
				  (not (member (cdr file) ports))) files)))
    (when (and (length> to-delete 0)
	       (y-or-n-p (format "Delete %d files?" (length to-delete))))
      (dolist (file to-delete)
	(delete-file (car file))))))

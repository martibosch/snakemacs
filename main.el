;; -*- lexical-binding: t; -*-
;;; main

;;; general
;; misc
;; (use-package exec-path-from-shell
;;   :config (exec-path-from-shell-initialize))

(use-package atomic-chrome :config (atomic-chrome-start-server))

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
 ;; ;; Do not use prescient in find-file
 ;; (ivy--alist-set
 ;;  'ivy-sort-functions-alist
 ;;  #'read-file-name-internal
 ;;  #'ivy-sort-file-function-default)
 )

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
(use-package treesit-auto :config (global-treesit-auto-mode))

;; indentation guides
(use-package
 highlight-indent-guides
 :straight (:host github :repo "bumblepup/highlight-indent-guides")
 :custom
 ;; `character' is the portable method, `bitmap' needs image support
 (highlight-indent-guides-method 'character)
 ;; highlight the guide of the block point is in
 (highlight-indent-guides-responsive 'top)
 ;; faces are derived from the theme background
 (highlight-indent-guides-auto-character-face-perc 25)
 (highlight-indent-guides-auto-top-character-face-perc 80)
 ;; hook both so guides survive a treesit-auto remap to `yaml-ts-mode'
 :hook ((yaml-mode yaml-ts-mode) . highlight-indent-guides-mode))

;;; YAML
(use-package yaml-mode :mode "\\.ya?ml\\'")

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
;; markdown as a lighter-weight alternative to org for writing things up, going through
;; pandoc rather than through the org exporters - ACHTUNG: `--citeproc' makes pandoc
;; resolve citations itself (CSL), emitting already-formatted output, so this route
;; never runs biblatex/biber; the `biber' pin in pixi.toml only constrains org/LaTeX
(defun my/markdown-export-pdf ()
  "Export the current markdown buffer to PDF with pandoc.

Citations come from the .bib files that `my/cite-local-bibliography' found next to
the file, so neither a YAML `bibliography:' key nor a `--bibliography' flag has to
be written by hand.  Uses tectonic as the PDF engine when available (see the `tex'
feature in pixi.toml), otherwise whichever engine pandoc defaults to."
  (interactive)
  (unless (executable-find "pandoc")
    (user-error "`pandoc' not found - it ships with the `tex' pixi environment"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  (let* ((md-filepath (buffer-file-name))
         (pdf-filepath (concat (file-name-sans-extension md-filepath) ".pdf"))
         (args
          (append
           (list "--citeproc")
           ;; one `--bibliography' per file, mirroring what citar completes against
           (mapcan
            (lambda (bib-filepath) (list "--bibliography" bib-filepath))
            (bound-and-true-p citar-bibliography))
           (when (executable-find "tectonic")
             (list "--pdf-engine" "tectonic"))
           (list "--output" pdf-filepath md-filepath))))
    (save-buffer)
    (compile (mapconcat #'shell-quote-argument (cons "pandoc" args) " "))))

(use-package
 markdown-mode
 :mode ("README\\.md\\'" . gfm-mode)
 ;; citar ships a markdown dispatch table built on pandoc's `[@key]' syntax, and
 ;; `citar-capf' completes keys inside it, so markdown buffers get the same citation
 ;; UI as org ones for free (see the citations block in the org section below)
 :hook
 ((markdown-mode . my/cite-local-bibliography)
  (markdown-mode . citar-capf-setup))
 ;; `C-c [' for parity with org and reftex, `C-c C-e' for parity with org's export
 ;; dispatch - both are unbound in `markdown-mode-map'
 :bind
 (:map
  markdown-mode-map
  ("C-c [" . citar-insert-citation)
  ("C-c C-e" . my/markdown-export-pdf))
 :config
 ;; pandoc for the `C-c C-c p' html preview too, so citations render there as well -
 ;; guarded because the defcustom otherwise picks the first markdown binary on $PATH
 (when (executable-find "pandoc")
   (setq markdown-command
         (concat
          "pandoc"
          " --from=markdown --to=html"
          " --standalone --mathjax --highlight-style=pygments"
          " --citeproc"
          " --quiet"))))

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
(use-package elsa :defer t :ensure t)
(use-package
 flymake-elsa
 :straight
 (flymake-elsa :type git :host github :repo "flymake/flymake-elsa")
 :hook (emacs-lisp-mode . flymake-elsa-load))
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
 ;; ACHTUNG: the session is started from the `lsp-pyright' hook below, not here -
 ;; that hook has to set the pixi interpreter *before* `lsp-deferred' runs, and
 ;; having both start it made the ordering depend on declaration order
 :commands
 lsp
 lsp-deferred
 :config
  ;; ignore .pixi folder
 (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pixi\\'")
 )
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
(advice-add
 (if (progn
       (require 'json)
       (fboundp 'json-parse-buffer))
     'json-parse-buffer
   'json-read)
 :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and
         (not test?) ;; for check lsp-server-present?
         (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
         lsp-use-plists
         (not (functionp 'json-rpc-connection)) ;; native json-rpc
         (executable-find "emacs-lsp-booster"))
        (progn
          (when-let
              ((command-from-exec-path
                (executable-find (car orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add
 'lsp-resolve-final-command
 :around #'lsp-booster--advice-final-command)

;; ACHTUNG: defined at top level rather than in `lsp-pyright''s `:config' because
;; directory-local variables are applied when the file is opened, which can precede
;; that package loading; `safe-local-variable' keeps `.dir-locals.el' from prompting
(defvar-local my/pixi-env-name "default"
  "Pixi environment name to use for lsp-pyright.  Set via .dir-locals.el to override.")
(put 'my/pixi-env-name 'safe-local-variable #'stringp)

(use-package
 lsp-pyright
 :custom
 (lsp-pyright-langserver-command
  (if (executable-find "basedpyright")
      "basedpyright"
    "pyright"))
 :config
 (defun my/lsp-pyright-set-pixi-python ()
   "Set lsp-pyright python interpreter to pixi env if present at project root.
Uses `my/pixi-env-name' (default: \"default\") to select the environment."
   (when-let* ((root (projectile-project-root))
               (pixi-python (expand-file-name
                             (format ".pixi/envs/%s/bin/python" my/pixi-env-name) root))
               ((file-executable-p pixi-python)))
     (setq-local lsp-pyright-python-executable-cmd pixi-python)))
 )

;; ty is still preview and its LSP feature surface lags basedpyright; keep it
;; disabled so only one type-checker LSP starts. Re-enable to A/B against pyright.
;; (use-package
;;  lsp-python-ty
;;  :straight nil
;;  :after lsp-mode
;;  :custom
;;  (lsp-python-ty-clients-server-command '("ty" "server")))

;; cython
;; ACHTUNG: `snakemake-mode' is derived from `python-mode', so `python-mode-hook' runs
;; in Snakefile buffers too and everything python-only has to opt out explicitly.  ruff
;; cannot even parse Snakefile syntax - "rule x:" is not python - so before this guard
;; existed, `ruff-format' errored on every Snakefile save.  Snakefiles get `snakefmt'
;; instead, hooked on `snakemake-mode' in the `reformatter' declaration below.
(defun my/python-mode-setup ()
  "Set up a genuine `python-mode' buffer, i.e. not a derived Snakefile one."
  (unless (derived-mode-p 'snakemake-mode)
    ;; formatting - ruff over stdin, see the `reformatter' declaration
    (ruff-check-fix-on-save-mode 1)
    (ruff-format-on-save-mode 1)
    ;; docstrings and filling
    (python-docstring-mode 1)
    (filladapt-mode 1)
    (setq fill-column 88)
    (display-fill-column-indicator-mode 1)
    ;; lsp last - the pixi interpreter has to be set before the session starts
    (my/lsp-pyright-set-pixi-python)
    (require 'lsp-pyright)
    (lsp-deferred)))

(use-package
 python
 :straight (:type built-in)
 :hook (python-mode . my/python-mode-setup))

(use-package cython-mode)

;; formatting
(use-package
 reformatter
 ;; ACHTUNG: the two ruff modes are enabled from `my/python-mode-setup', not hooked
 ;; here - `python-mode-hook' also runs in Snakefile buffers and ruff cannot parse
 ;; Snakefile syntax; those get `snakefmt' below instead
 :hook (snakemake-mode . snakefmt-on-save-mode)
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
  :args `("format" "-"))
 (reformatter-define snakefmt :program "snakefmt" :args `("-")))

(use-package python-docstring :demand t)

(use-package filladapt :demand t)

;;; Snakemake
(use-package snakemake-mode)

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

;;; just
(use-package just-mode)

(use-package justl)

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

;; citations
;; ACHTUNG: the bibliography variables are set buffer-locally below, so a report and its
;; `references.bib' are associated by living in the same folder - no `#+bibliography:'
;; keyword needed (adding one still works, it just adds more files).
(defun my/cite-local-bibliography ()
  "Point the bibliography variables at the .bib files next to the current file.

Looks in the buffer's own directory and, failing that, walks up until a directory
containing at least one .bib file is found.  Sets both
`org-cite-global-bibliography' (used by the org exporters) and `citar-bibliography'
(used by the citar completion UI, in org, markdown and LaTeX buffers alike) -
ACHTUNG: `citar-org-local-bib-files' returns the org-cite files *minus* the global
ones, so setting only the former would leave citar with nothing."
  (interactive)
  (when-let* ((filepath (buffer-file-name))
              (bib-dir
               (locate-dominating-file
                filepath
                (lambda (dir) (directory-files dir nil "\\.bib\\'" t))))
              (bib-filepaths (directory-files bib-dir t "\\.bib\\'" t)))
    (setq-local org-cite-global-bibliography bib-filepaths)
    (setq-local citar-bibliography bib-filepaths)))

(use-package
 oc
 :straight (:type built-in)
 :after (org)
 :hook (org-mode . my/cite-local-bibliography)
 ;; parity with reftex-citation in LaTeX buffers, shadowing the default
 ;; `org-agenda-file-to-front' (still reachable via M-x); `C-c C-x @' also works
 :bind (:map org-mode-map ("C-c [" . org-cite-insert))
 :config
 (setq org-cite-global-bibliography nil)
 (setq org-cite-insert-processor 'citar)
 (setq org-cite-follow-processor 'citar)
 (setq org-cite-activate-processor 'citar)
 ;; biblatex for latex/pdf export, citeproc (CSL) everywhere else
 (setq org-cite-export-processors '((latex biblatex) (t csl))))

;; the completion UI behind the org-cite processors above; the `citar' processor is
;; registered from citar's autoloads via `with-eval-after-load' on `oc', so no require
(use-package
 citar
 :after (org)
 ;; completion-at-point inside `[cite:@...]', picked up by company
 :hook ((org-mode LaTeX-mode) . citar-capf-setup)
 :custom (citar-bibliography nil))

;; pdf export
(use-package
 ox-latex
 :straight (:type built-in)
 :after (org)
 :config
 ;; prefer tectonic, which is an optional pixi dependency (see the `pdf' feature in
 ;; pixi.toml) - ACHTUNG: resolved once at startup, so restart emacs after switching
 ;; environments with `pixi run -e tex ...'
 (if (executable-find "tectonic")
     (progn
       ;; tectonic is xetex-based; telling org so drops the pdflatex-only `inputenc'
       ;; and `fontenc' in favour of `fontspec', without which the unicode citeproc
       ;; emits (en-dashes in page ranges) falls outside the T1 fonts
       (setq org-latex-compiler "xelatex")
       ;; tectonic fetches missing packages by itself and reruns the engine - and
       ;; biber - as many times as needed, so a single invocation is enough
       (setq org-latex-pdf-process '("tectonic --outdir %o %f")))
   ;; ACHTUNG: the default value runs the latex compiler three times but never calls
   ;; biber, so every biblatex citation comes out undefined - interleave a biber run
   ;; (`%o' is the output dir, `%b' the base name, `%f' the tex file)
   (setq org-latex-pdf-process
         '("%latex -interaction nonstopmode -output-directory %o %f"
           "biber --input-directory %o --output-directory %o %b"
           "%latex -interaction nonstopmode -output-directory %o %f"
           "%latex -interaction nonstopmode -output-directory %o %f")))
 ;; keep the .bbl out of the way too, `org-latex-logfiles-extensions' misses it
 (add-to-list 'org-latex-logfiles-extensions "bbl"))

;; needed by the `csl' export processor, i.e., non-latex export backends
(use-package citeproc :after (org))

;; (use-package poly-org)

;; python and jupyter
;;; the conda-forge `compilers' package sets CC/CFLAGS/CPPFLAGS/LDFLAGS (sysroot and
;;; include/lib paths included) on environment activation, so the module builds with no
;;; help from here - ACHTUNG: emacs-zmq downloads and statically links its own libzmq
;;; (4.3.1, pinned in its Makefile), so the conda `zeromq' in the environment is unused
(use-package zmq)
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
   (sort (directory-files-and-attributes my/jupyter-runtime-folder
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

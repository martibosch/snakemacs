;;;; .emacs.d
;;; description: emacs27 setup for Python with conda and Jupyter
;;; author: Martí Bosch <marti.bosch@protonmail.com>

;;;; bootstrap
;;; https://cestlaz.github.io/posts/using-emacs-1-setup/
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; https://ianyepan.github.io/posts/setting-up-use-package/
(eval-and-compile
  (setq use-package-always-ensure t use-package-expand-minimally t))
(require 'use-package)

;;; measure startup speed
;;; https://sqrtminusone.xyz/configs/emacs
(setq my/emacs-started nil)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "*** Emacs loaded in %s with %d garbage collections."
                                         (format "%.2f seconds" (float-time (time-subtract
                                                                             after-init-time
                                                                             before-init-time)))
                                         gcs-done))
          (setq my/emacs-started t))

(setq inhibit-startup-screen t)

;;; conda
(if (version< emacs-version "28")
    (defun string-replace (what with in)
      (replace-regexp-in-string (regexp-quote what) with in nil 'literal)))
(use-package
  conda
  :custom
  ;; (conda-anaconda-home (string-replace "/bin/mamba" "" (executable-find "mamba")))
  (conda-anaconda-home (expand-file-name "~/mambaforge/"))
  ;; (conda-env-home-directory (expand-file-name "~/mambaforge/"))
  (conda-env-subdirectory "envs")
  :config
  (conda-env-autoactivate-mode t)
  ;; (advice-add 'conda-env-activate 
  ;;             :after (lambda 
  ;; 		       (&rest 
  ;; 			_) 
  ;; 		       (setenv "EMACS_CONDA_ENV" conda-env-current-name) 
  ;; 		       (setenv "INIT_CONDA" "true"))) 
  ;; (advice-add 'conda-env-deactivate 
  ;;             :after (lambda 
  ;; 		       (&rest 
  ;; 			_) 
  ;; 		       (setenv "EMACS_CONDA_ENV" nil) 
  ;; 		       (setenv "INIT_CONDA" nil)))
  (conda-env-activate "emacs"))

;;;; config files
;;; custom config file location (rather than setting custom variables in init.el)
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;;; user interface
;;; no externals: remove scrollbars, menu bars, and toolbars
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; theme
(use-package
  zenburn-theme
  :config (load-theme 'zenburn t))

;;; fonts and icons
(use-package
  all-the-icons)

;;; highlighting
(use-package
  highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         ;; (vue-mode . highlight-indent-guides-mode)
         (LaTeX-mode . highlight-indent-guides-mode))
  :custom (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line))

(use-package
  rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package
  rainbow-mode
  :commands (rainbow-mode))

;; (use-package
;;   hl-todo
;;   :hook (prog-mode . hl-todo-mode))

;;; modeline
(use-package
  doom-modeline
  ;; :if (not (display-graphic-p))
  :init (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  :config (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-state-icon nil)
  (doom-modeline-mode 1)
  (progn 
    (require 'doom-modeline-segments)
    ;; https://martinralbrecht.wordpress.com/2020/08/23/conda-jupyter-and-emacs/
    (doom-modeline-def-segment conda-env
      "The current conda environment.  Works with `conda'." (when (bound-and-true-p
                                                                   conda-env-current-name) 
                                                              (propertize (format " |%s|"
                                                                                  conda-env-current-name)
                                                                          'face (if
                                                                                    (doom-modeline--active)
                                                                                    'mode-line
                                                                                  'mode-line-inactive)
                                                                          'help-echo (format
                                                                                      "Conda environment: %s"
                                                                                      conda-env-current-name)))))
  (doom-modeline-def-modeline 'main '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info conda-env)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  )

;;;; general settings
;;; key bindings
(use-package
  which-key
  :custom (which-key-popup-type 'frame)
  :config (which-key-mode)
  (which-key-setup-side-window-bottom)
  (set-face-attribute 'which-key-local-map-description-face nil
                      :weight 'bold))

;; (use-package
;;   avy
;;   :custom (avy-timeout-seconds 0.5)
;;   (avy-ignored-modes '(image-mode doc-view-mode pdf-view-mode exwm-mode))
;;   ;; :config (general-define-key :states '(normal motion)
;;   ;;                             "-" nil "--" #'avy-goto-char-2 "-=" #'avy-goto-symbol-1)
;;   )

;;; text editing

;; (use-package
;;   undo-tree
;;   :custom (undo-tree-visualizer-diff t)
;;   (undo-tree-visualizer-timestamps t)
;;   (undo-limit 6710886400)
;;   (undo-strong-limit 100663296)
;;   (undo-outer-limit 1006632960)
;;   :config (global-undo-tree-mode))

(use-package
  yasnippet-snippets)

(use-package
  yasnippet
  :config (setq yas-snippet-dirs `(,(concat (expand-file-name user-emacs-directory) "snippets")
                                   yasnippet-snippets-dir))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(use-package
  smartparens)


;;; project management
(use-package
  projectile
  :custom (projectile-project-search-path '("~/libraries" "~/data-science" "~/web-apps" "~/proposals"))
  :config (projectile-mode +1))
(use-package
  counsel-projectile
  :after (counsel projectile))

(use-package
  magit)

(use-package
  editorconfig
  :config (editorconfig-mode 1))

;;; completion
(use-package
  ivy
  :custom (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package
  counsel
  :after ivy
  :config (counsel-mode))

(use-package
  swiper
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
  (ivy-prescient-sort-commands
   '(:not swiper
          swiper-isearch
          ivy-switch-buffer
	  ;; ivy-resume
	  ;; ivy--restore-session
	  lsp-ivy-workspace-symbol
          dap-switch-stack-frame
	  ;; my/dap-switch-stack-frame
	  dap-switch-session
          dap-switch-thread
          counsel-grep
	  ;; counsel-find-file
	  counsel-git-grep
          counsel-rg
          counsel-ag
          counsel-ack
          counsel-fzf
          counsel-pt
          counsel-imenu
          counsel-yank-pop
          counsel-recentf
          counsel-buffer-or-recentf
          proced-filter-interactive
          proced-sort-interactive
          perspective-exwm-switch-perspective
	  ;; my/persp-ivy-switch-buffer-other-window
	  lsp-execute-code-action
          dired-recent-open))
  :config (ivy-prescient-mode +1)
  (prescient-persist-mode 1)
  ;; Do not use prescient in find-file
  (ivy--alist-set 'ivy-sort-functions-alist #'read-file-name-internal
                  #'ivy-sort-file-function-default))
(use-package
  company
  :custom (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config (global-company-mode))

(use-package
  company-box
  :if (display-graphic-p)
  :after (company)
  :hook (company-mode . company-box-mode))

;;; help
(use-package
  helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point helpful-function
                              helpful-command)
  :bind ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-c C-d" . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h F" . helpful-function)
  ("C-h C" . helpful-command))



;;;; programming

;;; config
(use-package
  dotenv-mode
  :mode "\\.env\\..*\\'")

;;; syntax checking
(use-package
  tree-sitter
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)))
(use-package
  flycheck
  ;; :custom (flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
  :config (global-flycheck-mode)

  ;; (add-hook 'evil-insert-state-exit-hook
  ;;           (lambda ()
  ;;             (if flycheck-checker
  ;;                 (flycheck-buffer))
  ;;             ))
  ;; (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*Flycheck errors*" eos)
  ;;       	 (display-buffer-reuse-window
  ;;       	  display-buffer-in-side-window)
  ;;       	 (side            . bottom)
  ;;       	 (reusable-frames . visible)
  ;;       	 (window-height   . 0.33)))
  )

;;; formatting
(use-package
  reformatter)
;; (use-package
;;   prettier
;;   :commands (prettier-prettify)
;;   :bind (:map js-mode-map web-mode-map typescript-mode-map
;;               ("rr" 'prettier-prettify)))

;;; Python
(use-package
  lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))
(use-package
  py-isort
  :hook (before-save . py-isort-before-save))
(use-package
  blacken
  :hook (python-mode . blacken-mode))

;;; Dockerfile
(use-package
  dockerfile-mode
  :mode "Dockerfile\\'"
  :config (add-hook 'dockerfile-mode 'smartparens-mode))

;;; YAML
(use-package
  yaml-mode
  :mode "\\.yml\\'"
  :config (add-hook 'yaml-mode-hook 'smartparens-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;; LaTeX
(use-package
  tex
  :defer t
  :ensure auctex
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
  (add-to-list 'TeX-command-list '("tectonic" "%`tectonic -X compile --synctex --keep-logs %t"
                                   TeX-run-command nil t))
  ;; ;; pdf view with eaf
  ;; (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
  ;; (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))

  ;; Do not run lsp within templated TeX files
  :hook (LaTeX-mode . (lambda ()
                        (unless (string-match "\.hogan\.tex$" (buffer-name))
                          (lsp))
                        (setq-local lsp-diagnostic-package
                                    :none)
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

(use-package
  gscholar-bibtex)


;;;; eaf
(use-package
  eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config (require 'eaf-browser)
  ;; (require 'eaf-org-previewer)
  ;; (require 'eaf-pdf-viewer)
  ;; (require 'eaf-camera)
  ;; (require 'eaf-jupyter)
  ;; (require 'eaf-terminal)
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_up "C-o" eaf-org-previewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ;; (eaf-browser-dark-mode t)
  ;; (eaf-get-theme-foreground-color "#ff8103")
  ;; (eaf-terminal-dark-mode nil)
  ;; (eaf-mindmap-dark-mode "follow")     ; default option
  ;; (eaf-pdf-dark-mode "ignore")         ; see below
  ) ;; unbind, see more in the Wiki

;;;; applications
;;; shell
(use-package
  vterm
  :custom (vterm-install t))

;;; .emacs.d --- emacs27 setup for Python with conda and Jupyter
;;; Commentary:
;;; author: Martí Bosch <marti.bosch@protonmail.com>

(setq inhibit-startup-screen t)

;; BEGIN custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file) 
  (load custom-file))
;; END custom-file

;; BEGIN use-package
;; https://cestlaz.github.io/posts/using-emacs-1-setup/
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))
;; https://ianyepan.github.io/posts/setting-up-use-package/
(eval-and-compile 
  (setq use-package-always-ensure t use-package-expand-minimally t))
(require 'use-package)
;; END use-package

;; BEGIN no-externals
;; Remove scrollbars, menu bars, and toolbars
					; when is a special form of "if", with no else clause, it reads:
					; (when <condition> <code-to-execute-1> <code-to-execute2> ...)
(when (fboundp 'menu-bar-mode) 
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) 
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) 
  (scroll-bar-mode -1))
;; END no-externals

;; BEGIN eaf
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
(use-package 
  eaf 
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t) 
  (eaf-browser-enable-adblocker t) 
  (browse-url-browser-function 'eaf-open-browser) 
  :config (require 'eaf-browser) 
  (require 'eaf-org-previewer) 
  (require 'eaf-pdf-viewer) 
  (require 'eaf-camera) 
  (require 'eaf-jupyter) 
  (defalias 'browse-web #'eaf-open-browser) 
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding) 
  (eaf-bind-key scroll_up "C-o" eaf-org-previewer-keybinding) 
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding) 
  (eaf-bind-key take_photo "p" eaf-camera-keybinding) 
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ;; (eaf-browser-dark-mode t)
  ;; (eaf-get-theme-foreground-color "#ff8103")
  ;; (eaf-terminal-dark-mode nil)
  ;; (eaf-mindmap-dark-mode "follow")     ; default option
  ;; (eaf-pdf-dark-mode "ignore")         ; see below
  ) ;; unbind, see more in the Wiki
;; END eaf

;; BEGIN doom-modeline
(use-package 
  doom-modeline 
  :init (doom-modeline-mode 1) 
  :config (progn 
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
  (setq doom-modeline-icon t doom-modeline-major-mode-icon t doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project doom-modeline-buffer-state-icon
        t doom-modeline-github nil doom-modeline-buffer-encoding nil doom-modeline-minor-modes nil) 
  (doom-modeline-def-modeline 'main '(bar workspace-name window-number modals matches buffer-info
                                          remote-host buffer-position word-count parrot
                                          selection-info conda-env) 
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes
                  input-method indent-info buffer-encoding major-mode process vcs checker)) 
  (if (bound-and-true-p imenu-list-mode-line-format) 
      (setq imenu-list-mode-line-format 
            '((:eval (doom-modeline-segment--bar)) 
              (:propertize "%b" 
                           face
                           mode-line-buffer-id)
              " " 
              (:eval (buffer-name imenu-list--displayed-buffer))
              " " 
              (:eval (doom-modeline-segment--matches)))))
  ;; :custom (doom-modeline-project-detection 'projectile)
  )
;; END doom-modeline

;; BEGIN zenburn-theme
(use-package 
  zenburn-theme 
  :config (load-theme 'zenburn t))
;; END zenburn-theme

;; BEGIN better-defaults
(use-package 
  better-defaults)
;; END better-defaults

;; BEGIN format-all
(use-package 
  format-all 
  :config (add-hook 'prog-mode-hook 'format-all-mode))
;; END format-all

;; BEGIN
(use-package 
  exec-path-from-shell 
  :ensure t 
  :config ;; (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
;; END

;; BEGIN vterm
(use-package 
  vterm 
  :custom (vterm-install t))
;; END vterm

;; BEGIN magit
(use-package 
  magit)
;; END magit

;; BEGIN projectile
(use-package 
  projectile 
  :init (projectile-mode +1) 
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))
;; END projectile

;; BEGIN yasnippet
(use-package 
  yasnippet 
  :config (progn 
            (use-package 
              yasnippet-snippets) 
            (yas-global-mode)))
;; END yasnippet

;; BEGIN elisp-format
(use-package 
  elisp-format 
  :hook (before-save . elisp-format-before-save) 
  :config (defun elisp-format-before-save () 
            "Apply elisp-format to any elisp-buffer before saving." 
            (interactive) 
            (when (eq major-mode 'emacs-lisp-mode) 
              (elisp-format-buffer))))
;; END elisp-format

;; BEGIN markdown-mode
(use-package 
  markdown-mode 
  :commands (markdown-mode gfm-mode) 
  :mode (("README\\.md\\'" . gfm-mode) 
         ("\\.md\\'" . markdown-mode) 
         ("\\.markdown\\'" . markdown-mode)) 
  :init (setq markdown-command "multimarkdown"))
;; END markdown-mode

;; BEGIN tex
(use-package 
  tex 
  :defer t 
  :ensure auctex 
  :config (setq TeX-auto-save t) 
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (add-to-list 'TeX-command-list '("tectonic" "%`tectonic -X compile --synctex --keep-logs %t"
  ;;                                  TeX-run-TeX nil t))
  (add-to-list 'TeX-command-list '("tectonic" "%`tectonic -X compile --synctex --keep-logs %t"
                                   TeX-run-command nil t)) 
  (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view)) 
  (add-to-list 'TeX-view-program-selection '(output-pdf "eaf")))
;; END tex

;; BEGIN reftex
(use-package 
  reftex 
  :commands turn-on-reftex 
  :config (setq reftex-plug-into-AUCTeX t))
;; END reftex

;; BEGIN dockerfile-mode
(use-package 
  dockerfile-mode 
  :mode ("Dockerfile\\'" . dockerfile-mode))
;; END dockerfile-mode

;; BEGIN conda
(use-package 
  conda 
  :config (progn (conda-env-initialize-interactive-shells)
                 ;; (conda-env-initialize-eshell)
                 (conda-env-autoactivate-mode t)) 
  :custom ((conda-anaconda-home (expand-file-name "~/mambaforge/"))))
;; ;; use `emacs` default env, where we installed emacs, black, clangdev, etc. (see environment.yml)
;; (unless (getenv "CONDA_DEFAULT_ENV")
;;   (conda-env-activate "emacs"))
;; END conda

;; BEGIN docker-compose-mode
(use-package 
  docker-compose-mode)
;; END docker-compose-mode

;; BEGIN terraform-mode
(use-package 
  terraform-mode)
;; END terraform-mode

;; ;; BEGIN company-terraform
;; (use-package
;;   company-terraform
;;   :init (company-terrafrom-init))
;; ;; END company-terraform

;; BEGIN web-mode
(use-package 
  web-mode 
  :mode (("\\.html$" .  web-mode) 
         ("\\.phtml$" .  web-mode)) 
  :init (setq web-mode-engines-alist '(("django" .
                                        "\\(/templates/\\(.*/\\)*.*\\.html\\'\\|/\\(_includes\\|_layouts\\)/\\(.*/\\)*.*\\.html\\'\\)"))))
;; END web-mode

;; BEGIN snakemake
(use-package 
  snakemake-mode)
;; END snakemake

;; BEGIN elpy
(use-package 
  elpy 
  :init (elpy-enable) 
  :config (progn (setenv "WORKON_HOME" (expand-file-name "~/mambaforge/envs")) 
                 (setq elpy-rpc-virtualenv-path (expand-file-name "~/mambaforge/envs/emacs")) 
                 (add-hook 'elpy-mode-hook (lambda () 
                                             (add-hook 'before-save-hook 'elpy-black-fix-code nil
                                                       t)))))
;; END elpy

;; BEGIN py-isort
(use-package 
  py-isort 
  :config (add-hook 'before-save-hook 'py-isort-before-save))
;; END py-isort

;; BEGIN ein
(use-package 
  ein 
  :config (setq ein:output-area-inlined-images t))
;; END ein

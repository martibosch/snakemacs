;;; .emacs.d --- emacs27 setup for Python with conda and Jupyter
;;; Commentary:
;;; author: Martí Bosch <marti.bosch@protonmail.com>

(setq inhibit-startup-screen t)

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

;; BEGIN doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (progn
            (require 'doom-modeline-segments)
            ;; https://martinralbrecht.wordpress.com/2020/08/23/conda-jupyter-and-emacs/
            (doom-modeline-def-segment conda-env
              "The current conda environment.  Works with `conda'."
              (when (bound-and-true-p conda-env-current-name)
                (propertize (format " |%s|" conda-env-current-name)
                            'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                            'help-echo (format "Conda environment: %s"
                                               conda-env-current-name)))))
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-state-icon t
        doom-modeline-github nil
        doom-modeline-buffer-encoding nil
        doom-modeline-minor-modes nil)

  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals
          matches buffer-info remote-host
          buffer-position word-count parrot selection-info
          conda-env)
    '(objed-state misc-info persp-name battery grip irc mu4e
                  gnus github debug lsp minor-modes input-method
                  indent-info buffer-encoding major-mode process vcs checker))
  (if (bound-and-true-p imenu-list-mode-line-format)
      (setq imenu-list-mode-line-format
            '((:eval
               (doom-modeline-segment--bar))
              (:propertize "%b" face mode-line-buffer-id)
              " "
              (:eval (buffer-name imenu-list--displayed-buffer))
              " "
              (:eval
               (doom-modeline-segment--matches)))))
  ;; :custom (doom-modeline-project-detection 'projectile)
  )
;; END doom-modeline

;; BEGIN zenburn-theme
(use-package zenburn-theme
  :config (load-theme 'zenburn t))
;; END zenburn-theme

;; BEGIN better-defaults
(use-package better-defaults)
;; END better-defaults

;; BEGIN format-all
(use-package format-all
  :config (add-hook 'prog-mode-hook 'format-all-mode))
;; END format-all

;; BEGIN vterm
(use-package vterm
  :custom (vterm-install t))
;; END vterm

;; BEGIN magit
(use-package magit)
;; END magit

;; BEGIN markdown-mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; END markdown-mode

;; BEGIN conda
(use-package conda
  :config (progn (conda-env-initialize-interactive-shells)
                 (conda-env-initialize-eshell)
                 (conda-env-autoactivate-mode t))
  :custom ((conda-anaconda-home (expand-file-name "~/anaconda3/"))))
;; use `emacs` default env, where we installed emacs, black, clangdev, etc. (see environment.yml)
(unless (getenv "CONDA_DEFAULT_ENV")
  (conda-env-activate "emacs"))
;; END conda

;; BEGIN jupyter
(use-package jupyter
  :commands (jupyter-run-server-repl
             jupyter-run-repl
             jupyter-server-list-kernels))
;; END jupyter

;; BEGIN org-babel
(use-package ob
  :ensure nil
  :config (progn
            ;; load more languages for org-babel
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((python . t)
               (shell . t)
               (latex . t)
               ; (ditaa . t)
               (C . t)
               (dot . t)
               ; (plantuml . t)
               (makefile . t)
               (jupyter . t)))          ; must be last

            (setq org-babel-default-header-args:sh    '((:results . "output replace"))
                  org-babel-default-header-args:bash  '((:results . "output replace"))
                  org-babel-default-header-args:shell '((:results . "output replace"))
                  ;; org-babel-default-header-args:jupyter-python '((:async . "yes")
                  ;;                                                (:session . "py")
                  ;;                                                (:kernel . "sagemath"))
                  )

            ;; (setq org-confirm-babel-evaluate nil
            ;;       org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
            ;;       org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

            ;; (add-to-list 'org-src-lang-modes (quote ("plantuml" . plantuml)))
  ))
;; END org-babel

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(conda magit format-all zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

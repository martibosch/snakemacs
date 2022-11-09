;;; look
;;; -*- lexical-binding: t; -*-

;; no startup screen
(setq inhibit-startup-screen t)

;; no externals: remove scrollbars, menu bars, and toolbars
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
   (doom-modeline-def-segment
    conda-env
    "The current conda environment.  Works with `conda'."
    (when (bound-and-true-p conda-env-current-name)
      (propertize (format " |%s|" conda-env-current-name) 'face (if (doom-modeline--active)
								    'mode-line
								  'mode-line-inactive) 'help-echo
								  (format "Conda environment: %s"
									  conda-env-current-name)))))
 (doom-modeline-def-modeline
  'main
  '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position
	word-count parrot selection-info conda-env)
  '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes
		input-method indent-info buffer-encoding major-mode process vcs checker)))

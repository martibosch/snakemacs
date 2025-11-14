;; -*- lexical-binding: t; -*-

;;; look

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

;;; modeline
(use-package
  doom-modeline
  ;; :if (not (display-graphic-p))
  :config
  (progn
    (setq doom-modeline-icon t
          doom-modeline-major-mode-icon t
          doom-modeline-major-mode-color-icon t
          doom-modeline-buffer-file-name-style 'truncate-upto-project
          doom-modeline-buffer-state-icon t
          doom-modeline-github nil
          doom-modeline-buffer-encoding nil
          doom-modeline-minor-modes nil)
    (doom-modeline-def-modeline
      'main
      '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position
	    word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug lsp minor-modes
		    input-method indent-info buffer-encoding major-mode process vcs)))
  (doom-modeline-mode 1)
  :custom (doom-modeline-project-detection 'projectile)
  )

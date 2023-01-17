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

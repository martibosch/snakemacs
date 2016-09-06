
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/my-loadpackages.el")
(add-hook 'after-init-hook '(lambda ()
			      (load "~/.emacs.d/my-noexternals.el")))

;; BASIC CUSTOMIZATION
(add-to-list 'load-path "~/.emacs.d/custom") ;; custom scripts path
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'zenburn t)

;; SCROLLING IN TERM
(if (eq window-system nil)
    (let ((map (make-sparse-keymap)))
      (define-key input-decode-map "\e[1;5A" [C-up])
      (define-key input-decode-map "\e[1;5B" [C-down])
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])))

;; LINUM
(global-linum-mode t) ;; enable line numbers globally
(require 'linum-off)

(put 'downcase-region 'disabled nil)
;; (server-start)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(TeX-command-list
;;    (quote
;;     (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
;;       (plain-tex-mode ams-tex-mode texinfo-mode)
;;       :help "Run plain TeX")
;;      ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
;;       (latex-mode doctex-mode)
;;       :help "Run LaTeX")
;;      ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
;;       (texinfo-mode)
;;       :help "Run Makeinfo with Info output")
;;      ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
;;       (texinfo-mode)
;;       :help "Run Makeinfo with HTML output")
;;      ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
;;       (ams-tex-mode)
;;       :help "Run AMSTeX")
;;      ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
;;       (context-mode)
;;       :help "Run ConTeXt once")
;;      ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
;;       (context-mode)
;;       :help "Run ConTeXt until completion")
;;      ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
;;      ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
;;      ("View" "(lambda() (let ((f \"%o\")) (find-file-other-window f) (doc-view-mode)))" TeX-run-discard-or-function t t :help "Run Viewer")
;;      ("Print" "%p" TeX-run-command t t :help "Print the file")
;;      ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
;;      ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
;;      ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
;;      ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
;;      ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
;;      ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
;;      ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
;;      ("Check" "lacheck %s" TeX-run-compile nil
;;       (latex-mode)
;;       :help "Check LaTeX file for correctness")
;;      ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
;;       (latex-mode)
;;       :help "Check LaTeX file for common mistakes")
;;      ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
;;      ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
;;      ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
;;      ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme zenburn sphinx-doc solarized-theme py-autopep8 magit flycheck elpy ein better-defaults auto-complete auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; conkeror

(if (executable-find "conkeror")
    (progn (setq browse-url-generic-program (executable-find "conkeror"))
           (setq browse-url-browser-function 'browse-url-generic)))

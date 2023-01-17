;;; python
;; ein
(use-package ein
  :config
  (setq ein:output-area-inlined-images t))

;; elpy
(use-package f)
(use-package elpy
  :after f
  :init
  (elpy-enable)
  :config
  ;; get conda home (based on conda.el)
  (defcustom conda-home-candidates
    '("~/.anaconda3" "~/miniconda3" "~/mambaforge" "~/anaconda" "~/miniconda" "~/mamba" "~/.conda")
    "Location of possible candidates for conda environment directory."
    :type '(list string)
    :group 'conda)
  (defcustom conda-anaconda-home
    (expand-file-name (or (getenv "ANACONDA_HOME")
                          (catch 'conda-catched-home
                            (dolist (candidate conda-home-candidates)
                              (when (f-dir? (expand-file-name candidate))
				(throw 'conda-catched-home candidate))))))
    "Location of your conda installation.

Iterate over default locations in CONDA-HOME-CANDIDATES, or read from the
ANACONDA_HOME environment variable."
    :type 'directory
    :group 'conda)
  (setenv "WORKON_HOME" (f-join conda-anaconda-home "envs")))

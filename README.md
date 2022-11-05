# .emacs.d

emacs setup for Python, conda, Jupyter and web

## Installation

This setup uses emacs 28. To get it working, you can follow the steps below:

1. Navigate to your home folder and clone the repo, and navigate to the (newly-created) `~/.emacs.d/` folder:

   ```bash
   cd ~
   git clone https://github.com/martibosch/.emacs.d/
   cd .emacs.d
   ```

2. Create a conda environment with the `environment.yaml` file included in this repository (which will install emacs as well as the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation - **note**: this only works in Linux and OSX), and then activate the conda environment:

   ```bash
   conda env create -f environment.yaml
   conda activate emacs
   ```

3. Install [EAF](https://github.com/emacs-eaf/emacs-application-framework) without its system, core and Python dependencies (since they are already taken care of via conda):

   ```bash
   git clone --depth=100 -b master \
   	https://github.com/emacs-eaf/emacs-application-framework.git \
   	~/.emacs.d/site-lisp/emacs-application-framework
   cd ~/.emacs.d/site-lisp/emacs-application-framework
   # the following commands will not be needed when Qt6 can be installed from conda
   git checkout 98ebfb9
   python install-eaf.py --ignore-sys-deps --ignore-core-deps --ignore-py-deps
   cd app/browser
   git pull --unshallow
   git checkout 89e3dee
   ```

4. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, libvterm may not be installed properly):

   ```bash
   emacs
   ```

5. From inside emacs, install all the icon fonts `M-x all-the-icons-install-fonts`

## Notes for future versions

### emacs28

- In Linux, there may be [an issue](https://github.com/conda-forge/emacs-feedstock/issues/60) with the emacs 28.1 from conda-forge
- The conda-forge emacs recipe [does not natively support json yet](https://github.com/conda-forge/emacs-feedstock/issues/59), which can prevent environment activation in conda.el. A workaround is to manually modify the `conda--call-json` function in the local `conda.el` file (at, e.g., `.emacs.d/elpa/conda-20220830.1547/conda.el`), changing:

```cl
(condition-case err
        (if (version< emacs-version "27.1")
            (json-read-from-string output)
          (json-parse-string output :object-type 'alist :null-object nil))
      (error "Could not parse %s as JSON: %s" output err))))
```

to

```cl
(condition-case err
        (if ;; (version< emacs-version "27.1")
            (progn
              (require 'json)
              (fboundp 'json-parse-string))
	    (json-parse-string output :object-type 'alist :null-object nil)
          (json-read-from-string output))
      (error "Could not parse %s as JSON: %s" output err))))
```

### PyQt6

PyQt6 is not yet available in conda-forge. Accordingly, EAF must be pinned to the `98ebfb9` commit, because [its child commit `c3ab6a6`](https://github.com/emacs-eaf/emacs-application-framework/commit/c3ab6a600d2fce562bd15c0e0249604d7974bbac). Once it is, the following changes to the setup can be done:

- eaf-pdf-viewer can be used as the main TeX PDF viewer
- vterm can be changed for the eaf-terminal (but vterm may actually better because of its copy mode)

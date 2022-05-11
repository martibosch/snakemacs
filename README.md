# .emacs.d

emacs setup for Python, conda, Jupyter and web

## Installation

This setup uses emacs 27. To get it working, you can follow the steps below:

1. Navigate to your home folder and clone the repo, and navigate to the (newly-created) `~/.emacs.d/` folder:

    ```bash
    cd ~
    git clone https://github.com/martibosch/.emacs.d/
    cd .emacs.d
    ```

2. Create a conda environment with the `environment.yml` file included in this repository (which will install emacs as well as the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation - **note**: this only works in Linux and OSX), and then activate the conda environment:

    ```bash
    conda env create -f environment.yml
    conda activate emacs
    ```

3. Install [EAF](https://github.com/emacs-eaf/emacs-application-framework) without its system, core and Python dependencies (since they are already taken care of via conda):

    ```bash
	git clone --depth=1 -b master \
		https://github.com/emacs-eaf/emacs-application-framework.git \
		~/.emacs.d/site-lisp/emacs-application-framework
	cd ~/.emacs.d/site-lisp/emacs-application-framework
	git checkout 03412e21
	python install-eaf.py --ignore-sys-deps --ignore-core-deps --ignore-py-deps
	```

3. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, libvterm may not be installed properly):

   ```bash
   emacs
   ```

4. From inside emacs, install all the icon fonts `M-x all-the-icons-install-fonts`


## Notes for future versions

### emacs 28

Emacs 28 is not yet available in conda-forge. Once it is, the following changes to the setup can be done:

* As noted in step 3, EAF must be pinned to the `03412e21` commit, because [its child commit `6a07d96`](https://github.com/emacs-eaf/emacs-application-framework/commit/6a07d96779bb9f96b7fc5afb07244841c360b1c3) uses the `json-parse-buffer` function of Emacs 28.
* The setup of the conda package uses the `string-replace` function of Emacs 28. A workaround function definition is used with prior emacs versions.

### PyQt6

PyQt6 is not yet available in conda-forge. Once it is, the following changes to the setup can be done:

* vterm can be changed for the eaf-terminal
* eaf-pdf-viewer can be used as the main TeX PDF viewer

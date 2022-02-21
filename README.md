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
	python install-eaf.py --ignore-sys-deps --ignore-core-deps --ignore-py-deps
	```

3. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, libvterm may not be installed properly):

   ```bash
   emacs
   ```

4. From inside emacs, install all the icon fonts `M-x all-the-icons-install-fonts`


### Ubuntu only: add a desktop shortcut (optional)

To avoid having to launch a terminal, activate a conda environment and run `emacs`, you can add a desktop shortcut with the following command:

```bash
bash -c 'export PATH="~/anaconda3/envs/emacs/bin:$PATH" && ~/anaconda3/envs/emacs/bin/emacs'
```


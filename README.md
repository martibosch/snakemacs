[![build](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml/badge.svg)](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/martibosch/snakemacs/main.svg)](https://results.pre-commit.ci/latest/github/martibosch/snakemacs/main)

# snakemacs

![snakemacs logo](https://github.com/martibosch/snakemacs/blob/main/snakemacs.svg)

emacs28 setup for Python with conda/mamba

## Installation

This setup uses emacs 28. To get it working, you can follow the steps below:

1. Navigate to your home folder and clone the repo, and navigate to the (newly-created) `~/.emacs.d/` folder:

   ```bash
   cd ~
   git clone https://github.com/martibosch/snakemacs ~/.emacs.d
   cd .emacs.d
   ```

2. Create a conda environment with the `environment.yml` file included in this repository (which will install emacs as well as the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation - **note**: this only works in Linux and OSX), and then activate the conda environment:

   ```bash
   # or use mamba instead of conda
   conda env create -f environment.yml
   conda activate emacs
   ```

3. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, `libvterm` may not be installed properly):

   ```bash
   emacs
   ```

4. From inside emacs, install all the icon fonts `M-x all-the-icons-install-fonts`

## TODO

### Python, conda/mamba and lsp worfklow

Each buffer with Python code (e.g., `.py` files and `.org` files) must have a pyright language server with the proper conda/mamba environment.

### Default environment activation

The [`conda--infer-env-from-buffer` function of conda.el](https://github.com/necaris/conda.el/blob/main/conda.el#L264-L274) activates the base environment, i.e., happens if `conda-activate-base-by-default` is set to `true` (altough it is `nil` by default) or if the "auto_activate_base" conda setting is set to `true`, which depends on the user's settings. However, snakemacs should instead use the `emacs` environment by default. This can either be acheived by (a) contributing to conda.el or (b) overriding the `codna--infer-env-from-buffer` function within this configuration (see https://stackoverflow.com/questions/15717103/preferred-method-of-overriding-an-emacs-lisp-function).

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

2. Use conda/mamba and the `environment.yml` file included in this repository to install emacs (as well as the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation - **note**: this only works in Linux and OSX), by either:

   a. installing the requirements in the `base` environment (or any other existing environment _provided that it is active at the time of running the commands_):

   ```bash
   # or use mamba instead of conda
   conda env update -f environment.yml
   ```

   b. create a new dedicated environment (e.g., named `emacs`) - **note** that in such case, you must always run emacs from within the dedicated environment:

   ```bash
   # or use mamba instead of conda
   conda env create -n emacs -f environment.yml
   conda activate emacs
   ```

3. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, `libvterm` may not be installed properly):

   ```bash
   emacs
   ```

4. From inside emacs, install all the icon fonts `M-x all-the-icons-install-fonts`

## Caveats

### Conda environments and IDE features for Python buffers

Each buffer with Python code (e.g., `.py` and `.ipynb` files) is associated to a conda/mamba environment. To ensure that IDE features are properly provided, each environment must have a set of packages installed, e.g., [pyright](https://github.com/microsoft/pyright) for static type checking, [black](https://github.com/psf/black) for formatting on save, [ruff](https://beta.ruff.rs) to check sytnax and style on the fly via flycheck, [jupytext](https://github.com/mwouts/jupytext) to convert Jupyter notebooks to Python scripts...

In order to ensure that these packages are included by default in all environments at the time of their creation, you can set up the [`create_default_packages`](https://conda.io/projects/conda/en/latest/user-guide/configuration/use-condarc.html#config-add-default-pkgs) options by adding the following (feel free to adapt the list of packages to suit your needs) to the `.condarc` file:

```
create_default_packages:
  - black
  - jupytext
  - pandoc
  - pre-commit
  - pyright
  - ruff
  - ipykernel
```

There remains nevertheless a caveat with this approach, as these packages will inevitably appear when exporting a conda environment (i.e., `conda env export`), adding dependencies that other users trying to reproduce your code may not need.

### Default environment activation

The [`conda--infer-env-from-buffer` function of conda.el](https://github.com/necaris/conda.el/blob/main/conda.el#L264-L274) activates the base environment, i.e., happens if `conda-activate-base-by-default` is set to `true` (altough it is `nil` by default) or if the "auto_activate_base" conda setting is set to `true`, which depends on the user's settings. However, snakemacs should instead use the `emacs` environment by default. This can either be acheived by (a) contributing to conda.el or (b) overriding the `codna--infer-env-from-buffer` function within this configuration (see https://stackoverflow.com/questions/15717103/preferred-method-of-overriding-an-emacs-lisp-function).

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

2. Use conda/mamba and the `environment.yml` file included in this repository to install the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation by either:

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

3. Install emacs in your system. Currently, only ubuntu builds are supported, but the idea is to eventually (i.e., after several issues are addressed, see the caveats section below) use [conda-forge's emacs](https://github.com/conda-forge/emacs-feedstock) for a fully conda-based cross-platform setup.

4. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, `libvterm` may not be installed properly):

   ```bash
   emacs
   ```

5. From inside emacs, install all the icon fonts `M-x nerd-icons-install-fonts`

## Caveats

### Issues with emacs-jupyter and zmq

Several [emacs-jupyter](https://github.com/emacs-jupyter) users (including myself) have encountered an error of the form:

```
zmq.error.ZMQError: Address already in use
```

when trying to launch a Jupyter REPL, i.e., `M-x jupyter-run-repl`. In my case, I managed to avoid it following [the hack suggested by deepestthought42](https://github.com/emacs-jupyter/jupyter/issues/464#issuecomment-1937499393).

### Issues with conda-forge emacs

There are currently two main issues with the emacs from conda-forge, namely [an error with newer Linux versions](https://github.com/conda-forge/emacs-feedstock/issues/63) and [the lack of native JSON and native compilation](https://github.com/conda-forge/emacs-feedstock/issues/59), which slow down several features such as lsp. This may be addressed once [the v29.1 branch](https://github.com/conda-forge/emacs-feedstock/pull/73) is merged.

### Conda environments and IDE features for Python buffers

Each buffer with Python code (e.g., `.py` and `.ipynb` files) is associated to a conda/mamba environment. To ensure that IDE features are properly provided, each environment must have a set of packages installed, e.g., [python-lsp-ruff](https://github.com/python-lsp/python-lsp-ruff) for language server protocol (LSP) linting with [ruff](https://docs.astral.sh/ruff), also checking sytnax and style on the fly via flycheck, [jupytext](https://github.com/mwouts/jupytext) to convert Jupyter notebooks to Python scripts...

In order to ensure that these packages are included by default in all environments at the time of their creation, you can set up the [`create_default_packages`](https://conda.io/projects/conda/en/latest/user-guide/configuration/use-condarc.html#config-add-default-pkgs) options by adding the following (feel free to adapt the list of packages to suit your needs) to the `.condarc` file:

```
create_default_packages:
  - ipykernel
  - jupytext
  - nodejs
  - pandoc
  - pre-commit
  - python-lsp-ruff
  - ruff
```

There remains nevertheless a caveat with this approach, as these packages will inevitably appear when exporting a conda environment (i.e., `conda env export`), adding dependencies that other users trying to reproduce your code may not need.

### Default environment activation

The [`conda--infer-env-from-buffer` function of conda.el](https://github.com/necaris/conda.el/blob/main/conda.el#L264-L274) activates the base environment, i.e., happens if `conda-activate-base-by-default` is set to `true` (altough it is `nil` by default) or if the "auto_activate_base" conda setting is set to `true`, which depends on the user's settings. However, snakemacs should instead use the `emacs` environment by default. This can either be acheived by (a) contributing to conda.el or (b) overriding the `codna--infer-env-from-buffer` function within this configuration (see https://stackoverflow.com/questions/15717103/preferred-method-of-overriding-an-emacs-lisp-function).

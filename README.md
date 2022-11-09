[![build](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml/badge.svg)](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml)

# snakemacs

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

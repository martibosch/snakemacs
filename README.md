# .emacs.d

emacs setup for data-centric Python, C/C++ for CMake projects, web development, LaTeX and Markdown

## Installation

This setup uses emacs 27. The emacs packages are managed through [Cask](http://cask.readthedocs.io/). Follow [their instructions to install it in your system](https://cask.readthedocs.io/en/latest/guide/installation.html).

1. Navigate to your home folder and clone the repo, and navigate to the (newly-created) `~/.emacs.d/` folder:

    ```bash
    cd ~
    git clone https://github.com/martibosch/.emacs.d/
    cd .emacs.d
    ```

2. Create a conda environment with the `environment.yml` file included in this repository, which will install emacs as well as the Python and C/C++ dependencies required for autocompletion, syntax checking and function documentation (**note**: this only works in Linux and OSX):

    ```bash
    conda env create -f environment.yml
    ```
    
3. Install the emacs packages via Cask (you might check the `Cask` file of this repo to see the packages that are used within this configuration):

    ```bash
    cask install
    ```

4. You can now activate the conda environment and run emacs:

    ```bash
    conda activate emacs
    emacs
    ```
    
### Ubuntu only: add a desktop shortcut (optional)

To avoid having to launch a terminal, activate a conda environment and run `emacs`, you can add a desktop shortcut with the following command:

```bash
bash -c 'export PATH="~/anaconda3/envs/emacs/bin:$PATH" && ~/anaconda3/envs/emacs/bin/emacs'
```


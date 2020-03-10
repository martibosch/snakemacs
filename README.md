# .emacs.d

emacs26 setup for scientific Python (and LaTeX)

## Installation

This setup is designed for emacs 26. The emacs packages are managed through [Cask](http://cask.readthedocs.io/). Follow [their instructions to install it in your system](https://cask.readthedocs.io/en/latest/guide/installation.html).

Navigate to your home folder and clone the repo:

```bash
cd ~
git clone https://github.com/martibosch/.emacs.d/
```

Then navigate to the `~/.emacs.d/` folder and install the emacs packages via Cask

```bash
cd .emacs.d
cask install
```

You might check the `Cask` file of this repo to see the packages that are used within this configuration.

When editing Python files or Jupyter notebooks, you might get a popup "Automatically install the RPC dependencies from PyPI (needed for completion, autoformatting and documentation)?". Answer "Yes".

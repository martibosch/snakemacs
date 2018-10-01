# .emacs.d

emacs25 setup for Python.

## System Requirements

This setup requires emacs >= 25.1.2. The interactive emacs Python shell will use the `ipython` interpreter if you have it installed, otherwise it will use default python interpreter.

The emacs packages are managed through [Cask](http://cask.readthedocs.io/). Follow their instruction to install it in your system.

## Installation

Navigate to your home folder and clone the repo:

```bash
cd ~
git clone https://github.com/martibosch/.emacs.d/
```

Then install the emacs packages via Cask

```bash
cask install
```

### Python Package Dependencies

The setup (specially `elpy`) has functionalities that borrow from certain Python packages that can be obtained via `pip` as:

```bash
pip install jedi flake8 importmagic autopep8 yapf
```

To manage Python packages I strongly suggest that you use virtual environments with Anaconda. If you do not have or know Anaconda, you might go to [First Step: Download Anaconda part](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) of [this blog post](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) and install it in your computer. It is an open source Python distribution that comes with most of the data science packages that you will need.

In order to be able to preview Markdown documents this setup uses the `pandoc` converter, which can be installed as:

```bash
pip install pandoc
```

### Emacs Packages:

Check the `Cask` file of this repo to see the packages that are used within this configuration.

# .emacs.d

emacs25 setup for data-centric Python, web development and C/C++ for CMake projects

## Installation

This setup requires emacs >= 25.1.2. The emacs packages are managed through [Cask](http://cask.readthedocs.io/). Follow [their instructions to install it in your system](https://cask.readthedocs.io/en/latest/guide/installation.html).

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

### C/C++ Dependencies

In order to provide C/C++ autocompletion, syntax checking and function documentation, [`irony-server`](https://github.com/Sarcasm/irony-mode) must be installed. This requires the packages [CMake](http://www.cmake.org/) and [libclang](http://clang.llvm.org/doxygen/group__CINDEX.html), which can be installed as in:

``` bash
sudo apt install cmake libclang-dev
```

Then, `irony-server` can be installed within emacs as in:

```
M-x irony-install-server RET
```

The [clang-format emacs package](https://github.com/sonatard/clang-format) for automated C/C++ style formatting requires [clang-format](https://clang.llvm.org/docs/ClangFormat.html) to be installed within your system. This can be done via [npm](https://www.npmjs.com/) as in:

``` bash
npm install -g clang-format  # also `sudo apt install clang-format` in Ubuntu
```


### Python Dependencies

The interactive emacs Python shell will use the [ipython](https://ipython.org/) interpreter if you have it installed, otherwise it will use default python interpreter. 

The setup (specially [elpy](https://github.com/jorgenschaefer/elpy)) has functionalities that borrow from certain Python packages that can be obtained via [pip](https://pypi.org/project/pip/) as in:

```bash
pip install jedi flake8 importmagic autopep8 yapf
```

To manage Python packages I strongly suggest that you use virtual environments with Anaconda. If you do not have or know Anaconda, you might go to [First Step: Download Anaconda part](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) of [this blog post](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) and install it in your computer. It is an open source Python distribution that comes with most of the data science packages that you will need.

In order to be able to preview Markdown documents this setup uses the [pandoc](https://pandoc.org/) converter, which can be installed as:

```bash
pip install pandoc
```

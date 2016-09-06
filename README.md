# .emacs.d

emacs24.5 setup for Python. Use: 

```
git clone -b <branch> https://github.com/martibosch/.emacs.d/
```

where `<branch>` might be:
* `data` for a data analysis setup with `Jupyter` Notebooks, and using `Emacs` with the `X` window system
* `web` for a web development setup with `Django`, and using `Emacs` in the terminal

See the packages that each branch includes in the `Packages` section below.

## System Requirements

This setup requires Emacs >= 24.5. If you use `APT` this version can be obtained as follows:

```bash
sudo add-apt-repository ppa:ubuntu-elisp/ppa
sudo apt-get update
sudo apt-get install emacs-snapshot
```     

The package `magit` requires Git >= 1.9.4. If you use `APT` this version can be obtained as follows:

```bash
sudo add-apt-repository ppa:git-core/ppa
sudo apt-get update
sudo apt-get install git
```

The `data` setup requires the `ipython` interpreter, which if you use `APT` can be obtained with:

```bash
sudo apt-get install ipython
```

### Python Package Dependencies

The setup borrows from the following utilities that require certain python packages that can be obtained via `pip` as:

```bash
pip install pep8 # for automatic styling
pip install jedi # for autocompletion
```

#### Data Science Environment: Anaconda

If you are using the `data` setup, and you do not have or know Anaconda, you might go to [First Step: Download Anaconda part](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) of [this blog post](http://martibosch.github.io/blog/2016/08/27/how-to-do-your-machine-learning-assignments-in-10-mins.html#first-step-download-anaconda) and install it in your computer. It is an open source Python distribution that comes with most of the data science packages that you will need.

If you do install Anaconda and are working on its default `anaconda` virutal environment, you won't need to install `ipython` nor `pep8` nor `jedi`.

### Base Packages:

The following packages are commonly used by all the branches of the configuration:

* `bash-completion`
* `better-defaults`
* `elpy`
* `flycheck`
* `magit`
* `py-autopep8`
* `sphinx-doc`
* `yasnippet`
* `zenburn-theme`

It is useful to check that the Python libraries required for `elpy` are installed to get a better coding experience

### Packages for Data Analysis

These packages will be installed if using the `data` branch:

* `auctex`
* `auto-complete`
* `ein`
* `exec-path-from-shell`
* `solarized-theme`

### Packages for Web Development

These packages will be installed if using the `web` branch:

* `pony-mode`
* `scss-mode`
* `web-mode`

## Other

You might emacs-like browse with [Conkeror](https://github.com/retroj/conkeror). If you use Debian based systems (i.e. Ubuntu, Linux Mint...), follow these steps in order to install it:

1. Check your Debian version with `cat /etc/debian_version`

2. Go to [http://noone.org/conkeror-nightly-debs/], and add the two lines to `/etc/apt/sources.list` that correspond to your Debian version

3. Run:

```bash
sudo apt-get update
```

and if you encounter any untrusted keys (of the form `303A7CB080379429`), add them to your `APT` key ring as in:

```bash
sudo apt-key adv --keyserver pgp.uni-mainz.de --recv-keys 303A7CB080379429
```

and you will then be ready to install Conkeror:

```bash
sudo apt-get update
sudo apt-get install conkeror conkeror-spawn-process-helper
```

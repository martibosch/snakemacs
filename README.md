# .emacs.d

emacs25 setup for Python. Use: 

```
git clone https://github.com/martibosch/.emacs.d/
```

## System Requirements

This setup requires Emacs >= 25.1.2. If you use `aptitude`, you might install an appropriate version as in:

```bash
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs25
```

The interactive emacs Python shell will use the `ipython` interpreter if you have it installed, otherwise it will use default python interpreter.

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

The following packages will be automatically installed by the emacs package manager:

* `auctex`
* `auto-complete`
* `better-defaults`
* `ein`
* `elpy`
* `exec-path-from-shell`
* `flycheck`
* `magit`
* `markdown-mode`
* `py-autopep8`
* `readline-complete`
* `rvm`
* `scss-mode`
* `solarized-theme`
* `sphinx-doc`
* `web-mode`
* `yasnippet`
* `zenburn-theme`

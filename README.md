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


## Other

You might emacs-like browse with [Conkeror](https://github.com/retroj/conkeror). If you use Debian based systems (i.e. Ubuntu, Linux Mint...), follow these steps in order to install it:

1. Check your Debian version with `cat /etc/debian_version`

2. Go to [http://noone.org/conkeror-nightly-debs/], and add the two lines to `/etc/apt/sources.list` that correspond to your Debian version

3. Run `sudo apt-get update` and if you encounter any untrusted keys (of the form `303A7CB080379429`), add them to your `APT` key ring as in `sudo apt-key adv --keyserver pgp.uni-mainz.de --recv-keys 303A7CB080379429`

4. Now update the `APT` sources and you are ready to install Conkeror:

```bash
sudo apt-get update
sudo apt-get install conkeror conkeror-spawn-process-helper
```

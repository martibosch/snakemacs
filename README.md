# .emacs.d

emacs24.5 setup for Python mostly. The branches `data` and `web` might be used. See the packages that each branch 

## System Requirements

This setup requires Emacs >= 24.5. The package can be installed from the source downloaded at `foo`

The package `magit` requires Git >= 1.9.4. If you use `aptitude` this version can be obtained as follows:

```
sudo add-apt-repository ppa:git-core/ppa -y
sudo apt-get update
sudo apt-get install git
```

## Packages:

### Base Packages:

The following packages are commonly used by all the branches of the configuration:

* better-defaults
* elpy
* flycheck
* magit
* py-autopep8
* sphinx-doc
* yasnippet
* zenburn-theme

It is useful to check that the Python libraries required for `elpy` are installed to get a better coding experience

### Packages for Data Analysis

These packages will be installed if working on `data` branch:

* auto-complete
* ein

### Packages for Web Development

These packages will be installed if working on `web` branch:

* pony-mode
* web-mode

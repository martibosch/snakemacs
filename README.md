[![build](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml/badge.svg)](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/martibosch/snakemacs/main.svg)](https://results.pre-commit.ci/latest/github/martibosch/snakemacs/main)

# snakemacs

![snakemacs logo](https://github.com/martibosch/snakemacs/blob/main/snakemacs.svg)

emacs30 setup for Python with [pixi](https://pixi.sh)

## Installation

This setup uses emacs 30 and pixi. The only requirement is to [install pixi](https://pixi.sh/latest/installation), then you can follow the steps below:

1. Navigate to your home folder and clone the repository, and navigate to the (newly-created) `~/.emacs.d/` folder:

   ```bash
   cd ~
   git clone https://github.com/martibosch/snakemacs ~/.emacs.d
   cd .emacs.d
   ```

   Alternatively, you can clone this repository into any directory and use [plexus/chemacs2](https://github.com/plexus/chemacs2) to set up `snakemacs` as a (potentially default) profile.

2. Run emacs for the first time from the shell so that all packages can be installed (if you do not run it from the shell, `libvterm` may not be installed properly):

   ```bash
   pixi run emacs
   ```

3. From inside emacs, install all the icon fonts `M-x nerd-icons-install-fonts`

## Caveats

### Issues with emacs-jupyter and zmq

Several [emacs-jupyter](https://github.com/emacs-jupyter) users (including myself) have encountered an error of the form:

```
zmq.error.ZMQError: Address already in use
```

when trying to launch a Jupyter REPL, i.e., `M-x jupyter-run-repl`. In my case, I managed to avoid it following [the hack suggested by deepestthought42](https://github.com/emacs-jupyter/jupyter/issues/464#issuecomment-1937499393).

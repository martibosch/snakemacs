[![build](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml/badge.svg)](https://github.com/martibosch/snakemacs/actions/workflows/build.yaml)
[![docs](https://readthedocs.org/projects/snakemacs/badge/?version=latest)](https://snakemacs.readthedocs.io/en/latest/)
[![pre-commit.ci status](https://results.pre-commit.ci/badge/github/martibosch/snakemacs/main.svg)](https://results.pre-commit.ci/latest/github/martibosch/snakemacs/main)

# snakemacs

![snakemacs logo](https://github.com/martibosch/snakemacs/blob/main/docs/assets/snakemacs.svg)

emacs 30 setup for Python and Jupyter with [pixi](https://pixi.sh)

![snakemacs example screencast](docs/assets/example-screencast.gif)

## Features

- Jupyter-like mode using plain-text Python buffers with [code-cells](https://github.com/astoff/code-cells.el), [emacs-jupyter](https://github.com/emacs-jupyter/jupyter) and [jupytext](https://github.com/mwouts/jupytext) (see the blog post ["Jupyter in the Emacs universe"](https://martibosch.github.io/jupyter-emacs-universe) for more details), with [pixi-kernel](https://github.com/renan-r-santos/pixi-kernel) to run Jupyter kernels with the per-directory pixi environments.
- _Fast_ (with [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster)) IDE features using [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with [basedpyright](https://github.com/detachhead/basedpyright) and [ruff](https://github.com/astral-sh/ruff), wired automatically to the pixi environment of the project you are editing.
- Org and LaTeX for writing things up: citations from a `references.bib` next to your document, and PDF export through [tectonic](https://tectonic-typesetting.github.io) or a system TeX Live.

## Quickstart

The only requirement is to [install pixi](https://pixi.sh/latest/installation):

```bash
git clone https://github.com/martibosch/snakemacs ~/.emacs.d
cd ~/.emacs.d
pixi run emacs
```

Run that first launch from a shell so that `libvterm` is installed properly. See the [installation guide](https://snakemacs.readthedocs.io/en/latest/installation) for [chemacs2](https://github.com/plexus/chemacs2) profiles, the optional `tex` and `docs` environments, and ways to launch emacs afterwards.

## Documentation

Full documentation at **[snakemacs.readthedocs.io](https://snakemacs.readthedocs.io/en/latest/)**:

- [Dependencies](https://snakemacs.readthedocs.io/en/latest/dependencies) - what pixi provides, what you install yourself, and where the pixi story ends
- [Notebooks](https://snakemacs.readthedocs.io/en/latest/notebooks) and [environments and kernels](https://snakemacs.readthedocs.io/en/latest/repl-and-kernels) - the Jupyter workflow
- [IDE features](https://snakemacs.readthedocs.io/en/latest/ide) - LSP, basedpyright, ruff
- [Org](https://snakemacs.readthedocs.io/en/latest/org) and [LaTeX](https://snakemacs.readthedocs.io/en/latest/latex) - citations and PDF export
- [Troubleshooting](https://snakemacs.readthedocs.io/en/latest/troubleshooting)

## See also

- ["Jupyter in the Emacs universe"](https://martibosch.github.io/jupyter-emacs-universe) for more details about alternative Jupyter notebook emulations within emacs.
- Many concepts of this setup are inspired by the excellent post ["Replacing Jupyter Notebook with Org Mode"](https://sqrtminusone.xyz/posts/2021-05-01-org-python) by Pavel Korytov. In fact, several functions such as `M-x my/jupyter-connect-repl` are essentially copied from there.

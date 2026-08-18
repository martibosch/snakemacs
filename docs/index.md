______________________________________________________________________

## snakemacs

emacs 30 setup for Python and Jupyter with [pixi](https://pixi.sh).

![snakemacs example screencast](assets/example-screencast.gif)

## Features

- A Jupyter-like workflow on **plain-text Python buffers**, using
  [code-cells](https://github.com/astoff/code-cells.el),
  [emacs-jupyter](https://github.com/emacs-jupyter/jupyter) and
  [jupytext](https://github.com/mwouts/jupytext), with
  [pixi-kernel](https://github.com/renan-r-santos/pixi-kernel) so that each project
  gets the kernel of its own pixi environment. See the blog post ["Jupyter in the
  Emacs universe"](https://martibosch.github.io/jupyter-emacs-universe) for the
  reasoning behind it.
- _Fast_ IDE features - [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with
  [basedpyright](https://github.com/detachhead/basedpyright) and
  [ruff](https://github.com/astral-sh/ruff), accelerated by
  [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) - wired
  automatically to the pixi environment of the project you are editing.
- **Org and LaTeX** for writing things up: citations from a `references.bib` sitting
  next to your document, and PDF export through either
  [tectonic](https://tectonic-typesetting.github.io) or a system TeX Live.

## Where to start

:::{card} Installation
:link: installation.md

Install pixi, clone the repository, run emacs once. Two steps.
:::

:::{card} Dependencies
:link: dependencies.md

What pixi handles, what it does not, and why. Read this before wondering why
something is missing.
:::

:::{card} Notebooks
:link: python-jupyter/notebooks.md

The flagship workflow: editing notebooks as Python scripts.
:::

## See also

- ["Jupyter in the Emacs universe"](https://martibosch.github.io/jupyter-emacs-universe),
  on alternative Jupyter notebook emulations within emacs.
- ["Replacing Jupyter Notebook with Org Mode"](https://sqrtminusone.xyz/posts/2021-05-01-org-python)
  by Pavel Korytov, which inspired many concepts of this setup. Several functions,
  such as `my/jupyter-connect-repl`, are essentially taken from there.

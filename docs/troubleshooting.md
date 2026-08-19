---
title: Troubleshooting
---

## `libvterm` was not installed

Launch emacs from a shell for the first run - `pixi run emacs` - rather than from a
desktop launcher. See [Installation](installation.md).

## `M-x jupyter-run-repl` finds no kernel for my project

The project's pixi environment needs `ipykernel`. From the project directory:

```bash
pixi add ipykernel
```

See [Environments and kernels](python-jupyter/repl-and-kernels.md).

## The REPL uses the wrong pixi environment

The "Python (Pixi)" kernel always resolves to the workspace's *default* environment.
Use `PIXI_KERNEL_DEFAULT_ENVIRONMENT`, or attach to a manually started kernel with
`my/jupyter-connect-repl` - both described in
[Environments and kernels](python-jupyter/repl-and-kernels.md).

## LSP uses the wrong interpreter

`lsp-pyright` infers the interpreter from `./.pixi/envs/default/bin/python` relative
to the projectile project root. Point it elsewhere with `my/pixi-env-name` in
`.dir-locals.el`; see [IDE features](python-jupyter/ide.md).

## Citations export as `?` or "undefined citation"

Your PDF pipeline is not running biber. If you expect tectonic to be used, check that
it is actually on `$PATH` - `M-: (executable-find "tectonic")` - and restart emacs
after switching pixi environments, since the choice is made once at startup. See
[LaTeX](writing/latex.md).

## `Found biblatex control file version ... expected version ...`

The `biber` version does not match the biblatex that produced the document. If you
are using the `tex` environment, the pin in `pixi.toml` has drifted from the tectonic
bundle; see [Dependencies](dependencies.md).

## citar shows no references

The bibliography is resolved when `org-mode` or `markdown-mode` starts. If the `.bib`
file appeared afterwards, run `M-x my/cite-local-bibliography` or revert the buffer.
See [Org](writing/org.md) and [Markdown](writing/markdown.md).

## `emacs-jupyter` complains that the ZMQ module is missing

Build it from the workspace:

```bash
pixi run zmq-build
```

This compiles against the locked environment and needs no extra flags; see
[Installation](installation.md).

:::{warning}
If you launched emacs as a [pixi global tool](https://pixi.sh/latest/global_tools/introduction)
rather than through the workspace, the module is built against the *global*
environment instead, and the two can drift. Build it through
`pixi run zmq-build` so the module matches the environment that emacs actually runs
in.
:::

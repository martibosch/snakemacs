______________________________________________________________________

## title: IDE features

snakemacs uses [lsp-mode](https://github.com/emacs-lsp/lsp-mode) with
[basedpyright](https://github.com/detachhead/basedpyright) for type checking and
completion, and [ruff](https://github.com/astral-sh/ruff) for linting and formatting.

## The interpreter follows the pixi environment

IDE features are connected automatically to the per-directory pixi environment:
`lsp-pyright` infers the interpreter from `./.pixi/envs/default/bin/python`, relative
to the project root as detected by
[projectile](https://github.com/bbatsov/projectile).

To point a project at a non-default environment, set `my/pixi-env-name` in a
`.dir-locals.el` at the project root:

```elisp
((python-mode . ((my/pixi-env-name . "your-env-name"))))
```

## Formatting

The [ruff language server](https://docs.astral.sh/ruff/editors/#language-server-protocol)
can format over LSP, but that assumes a real file on disk. Since notebooks are edited
as plain-text Python buffers here (see [Notebooks](notebooks.md)), ruff would be
handed a script where it expects a notebook.

snakemacs therefore disables `lsp-format-buffer` and `lsp-organize-imports` in
`python-mode` and runs ruff through
[reformatter](https://github.com/purcell/emacs-reformatter) instead, piping the buffer
contents over standard input. Two on-save hooks are active:

- `ruff-check-fix-on-save-mode` - applies `ruff check --fix`
- `ruff-format-on-save-mode` - applies `ruff format`

:::{note}
Neither passes `--stdin-filename`, deliberately. Doing so makes ruff fail on
notebooks, since in emacs those buffers are Python scripts in percent format rather
than `.ipynb` files.
:::

## Speed

[emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) is an external,
optional dependency that sits between emacs and the language server and converts JSON
to bytecode, removing most of the parsing overhead. It is the single largest
contributor to how responsive this setup feels. See [Dependencies](../dependencies.md)
for how to install it.

`gc-cons-threshold` and `read-process-output-max` are also raised well above their
defaults, which matters for LSP throughput.

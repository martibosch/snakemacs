______________________________________________________________________

## title: Environments and kernels

snakemacs ships [pixi-kernel](https://github.com/renan-r-santos/pixi-kernel), so the
"Python (Pixi)" kernel resolves to the pixi environment of whatever project directory
you are in. There is no per-project kernel registration to maintain.

## The kernel needs `ipykernel`

For a project's pixi environment to be usable as a kernel, that environment must
contain `ipykernel`:

```bash
pixi add ipykernel
```

Run this from the project directory, not from the snakemacs one. Without it,
`M-x jupyter-run-repl` will not find a usable kernel for the project.

## Using a non-default environment

[pixi-kernel supports selecting an environment](https://github.com/renan-r-santos/pixi-kernel?tab=readme-ov-file#pixi-environments),
but snakemacs does not currently expose a user interface for it. Two workarounds:

- Set `PIXI_KERNEL_DEFAULT_ENVIRONMENT` to the environment you want before launching
  emacs.

- Open a terminal with `M-x vterm` and run, from the project directory:

  ```bash
  pixi run -e <your-environment> jupyter kernel
  ```

  This prints the path to a connection file. Then run `M-x my/jupyter-connect-repl`
  and select that connection file - it is offered in the minibuffer - to get a REPL
  buffer attached to that environment.

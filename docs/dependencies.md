______________________________________________________________________

## title: Dependencies

Almost everything snakemacs needs is declared in `pixi.toml` and pinned in
`pixi.lock`. This page is about the boundary: what pixi handles, what it cannot, and
what you have to install yourself.

## What pixi provides

| Dependency                                                            | Environment | Needed for                                      |
| --------------------------------------------------------------------- | ----------- | ----------------------------------------------- |
| `emacs` 30                                                            | default     | everything                                      |
| `jupyter`, `ipykernel`, `pixi-kernel`, `jupytext`                     | default     | notebooks and the Jupyter REPL                  |
| `basedpyright`                                                        | default     | Python type checking and completion             |
| `ruff`, `snakefmt`                                                    | default     | formatting and linting, driven by `reformatter` |
| `just`                                                                | default     | running recipes from `justl` and `just-mode`    |
| `compilers`, `cmake`, `libtool`, `autoconf`, `automake`, `pkg-config` | default     | building the `emacs-zmq` module                 |
| `tectonic`, `biber`                                                   | `tex`       | org and LaTeX export to PDF                     |
| `mystmd`                                                              | `docs`      | building this site                              |

Install an optional environment by naming it:

```bash
pixi install -e tex
pixi install -e docs
```

:::{note}
`ruff` and `snakefmt` are invoked by name through
[reformatter](https://github.com/purcell/emacs-reformatter), and `just` through
`justl-executable`, so they are resolved from `$PATH` rather than from an absolute
path. Running emacs through the workspace
puts the environment first, so they resolve to `.pixi/envs/default/bin/`. If you
launch emacs some other way and also have these installed as pixi global tools, you
may silently get the global ones instead - check with
`M-: (executable-find "ruff")`.
:::

## What you have to install yourself

| Dependency                                                         | How                                                                                                                                                           | Why it is not in pixi                                                                             |
| ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster) | [Download the prebuilt binary](https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#obtain-or-build-emacs-lsp-booster) and put it on your `$PATH` | Not packaged on conda-forge. Optional, but strongly recommended - it is what makes LSP feel fast. |
| [Nerd Fonts](https://www.nerdfonts.com)                            | `M-x nerd-icons-install-fonts` from inside emacs                                                                                                              | Fonts are a system-level concern; the emacs package installs them for you.                        |
| A system TeX Live                                                  | Your distribution's package manager                                                                                                                           | Only needed for arXiv and journal submissions. See below.                                         |

## The global tool route

Everything above assumes you run emacs through this pixi workspace. You can instead
install it as a
[pixi global tool](https://pixi.sh/latest/global_tools/introduction):

```bash
pixi global install emacs
```

Nothing in the configuration prevents this - it makes no assumptions about how emacs
was launched, and the per-project interpreter lookup resolves against the *project*
root rather than against `user-emacs-directory`. Combined with
[chemacs2](https://github.com/plexus/chemacs2), a bare `emacs` in the terminal works
fine.

What you give up is everything this page is about:

- **Reproducibility.** The global manifest lives in `~/.pixi/manifests/`, not in this
  repository, and its dependencies are unpinned. `pixi.lock` describes the workspace
  and nothing else.
- **The optional environments.** `pixi global` has no notion of an environment that
  layers on another, so `tex` and `docs` have no equivalent - including the exact
  `biber` pin the LaTeX toolchain depends on.
- **CI coverage.** The build workflow runs `pixi run emacs-setup` and
  `pixi run zmq-build` against the locked environment. There is no global equivalent
  to test.
- **A matching `emacs-zmq`.** The module is built against whichever environment emacs
  is running in. Build it under a global emacs and it will not match the workspace,
  and vice versa.

And it buys less than it looks like it does. The pixi wrapper accounts for roughly
1.3 s of a ~4 s startup; the remaining ~2.8 s is the configuration loading, which a
global install does nothing about. If startup time is the motivation, a daemon is the
answer - see [Launching emacs](installation.md#launching-emacs-afterwards).

## The LaTeX boundary

This is the one place where the pixi story genuinely runs out, so it is worth being
precise about where.

### Why `tectonic` covers most cases

[tectonic](https://tectonic-typesetting.github.io) carries its own TeX Live bundle and
downloads packages on demand, so a ~6 MB conda package replaces a multi-gigabyte TeX
Live installation. It reruns the engine - and biber - as many times as a document
needs, so a single invocation is enough. For internal reports exported from org, it is
strictly simpler than a system TeX Live.

### Why `biber` is pinned to 2.17

Tectonic bundles its own TeX Live, but it shells out to an **external** `biber`
binary, and biber is strictly version-coupled to the biblatex it reads. Tectonic 0.17
ships biblatex 3.17, which only biber 2.17 understands. Using a newer biber fails
with:

```text
ERROR - Error: Found biblatex control file version 3.8, expected version 3.11.
This means that your biber (2.20) and biblatex (3.17) versions are incompatible.
```

biber is [not packaged on conda-forge](https://github.com/plk/biber/issues/485) -
doing so would require packaging its ~60 Perl dependencies first - so it comes from
the community `dnachun` channel, declared on the `tex` feature rather than on the
workspace so that the default environment's resolution is untouched.

:::{warning}
Bump the `biber` pin only together with the tectonic version. Nothing will warn you
at install time; the mismatch only surfaces when you compile a document.
:::

### When you still need a system TeX Live

Tectonic is XeTeX-based and resolves packages from its own bundle, which has two
consequences:

- **arXiv never runs tectonic.** It runs its own TeX Live (2025 by default, 2023
  still offered). A document that compiles locally under tectonic is not thereby
  proven to compile there. Since November 2025 arXiv does support XeLaTeX and will run
  `biber`/`bibtex` on an uploaded `.bib`, so the old hard incompatibility is gone -
  but the version skew remains.
- **Many journal classes assume pdflatex.** revtex, elsarticle and older IEEE/ACM
  classes are written for it, and tectonic cannot reproduce a pdflatex build.

So: **tectonic for your own PDFs, a system TeX Live for anything you submit.**
snakemacs picks whichever is present - see [LaTeX](writing/latex.md).

:::{note}
conda-forge does ship `texlive-core`, but it is a 13 MB minimal core: no `biblatex`,
no `biber`, and its bundled `tlmgr` does not work. It is not a substitute for a real
TeX Live.
:::

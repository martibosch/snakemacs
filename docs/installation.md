---
title: Installation
---

This setup uses emacs 30 and pixi. The only prerequisite is to
[install pixi](https://pixi.sh/latest/installation); everything else is either pulled
in by pixi or listed under [Dependencies](dependencies.md).

## 1. Clone the repository

```bash
cd ~
git clone https://github.com/martibosch/snakemacs ~/.emacs.d
cd .emacs.d
```

Alternatively, clone it anywhere you like and use
[chemacs2](https://github.com/plexus/chemacs2) to set snakemacs up as a - potentially
default - profile.

## 2. Run emacs once from the shell

```bash
pixi run emacs
```

:::{important}
Run this from a shell for the first launch. If you start emacs from a desktop
launcher instead, `libvterm` may not be installed properly.
:::

The first run installs all packages, which takes a while. Subsequent launches are
fast.

Part of that first run builds [emacs-zmq](https://github.com/nnicandro/emacs-zmq), a
native module that [emacs-jupyter](https://github.com/emacs-jupyter/jupyter) depends
on. You will be asked:

```text
Check for compatible module binary to download?
```

Either answer works. Answering `n` and then `y` to `ZMQ module not found. Build it?`
compiles it from source against this workspace, which is the reproducible option and
what CI does. You can also build it at any time without leaving the shell:

```bash
pixi run zmq-build
```

:::{note}
The build needs no special configuration. conda-forge's `compilers` package sets
`CC`, `CFLAGS`, `CPPFLAGS` and `LDFLAGS` - sysroot and include/lib paths included -
when the environment activates, so the module compiles against the workspace
toolchain on its own. emacs-zmq statically links its own libzmq, so the `zeromq` in
the environment is not involved.
:::

## Launching emacs afterwards

**Use `pixi run emacs` to start with. Once emacs becomes your everyday editor, switch
to a daemon.**

That recommendation follows from where the time actually goes. Measured on this
repository, loading the full configuration in batch mode:

|                                        | time   |
| -------------------------------------- | ------ |
| emacs binary alone, no config          | 0.05 s |
| full configuration, direct binary      | 2.77 s |
| full configuration, through `pixi run` | 4.11 s |

So `pixi run` costs about 1.3 s, and **the configuration itself costs about 2.8 s**.
That ratio is the whole story: avoiding pixi saves you a third of the wait and leaves
the rest, while a daemon removes all of it.

:::{note}
These are batch-mode figures, which skip graphical frame setup - real interactive
startup is somewhat higher. Treat them as proportions rather than absolutes.
:::

### The default: `pixi run emacs`

Works from any directory, needs no setup, and uses the locked environment. If you
open emacs in the morning and keep it open, this is all you need.

### The upgrade: a daemon

The only option that addresses the 2.8 s, because you pay it once:

```sh
# ~/.local/bin/snakemacs-daemon
#!/bin/sh
exec pixi run --manifest-path "$HOME/snakemacs/pixi.toml" emacs --daemon
```

Then connect with a client, which returns in well under 0.05 s:

```sh
alias e='~/snakemacs/.pixi/envs/default/bin/emacsclient -t'   # terminal frame
alias ec='~/snakemacs/.pixi/envs/default/bin/emacsclient -c'  # graphical frame
export EDITOR='~/snakemacs/.pixi/envs/default/bin/emacsclient -t'
```

The daemon inherits the workspace environment, so `ruff`, `just`, `basedpyright` and
the Jupyter tooling all resolve to `.pixi/envs/default/bin/` inside it. Run the
daemon script from your session startup - a systemd user unit is the tidiest way on
Linux.

:::{warning}
`emacsclient` has to match the version of the daemon it connects to, and it carries
no environment of its own - it only writes to a socket. Point the alias at the
workspace binary rather than relying on `$PATH`, so that pixi keeps the two versions
locked together. If you also have emacs as a pixi global tool, a `pixi global update`
would otherwise break your client while the daemon stays behind.
:::

### If you want a bare `emacs` without a daemon

Use a script on `$PATH`, not a shell alias - an alias is invisible to `$EDITOR`, to
`git commit`, to desktop launchers and to anything else that is not your interactive
shell:

```sh
# ~/.local/bin/emacs
#!/bin/sh
exec pixi run --manifest-path "$HOME/snakemacs/pixi.toml" emacs "$@"
```

`pixi run --manifest-path` preserves the working directory, so relative paths and
project detection behave normally. Note that this does nothing for startup time; it
is purely about the command name.

### What not to reach for

Installing emacs as a pixi global tool to make it start faster is a poor trade: it
saves the ~1.3 s of pixi overhead but leaves the ~2.8 s of configuration loading, and
costs you the lockfile, the optional environments and CI coverage. See
[Dependencies](dependencies.md#the-global-tool-route). If startup time is what bothers
you, use a daemon.

## Optional environments

The default environment is everything you need to edit Python and run notebooks. Two
optional environments layer on top of it:

```bash
pixi run -e tex emacs     # adds tectonic + biber, for org/LaTeX -> PDF
pixi run -e docs myst start   # builds this documentation site
```

Named pixi environments implicitly include the default feature, so `-e tex` is the
full snakemacs environment *plus* the LaTeX toolchain, not a replacement for it.

:::{note}
`-e tex` installs a complete second copy of the environment - about 2.2 GB on disk.
Use it only if you export org or LaTeX documents to PDF. See
[Dependencies](dependencies.md) for what it contains and why.
:::

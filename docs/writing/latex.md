---
title: LaTeX
---

snakemacs uses [AUCTeX](https://www.gnu.org/software/auctex/) with
[RefTeX](https://www.gnu.org/software/auctex/reftex.html) for `.tex` files, and
`ox-latex` for exporting org documents. This page is about the second half: what
actually turns a document into a PDF.

## Two toolchains, picked automatically

At startup, snakemacs checks whether `tectonic` is on `$PATH` and configures
`org-latex-pdf-process` accordingly.

::::{tab-set}
:::{tab-item} tectonic (pixi `tex` environment)

```elisp
(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process '("tectonic --outdir %o %f"))
```

One invocation is enough: tectonic fetches missing packages itself and reruns the
engine - and biber - as many times as the document needs.

Setting `org-latex-compiler` to `"xelatex"` is not cosmetic. It makes org drop the
pdflatex-only `inputenc` and `fontenc` in favour of `fontspec`, without which the
unicode that citeproc emits - en-dashes in page ranges - falls outside the T1 fonts.
:::

:::{tab-item} system TeX Live

```elisp
(setq org-latex-pdf-process
      '("%latex -interaction nonstopmode -output-directory %o %f"
        "biber --input-directory %o --output-directory %o %b"
        "%latex -interaction nonstopmode -output-directory %o %f"
        "%latex -interaction nonstopmode -output-directory %o %f"))
```

The biber step is the point. Org's default value runs the LaTeX compiler three times
and never calls biber, so every biblatex citation comes out undefined.
:::
::::

Since the choice is made with `executable-find` at startup, **restart emacs after
switching environments** with `pixi run -e tex`.

## Which one to use

Use tectonic for your own documents and a system TeX Live for anything you submit.
The reasoning - version coupling, the arXiv toolchain, why conda's `texlive-core` is
not a substitute - is on the [Dependencies](../dependencies.md) page.

## Per-project overrides

Both `org-latex-pdf-process` and `org-cite-export-processors` are ordinary variables,
so a project that needs the other toolchain can override them in `.dir-locals.el`
rather than changing the global config:

```elisp
((org-mode . ((org-cite-export-processors . ((latex biblatex) (t csl))))))
```

## A note on the "undefined citation" warning

Exporting through the system TeX Live path prints:

```text
Warning (ox-latex): PDF file produced with warnings: [undefined citation]
```

This is spurious. It is picked up from the first LaTeX pass, which necessarily runs
before biber has written the `.bbl`. The finished PDF is correct.

## Cleaning up

`org-latex-logfiles-extensions` is extended with `"bbl"`, which it does not include by
default, so the bibliography intermediate is removed alongside the `.aux`, `.bcf` and
`.blg`. The generated `.tex` is left next to the PDF - that is ordinary `ox-latex`
behaviour, and worth a `*.tex` / `*.pdf` entry in the `.gitignore` of repositories
where you do not want the artefacts tracked.

______________________________________________________________________

## title: Org

snakemacs sets org up as a lightweight LaTeX alternative: a folder holding a report
and its `references.bib`, with citations completed from the minibuffer and a PDF at
the end of it.

## Bibliographies are found automatically

You do not need a `#+bibliography:` keyword. On opening an org file,
`my/org-cite-local-bibliography` looks for `.bib` files next to it and, failing that,
walks up until it finds a directory containing one - so a report in a subdirectory
still picks up a bibliography at the project root.

```text
reports/
├── report.org
└── references.bib      ← found automatically
```

Adding a `#+bibliography:` keyword still works; it simply adds more files.

:::{note}
The hook runs when `org-mode` starts. If you create the `.bib` *after* opening the
org file, run `M-x my/org-cite-local-bibliography` or revert the buffer.
:::

## Inserting citations

`C-c [` runs `org-cite-insert`, matching `reftex-citation` in LaTeX buffers. The
default `C-c C-x @` also works.

Completion is handled by [citar](https://github.com/emacs-citar/citar), which shows
author, title and year rather than bare keys:

- `TAB` marks a reference and keeps the prompt open
- `RET` finishes

For a single citation one `RET` is enough - it marks the highlighted entry and exits
in the same stroke. `C-u C-c [` additionally prompts for a citation style.

:::{note}
`C-c [` shadows `org-agenda-file-to-front`, which is still available via `M-x`. If
you maintain `org-agenda-files` by hand rather than pointing it at a directory, you
may want that key back.
:::

## Locators

Org's citation syntax is:

```text
[cite/STYLE: GLOBAL-PREFIX ; PREFIX @key SUFFIX ; ... ; GLOBAL-SUFFIX]
```

so the equivalent of `\cite[p. 10]{key}` is `[cite:@key p. 10]`, and biblatex's
two-argument `\cite[see][p. 10]{key}` is `[cite:see @key p. 10]`:

| org                         | biblatex                         | CSL                |
| --------------------------- | -------------------------------- | ------------------ |
| `[cite:@doe2020]`           | `\autocite{doe2020}`             | (Doe 2020)         |
| `[cite:@doe2020 p. 10]`     | `\autocite[p. 10]{doe2020}`      | (Doe 2020, 10)     |
| `[cite:see @doe2020 p. 10]` | `\autocite[see][p. 10]{doe2020}` | (see Doe 2020, 10) |
| `[cite/t:@doe2020 p. 10]`   | `\textcite[p. 10]{doe2020}`      | Doe (2020, 10)     |

Stick to the standard locator abbreviations - `p.`, `pp.`, `chap.`, `sec.`, `fig.`,
`no.`, `vol.` Biblatex passes them through verbatim, while CSL parses them and
renders proper en-dashes in page ranges.

:::{warning}
Global affixes are not portable. `[cite:see;@doe2020 p. 10;and others]` renders in
full under CSL, but the biblatex processor silently drops both. If you are exporting
to PDF, put affixes on the reference rather than on the citation as a whole.
:::

## Getting a PDF

A minimal document needs a title block and a bibliography - without
`#+print_bibliography:` the citation renders but no reference list is emitted:

```org
#+title: Lorem ipsum
#+author: Your Name
#+options: toc:nil

Some claim [cite:@doe2020 p. 10].

* References
:PROPERTIES:
:UNNUMBERED: notoc
:END:

#+print_bibliography: :heading none
```

Export with `C-c C-e l o` (PDF and open) or `C-c C-e l p` (PDF only).

`:heading none` becomes `\printbibliography[heading=none]`; without it biblatex emits
its own "References" heading in addition to your org one.

:::{note}
Only cited entries appear in the reference list. To include an uncited entry anyway,
add `[cite/n:@key]` - the `nocite` style - somewhere in the file.
:::

Which toolchain actually produces the PDF depends on what is installed; see
[LaTeX](latex.md).

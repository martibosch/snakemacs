---
title: Markdown
---

Markdown is the other lightweight route to a PDF in snakemacs. It gives you the same
citation UI as [Org](org.md) - the same `C-c [`, the same `references.bib` found in
the same way - but hands the document to [pandoc](https://pandoc.org) instead of to
the org exporters.

Pick it over org when you want plain markdown that renders on GitHub, or when you
would rather not learn a second markup language. Pick org when you want babel code
blocks and the rest of the org ecosystem.

## What you need

Citation completion works out of the box. Producing a PDF needs `pandoc`, which lives
in the optional `tex` environment alongside `tectonic`:

```bash
pixi run -e tex emacs
```

The engine is chosen at startup with `executable-find`, so **restart emacs after
switching environments**, exactly as on the [LaTeX](latex.md) page.

## Bibliographies are found automatically

`my/cite-local-bibliography` runs on `markdown-mode` just as it does on `org-mode`:
it looks for `.bib` files next to the current file and, failing that, walks up until
it finds a directory containing one.

```text
reports/
├── report.md
└── references.bib      ← found automatically
```

So there is no `bibliography:` key to write in the YAML header, and no
`--bibliography` flag to remember - `my/markdown-export-pdf` passes one per file it
found.

:::{note}
The hook runs when `markdown-mode` starts. If you create the `.bib` *after* opening
the markdown file, run `M-x my/cite-local-bibliography` or revert the buffer.
:::

## Inserting citations

`C-c [` runs `citar-insert-citation`, the same key as in org buffers. citar ships a
markdown backend, so the completion UI is identical - author, title and year rather
than bare keys - and only the inserted syntax differs.

After you pick a reference, citar prompts twice:

- **Prenote** - text before the key, e.g. `see`
- **Postnote** - text after it, e.g. `p. 3`

Leave either empty to skip it. Completion inside an existing `[@...]` is handled by
`citar-capf`, which is wired into `completion-at-point-functions` and therefore picked
up by company.

## Citation syntax

Pandoc's syntax is close enough to org's that the mapping is mechanical - drop the
`cite:` prefix, separate keys with `;`, and put the locator after a comma:

| pandoc                      | org                           | renders as                        |
| --------------------------- | ----------------------------- | --------------------------------- |
| `[@doe2020]`                | `[cite:@doe2020]`             | (Doe 2020)                        |
| `[@doe2020, p. 10]`         | `[cite:@doe2020 p. 10]`       | (Doe 2020, 10)                    |
| `[see @doe2020, chap. 2]`   | `[cite:see @doe2020 chap. 2]` | (see Doe 2020, chap. 2)           |
| `@doe2020 [p. 10]`          | `[cite/t:@doe2020 p. 10]`     | Doe (2020, 10)                    |
| `[@doe2020; @rasp2021data]` | `[cite:@doe2020;@rasp2021]`   | (Doe 2020; Rasp and Thuerey 2021) |
| `[-@doe2020]`               | -                             | (2020)                            |

The in-text form `@doe2020 [p. 10]` has to be typed by hand; `C-c [` always inserts
the bracketed form.

Locator abbreviations - `p.`, `pp.`, `chap.`, `sec.`, `fig.`, `no.`, `vol.` - are
parsed rather than passed through, which is why `p. 10` renders as a bare `10` under
the default author-date style while `chap. 2` keeps its label.

## Getting a PDF

`C-c C-e` runs `my/markdown-export-pdf`, which saves the buffer and shells out to
pandoc in a `*compilation*` buffer. A minimal document:

```markdown
---
title: Lorem ipsum
author: Your Name
---

Some claim [@doe2020, p. 10].

## References
```

Title and author come from the YAML header. The bibliography is appended at the end
of the document, so the trailing heading is optional - it only gives the reference
list something to sit under.

To put the reference list somewhere other than the end, add an empty `#refs` div and
pandoc will fill it in place:

```markdown
## References

::: {#refs}
:::

## Appendix

This section comes after the bibliography.
```

To include an entry that is never cited, list it under `nocite` in the YAML header -
the equivalent of org's `[cite/n:@key]`:

```yaml
nocite: |
  @doe2020
```

## No biber, no biblatex

This is the practical difference from the org and LaTeX routes. `--citeproc` makes
pandoc resolve citations itself, from the `.bib` and a CSL style, and emit
already-formatted text. LaTeX only ever sees a finished reference list.

|                   | org / LaTeX            | markdown                 |
| ----------------- | ---------------------- | ------------------------ |
| citation engine   | biblatex + biber       | pandoc citeproc (CSL)    |
| passes            | latex, biber, latex ×2 | one pandoc invocation    |
| PDF engine        | tectonic or TeX Live   | tectonic or TeX Live     |
| style switched by | `\usepackage[style=…]` | `--csl` / `csl:` in YAML |

The consequence worth knowing: the `biber = "==2.17"` pin in `pixi.toml` exists
because biber is version-coupled to the biblatex that tectonic's bundle ships (see
[Dependencies](../dependencies.md)). That pin does not constrain this route at all -
if the pin ever breaks, markdown documents keep exporting.

Both routes still end at the same PDF engine, `tectonic` when it is on `$PATH` and
pandoc's default otherwise.

## HTML preview

`C-c C-c p` renders the buffer in a browser. `markdown-command` is set to pandoc with
`--citeproc` when pandoc is available, so citations resolve in the preview too rather
than showing up as literal `[@key]`.

Without pandoc, `markdown-mode` falls back to whichever markdown binary it finds on
`$PATH`, and the preview shows unresolved keys.

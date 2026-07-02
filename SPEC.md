# Spec: `c2md` — Confluence Page to Markdown

## Decision

**Approach:** ADF (`atlas_doc_format`) → Markdown via **[adf4j](https://nthings.dev/projects/adf4j/)** (`dev.nthings:adf4j:1.0.0`).

**Runtime:** Java 25 (required by adf4j).

HTML/`export_view` was evaluated and rejected — lossy for Confluence macros and poor round-trip with `md2c`. See prior discussion.

---

## Assumptions

1. **Single page only** — `c2md` fetches one Confluence page by numeric ID.
2. **Confluence Cloud** — REST API v2, `body-format=atlas_doc_format`.
3. **Authentication** — Reuses `atbp.confluence` / `atbp.jira` HOCON config (same as `md2c`).
4. **Conversion** — adf4j `AdfToMarkdown` with Confluence resolvers for attachments and page links.
5. **Output** — UTF-8 YAML front matter (`title`) + GFM body; stdout when no `--target`.
6. **Round-trip** — Best-effort compatibility with `md2c`; adf4j diagnostics logged when conversion is lossy.

---

## Objective

Add `atbp c2md` to download a Confluence page by ID and convert its ADF body to Markdown.

**Acceptance criteria:**

- `atbp c2md <page-id>` prints Markdown to stdout.
- `atbp c2md --target out.md <page-id>` writes to file.
- Missing Confluence config → clear error (same as `md2c`).
- Invalid page ID / API failure → non-zero exit with stderr message.
- Output includes YAML front matter with page title.
- adf4j handles standard ADF/GFM constructs; lossy conversions logged at WARN.

---

## Tech Stack

| Component | Choice |
|-----------|--------|
| Language | Scala 3.8.4 |
| JVM | Java 25 |
| Effect system | ZIO 2.x |
| CLI | zio-cli 0.8.1 |
| Confluence API | Existing `confluence` module |
| ADF → Markdown | adf4j 1.0.0 |
| File I/O | better-files |
| Tests | zio-test |

---

## Commands

```bash
sbt --client compile
sbt run -- c2md 123456789
sbt run -- c2md --target page.md 123456789
atbp c2md --help
sbt --client c2md/test
sbt --client fixup
```

### CLI interface

```
atbp c2md [--target FILE] PAGE_ID

Arguments:
  PAGE_ID    Confluence page ID (numeric string)

Options:
  --target   Write Markdown to FILE instead of stdout
```

---

## Project Structure

```
c2md/
  src/main/scala/ph/samson/atbp/c2md/
    Converter.scala          # Fetch page, build adf4j options, convert
    Format.scala             # YAML front matter wrapper
  src/test/scala/ph/samson/atbp/c2md/
    FormatSpec.scala
    ConverterSpec.scala      # adf4j fixture conversion

cli/src/main/scala/ph/samson/atbp/cli/
  Confluence2Markdown.scala

confluence/src/main/scala/ph/samson/atbp/confluence/
  Client.scala               # getPage(id, bodyFormat)
```

---

## Code Style

- `ToolCommand` case class + companion (match `Markdown2Confluence`)
- adf4j calls wrapped in `ZIO.attempt`
- `ZIO.logSpan` for top-level ops
- Log adf4j `wasLossy()` diagnostics at WARN
- `sbt --client fixup` before commit

---

## Testing Strategy

| Level | Scope |
|-------|-------|
| Unit | `Format` front matter; adf4j conversion of fixture ADF JSON |
| Integration | Live Confluence fetch, gated on `atbp.confluence.site` |

Fixtures under `c2md/src/test/resources/adf/`.

---

## Boundaries

### Always

- `body-format=atlas_doc_format` when fetching page body
- Supply `ConfluenceRenderContext` with page attachments to adf4j
- Log page title and ID on success; log lossy diagnostics

### Ask first

- Recursive child-page export, attachment binary download, storage/HTML fallback

### Never

- Commit credentials; silently drop content without adf4j diagnostics

---

## Success Criteria

- [ ] Java 25 in build, CI, and Docker image
- [ ] `sbt --client compile` succeeds
- [ ] `atbp c2md --help` shows usage
- [ ] `atbp c2md <id>` / `--target` work with Confluence config
- [ ] `sbt --client c2md/test` passes

---

## Resolved Questions

| Question | Decision |
|----------|----------|
| Conversion approach | ADF + adf4j |
| Title format | YAML front matter (`title:`) |
| Unsupported nodes | adf4j `unknownNodePolicy` default (`PLACEHOLDER`) |
| Media / attachments | adf4j resolvers + page attachment inventory |
| Draft pages | Fetch whatever API returns for the ID |
| Child page export | Out of scope for v1 |

---

## Out of Scope (v1)

- Recursive export, attachment download, storage/HTML fallback, batch IDs, CQL lookup

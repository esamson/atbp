# md2c Source Footer

## Problem Statement

How might we help readers and contributors of generated Confluence pages find
the source, make edits in the right place, and understand recent changes?

## Recommended Direction

Append a compact footer separated from the page body, with editing instructions
first and recent history last. For Markdown-backed pages, show these elements
in order:

1. Exact warning: "Do not edit here. This page was generated from markdown source."
2. Copyable `git clone` command using the local checkout's remote.
3. Source Markdown file's repository-relative path.
4. A note when that source file has uncommitted changes.
5. Up to five recent Git commits affecting that file, each showing date, author,
   abbreviated commit hash, and commit subject line only.

Directory-only pages receive a directory-specific notice, clone command, and
repository-relative directory path, with no file history. The notice identifies
a generated directory listing rather than claiming a Markdown source exists.

Select `origin` when present; otherwise use the sole remote. If there are no
remotes or multiple remotes without `origin`, clearly report the missing or
ambiguous remote instead of guessing. When Git metadata is incomplete, publish
the available facts and identify missing information. Changes to page content
or footer information trigger publishing; identical subsequent publishes remain
skipped.

## Key Assumptions to Validate

- [ ] Contributors can use Git and access the selected remote. Validate by
  following the footer from a representative published page.
- [ ] File history provides useful context without representing the exact
  published revision. Verify that the uncommitted-changes note makes this
  distinction clear for locally modified source files.
- [ ] Footer updates remain stable between identical runs. Verify that a new
  commit affecting the source file triggers publishing even when its Markdown
  body is unchanged, and that an unchanged repeat run is skipped.

The main failure risk is misleading source information. An ambiguous remote or
unavailable history must be identified rather than guessed. Repository access
and a Git-based contribution workflow are prerequisites for the editing route
to be useful.

## MVP Scope

Support Markdown-backed pages, directory-only pages, incomplete Git metadata,
source-file change detection, and footer-aware publishing. Keep the presentation
compact and consistent. Source location and history refer to the actual Markdown
file used to generate each page, including folder notes.

## Not Doing (and Why)

- Clickable repository links — the clone command and path satisfy the intended
  editing route without repository-host-specific URL handling.
- Full commit messages or full history — subject lines and at most five entries
  keep the footer readable.
- Preventing Confluence edits — the footer directs behavior without enforcing it.
- Diffs or contribution workflows — the footer identifies where to work;
  repository tooling handles the editing process.
- File history for directory-only pages — these pages have no Markdown source
  file whose history can be shown.

## Open Questions

- No remaining product-direction questions. Detailed Git history semantics,
  formatting, and failure handling belong in the downstream specification.

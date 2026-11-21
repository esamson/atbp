# Markdown to Confluence

`atbp md2c` publishes a Markdown directory tree to Confluence. A `.md2c.conf`
file selects the Confluence space (`spaceKey`) and root page (`pageId`).

Links to Markdown documents in the same published tree become links to their
Confluence pages. Paths resolve relative to the document containing the link:

```markdown
[Getting started](Getting%20started.md)
[Reference](../Reference.md#details)
[Folder overview](guide/README.md)
```

Inline and reference-style links work across siblings, parents and descendants,
including forward and circular references. Folder notes (`README.md`, `index.md`
or a file named after its directory) link to the corresponding branch page.
Page titles set in YAML front matter do not change how paths resolve. Link text,
formatting, titles, query strings and fragments are preserved; heading fragments
are not translated between Markdown and Confluence anchor conventions.

Only destinations present in the published tree are converted. External links,
images and code retain their existing behavior. Other relative links remain
subject to the Markdown-to-ADF library's behavior, which can render them as plain
text.

md2c resolves or creates all destination pages before publishing their content.
Resolved destinations are included in the content hash, so existing pages receive
converted links on the next publish, and links update if a destination page is
recreated. Identical subsequent publishes are still skipped.

## Source footer

Every published page ends with a horizontal rule and a short footer that tells
readers where the page came from. Do not edit the Confluence page: changes are
overwritten by the next publish.

A page generated from Markdown shows, in order: the warning `Do not edit here.
This page was generated from markdown source.`, a copyable `git clone` command,
`Source:` with the repository-relative path, an uncommitted-changes notice when
it applies, and up to five recent commits (`date | author | abbreviated hash |
subject`; message bodies are never shown). A directory-only page keeps its child
listing and shows the warning `Do not edit here. This page is a generated
directory listing.`, the clone command, and `Source directory:` (`.` for the
repository root); it has no change status or history.

### Source selection

The footer describes the actual file a page was generated from. A folder note is
the selected Markdown file for its directory, using the existing precedence:
`<directory>.md`, then `README.md`, then `index.md`. The footer path and history
refer to that file, not to the containing directory. A directory with no folder
note is a directory-only page.

### Local Git prerequisites

Footer metadata comes from the local `git` executable only: md2c never fetches,
pushes, or modifies the checkout, and each command is noninteractive and time
limited. A publication root below the repository root still shows
repository-relative paths, and linked worktrees are supported.

- The clone command uses the `origin` remote, or the only configured remote, and
  its first fetch URL. The command quotes that URL as one shell argument.
  Supported forms are HTTP(S), Git and SSH URLs (including `git+ssh://` and
  `ssh+git://`) and SCP-style SSH addresses. Ordinary SSH user names are
  allowed. Embedded credentials, query or fragment values, network URLs with
  invalid components, unsupported transports, and relative or absolute local
  paths show
  `Clone command unavailable: selected remote URL contains credentials or is unusable.`
  The stored URL is shown as configured, without `url.<base>.insteadOf`
  rewriting or SSH host-alias expansion.
- History is local history reachable from `HEAD`, following renames, with author
  dates and names (no email addresses).

### Uncommitted and incomplete history

- A source file with staged or unstaged changes, or one that is not in the Git
  index (untracked or ignored), shows `Source has uncommitted changes; history
  below contains committed changes only.` History shows committed changes, not
  the exact bytes published. Changes to other files do not count.
- An untracked source, or a repository with no commits (unborn `HEAD`), shows
  `No committed history for this source file.` An untracked file never inherits
  the history of an unrelated file with the same name.
- A shallow clone shows the entries it has plus `History may be incomplete:
  shallow checkout.` md2c does not fetch more history.

### Notices

Missing Git information never stops publication. Each fact degrades on its own:

- `Git metadata unavailable: git executable not available.`
- `Git metadata unavailable: source is not in a Git worktree.`
- `Git metadata unavailable: repository lookup failed.`
- `Clone command unavailable: no Git remote configured.`
- `Clone command unavailable: multiple Git remotes and no origin.`
- `Clone command unavailable: remote lookup failed.`
- `Repository-relative source path unavailable.`
- `Source change status unavailable.`
- `Source history unavailable.`

Git diagnostics go to the local log with credentials removed, never to Confluence.

### Footer-driven updates

The footer's visible content is part of each page's content hash, together with
the Markdown and resolved links. A page is updated when its clone command, source
path, uncommitted notice, history, or any of the notices above changes. A commit
that touches only the source's front matter changes its history and so updates
the page. A commit that leaves the page's footer and body unchanged, such as one
to an unrelated file, does not. Pages published before the footer existed receive
it on the next run; identical later runs are skipped.

Keep source files and repository state (branch, index, remotes) stable while a
publish runs: sources and metadata are read as each page is published, one page
at a time.

## Duplicate page titles

Confluence requires unique page titles in a space. When two or more pages in the
published tree share the same logical title (YAML `title` or node name), md2c
assigns a distinct Confluence title for the **whole tree** before it creates or
matches any draft. A title that appears only once stays as written. In each
duplicate set, source paths are sorted; the first keeps the bare logical title
and each later path gets one more invisible U+200B (ZERO WIDTH SPACE) appended.
Child lookup uses that assigned string. If no child has it, an existing page
in the space with that exact title is reused and moved under the source parent;
otherwise a draft is created. A reused page keeps its id. Link resolution, link
text, content hashes, and leading-H1 trimming stay on the logical title—do not
put U+200B into Markdown sources. If duplicate-set membership or path order
changes so a page’s suffix count changes, exact title match misses the old
page: md2c creates a new page with the new title and deletes the previous
unmatched child, the same outcome as a logical title change today.

## Memory use on CI runners

Publishing reserves page IDs using titles and empty drafts, then converts and
publishes one page at a time, children before parents. Completed page bodies are
discarded. Footer metadata is collected during each page's conversion, with
bounded concurrent Git commands, rather than for the whole tree up front. Diagram rendering and attachment uploads also run one at a time during
publishing. This trades parallel throughput for lower peak heap use.

The source tree and page-ID map still grow with the number of files, but Markdown
and rendered page bodies no longer accumulate across the document set. The heap
must accommodate the largest page and its diagram renderer. Keep source files
unchanged while a publish is running, since bodies are read as each page is
published.

For an explicit JVM heap cap with the packaged launcher:

```bash
atbp -J-Xmx512m md2c /path/to/docs
```

The cap covers Java heap, not total process memory or D2 subprocess memory.
Large individual pages or diagrams may need a larger cap.

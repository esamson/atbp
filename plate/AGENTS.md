# Plate Module

`plate` implements "The Plate" tooling: CLI actions that read a person's
planning Markdown document (a list of Jira issue links, usually organized
under headings) and use it as the source of truth for Jira housekeeping —
labeling, staleness/progress reporting, cross-project radar, and rank
sorting. Entry point is `cli`'s `Plate` command
(`cli/src/main/scala/ph/samson/atbp/cli/Plate.scala`), which wires each
subcommand to a service in this module. `plate` depends on the `jira`
module for `Client` and the Jira domain model; it has no tests.

All four actions are read *and* two of them (`label`, `sort`) are read-write
against a live Jira instance — `label` calls `addLabel`/`removeLabel`,
`sort` calls `rankIssuesBefore`. Treat changes to those two paths as
changes with a real external side effect on the team's actual Jira board,
not local-only refactors.

## The Jira-link convention

A "Plate" document is Markdown where lines reference issues as
`[summary](https://<host>/browse/KEY-123)`. Two different regexes extract
the key, and they are **not interchangeable**:

- `Inspector.JiraLink` — `.*\(https://.*/browse/([A-Z]+-\d+)\).*` — anchored
  to match a *whole line*; used with `line match { case JiraLink(key) => }`
  to classify one line at a time (`check` actions).
- `Labeler.JiraKey` / `Sorter.JiraKey` — `\(https://.*/browse/([A-Z]+-\d+)\)`
  — unanchored, used with `findAllMatchIn` over a whole file's content to
  collect every key regardless of line structure (`label`, `sort`, and
  `RadarScanner` reuses `Inspector.JiraLink` the same way).

If you add a fifth action that needs to pull keys out of a document,
match the pattern the sibling actions already use for the same style of
extraction (line-classification vs. whole-content scan) rather than
inventing a third regex.

## Shared vocabulary: `JiraOps`

`JiraOps.scala` is the extension-method layer every other file in this
module builds on:

- `Client` extensions: `getIssues`, `getIssuesRanked` (Rank-ordered),
  `getChildren`/`getDescendants`, `getParents`/`getAncestors` — all
  recurse via the issue's `parent` field, all short-circuit to `Nil` on an
  empty key list (so callers don't need to guard).
- `Issue` extensions: `inProgress`/`isDone` read `statusCategory.name`
  ("In Progress" / "Done") — **the Jira status *category*, not the literal
  status name**. Don't compare against raw `status.name` for these checks.
  `projectKey` is the issue key's prefix before `-`. `webUrl` derives the
  browse URL from `issue.self`.

Hierarchy level (`issue.fields.issuetype.hierarchyLevel`) is Jira's
epic/story/subtask rank number, used by `Sorter.atLevel` to walk up to an
ancestor or down to descendants exactly one level at a time — recursing
more than one level per step would loop forever between "above" and
"below" the target level, which is why `atLevel` explicitly checks
`issueLevel + 1` / `issueLevel - 1` before recursing.

## The four actions

| Action | Service | Reads | Writes |
|---|---|---|---|
| `label` | `Labeler` | source file(s)/dir(s) of `.md`/`.markdown`, expanded recursively | adds/removes a Jira label so labeled issues == (keys in doc) ∪ ancestors ∪ descendants, minus excluded projects |
| `check --status cooking\|stale\|done` | `Inspector` | one source file | sibling report file |
| `radar` | `RadarScanner` | one source file | sibling `.radar` report file |
| `sort` | `Sorter` | one source file | Jira rank order, per hierarchy level |

**Known quirk:** `Inspector.cooking` and `Inspector.done` both default their
output target to `sibling(source, ".done")` when `--target` is omitted —
`stale` is the only one that gets its own suffix (`.stale`). Running
`check --status cooking` and `check --status done` back-to-back without
`--target` overwrites the same default file with two different report
kinds. This looks unintentional; if you're touching `Inspector`, don't
"fix" it silently — flag it, since it's the kind of thing that could also
be a deliberate default someone relies on.

`cooking`/`stale`/`done` share a two-pass shape: `enrichLine`/`check`
classifies each line (attaching Jira data via `Enriched`), then
`synthesize`/`prune` decides what survives into the report and cleans up
now-empty Markdown headings and doubled blank lines. If you add a new
`check` status, follow this same enrich → synthesize → `prune` shape
rather than writing a bespoke filter — `prune` in particular is shared,
recursive, and easy to get subtly wrong if reimplemented.

## Progress model (`Inspector`)

"Progress" on an issue = a changelog entry or comment newer than a
threshold, where the changelog entry isn't *only* touching fields listed in
`nonProgress` (Rank, labels, Sprint, IssueParentAssociation, assignee,
summary, description, Mesh Group, Component, Link, Attachment, Story
Points, timeoriginalestimate, timeestimate, WorklogId, Project / Market,
issuetype, or a status change *to* "Todo"). This list is a manually curated
allowlist-of-noise tied to this team's actual Jira field names (e.g. "Mesh
Group", "Project / Market" are custom fields) — it will silently stop
matching if those custom fields are renamed on the Jira side, not if the
Scala code changes.

Thresholds: `CookingProgressDays = 14`, `StaleProgressDays = 28`. `cooking`
surfaces issues *with* recent progress; `stale` surfaces issues *without*
it (inverted — see `Inspector.stale`'s `.negate`).

## Report formatting conventions

Generated reports reuse a small emoji vocabulary wrapped in `<small>` tags
— 🚧 for a changelog change, 📝 for a comment, 📌 for a descendant-issue
header in `cooking`, ❎ for a not-done descendant in `done`. Keep new report
lines consistent with this style rather than introducing a new marker
scheme.

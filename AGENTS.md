# Agent Rules

## Scala tooling

### Default: always `sbt --client`

Use `sbt --client` for **every** sbt invocation unless one of the exceptions
below applies. This includes `compile`, `test`, `testOnly`, `fixup`, and
project-scoped tasks — not only `fixup`.

```bash
sbt --client compile
sbt --client "liga/test"
sbt --client "liga/testOnly *handicap*"
sbt --client fixup
```

**Do not use `sbt --batch` for routine compile/test work.** Common wrong
reasons that do **not** justify `--batch`:

- "I'm not sure the sbt server is running" — use `--client` anyway; it starts
  one if needed.
- "This is a one-off command" — still `--client`.
- "Only source files changed" — still `--client`.
- `git status` shows unrelated local changes under `project/` from a previous
  session — still `--client` unless **you** edited build files in this task.

### Exception: `sbt --batch` after build-definition edits

Use `sbt --batch` **only when you have changed** `*.sbt` files or files under
`project/` **in the current task** (e.g. `build.sbt`, `project/plugins.sbt`,
`project/Dependencies.scala`). A clean tree or stale uncommitted build edits
you did not touch does not switch the default.

```bash
# After editing build.sbt or project/*
sbt --batch compile
sbt --batch fixup
```

### Exception: `sbt run` (no `--client`)

To verify that the app starts, use `sbt run` **without** `--client` — the client
mode prevents interrupting the process.

### Do not run interactive sbt commands

Never launch interactive sbt tasks — they block waiting for input and leave the
agent hung. Examples:

- `sbt console` / `sbt --client console`
- bare `sbt` or `sbt --client` with no task
- `sbt ~compile` and other watch modes that run until interrupted

Use non-interactive alternatives instead: `compile`, `test`, `testOnly`, `run`
(when you need to verify startup), or `fixup`.

### Commit workflow (required — never skip)

Before **every** commit that touches Scala (or `build.sbt` / `project/`):

1. `git add` **all** files that belong in the commit (including **new/untracked**
   Scala sources). Untracked files are not formatted until they are part of the
   tree fixup will compile — do not leave them untracked through the loop.
2. Run `sbt --client fixup` on the whole project. No arguments. Not just changed files.
3. Run `git status` — if **any** file is modified **or** still untracked that you
   intend to commit, `git add` it and go back to step 2.
4. Repeat until **both** are true in the **same** shell check:
   - `fixup` exits successfully
   - `git status` shows a clean working tree (nothing modified, nothing untracked
     that belongs in this commit)
5. Commit once — the commit must contain both the logic change and the `fixup`
   output.

**A successful `fixup` exit code alone never means you are done.** Scalafmt often
rewrites files only on a later pass (e.g. after compile picks up a newly added
source). The gate is `git status`, not the exit code.

**Common failure mode (do not repeat):** run `fixup` once → see `[success]` →
commit new Scala files that were still untracked or never re-checked → next
`fixup` produces a leftover formatting diff. That is a workflow violation.

**Never** claim the loop is done, and **never** commit, until step 4’s dual
condition holds. If you already committed without it: amend (only if still
allowed) or add a follow-up commit that completes the loop.

`fixup` results must land in the **same** commit as the change they format, not in a
later commit — except when the commit was already pushed and amend is forbidden;
then a follow-up formatting commit is required immediately.

Verify with this loop (do not skip `git status`):

```bash
git add -A   # or add the specific paths for this commit, including new files
sbt --client fixup && git status
```

If `git status` lists modified or relevant untracked files, `git add` them, run
`sbt --client fixup && git status` again, and repeat until the working tree is
clean for this commit. **Only then** commit.

In user-facing verification or commit summaries, do **not** claim `fixup` is clean
unless the last `sbt --client fixup && git status` left the working tree clean.

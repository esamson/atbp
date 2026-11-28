# DMARC aggregate report viewer V1

Source spec: SPEC.md

Implement the local `atbp dmarc` command described in [SPEC.md](../SPEC.md). An administrator should be able to open one HTML file and trace its totals and authentication failures back to a receiver, reporting window and input. The [implementation checklist](todo.md) gives the acceptance criteria and verification for each slice.

## Implementation order

| Task | Observable result | Depends on |
| --- | --- | --- |
| 1 | A plain XML report produces a weighted HTML summary with all four alignment combinations. | — |
| 2 | The registered command safely refreshes the local file, prints its path and opens it. | 1 |
| 3 | The same command reads gzip and ZIP inputs and explains partial results. | 2 |
| 4 | Equivalent copies count once. Disputed reports appear separately with each source and its evidence. | 3 |
| 5 | The page shows receiver coverage, IP volume and policy evidence across domains and windows. | 4 |
| 6 | The page lists affected groups in priority order with their authentication details and provenance. | 4 |

Review a plain report through the CLI after task 2, review a mixed directory after task 4, and run the full CLI suite and desktop check after tasks 5 and 6. Tasks 5 and 6 use the same selected reports and can be verified independently. Implement them in order if they touch the same renderer.

## Fit with the repository

- Register `Dmarc.command` alongside `TraceViz.command` in `cli/src/main/scala/ph/samson/atbp/cli/Main.scala`. Follow `TraceViz.scala` for the command definition and `ToolCommand.run(conf)` for execution. Propagate failures to the existing exit handler in `Main`.
- Keep reading, analysis and rendering in the sibling `ph.samson.atbp.dmarc` package of the existing `cli` project. Carry the file/member identity alongside each parsed report. Use pure functions for alignment, copy comparison, aggregation and rendering. Scope directory, archive and output resources with ZIO, and inject the browser opener so command tests can observe publication order.
- Use the CLI module’s existing ZIO Test dependencies. Follow `PlateParallelismSpec.scala` for argument parsing and scoped temporary files. Author fixtures from the spec using fictional reports and generate compressed variants during tests. Keep private inputs and their derivatives out of every artifact, including commit messages. Keep generated HTML out of version control.

## Reviewed decisions

- **Conflicting copies.** Group by receiver, policy domain and report ID. Compare parsed content, including window, policy, rows and raw details, independent of encoding, XML formatting and row order. If any copies disagree, exclude the entire identity from all aggregates and affected-group tables. List it once as disputed, naming each file/member and showing the differing evidence and candidate counts. If every parsed report is disputed, publish and open the page with the disputes visible and the pass rate labelled “unavailable.”

- **ZIP processing limits.** If the archive-wide expanded-byte or entry-count limit prevents finishing a ZIP, stop reading that archive. Keep each fully read and parsed report eligible for the usual duplicate and conflict checks. Discard any unfinished member and continue with the next file. Mark the published summary partial, name the archive and limiting condition, and identify the stopping member when known.

  In task 3, choose explicit production limits for expanded bytes per input/member, cumulative expanded bytes per archive and ZIP entry count. Check limits while reading and test their boundaries with small injected values.

- **Publication.** Write and close a temporary file in the output directory, then atomically replace `dmarc-report.html`. If writing or replacement fails, preserve the previous report, remove the temporary file, explain the failure and exit nonzero before launching the browser. After successful replacement, print the absolute path and open its file URI. A browser failure leaves the published file available at that path.

# DMARC aggregate report viewer (V1)

## Objective

Give a domain administrator one local command to answer three questions about a directory of DMARC aggregate reports. How much reported mail passed DMARC? Which receivers and sending IPs accounted for it? Which observations failed either authentication check?

## Commands

| Invocation | Result |
| --- | --- |
| `atbp dmarc` | Analyze the current working directory. |
| `atbp dmarc reports/` | Analyze the directory given as the single positional argument. Support relative and absolute paths, including paths with spaces. |
| `atbp dmarc --help` | Show the optional directory argument, supported formats, output filename and browser behavior. |

On success, create or replace `dmarc-report.html` in the input directory, print its absolute path, then open its file URI in the local default browser. A browser-launch failure returns a nonzero exit and leaves the completed file available at the printed path.

## Input and analysis

- Scan immediate regular files for plain XML, gzip-compressed XML (`.xml.gz`), and XML members of ZIP archives. Ignore unrelated files, including the generated HTML. Use report metadata for receiver, policy domain, report ID and UTC reporting window.
- Count repeated copies of the same report once using receiver, policy domain and report ID. Identify duplicate inputs in the report. Flag conflicting copies so discrepancies are visible.
- Weight every record by its DMARC `row/count`. Use the aligned SPF and DKIM results in `policy_evaluated`. Either aligned pass yields a DMARC pass. Use `auth_results` for details such as signing domains and selectors. Describe an absent authentication result as unreported.
- Show reporting dates, receiver coverage, report and observation totals, pass rate, all four alignment combinations, source IP volumes, published policies and observed dispositions. Keep different policy domains distinguishable. Attribute policy changes or disagreements to the reporting receiver and window.
- List full failures first, followed by SPF-only and DKIM-only passes. Give each affected group's date window, receiver, source IP, observation count, alignment results and source filename/member. Report the evidence without inferring sender ownership, intent or final delivery.
- For an invalid directory, unreadable directory or zero usable reports, return a nonzero exit with a diagnostic and preserve any previous HTML. For mixed valid and malformed reports, open the usable results and mark the summary as partial, naming each skipped input and reason.

## Project structure and code style

Use the existing Scala 3 CLI project. Register `Dmarc.command` in `cli/src/main/scala/ph/samson/atbp/cli/Main.scala` and follow the adjacent `TraceViz` and `ToolCommand` patterns. Keep report reading, aggregation and HTML rendering under `cli/src/main/scala/ph/samson/atbp/dmarc/`. Use the existing ZIO Test source tree for command and analysis tests.

Keep authentication decisions pure and use ZIO at filesystem and browser boundaries. For example, when both values represent receiver-evaluated alignment:

```scala
def passesDmarc(alignedDkim: Boolean, alignedSpf: Boolean): Boolean =
  alignedDkim || alignedSpf
```

Use descriptive field names, repository formatting and deterministic ordering for tied counts. Render the summary and details as a self-contained UTF-8 HTML document with labelled tables.

## Testing and acceptance

- All automated tests must run from a fresh checkout using independently authored synthetic reports. Use reserved example domains, documentation IP addresses, fictional receivers and invented report metadata, policies and counts. Generate compressed inputs from the synthetic XML.
- For a synthetic report for `example.test`, give four rows observation counts of 5 (both aligned), 4 (SPF alone), 2 (DKIM alone) and 1 (neither). Assert 12 observations, 11 DMARC passes (91.7%) and one failure. Assign the first two rows to `192.0.2.10` and the other two to `192.0.2.20`, and verify source totals of 9 and 3. Verify receiver totals independently with a second fictional receiver.
- Use small synthetic fixtures for counted rows, multiple policy domains, duplicates, all supported encodings, multi-member ZIPs, missing raw authentication details, XML/HTML escaping and malformed input. Exercise both directory argument forms, successful refresh, write failure, and browser-launch failure. Capture the URI with a test browser opener and verify the completed file exists before launch.
- Run `sbt --client "cli/test"` and manually run `atbp dmarc reports/` on a desktop with a directory of synthetic reports to confirm the default browser opens the generated report. Follow the root AGENTS.md fixup and staging loop for a Scala commit.

## Boundaries

Always process locally, read archives without extracting members to disk, disable external XML entities and DTDs, and escape report-controlled text in HTML. Apply bounded decompression and release streams on failure. Replace the output only after the new document has been written successfully.

Ask before expanding the command to upload reports, query external services or change DNS policy. Treat input reports as read-only and keep generated HTML out of version control.

The supplied reports are private local development references only. Keep the originals, sanitized copies and any derived configuration, statistics or output out of version control. This applies to source, fixtures, assertions, snapshots, documentation and commit messages.

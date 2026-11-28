# DMARC V1 implementation checklist

The criteria below come from [SPEC.md](../SPEC.md). Task numbers refer to the [implementation plan](plan.md). Each verification command names a proposed ZIO Test suite under `cli/src/test/scala/ph/samson/atbp/`.

## Phase 1 — Open one local report

- [ ] **1. Read a plain XML report into a weighted summary.** No dependencies. Work in the `dmarc` reader, analysis, renderer and `DmarcSummarySpec`.
  - Parse receiver, policy domain, report ID, UTC window and counted rows. Reject DTDs, disable external entity resolution, bound input size and close resources even on parse failure.
  - Derive DMARC from aligned SPF or DKIM in `policy_evaluated`. With the spec’s four `example.test` rows, the rendered summary shows counts of 5 both, 4 SPF only, 2 DKIM only and 1 neither, giving 12 observations, 11 passes (91.7%) and one failure. Include a case where raw authentication and alignment disagree.
  - Verify with `sbt --client "cli/testOnly *DmarcSummarySpec"`. Assert the labelled summary values, include escaped report-controlled text, and verify that a synthetic document with a DTD is rejected before resolution.

- [ ] **2. Publish and open the summary through the CLI.** Depends on 1. Work in `cli/Main.scala`, `cli/Dmarc.scala`, the local publication code and `DmarcCommandSpec`.
  - Register the optional directory argument. Exercise the current-directory default, relative and absolute paths with spaces, and help describing the formats, output filename and browser launch. Scan immediate regular inputs, ignoring subdirectories, unrelated files and existing output.
  - Create or refresh UTF-8 `dmarc-report.html` only after successful analysis and writing. The test opener must observe the already-printed absolute path and read the completed file at the supplied file URI. An opener failure produces a diagnostic and nonzero exit while leaving the new file readable.
  - Verify with `sbt --client "cli/testOnly *DmarcCommandSpec"`. Cover successful replacement, invalid/unreadable directories, zero usable reports and write failure. Seed prior output where possible and check its bytes and zero opener calls on pre-publication failures. Use controlled I/O failures where filesystem permissions are unreliable.
  - Force atomic replacement to fail after the temporary HTML is complete and closed. Check that the previous report is byte-for-byte intact, the temporary file was removed, a diagnostic was emitted, the command exits nonzero and the browser opener received zero calls.

- [ ] **Checkpoint 1.** Run `sbt --client "cli/testOnly *Dmarc*Spec"`. Review the four-row report through the registered command, including a second run into the same directory.

## Phase 2 — Account for every input

- [ ] **3. Read compressed inputs and report partial results.** Depends on 2. Work in the directory/archive reader, summary diagnostics and `DmarcInputSpec`.
  - Use the same XML reader for plain files, `.xml.gz` and each XML member of a ZIP. Read in place, keep source names and scope all streams. Enforce the expansion and entry limits in the plan while streaming.
  - A mixture of usable and malformed inputs still publishes and opens a page marked partial. Identify each skipped report or member and its reason. Retain readable ZIP siblings where processing can continue. If nothing is usable, preserve the previous page and return a diagnostic.
  - Verify with `sbt --client "cli/testOnly *DmarcInputSpec"`. Generate gzip and multi-member ZIPs from synthetic XML. Test equivalent totals per encoding, a bad member between good members, a non-XML member, a corrupt archive and inputs at and over the configured limits. Check that no members appear on disk.
  - Force the archive byte limit midway through a member and, separately, the entry limit between members. Include another valid file after the ZIP. Assert that completed reports remain eligible, the unfinished and unread members contribute zero observations, and the later file is processed. Check the partial notice for the archive, the limit reached and the stopping member when known.

- [ ] **4. Count undisputed reports and show conflicts separately.** Depends on 3. Work in copy comparison, summary diagnostics and `DmarcDuplicateSpec`.
  - Group by receiver, policy domain and report ID before summing. Equivalent copies across files or members count once and are listed by source. The same ID under another receiver or domain counts independently.
  - When copies disagree, exclude the entire identity from the report count, observation totals, pass-rate numerator and denominator, and all downstream breakdowns. Show the disputed identity once with each source, candidate count and discrepancy. If every parsed identity is disputed, still publish and open the page with the pass rate labelled “unavailable.”
  - Verify with `sbt --client "cli/testOnly *DmarcDuplicateSpec"`. Add a pair claiming 12 and 15 observations to an undisputed baseline. Assert the baseline totals and rate, one dispute and both candidate counts in either input order. Also test conflicts over policy with equal counts, equivalent re-encoded/reordered copies, independent identities and a directory containing only disputed copies.

- [ ] **Checkpoint 2.** Run `sbt --client "cli/testOnly *Dmarc*Spec"`. Review one synthetic mixed-directory result and reconcile the displayed report and observation counts with the selected sources, skipped-input reasons and conflict notices.

## Phase 3 — Explain the observations

- [ ] **5. Show coverage, source volumes and policy history.** Depends on 4. Work in aggregation, the HTML renderer and `DmarcCoverageSpec`.
  - Display reporting dates in UTC, receiver coverage and totals, distinct reports, weighted observations, pass rate and source IP volumes. Keep policy domains distinguishable and use deterministic tie ordering.
  - Show published policy by domain, receiver and reporting window alongside observed disposition counts. Preserve evidence of policy changes across windows and disagreements between receivers.
  - Verify with `sbt --client "cli/testOnly *DmarcCoverageSpec"`. Assert 9 observations for `192.0.2.10` and 3 for `192.0.2.20` in the four-row report. Add a second fictional receiver with independently calculated totals, another domain, changed policies and tied counts. Assert attribution in the labelled HTML tables.

- [ ] **6. Trace affected observations to their source.** Depends on 4. Work in affected-group aggregation, the HTML renderer and `DmarcEvidenceSpec`.
  - List full failures first, then SPF-only passes, then DKIM-only passes. Exclude both-pass rows from this listing. Keep UTC window, domain, receiver, source IP, weighted count, both alignment results and filename/member visible for each group.
  - Show available raw authentication details, including signing domains and selectors, and label absent details “unreported.” Describe observed evidence without claims about ownership, intent or delivery. Produce self-contained UTF-8 HTML with labelled tables and escaped report text, source names and diagnostics.
  - Verify with `sbt --client "cli/testOnly *DmarcEvidenceSpec"`. Assert group order and counts of 1, 4 and 2 in the four-row case, separation across sources/windows, missing details, and literal display of synthetic XML/HTML special characters.

- [ ] **Final checkpoint.** Run `sbt --client "cli/test"` with only synthetic fixtures available. On a desktop, use the newly built CLI to run `atbp dmarc reports/` against synthetic reports and confirm the default browser opens the file at the printed path. Review its summary and trace one failure to its input.
- [ ] **Before each Scala commit**, stage all intended files including new sources, run `sbt --client fixup && git status`, stage any intended changes and repeat until fixup succeeds and the same status check shows no outstanding work for the commit. Include its formatting in that commit. Follow [AGENTS.md](../AGENTS.md) for build-definition edits.

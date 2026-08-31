## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes are located outside the package. The first is `New submission`, from
the CRAN incoming checks, and cannot be avoided. The second is
`Skipping checking HTML validation: no command 'tidy' found`, a property of this
machine — it has no `tidy` — and not of the manual: the win-builder runs, on
machines that do have `tidy`, have reported `checking HTML version of
manual ... OK` on every earlier upload. A rerun with
`_R_CHECK_CRAN_INCOMING_=false --no-manual` is kept alongside the other logs to
show that removing the incoming checks and the manual validation removes both
notes and nothing else appears.

## A check that CRAN runs, and that this package now runs first

```sh
_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran lupa_0.1.0.tar.gz
```

Result on these sources: **`Status: 1 NOTE`**, the new-submission note and nothing
else — the same result as the ordinary check, which is the point: removing the
optional packages changes nothing.

This runs before any external service and is the first step of the release script.
An earlier revision passed in eight environments and still failed this one with
`1 ERROR` and 68 test failures: twenty-two test blocks asserted behaviour that
depends on `stringdist` without declaring it with `skip_if_not_installed()`. All
eight of those environments had the optional packages installed, so none could see
the gap.

Eight green environments are not eight different environments if all eight have the
same packages installed.

**And the same gap reopened, in a smaller way, without this section noticing.**
Rebuilding the environment matrix on 2026-08-21 found this check at `1 ERROR`
again, with ten failures: nine in one test file that carried no
`skip_if_not_installed()` at all and whose assertions read the output of the
`stringdist`-backed vocabulary detector, and one asserting a DBI error message
that a machine without DBI never produces. The failures predate the current
revision, so this paragraph had been claiming a result it no longer had. Both
files now guard per test rather than per file, so the blocks that do run without
the optional packages still run.

**It reopened a third time on 2026-08-24, in a new test file, and this check was
again the only thing that saw it.** Six test blocks exercising approximate
duplicate detection were written; `skip_if_not_installed("stringdist")` was added
to one of them — the last one written. The local suite reported `FAIL 0` with
16 064 passing checks, and the three continuous-integration workflows reported
success, because all of them have the optional packages installed. With
`_R_CHECK_DEPENDS_ONLY_=true` the same sources gave **`1 ERROR` with 7 failures**,
all in that one file, the first of them a `subscript out of bounds` on a table
that comes back empty when the package is absent. After guarding all six, the
same check gives **`Status: 1 NOTE`** with `FAIL 0 | SKIP 278`, six more skips
than before — which is the number of blocks now declining to run, and the check
that the guard is real rather than decorative.

The recurrence is worth stating plainly: the note in the working memory of this
project already recorded the first occurrence, with this same optional package,
and it did not prevent the third. The guard is missed on the blocks written
*first*, because portability is what one thinks about at the end. So the rule is
no longer "remember to add it" but a step: on creating a test file, check whether
it touches a `Suggests` package and guard **every** block, and verify with this
command rather than with the local suite, which structurally cannot see it.

**And this check turns out not to be equivalent to the environment it stands in
for.** Running the suite in a container where `bit64` genuinely has no build,
two tests failed that pass here — both of them tests *about* `bit64` being
absent, which built their fixture in a way that needed `bit64` present to
construct it. This check had hidden `bit64` well enough for
`skip_if_not_installed()` to skip eight other tests, and still those two ran and
passed. A check that removes packages from the library path is close to, but not
the same as, a machine that never had them.

The lesson is about this letter and not about those tests. A section stating a
check result is exactly as verifiable — and as forgettable — as a code comment
stating that something is fast. It is now re-run for every revision rather than
carried forward.

## Test environments

Each result below names the commit whose sources it measured. The sources are
identified by their commit, not by the `Packaged:` stamp: `R CMD build` writes
that stamp at build time, so two builds of identical sources carry different
stamps and the stamp identifies a build, not a revision. Each result was read
from that run's own check log. Where a run has not yet been repeated on the
current sources, the line says so rather than carrying the older result forward.

The package sources submitted are those of `d837225`.

**This paragraph was wrong until it was re-run.** It claimed the sources were
those of `375d7c3` and that everything committed after it touched only this
letter, leaving the package byte-identical. Running the very command it cites
returns `28 files changed, 1565 insertions(+), 439 deletions(-)`, of which 23
travel in the tarball, six of them under `R/`. The claim had been carried
forward across a day of work that rewrote the tie-breaking of a trimming step,
changed a default from a bounded sample to the whole table, and added progress
reporting.

It is recorded rather than quietly corrected because the failure is the subject
of the section above: a statement of fact in prose ages silently, and the
reproducer being written next to it is not the same as the reproducer being run.
The command is now run for each revision and its output pasted, not summarised.

* Local: R 4.6.1, x86_64-pc-linux-gnu, Pop!_OS 22.04 LTS — **`Status: 1 NOTE`**
  with `--as-cran`, the note being `New submission`, and the same single note
  with `_R_CHECK_DEPENDS_ONLY_=true`. With `_R_CHECK_CRAN_INCOMING_=false` the
  result is **`Status: OK`**, no notes at all, which locates that note in the
  incoming checks rather than in the package.
* Local, `_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran` on the tarball
  built from the submission tree — **`Status: 2 NOTEs`**, the two described
  above and nothing else. (`cran-comments.md` is build-ignored, so a commit
  that touches only this letter does not change the sources; the run is
  repeated by the release script on the final commit and its log replaces the
  one cited here.) This is the check that CRAN's environment variable runs, the one
  that has caught what twenty-two thousand green tests structurally cannot see;
  its log is kept with the other runs of this revision. The tarball build
  rebuilds all nine vignettes without a warning.
* Continuous integration (GitHub Actions, `R-CMD-check`, run 33342198219) **on
  `d837225`, the submitted sources**: 5 of 5 with **`Status: OK`** and test
  summaries of `FAIL 0` with 22 124–22 128 passing checks — Ubuntu with R
  release, R-devel and R oldrel-1; Windows with R release; macOS with R release
  on `aarch64-apple-darwin23`. Each `Status:` line and each test summary was
  read from that run's own log rather than inferred from the green tick.
* R-hub v2, R-devel (run 33342198329) **on `d837225`, the submitted sources**:
  Linux, Windows and macOS — all three **`Status: OK`**. Each result was read
  from that job's own log. Earlier revisions of this letter carried an R-hub
  result measured one commit behind the submission and said so; this one is
  measured on what is submitted.
* win-builder, R release (4.6.1) and R-devel (r90452), uploaded from `d837225`
  on 2026-08-31: **`Status: 1 NOTE` on both**, and the note is `New submission`
  and nothing else. Read from each run's own `00check.log` (`TeCC2H9YPP1c` and
  `7WGhSWm53Zru`), not from the notification e-mail. `checking tests ... OK` at
  109 and 111 minutes respectively, and `checking HTML version of manual ... OK`
  on both.

  **An earlier upload of this same revision cycle failed here, and only here.**
  The release machine's check died 29 minutes in with no test summary; nine
  other environments were green. The cause was in the package: to decide
  whether a raw column is WKB, bytes were handed to GDAL through `sf`, and a
  `tryCatch` cannot catch a segfault in C. That build of GDAL crashed where
  every other one raised an error. The fix validates the WKB header
  arithmetically before `sf` is ever called, so garbage bytes never reach GDAL;
  the test that used to feed them now asserts that `st_as_sfc` is not called.
  The rerun above — full suite, 109 minutes, `1 NOTE` — is the measurement that
  the crash is gone. It is recorded here because a result that failed and was
  fixed is part of what was measured, and because win-builder release was the
  only environment of ten able to see it.
* Container: R 4.1.3 (`rocker/r-ver:4.1.3`) for the declared minimum, with the
  suggested packages installed from a period-appropriate CRAN snapshot and the
  full test suite running: **`FAIL 0 | WARN 0 | SKIP 21 | PASS 22086`** on
  `d837225`, the submitted sources. The check reports one ERROR and one WARNING, both from
  `checking PDF version of manual`: the image has no `pdflatex`. They are
  properties of the container — every external service above builds the manual
  and reports OK. Of its three NOTEs, one is `New submission`, one is the
  future-timestamp note from the container clock, and one says
  `found 1 marked UTF-8 string`: that string is **`Paysandú`** in the department
  column of the example data. It is correctly marked, R 4.6.1 reports `OK` for
  that same check, and a package about the quality of Uruguayan data has to be
  able to write that name the way it is written.
* The engine matrix — the full profile against PostgreSQL 16 and 9.3.25,
  MariaDB 11.8, MySQL 8.4, SQLite, DuckDB and SQL Server 2022, all real engines
  — reports **7 of 7 measured** on `d837225`, regenerated by the script that
  refuses to print a row without a log.

## Implementation notes

This release includes a pure-R encoding-repair engine that follows the design and
frozen data of [ftfy 6.3.1](https://github.com/rspeer/python-ftfy) by
[Robyn Speer](https://github.com/rspeer). The frozen character tables, badness
rules and byte-level transcoders are documented in `LICENSE.note`; the package is
GPL-3-only, with the ftfy-derived material identified under Apache-2.0. The
upstream ftfy release has no NOTICE file. The package also redistributes a small
frozen sentinel vector from [naniar 1.1.0](https://github.com/njtierney/naniar);
its MIT copyright and license notice are recorded in `LICENSE.note`. Five
deliberate departures from ftfy are declared in `NEWS.md`.

A diagnostic that cannot run is never reported as a finding about the data. When
an optional package is absent, when a date-time column carries no declared time
zone, or when a vocabulary comparison is truncated or does not apply, the profile
records the fact in `cobertura_diagnosticos` — a table separate from `hallazgos`
and outside the ordered `ok < sospechoso < error` severity scale — and the
per-column scope field is `NA` rather than zero. A diagnostic that did run and
found nothing is reported at severity `ok` with zero affected units, never as a
suspicion. A profile with no findings and a non-empty `cobertura_diagnosticos` is
therefore not a clean profile, and the documentation says so where an automated
consumer will read it.

The same rule now covers the pattern diagnostic. A column whose shape varies by
nature — names, addresses, free text — has no dominant pattern reaching the
threshold, so the diagnostic does not apply; that non-measurement is recorded in
`cobertura_diagnosticos` with the observed proportion and the argument that
controls it, rather than leaving the reader to infer that the column is clean. The
evidence of a pattern finding states the proportion of the dominant pattern and how
many rows fall in non-dominant patterns that were excluded for exceeding the
rareness threshold, so that a count of thirty-two affected rows in a column with a
hundred and sixty corrupt ones cannot be read as the whole story.

Findings that name rows now derive those rows from what the detector decided rather
than recomputing the criterion. Where the two used to be computed separately they
could disagree silently, because nothing compared the evidence against the indices.
A guard walks the findings and raises a condition of class
`lupa_trazabilidad_incoherente` when a count and its trace cannot be reconciled; it
compares the pre-truncation total, works in both directions, and respects the
declared unit. The finding is kept: warning is not a reason to hide the evidence.
The test suite checks identities and not only counts — fixtures build tables whose
corrupted row indices are known in advance and assert hits, false positives and
misses across the canonical finding vocabulary. The current count is **56
`tipo_hallazgo` names**: 53 types constructed by `.nuevo_hallazgo()` and three
additional duplicate-finding names constructed by the approximate-duplicate
detector (`duplicados_aproximados`, `duplicados_exactos_columnas` and
`duplicados_exactos_normalizados`).

Aggregated measurements carry an explicit orientation — `conformidad`, `defecto`
or `no_aplica` — because a `0.006` proportion of duplicated entities and a
`0.999` proportion of non-null cells are both valid results that must not be read
the same way. `indice_calidad()` returns a single number only when the caller
declares the weights; without them it returns the dashboard. The index always
travels with its coverage of the declared framework, records the weights used,
states which components were inverted because their orientation is `defecto`, and
warns that its components come from different universes. The package ships no
default weights and computes no global score of its own.

The package ships a battery of clean tables as a regression test: thirty-one
tables of correct data covering the idioms a naive detector confuses — names with
commas, addresses, decimal commas, dates stored as text, sequential identifiers,
zero-padded codes, e-mail addresses, URLs, a single currency, a single unit and
accented Spanish. The test asserts that no finding of severity `error` is raised
on any of them and enumerates, one by one, the findings above `ok` whose claim is
true of the data, so that a new false positive makes the suite fail.

The internal `.con_rng_interno_lsh()` uses a fixed seed so that MinHash and LSH
remain reproducible without depending on the caller's RNG configuration. It
captures `RNGkind()` and `.Random.seed`, restores both with `on.exit()`, and
leaves the caller's state unchanged; the regression checks exercise both
properties. The `<<-` assignments in the LSH and profiling closures update only
their enclosing function environments, never `.GlobalEnv`, and accumulate local
state across callbacks.

Tests that assert a wall-clock budget are skipped on CRAN and on continuous
integration, where a shared runner cannot support the claim; the structural
assertions they used to carry — that sampling activates, that the result stays
within a memory bound — run everywhere.

The declared minimum R version is 4.1.0, and the reason `cli` carries a floor of
its own came out of running against an old snapshot: there `cli` resolves to
2.0.2, which does not export `cli_progress_bar()`, `cli_progress_update()` or
`cli_progress_done()`; those arrived in cli 3.0.0. `DESCRIPTION` declares
`cli (>= 3.0.0)` so the requirement is stated rather than assumed.

Runs against old R releases also found three compatibility defects, all fixed:
`utils::URLencode()` is scalar in R 3.6, so `.escapar_clave()` applies it element
by element; factor columns reached text-oriented methods, so all operational
metric inputs pass through a factor-to-character boundary while profiling retains
the declared factor type; and the Date-to-POSIXct historical conversion sets
`tzone` to `UTC` explicitly.

The quality frameworks shipped with the package are taxonomies, not measurements.
`marco_cepal()` declares the four levels and nineteen principles of the United
Nations National Quality Assurance Framework as adapted for Latin America and the
Caribbean by the CEA/CEPAL; thirteen of the nineteen are declared out of scope
for a table, because they concern the statistical system, the institution and the
process rather than the data, and `cobertura_analisis()` reports them as such.
None of the nineteen is reported as measured by profiling.

Package documentation, help pages and vignettes are written in Spanish, as
declared by `Language: es`. `DESCRIPTION`, `NEWS.md` and this file are in
English, and an English README is provided alongside the Spanish one.

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

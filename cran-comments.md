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

### The four URLs that the incoming check has flagged

`checking CRAN incoming feasibility` reaches the network, so it reports
different things on different machines, and we would rather list all of them
than have you find one. Across the environments below, three distinct outcomes
were observed on the same sources:

* the local run and win-builder's R release machine report only `New submission`;
* the R 4.1.0 container, which has network and an older `curl`, additionally
  lists the four URLs in the table below;
* win-builder additionally reported one `possibly invalid file URI`,
  `benchmark/perdida_lsh.md` from `README.md` — reported in that run's result
  notice, of which no log was kept. **That one was a real defect and
  is fixed**: `benchmark/` is in `.Rbuildignore`, so the link resolved on GitHub
  and pointed at nothing inside the package. It is now the absolute GitHub URL,
  and a pre-submission check refuses any relative `README.md` link whose target
  `.Rbuildignore` excludes. No local run had ever reported it, which is the
  clearest argument this letter can make for the environment table.

The four URLs, as `checking CRAN incoming feasibility` reports them in the
container run of these sources
(`verificacion/2026-09-16/contenedor-4.1.0.log`, the only environment of the
thirteen that has both network and a `curl` old enough to be refused):

| URL | where it is cited | what it cites | measured |
| --- | --- | --- | ---: |
| `doi.org/10.1109/SEQUEN.1997.666900` | `man/detectar_duplicados_aproximados.Rd` | Broder (1997), *On the resemblance and containment of documents* — the MinHash paper the approximate-duplicate detector implements | `202` |
| `www.iso.org/standard/35736.html` | `DESCRIPTION`, `man/lupa-package.Rd`, `man/marco_calidad.Rd` | ISO/IEC 25012:2008, the data quality model that `marco_calidad()` declares | `403` |
| `www.iso.org/standard/31531.html` | `man/validadores_formato.Rd` | ISO/IEC 7064:2003, the check-character system the format validators implement | `403` |
| `www.iso.org/iso-3166-country-codes.html` | `man/validadores_formato.Rd` | ISO 3166, the country codes that `validar_iso3166()` validates | `403` |

None of the four is a wrong address, and none is dead.

The DOI is written as `\doi{10.1109/SEQUEN.1997.666900}` in the Rd source, so it
is the resolver that is being asked, and the resolver redirects (`302`) to IEEE
Xplore, which answers `202 Accepted` to an automated client — a challenge
pending, not a missing document. The package's other two DOIs, resolved the same
way in the same minute, answer `200`, which locates the `202` at the publisher
and not in the reference.

The three `403`s come from iso.org, which refuses non-browser clients: the same
`403` with and without a browser user agent, so it is the server declining the
request and not a wrong address. Each is the standard's own landing page at the
body that publishes it, which is the address a reader needs and the only one
that stays correct — no mirror or vendor copy is authoritative for an ISO
standard. They are left as they are, and reported here rather than quietly
replaced by a link that answers `200` and cites something else.

## A check that CRAN runs, and that this package now runs first

> **Where the figures in this letter come from.** Everything in the three
> generated tables is read from a log that is named beside it and that stamps
> the commit it measured; a script writes those tables and refuses to write them
> at all if a figure has no log or if its log measured a different commit. The
> recurrences narrated below are a different kind of claim: they are the
> project's own history, and for several of them the log kept is the run
> **after** the fix, not the failing one. Where that is so, it is said. This
> distinction is made explicit because an earlier revision of this letter stated
> historical counts it could not back, and a reviewer who checks one figure and
> finds nothing has no way to tell which of the others are sound.

```sh
_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran lupa_0.1.0.tar.gz
```

Result on these sources: **`Status: 2 NOTEs`** — the new-submission note and the
missing-`tidy` note described above, and nothing else: the same two notes as the
ordinary check, which is the point. Removing the optional packages changes
nothing.

This runs before any external service and is the first step of the release
script. An earlier revision passed in eight environments and still failed this
one with `1 ERROR`: twenty-two test blocks asserted behaviour that depends on
`stringdist` without declaring it with `skip_if_not_installed()`. All eight of
those environments had the optional packages installed, so none could see the
gap.

Eight green environments are not eight different environments if all eight have
the same packages installed.

**And the same gap reopened, in a smaller way, without this section noticing.**
Rebuilding the environment matrix on 2026-08-21 found this check at `1 ERROR`
again: nine failures in one test file that carried no `skip_if_not_installed()`
at all and whose assertions read the output of the `stringdist`-backed
vocabulary detector, and one asserting a DBI error message that a machine
without DBI never produces. The failures predate the current revision, so this
paragraph had been claiming a result it no longer had. The log kept for that
date (`verificacion/2026-08-21/`) is the run **after** the guards were added,
and reports `Status: 1 NOTE` with `FAIL 0`; the failing run's log was not kept.
Both files now guard per test rather than per file, so the blocks that do run
without the optional packages still run.

**It reopened a third time on 2026-08-24, in a new test file, and this check was
again the only thing that saw it.** Six test blocks exercising approximate
duplicate detection were written; `skip_if_not_installed("stringdist")` was
added to one of them — the last one written. The local suite reported `FAIL 0`,
and the three continuous-integration workflows reported success, because all of
them have the optional packages installed. With `_R_CHECK_DEPENDS_ONLY_=true`
the same sources gave **`1 ERROR` with 7 failures**, all in that one file, the
first of them a `subscript out of bounds` on a table that comes back empty when
the package is absent. After guarding all six, the same check gives **`Status:
1 NOTE`** with `FAIL 0 | WARN 0 | SKIP 278 | PASS 15314`, six more skips than
before — which is the number of blocks now declining to run, and the check that
the guard is real rather than decorative.

The recurrence is worth stating plainly: the note in the working memory of this
project already recorded the first occurrence, with this same optional package,
and it did not prevent the third. The guard is missed on the blocks written
*first*, because portability is what one thinks about at the end. So the rule is
no longer "remember to add it" but a step: on creating a test file, check
whether it touches a `Suggests` package and guard **every** block, and verify
with this command rather than with the local suite, which structurally cannot
see it.

**And this check turns out not to be equivalent to the environment it stands in
for.** Running the suite in a container where `bit64` genuinely has no build,
two tests failed that pass here — both of them tests *about* `bit64` being
absent, which built their fixture in a way that needed `bit64` present to
construct it. This check had hidden `bit64` well enough for
`skip_if_not_installed()` to skip eight other tests, and still those two ran and
passed. A check that removes packages from the library path is close to, but not
the same as, a machine that never had them.

**A fourth occurrence, on 2026-09-01.** A test of
the new external-LSH executor asserted a computed result without
`skip_if_not_installed("stringdist")`; the local suite (22 872 passing checks)
and five continuous-integration platforms were green, and this check alone
failed with `1 ERROR`. The guard was added and the check re-run to
**`Status: 2 NOTEs`**. Four occurrences of the same gap, each caught by the same
single check, is the strongest argument this letter makes for running it.

**The minimum-version container caught a sibling class the same day**: one test
called `duckdb::duckdb(shared_home = FALSE)`, an argument added in newer duckdb
releases, so on the period-appropriate snapshot the connection failed with a raw
error. Every modern environment — local, five CI platforms, R-hub — was green.
The call is now the plain `duckdb::duckdb()` the sibling test already used. A
matrix row exists to see what the other rows cannot.

The lesson is about this letter and not about those tests. A section stating a
check result is exactly as verifiable — and as forgettable — as a code comment
stating that something is fast. It is now re-run for every revision rather than
carried forward.

## Test environments

Every row below measured the sources named at the head of the table. Those
sources are identified by their commit, not by the `Packaged:` stamp: `R CMD
build` writes that stamp at build time, so two builds of identical sources
carry different stamps and the stamp identifies a build, not a revision. Each
result was read from that run's own check log — not from a green tick, not from
the conclusion of a CI run, and not from the notification e-mail.

The package sources submitted are those of
`23342b41aa445f03957f3604f70381f96cff0cb0`. The tarball was built from a clean
tree at that commit, which is checkable rather than asserted:

```sh
git rev-parse 23342b4^{commit}   # the revision these sources come from
git status --porcelain            # no output: nothing uncommitted travels in the build
git diff --name-only 23342b4..HEAD   # only cran-comments.md, which .Rbuildignore keeps out
R CMD build --compact-vignettes=gs+qpdf lupa
```

The third command is the one that matters and it is why this letter can name a
commit that is not `HEAD`: everything committed after the named revision touches
only this file, and `.Rbuildignore` keeps this file out of the tarball, so the
sources being submitted are byte-identical to that commit. That claim used to be
made in prose and went stale twice; it is now a command whose output is empty or
the claim is false.

The environment table below is generated from the check logs of that commit by a
script, and written into this file between two markers rather than copied by
hand. It refuses to produce the table at all if any row lacks a log, if a log
does not say which commit it measured, or if it says a different one — each log
carries that commit as its first line, written by whatever produced it. That
last guard exists because on 2026-09-05 rows measured on two different commits
sat in the same table and nothing noticed.

**This paragraph was wrong until it was re-run.** It claimed the sources were
those of `375d7c3` and that everything committed after it touched only this
letter, leaving the package byte-identical. Running the very command it cites
returns `28 files changed, 1565 insertions(+), 439 deletions(-)`, of which 23
travel in the tarball, six of them under `R/`. The claim had been carried
forward across a day of work that rewrote the tie-breaking of a trimming step,
changed a default from a bounded sample to the whole table, and added progress
reporting.

**And it went stale a second time, the same way.** It was corrected to
`688c4e7` and then carried that commit for twenty-six more commits, across the
work of a full day, while the table beneath it was copied in by hand from the
generator's output. Generating the table and pasting it are two steps, and the
second is the one that gets skipped; so it is now one step. The generator writes
the table into this file between two markers, and it refuses to write anything
at all while any row still lacks a log — the rule that a row is worth its log
is enforced by the tool rather than remembered by the person running it.

It is recorded rather than quietly corrected because the failure is the subject
of the section above: a statement of fact in prose ages silently, and the
reproducer being written next to it is not the same as the reproducer being run.

**The table below is empty as this letter stands, and that is the generator
refusing rather than an omission.** The seven local rows are measured on the
revision named above — strict-dependency `--as-cran`, the check with incoming
checks disabled, the R 4.1.0 container and the suite inside it, the nine-step
local revalidation, the engine matrix against seven real engines, and the
utility battery. The six external rows — GitHub Actions across five platforms,
three R-hub environments and the two win-builder queues — have no log for this
revision, because those runs have not been made for it. The generator writes
nothing at all while any row lacks its log, so the letter carries no table until
those runs exist. This letter is not ready to be sent while this paragraph is
here.

<!-- MATRIZ:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 23342b4.
     Do not edit by hand: it is regenerated. -->

**Sources: `23342b41aa445f03957f3604f70381f96cff0cb0`.**

| environment | result | log |
| --- | --- | --- |
| local, `_R_CHECK_DEPENDS_ONLY_=true` `--as-cran` | Status: 2 NOTEs | `verificacion/2026-09-16/check-depends-only.log` |
| local, `_R_CHECK_CRAN_INCOMING_=false` `--no-manual` | Status: OK | `verificacion/2026-09-16/check-sin-incoming.log` |
| local, container R 4.1.0 (the declared minimum) | Status: 1 ERROR, 1 WARNING, 3 NOTEs | `verificacion/2026-09-16/contenedor-4.1.0.log` |
| full test suite inside the R 4.1.0 container | [ FAIL 0 \| WARN 0 \| SKIP 38 \| PASS 23963 ] | `verificacion/2026-09-16/contenedor-4.1.0-suite.log` |
| local revalidation, all nine steps | Revalidacion local en verde | `verificacion/2026-09-16/revalidacion-completa.log` |
| engine matrix against real engines | motores medidos: 7 de 7 | `verificacion/2026-09-16/matriz-motores.log` |
| utility battery (recall and precision) | recall: 9 de 9 | `verificacion/2026-09-16/bateria-utilidad.log` |
| GitHub Actions, 5 platforms (macOS release included) | platforms with Status: OK: 5 of 5; FAIL 0 \| WARN 0 on all of them; PASS from 24353 to 24414 | `verificacion/2026-09-16/actions-5-plataformas.log` |
| R-hub v2, Linux (R-devel) | Status: OK | `verificacion/2026-09-16/rhub-linux.log` |
| R-hub v2, Windows (R-devel) | Status: OK | `verificacion/2026-09-16/rhub-windows.log` |
| R-hub v2, macOS x86_64 (R-devel) | Failed to build source package s2. | `verificacion/2026-09-16/rhub-macos.log` |
| win-builder release | Status: 1 NOTE | `verificacion/2026-09-16/winbuilder-release.log` |
| win-builder devel | Status: 1 NOTE | `verificacion/2026-09-16/winbuilder-devel.log` |

> The container's `WARNING` and `ERROR` come from two consecutive
> steps: `checking PDF version of manual` reports the `WARNING`, and
> `checking PDF version of manual without hyperrefs or index` reports
> the `ERROR`. The log names the cause inside the second one: that
> image has no LaTeX — `texi2dvi script/program not available` and
> `pdflatex is not available`. Its NOTEs are the container too —
> "unable to verify current time" because it has no network, and the
> `lupa-manual.tex` left by the failed PDF build. They are properties
> of the image and not of the package — the suite inside it reports
> `FAIL 0` — and they are stated here rather than left for you to find.
>
> **A correction this letter owes itself.** An earlier revision of this
> note said the container's test warnings were `duckdb`'s
> garbage-collection notices, because the image pins `duckdb 0.8.0`.
> That was wrong, and measuring it is what showed it: they were **our own
> tests** asking for `LC_CTYPE=es_UY.UTF-8` on machines that do not have
> it. `Sys.setlocale()` warns rather than fails, and `try(silent = TRUE)`
> silences errors, not warnings. With that fixed the container reports
> `WARN 0`, and so do all five CI platforms. The `duckdb` notices do
> exist, but at session exit, outside any test, in both the before and the
> after. The claim is corrected rather than quietly dropped: an explanation
> stated with confidence and later found false is worth more as a
> correction than as a deletion.
>
> **And a second correction, on 2026-09-17.** This same note used to say
> the `ERROR` and the `WARNING` came from the SAME step and that the
> image had no `texi2dvi`. The log shows two distinct steps, and names
> the missing `pdflatex` as well. It is the same failure as the one
> above — explaining a cause the log does not show — found the same way,
> by reading the log against the claim instead of trusting the claim.
>
> The R-hub macOS x86_64 row **is not a result about the package**: the
> check never ran there. R-devel is 4.7 and CRAN does not yet publish
> macOS binaries for 4.7 — `contrib/4.7/PACKAGES` answers 404 — so
> `pak` built `s2` from source and its bundled `abseil-cpp` came out
> missing a symbol at link time. `s2` arrives through `sf`, which is in
> Suggests and is always reached behind `requireNamespace()`. macOS with
> R-release **is** measured, in the GitHub Actions row. The row is left
> written rather than omitted.
>
> Three cells quote, verbatim, the wording of the local tool that wrote
> that log; they are reproduced rather than paraphrased, so that the cell
> and its log say the same thing.
>
> The external-service rows are filled in by reading each run's own
> `00check.log`, **not** its conclusion nor the time its notice arrived.

<!-- MATRIZ:FIN -->

### What the table cannot fit

> The win-builder identifiers in this section are the ones its result notice
> carries. Of the three, only `TeCC2H9YPP1c` has a log kept in `verificacion/`;
> the other two runs were read from their notices and their logs were not kept.
> The rows of the table above are a different matter: every one of them names a
> log that exists and that stamps the commit it measured.

**The win-builder release row is itself the closing measurement of a failure
this cycle.** One revision earlier, the release check there — and there alone,
out of ten environments — died 29 minutes into the tests with no summary
(`x84xirE35eoq`), its output ending at `OGR: Corrupt data`: that machine's GDAL
build segfaults, rather than erroring, on a WKB whose header is valid and whose
body is truncated, and a `tryCatch` cannot catch a segfault in C. The same
machine's R-devel, on a different GDAL build, ran the same sources to `1 NOTE`
(`Qfj1g7YA3q91`) — the crash was specific to the release build. The fix extends
the arithmetic WKB guard with a minimum body length per geometry type and adds
an equivalent arithmetic guard for WKT, so corrupt bytes and corrupt text are
rejected by the package's own arithmetic and never handed to GDAL at all; the
two tests that used to feed them now assert, with a mock, that `sf` receives
nothing.

**An earlier revision cycle failed on that same machine, and only there.** The
cause was in the package: to decide whether a raw column is WKB, bytes were
handed to GDAL through `sf`. That build of GDAL crashed where every other one
raised an error. The fix of that cycle validated the WKB header arithmetically
before `sf` was ever called, and its 109-minute rerun (`TeCC2H9YPP1c`, `1 NOTE`)
measured that crash gone. The header alone later proved insufficient — the
truncated-body case above went through it — which is why the guard now also
demands the minimum body length. Twice now, this machine has been the only one
of ten able to see this class; both failures and both closing measurements are
recorded here, because a result that failed and was fixed is part of what was
measured.

**The minimum-version container row earned its place on this very revision.**
One test called `duckdb::duckdb(shared_home = FALSE)`, an argument added in
newer duckdb releases, so on the period-appropriate snapshot the connection
failed with a raw error. Every modern environment — local, five CI platforms,
R-hub — was green. The call is now the plain `duckdb::duckdb()` that the sibling
test already used. A matrix row exists to see what the other rows cannot.

The engine matrix runs the full profile against PostgreSQL 16 and 9.3.25,
MariaDB 11.8, MySQL 8.4, SQLite, DuckDB and SQL Server 2022 — all real engines,
none of them mocked.

## Implementation notes

This release includes a pure-R encoding-repair engine that follows the design
and frozen data of [ftfy 6.3.1](https://github.com/rspeer/python-ftfy) by
[Robyn Speer](https://github.com/rspeer). The frozen character tables, badness
rules and byte-level transcoders are documented in `LICENSE.note`; the package
is GPL-3-only, with the ftfy-derived material identified under Apache-2.0. The
upstream ftfy release has no NOTICE file. The package also redistributes a
small frozen sentinel vector from [naniar
1.1.0](https://github.com/njtierney/naniar); its MIT copyright and license
notice are recorded in `LICENSE.note`. Five deliberate departures from ftfy are
declared in `NEWS.md`.

A diagnostic that cannot run is never reported as a finding about the data.
When an optional package is absent, when a date-time column carries no declared
time zone, or when a vocabulary comparison is truncated or does not apply, the
profile records the fact in `cobertura_diagnosticos` — a table separate from
`hallazgos` and outside the ordered `ok < sospechoso < error` severity scale —
and the per-column scope field is `NA` rather than zero. A diagnostic that did
run and found nothing is reported at severity `ok` with zero affected units,
never as a suspicion. A profile with no findings and a non-empty
`cobertura_diagnosticos` is therefore not a clean profile, and the
documentation says so where an automated consumer will read it.

The same rule now covers the pattern diagnostic. A column whose shape varies by
nature — names, addresses, free text — has no dominant pattern reaching the
threshold, so the diagnostic does not apply; that non-measurement is recorded
in `cobertura_diagnosticos` with the observed proportion and the argument that
controls it, rather than leaving the reader to infer that the column is clean.
The evidence of a pattern finding states the proportion of the dominant pattern
and how many rows fall in non-dominant patterns that were excluded for
exceeding the rareness threshold, so that a count of thirty-two affected rows
in a column with a hundred and sixty corrupt ones cannot be read as the whole
story.

Findings that name rows now derive those rows from what the detector decided
rather than recomputing the criterion. Where the two used to be computed
separately they could disagree silently, because nothing compared the evidence
against the indices. A guard walks the findings and raises a condition of class
`lupa_trazabilidad_incoherente` when a count and its trace cannot be
reconciled; it compares the pre-truncation total, works in both directions, and
respects the declared unit. The finding is kept: warning is not a reason to
hide the evidence. The test suite checks identities and not only counts —
fixtures build tables whose corrupted row indices are known in advance and
assert hits, false positives and misses across the canonical finding
vocabulary. The current count is **58 `tipo_hallazgo` names**: 55 types
constructed by `.nuevo_hallazgo()` and three additional duplicate-finding names
constructed by the approximate-duplicate detector (`duplicados_aproximados`,
`duplicados_exactos_columnas` and `duplicados_exactos_normalizados`).

Aggregated measurements carry an explicit orientation — `conformidad`,
`defecto` or `no_aplica` — because a `0.006` proportion of duplicated entities
and a `0.999` proportion of non-null cells are both valid results that must not
be read the same way. `indice_calidad()` returns a single number only when the
caller declares the weights; without them it returns the dashboard. The index
always travels with its coverage of the declared framework, records the weights
used, states which components were inverted because their orientation is
`defecto`, and warns that its components come from different universes. The
package ships no default weights and computes no global score of its own.

The package ships a battery of clean tables as a regression test: thirty-one
tables of correct data covering the idioms a naive detector confuses — names
with commas, addresses, decimal commas, dates stored as text, sequential
identifiers, zero-padded codes, e-mail addresses, URLs, a single currency, a
single unit and accented Spanish. The test asserts that no finding of severity
`error` is raised on any of them and enumerates, one by one, the findings above
`ok` whose claim is true of the data, so that a new false positive makes the
suite fail.

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
`utils::URLencode()` is scalar in R 3.6, so `.escapar_clave()` applies it
element by element; factor columns reached text-oriented methods, so all
operational metric inputs pass through a factor-to-character boundary while
profiling retains the declared factor type; and the Date-to-POSIXct historical
conversion sets `tzone` to `UTC` explicitly.

The quality frameworks shipped with the package are taxonomies, not
measurements. `marco_cepal()` declares the four levels and nineteen principles
of the United Nations National Quality Assurance Framework as adapted for Latin
America and the Caribbean by the CEA/CEPAL; thirteen of the nineteen are
declared out of scope for a table, because they concern the statistical system,
the institution and the process rather than the data, and
`cobertura_analisis()` reports them as such. None of the nineteen is reported
as measured by profiling.

Package documentation, help pages and vignettes are written in Spanish, as
declared by `Language: es`. `DESCRIPTION`, `NEWS.md` and this file are in
English, and an English README is provided alongside the Spanish one.

### Check time, measured rather than estimated

The test suite is long, and we would rather state the numbers than have you find
them. `checking tests`, read from each run's own log — R-hub does not set
`NOT_CRAN`, so it skips what CRAN skips. **This table is generated from those
logs by the same script that generates the environment matrix**, and it refuses
to be written at all if a figure has no log behind it or if its log measured a
different commit:

<!-- TIEMPOS:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 23342b4.
     Do not edit by hand: it is regenerated. -->

| environment | `checking tests` | log |
| --- | ---: | --- |
| R-hub, Linux, R-devel | [17m/17m] | `verificacion/2026-09-16/rhub-linux.log` |
| R-hub, Windows, R-devel | [24m] | `verificacion/2026-09-16/rhub-windows.log` |
| this machine, `_R_CHECK_DEPENDS_ONLY_=true --as-cran` | [326s/326s] | `verificacion/2026-09-16/check-depends-only.log` |

| the suite behind those runs | result | log |
| --- | ---: | --- |
| full suite, this machine | BLOQUES: 1628 FALLOS: 0 ERROR: 0 SKIP: 2 PASS: 24462 | `verificacion/2026-09-16/revalidacion-completa.log` |

> These figures are **not** a guarantee about your machines: they are what
> each run's own log reports, and they move with the load of the machine
> that produced them. The local one is compared against a declared ceiling
> by the revalidation, so it cannot grow unnoticed.

<!-- TIEMPOS:FIN -->

**This table used to compare a "before" against a "now", and the comparison had
gone stale.** It claimed 9m on R-hub's Linux builder; that builder, on these
sources, reports the figure in the table above. It also carried a macOS figure,
and macOS did not run this cycle at all. The earlier pair of numbers was true of
an earlier revision and was carried forward across a suite that kept growing —
which is the same failure this letter documents twice above, in a third place.
The table now states one column, measured on the sources being submitted, and
names the log it came from.

The cut those figures used to describe is still real: four blocks no longer run
under `R CMD check` on CRAN. They were chosen by profiling the suite block by
block, not by guessing — one of them alone accounted for 24 % of the whole
suite. None of them was shortened: shortening the largest would have turned a
boundary test into a comfortable one, since its 2,000 forms of 83 characters sit
just below both of the detector's budget ceilings. They run in full on
continuous integration and in the local revalidation on every revision, and one
was split so the property it guards is still checked on CRAN with a 300-form
fixture that takes 1.6 seconds.

What remains is genuine coverage, spread across the suite at about a second and
a half per file. A local guard fails the revalidation if `checking tests` goes
above a declared ceiling of 400s, so this does not quietly grow again; the
figure it compared against that ceiling on these sources is the local row of the
table above, taken from the same log.

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

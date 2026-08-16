## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* Some check environments do not provide the external `tidy` executable, so R
  skips HTML manual validation and emits a NOTE. Where `tidy` is available the
  check reports no such NOTE. Examples, tests, vignettes, the PDF manual and the
  self-contained HTML produced by the package are checked in every environment.

## Test environments

> **Not ready for submission.** The sources changed after the last full round of
> external checks. The local result below is from the current sources; the
> Windows and macOS builder runs were resubmitted for these same sources and
> their results must be recorded here before this package is uploaded.

Every result below is from a run of one build of these exact sources, with no
change to the package between them. `R CMD build` stamps `Packaged:` into
`DESCRIPTION`, so two builds of identical sources are never byte-identical; the
claim is about the sources, which is what can be checked.

* Local: R 4.6.1, x86_64-pc-linux-gnu, Pop!_OS 22.04 LTS — 0 errors, 0 warnings,
  2 notes (new submission; no `tidy` executable in this environment). Examples
  OK; tests OK in 98s; vignettes rebuilt; PDF manual OK.
* win-builder, R 4.6.1 and R-devel, x86_64-w64-mingw32 — resubmitted, result
  pending.
* macOS builder, R 4.6.1, macOS, arm64 (Apple M1) — resubmitted, result pending.
* R-hub v2, R-devel: Ubuntu Linux x86_64, Windows x86_64 and macOS x86_64 —
  pending.
* Continuous integration (GitHub Actions, `R-CMD-check`): Ubuntu with R release,
  R-devel and R oldrel-1, plus macOS and Windows with R release — pending.
* Container: R 3.6.3 (`rocker/r-ver:3.6.3`) for the declared minimum, against a
  2023-04-15 CRAN snapshot with `cli` 3.6.1, run with
  `--ignore-vignettes --no-tests --no-manual` and `_R_CHECK_FORCE_SUGGESTS_=false`
  — pending.

The snapshot date matters and is not arbitrary: `DESCRIPTION` declares
`cli (>= 3.0.0)`, and `cli` 3.0.0 was published in 2021, so any earlier CRAN
snapshot fails with `Package required and available but unsuitable version` and
cannot exercise the package at all.

The arm64 run matters for this package: an earlier revision classified duplicate
pairs by testing a floating-point distance for equality, which held on x86_64 and
failed on Apple silicon. The classification now compares the normalised strings
directly, so the result no longer depends on the architecture, and a regression
test forces a non-zero distance of 1e-16 to keep it that way.

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

The declared minimum R version is 3.6.0, and the R 3.6.3 run is why `cli` carries
a floor. Under an older snapshot `cli` resolves to 2.0.2, which does not export
`cli_progress_bar()`, `cli_progress_update()` or `cli_progress_done()`; those were
added in cli 3.0.0. `DESCRIPTION` declares `cli (>= 3.0.0)` so the requirement is
stated rather than assumed.

An earlier R 3.6.3 run found two further compatibility defects, both fixed:
`utils::URLencode()` is scalar in that release, so
`.escapar_clave()` now applies it element by element, and factor columns reached
text-oriented methods, so all operational metric inputs pass through a
factor-to-character boundary while profiling retains the declared factor type.
The Date-to-POSIXct historical conversion sets `tzone` to `UTC` explicitly.

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

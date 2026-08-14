## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.
* Some check environments do not provide the external `tidy` executable, so R
  skips HTML manual validation and emits a NOTE. Where `tidy` is available the
  check reports no such NOTE. Examples, tests, vignettes, the PDF manual and the
  self-contained HTML produced by the package are checked in every environment.

## Test environments

Every result below is from a run of this exact tarball.

* win-builder, R 4.6.1 (2026-06-24 ucrt), x86_64-w64-mingw32 — 1 NOTE
  (new submission); examples and tests OK.
* win-builder, R-devel (2026-08-13 r90397 ucrt), x86_64-w64-mingw32 — 1 NOTE
  (new submission); examples and tests OK.
* macOS builder, R 4.6.1, macOS 26.6, **arm64 (Apple M1)** — Status: OK, no
  errors, warnings or notes; examples and tests OK.
* R-hub v2, R-devel: Ubuntu Linux x86_64, Windows x86_64 and macOS x86_64 —
  Status: OK on all three, no errors, warnings or notes.
* Local: R 4.6.1, x86_64-pc-linux-gnu, Pop!_OS 22.04 LTS — 0 errors, 0 warnings,
  2 notes (new submission; no `tidy` executable in this environment).
* Continuous integration (GitHub Actions, `R-CMD-check`): Ubuntu with R release,
  R-devel and R oldrel-1, plus macOS and Windows with R release — 5 of 5 green.
* Container: R 3.6.3 (`rocker/r-ver:3.6.3`) for the declared minimum, against a
  2023-04-15 CRAN snapshot with `cli` 3.6.1, run with
  `--ignore-vignettes --no-tests --no-manual` and `_R_CHECK_FORCE_SUGGESTS_=false`
  — 0 errors, 0 warnings, 3 notes, and `checking examples` OK. The three notes
  are properties of that environment, not of the package: eleven suggested
  packages have no installable build for R 3.6 in that snapshot, one Rd
  cross-reference to `tibble` therefore cannot be resolved, and the shipped data
  contains one marked UTF-8 string. Vignettes, tests and the manual are checked
  under R 4.6.1 and on the services above.

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
an optional package is absent, or when a date-time column carries no declared
time zone, the profile records the fact in `cobertura_diagnosticos` — a table
separate from `hallazgos` and outside the ordered `ok < sospechoso < error`
severity scale — and the per-column scope field is `NA` rather than zero. A
profile with no findings and a non-empty `cobertura_diagnosticos` is therefore
not a clean profile, and the documentation says so where an automated consumer
will read it.

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

Package documentation, help pages and vignettes are written in Spanish, as
declared by `Language: es`. `DESCRIPTION`, `NEWS.md` and this file are in
English, and an English README is provided alongside the Spanish one.

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

## Resubmission

This is a resubmission. The incoming pre-tests archived the previous one with
"Overall checktime 13 min > 10 min" on r-devel-windows-x86_64, and Uwe Ligges
asked whether it could be reduced below 10 minutes. In this version:

* The fixed cost of profiling is lower. A table of date formats that was rebuilt
  for every column is now built once, at installation; PCRE regular expressions
  called thousands of times on short vectors are avoided where a fixed or TRE
  match gives the same result (on Windows each PCRE call costs about 47 us,
  against about 5 on Linux); and the masking of possible personal data, about
  40% of the cost of `perfilar()`, is now requested with
  `proteger_datos_personales = TRUE` instead of being on by default. On the
  13-row example table, `perfilar()` went from 0.63 s to 0.16 s and `analizar()`
  from 1.36 s to 0.33 s, and every example, test and vignette that profiles a
  table gains the same.
* The package ships vignettes 1 to 6, the basic path. Vignettes 7 to 12 are now
  articles on the package website, as the 4 reference articles already were.
* 38 more test files start with `testthat::skip_on_cran()`; they still run on
  continuous integration and on R-hub (see "Tests on CRAN" below).

<!-- RELOJ:INICIO -->
On win-builder the check time is now 492 s on R-devel (it was 795 s), 483 s on R
release and 334 s on R oldrelease, each with `Status: 1 NOTE` (the new
submission).
<!-- RELOJ:FIN -->

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The sources submitted are those of `7036f7e6fea83d6487034a8e925e07124dfe79c5`.

A local run additionally reports `Skipping checking HTML validation: no command
'tidy' found`, which is a property of that machine: the win-builder runs, which
have `tidy`, report `checking HTML version of manual ... OK`.

### Installed size

<!-- TAMANO:INICIO -->
On Linux the check reports `installed size is  5.2Mb`, of which `R` is 3.9Mb:
R 4.6 reports it as INFO and the R 4.2.0 container as a NOTE. On Windows the
win-builder runs, release and devel, report
`checking installed package size ... OK`.
<!-- TAMANO:FIN -->

The package has no compiled code and its two example datasets are under 1 kB
each: the R directory is the byte-compiled code (about 74,000 lines of code, 89
exported functions and 34 S3 methods), and the rest is the help pages and the
6 vignettes. The only figure in the tarball is the logo that `README.md`
shows.

### URLs that the incoming check may flag

`checking CRAN incoming feasibility` reaches the network, and some publishers
refuse automated clients. None of these addresses is wrong or dead; each is the
publisher's own page for the work it cites:

* `https://www.iso.org/standard/35736.html`,
  `https://www.iso.org/standard/31531.html` and
  `https://www.iso.org/iso-3166-country-codes.html` (ISO/IEC 25012, ISO/IEC 7064
  and ISO 3166) answer `403` to non-browser clients.
* `https://repositorio.cepal.org/handle/11362/47464` (the CEPAL data quality
  framework that `marco_cepal()` declares) answers `403` to non-browser clients,
  with and without a browser user agent.
* `https://doi.org/10.1109/SEQUEN.1997.666900` (Broder, 1997) resolves to IEEE
  Xplore, which answers `202 Accepted` to an automated client.

## Notes for the reviewer

* Language: the help pages, the vignettes and the messages are in Spanish
  (`Language: es`); the Title, the Description, `README.md` and this letter
  are in English. Argument names that matter for the policies: `archivo` is
  the file, `sobrescribir` is overwrite and `nucleos` is the number of cores.
  `reportar()` writes to `tempfile()` by default; `guardar_historico()` and
  `guardar_analisis()` have no default path and do not overwrite an existing
  file unless `sobrescribir = TRUE`; `nucleos` defaults to
  `getOption("lupa.nucleos", 2L)`.
* Acronyms in the Description: AGESIC, CEA and CEPAL are the Spanish acronyms
  of the Uruguayan Agency for Electronic Government and Information and
  Knowledge Society, of the Statistical
  Conference of the Americas and of the Economic Commission for Latin America
  and the Caribbean (ECLAC in English). The Description gives the full name of
  each institution in English and says that the acronyms are Spanish.
* Third-party material: `R/reparacion-texto.R` translates to R the byte
  tables, character categories, regular expressions and repair functions of
  ftfy 6.3.1
  (Apache-2.0, copyright Robyn Speer), and the missing-value sentinels are
  those of naniar 1.1.0 (MIT, copyright Nicholas Tierney). Both are listed in
  `Authors@R` as `ctb` and `cph`, the `Copyright` field says what each holds,
  and `inst/COPYRIGHTS` reproduces both notices.
* Random numbers: the functions in `R/` do not use R's random number
  generator and do not read or write `.Random.seed`. The MinHash family and
  the sample of the cost estimate come from a deterministic 32-bit integer
  hash.
* Global state: every `options()` and `Sys.setlocale()` change is undone on
  exit (`.fijar_numerica_c()` registers its `on.exit()` in the caller's frame).
  The `par()` in `R/agregacion.R` is a local helper, not `graphics::par()`.
  `<<-` is only used inside closures. `cat()` and `print()` are only used in
  print methods; all other output is a message or a warning condition.

## Test environments

<!-- MATRIZ:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 7036f7e6.
     Do not edit by hand: it is regenerated. -->

**Sources: `7036f7e6fea83d6487034a8e925e07124dfe79c5`.**

| environment | result | log |
| --- | --- | --- |
| local, `_R_CHECK_DEPENDS_ONLY_=true` `--as-cran` | Status: 2 NOTEs | `verificacion/2026-10-25-reenvio-c/check-depends-only.log` |
| local, `_R_CHECK_CRAN_INCOMING_=false` `--no-manual` | Status: OK | `verificacion/2026-10-25-reenvio-c/check-sin-incoming.log` |
| local, container R 4.2.0 (the declared minimum) | Status: 1 ERROR, 1 WARNING, 4 NOTEs | `verificacion/2026-10-25-reenvio-c/contenedor-4.2.0.log` |
| full test suite inside the R 4.2.0 container | [ FAIL 0 \| WARN 0 \| SKIP 65 \| PASS 33408 ] | `verificacion/2026-10-25-reenvio-c/contenedor-4.2.0-suite.log` |
| local revalidation, all nine steps | Revalidacion local en verde | `verificacion/2026-10-25-reenvio-c/revalidacion-completa.log` |
| engine matrix against real engines | motores medidos: 7 de 7 | `verificacion/2026-10-25-reenvio-c/matriz-motores.log` |
| utility battery (recall and precision) | bateria: recall 26 de 26 columnas \| falsos positivos no explicados: 0 | `verificacion/2026-10-25-reenvio-c/bateria-utilidad.log` |
| GitHub Actions, 4 platforms | platforms with Status: OK: 4 of 4; FAIL 0 \| WARN 0 on all of them; PASS from 33419 to 33504 | `verificacion/2026-10-25-reenvio-c/actions-plataformas.log` |
| GitHub Actions, macOS (deps without pak) | Status: OK | `verificacion/2026-10-25-reenvio-c/actions-macos.log` |
| R-hub v2, Linux (R-devel) | Status: OK | `verificacion/2026-10-25-reenvio-c/rhub-linux.log` |
| R-hub v2, Windows (R-devel) | Status: OK | `verificacion/2026-10-25-reenvio-c/rhub-windows.log` |
| R-hub v2, macOS x86_64 (R-devel) | Status: OK | `verificacion/2026-10-25-reenvio-c/rhub-macos.log` |
| win-builder release | Status: 1 NOTE | `verificacion/2026-10-25-reenvio-c/winbuilder-release.log` |
| win-builder devel | Status: 1 NOTE | `verificacion/2026-10-25-reenvio-c/winbuilder-devel.log` |

> The container's `WARNING` and `ERROR` come from two consecutive
> steps: `checking PDF version of manual` reports the `WARNING`, and
> `checking PDF version of manual without hyperrefs or index` reports
> the `ERROR`. The log names the cause inside the second one: that
> image has no LaTeX — `texi2dvi script/program not available` and
> `pdflatex is not available`. Its four NOTEs are the new submission
> with the URLs listed above, the installed size discussed above,
> "unable to verify current time", which R reports when its time service
> does not answer, and the `lupa-manual.tex` left by the failed PDF build.
> The `ERROR` and the `WARNING` are properties of the image and not of
> the package —the suite inside it reports `FAIL 0`— and they are stated
> here rather than left for you to find.
>
> Three cells quote, verbatim, the wording of the local tool that wrote
> that log; they are reproduced rather than paraphrased, so that the cell
> and its log say the same thing.
>
> **How each row is anchored.** The local rows and the container row carry
> the checksum of the tarball they measured, and the matrix refuses the row
> unless that tarball's sources match the declared commit, ignoring only the
> `Packaged:` stamp. They come from three runs —the revalidation, the
> container and the check without incoming checks—, each of which builds its
> own tarball from the commit, and the rows of one run must share it. The
> external rows cannot: their tarball is built
> by the service from the pushed commit and never exists here. Those rows
> are anchored by commit alone and say so in their own log; writing them a
> checksum derived from the commit would restate the commit under another
> name, which is the false assurance this check exists to prevent.
>
> The external-service rows are filled in by reading each run's own
> `00check.log`, **not** its conclusion nor the time its notice arrived.

<!-- MATRIZ:FIN -->

## Tests on CRAN

The full test suite runs on continuous integration and on R-hub, where
`NOT_CRAN` is `true`. To keep the check within CRAN's time, the regression tests
and every test file that takes more than 0.2 s in CRAN mode start with
`testthat::skip_on_cran()`, except the five that cover profiling, the cleaning
plan and its application, the HTML report, the package data and the `integer64`
guard. What runs on CRAN is those five plus the fast unit tests, among them the
locale and date tests. Examples and vignettes run on CRAN. Two vignette chunks
are shown but not evaluated: they call the interactive helpers `elegir_clave()`
and `guiar_limpieza()`, which wait for an answer.

<!-- TIEMPOS:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 7036f7e6.
     Do not edit by hand: it is regenerated. -->

| environment | `checking tests` | log |
| --- | ---: | --- |
| win-builder, R release | [79s] | `verificacion/2026-10-25-reenvio-c/winbuilder-release.log` |
| win-builder, R-devel | [77s] | `verificacion/2026-10-25-reenvio-c/winbuilder-devel.log` |
| this machine, Suggests installed, `--as-cran` | [15s/15s] | `verificacion/2026-10-25-reenvio-c/check-granja.log` |
| this machine, `_R_CHECK_DEPENDS_ONLY_=true --as-cran` | [13s/13s] | `verificacion/2026-10-25-reenvio-c/check-depends-only.log` |

| the suite behind those runs | result | log |
| --- | ---: | --- |
| full suite, this machine | BLOQUES: 2936 FALLOS: 0 ERROR: 0 SKIP: 4 PASS: 33805 | `verificacion/2026-10-25-reenvio-c/revalidacion-completa.log` |

> These figures are **not** a guarantee about your machines: they are what
> each run's own log reports, and they move with the load of the machine
> that produced them. The local one is compared against a declared ceiling
> by the revalidation, so it cannot grow unnoticed.

<!-- TIEMPOS:FIN -->

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

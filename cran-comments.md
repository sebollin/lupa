## Update

This is an update from 0.1.0, published on 2026-11-10. It is submitted soon
after because it corrects values that 0.1.0 publishes wrong. The NEWS file
lists more than a hundred corrections; the ones a user is most likely to meet are:

* **Cleaning plans.** `planificar_limpieza()` could propose converting a number
  written as text that the profile itself did not read as a number, such as a
  telephone number like `099.123.456` among amounts with a decimal comma, and
  `aplicar()` then wrote `99123456` into the data. That conversion is now
  blocked until the user decides what to do with those values.
  The action that turns numeric missing-value codes into `NA` carried the
  package's whole list of codes instead of the ones the profile had counted in
  that column, so ticking it also set a legitimate `999` in an identifier
  column to `NA`. With the default settings, when the finding names every code
  it counted, the action now carries only those and any the user declared.
* **Type inference.** Amounts with grouped thousands (`1.234,56`,
  `1,234.56`) were left as text, without a numeric summary, although the
  profile reported the column's convention.
* **Database profiles.** Some statistics came out wrong and were marked as
  computed: the median of extreme values on MariaDB, and the mode of decimals
  of 2^53 or more on PostgreSQL, SQL Server, MySQL, MariaDB and DuckDB. On
  PostgreSQL, a `NaN` in a column left its minimum, maximum, mean and standard
  deviation uncomputed; they are now computed over the finite values, as
  `perfilar()` does for a data frame.
* **Declared missing-value codes.** A value declared in `sentinelas_numericos`
  is now treated as missing by the functional dependencies and the
  associations too, as the documentation promised. In a text column it is
  recognized by the number the cell writes, so `"-999"`, `"-999.0"` and
  `"-9.99e2"` are the same `-999`, with the same result on every platform,
  including macOS on arm64.
* **Text read with the wrong encoding.** In UTF-8 text read as Latin-1, the
  second byte of many letters becomes an invisible character (a C1 control, a
  soft hyphen or a no-break space). The profile reported such a letter twice,
  as an invisible character and as broken encoding, and if the user deselected
  `reparar_codificacion` in the cleaning plan, the actions on invisible
  characters removed a C1 control or a soft hyphen, leaving the value
  unrepairable and no longer flagged, and replaced a no-break space with a
  plain space. For the letters the repair restores, those positions are now
  left to it. Cells declared `latin1` with one of the five bytes that
  Windows-1252 leaves undefined (0x81, 0x8D, 0x8F, 0x90, 0x9D) also made
  `recortar_espacios` fail; they are now read correctly.

<!-- RELOJ:INICIO -->
On win-builder the check time is 343 s on R-devel, 351 s on R release and 450 s
on R oldrelease, each with `Status: 1 NOTE`: the days since the last update,
and on R oldrelease also the four URLs listed below that answer `403` and one
that timed out. 0.1.0 took 492 s, 483 s and 334 s on another day, and these
times move with the load of each queue. On the same local machine, the check of
0.1.1 takes 14 s more in `checking R code for possible problems` (from 45 s to
59 s) and 11 s more in the tests (from 15 s to 26 s), with R 4.6.1 then and
R 4.6.2 now.
<!-- RELOJ:FIN -->

## R CMD check results

0 errors | 0 warnings | 1 note

* The incoming check notes the days since the last update: 0.1.0 was
  published on 2026-11-10. This update follows it so soon because it corrects
  values that 0.1.0 publishes wrong (see "Update" above).

The sources submitted are those of `c44ff236b143e56981f20b94a96f8d16a4ebe15e`.

A local run additionally reports `Skipping checking HTML validation: no command
'tidy' found`, which is a property of that machine: the win-builder runs, which
have `tidy`, report `checking HTML version of manual ... OK`.

### Installed size

<!-- TAMANO:INICIO -->
The local Linux check reports `installed size is  6.2Mb`, of which `R` is 4.7Mb:
R 4.6 reports it as INFO, and the R 4.2.0 container (`6.1Mb`) as a NOTE. The
three win-builder runs report `checking installed package size ... OK`. For
0.1.0 the local check reported 5.2Mb; the CRAN checks that print it report it
as INFO, from 5.1Mb to 5.6Mb, and every flavor has `Status: OK`.
<!-- TAMANO:FIN -->

The package has no compiled code and the files of its two example datasets
are under 1 kB each: the R directory is the byte-compiled code (about 88,000 lines of code, 89
exported functions and 34 S3 methods), and the rest is the help pages and the
6 vignettes. The only figures in the tarball are the two that `README.md`
shows: the logo and a 44 kB capture of the HTML report.

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
* `https://www.gnu.org/licenses/gpl-3.0.html` (the licence link in `README.md`)
  timed out for the R oldrelease run on win-builder; the R release and R-devel
  runs reached it.

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
  ftfy 6.3.1 (Apache-2.0, copyright Robyn Speer), and seven test fixtures are
  data exported from it; the exported `sentinelas_naniar` and the default
  missing-value strings reproduce the lists of naniar 1.1.0 (MIT, copyright
  Nicholas Tierney). Both are listed in
  `Authors@R` as `ctb` and `cph`, the `Copyright` field says what each holds,
  and `inst/COPYRIGHTS` reproduces both notices.
* Random numbers: the functions in `R/` do not use R's random number
  generator and do not read or write `.Random.seed`. The MinHash family and
  the sample of the cost estimate come from a deterministic 32-bit integer
  hash.
* Global state: every `options()` and `Sys.setlocale()` change is undone on
  exit (`.fijar_numerica_c()` registers its `on.exit()` in the caller's frame).
  The `par()` calls in `R/agregacion.R` and `R/acumuladores-bloques.R` are
  local helpers, not `graphics::par()`. `<<-` is only used inside closures.
  `cat()` and `print()` are only used in print methods and their internal
  helpers; progress bars come from cli's progress API, and all other output is
  a message or a warning condition. In an interactive session, `elegir_clave()`,
  `guiar_limpieza()` and the approximate-duplicate detection can ask a question
  with `readline()` or `utils::menu()`; in a non-interactive session none of
  them does.
* New in Suggests: `rJava`. When it is installed and the connection is a JDBC
  one, `perfilar_dbi()` reads the product name, driver name and version of the
  database from the connection's JDBC metadata to identify the engine; without
  it, the engine is identified from the connection class and
  `DBI::dbGetInfo()`.

## Test environments

<!-- MATRIZ:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of c44ff236.
     Do not edit by hand: it is regenerated. -->

**Sources: `c44ff236b143e56981f20b94a96f8d16a4ebe15e`.**

| environment | result | log |
| --- | --- | --- |
| local, `_R_CHECK_DEPENDS_ONLY_=true` `--as-cran` | Status: 2 NOTEs | `verificacion/2026-11-12/check-depends-only.log` |
| local, `_R_CHECK_CRAN_INCOMING_=false` `--no-manual` | Status: OK | `verificacion/2026-11-12/check-sin-incoming.log` |
| local, container R 4.2.0 (the declared minimum) | Status: 1 ERROR, 1 WARNING, 4 NOTEs | `verificacion/2026-11-12/contenedor-4.2.0.log` |
| full test suite inside the R 4.2.0 container | [ FAIL 0 \| WARN 0 \| SKIP 171 \| PASS 40187 ] | `verificacion/2026-11-12/contenedor-4.2.0-suite.log` |
| local revalidation, all nine steps | Revalidacion local en verde | `verificacion/2026-11-12/revalidacion-completa.log` |
| engine matrix against real engines | motores medidos: 7 de 7 | `verificacion/2026-11-12/matriz-motores.log` |
| utility battery (recall and precision) | bateria: recall 26 de 26 columnas \| falsos positivos no explicados: 0 | `verificacion/2026-11-12/bateria-utilidad.log` |
| GitHub Actions, 4 platforms | platforms with Status: OK: 4 of 4; FAIL 0 \| WARN 0 on all of them; PASS from 40197 to 40282 | `verificacion/2026-11-12/actions-plataformas.log` |
| GitHub Actions, macOS (deps without pak) | Status: OK | `verificacion/2026-11-12/actions-macos.log` |
| R-hub v2, Linux (R-devel) | Status: OK | `verificacion/2026-11-12/rhub-linux.log` |
| R-hub v2, Windows (R-devel) | Status: OK | `verificacion/2026-11-12/rhub-windows.log` |
| R-hub v2, macOS x86_64 (R-devel) | Status: OK | `verificacion/2026-11-12/rhub-macos.log` |
| win-builder release | Status: 1 NOTE | `verificacion/2026-11-12/winbuilder-release.log` |
| win-builder devel | Status: 1 NOTE | `verificacion/2026-11-12/winbuilder-devel.log` |

> The container's `WARNING` and `ERROR` come from two consecutive
> steps: `checking PDF version of manual` reports the `WARNING`, and
> `checking PDF version of manual without hyperrefs or index` reports
> the `ERROR`. The log names the cause inside the second one: that
> image has no LaTeX — `texi2dvi script/program not available` and
> `pdflatex is not available`. Its four NOTEs are the days since the last update
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

The full test suite runs on continuous integration, including macOS on arm64,
and on R-hub, where `NOT_CRAN` is `true`. To keep the check within CRAN's time,
most test files skip on CRAN with `testthat::skip_on_cran()`: in CRAN's
configuration (all Suggests installed, `NOT_CRAN` unset) 70 of the 525 test
files in the tarball run at least one assertion. Among them are the tests that
cover profiling, the cleaning plan and its application, the HTML report, the
package data and the `integer64` guard, and the locale and date tests. Examples
and vignettes run on CRAN. Two vignette chunks are shown but not evaluated: they
call the interactive helpers `elegir_clave()` and `guiar_limpieza()`, which wait
for an answer.

<!-- TIEMPOS:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of c44ff236.
     Do not edit by hand: it is regenerated. -->

| environment | `checking tests` | log |
| --- | ---: | --- |
| win-builder, R release | [77s] | `verificacion/2026-11-12/winbuilder-release.log` |
| win-builder, R-devel | [71s] | `verificacion/2026-11-12/winbuilder-devel.log` |
| this machine, Suggests installed, `--as-cran` | [26s/26s] | `verificacion/2026-11-12/check-granja.log` |
| this machine, `_R_CHECK_DEPENDS_ONLY_=true --as-cran` | [20s/20s] | `verificacion/2026-11-12/check-depends-only.log` |

| the suite behind those runs | result | log |
| --- | ---: | --- |
| full suite, this machine | BLOQUES: 3462 FALLOS: 0 ERROR: 0 SKIP: 53 PASS: 43120 | `verificacion/2026-11-12/revalidacion-completa.log` |

> These figures are **not** a guarantee about your machines: they are what
> each run's own log reports, and they move with the load of the machine
> that produced them. The local one is compared against a declared ceiling
> by the revalidation, so it cannot grow unnoticed.

<!-- TIEMPOS:FIN -->

## Reverse dependencies

0.1.0 was published on 2026-11-10 and has no reverse dependencies on CRAN.

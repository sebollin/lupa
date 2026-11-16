## Update

This is an update from 0.1.1, published on 2026-11-12. It follows it closely
because 0.1.1 stops with an error on data that users meet often (in the first
three cases below `perfilar()` and `analizar()` return nothing for the whole
table, and in the fourth the profile is built but cannot be printed), and
because its default cleaning plan writes wrong values into the data without a
warning:

* **Text read without declaring its encoding.** In a UTF-8 session, a CSV file
  written in Latin-1 or Windows-1252 and read with `read.csv()` without
  `fileEncoding` leaves unmarked strings with bytes that are not valid UTF-8 (a
  no-break space used as thousands separator in amounts, a degree sign). One
  such byte in a column of numbers written as text made `perfilar()` and
  `analizar()` stop with `input string N is invalid UTF-8`. The profile now
  describes the rest of the column and counts that cell as unreadable.
* **Year, month and day columns.** A table with year, month and day columns
  whose first row is not a date (a missing value is enough) made `perfilar()`
  and `analizar()` stop with "character string is not in a standard
  unambiguous format".
* **Lists with two delimiters.** A column with lists written with two
  delimiters, where the lists of one have a number of values that those of the
  other do not (`"12, 15, 18"` and `"16; 17"`), made `perfilar()` and
  `analizar()` stop with "subscript out of bounds".
* **Printing the findings.** With a column name that is not valid UTF-8 (what
  `data.table::fread()` leaves from a Latin-1 file with accented headers),
  printing `hallazgos()` or `cobertura()` stopped with "invalid multibyte
  string".
* **Numbers with a suffix or a code.** The default cleaning plan of 0.1.1
  converted to numbers values that carry a unit or a code, and `aplicar()`
  wrote the bare number into the data: `"1.5TB"` and `"500GB"` became `1.5`
  and `500`, `"850K"` became `850`, `"LEY 18331"` became `18331`, `"101a"`
  became `101` and `"12 KG"` became `12`. Under the default plan these values
  now stay as they are: no conversion is proposed for the first three columns;
  for codes like `101a`, in a column inferred as an identifier, it is proposed
  unticked; and a column of numbers with one such value, like `"12 KG"` next
  to `"1,5"` and `"2,5"`, is reported and its conversion is blocked.
* **Encoding repair.** Letters of well-written text that the repair heuristic
  took for mojibake are no longer rewritten: an upper-case word ending in `Ã`
  before a no-break space and another word came out with a lower-case `à`
  joined to the next word. The letters are now kept; the no-break space is
  still replaced by a plain space, by the plan's action on invisible
  characters.

Each of these was reproduced on 0.1.1 and checked on this version before
submitting. They are among the 68 corrections that the NEWS file lists, and
`perfilar()` is also about six to eight times faster on text columns with
numbers (measured on tables of 200,000 rows).

We are aware that this is the third release in about a week (0.1.0 on
2026-11-10, 0.1.1 on 2026-11-12). We did not want to leave a version on CRAN
that stops on common inputs and that changes the user's data under its default
plan. We do not plan another submission before the usual interval.

<!-- RELOJ:INICIO -->
On win-builder the check time is 567 s on R-devel and 547 s on R release,
each with `Status: 1 NOTE`: the days since the last update and the DeepWiki
URL listed below. 0.1.1 took 343 s and 351 s there on 2026-11-12. The
difference is the load of those queues, not the package: on R-devel every
timed step except the network one took 1.4 to 1.8 times as long (`checking R
code for possible problems` from 142 s to 254 s, the tests from 72 s to 118 s,
the PDF manual from 21 s to 30 s), and on the same local machine, with R 4.6.2
for both, this version takes 57 s in `checking R code for possible problems`
and 24 s in the tests, against 59 s and 26 s for 0.1.1.
<!-- RELOJ:FIN -->

## R CMD check results

0 errors | 0 warnings | 1 note

* The incoming check notes the days since the last update: 0.1.1 was
  published on 2026-11-12. This update follows it so soon because 0.1.1 stops
  on common inputs and its default plan writes wrong values (see "Update"
  above). On the runs that reach the network, the same NOTE also lists the
  DeepWiki URL discussed below.

The sources submitted are those of `64d9e762238e12af42a66ae57d56ca9786968ac2`.

A local run additionally reports `Skipping checking HTML validation: no command
'tidy' found`, which is a property of that machine: the win-builder runs, which
have `tidy`, report `checking HTML version of manual ... OK`.

### Installed size

<!-- TAMANO:INICIO -->
The local Linux check reports `installed size is  6.3Mb`, of which `R` is 4.8Mb:
R 4.6 reports it as INFO, and the R 4.2.0 container (`6.3Mb`) as a
NOTE. On win-builder, `checking installed package size` is `OK` on R-devel
and R release. For 0.1.1 the local check reported 6.2Mb; the CRAN
checks that print it report it as INFO, from 6.1Mb to 9.7Mb, and every flavor
has `Status: OK`.
<!-- TAMANO:FIN -->

The package has no compiled code and the files of its two example datasets
are under 1 kB each: the R directory is the byte-compiled code (about 90,000 lines of code, 89
exported functions and 35 S3 methods), and most of the rest is the help pages and
the 6 vignettes. The only figures in the tarball are the two that `README.md`
shows: the logo and a 44 kB capture of the HTML report.

### URLs that the incoming check may flag

`checking CRAN incoming feasibility` reaches the network, and some publishers
refuse automated clients. None of these addresses is wrong or dead; each is the
publisher's own page for what the package cites or links to:

* `https://www.iso.org/standard/35736.html`,
  `https://www.iso.org/standard/31531.html` and
  `https://www.iso.org/iso-3166-country-codes.html` (ISO/IEC 25012, ISO/IEC 7064
  and ISO 3166) answer `403` to non-browser clients.
* `https://repositorio.cepal.org/handle/11362/47464` (the CEPAL data quality
  framework that `marco_cepal()` declares) answers `403` to non-browser clients,
  with and without a browser user agent.
* `https://doi.org/10.1109/SEQUEN.1997.666900` (Broder, 1997) resolves to IEEE
  Xplore, which answers `202 Accepted` to an automated client.
* `https://deepwiki.com/sebollin/lupa` (the target of the DeepWiki badge in
  `README.md`, a generated wiki of the repository) answers `429 Too Many
  Requests` to automated clients; the local incoming check reports it.
* `https://www.gnu.org/licenses/gpl-3.0.html` (the licence link in `README.md`)
  timed out once, in the R oldrelease run of 0.1.1 on win-builder; the runs of
  this version reached it.

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
* The Title now says what the package profiles: "Data Profiling and Quality
  Measurement for Tables and Databases". The dependencies are the same as in
  0.1.1. The new S3 method is `print.tabla_lupa()`, which prints the tables
  returned by `hallazgos()` and `cobertura()`; `as.data.frame()` returns them
  without that class.

## Test environments

<!-- MATRIZ:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 64d9e762.
     Do not edit by hand: it is regenerated. -->

**Sources: `64d9e762238e12af42a66ae57d56ca9786968ac2`.**

| environment | result | log |
| --- | --- | --- |
| local, `_R_CHECK_DEPENDS_ONLY_=true` `--as-cran` | Status: 2 NOTEs | `verificacion/2026-11-16/check-depends-only.log` |
| local, `_R_CHECK_CRAN_INCOMING_=false` `--no-manual` | Status: OK | `verificacion/2026-11-16/check-sin-incoming.log` |
| local, container R 4.2.0 (the declared minimum) | Status: 1 ERROR, 1 WARNING, 4 NOTEs | `verificacion/2026-11-16/contenedor-4.2.0.log` |
| full test suite inside the R 4.2.0 container | [ FAIL 0 \| WARN 0 \| SKIP 173 \| PASS 43643 ] | `verificacion/2026-11-16/contenedor-4.2.0-suite.log` |
| local revalidation, all nine steps | Revalidacion local en verde | `verificacion/2026-11-16/revalidacion-completa.log` |
| engine matrix against real engines | motores medidos: 7 de 7 | `verificacion/2026-11-16/matriz-motores.log` |
| utility battery (recall and precision) | bateria: recall 26 de 26 columnas \| falsos positivos no explicados: 0 | `verificacion/2026-11-16/bateria-utilidad.log` |
| GitHub Actions, 4 platforms | platforms with Status: OK: 4 of 4; FAIL 0 \| WARN 0 on all of them; PASS from 43658 to 43743 | `verificacion/2026-11-16/actions-plataformas.log` |
| GitHub Actions, macOS (deps without pak) | Status: OK | `verificacion/2026-11-16/actions-macos.log` |
| R-hub v2, Linux (R-devel) | Status: OK | `verificacion/2026-11-16/rhub-linux.log` |
| R-hub v2, Windows (R-devel) | Status: OK | `verificacion/2026-11-16/rhub-windows.log` |
| R-hub v2, macOS x86_64 (R-devel) | Status: OK | `verificacion/2026-11-16/rhub-macos.log` |
| win-builder release | Status: 1 NOTE | `verificacion/2026-11-16/winbuilder-release.log` |
| win-builder devel | Status: 1 NOTE | `verificacion/2026-11-16/winbuilder-devel.log` |

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
configuration (all Suggests installed, `NOT_CRAN` unset) 72 of the 564 test
files in the tarball run at least one assertion. Among them are the tests that
cover profiling, the cleaning plan and its application, the HTML report, the
package data and the `integer64` guard, and the locale and date tests. Examples
and vignettes run on CRAN. Two vignette chunks are shown but not evaluated: they
call the interactive helpers `elegir_clave()` and `guiar_limpieza()`, which wait
for an answer.

<!-- TIEMPOS:INICIO -->

<!-- GENERATED by notas-desarrollo/matriz-carta.sh from the logs of 64d9e762.
     Do not edit by hand: it is regenerated. -->

| environment | `checking tests` | log |
| --- | ---: | --- |
| win-builder, R release | [113s] | `verificacion/2026-11-16/winbuilder-release.log` |
| win-builder, R-devel | [118s] | `verificacion/2026-11-16/winbuilder-devel.log` |
| this machine, Suggests installed, `--as-cran` | [24s/24s] | `verificacion/2026-11-16/check-granja.log` |
| this machine, `_R_CHECK_DEPENDS_ONLY_=true --as-cran` | [19s/19s] | `verificacion/2026-11-16/check-depends-only.log` |

| the suite behind those runs | result | log |
| --- | ---: | --- |
| full suite, this machine | BLOQUES: 3745 FALLOS: 0 ERROR: 0 SKIP: 12 PASS: 48128 | `verificacion/2026-11-16/revalidacion-completa.log` |

> These figures are **not** a guarantee about your machines: they are what
> each run's own log reports, and they move with the load of the machine
> that produced them. The local one is compared against a declared ceiling
> by the revalidation, so it cannot grow unnoticed.

<!-- TIEMPOS:FIN -->

## Reverse dependencies

lupa has no reverse dependencies on CRAN (checked on 2026-11-16 with
`tools::package_dependencies(reverse = TRUE, which = "all")`).

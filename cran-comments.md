## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The sources submitted are those of `COMMIT_COMPLETO`.

A local run additionally reports `Skipping checking HTML validation: no command
'tidy' found`, which is a property of that machine: the win-builder runs, which
have `tidy`, report `checking HTML version of manual ... OK`.

### Installed size

<!-- TAMANO:INICIO -->
<!-- TAMANO:FIN -->

The package has no compiled code and its two example datasets are under 1 kB
each: the R directory is the byte-compiled code (about 74,000 lines, 89
exported functions and 34 S3 methods), and the rest is the help pages and the
12 vignettes. The only figure in the tarball is the logo that `README.md`
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
  hash. Examples, vignettes and tests set a seed where they draw data, as the
  Cookbook recommends.
* Global state: every `options()` and `Sys.setlocale()` change is undone on
  exit (`.fijar_numerica_c()` registers its `on.exit()` in the caller's frame).
  The `par()` in `R/agregacion.R` is a local helper, not `graphics::par()`.
  `<<-` is only used inside closures. `cat()` and `print()` are only used in
  print methods; all other output is a message or a warning condition.

## Test environments

<!-- MATRIZ:INICIO -->
<!-- MATRIZ:FIN -->

## Tests on CRAN

The full test suite runs on continuous integration and on R-hub, where
`NOT_CRAN` is `true`. To keep the check within CRAN's time, test files that take
more than about a second, and the regression tests, start with
`testthat::skip_on_cran()`. What runs on CRAN covers profiling, the cleaning plan
and its application, the HTML report and the package data, plus the fast unit
tests. Examples and vignettes run on CRAN. Three vignette chunks are shown but
not evaluated: two call the interactive helpers `elegir_clave()` and
`guiar_limpieza()`, which wait for an answer, and one lists the system
requirements of the optional database drivers.

<!-- TIEMPOS:INICIO -->
<!-- TIEMPOS:FIN -->

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

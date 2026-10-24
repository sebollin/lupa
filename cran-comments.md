## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

The sources submitted are those of `COMMIT_COMPLETO`.

A local run additionally reports `Skipping checking HTML validation: no command
'tidy' found`, which is a property of that machine: the win-builder runs, which
have `tidy`, report `checking HTML version of manual ... OK`.

### URLs that the incoming check may flag

`checking CRAN incoming feasibility` reaches the network, and some publishers
refuse automated clients. None of these addresses is wrong or dead; each is the
publisher's own page for the work it cites:

* `https://www.iso.org/standard/35736.html`, `.../31531.html` and
  `https://www.iso.org/iso-3166-country-codes.html` (ISO/IEC 25012, ISO/IEC 7064
  and ISO 3166) answer `403` to non-browser clients.
* `https://repositorio.cepal.org/handle/11362/47464` (the CEPAL data quality
  framework that `marco_cepal()` declares) answers `403` to non-browser clients,
  with and without a browser user agent.
* `https://doi.org/10.1109/SEQUEN.1997.666900` (Broder, 1997) resolves to IEEE
  Xplore, which answers `202 Accepted` to an automated client.

## Test environments

<!-- MATRIZ:INICIO -->
<!-- MATRIZ:FIN -->

## Tests on CRAN

The full test suite runs on continuous integration and on R-hub, where
`NOT_CRAN` is `true`. To keep the check within CRAN's time, test files that take
more than about a second, and the regression tests, start with
`testthat::skip_on_cran()`. What runs on CRAN covers profiling, the cleaning plan
and its application, the HTML report and the package data, plus the fast unit
tests; examples and vignettes run in full.

<!-- TIEMPOS:INICIO -->
<!-- TIEMPOS:FIN -->

## Reverse dependencies

This is the first release, so there are no reverse dependencies.

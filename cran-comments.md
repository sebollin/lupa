> **Este bloque en espanol se saca antes de enviar.** Esta en otro idioma que
> el resto de la carta justamente para que no se pueda enviar sin verlo.
>
> **Las fuentes se identifican por commit, no por el sello `Packaged:`.** La
> version anterior de esta carta citaba `Packaged: 2026-08-21 19:03:52 UTC` como
> identidad de lo chequeado, y al ir a verificarlo resulto que **ninguna de las
> cuatro corridas de win-builder uso ese tarball**: `R CMD build` sella el
> momento de armar, asi que cada subida tiene su propio sello y el sello no
> identifica fuentes. Lo que identifica fuentes es el commit.
>
> **Y el estado de cada fila sale del log de esa corrida.** No de su conclusion
> -una corrida puede concluir con exito y traer notas- ni de la hora en que
> llego el aviso. Los logs de las corridas locales quedan en
> `../verificacion/2026-08-21/`, al lado del repositorio y no adentro, porque la matriz anterior
> declaraba `Status: OK` en dos filas locales de las que **no quedo ningun log**.
>
> **Estado de la matriz sobre `375d7c3`:**
>
> | entorno | estado | de donde sale |
> | --- | --- | --- |
> | local, R 4.6.1, `--as-cran` | **`Status: 1 NOTE`**, y la nota es `New submission` | `../verificacion/2026-08-24f/normal/lupa.Rcheck/00check.log` |
> | local, `_R_CHECK_DEPENDS_ONLY_=true` | **`Status: 1 NOTE`**, la misma | `../verificacion/2026-08-24f/depends-only/lupa.Rcheck/00check.log` |
> | local, `_R_CHECK_CRAN_INCOMING_=false` | **`Status: OK`**, cero notas | `../verificacion/2026-08-24f/sin-incoming/lupa.Rcheck/00check.log` |
> | local, `--as-cran` **construyendo vinietas** | **`Status: 1 NOTE`**, la misma. Las nueve construyen sin aviso | `../verificacion/2026-08-24f/con-vinietas/lupa.Rcheck/00check.log` |
> | contenedor R 4.1.3 (el minimo declarado) | **2 NOTEs del entorno**: paquetes sugeridos sin build ahi, y una cadena UTF-8 marcada | `../verificacion/2026-08-24f/r41/lupa.Rcheck/00check.log` |
> | suite completa | **16.034 comprobaciones, 0 fallos, 0 errores, 0 avisos** | leido de `sum(r$failed)`, no del texto |
> | GitHub Actions (5 plataformas) | verde | log de la corrida |
> | R-hub v2 R-devel (3 plataformas) | PENDIENTE sobre estas fuentes | log de la corrida |
> | win-builder release y devel | PENDIENTE sobre estas fuentes | log de cada corrida |
>
> **Que la corrida sin comprobaciones de entrada de `OK` sin ninguna nota ubica
> esa nota entera fuera del paquete**: es la de primera entrega.
>
> **La cadena UTF-8 marcada es un nombre de departamento con acento** en los
> datos de ejemplo. Esta correctamente marcada, R 4.6.1 da `OK` en esa misma
> comprobacion, y solo R 4.1.3 la nota.
>
> **Dos WARNINGs que la suite no podia ver.** La matriz anterior, sobre
> `9956c6c`, dio `2 WARNINGs` donde la de `031fa59` habia dado `1 NOTE`. Los dos
> los introdujo esa misma tanda y ninguno aparecia con 15.887 comprobaciones en
> verde:
>
> - **`non-ASCII characters in R code`**: un mensaje nuevo tenia la palabra
>   "espanol" escrita con enie dentro de una cadena. Los acentos de los
>   comentarios estan permitidos -y hay archivos con 76 lineas asi que nunca
>   dispararon nada-; el que cuenta es el de codigo. Comprobar esa diferencia
>   antes de tocar evito "limpiar" medio paquete sin motivo.
> - **`'::' import not declared from 'withr'`**: una prueba usaba
>   `withr::local_tempfile()` y `withr` no esta en `Suggests`. Se reemplazo por
>   `tempfile()` con su `on.exit(unlink())`.
>
> Es la razon por la que la matriz se rehace entera y no se confia en la suite:
> **`R CMD check` ve clases de defecto que ninguna comprobacion de la suite
> alcanza.**
>
> **El `WARN 1` que traia la fila del contenedor ya no esta.** Venia de una
> prueba que se tragaba el fallo de `Sys.setlocale` con un `try` y, en una
> imagen sin esos locales generados, comparaba el resultado contra si mismo:
> pasaba sin probar su propia afirmacion. Ahora comprueba que el locale quedo
> puesto y, si no se puede poner, se saltea diciendo por que. De ahi que los
> `SKIP` pasen de 165 a 169.
>
> **El `WARN 1` de la fila del contenedor no es nuevo y la carta anterior no lo
> decia.** Estaba igual en la corrida del 2026-08-22 -`[ FAIL 0 | WARN 1 | SKIP
> 165 | PASS 14660 ]`- y se transcribio como `[ FAIL 0 | PASS 14660 ]`. Copiar
> la mitad buena de un resumen es la misma falta que el paquete persigue en los
> demas. Las dos NOTEs son del entorno: siete paquetes de `Suggests` que no estan
> en la imagen, y una cadena UTF-8 en los datos.
>
> **Un fallo intermitente que no es del paquete.** Una corrida de la suite dio
> un error en `test-ronda90.R` dentro de un `data.frame()` de constantes que no
> depende de los datos de entrada; el archivo pasa entero corrido aparte y la
> corrida siguiente dio cero fallos. Es el estado corrupto que deja
> `pkgload::load_all()` de vez en cuando, y por eso la fila de la suite sale de
> una corrida completa y no de la primera que se mire. `R CMD check`, que corre
> los tests contra el paquete instalado, da `checking tests ... OK`.
>
> **El minimo declarado se midio antes de declararlo**, que es justamente lo que
> no se habia hecho con `R (>= 3.6.0)`: ahi la carta afirmaba que la suite no
> podia correr, y contra el snapshot de la epoca corre y da 18 fallos. Ver la
> seccion del contenedor mas abajo.
>
> **win-builder del 2026-08-21, para el archivo:** las cuatro corridas de ese dia
> dan `Status: 1 NOTE`, siempre la misma -`New submission` mas
> `https://www.gnu.org/licenses/gpl-3.0.html` con `Timeout was reached`, que es
> la maquina de win-builder sin poder conectar, no una URL rota-. Dos de esas
> cuatro chequearon fuentes anteriores a las que se queria probar: se supo
> leyendo el `Packaged:` del binario de cada corrida, no la hora del aviso. Dato
> util: ahi si corre `checking HTML version of manual` y da **OK**, que aca no se
> puede medir por falta de `tidy`.
>
> Los commits posteriores que tocan **solo este archivo** no mueven las fuentes,
> porque `cran-comments.md` esta en `.Rbuildignore` y no entra al tarball. Si se
> toca cualquier otra cosa, la matriz se rehace entera.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission. That note comes from the CRAN incoming checks and
  cannot be avoided.

Run locally with `_R_CHECK_CRAN_INCOMING_=false` and `--no-manual`, the check on
these sources reports **`Status: OK`** with no notes at all. The `tidy` note that
earlier revisions reported is not absent because the environment gained `tidy` —
it has none — but because `--no-manual` skips HTML manual validation altogether.
The services above build the manual and report no such note. Examples, tests,
vignettes, the PDF manual and the self-contained HTML produced by the package are
checked in every environment that can run them.

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

The package sources submitted are those of `ee5f6ac`.

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
* Local, `--as-cran` **building the vignettes** — **`Status: 1 NOTE`**, the same
  one, with `checking package vignettes ... OK` and `checking re-building of
  vignette outputs ... OK`. The other local runs pass `--ignore-vignettes`
  because the R 4.1.3 container has no `pandoc`, and carrying that flag over to
  the local runs left unchecked something CRAN does do. All nine vignettes build
  without a warning.
* Continuous integration (GitHub Actions, `R-CMD-check`, run 32800861652) on
  `ee5f6ac`, 5 of 5 with **`Status: OK`** and no notes: Ubuntu with R release, R-devel and R oldrel-1;
  Windows with R release; and macOS with R release on
  **`aarch64-apple-darwin23`**. The platforms exercised are
  `x86_64-pc-linux-gnu`, `x86_64-w64-mingw32` and `aarch64-apple-darwin23`. The
  five `Status: OK` lines and the absence of notes are read from the run's own
  log rather than inferred from the green tick, because a run can conclude
  successfully and still carry notes.
* R-hub v2, R-devel (run 32800095271) on **`7244ee1`, one commit before the
  submitted sources**: Linux, Windows and macOS — all three **`Status: OK`**, no
  notes. The gap is one commit that adds a progress bar to collection profiling;
  it is named here rather than glossed, because a result that measured other
  sources is not a result for these. Re-dispatching R-hub on `ee5f6ac` costs
  time and no work, and should precede the actual submission.
  The macOS result was read from that job's own log rather than from the
  combined run log, which the API returned truncated before the check summary —
  a green tick with no readable `Status:` line is not a result.
* win-builder, R release (4.6.1) and R-devel (r90445), from `ee5f6ac`, uploaded
  on 2026-08-25: **`Status: 1 NOTE` on both**, and the note is `New submission`
  and nothing else. Read from each run's own `00check.log`
  (`uFmJuRmKB3GQ` and `8PUkKn3a9w9D`) rather than from the notification e-mail,
  which reports the count but not which note it is. `checking tests ... OK` at
  108 and 105 minutes respectively, `checking re-building of vignette
  outputs ... OK`, `checking PDF version of manual ... OK`.

  **The URL note recorded below did not recur.** An earlier revision reported
  `https://www.gnu.org/licenses/gpl-3.0.html` as unreachable on every win-builder
  run of that day, and the paragraph concluded it was that machine's network.
  These two runs report no URL note at all, which is what a transient network
  failure looks like once it stops — and is the reason that paragraph said what
  it could support rather than declaring the link fine.
* Container: R 4.1.3 (`rocker/r-ver:4.1.3`) for the declared minimum, with the
  suggested packages installed and the test suite running. Result:
  **0 errors, 0 warnings**, `checking tests ... OK`, and two notes that are
  properties of that container rather than of the package. The first is that
  `bit64`, `covr`, `knitr`, `rmarkdown`, `RSQLite` and `sf` have no build there.
  The second says `found 1 marked UTF-8 string`, and that string is
  **`Paysandú`** in the department column of the example data: it is correctly
  marked, R 4.6.1 reports `OK` for that same check, and only R 4.1.3 notes it. A
  package about the quality of Uruguayan data has to be able to write that name
  the way it is written.

  **`DESCRIPTION` used to declare `R (>= 3.6.0)`, and that was a claim this
  package did not keep.** An earlier revision stated that the suite could not be
  run under R 3.6 because `testthat` declares `R (>= 4.1.0)`. That is true
  against current CRAN and false against a period-appropriate snapshot, where
  `testthat 3.1.7` installs without trouble. Run there, the suite reports
  `[ FAIL 18 | PASS 15356 ]`.

  Six of those eighteen came from one cause: under R < 4.0 `data.frame()`
  defaults to `stringsAsFactors = TRUE`, and at the time of that run the package
  had 275 `data.frame()` calls that did not say otherwise, so text columns were
  born as factors. The consequence is not cosmetic — writing the personal-data
  marker `"[valor protegido]"` into a factor column yields `NA` instead, so a
  promise the package makes about that cell silently goes unkept. Under R 4.0.5
  with the same period packages those six disappeared.

  **That count is no longer 275, and this line said it was until it was
  measured.** Parsing `R/` rather than grepping it — `grep` counts
  `as.data.frame(` and `is.data.frame(` as well, and returns 297 where the
  constructor is called 195 times — there is exactly **one** call without the
  argument, and it constructs an empty frame, where there is no column to
  coerce. The sentence is left in the past tense with the current figure beside
  it rather than deleted: what it describes is why the floor was raised, and a
  letter that erases the state it was written about stops being evidence of
  anything.

  The minimum is now `R (>= 4.1.0)`, which is both what current `testthat`
  requires — so the suite runs at the declared floor with today's tools — and a
  floor that was measured in a container before being declared, rather than
  asserted. The only import, `cli`, declares `R (>= 3.4)`, so nothing forced the
  older number.

  The remaining twelve failures under old suggested-package versions are not
  about the R version: with `RSQLite 2.3.1` and `bit64 4.0.5`, counts come back
  as `integer64` and travel into the profile that way, so fields change class
  with the user's installed optional packages. CRAN does not check against old
  `Suggests` versions and none of this affects the checks above; it is recorded
  as open work rather than presented as solved.

* win-builder on an **earlier revision** (R-release 4.6.1 and R-devel r90440),
  kept because of what it records about reading results rather than for its
  status: **1 NOTE on each**. The
  note is the new-submission one plus a URL the checker could not reach —
  `https://www.gnu.org/licenses/gpl-3.0.html`, reported as
  `Timeout was reached ... Failed to connect to www.gnu.org port 443`. The link
  is the GPL-3 text and resolves; the timeout is that machine's network, and it
  appeared on every run of the day. Both queues report `checking tests ... OK`
  and both manual formats OK, including the HTML manual that cannot be checked
  locally for want of `tidy`.

  Results were matched to this build by reading each check log rather than by the
  arrival time of the notification. That distinction mattered here: an earlier
  build of the same sources sat in the R-release queue for over three hours, and
  its result arrived after the corrected build had already been submitted. It was
  identified as stale because it reported a note that these sources cannot
  produce.

The macOS builder at <https://mac.r-project.org/macbuilder/> returned HTTP 502 for
every submission attempt, as it did while the previous revision was being prepared.
Apple silicon is covered instead by the GitHub Actions `macos-latest` runner, which
reports `using platform: aarch64-apple-darwin23`.

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
misses across the canonical finding vocabulary. The current count is **57
`tipo_hallazgo` names**: 54 types constructed by `.nuevo_hallazgo()` and three
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

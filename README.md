# lupa <a href="https://sebollin.github.io/lupa/"><img src="man/figures/logo.png" align="right" height="139" alt="lupa website" /></a>

<!-- badges: start -->
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![README en español](https://img.shields.io/badge/README-espa%C3%B1ol-2e7d32.svg)](https://github.com/sebollin/lupa/blob/main/README.es.md)
<!-- badges: end -->

## 🔎 What it is and how it differs

`lupa` is an auditable R toolkit that connects first-pass profiling with a
quality model declared for a particular use, explicit measurement, controlled
cleanup of a copy, and approximate duplicate detection at scale. Instead of a
single opaque score, every result carries its scope, evidence, and uncertainty.

**Profiling never touches your data.** No analysis function alters the table it
receives — not its values, its types, its names, or its attributes — including
`data.table` inputs, which R allows to be modified by reference. Only the
remediation layer produces different data, and it returns a copy: the table you
passed in is still the table you have. A regression test asserts this for every
entry point.

## 🌎 API language

The public names are Spanish in examples, help pages, and vignettes:

| Spanish API | English meaning |
| --- | --- |
| `perfilar()` | profile |
| `analizar()` | analyse |
| `marco_calidad()` | quality framework |
| `planificar_limpieza()` | plan a cleanup |
| `guiar_limpieza()` | guide a cleanup |
| `aplicar()` | apply a selected cleanup |
| `medir()` / `tablero_calidad()` / `evaluar()` | measure / dashboard / evaluate |
| `detectar_duplicados_aproximados()` | find approximate duplicates |
| `reportar()` | create a report |

The [Spanish README](https://github.com/sebollin/lupa/blob/main/README.es.md)
tells the same story. Contributors should keep the public contract in Spanish;
the surrounding guidance can be internationalised.

## ⚡ A five-minute start

Until the first CRAN release, install the development version from GitHub:

~~~r
# install.packages("pak")
pak::pak("sebollin/lupa")
~~~

Then profile a table or run the complete analysis route:

~~~r
library(lupa)
data(datos_operativos)

perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
head(perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")], 5)

analisis <- analizar(datos_operativos)
analisis$tablero
archivo <- tempfile(fileext = ".html")
reportar(analisis, archivo = archivo)
stopifnot(file.exists(archivo))
unlink(archivo)
~~~

Profiling is read-only: it never changes the input table. Findings are ordinary
inspectable data frames, and personal-data evidence is masked when the
classification warrants it. This is a real console preview:

![A captured `perfilar()` console result](man/figures/perfil-console.png)

## 🧭 What can I do with lupa?

The [pkgdown reference](https://sebollin.github.io/lupa/reference/) and the
linked vignettes are the detailed manual. This table is the short map:

| Task | Main functions | Read more |
| --- | --- | --- |
| Look at data for the first time | `perfilar()`, `analizar()`, `distribucion_valores()`, `detectar_asociaciones()`, `analizar_tiempo()`, `clasificar_variables()`, `inferir_tipo()`, `descubrir_patrones()`, `detectar_formatos_fecha()`, `sentinelas_naniar` | [Getting started](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Profile against a database | `perfilar_dbi()` — full-table SQL aggregates plus a 99-field profile from a declared sample; the scopes stay separate | [Profiling a database](https://sebollin.github.io/lupa/articles/perfilar-una-base.html) |
| Find undeclared structure | `detectar_claves()`, `detectar_relaciones()`, `detectar_dependencias()`, `granularidades()`, `transiciones_granularidad()` | [Undeclared structure](https://sebollin.github.io/lupa/articles/estructura-no-declarada.html) |
| Define quality | `marco_calidad()`, `marco_agesic()`, `marco_iso25012()`, `marco_cepal()`, `catalogo_agesic()`, `metrica()`, `especializar()`, `instanciar()`, `modelo()`, `metricas_nucleo()`, `metricas_referencial()`, `proponer_modelo()`, `modelo_desde_propuesta()`, `perfiles_madurez()`, `cobertura_analisis()` | [Define quality](https://sebollin.github.io/lupa/articles/definir-la-calidad.html) |
| Measure and evaluate | `medir()`, `agregar()`, `tablero_calidad()`, `indice_calidad()` with project weights, `evaluar()`, `regla_evaluacion()` with the user-declared instruction `desenlace = "suprimir"` (not a factory threshold), `perfil_evaluacion()`, `escala()`, `referencial()`, `vigencia()` | [Measure and evaluate](https://sebollin.github.io/lupa/articles/medir-y-evaluar.html) |
| Clean safely | `planificar_limpieza()`, `guiar_limpieza()`, `aplicar()` | [Cleaning plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.html) |
| Find approximate duplicates | `detectar_duplicados_aproximados()`, `estimar_costo()` | [Scale and duplicates](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html) |
| Repair encoding damage | `reparar_codificacion` through `planificar_limpieza()` and `aplicar()` | [Cleanup reference](https://sebollin.github.io/lupa/reference/planificar_limpieza.html) |
| Follow quality over time | `historico_calidad()`, `acumular_historico()`, `guardar_historico()`, `leer_historico()`, `detectar_deriva_calidad()`, `comparar_perfiles()`, `comparar_evaluaciones()` | [History and drift](https://sebollin.github.io/lupa/articles/historico-y-deriva.html) |
| Share results | `reportar()`, `guardar_analisis()`, `leer_analisis()` | [Reporting reference](https://sebollin.github.io/lupa/reference/reportar.html) |
| Validate and extend | `validadores_internacionales()`, `validadores_uruguay()`, `pack_validadores()`, `validar_ci_uy()`, `validar_rut_uy()`, `validar_luhn()`, `validar_mod97()`, `validar_iso3166()`, `validar_iso4217()`, `validar_correo()`, `validar_url()` | [Reference](https://sebollin.github.io/lupa/reference/) |

## 📐 Scope: what uses every row and what is sampled

`perfilar()` uses every row for table and column counts, real and disguised
missingness, distinct values, exact duplicates, quantitative summaries, and
the findings derived from those quantities. By default, `muestra = 1e5` limits
pattern discovery, type inference, date-format discovery, and the common sample
used to search for functional dependencies. Set another limit or `Inf` to
change or disable that sampling.

Personal-document validators have a separate preliminary filter:
`muestra_validadores = 1000` by default. A validator that passes that filter is
then evaluated on the complete column; `Inf` makes even the preliminary pass
complete. Approximate duplicates are off by default and have their own declared
bounds when enabled.

In `detectar_duplicados_aproximados()`, `pares$tipo_par` is self-describing:
`exacto` means the stored texts are equal, `exacto_normalizado` means they only
match after the declared normalization, and `aproximado` means they remain
similar rather than equal. `pares$igualo_normalizar` marks the middle case.
The corresponding scope counts are `n_pares_exactos`,
`n_pares_exactos_normalizados`, and `n_pares_aproximados`.

The result records the effective scope in `meta$muestra`,
`meta$filas_analizadas`, and `meta$muestreo`; each column also records
`n_filas_analizadas_tipo` and `muestreado_tipo_inferido`, while the dependency
table carries its analysed-row and sampling attributes. `analizar()` reuses
`muestra = 1e5` for its profile, distributions, and observed-level enumeration,
and declares separate limits for associations and the other components.

## 🛣️ `perfilar()` or `analizar()`?

Use `perfilar()` when you want the focused, inspectable profile: column
summaries, patterns, inferred types, findings, diagnostic coverage, and
undeclared structural relationships. Use `analizar()` when you want the full
route around that profile: distributions, associations, temporal analysis,
confirmable variable classification, a model proposal, a cleanup plan,
conceptual coverage, and a dashboard.

With no confirmed model or proposal, `analizar()` measures every proposal row
whose state is `"lista"` by default. That proposal was inferred by `lupa`; nobody
has confirmed it. Use `medir_propuesta = FALSE` to keep the route descriptive,
or supply a confirmed proposal/model. The function aggregates immediately and
keeps the small dashboard; `conservar_detalle_medicion = TRUE` retains the
row-level measurement detail.

**Where the value distribution and the correlations live.** Both are in
`analizar()`, not in `perfilar()`, and that separation is deliberate:
`perfilar()` is the cheap pass whose object you carry around, while
`distribucion_valores()` and `detectar_asociaciones()` cost more and produce
tables of their own. `distribucion_valores()` returns per-column frequencies
and quantiles with a declared cap and truncation flag; `detectar_asociaciones()`
returns Pearson between numeric columns — or Spearman, with
`metodo_numerico = "spearman"`, for a monotone relationship that isn't linear —
plus Cramér's V and eta squared, each row declaring its method and its
assumption. Both are exported, so you can call them on their own without paying
for the whole route.

## 🚦 Severities and automation

`severidad` is an **ordered factor**: `ok < sospechoso < error`.

- `ok` records an observed condition that is acceptable or informational; it
  is not an adverse decision.
- `sospechoso` is evidence worth reviewing. It is heuristic or needs domain
  context and must not by itself reject, repair, or suppress data automatically.
- `error` states that the applicable check crossed its declared criterion. Of
  these three levels, it is the only candidate for an adverse automated gate,
  and only after the project accepts that criterion and verifies the scope.

`cobertura_diagnosticos` is outside this scale. It lists checks that could not
be evaluated and how to resolve them. Automation must inspect it as well as
`error`: zero errors does not mean a clean profile when diagnostics were not
run. Cleanup is always explicit—`aplicar()` changes only actions selected from
an editable plan.

## ✨ What lupa does in detail

- Profiles a delivery and surfaces missingness, types, patterns, dates, and
  personal-data evidence.
- Profiles `sf` geometries and declares CRS, geometry families, emptiness,
  planar validity, coordinate domain, and bounding-box scope; it does not
  perform spatial analysis.
- Applies Benford's law only when its preconditions hold, and records
  non-applicability in `cobertura_diagnosticos`.
- Reports `unidades_mixtas` and `monedas_mixtas` in a column without converting
  values or assuming exchange rates.
- Reports `celdas_multivaluadas` only when homogeneous parts match the
  column's patterns.
- Finds `relacion_aritmetica_columnas` as observed identities or proportions
  between numeric columns, not as domain rules.
- Finds `relacion_orden_columnas` between comparable columns and declares the
  comparison scope.
- Finds keys, relationships, dependencies, and measurement granularities that
  were never declared.
- Reports `casi_clave` when a non-temporal column has at least 100 rows, is
  almost unique, and its collisions concentrate in a few values, which
  separates a key with duplicates from free text of high cardinality.
- Lets a project define its own quality framework instead of forcing a global
  score.
- Measures and evaluates explicit metrics, scales, validity rules, and
  referential domains.
- Produces editable cleanup plans, applies only selected actions to a copy, and
  keeps an audit log.
- Finds approximate duplicates with exact tiles, deterministic MinHash/LSH,
  blocking, cost estimates, and disk-backed lots.
- Repairs encoding damage in R, including repeated mojibake and CESU-8, while
  refusing unsafe lossy conversions.
- Follows quality through time and creates self-contained HTML reports.

## 🧪 Evidence, with its declared scope

Each check below uses a different declared unit and reference. None of them
estimates a single package-wide accuracy.

| Check | Declared unit | Result |
| --- | --- | ---: |
| Raha dirty/clean pairs | columns containing at least one changed cell | 26/26 received at least one finding; 8 further columns were flagged |
| Constructed clean controls | 43 tables | 0 error-severity findings; 25 review signals |
| Seeded defects | 9 planted defects | 9/9 detected |
| Real sanctions register | error-severity findings over 2,556 rows | 8/8 independently confirmed |

In the Raha pairs the dirty/clean comparison labels changed cells; it does not
label every property observable in an unchanged column. Manual review found a
supported observation in each of the eight further columns—constants,
duplicated columns, inconsistent case, empty strings, and high-cardinality
text. We therefore report neither precision nor diagnostic recall from Raha:
26/26 is column coverage, not evidence that every changed cell was identified.
[`benchmark/`](benchmark/) reproduces the table from the published sources and
records the exact file fingerprints used by the published run, but only when
`lupa` is installed from a build of these same sources. From the repository
root, reproduce that condition and run the scripts with:

```sh
R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz
Rscript benchmark/verdad_raha.R
Rscript benchmark/medir_lupa.R
```

The benchmark records the installed version and full `Built` stamp and stops
when that installation lacks a capability required by the published table.

## 🔍 Limits, fit, stability, and references

There is no factory quality score: `indice_calidad()` returns the dashboard
unless a project supplies complete named weights, and a calculated index always
travels with its coverage, weights, transformations, and heterogeneous
universes. The core is universal and catalogues are pluggable;
[AGESIC](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
v1.6 is a reference implementation, not a country lock. The only required
import is [`cli`](https://cran.r-project.org/package=cli). Suggested packages
enable bounded capabilities: [`sf`](https://cran.r-project.org/package=sf)
enables geometry profiling; [`DBI`](https://cran.r-project.org/package=DBI)
provides the database interface and
[`RSQLite`](https://cran.r-project.org/package=RSQLite) a backend for
`perfilar_dbi()`; [`stringdist`](https://cran.r-project.org/package=stringdist)
enables approximate text comparison.

Work that can be parallelised uses **two threads by default**, the ceiling CRAN
asks packages to respect. On your own machine you can raise it per call with
`nucleos = 8` or for the whole session with `options(lupa.nucleos = 8)`; the
result does not change, only how long it takes.

### Where it fits

[`skimr`](https://cran.r-project.org/package=skimr) and
[`DataExplorer`](https://cran.r-project.org/package=DataExplorer) explore;
[`pointblank`](https://cran.r-project.org/package=pointblank),
[`validate`](https://cran.r-project.org/package=validate), and
[`dataquieR`](https://cran.r-project.org/package=dataquieR) express or evaluate
rules; [`zoomerjoin`](https://cran.r-project.org/package=zoomerjoin),
[`textreuse`](https://cran.r-project.org/package=textreuse), and
[`reclin2`](https://cran.r-project.org/package=reclin2) focus on text comparison
or record linkage. [`calidad`](https://github.com/inesscc/calidad), maintained
by [Klaus Lehmann](https://github.com/Klauslehmann) and
[Ricardo Pizarro](https://github.com/ricardoflopiza), is a complementary axis:
it evaluates the quality of **survey estimates**, while `lupa` evaluates the
tabular data that produces an estimate.

Encoding repair follows the approach and frozen data of
[`ftfy`](https://github.com/rspeer/python-ftfy) 6.3.1 by
[Robyn Speer](https://github.com/rspeer), in R. It includes eleven byte tables,
CESU-8 and Java `C0 80` handling, and five deliberate extensions documented in
the [NEWS](NEWS.md). It reproduces 159 of the 161 distributed corpus cases and
leaves all 31 negative cases untouched. It deliberately does not provide
`ftfy`'s style-oriented `fix_text` steps such as HTML unescaping, quote curling,
width normalization, or Unicode normalization: changing legitimate data
silently is not repair.

### Citation and references

~~~r
citation("lupa")
~~~

Conceptual references are [Batini and Scannapieco
(2016)](https://doi.org/10.1007/978-3-319-24106-7), the
[AGESIC Digital Government Data Quality Framework
v1.6](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/),
and [ISO/IEC 25012:2008](https://www.iso.org/standard/35736.html).

### Stability, contribution, and license

Version 0.1.0 is pre-CRAN: the public API may change before 1.0; breaking
changes will be announced in `NEWS.md` and release notes, with a deprecation
warning first whenever practical.

Please use the [issue tracker](https://github.com/sebollin/lupa/issues) for
bugs, proposals, and documentation fixes. The stable contracts are the
declared units, scope, protection, and audit trail; implementation details and
benchmark times can change between releases when those contracts remain true.

`lupa` is released under the [GPL-3](https://www.gnu.org/licenses/gpl-3.0.html).
See [`LICENSE.note`](LICENSE.note) for the Apache-2.0 data derived from
[`ftfy`](https://github.com/rspeer/python-ftfy) and the MIT data derived from
[`naniar`](https://github.com/njtierney/naniar).

# lupa <a href="https://sebollin.github.io/lupa/"><img src="man/figures/logo.png" align="right" height="139" alt="lupa website" /></a>

<!-- badges: start -->
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml)
[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![README en español](https://img.shields.io/badge/README-espa%C3%B1ol-2e7d32.svg)](https://github.com/sebollin/lupa/blob/main/README.es.md)
<!-- badges: end -->

`lupa` is an auditable R toolkit for profiling tabular data, defining what
quality means for a particular use, measuring it, cleaning a copy without
silently changing the input, and finding approximate duplicate records at
scale. It reports the scope, evidence, and uncertainty of every result.

The public API, help pages, and vignettes are in Spanish. The names are stable
and can be copied from this English guide; the [Spanish
README](https://github.com/sebollin/lupa/blob/main/README.es.md) tells the same
story in Spanish.
This is an internationalized core with a Spanish interface: translating the
public names would break code, tests, and vignettes, so contributors should
expect the contract itself to remain in Spanish while the surrounding guidance
can be read in English.

## 🌎 API language

The public names are Spanish in both examples and help pages:

| Spanish API | English meaning |
| --- | --- |
| `perfilar()` | profile |
| `analizar()` | analyse |
| `marco_calidad()` | quality framework |
| `planificar_limpieza()` | plan a cleanup |
| `guiar_limpieza()` | guide a cleanup |
| `aplicar()` | apply a selected cleanup |
| `medir()` / `evaluar()` | measure / evaluate |
| `detectar_duplicados_aproximados()` | find approximate duplicates |
| `reportar()` | create a report |

## ✨ What lupa does

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

## 📦 Installation

Until the first CRAN release, install the development version directly from
GitHub:

~~~r
# install.packages("pak")
pak::pak("sebollin/lupa")
~~~

For a local clone or release tarball, use:

~~~sh
R CMD build .
R CMD INSTALL lupa_0.1.0.tar.gz
~~~

Or install a local source tarball from R:

~~~r
install.packages("lupa_0.1.0.tar.gz", repos = NULL)
~~~

## ⚡ A five-minute start

~~~r
library(lupa)
data(datos_operativos)

perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
head(perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")], 5)

analisis <- analizar(datos_operativos)
archivo <- tempfile(fileext = ".html")
reportar(analisis, archivo = archivo)
stopifnot(file.exists(archivo))
unlink(archivo)
~~~

The profile is read-only: it never changes the input table. Findings are
ordinary inspectable data frames, and personal-data evidence is masked when
the classification warrants it. A real console preview is shown below.

![A captured `perfilar()` console result](man/figures/perfil-console.png)

## 🧭 What can I do with lupa?

The [pkgdown reference](https://sebollin.github.io/lupa/reference/) and the
linked vignettes are the detailed manual. This table is the short map:

| Task | Main functions | Read more |
| --- | --- | --- |
| Look at data for the first time | `perfilar()`, `analizar()`, `distribucion_valores()`, `detectar_asociaciones()`, `analizar_tiempo()`, `clasificar_variables()`, `inferir_tipo()`, `descubrir_patrones()`, `detectar_formatos_fecha()`, `sentinelas_naniar` | [Getting started](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Profile against a database | `perfilar_dbi()` — full-table SQL aggregates plus a 93-field profile from a declared sample; the scopes stay separate | [Reference](https://sebollin.github.io/lupa/reference/) |
| Find undeclared structure | `detectar_claves()`, `detectar_relaciones()`, `detectar_dependencias()`, `granularidades()`, `transiciones_granularidad()` | [Getting started](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Define quality | `marco_calidad()`, `marco_agesic()`, `marco_iso25012()`, `marco_cepal()`, `catalogo_agesic()`, `metrica()`, `especializar()`, `instanciar()`, `modelo()`, `metricas_nucleo()`, `metricas_referencial()`, `proponer_modelo()`, `modelo_desde_propuesta()`, `perfiles_madurez()`, `cobertura_analisis()` | [Quality model](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Measure and evaluate | `medir()`, `agregar()`, `evaluar()`, `regla_evaluacion()` with the user-declared instruction `desenlace = "suprimir"` (not a factory threshold), `perfil_evaluacion()`, `escala()`, `referencial()`, `vigencia()` | [Quality model](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Clean safely | `planificar_limpieza()`, `guiar_limpieza()`, `aplicar()` | [Cleaning plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.html) |
| Find approximate duplicates | `detectar_duplicados_aproximados()`, `estimar_costo()` | [Scale and duplicates](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html) |
| Repair encoding damage | `reparar_codificacion` through `planificar_limpieza()` and `aplicar()` | [Cleanup reference](https://sebollin.github.io/lupa/reference/planificar_limpieza.html) |
| Follow quality over time | `historico_calidad()`, `acumular_historico()`, `guardar_historico()`, `leer_historico()`, `detectar_deriva_calidad()`, `comparar_perfiles()`, `comparar_evaluaciones()` | [History and drift](https://sebollin.github.io/lupa/articles/historico-y-deriva.html) |
| Share results | `reportar()`, `guardar_analisis()`, `leer_analisis()` | [Reporting reference](https://sebollin.github.io/lupa/reference/reportar.html) |
| Validate and extend | `validadores_internacionales()`, `validadores_uruguay()`, `pack_validadores()`, `validar_ci_uy()`, `validar_rut_uy()`, `validar_luhn()`, `validar_mod97()`, `validar_iso3166()`, `validar_iso4217()`, `validar_correo()`, `validar_url()` | [Reference](https://sebollin.github.io/lupa/reference/) |

~~~r
library(lupa)
data(datos_operativos)
marco <- marco_calidad(
  "Marco operativo",
  list(Estructura = c("Ausencias observadas", "Duplicacion exacta"))
)
propuesta <- proponer_modelo(perfilar(datos_operativos,
                                      analizar_dependencias = FALSE))
list(marco = marco, propuesta = propuesta)
~~~

The API has a few boundaries worth knowing. There is no global quality score:
dimensions, units, and rules stay visible. A factory weighting would be a
verdict about what matters, so `lupa` exposes the components and a recipe and
leaves the weights to each project. The core is universal and
catalogues are pluggable; [AGESIC](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
v1.6 is a reference implementation, not a country lock. The only required
import is [`cli`](https://cran.r-project.org/package=cli). Suggested packages
enable these capabilities: [`sf`](https://cran.r-project.org/package=sf)
enables geometry profiling; [`DBI`](https://cran.r-project.org/package=DBI)
provides the database interface and
[`RSQLite`](https://cran.r-project.org/package=RSQLite) a backend for
`perfilar_dbi()`. [`stringdist`](https://cran.r-project.org/package=stringdist)
is optional for approximate text comparison.

Work that can be parallelised uses **two threads by default**, the ceiling CRAN
asks packages to respect. On your own machine you can raise it, per call with
`nucleos = 8` or for the whole session with `options(lupa.nucleos = 8)`; the
result does not change, only how long it takes.

## 🔍 Where it fits

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
[`ftfy`](https://github.com/rspeer/python-ftfy)'s style-oriented `fix_text` steps such as HTML unescaping, quote
curling, width normalization, or Unicode normalization: changing legitimate
data silently is not repair.

## 📖 Citation and references

~~~r
citation("lupa")
~~~

Conceptual references are [Batini and Scannapieco
(2016)](https://doi.org/10.1007/978-3-319-24106-7), the
[AGESIC Digital Government Data Quality Framework
v1.6](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/),
and [ISO/IEC 25012:2008](https://www.iso.org/standard/35736.html).

## 🤝 Contribute and report

Please use the [issue tracker](https://github.com/sebollin/lupa/issues) for
bugs, proposals, and documentation fixes. The stable contracts are the
declared units, scope, protection, and audit trail; implementation details and
benchmark times can change between releases when those contracts remain true.

## 📄 License

`lupa` is released under the [GPL-3](https://www.gnu.org/licenses/gpl-3.0.html).
See [`LICENSE.note`](LICENSE.note) for the Apache-2.0 data derived from
[`ftfy`](https://github.com/rspeer/python-ftfy) and the MIT data derived from
[`naniar`](https://github.com/njtierney/naniar).

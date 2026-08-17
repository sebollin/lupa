# lupa

## 🔎 What it is and how it differs

`lupa` is an auditable R toolkit that connects first-pass profiling with
a quality model declared for a particular use, explicit measurement,
controlled cleanup of a copy, and approximate duplicate detection at
scale. Instead of a single opaque score, every result carries its scope,
evidence, and uncertainty.

## 🌎 API language

The public names are Spanish in examples, help pages, and vignettes:

| Spanish API | English meaning |
|----|----|
| [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) | profile |
| [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) | analyse |
| [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md) | quality framework |
| [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | plan a cleanup |
| [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md) | guide a cleanup |
| [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | apply a selected cleanup |
| [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) / [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md) / [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md) | measure / dashboard / evaluate |
| [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md) | find approximate duplicates |
| [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md) | create a report |

The [Spanish
README](https://github.com/sebollin/lupa/blob/main/README.es.md) tells
the same story. Contributors should keep the public contract in Spanish;
the surrounding guidance can be internationalised.

## ⚡ A five-minute start

Until the first CRAN release, install the development version from
GitHub:

``` r

# install.packages("pak")
pak::pak("sebollin/lupa")
```

Then profile a table or run the complete analysis route:

``` r

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
```

Profiling is read-only: it never changes the input table. Findings are
ordinary inspectable data frames, and personal-data evidence is masked
when the classification warrants it. This is a real console preview:

![A captured perfilar() console
result](reference/figures/perfil-console.png)

A captured
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
console result

## 🧭 What can I do with lupa?

The [pkgdown reference](https://sebollin.github.io/lupa/reference/) and
the linked vignettes are the detailed manual. This table is the short
map:

| Task | Main functions | Read more |
|----|----|----|
| Look at data for the first time | [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md), [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md), [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md), [`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md), [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md), [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md), [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md), [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md), `sentinelas_naniar` | [Getting started](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Profile against a database | [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md) — full-table SQL aggregates plus a 99-field profile from a declared sample; the scopes stay separate | [Reference](https://sebollin.github.io/lupa/reference/) |
| Find undeclared structure | [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md), [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md), [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md), [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md), [`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md) | [Getting started](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Define quality | [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md), [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md), [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md), [`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md), [`perfiles_madurez()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md), [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md) | [Quality model](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Measure and evaluate | [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md), [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md), [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md) with project weights, [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md), [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md) with the user-declared instruction `desenlace = "suprimir"` (not a factory threshold), [`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md), [`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md), [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md), [`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md) | [Quality model](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Clean safely | [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md), [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md), [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | [Cleaning plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.html) |
| Find approximate duplicates | [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md), [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md) | [Scale and duplicates](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html) |
| Repair encoding damage | `reparar_codificacion` through [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) and [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | [Cleanup reference](https://sebollin.github.io/lupa/reference/planificar_limpieza.html) |
| Follow quality over time | [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md), [`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md), [`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md), [`leer_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md), [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md), [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md), [`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md) | [History and drift](https://sebollin.github.io/lupa/articles/historico-y-deriva.html) |
| Share results | [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md), [`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md), [`leer_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md) | [Reporting reference](https://sebollin.github.io/lupa/reference/reportar.html) |
| Validate and extend | [`validadores_internacionales()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`validadores_uruguay()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`validar_ci_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md), [`validar_rut_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md), [`validar_luhn()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_mod97()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_iso3166()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_iso4217()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_correo()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_url()`](https://sebollin.github.io/lupa/reference/validadores_formato.md) | [Reference](https://sebollin.github.io/lupa/reference/) |

## 📐 Scope: what uses every row and what is sampled

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
uses every row for table and column counts, real and disguised
missingness, distinct values, exact duplicates, quantitative summaries,
and the findings derived from those quantities. By default,
`muestra = 1e5` limits pattern discovery, type inference, date-format
discovery, and the common sample used to search for functional
dependencies. Set another limit or `Inf` to change or disable that
sampling.

Personal-document validators have a separate preliminary filter:
`muestra_validadores = 1000` by default. A validator that passes that
filter is then evaluated on the complete column; `Inf` makes even the
preliminary pass complete. Approximate duplicates are off by default and
have their own declared bounds when enabled.

The result records the effective scope in `meta$muestra`,
`meta$filas_analizadas`, and `meta$muestreo`; each column also records
`n_filas_analizadas_tipo` and `muestreado_tipo_inferido`, while the
dependency table carries its analysed-row and sampling attributes.
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
reuses `muestra = 1e5` for its profile, distributions, and
observed-level enumeration, and declares separate limits for
associations and the other components.

## 🛣️ `perfilar()` or `analizar()`?

Use
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
when you want the focused, inspectable profile: column summaries,
patterns, inferred types, findings, diagnostic coverage, and undeclared
structural relationships. Use
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
when you want the full route around that profile: distributions,
associations, temporal analysis, confirmable variable classification, a
model proposal, a cleanup plan, conceptual coverage, and a dashboard.

With no confirmed model or proposal,
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
measures every proposal row whose state is `"lista"` by default. That
proposal was inferred by `lupa`; nobody has confirmed it. Use
`medir_propuesta = FALSE` to keep the route descriptive, or supply a
confirmed proposal/model. The function aggregates immediately and keeps
the small dashboard; `conservar_detalle_medicion = TRUE` retains the
row-level measurement detail.

## 🚦 Severities and automation

`severidad` is an **ordered factor**: `ok < sospechoso < error`.

- `ok` records an observed condition that is acceptable or
  informational; it is not an adverse decision.
- `sospechoso` is evidence worth reviewing. It is heuristic or needs
  domain context and must not by itself reject, repair, or suppress data
  automatically.
- `error` states that the applicable check crossed its declared
  criterion. Of these three levels, it is the only candidate for an
  adverse automated gate, and only after the project accepts that
  criterion and verifies the scope.

`cobertura_diagnosticos` is outside this scale. It lists checks that
could not be evaluated and how to resolve them. Automation must inspect
it as well as `error`: zero errors does not mean a clean profile when
diagnostics were not run. Cleanup is always
explicit—[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
changes only actions selected from an editable plan.

## ✨ What lupa does in detail

- Profiles a delivery and surfaces missingness, types, patterns, dates,
  and personal-data evidence.
- Profiles `sf` geometries and declares CRS, geometry families,
  emptiness, planar validity, coordinate domain, and bounding-box scope;
  it does not perform spatial analysis.
- Applies Benford’s law only when its preconditions hold, and records
  non-applicability in `cobertura_diagnosticos`.
- Reports `unidades_mixtas` and `monedas_mixtas` in a column without
  converting values or assuming exchange rates.
- Reports `celdas_multivaluadas` only when homogeneous parts match the
  column’s patterns.
- Finds `relacion_aritmetica_columnas` as observed identities or
  proportions between numeric columns, not as domain rules.
- Finds `relacion_orden_columnas` between comparable columns and
  declares the comparison scope.
- Finds keys, relationships, dependencies, and measurement granularities
  that were never declared.
- Reports `casi_clave` when a non-temporal column has at least 100 rows,
  is almost unique, and its collisions concentrate in a few values,
  which separates a key with duplicates from free text of high
  cardinality.
- Lets a project define its own quality framework instead of forcing a
  global score.
- Measures and evaluates explicit metrics, scales, validity rules, and
  referential domains.
- Produces editable cleanup plans, applies only selected actions to a
  copy, and keeps an audit log.
- Finds approximate duplicates with exact tiles, deterministic
  MinHash/LSH, blocking, cost estimates, and disk-backed lots.
- Repairs encoding damage in R, including repeated mojibake and CESU-8,
  while refusing unsafe lossy conversions.
- Follows quality through time and creates self-contained HTML reports.

## 🧪 Evidence, with its declared scope

Each check below uses a different declared unit and reference. None of
them estimates a single package-wide accuracy.

| Check | Declared unit | Result |
|----|----|---:|
| Raha dirty/clean pairs | columns containing at least one changed cell | 26/26 received at least one finding; 8 further columns were flagged |
| Constructed clean controls | 43 tables | 0 error-severity findings; 25 review signals |
| Seeded defects | 9 planted defects | 9/9 detected |
| Real sanctions register | error-severity findings over 2,556 rows | 8/8 independently confirmed |

In the Raha pairs the dirty/clean comparison labels changed cells; it
does not label every property observable in an unchanged column. Manual
review found a supported observation in each of the eight further
columns—constants, duplicated columns, inconsistent case, empty strings,
and high-cardinality text. We therefore report neither precision nor
diagnostic recall from Raha: 26/26 is column coverage, not evidence that
every changed cell was identified.
[`benchmark/`](https://sebollin.github.io/lupa/benchmark/) reproduces
the table from the published sources and records the exact file
fingerprints used by the published run, but only when `lupa` is
installed from a build of these same sources. From the repository root,
reproduce that condition and run the scripts with:

``` sh
R CMD build . && R CMD INSTALL lupa_0.1.0.tar.gz
Rscript benchmark/verdad_raha.R
Rscript benchmark/medir_lupa.R
```

The benchmark records the installed version and full `Built` stamp and
stops when that installation lacks a capability required by the
published table.

## 🔍 Limits, fit, stability, and references

There is no factory quality score:
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
returns the dashboard unless a project supplies complete named weights,
and a calculated index always travels with its coverage, weights,
transformations, and heterogeneous universes. The core is universal and
catalogues are pluggable;
[AGESIC](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
v1.6 is a reference implementation, not a country lock. The only
required import is [`cli`](https://cran.r-project.org/package=cli).
Suggested packages enable bounded capabilities:
[`sf`](https://cran.r-project.org/package=sf) enables geometry
profiling; [`DBI`](https://cran.r-project.org/package=DBI) provides the
database interface and
[`RSQLite`](https://cran.r-project.org/package=RSQLite) a backend for
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md);
[`stringdist`](https://cran.r-project.org/package=stringdist) enables
approximate text comparison.

Work that can be parallelised uses **two threads by default**, the
ceiling CRAN asks packages to respect. On your own machine you can raise
it per call with `nucleos = 8` or for the whole session with
`options(lupa.nucleos = 8)`; the result does not change, only how long
it takes.

### Where it fits

[`skimr`](https://cran.r-project.org/package=skimr) and
[`DataExplorer`](https://cran.r-project.org/package=DataExplorer)
explore; [`pointblank`](https://cran.r-project.org/package=pointblank),
[`validate`](https://cran.r-project.org/package=validate), and
[`dataquieR`](https://cran.r-project.org/package=dataquieR) express or
evaluate rules;
[`zoomerjoin`](https://cran.r-project.org/package=zoomerjoin),
[`textreuse`](https://cran.r-project.org/package=textreuse), and
[`reclin2`](https://cran.r-project.org/package=reclin2) focus on text
comparison or record linkage.
[`calidad`](https://github.com/inesscc/calidad), maintained by [Klaus
Lehmann](https://github.com/Klauslehmann) and [Ricardo
Pizarro](https://github.com/ricardoflopiza), is a complementary axis: it
evaluates the quality of **survey estimates**, while `lupa` evaluates
the tabular data that produces an estimate.

Encoding repair follows the approach and frozen data of
[`ftfy`](https://github.com/rspeer/python-ftfy) 6.3.1 by [Robyn
Speer](https://github.com/rspeer), in R. It includes eleven byte tables,
CESU-8 and Java `C0 80` handling, and five deliberate extensions
documented in the [NEWS](https://sebollin.github.io/lupa/NEWS.md). It
reproduces 159 of the 161 distributed corpus cases and leaves all 31
negative cases untouched. It deliberately does not provide `ftfy`’s
style-oriented `fix_text` steps such as HTML unescaping, quote curling,
width normalization, or Unicode normalization: changing legitimate data
silently is not repair.

### Citation and references

``` r

citation("lupa")
```

Conceptual references are [Batini and Scannapieco
(2016)](https://doi.org/10.1007/978-3-319-24106-7), the [AGESIC Digital
Government Data Quality Framework
v1.6](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/),
and [ISO/IEC 25012:2008](https://www.iso.org/standard/35736.html).

### Stability, contribution, and license

Version 0.1.0 is pre-CRAN: the public API may change before 1.0;
breaking changes will be announced in `NEWS.md` and release notes, with
a deprecation warning first whenever practical.

Please use the [issue tracker](https://github.com/sebollin/lupa/issues)
for bugs, proposals, and documentation fixes. The stable contracts are
the declared units, scope, protection, and audit trail; implementation
details and benchmark times can change between releases when those
contracts remain true.

`lupa` is released under the
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.html). See
[`LICENSE.note`](https://sebollin.github.io/lupa/LICENSE.note) for the
Apache-2.0 data derived from
[`ftfy`](https://github.com/rspeer/python-ftfy) and the MIT data derived
from [`naniar`](https://github.com/njtierney/naniar).

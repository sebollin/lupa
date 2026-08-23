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

**Whole databases, not just one table.** `coleccion()` declares which tables
make up a database — schema included, because the schema is part of a table's
identity — and `perfilar_coleccion()` returns one row per table plus the
coverage of what it could not measure. The boundary is *declared*, never
discovered: walking a catalogue would turn a permissions error into a result,
and real collections run past a thousand tables across dozens of schemas.
Tables a credential cannot read land in `cobertura_coleccion` with their reason,
never as a zero — partial permissions are the normal case, not the edge. There
is no snapshot: every table carries the moment it was measured, and the object
says so.

**Contradictions no single column shows.** Declare that several columns encode
the same fact with `senal_redundante()`, and `detectar_discordancias()` reports
the rows where they disagree — the year of the date against the fiscal year
against the file year. Each of the three can be perfectly plausible on its own
and still contradict the others. The group is declared, never guessed: two year
columns might be birth year and enrolment year, and there is no reason for those
to match.

**Findings you can verify, not just read.** Pass `clave` to `perfilar()` with
the columns that identify a row, and every finding's traceability carries those
values for the rows it points at — so you can look the case up in the source
system without opening the table. Row indices stay as the fallback;
`trazabilidad$localizador` says which one you got. There is a tension the
feature cannot ignore: the key that lets you verify is exactly what identifies a
person, so a key column classified as personal data comes back masked, the same
way evidence does, and `claves_protegidas` says which.

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
| Profile against a database | `perfilar_dbi()` — full-table SQL aggregates plus a 109-analytic-field profile from a declared sample; the scopes stay separate | [Profiling a database](https://sebollin.github.io/lupa/articles/perfilar-una-base.html) |
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

## 🗄️ Engines: what is tested and what is expected

`perfilar_dbi()` does not promise a universal dialect. It resolves the dialect
with a zero-row probe **before** issuing the aggregate block, and whatever the
engine rejects is recorded as unavailable with its reason — never as zero.

| engine | dialect | status |
| --- | --- | --- |
| SQLite | `limit` | **tested** against the real engine, in the suite |
| engine that rejects `LIMIT` | `top` / `portable` | **tested** with a simulated engine in the suite |
| engine that folds aliases to upper case | any | **tested** with a simulated engine |
| engine that rejects `SELECT *` over one column | any | **tested** with a simulated engine |
| **PostgreSQL 16** | `limit` | **tested** against the real engine: dialect resolved by probe, mean, median and standard deviation verified against R, schemas, collections and partial permissions; re-tested for the five modes, where the probe picks row-level `BERNOULLI` over block-level `SYSTEM` |
| **MySQL 8** | `limit` | **tested** against the real engine: same three statistics verified against R |
| **SQL Server 2022** | `top` | **tested** against the real engine: the probe resolves `top` on its own, and the three statistics match R |
| **DuckDB 1.5** | `limit` | **tested** against the real engine: all five modes with no unavailable metric, and the three statistics verified against R |
| **MariaDB 11** | `limit` | **tested** against the real engine: all five modes with no unavailable metric, the three statistics against R, and the plan's lower bound matching the queries actually emitted in the five |
| **Oracle Free 23 (23c)** | `fetch_first` | **tested** against the real engine: dialect resolved by probe, five modes with no unavailable metric, the three statistics against R, the plan's lower bound matching the queries actually emitted, qualified names by text and by `DBI::Id`, and `SAMPLE (p)` sampling |
| Oracle 11 and earlier | `rownum` | expected, not checked against the engine |
| any other DBI-compatible engine | `portable` | fallback: `dbSendQuery()` + `dbFetch(n)` |

The claim is reproducible: `benchmark/verificar_motor.R` takes any DBI
connection and checks the six things the table promises — dialect resolved by
probe, no unavailable metric in the five modes, the three statistics against R,
the plan against the queries actually emitted, schema-qualified names by text and
by `DBI::Id`, and a two-table collection.

What that script checks is **behaviour**, and it can be redone against any
connection. The **timings** of those runs — the seconds and row reads that appear
in the release notes — cannot be redone from the repository: they need the
infrastructure of the run, up to two million rows by forty columns on an engine
brought up for the occasion. They are published as what they are, references from
a one-off run, and no result of the package depends on them.

**Expected** means the dialect is built and tested against a simulated engine that
reproduces the restriction, not that it has been run against the real engine. The
distinction matters, which is why it is written down: the defects this version
fixed did not surface in eight green environments precisely because all of them
used the same engine.

Every engine added to this table so far has found a defect no simulated engine
could. DuckDB found the sharpest one: it accepts
`TABLESAMPLE SYSTEM (10) WHERE 1 = 0` and rejects the same clause without the
filter, because with a trivially false filter its parser never validates the
sampling method. The capability probe used exactly that filter to stay cheap, so
it passed and the real query failed. **A probe that does not exercise the form it
later emits proves nothing** — the same lesson the standard-deviation probe
taught one round earlier.

The dialect can be declared with `dialecto =` if the probe gets it wrong. A
partial failure never discards what was already measured: if reading the sample
fails, the object comes back with a complete `resumen_tabla`, `perfil_muestra =
NULL`, and a coverage row carrying the reason.

### Knowing what is missing before you hit it

`lupa` has one hard dependency, `cli`. Everything else is optional — and what
used to happen when something was missing was an R error, or a driver error,
that named neither what was missing nor how to get it. The hard case is not the
R package but the **system library underneath it**: `RMariaDB` does not compile
without the MySQL or MariaDB client headers, `ROracle` needs Oracle Instant
Client, and someone staring at `installation of package 'RMariaDB' had non-zero
exit status` has no way to know the answer is `libmariadb-dev`.

```r
requisitos_motor()            # the whole catalogue
requisitos_motor("oracle")    # what Oracle needs, and how to get it
```

For each engine it declares the R package, the system library with its name on
Debian and on Fedora, **the way around it without administrator rights** where
one exists, the expected dialect, and whether it is tested against the real
engine. The escape hatches are not hypothetical: SQL Server had no ODBC driver
and no way to install one, and it was solved by compiling FreeTDS into a user
prefix and pointing `odbc` at the `.so` by path; Oracle Instant Client unzips
into a user directory. Neither needed `sudo`.

What it does **not** do is claim to have checked a system library it cannot
check. When it can only say "the R package is missing, and if installing it
fails to compile, this is what you need", it says exactly that — the package's
own invariant applied to its own installation.

### One query per batch, not one per column

Profiling issued **one query per column** for each block of metrics. On a table
of tens of millions of rows that is the cost: not the sampling, the number of
scans. The flat aggregates — counts, min/max/mean/zeros/negatives, and standard
deviation — are now asked for **several columns in one query**, in batches. Mode
and median stay one per column, because they group and sort.

Measured against PostgreSQL 16 with **2 million rows by 40 columns**:

| mode | before | after |
| --- | --- | --- |
| `conteos` | 46 queries, 5.4 s | **8 queries, 2.4 s** |
| `seguro` | 128 queries, 15.2 s | **14 queries, 5.3 s** |

Same 160 and 400 metrics computed, and the same numbers: on one table seeded
once, the consolidated profile and the previous one agree on all sixteen summary
fields across six column types.

**If a batch fails, the batch is not lost.** It is retried column by column, and
whatever still fails is left `no_disponible` with its reason while its
neighbours are computed. A shared query is the perfect way to reintroduce the
all-or-nothing reflex this package fixed in five places, so the degradation was
built first and has its own tests.

`resumen_tabla$sql` keeps **one row per column and metric** with every field it
had, and adds `lote` and `columnas_compartidas` so a shared query is visible.

### Reading a profile without knowing its shape

`perfilar()` returns a flat `perfil`; `perfilar_dbi()` returns a container. So
`perfil$general$filas` worked on one and returned `NULL` on the other, where the
count lives in `resumen_tabla$meta$filas`. A silent `NULL` in a measurement
script is the worst way to fail: it does not warn, and everything after it
computes on nothing.

```r
hallazgos(x)    columnas(x)    cobertura(x)    n_filas(x)    sql_perfil(x)
```

They work over `perfil`, `analisis`, `perfil_dbi` and `perfil_coleccion`, and
they do not invent what is not there: a DBI profile with no sample read returns
an empty findings table **with its warning**, and `sql_perfil()` on an in-memory
profile returns `NULL`, because a table with no rows would suggest SQL was
issued and found nothing.

### A budget measures work, it does not count units

A cap that counts units treats a column of ten-character codes and one of
thousand-character WKT alike. A table in the PostGIS catalogue — 3,912 rows —
took 243 seconds, and the vocabulary detector was 99.6% of it: 800 distinct
values are 319,600 pairs, well under the two-million cap, but each comparison
was a Jaro-Winkler over 900 characters.

The budget is now measured in **character comparisons**, the inner loop of the
distance. Calibrated against measurement, the pathological column drops from
61.3 s to 4.6 s. An ordinary column of two thousand values is compared in full
**as long as its values are under about a hundred characters**: the budget bites
when `L² · n(n−1)/2` exceeds `2e10`, which for two thousand distinct values means
a length of 101. Saying "two thousand values are compared in full" without that
qualifier was wrong, and wrong in the worst place — the 900-character WKT column
that motivated the budget is exactly the kind that gets trimmed. What does get trimmed is declared: how many normalised forms
went uncompared, how much work that was, and which cap did the trimming.

And when a budget must trim, the forms it keeps are the **alphabetically first**,
not the first to appear. That distinction was a defect, measured on a real column
— 45,400 street names from the national open-data catalogue, 8,318 distinct
forms. The same rows yielded 26 near-duplicate groups in the order the file
arrives, 70–85 shuffled, and 148 sorted. A profiler whose verdict depends on the
row order is measuring the physical shape of the table rather than the data.
Sorting first, all five orders yield 148 — and sorting also keeps near-duplicates
adjacent, so the cut falls between families instead of splitting them.

### Cost is planned before it is paid

Profiling a 158-column table in `modo = "exacto"` emits 262 queries, and 256 of
them scan, sort or group the whole table. The count follows the composition, not
the column count: the same 158 columns as text only cost 172, because a median
asks for a full sort per numeric column. `muestra` does not bound any of it — it
bounds what is brought into R, not the work the engine does, and the sampled plan
over the same table costs 271. So the cost is declared and chosen
(`benchmark/medir_plan_ancho.R` reproduces the four numbers):

```r
plan_perfilado_dbi(con, "tabla", modo = "muestreado")   # 5 queries, predicts the rest
```

The plan gives a **range** for how many queries the profiling will emit, and it
says so in `attr(plan, "supuesto")`. The low end is `total`, reached when no
batch is rejected: a column with no value emits neither median nor standard
deviation, and the plan cannot know which ones are empty without asking — which
would change its own cost. The high end is `total_lotes_rechazados`, reached
when the engine rejects every batch and each column is retried on its own. The
real cost falls between the two, and the plan says so in both directions rather
than promising a bound it cannot keep.

The part that *is* a hard design constraint is that the prediction does not depend
on the engine: every capability probe costs a fixed number of queries even when
it succeeds on the first form, because a cost that varied by engine would leave
the user guessing again.

But counting queries does not answer the question the reader actually brings:
fourteen queries over two million rows are far more work than two hundred over a
thousand. So the plan also estimates **magnitude**, in real counts rather than an
invented index — and it estimates it in **two halves**, because the clock is not
always set by the engine. The engine half is `filas_leidas` and
`ordenaciones_completas`, summarised in `magnitud_motor`; the client half is
`columnas_texto` and `pares_texto` — how many pairs of forms the vocabulary
detector could compare in R over the sample — summarised in `magnitud_texto`.
`magnitud` is the larger of the two.

Counting only the engine gave false verdicts out of true numbers: a 3,912-row
PostGIS catalogue table with one geometry column stored as text asked for 64,592
row reads and no sorts — magnitude `"baja"` — and took 35 seconds, because the
work was in comparing forms, which is not a row read. Printing the plan shows
both halves, and the high-work warning names the levers that bound it, which
differ on each side. It is an estimate and says so: the engine half counts the
rows that would have to be read if no index helped, and the client half counts
pairs, whose unit cost depends on value length — something the plan cannot know
without reading them, so for very long text the real time is several times what
the reference suggests. The published
numbers do not depend on those assumptions, so anyone who disagrees with them can
redo the arithmetic.

| mode | what it does |
| --- | --- |
| `exacto` | every metric over the whole table |
| `seguro` | drops the metrics that sort the whole column |
| `conteos` | counts only |
| `muestreado` | metrics over rows sampled **in the engine**: `TABLESAMPLE` where it exists, a pseudo-random order with a limit where it does not |
| `aproximado` | native approximate functions: `APPROX_COUNT_DISTINCT`, `PERCENTILE_CONT`, `approx_quantile` and their fallbacks |

Every sampled or approximated metric travels saying so. `estado` distinguishes
`calculado`, `estimado` and `no_disponible`, and each row carries `universo`,
`tamano_muestra`, `fraccion`, `metodo` and `error_esperado` — `desconocido` when
the engine documents no bound, never an invented one. Distinct counts get their
own state, `observado_muestra`: the cardinality of a sample does not estimate
the cardinality of the universe without a declared estimator, so it is reported
as what it is — what was seen in the sample, with the universe stated beside it.
An engine with no sampling capability does not break: the mode degrades and says
so in the coverage table.

## 🕳️ Emptiness by design is declared, not counted as a defect

Every profiler assumes a table shape. `lupa` assumes one row is one fact, one
column is one semantic domain, and an empty cell should have had a value. The
third assumption is the one that hurts: an administrative table is full of
legitimate emptiness — an open-ended validity interval, a survey skip pattern,
columns that are mutually exclusive by subtype, an entity-attribute-value model.
Counting those as missing is arithmetically right and semantically wrong.

`aplicabilidad` declares, per column, the rows where the column applies. Rows
outside that universe leave `n_faltantes` and `prop_faltantes` instead of being
reported as absence:

```r
perfilar(encuesta, aplicabilidad = list(marca_auto = ~ tiene_auto == "Si"))
```

`columnas_opcionales` covers the simpler case, where absence is never a defect
and there is no rule to write. The declared rule, the resulting universe, and
the rows where the rule could not be evaluated all land in
`cobertura_diagnosticos`: a narrowed universe without a record would be the same
defect in reverse. Rows whose rule cannot be determined are counted apart, in
`n_aplicabilidad_indeterminada`, because not knowing is not the same as not
applying.

Declaring the universe also enables the symmetric error, which had no way to
appear before: `valor_fuera_de_aplicabilidad` reports a value present where the
rule says the column does not apply.

The same idea governs the statistical tests. Benford assumes a multiplicative
process and Tukey's fences assume a distribution; a numbering — an identifier, a
code — is neither, and a code sitting far from the median says nothing about its
quality.

Recognising a numbering takes **two signals, and it needs both**. The first is
**density**: an identifier occupies a compact stretch of the integers while a
magnitude spreads across several orders. Uniqueness does not work, since an
amount is nearly unique too. The second is the **absence of a scale jump**, and
without it the first does harm: a value off the scale by up to twice the maximum
does not lower the density enough, so a `120` among ages 18 to 70 — or a `2000`
behind 1..1000 — was hidden exactly when it was the only thing worth seeing. What
does give them away is the gap they open: 50 and 1,000 where the typical one is 1.

The criterion was chosen by measuring. A bench of thirteen columns with the known
answer — five numberings and eight magnitudes with a bad value inside — compared
four variants: crossing both signals gets all thirteen right and **never silences
a real bad value**; density alone got eleven and silenced two. It lives in
`test-ronda118.R`.

**And what is not run is not switched off silently**: it leaves its row in
`cobertura_diagnosticos` with the measured reason — what share of the integers
the column covers, how many values would have been flagged, how many rows out of
how many the sample carries.

Where no signal discriminates, `lupa` speaks. High cardinality in a text column is
always reported, because the length of the values does not tell a catalogue from
prose — it fails in both directions, measured — and the finding does not claim it
is a defect: it offers the three possible readings so that whoever knows the
column decides.

`perfilar_por()` answers the long format, where one column stacks unrelated
domains. It profiles each group separately, drops the wholly-absent columns
inside each group before profiling, and declares what it dropped.

`lupa` does not infer the model. But declaring the universe requires knowing the
option exists, and someone profiling a conditioned table without declaring
anything got exactly the misleading report the feature was built to prevent. So
the package **measures the evidence and offers it**: when the value of one
column decides which rows have another, or when two columns split the rows
without overlapping, `posible_ausencia_estructural` reports it with severity
`ok`, the measured evidence, and the line to paste:

```
valor_a  posible_ausencia_estructural  ok
  evidence  `tipo` predicts the presence of `valor_a` in 100.0 % of 200 rows,
            with 2 distinct values. The column applies when tipo is "A".
  suggests  perfilar(datos, aplicabilidad = list(valor_a = ~ tipo == "A"))
```

It suggests; it does not decide, and it never rewrites the universe on its own.
Columns already declared are left out of the examination. On twenty real
datasets shipped with R and sixty random tables with independent missingness it
produces zero signals; it fires on the entity-attribute-value model, the survey
skip pattern and the mutually exclusive columns, and stays quiet when ten per
cent of the rows break the rule, because then the relation exists and is not a
rule.

The other side of the same coin is `regla_silencia_ausencia`, also `ok`: a
column declared optional or with its own universe that stays almost empty
*inside* that universe gets a notice. The declaration worked and that is why the
profile came out clean — the notice exists so that is a decision and not a side
effect.

`columnas_personales` closes the equivalent gap on the other declaration the
package cannot make alone. No lexicon of column names can be complete: a column
holding identity documents can be called `cod_benef`, and no list of frequent
names will recognise it. Declaring it wins over inference and is not re-examined.

The vignette `vacio-por-diseno` documents the assumption and the six table
shapes where it does not hold.

## 🔢 Declared units and row traceability

Every finding declares the unit used by `n_evaluados`, `n_afectados`, and
`unidad_conteo`. `mayusculas_inconsistentes` and `normalizacion_unicode` use
`valor_distinto`: they count distinct values, while their trace remains a row
trace. It lists every row containing an affected value, not only rows that are
themselves defective. `casi_duplicados_vocabulario` follows the same contract:
its count is the number of variant values, and its trace lists every row whose
value belongs to a selected group, including the dominant form. A trace can
therefore contain more rows than `n_afectados`; those rows are useful when a
whole collision group must be reviewed or unified. The vocabulary detector is
heuristic, so the trace is evidence for review, not a verdict that every row
must be corrected. The trace presents non-dominant forms first and dominant
forms afterward; its evidence reports how many displayed rows belong to each.

For `patron_raro`, `resumen_patrones` and the evidence show at most six rare
patterns. Traceability uses the complete set of rare pattern names, without
retaining their frequency table, up to a separate limit of 5,000 names. If
that limit is reached, the trace scope is partial and `cobertura_diagnosticos`
states the limit; the six-pattern presentation cap is not itself a trace gap.
Every finding also reports the dominant pattern proportion and how many rows
belong to non-dominant patterns excluded for exceeding `umbral_patron_raro`.
If no dominant pattern reaches `umbral_patron_dominante`, no finding is emitted:
the non-measurement, its observed proportion, and how to adjust that argument
are recorded in `cobertura_diagnosticos`.

`filas_duplicadas` counts all rows participating in duplicate groups, matching
the metric and the default action that marks those rows. The number of excess
duplicates remains in the evidence. `0` means the check measured no affected
units; `NA` means the count was not measured. The same distinction applies to
diagnostic coverage: a check that could not run is listed in
`cobertura_diagnosticos`, never silently converted to zero.

When a finding and its trace disagree, `perfilar()` preserves the finding and
emits a warning with class `lupa_trazabilidad_incoherente`. The guard compares
the pre-truncation total, checks both directions, and respects the declared
counting unit; it is a diagnostic net, not a substitute for aligning the
detector and its trace.

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

| Check | Declared unit | Result | Reproduced by |
| --- | --- | ---: | --- |
| Raha dirty/clean pairs | columns containing at least one changed cell | 26/26 received at least one finding; 8 further columns were flagged | `benchmark/medir_lupa.R` |
| Constructed clean controls | 31 tables | 0 error-severity findings; 8 review signals | `test-ronda107.R` |
| Real sanctions register | error-severity findings over 2,556 rows | 9/9 independently confirmed | `benchmark/medir_sanciones.R` |

**Every row names what reproduces it, and that is part of the check.** This table
once carried three numbers nobody could verify from the repository: one described
a control set that had shrunk from 43 tables to 31 — and its noise from 25
signals to 8, meaning the package had improved while the text still said the old
figures — another counted nine seeded defects whose fixture is not here, and the
third a real registry with no script to fetch it. The first was measured again,
the second was removed until its fixture exists, and the third now has its
script.

In the Raha pairs the dirty/clean comparison labels changed cells; it does not
label every property observable in an unchanged column. Manual review found a
supported observation in each of the eight further columns—constants,
duplicated columns, inconsistent case, empty strings, and high-cardinality
text. We therefore report neither precision nor diagnostic recall from Raha:
26/26 is column coverage, not evidence that every changed cell was identified.
[`benchmark/`](https://github.com/sebollin/lupa/tree/main/benchmark) reproduces the table from the published sources and
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

# Engines and scale

> This guide is the English version of [Motores y
> escala](https://sebollin.github.io/lupa/articles/motores-y-escala.md).
> `lupa` writes its messages and tables in Spanish, so the output below,
> including the tables this page builds from the package, is in Spanish.

This page is the reference of
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
and
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
for whoever is going to profile a database in production:

- what is computed in the engine and what is brought into R;
- how much work each option asks for;
- what is declared;
- what is not estimated.

The walkthrough with an example is in [Profiling a
database](https://sebollin.github.io/lupa/articles/profiling-a-database.md).
The help ships with the package:
[`?perfilar_dbi`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`?plan_perfilado_dbi`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
and
[`?requisitos_motor`](https://sebollin.github.io/lupa/reference/requisitos_motor.md).
It is the complete source for each argument and each returned field.

The examples run against SQLite and DuckDB in memory every time the page
is built. What needs another engine is described without running it and
with its source.

## Contents

1.  [Default values](#defaults)
2.  [What is computed in the engine and what is brought into
    R](#universes)
3.  [Sampling metadata: 2 paths](#metadata)
4.  [Sampling forms in the engine](#sampling-forms)
5.  [Caps on the read into R](#caps)
6.  [Caps on the approximate comparison](#duplicate-caps)
7.  [What the SQL aggregates do not translate](#not-translated)
8.  [Metrics and strategies](#metrics)
9.  [Batched queries](#batches)
10. [Cost policy](#cost-policy)
11. [The plan before running](#plan)
12. [Cost and spill warnings](#warnings)
13. [Instrumentation and durations](#instrumentation)
14. [Processing memory](#memory)
15. [A run on production databases](#production-run)
16. [Consistency across queries](#consistency)
17. [Dialects and probes](#dialects)
18. [What the engine stores differently from R](#differences-with-r)
19. [The primary key by engine](#primary-key)
20. [Tested engines](#tested-engines)
21. [Partial failures and errors](#failures)
22. [Dependencies and installation](#dependencies)
23. [Threads](#threads)

## Default values

The table comes from the signatures of the 2 functions when this page is
built. A dash means that the function does not have that argument. For
the arguments that choose between options, the one that applies by
default is shown and, in parentheses, the others.

| argumento | [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md) | [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md) |
|:---|:---|:---|
| `universo` | `"tabla_completa"` (`"muestra_motor"`) | `"tabla_completa"` (`"muestra_motor"`) |
| `muestra_motor` | `NULL` | `NULL` |
| `muestra` | `Inf` | `Inf` |
| `orden_muestra` | `NULL` | `NULL` |
| `metricas` | `c("validos", "distintos", "moda", "basicos", "mediana", "desvio")` | `c("validos", "distintos", "moda", "basicos", "mediana", "desvio")` |
| `estrategia_distintos` | `"exacta"` | `"exacta"` |
| `estrategia_mediana` | `"exacta"` (`"aproximada_motor"`) | `"exacta"` (`"aproximada_motor"`) |
| `politica_costo` | `"todas"` (`"por_cardinalidad"`) | `"todas"` (`"por_cardinalidad"`) |
| `bloque_muestra` | `"con_muestra"` (`"solo_agregados"`) | `"con_muestra"` (`"solo_agregados"`) |
| `max_consultas` | `Inf` | `Inf` |
| `dialecto` | `"auto"` | `"auto"` |
| `incluir_valores` | `TRUE` | `TRUE` |
| `tamano_lote` | `NULL` | `NULL` |
| `tamano_lote_planos` | `20L` | `20L` |
| `tamano_lote_distintos` | `2L` | `2L` |
| `instrumentar` | `TRUE` | `FALSE` |
| `umbral_cardinalidad` | `0.5` | `0.5` |
| `avisar_costo_distintos` | `TRUE` | — |
| `umbral_segundos_aviso_distintos` | `30` | — |
| `avisar_costo_moda` | `TRUE` | — |
| `umbral_segundos_aviso_moda` | `30` | — |
| `avisar_costo_mediana` | `TRUE` | — |
| `umbral_segundos_aviso_mediana` | `30` | — |
| `avisar_derrame_estimado` | `TRUE` | — |
| `umbral_bytes_aviso_derrame_estimado` | `0` | — |
| `max_celdas_muestra` | `1.000.000` | `1.000.000` |
| `max_bytes_muestra` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `bloque_filas` | `NULL` | `NULL` |
| `max_bytes_procesamiento` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `max_bytes_materializacion` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `columnas_opcionales` | `character(0)` | — |
| `aplicabilidad` | `NULL` | — |
| `sentinelas_numericos` | `c(-9, -99, -999, -9999, 999)` | — |

Consequences for a run in production:

- With `universo = "tabla_completa"` (*universe = whole table*) and
  `muestra = Inf` (*sample*), the SQL aggregates go through the whole
  table. With those same values, the block `perfil_muestra` (*sample
  profile*) tries to bring into R **all** the rows that
  `max_celdas_muestra` and `max_bytes_muestra` allow. `muestra` bounds
  only that read, never the aggregates. To bound the work of the engine
  there is `universo = "muestra_motor"` (*engine sample*); to bring no
  rows, `bloque_muestra = "solo_agregados"` (*sample block = aggregates
  only*).

- `instrumentar` (*instrument*) is `TRUE` in
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  and `FALSE` in the plan: the run times each query and the plan does
  not.

- `perfil_muestra` is computed with
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  on the rows brought in, with `muestra = Inf` and no caps of its own.
  The caps were already applied when reading. That is why the discovery
  of patterns, types and dates goes through all those rows.
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  in memory, instead, does it by default on a sample of 100,000 rows.
  The other arguments of
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  arrive through `...`.

  In `universo = "muestra_motor"`, the materialization is bounded before
  `dbFetch(n)` with a conservative bound on the maximum row size. Long
  texts are requested as length and summary: that way their content
  cannot make the fetch exceed that bound. A type without a safe bound
  is probed one row at a time. The part that fits is kept if the budget
  runs out. A cut, a short final sample or a query failure leaves
  metadata and emits `lupa_aviso_muestra_motor`.

## What is computed in the engine and what is brought into R

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
returns an object of class `perfil_dbi` with 2 blocks. They come from
different universes and do not replace each other:

- `resumen_tabla` (*table summary*): SQL aggregates that the engine
  computes (counts, completeness, cardinality, mode, extremes, mean,
  median, standard deviation). It carries one row per column and metric
  in `resumen_tabla$sql`.
- `perfil_muestra`: the full profile of
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  on the rows brought into R, with its findings and its coverage of
  diagnostics.

What goes into each block is decided by 4 arguments:

| argument | decides |
|----|----|
| `universo` | on which rows the engine computes the aggregates: the whole table (`"tabla_completa"`) or a selection made by the engine itself (`"muestra_motor"`, with `muestra_motor = n` required) |
| `muestra` | how many rows are brought into R for `perfil_muestra`; `Inf` brings all the ones the caps allow |
| `orden_muestra` (*sample order*) | which rows are brought and whether the read is reproducible |
| `bloque_muestra` | whether any row is brought (`"con_muestra"`, *with sample*) or only the aggregates are computed (`"solo_agregados"`) |

The usual combinations:

| configuration | SQL aggregates (`resumen_tabla`) | rows brought into R (`perfil_muestra`) |
|----|----|----|
| default | the whole table | all the ones `max_celdas_muestra` and `max_bytes_muestra` allow |
| `muestra = n` | the whole table | up to `n`: the first ones the engine returns, or the first ones by `orden_muestra` |
| `universo = "muestra_motor"`, `muestra_motor = n` | up to `n` rows selected by the engine | the rows actually materialized, cut by `muestra` and the caps |
| `bloque_muestra = "solo_agregados"` | whatever `universo` says | none: `perfil_muestra` is `NULL` and the coverage says `no_solicitado` (*not requested*) |

`bloque_muestra = "solo_agregados"` is independent of `universo`. With
`muestra_motor`, the aggregates still use the selection of the engine,
but no rows are selected, read or kept in the client:

- `meta$materializacion$estado` says `no_solicitado`;
- `meta$bloques$filas_vistas` is 0.

The example uses a table of 2,000 rows in SQLite in memory.

``` r

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
set.seed(11)
n <- 2000
tramites <- data.frame(
  id_tramite = seq_len(n),
  categoria = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  monto = round(stats::rlnorm(n, 9, 1.1), 2),
  edad = c(sample(18:85, n - 20, replace = TRUE), rep(NA, 20))
)
DBI::dbWriteTable(con, "tramites", tramites)
```

``` r

por_omision <- perfilar_dbi(con, "tramites")
con_muestra <- perfilar_dbi(con, "tramites", muestra = 500, orden_muestra = "id_tramite")
del_motor <- perfilar_dbi(con, "tramites", universo = "muestra_motor", muestra_motor = 300)
solo_agregados <- perfilar_dbi(con, "tramites", bloque_muestra = "solo_agregados")

alcance <- function(x) {
  r <- x$resumen_tabla
  data.frame(
    universo = r$meta$universo,
    n_resumen = r$columnas$n[r$columnas$columna == "id_tramite"],
    validos_resumen = r$columnas$n_validos[r$columnas$columna == "id_tramite"],
    filas_en_r = if (is.null(x$perfil_muestra)) 0 else n_filas(x$perfil_muestra)
  )
}
rbind(
  por_omision = alcance(por_omision),
  con_muestra = alcance(con_muestra),
  del_motor = alcance(del_motor),
  solo_agregados = alcance(solo_agregados)
)
#>                      universo n_resumen validos_resumen filas_en_r
#> por_omision    tabla_completa      2000            2000       2000
#> con_muestra    tabla_completa      2000            2000        500
#> del_motor       muestra_motor      2000             300        300
#> solo_agregados tabla_completa      2000            2000          0
```

The metric `n` always keeps the total of the table, also with
`muestra_motor`. The other SQL metrics of that universe are observations
of the selection of the engine, not extrapolations to the universe.

`resumen_tabla$columnas` carries 15 fields per column; `perfil_muestra`,
the full profile of
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
carries 117. The accessors
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
and
[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
work on both kinds of object. Their contract is in [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
(*Reading a profile*, in Spanish).

Without a sample, the protection of personal data, if requested, cannot
classify the columns. It then protects the cell values of all of them:
mode, extremes, mean, median and standard deviation stay
`omitido_por_privacidad` (*omitted for privacy*).

``` r

con_proteccion <- perfilar_dbi(
  con, "tramites", bloque_muestra = "solo_agregados",
  proteger_datos_personales = TRUE
)
table(sql_perfil(con_proteccion)$estado)
#> 
#>              calculado              no_aplica omitido_por_privacidad 
#>                     34                      7                     19
table(sql_perfil(solo_agregados)$estado)
#> 
#> calculado no_aplica 
#>        51         9
```

With a sample, the classification is done on `perfil_muestra` and also
reaches the summary. The detail of what is masked is in [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html)
(*Scope and coverage*, in Spanish).

## Sampling metadata: 2 paths

There are 2 places with sampling metadata, and they describe different
things:

| path | exists when | describes |
|----|----|----|
| `perfil_muestra$meta$origen_dbi$muestreo` | the block of rows was requested and read (`bloque_muestra = "con_muestra"`), with any `universo` | the read into R: `filas_solicitadas`, `filas_obtenidas`, `acotado_en`, `metodo`, `reproducible` and the statement `sql_muestra` |
| `resumen_tabla$meta$muestreo` | only with `universo = "muestra_motor"` | the selection of the engine: `metodo_muestreo`, `sesgo_muestreo`, `tamano_muestra`, `fraccion`, `universo` and the identifiers of the local copy |

In the usual case —`tabla_completa` with `muestra`— the second path is
`NULL`. A script that reads it out of habit, without checking for
`NULL`, computes on nothing:

``` r

is.null(con_muestra$resumen_tabla$meta$muestreo)
#> [1] TRUE
lectura <- con_muestra$perfil_muestra$meta$origen_dbi$muestreo
str(lectura[c("filas_solicitadas", "filas_obtenidas", "acotado_en", "metodo",
              "reproducible")])
#> List of 5
#>  $ filas_solicitadas: num 500
#>  $ filas_obtenidas  : num 500
#>  $ acotado_en       : chr "motor"
#>  $ metodo           : chr "primeras_filas_segun_orden"
#>  $ reproducible     : logi TRUE
```

With `muestra_motor` both paths show up, and their sizes answer
different questions:

- `filas_obtenidas`, from the read into R: how many rows
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  received;
- `tamano_muestra`, from the selection of the engine: how many the
  engine query asked for.

``` r

seleccion <- del_motor$resumen_tabla$meta$muestreo
str(seleccion[c("metodo_muestreo", "sesgo_muestreo", "tamano_muestra",
                "fraccion", "universo", "filas_obtenidas")])
#> List of 6
#>  $ metodo_muestreo: chr "random_limit"
#>  $ sesgo_muestreo : chr "por_fila"
#>  $ tamano_muestra : num 300
#>  $ fraccion       : num 0.15
#>  $ universo       : num 2000
#>  $ filas_obtenidas: num 300
```

In `resumen_tabla$meta$muestreo`:

- `tamano_muestra`: the effective size requested from the query;
- `filas_solicitadas`: the original request;
- `filas_pedidas`: the request after the caps;
- `filas_obtenidas`: the rows that the local copy returned.

If `muestra` is smaller than `muestra_motor`,
`meta$materializacion$n_filas` can exceed `n_filas(perfil_muestra)`.

The cardinality of a sample is published with status `observado_muestra`
(*observed in the sample*). It is what was seen in the sample, with the
universe next to it. It does not estimate the one of the universe
without a declared estimator.

Without `orden_muestra`, the rows brought are the first ones the engine
returns, in an order that SQL does not guarantee. `reproducible` says
so. With `orden_muestra`, the read is declared reproducible only if the
combination of columns is unique in the whole table. The package checks
it (`orden_unico_verificado`).

## Sampling forms in the engine

With `universo = "muestra_motor"`, the selection is made by the engine
with the first form that its dialect offers and its probe accepts. The
order is fixed in the code:

| order | form | offered to | size obtained |
|---:|----|----|----|
| 1 | `TABLESAMPLE RESERVOIR (n ROWS)` | DuckDB | exactly `n` rows |
| 2 | `TABLESAMPLE SYSTEM (p)` | PostgreSQL, SQL Server | whole blocks: it varies around the request and can give 0 rows in a small table |
| 3 | `TABLESAMPLE BERNOULLI (p)` | PostgreSQL, DuckDB | row by row: it varies little around the request, at the price of going through the table |
| 4 | `TABLESAMPLE (p PERCENT)` | SQL Server | by pages |
| 5 | `SAMPLE (p)` | Oracle | row by row |
| 6 | pseudorandom order with the limit of the dialect | any engine with a limit | exactly `n`, at the price of sorting the whole table |

The fixed-quantity form goes first because 2 queries of the same profile
must see samples of comparable size. The block forms cost less on large
tables, and that is why they come before the row-by-row ones. But their
size jumps. DuckDB lets it be seen on the same table of 5,000 rows,
asking for 20% with 6 seeds:

``` r

duck <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
DBI::dbWriteTable(duck, "t", data.frame(id = seq_len(5000)))
contar <- function(muestreo, semillas = 1:6) {
  vapply(semillas, function(s) {
    sql <- paste("SELECT COUNT(*) FROM t TABLESAMPLE", sprintf(muestreo, s))
    DBI::dbGetQuery(duck, sql)[[1]]
  }, numeric(1))
}
tamanos <- rbind(
  bloques = contar("20 PERCENT (system, %d)"),
  fila_por_fila = contar("20 PERCENT (bernoulli, %d)"),
  cantidad_fija = contar("RESERVOIR (1000 ROWS) REPEATABLE (%d)")
)
colnames(tamanos) <- paste0("semilla_", 1:6)
tamanos
#>               semilla_1 semilla_2 semilla_3 semilla_4 semilla_5 semilla_6
#> bloques            2048         0         0      5000      2048         0
#> fila_por_fila      1025       996       984       963       960       985
#> cantidad_fija      1000      1000      1000      1000      1000      1000
```

Out of 1,000 rows requested:

- block sampling gave between 0 and 5,000;
- row-by-row sampling, between 960 and 1,025;
- fixed-quantity sampling, always 1,000.

What each form implies is published:

- `TABLESAMPLE SYSTEM` publishes `sesgo_muestreo = "por_bloques"`
  (*sampling bias = by blocks*) with the reason
  `sesgo_muestreo:tablesample_system_por_bloques`. If it receives 0 rows
  or less than half of the requested ones, a broader form is tried once.
  The methods, the retry and the rows obtained stay in
  `resumen_tabla$meta$muestreo`. If the retry reaches the request, no
  warning is emitted. Only the short final sample stays `no_disponible`
  (*not available*) with
  `muestra_inestable:tablesample_system_tamano_insuficiente` and emits
  the degradation warning.

- The pseudorandom order publishes `sesgo_muestreo = "por_fila"` (*by
  row*). In SQL Server, where the function is `NEWID()`, the median
  under `muestra_motor` only accepts that form if:

  - the table has up to 100,000 rows;
  - the requested sample is at least 20% of the table;
  - the projection, of 650 ms per million rows per pending median, does
    not go over 1,000 ms.

  They are constants of the code, not measurements of the run. Outside
  those limits, the median stays `no_disponible` with
  `capacidad_no_aceptada:newid_costo_excede_presupuesto`.

- If no form is accepted, the SQL metrics stay `no_disponible`. They are
  never replaced by results of the whole table.

- If the sample comes back empty, the metrics with scope `muestra` are
  published as `NA` with status `no_disponible` and a reason
  `muestra_vacia:...`. Zero is not published, and the column is not
  concluded to be empty.

## Caps on the read into R

`perfil_muestra` has 2 caps, which do not touch any aggregate query:

- `max_celdas_muestra` (1,000,000 cells by default) is resolved **before
  reading**, with the row count and the width of the schema.
- `max_bytes_muestra` (512 MiB by default) is crossed with the budget of
  the spool. In the engines that allow it, it uses the catalog. In the
  others, it starts with a small block and adapts `dbFetch(n)`. The
  sample is not brought in whole to be cut in R.

If `muestra = n` and a cap is stricter, the smaller one rules. The
coverage of diagnostics of `perfil_muestra` declares the cells or bytes
observed, the threshold and which cap ruled. `Inf` in a cap turns it
off. With a cap of 1,000 cells on 4 columns:

``` r

acotado <- perfilar_dbi(con, "tramites", max_celdas_muestra = 1000)
str(acotado$perfil_muestra$meta$origen_dbi$muestreo[c(
  "filas_solicitadas", "filas_maximas_por_celdas", "celdas_solicitadas",
  "filas_obtenidas", "acotado_en"
)])
#> List of 5
#>  $ filas_solicitadas       : num Inf
#>  $ filas_maximas_por_celdas: num 250
#>  $ celdas_solicitadas      : num 8000
#>  $ filas_obtenidas         : num 250
#>  $ acotado_en              : chr "motor"
"muestra_perfilado" %in% cobertura(acotado$perfil_muestra)$diagnostico
#> [1] TRUE
```

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
anticipates the cut of cells in `attr(plan, "tope_muestra")` and warns
that the one of bytes will need the probe.

The work is governed by 4 other limits:

| argument | what it bounds | when it runs out |
|----|----|----|
| `max_consultas` | number of queries of the run | the remaining metrics stay `no_disponible` and the coverage says `presupuesto_agotado` (*budget exhausted*) |
| `max_bytes_materializacion` | bytes of the local copy of `muestra_motor`, bounded before each `dbFetch(n)` and checked before each write | `spool_presupuesto_excedido` and `muestra_inestable:presupuesto_materializacion`; the part that fit is kept |
| `max_bytes_procesamiento` | state retained by the read in blocks and by the reread of the local copy | if the object exceeds the limit, the sample is discarded with `lupa_aviso_muestra_motor` and `tope_que_mando = "procesamiento"` is published |
| `bloque_filas` | turns on the pass through the whole table in blocks of that size | — |

`bloque_filas = n` (*row block*) computes in R the summary of the whole
table, without retaining all the rows. It reads the table in blocks of
`n` rows from a single result set (`dbSendQuery()` and `dbFetch(n)`).
The coverage is read in `meta$bloques$filas_vistas`. `perfil_muestra` is
still the diagnostic sample bounded by `muestra` and the caps. The
drivers without proven incremental retention —DuckDB among them— stay
`no_disponible` with their reason and a warning
`lupa_aviso_muestra_motor`.

``` r

por_bloques <- perfilar_dbi(con, "tramites", bloque_filas = 500, muestra = 100)
por_bloques$resumen_tabla$meta$bloques$filas_vistas
#> [1] 2000
n_filas(por_bloques$perfil_muestra)
#> [1] 100
```

## Caps on the approximate comparison

The caps of
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
(*detect approximate duplicates*) and of the vocabulary diagnostic cut
work in R. They do not cut the queries that an engine already ran. This
example recreates the table of suppliers of [Escala y
duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html#un-registro-de-proveedores)
(*Scale and approximate duplicates*, in Spanish) so that each output is
independent.

### The length of the values

A normalized distance loses its power to tell values apart when the
texts are long. The measure of the detector is Jaro-Winkler with
`p = 0.1`. It shows with 2 texts that differ in exactly 100 characters,
at different lengths.

``` r

distancia_cien_cambios <- function(largo, semilla) {
  set.seed(semilla)
  texto <- sample(letters, largo, replace = TRUE)
  otro <- texto
  cambios <- sample(largo, 100)
  otro[cambios] <- ifelse(otro[cambios] == "a", "b", "a")
  stringdist::stringdist(
    paste(texto, collapse = ""), paste(otro, collapse = ""),
    method = "jw", p = 0.1
  )
}
largos <- c(500, 1000, 2000, 4000, 8000)
data.frame(
  largo = largos,
  distancia = vapply(largos, function(largo) {
    round(median(vapply(1:5, distancia_cien_cambios, numeric(1), largo = largo)), 3)
  }, numeric(1))
)
#>   largo distancia
#> 1   500     0.162
#> 2  1000     0.142
#> 3  2000     0.074
#> 4  4000     0.047
#> 5  8000     0.027
```

The 100 differences are the same: what changes is how much they weigh in
the text. Past a certain length, the distance drops below the threshold
of 0.10. Then 2 texts that differ in 100 characters count as almost
equal.

That is why the detector does not compare values longer than
`max_largo_valor` (*maximum value length*; 10,000 characters by default,
an assumption of the code). The cap does not remove the effect, it
bounds it: a column of long texts can ask for a smaller one. It is
measured on the string that is really compared, with the columns already
joined and normalized. If it is exceeded, the whole combination stays
out of scope and is declared. With a cap of 20 characters, the string of
supplier and line of business does not fit:

``` r

corto <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro"), max_largo_valor = 20
)
corto$alcance[, c(
  "max_largo_valor", "largo_maximo", "columnas_excluidas_largo", "n_pares_hallados"
)]
#>   max_largo_valor largo_maximo columnas_excluidas_largo n_pares_hallados
#> 1              20           31         proveedor, rubro               NA
```

`largo_maximo` is the length of the compared string, not the one of each
column. `n_pares_hallados` is `NA`, not zero: nothing was compared. With
`max_largo_valor = Inf` there is no cap and no length is measured: that
field is `NA`, not zero.

The vocabulary diagnostic of
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
has its own cap, `max_largo_valor_vocabulario`, which is not inherited.
The one of the detector is passed with
`perfilar(duplicados_aproximados = list(max_largo_valor = ...))`. In
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
the same cap also declares the coverage of the costly text operations
that are not run on those values.

### The vocabulary work in `perfilar()`

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
looks for nearly identical variants within each text column
(`casi_duplicados_vocabulario`, *near-duplicate vocabulary*), comparing
their distinct values in pairs. Comparing 2 values of lengths `L1` and
`L2` costs on the order of `L1 × L2`. That is why the budget,
`max_trabajo_vocabulario`, is measured in character comparisons: `2e10`
by default. When it falls short, it compares the first forms in
alphabetical order —not a sample— and declares the rest. With a minimal
budget it shows:

``` r

recorte <- cobertura(ajustado)
recorte <- recorte[recorte$diagnostico == "proximidad_vocabulario", ]
writeLines(strwrap(paste0(recorte$columna, ": ", recorte$motivo), 88))
#> proveedor: El vocabulario excede el alcance de comparación: se evaluaron 7 de 7 valores
#> distintos, que son 5 formas normalizadas, y se comparó entre sí a las 4 primeras en
#> orden alfabético. Eso son 6 de 10 pares posibles y 1.531 de 2.555 comparaciones de
#> caracter. Quedaron 1 forma y 4 pares sin comparar por max_trabajo. 1 grupo de formas
#> cercanas no se formó porque sus frecuencias fueron parecidas: el criterio de variante
#> rara no pudo distinguir una forma correcta de otra. Se identificaron 1 par cercano con
#> asimetría <= 2.0.
```

The alphabetical order makes the result independent of the order of the
rows of the file. The search for dependencies has its own budget:
`max_trabajo_dependencias`, of `1e8` row-pair comparisons by default.
`Inf` turns off either of the 2.

### How many pairs are returned

`max_resultados` (100 by default) does not decide what is compared, but
how many pairs are returned: it keeps the closest ones. With a cut of 2,
on the 3 pairs at distance 0:

``` r

dos <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro"), max_resultados = 2
)
dos$pares[, c("fila_1", "fila_2", "distancia", "tipo_par")]
#>   fila_1 fila_2 distancia           tipo_par
#> 1      3      4         0             exacto
#> 2      1      2         0 exacto_normalizado
dos$alcance[, c("truncado", "distancia_corte", "corte_en_empate")]
#>   truncado distancia_corte corte_en_empate
#> 1     TRUE               0            TRUE
```

`corte_en_empate = TRUE` (*cut in a tie*) warns that at least one pair
**as close** as the kept ones was left out. It is not `truncado`
(*truncated*) under another name: if the cut falls on a unique distance,
it is `FALSE` even if something was cut. To bring all the tied ones,
`max_resultados` has to go up.

Among tied pairs, the tie-break uses the content of the values and not
the position of the rows. Reordering the table does not change which
pairs stay:

``` r

ids_conservados <- function(datos, resultado) {
  a <- datos$id[resultado$pares$fila_1]
  b <- datos$id[resultado$pares$fila_2]
  sort(paste(pmin(a, b), pmax(a, b), sep = "-"))
}
invertidos <- proveedores[8:1, ]
dos_invertidos <- detectar_duplicados_aproximados(
  invertidos, columnas = c("proveedor", "rubro"), max_resultados = 2
)
ids_conservados(proveedores, dos)
#> [1] "1-2" "3-4"
ids_conservados(invertidos, dos_invertidos)
#> [1] "1-2" "3-4"
```

## ⚠️ What the SQL aggregates do not translate

`sentinelas_numericos`, `aplicabilidad` and `columnas_opcionales`
(*numeric sentinels*, *applicability*, *optional columns*) change what
`perfil_muestra` summarizes. The SQL aggregates are computed without
them: `AVG()` knows nothing of sentinels. When any of them is declared,
`resumen_tabla$cobertura` receives a `degradado` (*degraded*) row that
says so. With 30 ages `-999` declared as a sentinel, the 2 blocks
publish 2 different means on the same rows:

``` r

centinelas <- tramites
centinelas$edad[1:30] <- -999L
DBI::dbWriteTable(con, "centinelas", centinelas)
declarado <- perfilar_dbi(con, "centinelas", sentinelas_numericos = -999)

media_edad <- function(cols) cols$media[cols$columna == "edad"]
c(
  resumen_sql = media_edad(columnas(declarado)),
  perfil_muestra = media_edad(columnas(declarado$perfil_muestra))
)
#>    resumen_sql perfil_muestra 
#>       34.63182       50.53385
cobertura(declarado)[, c("bloque", "elemento", "estado")]
#>          bloque             elemento    estado
#> 1 resumen_tabla sentinelas_numericos degradado
```

On a `perfil_dbi`,
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
returns those of the SQL summary. Those of the sample profile are
requested on `perfil_muestra`.

When the 2 blocks exist,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
crosses them. It compares only what both measure with the same
semantics. What a declaration changes on purpose stays in
`meta$corroboracion_bloques$no_comparadas`, with its cause:

``` r

head(declarado$resumen_tabla$meta$corroboracion_bloques$no_comparadas, 4)
#>   columna metrica                causa
#> 1    edad   media sentinelas_numericos
#> 2    edad mediana sentinelas_numericos
#> 3    edad  minimo sentinelas_numericos
#> 4    edad  maximo sentinelas_numericos
```

If 2 comparable values differ beyond the tolerance, the coverage
receives a `divergencia` (*divergence*) row with the 2 values. No winner
is chosen. If the sample covers only part of the table, an ordinary
difference is to be expected: it is declared `no_corroborada` (*not
corroborated*), not `divergencia`.

The tolerance is in `meta$corroboracion_bloques$tolerancia`:

- for the values read, `1e-10` absolute plus 64 machine epsilons
  relative;
- for the mean and the standard deviation, which each side accumulates
  in its own way, `1e-10` relative.

`columnas_codigo` (*code columns*), which arrives through `...`, does
apply to the summary. The mean, the median and the standard deviation of
a declared column stay `NA`, and the coverage marks them `no_aplica`
(*does not apply*).

## Metrics and strategies

`metricas` (*metrics*) chooses groups; each group publishes these fields
of `resumen_tabla$columnas`:

| grupo         | campos                                                |
|:--------------|:------------------------------------------------------|
| `"validos"`   | `n_validos`, `n_faltantes`, `prop_faltantes`          |
| `"distintos"` | `n_distintos`, `tasa_distintos`                       |
| `"moda"`      | `moda`, `frecuencia_moda`                             |
| `"basicos"`   | `minimo`, `maximo`, `media`, `n_ceros`, `n_negativos` |
| `"mediana"`   | `mediana`                                             |
| `"desvio"`    | `desvio`                                              |

`metricas = c("validos", "basicos", "desvio")` avoids the cardinality,
the mode and the median, which are the queries that group or sort.
`metricas = "validos"` leaves only completeness.

Each row of `resumen_tabla$sql` carries `estado`, `alcance`, `universo`,
`tamano_muestra`, `fraccion`, `metodo` and `error_esperado`. `estado`
tells apart what was computed, what was estimated and what is not
available, besides what was omitted for cost or for protection. In an
engine sample, `error_esperado` (*expected error*) is:

- `no_estimado` (*not estimated*) when the error could be computed under
  a probabilistic design, but was not computed;
- `no_estimable` (*not estimable*) for the mode, the median and the
  observed cardinality;
- `no_aplica` when there was no effective sampling.

No numeric bound is published without a formula that justifies it.

**`estrategia_distintos`** (*distinct-count strategy*) chooses where
`n_distintos` comes from:

- `"exacta"` (*exact*, the default): `COUNT(DISTINCT)` on the rows of
  the run.
- `"aproximada_motor"` (*engine approximation*): a native function, only
  if the probe accepts it. Its undocumented error bounds stay as
  `"desconocido"` (*unknown*). If the function only knows how to build a
  full query, valid and distinct values are emitted separately. Each
  record keeps the method of the query that ran.
- `"catalogo"` (*catalog*): `pg_stats.n_distinct` of PostgreSQL,
  published as `estimado_catalogo` (*catalog estimate*), only with
  `universo = "tabla_completa"`.
- `"omitida"` (*omitted*): it does not emit the query.

There is no fallback between strategies: a strategy without the
capability stays `no_disponible` and is never turned into an exact
count. In SQLite, the catalog does not exist:

``` r

por_catalogo <- perfilar_dbi(
  con, "tramites", estrategia_distintos = "catalogo",
  bloque_muestra = "solo_agregados"
)
estrategia <- por_catalogo$resumen_tabla$meta$estrategia_distintos
estrategia$estado
#> [1] "no_disponible"
writeLines(strwrap(estrategia$motivo, 88))
#> `catalogo` requiere PostgreSQL y su `pg_stats`: no hay una estadística portable de
#> cardinalidad para este motor.
```

What was requested, what was resolved and the status stay in
`meta$estrategia_distintos` and in the columns `estrategia_solicitada`
and `estrategia_resuelta` of `resumen_tabla$sql`.

**`estrategia_mediana`** (*median strategy*) enables an approximation,
but does not impose it. The probe tries in this order:

1.  a consolidated exact form;
2.  an exact one per column;
3.  the approximate ones, at the end.

`resumen_tabla$sql$metodo` says what ran. Only an approximation that was
run stays `estimado` (*estimated*). `universo = "muestra_motor"` with
`estrategia_mediana = "aproximada_motor"` is rejected on entry.

**`incluir_valores = FALSE`** (*include values*) does not emit the
queries of mode, extremes and median. It also clears the cell values of
the summary, the sample, the blocks, the coverage and the reasons.
Counts and proportions are still published.

## Batched queries

The cost of a large table is the number of passes, not the sampling.
That is why the queries are grouped:

- **Flat aggregates**: `COUNT(col)`, minimum, maximum, mean, zeros,
  negatives and standard deviation. They travel in a single query per
  batch of up to `tamano_lote_planos` (*flat batch size*) columns (20 by
  default). In SQL Server and for `float8` of PostgreSQL, the standard
  deviation adds a second query per batch. That way it keeps the
  centered computation. Each query brings
  `COUNT(*) AS n_total_consulta`: completeness uses that local
  denominator and not the total of another batch.
- **Exact cardinalities** go in their own class, in batches of
  `tamano_lote_distintos` (*distinct batch size*; 2 by default). Each
  `COUNT(DISTINCT col)` travels with `COUNT(col) AS n_validos_guard`, a
  control count of the same statement.
- **The mode** is one query per column, because it groups. It tries to
  bring `SUM(COUNT(*)) OVER () AS n_validos_guard` along with its
  frequency. The form is probed. If the engine rejects it, the query
  without that count is used and `resumen_tabla$meta$moda_guardian`
  declares it.
- **The median** is one query per column as a fallback. If the probe
  accepts `PERCENTILE_CONT(...) WITHIN GROUP` and the percentile returns
  its full digits, several travel in a single `SELECT` per batch. The
  test asks for a row with a value above 1e8 and another below 1e-10,
  which have to come back identical. In SQL Server that requires
  compatibility level 110 or higher. MariaDB accepts the form, but
  returns the percentile with 10 decimals and a cap at 1e8: it uses the
  median per column, like MySQL. The reason for a rejection stays in
  `meta$mediana_consolidada$motivo`.
- In the dialects with `LIMIT` that do not have `PERCENTILE_CONT`, like
  SQLite and PostgreSQL 9.3, the count of the median goes in a scalar
  subquery of the same statement. For that, the probe has to accept
  integer division with `%` and `/`.

`tamano_lote` sets both sizes at once. The number of queries that each
size produces on the table of the example, according to the plan:

``` r

consultas_por_lote <- function(...) {
  plan <- plan_perfilado_dbi(con, "tramites", ...)
  tabla <- as.data.frame(plan)
  c(
    planos = sum(tabla$n_consultas[grepl("planos", tabla$clase_consulta)]),
    distintos = sum(tabla$n_consultas[grepl("distintos", tabla$clase_consulta)]),
    total = attr(plan, "total")
  )
}
rbind(
  planos_1 = consultas_por_lote(tamano_lote_planos = 1),
  por_omision = consultas_por_lote(),
  distintos_1 = consultas_por_lote(tamano_lote_distintos = 1),
  distintos_4 = consultas_por_lote(tamano_lote_distintos = 4)
)
#>             planos distintos total
#> planos_1         6         2    24
#> por_omision      3         2    21
#> distintos_1      3         4    23
#> distintos_4      3         1    20
```

The value of `tamano_lote_distintos` is an assumption of the code, noted
next to the constant (`R/perfilar-dbi.R`). In PostgreSQL, the read of
the table is the same with any batch. The engine amortizes a pass among
the aggregates of the same `SELECT`. What grows with the batch is the
spill to disk of PostgreSQL. The optimum depends on `work_mem`, which
changes by server. To choose another value, `instrumentar = TRUE`
publishes the duration of each query.

If a batch fails, the batch is not lost. Its halves are probed by
bisection:

- the accepted groups are reused as measurements;
- the culprit columns are retried by metric, with their local
  denominator;
- what still fails stays `no_disponible` with its reason.

If the budget runs out before isolating everything, the pending columns
stay unmeasured. They are not assumed to be culprits or readable.
`meta$tamano_lote_funciono` keeps the largest batch accepted in that
run. No state tied to the connection is kept.

`resumen_tabla$sql` adds `lote` and `columnas_compartidas` to see which
query was shared. The order of degradation is:

1.  flat aggregates;
2.  total of the universe, when needed;
3.  distinct values;
4.  mode;
5.  median.

## Cost policy

The mode and the median are the expensive metrics. `politica_costo`
(*cost policy*) accepts 2 values, and no other:

``` r

tryCatch(
  perfilar_dbi(con, "tramites", politica_costo = "ninguna"),
  error = function(e) writeLines(strwrap(conditionMessage(e), 88))
)
#> `politica_costo` debe ser un único valor: `todas` o `por_cardinalidad`.
```

- `"todas"` (*all*, the default) keeps mode and median in all the
  requested columns. The package does not choose for the user.
- `"por_cardinalidad"` (*by cardinality*) first resolves the structural
  sources: a guaranteed primary key says how many distinct values there
  are without counting them. It measures `validos` and `distintos` only
  if needed and if the strategy allows it. Then it omits, per column,
  **only the mode**, when
  `n_distintos / n_validos >= umbral_cardinalidad`. The threshold is
  `0.5` by default and can be moved in each call. The median is not
  omitted by cardinality: its cost is governed by the rows.

``` r

barato <- perfilar_dbi(con, "tramites", politica_costo = "por_cardinalidad")
s <- sql_perfil(barato)
s[s$estado == "omitido_por_costo", c("columna", "metrica", "estado")]
#>       columna         metrica            estado
#> 10 id_tramite            moda omitido_por_costo
#> 11 id_tramite frecuencia_moda omitido_por_costo
#> 38      monto            moda omitido_por_costo
#> 39      monto frecuencia_moda omitido_por_costo
writeLines(strwrap(barato$resumen_tabla$meta$decisiones_costo$monto$detalle$motivo, 88))
#> Se omitió la moda de la columna `monto`: tiene 2000 valores distintos de 2000 válidos
#> (1.000) sobre la tabla completa; la política optativa considera que agrupar u ordenar
#> toda la columna no justifica el costo. Para pedirla igual, usar `politica_costo =
#> "todas"`; para mover el criterio, cambiar `umbral_cardinalidad`.
```

On a wider table, the difference shows in the number of queries. The
table that follows has 200 rows and 158 columns:

- numeric: 60;
- text: 60;
- integer: 20;
- dates stored as text: 18.

It is the same one that `benchmark/medir_plan_ancho.R` and
`benchmark/medir_politica_costo.R` use:

``` r

set.seed(1)
filas <- 200
generar <- function(prefijo, k, valores) {
  stats::setNames(lapply(seq_len(k), function(i) valores()), paste0(prefijo, seq_len(k)))
}
ancha <- as.data.frame(c(
  generar("num", 60, function() round(stats::rnorm(filas) * 100, 2)),
  generar("txt", 60, function() sample(letters, filas, replace = TRUE)),
  generar("ent", 20, function() sample(seq_len(50L), filas, replace = TRUE)),
  generar("fec", 18, function() {
    as.character(as.Date("2026-01-01") - sample(seq_len(900L), filas, replace = TRUE))
  })
))
DBI::dbWriteTable(con, "ancha", ancha)

corrida <- function(politica) {
  perfilar_dbi(
    con, "ancha", bloque_muestra = "solo_agregados", instrumentar = FALSE,
    politica_costo = politica, proteger_datos_personales = FALSE
  )
}
emitidas_ancha <- function(x) x$resumen_tabla$meta$consultas$emitidas
todas <- corrida("todas")
por_cardinalidad <- corrida("por_cardinalidad")
por_politica <- c(
  todas = emitidas_ancha(todas),
  por_cardinalidad = emitidas_ancha(por_cardinalidad)
)
por_politica
#>            todas por_cardinalidad 
#>              334              256
```

With `"por_cardinalidad"`, the run saved 78 of 334 queries, all of them
of mode.

The structural sources are resolved whenever the policy needs the
cardinality, even if `estrategia_distintos` is omitted or not available.
The strategy decides whether it can be measured, but does not hide a
guaranteed primary key. If there is no structural source and the
strategy does not allow measuring, the cardinality stays unknown. Then
the policy follows its rule for that case.

Each omission stays in `resumen_tabla$sql` as `omitido_por_costo`
(*omitted for cost*). It says what was omitted, why and how to request
it again (`politica_costo = "todas"` or another threshold).
`meta$decisiones_costo` explains separately the decision on each metric
of each column.

`fuente_cardinalidad_costo` says where the proportion came from, and is
independent of `estrategia_distintos`. The policy never turns a request
`"aproximada_motor"` or `"catalogo"` into an exact `COUNT(DISTINCT)`.

## ⏱ The plan before running

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
emits only preparation queries:

- the schema;
- the capability probes;
- the catalog of the primary key;
- in PostgreSQL, its statistics.

It says how many queries the profiling would emit, of which class and
with which scope. It does not scan data to decide the cost.

``` r

plan <- plan_perfilado_dbi(con, "tramites")
knitr::kable(as.data.frame(plan))
```

| clase_consulta | n_consultas | alcance | n_consultas_max |
|:---|---:|:---|---:|
| portones (esquema y sondas) | 7 | una vez | 7 |
| agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio) | 3 | escanea la tabla completa | 3 |
| distintos por lotes (clase separada) | 2 | escanea la tabla completa | 2 |
| moda (GROUP BY + orden + limite) | 4 | ordena o agrupa la tabla completa | 4 |
| mediana (orden total + limite/salto) | 3 | ordena la tabla completa | 3 |
| muestra | 2 | lee las filas pedidas | 2 |

The cost is a **range**:

- `attr(plan, "total")` (21 here) counts the queries if no batch is
  rejected. With `politica_costo = "por_cardinalidad"`, it assumes that
  the mode whose cardinality is not known is omitted.
- `attr(plan, "total_maximo")` (34) adds the path that runs that mode
  and the bisection probes, if the engine rejected every batch. They are
  up to `2n − 1` per batch of `n` columns. `total_lotes_rechazados`
  publishes the same number.

The default run of the example emitted 21.

The range holds when the sampling can be built. If
`universo = "muestra_motor"` and the form cannot be built,
`attr(plan, "muestreo")` declares it and the plan excludes those
metrics. In status `"no_sondeado"` (*not probed*), the form was only
built locally. The range stays conditional on the probe of the run
accepting it.

The count follows the composition of the table, not the number of
columns. With the wide table of the previous section and one of 158
columns, all of them text:

``` r

plan_ancho <- plan_perfilado_dbi(con, "ancha")
recorre <- !as.character(plan_ancho$alcance) %in% c("una vez", "lee las filas pedidas")
texto <- as.data.frame(generar("txt", 158, function() sample(letters, filas, replace = TRUE)))
DBI::dbWriteTable(con, "ancha_texto", texto)
c(
  ancha = attr(plan_ancho, "total"),
  ancha_recorren_la_tabla = sum(plan_ancho$n_consultas[recorre]),
  solo_texto = attr(plan_perfilado_dbi(con, "ancha_texto"), "total")
)
#>                   ancha ancha_recorren_la_tabla              solo_texto 
#>                     336                     327                     253
```

Of the 336 queries of the wide table, 327 scan, sort or group the whole
table. Text columns cost less, above all because the median —which sorts
the table— is requested only on numeric columns. `muestra` bounds none
of that: it bounds what is brought into R, not the work of the engine.
`benchmark/medir_plan_ancho.R` redoes these figures and leaves a dated
output with the environment.

Each capability probe spends a fixed number of queries even if it
succeeds with the first form. It is a design decision. If the cost
varied by engine, the plan could not say exactly how many queries it is
going to emit.

Counting queries does not say how much they cost: 14 queries on 2
million rows are more work than 200 on 1,000. The plan also estimates
the **magnitude**, in 2 halves:

- the one of the engine: `filas_leidas` and `ordenaciones_completas`,
  summarized in `magnitud_motor`;
- the one of the client: `columnas_texto` and `pares_texto`, summarized
  in `magnitud_texto`. `pares_texto` are the pairs of forms that the
  vocabulary detector could compare in R on the sample.

`magnitud` (*magnitude*) is the larger of the 2: `"baja"`, `"media"`,
`"alta"` (*low*, *medium*, *high*), or `"desconocida"` (*unknown*) if
the number of rows is not known. The plan does not count rows. Only in
PostgreSQL, a positive `pg_class.reltuples` provides a catalog estimate,
labeled as such. In SQLite it stays unknown:

``` r

attr(plan, "magnitud")
#> [1] "desconocida"
num(attr(plan, "pares_texto"))
#> [1] "2.000.000"
```

The magnitude is an estimate:

- the one of the engine counts the rows that would have to be read if no
  index helped;
- the one of the client counts pairs. Its cost depends on the length of
  the values, which the plan does not know without reading them.

Printing the plan shows the 2 halves, the high-work warning and the
levers to bound it. That text cites reference speeds of other runs to
turn counts into seconds. They are assumptions of the code, not
measurements of the table.

The plan does not project seconds for `COUNT(DISTINCT)`: without reading
data it has no measured reference. With `universo = "muestra_motor"`,
the plan also declares the local copy:

- `attr(plan, "materializacion")`: the single selection that will be
  paid, with `pagado = FALSE`;
- `attr(plan, "pasadas")`: the passes that will read that copy.

## ⚠️ Cost and spill warnings

The warnings of
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
also arrive in non-interactive scripts, because the cost is paid on the
server. Each one has its switch and its threshold. Turning it off does
not turn off the measurement or its metadata.

| warning | how it projects | when it warns | where it stays |
|----|----|----|----|
| `COUNT(DISTINCT)` | median of the durations of the first batch of distinct values of this run, times the number of batches | after the first batch and before the second, if there is more than one and the projection reaches `umbral_segundos_aviso_distintos` | `meta$costo_distintos` |
| mode | ms per distinct value of the first mode measured, times the cardinality of the others | before the next mode | `meta$costo_moda` |
| median | ms per row of the first median measured | before the pending medians | `meta$costo_mediana` |
| estimated spill (PostgreSQL) | size of the hash with `pg_stats` and `pg_class.reltuples` against `work_mem` | before the first `COUNT(DISTINCT)` | `meta$estimacion_derrame` |

The 3 time thresholds are 30 seconds by default, and `Inf` turns off
each warning explicitly. With a single batch of distinct values there is
nothing to project. The projection of distinct values does not use
`reltuples`. If the mode has no cardinality, the projection declares it
instead of assuming it.

``` r

costo <- por_omision$resumen_tabla$meta$costo_distintos
str(costo[c("disponible", "n_lotes")])
#> List of 2
#>  $ disponible: logi TRUE
#>  $ n_lotes   : int 2
writeLines(strwrap(costo$fuente, 88))
#> mediana de 1 consulta del primer lote de distintos medido en esta corrida
```

When a total median does not leave a local measurement, the projection
uses a reference of 68 ms per million rows. It is a constant of the code
taken from another run, with no script that redoes it. It is not
presented as a measurement of the current run. If the query that counted
the rows was measured and gives a larger bound, that bound is published
separately, as an observed reading.

In PostgreSQL, the preparation also reads:

- `pg_stats.n_distinct` and `pg_stats.avg_width`;
- `pg_class.reltuples`;
- `work_mem` and, since PostgreSQL 13, `hash_mem_multiplier`.

If the estimated hash exceeds the effective limit, it warns that raising
`work_mem` in the session can avoid the spill. The package does not
change the configuration or wait for confirmation.

The estimate of the mode uses `EXPLAIN (FORMAT JSON, COSTS OFF)` without
`ANALYZE`. The one of the median models a sort with minimums of 32 bytes
per value for fixed types and 42 for `numeric`.

With `instrumentar = TRUE` and `pg_stat_statements` available, the
measured spill —temporary blocks written and read— prevails over the
estimate. If the call cannot be attributed, the status stays not
available: the elapsed time is not presented as evidence of spill.

The block `perfil_muestra` goes through
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
and in an interactive session it also receives its wide-table warning.

### Wide tables

Before starting the expensive stages,
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
projects how long a table can take from its number of cells. From
100,000 cells it warns of the estimated duration, only in an interactive
session. The projection is always kept:

``` r

str(ajustado$meta$costo_tabla_ancha[c(
  "celdas", "duracion_estimada_segundos", "umbral_celdas"
)])
#> List of 3
#>  $ celdas                    : num 32
#>  $ duracion_estimada_segundos: num 0.0064
#>  $ umbral_celdas             : int 100000
```

The duration comes from dividing the cells by a reference speed of 5,000
cells per second. It stays below the slowest measurement of
`benchmark/medir_referencias.R`: 8,044 cells per second, with 500 rows
and numeric and categorical columns.

It is a reference for the common case, not a bound.
`benchmark/medir_topes_valores_ancho.R` measures equal columns of text
codes (`"valor_1"`, `"valor_2"`…), with the near-duplicate search of the
vocabulary turned off and a `columnas_duplicadas` finding for each pair
of columns. Its slowest case is 9,619 cells per second, also above the
reference.

The 2 CSV files, in `benchmark/datos/`, keep the date, the environment
and the fingerprint of the code. The warning is turned off with
`avisar_costo_tabla_ancha = FALSE`, and
`umbral_celdas_aviso_tabla_ancha = Inf` is the same decision, explicit.

The dated measurements of wide tables are in [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html#tablas-anchas)
(*Evidence and performance*, in Spanish). They are the query plan of an
administrative table of 158 columns and the 2 tables of speed.

## Instrumentation and durations

With `instrumentar = TRUE` (the default in the run), each row of
`resumen_tabla$sql` adds these fields:

- `duracion_ms`, which uses
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html);
- `cpu_ms`, which adds `proc.time()[c("user.self", "sys.self")]` and
  measures the CPU work of the client, including the conversion of the
  result that the driver does;
- `n_filas_resultado` and `bytes_resultado_r`;
- `consulta_id`, `etapa` and `nivel`.

A `0` is measured; an `NA`, not measured. With `instrumentar = FALSE`
the same queries are emitted in the same order, and those fields stay
`NA`.

`duracion_ms` is the duration of the **query**, repeated in each row
that query produced. To add up, only the rows of `nivel = 1` (*level*)
count, the first of each `consulta_id`:

``` r

s <- sql_perfil(por_omision)
table(s$nivel, useNA = "ifany")
#> 
#>  1  2 
#> 17 43
length(unique(stats::na.omit(s$consulta_id)))
#> [1] 10
```

``` r

sum(s$duracion_ms[s$nivel == 1], na.rm = TRUE)
```

There are 2 identifiers that answer different questions:

- `consulta_id` identifies the statement that produced each measurement
  within the run, and defines the consistency group.
- `id_consulta` identifies the data query: 2 metrics with the same value
  saw exactly the same rows. It does not depend on `instrumentar`, and
  it is `NA` in the mode and the median, which do not share rows with
  other metrics.

`memoria_trabajo` (*working memory*) classifies the working state of the
engine as `creciente` (*growing*), `acotado` (*bounded*) or `NA`.

`resumen_tabla$tiempos` gathers the large stages of the client in the
same unit. Those of `nivel = 1` are disjoint; those of a higher level
are contained in one of them:

``` r

por_omision$resumen_tabla$tiempos[, c("etapa", "nivel", "estado")]
#>                            etapa nivel        estado
#> 1           ausencia_estructural     2        medido
#> 2    casi_duplicados_vocabulario     2        medido
#> 3 clasificacion_datos_personales     2        medido
#> 4                   dependencias     2        medido
#> 5         duplicados_aproximados     2 no_solicitado
#> 6                lectura_muestra     1        medido
#> 7             perfilado_columnas     2        medido
#> 8              perfilado_muestra     1        medido
#> 9    proteccion_datos_personales     2 no_solicitado
```

In an interactive session, a bar advances against the queries that the
plan expects to emit. `options(lupa.progreso = FALSE)` turns it off and
`TRUE` forces it. It does not change any measured value.

## Processing memory

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
does not estimate the processing memory in R, and declares it:

``` r

memoria <- attr(plan, "memoria_procesamiento")
memoria$estado
#> [1] "no_estimada"
writeLines(strwrap(memoria$motivo, 88))
#> no escala de forma predecible con las filas ni con las celdas; se midió.
```

The attribute keeps 2 things:

- the known magnitude of the work (rows, cells and text pairs), labeled
  as magnitude and not as consumption;
- references of other runs, which are fixed texts of the code.

``` r

names(memoria)
#> [1] "estado"            "estimada"          "motivo"            "magnitud"         
#> [5] "referencias"       "distincion"        "reparto_observado"
names(memoria$referencias)
#> [1] "traer"     "procesar"  "variacion"
```

Those references say how much it cost to bring the rows, how much to
process them in R and how much that varied between tables. They come
from a single run, dated 2026-08-28, against a remote production engine,
and this repository cannot redo it.

They are not a prediction for the table of the plan. They are not used
to estimate because they vary between tables of the same magnitude.
`print(plan)` shows them, and the figures with their source are in
[Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html)
(*Evidence and performance*, in Spanish).

Seeing every row and holding them all in memory is not the same. There
are 3 recipes, depending on what is needed:

| need | configuration |
|----|----|
| bounded aggregates, without rows in R | `universo = "muestra_motor"`, `muestra_motor = n`, `bloque_muestra = "solo_agregados"` |
| row-level findings | `muestra = n`, with `orden_muestra` if the read has to be reproducible, and the caps on cells and bytes |
| summary of the whole table without retaining it | `bloque_filas = n` |

## A run on production databases

Between 26 and 30 October 2026, an external team ran
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
on 8 production databases:

- PostgreSQL: 5 databases, from 9.3 to 13;
- SQL Server: 3 databases, 2008 R2 and 2016.

The machine had 121 GB of memory and R 4.5.2. They are figures of a
single run, with the version of those dates and on data that the
repository does not have. They are not redone from here and are not
extrapolated to another machine.

|  | Small tables, single path | Aggregates (`tabla_completa`) | Engine sample |
|----|---:|---:|---:|
| Median | 0.6 s | 12 s | 26 s |
| 99th percentile | 47 s | 1,822 s | 323 s |

There were 3,709 calls on 3,316 tables, and 3,693 finished fine. Another
3 finished with a warning. Of the remaining 13, 11 were cut by the
maximum time that the team set and 2, by an error of the process. The
median memory of a call was between 230 and 530 MB depending on the
path. On those profiles,
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
took a median of 0.06 s and
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md),
0.008 s.

The protection of personal data was measured on 3 tables of up to
100,000 rows, with the same call with and without
`proteger_datos_personales = TRUE`. It cost between 11% and 18% more
time, with a memory between 5% lower and 11% higher. Each of those calls
drew its own engine sample, so the rows were not exactly the same. The
engine and the driver also take their share. The protection is a cost in
the low double digits, not a null cost.

The slowest parts of that run are solved in the code that is published:

- the standard deviation in SQL Server, which went in one query per
  column, each one on the whole table ([Batched queries](#batches));
- the date columns in R before 4.6;
- the very long texts;
- the engine sample, which could bring into R more than its budget
  ([Caps on the read into R](#caps)).

The agreement of the figures with an SQL count written separately is in
[Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html#contraste-contra-el-servidor)
(*Evidence and performance*, in Spanish).

## Consistency across queries

The aggregates are separate statements, and the table can change between
them. The summary publishes it:

``` r

por_omision$resumen_tabla$meta$snapshot
#> [1] FALSE
writeLines(strwrap(por_omision$resumen_tabla$meta$nota_snapshot, 88))
#> No hubo lectura instantánea: cada agregado se midió en su propio momento y la tabla
#> pudo cambiar entre consultas. La cobertura declara las comparaciones incoherentes entre
#> grupos de consistencia distintos.
```

The hard bounds are checked only within one statement:

- the frequency of the mode, against its control count;
- the distinct values, against the `n_validos_guard` of their own query.

If exact `n_validos` and `n_distintos` are inconsistent and come from
different statements, the coverage adds an `alcance_distinto`
(*different scope*) row. Its reason names the 2 statements. It is
evidence that the table changed during the run, not an accusation
against the engine or the package. If an approximation cannot bring the
control count, the check is declared as not available.

With `universo = "muestra_motor"` and `bloque_muestra = "con_muestra"`,
the selection of the engine runs **only once**. It is kept in a local
copy: a temporary file of the R session (the *spool*). The whole profile
reads that copy and never samples the engine again.

On each reread, a closing record checks `muestra_id`, `snapshot_id`,
`orden_id`, `n_filas`, `bytes` and the checksum. A missing record
publishes `spool_incompleto:trailer_ausente`, and a checksum that does
not match, `spool_checksum_invalido`. The copy does not write to the
connection or create temporary objects in the engine:

``` r

copia <- del_motor$resumen_tabla$meta$materializacion
str(copia[c("backend", "estado", "externo", "objetos_temporales_motor",
            "validado_relectura")])
#> List of 5
#>  $ backend                 : chr "spool_sesion_cliente"
#>  $ estado                  : chr "cerrada_verificada"
#>  $ externo                 : logi TRUE
#>  $ objetos_temporales_motor: logi FALSE
#>  $ validado_relectura      : logi TRUE
```

Rereading the copy can cost more than asking the engine for the
selection again on each pass. It is chosen because it guarantees that
every metric describes the same rows. `snapshot_id` identifies the copy
of the client, not a transactional snapshot of the engine.

For a consistent read, the path can run inside a transaction opened by
the user. That user sets the isolation level the engine needs. The
package never sends `BEGIN`, `COMMIT` or `ROLLBACK`. Outside a
transaction, it does not add statements of that kind. The same holds for
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
(*profile a collection*) and
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
(*collection relationships*).

| engine | inside a transaction |
|----|----|
| PostgreSQL | an error aborts the transaction, so each statement goes between its own `SAVEPOINT` and its `RELEASE`, with `ROLLBACK TO SAVEPOINT` if it fails |
| SQL Server | it can roll back the transaction on an error; the path measures `@@TRANCOUNT` and `XACT_STATE()` before and after each send and read, and if they change it raises `lupa_error_transaccion_dbi` |
| MySQL, MariaDB, SQLite | the error of a query does not abort the transaction |
| DuckDB | an error while preparing aborts it and there is no `SAVEPOINT`: a view that fails leaves the transaction aborted, and it has to be closed with `ROLLBACK` |

## Dialects and probes

Bounding rows is not standard SQL. Before emitting the aggregates,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
probes the engine with a query that returns no rows. It chooses the
first capability that the engine accepts:

| dialecto | forma | motores |
|:---|:---|:---|
| `"limit"` | LIMIT n \[OFFSET k\] | PostgreSQL, SQLite, MySQL/MariaDB, DuckDB y compatibles |
| `"top"` | TOP (n), y OFFSET k ROWS FETCH NEXT n ROWS ONLY con orden | SQL Server 2012 o posterior, Sybase |
| `"fetch_first"` | OFFSET k ROWS FETCH FIRST n ROWS ONLY | Oracle 12c o posterior, DB2, Derby, H2 |
| `"rownum"` | ROWNUM \<= n; no expresa salto | Oracle anterior a 12c |
| `"portable"` | sin cláusula de límite: se acota con dbSendQuery() y dbFetch(n) | cualquier motor con DBI |

``` r

dialecto <- por_omision$resumen_tabla$meta$dialecto
str(dialecto[c("nombre", "declarado")])
#> List of 2
#>  $ nombre   : chr "limit"
#>  $ declarado: logi FALSE
writeLines(strwrap(dialecto$sondas, 88))
#> SELECT `id_tramite`, `categoria`, `monto`, `edad` FROM `tramites` WHERE 1 = 0 LIMIT 1
```

If no capability works, the portable path remains, which bounds in the
client with `dbSendQuery()` and `dbFetch(n)`. There the exact median is
declared not available, instead of bringing half a table into memory. If
the probe gets it wrong, `dialecto =` (*dialect*) declares the
capability without probing:

``` r

portable <- perfilar_dbi(con, "tramites", dialecto = "portable",
                         bloque_muestra = "solo_agregados")
s <- sql_perfil(portable)
writeLines(strwrap(s$motivo[s$columna == "monto" & s$metrica == "mediana"], 88))
#> NA
```

Each probe tries the same form that is emitted afterwards. A cheaper
form is not enough: an engine can accept a clause with a trivially false
filter and reject it without it. DuckDB does it with block sampling:

``` r

probar <- function(sql) {
  tryCatch({ DBI::dbGetQuery(duck, sql); "aceptada" },
           error = function(e) "rechazada")
}
c(
  con_filtro_falso = probar("SELECT 1 FROM t TABLESAMPLE SYSTEM (10) WHERE 1 = 0"),
  sin_filtro = probar("SELECT 1 FROM t TABLESAMPLE SYSTEM (10)")
)
#> con_filtro_falso       sin_filtro 
#>       "aceptada"       "aceptada"
```

Other rules of the dialect:

- The subquery alias is written with `AS` or without it, depending on
  the engine. The column aliases go between quotes and are compared
  without case sensitivity, because there are engines that fold them.
- The median under `muestra_motor` uses a single CTE with
  `COUNT(*) OVER ()` and `ROW_NUMBER() OVER` in 2 cases: in the dialects
  without a consolidated or scalar form (MySQL, MariaDB, SQL Server with
  compatibility below 110) and in each column that cannot use the
  consolidated one (a `float8` of PostgreSQL, a column with extremes).
  It chooses the 2 central positions without dividing and, with
  extremes, averages them without overflowing within the same statement.
  Its probe repeats that construction on
  `SELECT 1.0 UNION ALL ... SELECT 4.0`, and requires `2.5` and a
  negative control that fails. The production table is not probed. With
  `dialecto = "portable"`, which has no row skipping, the median of the
  whole table also comes from that CTE.
- With the driver recognized, the syntax probes of other engines are not
  run. In SQL Server with `XACT_ABORT ON`, a statement that does not
  belong would abort the transaction.

## What the engine stores differently from R

The SQL summary describes what the engine has in front of it, and
sometimes it is not the same as the table in R. The full detail is in
the section «Lo que el motor guarda distinto que R» of
[`?perfilar_dbi`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
The known cases, with what the package publishes:

| engine | what happens | what `lupa` publishes |
|----|----|----|
| SQLite | it has no date type: a `Date` is stored as days since 1970 | without more information it describes it as a number; with `extended_types = TRUE` when connecting, the column is temporal and receives no quantitative aggregates |
| SQLite | a column of numeric affinity admits text, and RSQLite reads it as `0` | it counts with [`typeof()`](https://rdrr.io/r/base/typeof.html) the values stored as text or blob and declares the divergence with that cause |
| DuckDB | `NaN` is a value: it counts as present and distinct, and `MAX` or `AVG` answer `NaN` | a metric answered with `NaN` stays `no_disponible` with its reason |
| SQLite, PostgreSQL, DuckDB | `Inf` and `-Inf` enter `MIN`, `MAX`, `AVG` and the median | the magnitude statistics are requested again on the finite values, as in [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) |
| PostgreSQL | RPostgres reads `0` in every `money` cell, and a `real` through its text | both are read converted in the engine; the conversion stays in `meta$lectura_convertida` |
| PostgreSQL 9.3 to 11, or `extra_float_digits <= 0` | the text of a `double precision` has fewer digits | the read decodes the 8 bytes of the value with `float8send()` |
| SQL Server | `datetimeoffset` can arrive converted to UTC by FreeTDS | the lost offset is not rebuilt; the corroboration leaves it as a divergence |
| SQL Server | `STDEV` accumulates in one pass and loses digits with large means | the standard deviation is requested on the values centered on the mean |
| SQL Server | it does not admit `COUNT(DISTINCT)`, `GROUP BY`, `ORDER BY` or `MIN`/`MAX` on `text`, `ntext`, `image`, `xml` and `geography` | those metrics stay `no_disponible` without sending the statement |
| any | the collation of the column groups values that R tells apart (`COLLATE NOCASE`, non-deterministic ICU, trailing spaces in SQL Server) | `meta$criterio_comparacion` and `meta$criterio_moda` declare it; the corroboration leaves it `no_corroborada`, not `divergencia` |
| any | 64-bit integers (or `HUGEINT` and `UHUGEINT` in DuckDB) or decimals that arrive as double from 2^53 | the 7 magnitude metrics stay `no_disponible`; the mode is requested from the engine as exact text; the sample reads those columns as text and, with [`bit64`](https://CRAN.R-project.org/package=bit64), as `integer64` |
| any | each engine adds up in its own way, and with values that cancel out the means differ between engines and from the one of R | the mean is requested as double where decimal arithmetic would cut it; the corroboration declares the difference |

The mean of `{1e16, 1, -1e16}` is 1/3. Each path computes it in its own
way, and none is taken as true:

``` r

cancelan <- data.frame(x = c(1e16, 1, -1e16))
DBI::dbWriteTable(con, "cancelan", cancelan)
DBI::dbWriteTable(duck, "cancelan", cancelan)
c(
  sqlite = DBI::dbGetQuery(con, "SELECT AVG(x) FROM cancelan")[[1]],
  duckdb = DBI::dbGetQuery(duck, "SELECT AVG(x) FROM cancelan")[[1]],
  r = mean(cancelan$x)
)
#>    sqlite    duckdb         r 
#> 0.3333333 0.0000000 0.3336589
```

When the 2 blocks exist and differ beyond the tolerance, the
corroboration leaves the `divergencia` row with the 2 values.

In SQLite, the difference of the dates shows like this:

``` r

fechas <- data.frame(f = as.Date("2019-03-01") + 0:9)
perfilar_fechas <- function(...) {
  base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", ...)
  on.exit(DBI::dbDisconnect(base))
  DBI::dbWriteTable(base, "fechas", fechas)
  x <- perfilar_dbi(base, "fechas", bloque_muestra = "solo_agregados",
                    proteger_datos_personales = FALSE)
  columnas(x)[, c("columna", "minimo", "maximo", "media")]
}
perfilar_fechas()
#>   columna minimo maximo   media
#> 1       f  17956  17965 17960.5
perfilar_fechas(extended_types = TRUE)
#>   columna minimo maximo media
#> 1       f     NA     NA    NA
```

## The primary key by engine

The primary key is not guessed: it is read from the catalog of the
engine. That metadata query does not go through the table, and it is
done in every run, with any cost policy. The answer stays in
`resumen_tabla$meta$clave`, with `columnas`, `fuente`, `motivo`,
`garantia` and `estado`. `garantia` (*guarantee*) is `garantizada`
(*guaranteed*), `declarada_no_garantizada` (*declared, not guaranteed*),
`desconocida` (*unknown*) or `no_declarada` (*not declared*). A failed
query keeps its reason and is not confused with a table without a key.

With an unqualified name, the key published is the one of the relation
that the engine resolves, with its same schema rules. A key whose
columns are not among the ones just measured is discarded whole, in any
engine, and the reason is given.

What each catalog can state:

| engine | catalog path | `garantizada` when |
|----|----|----|
| PostgreSQL | `pg_catalog` | the constraint is enforced and validated, without child tables by inheritance (partitions do not count), it is not `DEFERRABLE` and its index is unique; otherwise, `declarada_no_garantizada` |
| MySQL | `information_schema` | `ENFORCED` is `YES` |
| Oracle | `all_constraints` | `STATUS` is `ENABLED`, it is `VALIDATED` and it is not `DEFERRABLE` (tested with a simulated engine) |
| SQLite | `pragma` | all the columns of the key are `NOT NULL`; otherwise, uniqueness among non-null values is guaranteed but the guarantee stays `desconocida` |
| MariaDB | `show_index` | never: the catalog does not expose a comparable status and the guarantee stays `desconocida` |
| SQL Server | `information_schema` | never: it stays `desconocida` |
| DuckDB | `duckdb_constraints` | never: it stays `desconocida` |

``` r

crear <- function(conexion, sql) invisible(DBI::dbExecute(conexion, sql))
crear(con, "CREATE TABLE con_nulos (id INTEGER PRIMARY KEY, x TEXT)")
crear(con, "CREATE TABLE sin_nulos (id INTEGER NOT NULL PRIMARY KEY, x TEXT)")
crear(con, "CREATE TABLE sin_clave (id INTEGER, x TEXT)")
crear(duck, "CREATE TABLE padron (id INTEGER PRIMARY KEY, x VARCHAR)")
leer_clave <- function(conexion, tabla) {
  x <- perfilar_dbi(conexion, tabla, bloque_muestra = "solo_agregados")
  k <- x$resumen_tabla$meta$clave
  data.frame(tabla = tabla, columnas = paste(k$columnas, collapse = ", "),
             fuente = k$fuente, garantia = k$garantia)
}
rbind(
  leer_clave(con, "con_nulos"),
  leer_clave(con, "sin_nulos"),
  leer_clave(con, "sin_clave"),
  leer_clave(duck, "padron")
)
#>       tabla columnas             fuente     garantia
#> 1 con_nulos       id             pragma  desconocida
#> 2 sin_nulos       id             pragma  garantizada
#> 3 sin_clave                      pragma no_declarada
#> 4    padron       id duckdb_constraints  desconocida
```

With `politica_costo = "por_cardinalidad"`, a guaranteed key says how
many distinct values its column has without counting them. The check of
a key that the user declares on data in memory (`perfilar(clave = ...)`)
is in [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
(*Reading a profile*, in Spanish). The key suggestions are there too.

## 🗄️ Tested engines

The matrix is assembled when the page is built, with the output of
`benchmark/verificar_motor.R`: the file
`benchmark/datos/verificar-motor.csv`. Each row is an engine of the same
run, with:

- the engine and the version that the server reports;
- the R driver and its version;
- the result of each check: `ok`, or `fallo` (*failure*) with the
  message.

Below go the date of the run, the revision of the package measured and
the fixture. An engine that did not respond stays `no_medido` (*not
measured*), with its reason.

| motor | servidor | controlador | estado | cinco columnas | dialecto | media | clave primaria | cobertura |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| PostgreSQL 16 | 16.15 | RPostgres 1.4.10 | `medido` | ok | ok | ok | ok | ok |
| PostgreSQL 9.3 | 9.3.25 | RPostgres 1.4.10 | `medido` | ok | ok | ok | ok | ok |
| MySQL 8.4 | 8.4.11 | RMySQL 0.11.3 | `medido` | ok | ok | ok | ok | ok |
| MariaDB 11.8 | 11.8.8 | RMariaDB 1.3.5 | `medido` | ok | ok | ok | ok | ok |
| SQL Server 2022 | 16.0.4265.3 | odbc/FreeTDS 1.7.0 | `medido` | ok | ok | ok | ok | ok |
| SQLite | 3.53.3 | RSQLite 3.53.3 | `medido` | ok | ok | ok | ok | ok |
| DuckDB | 1.5.5 | duckdb 1.5.5 | `medido` | ok | ok | ok | ok | ok |

Corrida del 2026-10-24T13:12:59Z (UTC), sobre la revisión
`0.1.0; R/ md5 08c0a1af7ccb` del paquete. Fixture declarado por el
guion: «tramites: cinco columnas; padron: clave primaria id_persona».

What `benchmark/verificar_motor.R` does:

- It is run from the root of the source code:
  `Rscript benchmark/verificar_motor.R [motores]`. Without arguments it
  goes through the 7 engines of the matrix (`postgres16`, `postgres93`,
  `mysql84`, `mariadb118`, `sqlserver2022`, `sqlite` and `duckdb`). With
  arguments, only the named ones.
- It loads the package from the source with `pkgload`, if available: it
  measures the code of the tree and not the installed package.
- It does not take an arbitrary connection: each engine has its
  connection written in the script. SQLite and DuckDB go in memory; the
  others, in local containers with their own ports. The passwords of
  MySQL, MariaDB and SQL Server are read from `LUPA_MYSQL_PWD`,
  `LUPA_MARIADB_PWD` and `LUPA_MSSQL_PWD`. For another server or another
  engine, its connection has to be added to the script or the checks
  repeated by hand.
- It writes the fixture in each engine, and profiles `tramites` with the
  call that follows this list. The fixture has 2 tables:
  - `tramites`, of 500 rows and 5 columns: identifier, document,
    category, amount and age with 20 missing values;
  - `padron`, with primary key `id_persona` declared `NOT NULL`.
- It makes 5 checks:
  - that the summary has 5 columns;
  - that the dialect was resolved;
  - that the mean of `monto` of the engine matches the one of R on the
    same column, with relative tolerance of `1e-8`;
  - that the primary key of `padron` is read from the catalog as
    `id_persona`;
  - that `resumen_tabla$cobertura` is a table.
- It ends with a non-zero code if any check fails.

``` r

perfilar_dbi(
  con, "tramites", muestra = 500L, analizar_dependencias = FALSE,
  duplicados_aproximados = FALSE, proteger_datos_personales = FALSE
)
```

It does not check how many metrics are computed or how many stay
`no_aplica`, nor does it time anything. The check of the mean holds for
finite values of ordinary scale, like those of the fixture. With `NaN`,
infinities or sums that lose precision, the engine and R do not match.
That is declared by the corroboration between blocks
([above](#not-translated)).

What is not in the run:

| case | status | source |
|----|----|----|
| engine that rejects `LIMIT` (the case of SQL Server, whose real matrix does not exercise that rejection) | tested with a simulated engine | `tests/testthat/test-arreglos-dbi.R` |
| engine that folds the aliases to upper case, like Oracle, DB2, Firebird or Snowflake | tested with a simulated engine | `tests/testthat/test-arreglos-dbi.R` |
| engine that rejects a column (a LOB that the driver does not materialize, a per-column permission) | tested with a simulated engine | `tests/testthat/test-arreglos-dbi.R` |
| Oracle (`fetch_first`, `rownum`, `SAMPLE`) | tested with a simulated engine; without a run against the real engine | `tests/testthat/test-dialecto-oracle.R` |
| BigQuery | not tested | — |
| another engine compatible with DBI | not tested; it runs through the portable path | — |

The simulated engine is a subclass of the RSQLite connection. It
intercepts the queries and behaves like the engine it imitates. It tests
that restriction, not the engine. With another engine, another driver or
another version, the function runs and declares in the coverage what it
could not measure. But its figures are not checked against that engine.

## Partial failures and errors

No block discards the other:

- If the sample is requested and the engine rejects its query, or the
  sample cannot be profiled, the result comes out anyway. It carries the
  full `resumen_tabla`, `perfil_muestra = NULL` and a `no_disponible`
  row in the coverage, with the reason and how to solve it. If it was
  not requested, the row says `no_solicitado`, which is not a failure.
- If the engine rejects a column, its metrics stay `no_disponible` and
  the others are profiled whole. If the read of the sample fails, the
  culprit column is searched for by reading the same rows with fewer
  columns. An `alcance_distinto` row names it.
- A column that the credential cannot read, because of a per-column
  permission, is recognized in the catalog before reading.
  `has_column_privilege()` does it in PostgreSQL and
  `HAS_PERMS_BY_NAME()` in SQL Server. That column shows up with its
  metrics in `no_disponible` and is not named in any query.
- With dynamic masking of SQL Server and a credential without `UNMASK`,
  completeness is published. What depends on the value stays
  `no_disponible`.
- If a view fails on a row under `muestra_motor` or `bloque_filas`, the
  path tries the same summary on the whole table. The sample or the pass
  stay declared as `no_disponible`.
- With odbc, under a locale that is not UTF-8, a column name with an
  accent is not sent: the column stays `no_disponible`. The details of
  encoding are in [Codificación y
  locales](https://sebollin.github.io/lupa/articles/codificacion-y-locales.html)
  (*Encoding and locales*, in Spanish).

The errors of the path carry their own class, to catch them with
[`tryCatch()`](https://rdrr.io/r/base/conditions.html). All of them
inherit from `lupa_error_dbi`:

- `lupa_error_conexion_dbi`;
- `lupa_error_tabla_dbi`;
- `lupa_error_campos_dbi`;
- `lupa_error_conteo_dbi`;
- `lupa_error_esquema_dbi`;
- `lupa_error_codificacion_dbi`;
- `lupa_error_argumento_dbi`;
- `lupa_error_transaccion_dbi`.

``` r

error <- tryCatch(perfilar_dbi(con, "no_existe"), error = function(e) e)
class(error)
#> [1] "lupa_error_tabla_dbi" "lupa_error_dbi"       "error"               
#> [4] "condition"
```

## Dependencies and installation

`lupa` has 2 required dependencies, `cli` and `data.table`. It does not
import any of them into its namespace: it calls them with `::`. It is
deliberate: `data.table` decides the meaning of `[` depending on whether
the calling package imports it. With the import in place,
`tabla[, columnas, drop = FALSE]` would stop selecting columns in the
functions that receive a table from the user.

`data.table` is used for 3 things:

- **Counting exact duplicate rows.** From 25 rows, the count uses
  [`data.table::frank()`](https://rdrr.io/pkg/data.table/man/frank.html)
  with dense ranks. It falls back to
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html) of base,
  which is the one that sets the result, in these cases:
  - list or compound columns;
  - `NaN` in double columns;
  - a [`duplicated()`](https://rdrr.io/r/base/duplicated.html) method of
    its own;
  - a numeric rounding setting of `data.table` in the session that is
    not exact (`setNumericRounding()`).
- **Copying `data.table` inputs before selecting columns.** A table of
  that class is copied to a `data.frame` with
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html),
  so the `[` that runs is the one of base.
- **Returning a usable `data.table` in remediation.**
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  copies the input, keeps the key (`setkeyv()`) and restores the column
  reserve (`setalloccol()`). That way the result is still a sound
  `data.table`.

The suggested ones are `bit64`, `covr`, `DBI`, `duckdb`, `jsonlite`,
`knitr`, `odbc`, `rJava`, `rmarkdown`, `RPostgres`, `RSQLite`, `sf`,
`stringdist`, `stringi`, `testthat`, `tibble` and `units`. In the DBI
path these take part:

- `DBI`;
- the driver of each engine (`RSQLite`, `duckdb`, `RPostgres`, `odbc`);
- `bit64`, which allows reading 64-bit integers in the sample without
  losing values.

The drivers of other engines (`RMariaDB`, `RMySQL`, `ROracle`,
`bigrquery`) are installed separately.

The hard case is not the R package but the system library underneath.
`RMariaDB` does not compile without the headers of the MySQL or MariaDB
client, and `ROracle` needs the Oracle Instant Client.
[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
(*engine requirements*) says so before running into it.

Its catalog has 12 entries: 9 for named engines and 3 generic ones. The
2 versions of Oracle count separately. The generic ones (`dbi`, `odbc`
and `otro_dbi`) are not measured engines.

``` r

r <- requisitos_motor()
knitr::kable(r[, c("id_motor", "motor", "version", "paquete_r", "dialecto",
                   "estado_prueba")], row.names = FALSE)
```

| id_motor | motor | version | paquete_r | dialecto | estado_prueba |
|:---|:---|:---|:---|:---|:---|
| dbi | DBI | — | DBI | — | no_aplica |
| sqlite | SQLite | — | RSQLite | limit | probado |
| postgresql | PostgreSQL | 16 | RPostgres | limit | probado |
| mysql | MySQL | 8 | RMySQL | limit | probado |
| sql_server | SQL Server | 2022 | odbc | top | probado |
| duckdb | DuckDB | 1.5 | duckdb | limit | probado |
| mariadb | MariaDB | 11 | RMariaDB | limit | probado |
| oracle_12c | Oracle | Free 23 (12c+) | ROracle | fetch_first | esperado |
| oracle_11 | Oracle | 11 y anteriores | ROracle | rownum | esperado |
| bigquery | BigQuery | — | bigrquery | portable | no_documentado |
| odbc | ODBC | — | odbc | portable | no_documentado |
| otro_dbi | Otro compatible con DBI | — | — | portable | reserva |

Each entry also declares the system library and its package in Debian
and in Fedora. If there is a way out without administrator rights, it
declares it too:

``` r

writeLines(strwrap(requisitos_motor("sql_server")$alternativa_sin_administrador, 88))
#> Compilar FreeTDS en un prefijo del usuario y pasar la ruta directa del controlador en
#> la cadena de conexión; no hace falta registrar un DSN.
```

[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
does not claim to have checked a system library that it cannot check. In
that case, its status is `no_comprobada` (*not checked*).

`estado_prueba` (*testing status*) is a declaration of the catalog, with
its source in `fuente_estado`. `esperado` (*expected*) means that the
dialect is built and tested against a simulated engine that reproduces
its restriction, not against the real engine. The evidence against each
engine is the run of [Tested engines](#tested-engines).

The columns that depend on the machine —whether the R package is
installed— are computed when called and are not shown here.

## Threads

The work that can be parallelized uses **2 threads by default**
(`getOption("lupa.nucleos", 2L)`), the cap that CRAN asks to respect. It
is raised per call with `nucleos = 8` (*cores*), or for the whole
session with `options(lupa.nucleos = 8)`. The value is limited to the
available cores and stays published in `alcance$nucleos_usados`. The
result does not change, only how long it takes.

The threads govern the text comparisons that are delegated to
`stringdist`. They are those of:

- [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md);
- [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
  (*estimate cost*);
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  when approximate duplicates are requested, also for `perfil_muestra`
  through `...`.

The exact count of duplicate rows uses the thread configuration of
`data.table` itself
([`data.table::setDTthreads()`](https://rdrr.io/pkg/data.table/man/openmp-utils.html)),
which the package does not change.

[Escala y
duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)
(*Scale and approximate duplicates*, in Spanish) explains how to choose
the number of threads. The times measured by number of threads, with
their date and their environment, are in [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html)
(*Evidence and performance*, in Spanish).

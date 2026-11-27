# Profiling a database

> This guide is the English version of [10. Perfilar una
> base](https://sebollin.github.io/lupa/articles/perfilar-una-base.md).
> `lupa` writes its messages and tables in Spanish, so the output below
> is in Spanish.

There is a sales table in a database, perhaps too large for the memory
of the session. How is it decided what the engine computes and how many
rows reach R, and where is it recorded how they were chosen?

> **In this guide:**
>
> - profiling a table with
>   [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
>   and telling apart its 2 blocks, `resumen_tabla` and
>   `perfil_muestra`;
> - comparing what each option brings into R: the default call,
>   `muestra = n`, `universo = "muestra_motor"` and
>   `bloque_muestra = "solo_agregados"`;
> - finding the sampling metadata, which lives in 2 places depending on
>   the option;
> - reading the result with the same accessors as a profile in memory;
> - counting sentinel dates, periods and recency, in memory and in the
>   engine.

## A table in a database

The example uses SQLite in memory, with the packages `DBI` and
`RSQLite`. The table has 1,000 rows so that the guide runs in seconds;
the decisions are the same with millions.
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
only prepares the example.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
(*profile through DBI*) sends read queries: it does not write to the
database or create temporary tables.

``` r

library(lupa)

n <- 1000
ventas <- data.frame(
  id_venta = seq_len(n),
  sucursal = rep(c("Norte", "Sur", "Este", "Oeste"), length.out = n),
  monto = 100 + (seq_len(n) * 37) %% 800,
  fecha = format(as.Date("2026-01-01") + seq_len(n) %% 180),
  stringsAsFactors = FALSE
)
ventas$monto[seq(7, n, by = 50)] <- NA
head(ventas, 3)
#>   id_venta sucursal monto      fecha
#> 1        1    Norte   137 2026-01-02
#> 2        2      Sur   174 2026-01-03
#> 3        3     Este   211 2026-01-04
```

``` r

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conexion, "ventas", ventas)
```

## Profiling with the default values

``` r

completo <- perfilar_dbi(conexion, "ventas")
completo
#> Perfil DBI de ventas
#> Resumen de tabla completa: 4 columnas sobre 1000 filas
#> Clave primaria: no declara clave (fuente `pragma`).
#> Estabilidad de la tabla: sin cambios en los controles verificados (conteo de filas
#> (COUNT(*)); no descarta cambios fuera de esos controles).
#> Duplicados exactos: 0 excedente(s) (calculado).
#> Métricas: calculado 45, no_aplica 15
#> Consultas emitidas: 23 (dialecto limit)
#> Derrame estimado: no se pudo estimar (No se pudo estimar el derrame: la conexión no fue
#> reconocida como PostgreSQL y no hay un `pg_stats` portable.).
#> Derrame: no disponible (El controlador no fue reconocido como PostgreSQL; no se puede
#> consultar una estadística de bloques temporales portable.).
#> Perfil de muestra: 1000 filas, reproducible: FALSE
#> Diagnósticos no evaluados sobre la muestra: 3, en `perfil_muestra$cobertura_diagnosticos`
#> No se imprime ningún valor de celda: están en `resumen_tabla$columnas`.
```

The printout shows the 2 blocks:

- «Resumen de tabla completa» is `resumen_tabla` (*table summary*):
  aggregates that the engine computes in SQL over all the rows.
- «Perfil de muestra» is `perfil_muestra` (*sample profile*):
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  run in R on the rows that were read.

The spill lines only have content in PostgreSQL.

By default, `universo = "tabla_completa"` (*universe = whole table*) and
`muestra = Inf` (*sample*). That means:

- the aggregates go through the whole table;
- `lupa` also brings into R **every row that fits within the read
  caps**. The caps are `max_celdas_muestra` (1,000,000 cells by default)
  and `max_bytes_muestra` (512 MiB).

With this table, all 1,000 rows fit. The default call is not a cheap
sample: it is the whole table, if it fits.

## What each option brings into R

The other 3 decisions are tried on the same table. The result has one
row per option, with:

- how many rows the SQL summary describes;
- how many were read into R;
- in which of the 2 places the sampling metadata was left.

``` r

acotado <- perfilar_dbi(conexion, "ventas", muestra = 200, orden_muestra = "id_venta")
en_motor <- perfilar_dbi(
  conexion, "ventas", universo = "muestra_motor", muestra_motor = 150
)
agregados <- perfilar_dbi(conexion, "ventas", bloque_muestra = "solo_agregados")

que_trae <- function(perfil, opcion) {
  muestreo_sql <- perfil$resumen_tabla$meta$muestreo
  muestreo_r <- perfil$perfil_muestra$meta$origen_dbi$muestreo
  data.frame(
    opcion = opcion,
    filas_resumen = if (is.null(muestreo_sql)) {
      n_filas(perfil)
    } else {
      muestreo_sql$filas_obtenidas
    },
    filas_en_r = if (is.null(muestreo_r)) 0 else muestreo_r$filas_obtenidas,
    meta_resumen = !is.null(muestreo_sql),
    meta_lectura = !is.null(muestreo_r)
  )
}

rbind(
  que_trae(completo, "por omisión"),
  que_trae(acotado, "muestra = 200"),
  que_trae(en_motor, "universo = \"muestra_motor\""),
  que_trae(agregados, "bloque_muestra = \"solo_agregados\"")
)
#>                              opcion filas_resumen filas_en_r meta_resumen meta_lectura
#> 1                       por omisión          1000       1000        FALSE         TRUE
#> 2                     muestra = 200          1000        200        FALSE         TRUE
#> 3        universo = "muestra_motor"           150        150         TRUE         TRUE
#> 4 bloque_muestra = "solo_agregados"          1000          0        FALSE        FALSE
```

Each row answers a different question (`por omisión` is the default
call):

- **`muestra = n`** bounds the read into R, not the aggregates: the
  summary still describes the 1,000 rows, and the row-level profile,
  200.
- **`universo = "muestra_motor"`** (*engine sample*) with
  `muestra_motor = n` changes the universe of the aggregates. The engine
  chooses the sample only once and `lupa` keeps it in a copy in the R
  session. Everything else is computed on those same rows: summary and
  profile describe the same 150.
- **`bloque_muestra = "solo_agregados"`** (*sample block = aggregates
  only*) brings no rows: `perfil_muestra` stays `NULL` and there are
  only counts and summaries computed in the engine. It works for both
  universes.

[`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*number of rows*) always returns the count of the whole table, also
with `muestra_motor`.

## Where the sampling is written down

The last 2 columns of the table above say whether each place where the
sampling metadata lives exists:

- `meta_resumen` looks at `resumen_tabla$meta$muestreo`;
- `meta_lectura` looks at `perfil_muestra$meta$origen_dbi$muestreo`.

Each one exists under its own condition:

| path | describes | exists when |
|----|----|----|
| `perfil_muestra$meta$origen_dbi$muestreo` | the rows read into R | there is a `perfil_muestra`, with any universe |
| `resumen_tabla$meta$muestreo` | the sample chosen by the engine | `universo = "muestra_motor"`; otherwise it is `NULL` |

For the read into R, the metadata says:

- how many rows were requested and how many arrived;
- how many the table has;
- how they were chosen;
- whether the choice can be repeated.

``` r

lectura <- acotado$perfil_muestra$meta$origen_dbi$muestreo
str(lectura[c(
  "filas_solicitadas", "filas_obtenidas", "filas_totales_fuente", "metodo",
  "reproducible"
)])
#> List of 5
#>  $ filas_solicitadas   : num 200
#>  $ filas_obtenidas     : num 200
#>  $ filas_totales_fuente: num 1000
#>  $ metodo              : chr "primeras_filas_segun_orden"
#>  $ reproducible        : logi TRUE
```

`reproducible` is `TRUE` because `orden_muestra = "id_venta"` (*sample
order*) was declared and `lupa` checked on the whole table that this
order is unique. Without a declared order, a `LIMIT` has no guaranteed
order. Then `reproducible` is `FALSE`, as in the default call:

``` r

sin_orden <- completo$perfil_muestra$meta$origen_dbi$muestreo
sin_orden$reproducible
#> [1] FALSE
writeLines(strwrap(sin_orden$motivo_reproducibilidad, 88))
#> No se declaró `orden_muestra`; SQL no garantiza el orden de las filas.
```

For the engine sample, the metadata is in the summary. The method
depends on what the engine accepts. `lupa` first tries the variants of
`TABLESAMPLE`. If none works, it orders by a pseudorandom function with
the limit of the dialect, as in SQLite.

``` r

del_motor <- en_motor$resumen_tabla$meta$muestreo
str(del_motor[c("metodo", "tamano_muestra", "fraccion", "universo", "reproducible")])
#> List of 5
#>  $ metodo        : chr "random_limit"
#>  $ tamano_muestra: num 150
#>  $ fraccion      : num 0.15
#>  $ universo      : num 1000
#>  $ reproducible  : logi FALSE
```

There `universo` is the size of the table the sample came from, and
`fraccion` (*fraction*) the part that was taken. The pseudorandom sample
changes on every run, and `reproducible` says so. Each metric of
`resumen_tabla$sql` also declares its scope: the row count is from the
whole table and the rest, from the sample.

``` r

sql <- en_motor$resumen_tabla$sql
sql[sql$columna == "monto", c("columna", "metrica", "estado", "alcance")][1:6, ]
#>     columna        metrica            estado        alcance
#> 3     monto              n         calculado tabla_completa
#> 110   monto      n_validos observado_muestra        muestra
#> 22    monto    n_faltantes observado_muestra        muestra
#> 32    monto prop_faltantes observado_muestra        muestra
#> 42    monto    n_distintos observado_muestra        muestra
#> 52    monto tasa_distintos observado_muestra        muestra
```

The status `observado_muestra` (*observed in the sample*) warns that the
value describes the sample. In particular, `n_distintos` is the
cardinality observed in those 150 rows, not an estimate of the
cardinality of the table.

## The 2 blocks, side by side

``` r

data.frame(
  bloque = c("resumen_tabla", "perfil_muestra"),
  calculado_en = c("el motor", "R"),
  filas = c(n_filas(acotado), n_filas(acotado$perfil_muestra)),
  campos_analiticos = c(
    ncol(acotado$resumen_tabla$columnas) - 1L,
    ncol(acotado$perfil_muestra$columnas) - 1L
  )
)
#>           bloque calculado_en filas campos_analiticos
#> 1  resumen_tabla     el motor  1000                16
#> 2 perfil_muestra            R   200               117
```

The summary has 16 analytic fields per column. They are the ones SQL can
compute without bringing the data: missing values, distinct values,
mode, minimum, maximum, mean, median and standard deviation, among
others. The profile of the sample has the 117 of
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
but on the rows that were read. That is why `lupa` does not mix them.
Calling the summary a *profile* would claim a completeness it does not
have; reading the profile as if it were of the whole table, too.

## Reading the result with the accessors

The accessors of [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
(*Reading a profile*, in Spanish) work the same on the result of
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*columns*) returns the summary of the engine and
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*findings*), the findings of the sample profile:

``` r

columnas(acotado)[, c("columna", "n", "n_faltantes", "n_distintos", "minimo", "maximo")]
#>    columna    n n_faltantes n_distintos minimo maximo
#> 1 id_venta 1000           0        1000      1   1000
#> 2 sucursal 1000           0           4     NA     NA
#> 3    monto 1000          20         784    100    899
#> 4    fecha 1000           0         180     NA     NA
hallazgos(acotado)[, c("columna", "tipo_hallazgo", "severidad", "n_afectados")]
#>    columna               tipo_hallazgo  severidad n_afectados
#> 1 id_venta       posible_identificador         ok           1
#> 2    fecha           alta_cardinalidad sospechoso           1
#> 3    fecha     tipo_declarado_distinto sospechoso           1
#> 4 sucursal casi_duplicados_vocabulario         ok           0
#> 5    fecha casi_duplicados_vocabulario         ok           0
nrow(sql_perfil(acotado))
#> [1] 64
```

The engine counted the 20 missing values of `monto` in the 1,000 rows.
In its 200 rows, the row-level profile found that:

- `fecha` is stored as text although it holds dates
  (`tipo_declarado_distinto`, *declared type differs*);
- almost every row carries a different date (`alta_cardinalidad`, *high
  cardinality*).

[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
(*SQL profile*) returns one row per column and metric, with the query
that produced it, its status and its reason.

When the sample was not requested, the accessors do not make up what is
not there:

- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  returns an empty table with a warning;
- [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  (*coverage*) declares the block as `no_solicitado` (*not requested*),
  which is not a failure.

``` r

aviso <- NULL
sin_filas <- withCallingHandlers(
  hallazgos(agregados),
  warning = function(w) {
    aviso <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
nrow(sin_filas)
#> [1] 0
writeLines(strwrap(aviso, 88))
#> Este perfil DBI no solicitó la muestra, así que no hay hallazgos por fila. El resumen
#> por columna sigue disponible en columnas(); el detalle, en cobertura().
cobertura(agregados)[, c("bloque", "elemento", "estado")]
#>           bloque elemento        estado
#> 1 perfil_muestra   ventas no_solicitado
```

## 📅 Sentinel dates, periods and recency

A date can be there to say something else: `1900-01-01` for “unknown”,
`9999-12-31` for “still in force”. By default
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
suspects 4 of them (1900-01-01, 9999-12-31, 1970-01-01 and 1899-12-30),
and only if they repeat and stand apart from the rest of the dates.
Three arguments say more, and they work the same in
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
and in
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):

- `centinelas_fecha` (*sentinel dates*) declares the sentinel dates of
  the database, which are flagged as `error` from their first
  appearance;
- `periodos` (*periods*) declares which pair of columns is the start and
  the end of a period and, with `fin_abierto` (*open end*), which end
  date means “in force”. They are never guessed from the name;
- `resumen_fechas = TRUE` (*date summary*) adds the counts of sentinel
  and future dates of each date column, and its recency: the latest
  value and how many days ago it was.

The example is a table of contracts. The ones in force end on
`9999-12-31`, 2 have no end and one ends before it starts. In `alta`
(*registration*) there are 3 `1900-01-01`, one `2000-01-01`, which in
this database also means “no data”, and one future date. The date of the
run is fixed so that the example always gives the same result.

``` r

dias <- as.Date("2025-01-01") + (1:60 * 11) %% 500
contratos <- data.frame(id_contrato = 1:60, alta = dias, desde = dias, hasta = dias + 90)
contratos$hasta[c(3, 9, 15, 21)] <- as.Date("9999-12-31")
contratos$hasta[c(4, 10)] <- NA
contratos$hasta[5] <- contratos$desde[5] - 10
contratos$alta[c(7, 14, 28)] <- as.Date("1900-01-01")
contratos$alta[33] <- as.Date("2000-01-01")
contratos$alta[40] <- as.Date("2027-06-01")

periodo <- list(inicio = "desde", fin = "hasta", fin_abierto = "9999-12-31")
en_memoria <- perfilar(
  contratos, fecha = "2026-06-30", centinelas_fecha = "2000-01-01",
  periodos = periodo, resumen_fechas = TRUE
)
campos_fecha <- c(
  "columna", "n_fechas_centinela", "n_fechas_centinela_declaradas",
  "n_fechas_fin_abierto", "n_fechas_futuras", "lectura_fechas_futuras",
  "ultimo_dato_fecha"
)
columnas(en_memoria)[-1, campos_fecha]
#>   columna n_fechas_centinela n_fechas_centinela_declaradas n_fechas_fin_abierto
#> 2    alta                  4                             1                    0
#> 3   desde                  0                             0                    0
#> 4   hasta                  4                             0                    4
#>   n_fechas_futuras lectura_fechas_futuras ultimo_dato_fecha
#> 2                1                   fila        2026-05-11
#> 3                0                   <NA>        2026-05-11
#> 4                8              admitidas        2026-06-26
```

In `alta`, the 4 sentinel dates are the 3 built-in ones and the declared
one; the future date is a stray value, and
`lectura_fechas_futuras = "fila"` (*read as row*) says it is flagged
with its row. In `hasta` (*until*), the 4 `9999-12-31` are open ends,
not defects. In a column whose name looks ahead, such as `hasta` or an
expiry date, future dates are counted all the same, but they can be
legitimate and their reading is `admitidas` (*admitted*).

`general$actualidad` (*recency*) gives, per column, the latest value
that is neither future nor a declared sentinel, and how many days before
the `fecha` of the run it falls. There is no winning date across columns
and no score. `general$periodos` splits the rows of each declared period
into categories that do not overlap:

``` r

en_memoria$general$actualidad$columnas[
  , c("columna", "ultimo_dato_fecha", "antiguedad_dias", "n_evaluadas")
]
#>   columna ultimo_dato_fecha antiguedad_dias n_evaluadas
#> 1    alta        2026-05-11              50          58
#> 2   desde        2026-05-11              50          60
#> 3   hasta        2026-06-26               4          50
particion <- en_memoria$general$periodos
particion[, c(
  "n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
  "n_fin_abierto_centinela"
)]
#>    n n_cerrados_validos n_fin_antes_inicio n_fin_abierto_nulo n_fin_abierto_centinela
#> 1 60                 53                  1                  2                       4
writeLines(strwrap(particion$aviso, 88))
#> El fin abierto mezcla dos convenciones: fines ausentes y fines con la fecha declarada
#> en `fin_abierto`.
```

Of the 60 rows, 53 are valid closed periods, 1 ends before it starts, 2
have no end and 4 end on the in-force date. The warning points out that
the open end is written in 2 ways. An end date that is usually a
sentinel and was not declared is counted apart, with a warning of a
possible open end: it does not pass for a closed period.

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
takes the same arguments and counts the same on the whole table, in
SQLite and DuckDB, against literals and with the same rule as memory.
The counts go to `resumen_tabla$columnas`, the recency to
`resumen_tabla$meta$actualidad` and the periods to
`resumen_tabla$meta$periodos`. SQLite has no date type: with
`extended_types = TRUE` when connecting, RSQLite stores dates as dates
([Engines and
scale](https://sebollin.github.io/lupa/articles/engines-and-scale.html#differences-with-r)
has the details).

``` r

con_fechas <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
DBI::dbWriteTable(con_fechas, "contratos", contratos)
en_motor <- perfilar_dbi(
  con_fechas, "contratos", fecha = "2026-06-30", centinelas_fecha = "2000-01-01",
  periodos = periodo, resumen_fechas = TRUE
)
identical(
  en_motor$resumen_tabla$columnas[, campos_fecha],
  columnas(en_memoria)[, campos_fecha]
)
#> [1] TRUE
en_motor$resumen_tabla$meta$periodos[, c("n", "n_cerrados_validos", "estado")]
#>    n n_cerrados_validos    estado
#> 1 60                 53 calculado
```

What SQL cannot count the way R reads it is declared. There are 3 cases:
a SQLite column that stores dates as text, instants less than a second
away from a distant sentinel with microsecond resolution, and a text
column that the sample reads as a date. The `fecha` column of `ventas`
is the last case:

``` r

texto <- perfilar_dbi(
  conexion, "ventas", muestra = 200, orden_muestra = "id_venta",
  fecha = "2026-06-30", resumen_fechas = TRUE
)
sql_fechas <- texto$resumen_tabla$sql
sql_fechas <- unique(
  sql_fechas[sql_fechas$etapa == "fechas", c("columna", "estado", "motivo")]
)
sql_fechas$estado
#> [1] "no_disponible"
writeLines(strwrap(sql_fechas$motivo, 88))
#> La columna `fecha` es texto en el motor y la muestra la lee como fecha: sus formatos
#> los decide la columna entera. Con `estrategia_no_sql = "bloques"` se cuenta en R.
por_bloques <- perfilar_dbi(
  conexion, "ventas", muestra = 200, orden_muestra = "id_venta",
  fecha = "2026-06-30", resumen_fechas = TRUE, estrategia_no_sql = "bloques"
)
por_bloques$resumen_tabla$meta$actualidad$columnas[
  , c("columna", "ultimo_dato_fecha", "antiguedad_dias", "n_evaluadas", "estado")
]
#>   columna ultimo_dato_fecha antiguedad_dias n_evaluadas    estado
#> 1   fecha        2026-06-29               1        1000 calculado
```

With `estrategia_no_sql = "no_medible"` (*strategy for what is not SQL*;
*not measurable*), the default, those counts are left `no_disponible`
(*not available*) in `resumen_tabla$sql` with their reason, and never as
zero. With `"bloques"` (*blocks*), `lupa` reads the column in R by
blocks, with the same driver, and counts it with the same rule: the
1,000 dates, not only the 200 of the sample. The partition of a period,
on the other hand, needs its 2 columns to be dates in the engine: on
text it is left `no_disponible` with its reason, also with `"bloques"`.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
takes the same arguments and counts those queries before running.

## ⚠️ What the summary does not guarantee

The aggregates are separate queries, and the table can change between
one and the next. The summary declares it:

``` r

completo$resumen_tabla$meta$snapshot
#> [1] FALSE
writeLines(strwrap(completo$resumen_tabla$meta$nota_snapshot, 88))
#> No hubo lectura instantánea: cada agregado se midió en su propio momento y la tabla
#> pudo cambiar entre consultas. La cobertura declara las comparaciones incoherentes entre
#> grupos de consistencia distintos.
```

If `n_validos` and `n_distintos` come out inconsistent and come from
different queries,
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
adds an `alcance_distinto` (*different scope*) row with the 2
statements. It is evidence that the table changed during the run, not an
accusation against the engine.

## Before connecting to another engine

The engine package and the system library underneath are installed by
whoever uses `lupa`. The error that shows up when one of them is missing
rarely says which.
[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
(*engine requirements*) says it before connecting:

``` r

requisitos_motor()            # the whole catalog
requisitos_motor("mariadb")   # what MariaDB needs and how to get it
```

For each engine it returns:

- the R package;
- the system library, with its name in Debian and in Fedora;
- the way to install it without administrator rights, when there is one;
- the expected dialect;
- the testing status of the engine.

## Compiling the plan before running

Counting the queries does not say whether the engine accepts them. With
`compilar = TRUE` (*compile*), the plan asks the engine to compile each
statement without executing it and without fetching rows, and leaves one
row per statement in `attr(plan, "compilacion")`:

``` r

plan <- plan_perfilado_dbi(conexion, "ventas", compilar = TRUE)
compilacion <- attr(plan, "compilacion")
table(compilacion$estado)
#> 
#> candidata_rechazada           compilada       no_verificada 
#>                   1                  11                  11
unique(compilacion$metodo[compilacion$estado == "compilada"])
#> [1] "EXPLAIN QUERY PLAN"
```

`compilada` means the engine accepted the statement; `no_compilada`,
that it rejected it, and then `motivo` carries its message: a column or
function that does not exist, a missing permission. The error shows up
before a long run, not halfway through. `no_verificada` is what could
not be known, because the shape of the query depends on an earlier
result or on reading rows, and `candidata_rechazada` is the answer of a
capability probe (the deviation is tried as `STDDEV_SAMP` and as
`STDEV`, and SQLite knows only one), not a failure of the plan. How it
compiles depends on the engine: `EXPLAIN` in SQLite, DuckDB and
PostgreSQL, `PREPARE` in MySQL and MariaDB, and `SET NOEXEC ON` in SQL
Server. It does not touch a transaction you have open.

Compiling does not guarantee that the query runs: an error that depends
on the data, such as a `CAST` of a text that is not a number, compiles
and fails on execution, and some engines do not check permissions when
compiling. Nor does it guarantee that nothing is executed: the
preparation queries of the plan run as they do without compiling,
without reading rows of the table.
[`?plan_perfilado_dbi`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
has the full “Compilación” section.
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md)
accepts the same argument. By default, `compilar = FALSE`, and nothing
changes.

## ⏱ For a large table

Before profiling a table of millions of rows, it pays to look at the
plan.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
(*DBI profiling plan*) counts the queries and the work that
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
would do with the same arguments, without reading data. The guide
[Engines and
scale](https://sebollin.github.io/lupa/articles/engines-and-scale.md)
gathers what is needed to decide:

- the cost of each metric and the batches of queries;
- the cost policy;
- the memory budgets;
- the copy of the sample that the session keeps;
- the dialects;
- the guarantee of the primary key depending on the engine;
- the matrix of tested engines.

``` r

DBI::dbDisconnect(conexion)
```

## Summary

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  returns 2 blocks: `resumen_tabla`, computed in the engine, and
  `perfil_muestra`, computed in R on the rows that were read.
- By default the aggregates cover the whole table. The row-level
  profile, also by default, brings every row that fits within the read
  caps.
- `muestra = n` bounds the read into R; `universo = "muestra_motor"`
  samples in the engine; `bloque_muestra = "solo_agregados"` brings no
  rows.
- The sampling of what was read is in
  `perfil_muestra$meta$origen_dbi$muestreo`; the one of the engine, in
  `resumen_tabla$meta$muestreo`, only with `muestra_motor`.
- `centinelas_fecha`, `periodos` and `resumen_fechas = TRUE` count
  sentinel and future dates, periods and recency.
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  counts them in the engine with the same rule as memory; what SQL
  cannot count the same way is declared, or counted in R with
  `estrategia_no_sql = "bloques"`.
- `meta$snapshot = FALSE`: the aggregates are not an instantaneous
  reading.
- `plan_perfilado_dbi(compilar = TRUE)` asks the engine to compile each
  query of the plan without executing it: a syntax, name or permission
  error shows up beforehand.

## 🧭 Next step

A database is not a table:
[Colecciones](https://sebollin.github.io/lupa/articles/colecciones.md)
(*Collections*, in Spanish) profiles several tables at once and looks
for the relationships between them.

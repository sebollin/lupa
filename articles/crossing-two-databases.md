# Crossing two databases

> This guide is the English version of [12. Cruzar dos
> bases](https://sebollin.github.io/lupa/articles/cruzar-dos-bases.md).
> `lupa` writes its messages and tables in Spanish, so the output below
> is in Spanish.

A registry lives in one SQLite database. The visits live in a DuckDB
database. The credential that opens one does not open the other, and no
query can name both at once. How do you know whether every visit finds
its person? And how do you turn that question into a measure that can be
repeated for every delivery?

> **In this guide:**
>
> - profiling each table in its engine with
>   [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md);
> - reading only the keys, in blocks, with
>   [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
>   and
>   [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html);
> - comparing the coverage in R with
>   [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md);
> - turning the comparison into a metric with
>   [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) and
>   `ReglaIntegridadInterEntidad`;
> - deciding what to do when the keys do not have the same type or the
>   same shape.

## Two databases, one question

The example has 8 people in the registry and 20 visits. The visits store
the person as a 2-digit text with a leading zero. Two visits cite people
9 and 10, who are not in the registry. Everything is synthetic.

``` r

library(lupa)

padron <- data.frame(
  id_persona = 1:8,
  sector = rep(c("Norte", "Sur"), each = 4),
  stringsAsFactors = FALSE
)
visitas <- data.frame(
  id_visita = 1:20,
  id_persona = sprintf("%02d", c(rep(1:8, 2), 1, 2, 9, 10)),
  motivo = rep(c("control", "consulta"), length.out = 20),
  stringsAsFactors = FALSE
)
head(visitas, 3)
#>   id_visita id_persona   motivo
#> 1         1         01  control
#> 2         2         02 consulta
#> 3         3         03  control
```

The tables are written to in-memory databases. This only prepares the
example: `lupa` does not write to the database.

``` r

con_padron <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con_padron, "padron", padron)
con_visitas <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
DBI::dbWriteTable(con_visitas, "visitas", visitas)
```

## Profiling each table in its engine

Each table is profiled with the connection that sees it.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
returns the 2 blocks of [Profiling a
database](https://sebollin.github.io/lupa/articles/profiling-a-database.md):
the summary computed by the engine and the profile of the rows that were
read.

``` r

perfil_padron <- perfilar_dbi(con_padron, "padron")
perfil_visitas <- perfilar_dbi(con_visitas, "visitas")
```

For the cross, only one thing in the profile matters: how the key is
stored. The declared type is reported by the engine; the inferred one
comes from the rows that were read.

``` r

clave_de <- function(perfil, columna) {
  fila <- perfil$perfil_muestra$columnas
  fila[
    fila$columna == columna,
    c("columna", "tipo_declarado", "tipo_inferido", "n", "n_distintos")
  ]
}
rbind(
  cbind(tabla = "padron", clave_de(perfil_padron, "id_persona")),
  cbind(tabla = "visitas", clave_de(perfil_visitas, "id_persona"))
)
#>     tabla    columna tipo_declarado tipo_inferido  n n_distintos
#> 1  padron id_persona         entero        entero  8           8
#> 2 visitas id_persona          texto        entero 20          10
```

The registry engine declares `id_persona` as an integer. The visits
engine declares it as text, although the profile, when reading it,
infers an integer. It is not a profile error: it is the first sign that
the key is not stored the same way on both sides. The visits profile
records it as a finding:

``` r

hallazgos(perfil_visitas)[
  hallazgos(perfil_visitas)$columna == "id_persona",
  c("columna", "tipo_hallazgo", "severidad")
]
#>      columna           tipo_hallazgo  severidad
#> 2 id_persona tipo_declarado_distinto sospechoso
```

`tipo_declarado_distinto` does not say that a value is wrong; it says
that the text looks like a number. In the registry, the same column is
an integer. That is where the cross begins to fail.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
does not compare the two tables: each call sees a single connection.

## Reading only the keys

Comparing does not require the whole rows: the keys are enough. And it
pays to read them in blocks, so that the memory peak depends on the
block rather than on the size of the table. The package does not export
a key reader; it is built with
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
and [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html),
the same functions with which
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
reads from the engine.

``` r

leer_por_bloques <- function(conexion, consulta, bloque = 1000) {
  resultado <- DBI::dbSendQuery(conexion, consulta)
  on.exit(DBI::dbClearResult(resultado), add = TRUE)
  partes <- list()
  repeat {
    parte <- DBI::dbFetch(resultado, n = bloque)
    if (!nrow(parte)) break
    partes[[length(partes) + 1L]] <- parte
  }
  if (length(partes)) do.call(rbind, partes) else parte
}
```

Here the block is 10 rows so that the walk is visible; in a real table,
choose the size by the available memory, not by the number of rows. The
visit identifier is read next to the key: it will later serve to locate
the orphan rows.

``` r

claves_padron <- leer_por_bloques(
  con_padron, "SELECT id_persona FROM padron ORDER BY id_persona",
  bloque = 10
)
claves_visitas_crudas <- leer_por_bloques(
  con_visitas, "SELECT id_visita, id_persona FROM visitas ORDER BY id_visita",
  bloque = 10
)
data.frame(
  lado = c("padron", "visitas"),
  clase = c(
    class(claves_padron$id_persona),
    class(claves_visitas_crudas$id_persona)
  ),
  ejemplo = c(
    claves_padron$id_persona[[1]],
    claves_visitas_crudas$id_persona[[1]]
  )
)
#>      lado     clase ejemplo
#> 1  padron   integer       1
#> 2 visitas character      01
```

The registry arrives with 8 integer keys; the visits, with 20 text keys.
The first one in the registry is `1`; the first one in the visits,
`"01"`. To the eye they are the same person; for the comparison, not
yet.

## Comparing in R

[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
compares 2 data frames in memory. The first table is the reference; the
second one depends on it. `columnas_candidatas` says which pair to
compare: without it, the function tries every combination and may find
matches that are only similar codes.

``` r

cruce_crudo <- detectar_relaciones(
  claves_padron, claves_visitas_crudas,
  columnas_candidatas = list("id_persona", "id_persona")
)
cruce_crudo[, c(
  "cardinalidad", "n_valores_comunes", "cobertura_tabla2_en_tabla1",
  "n_filas_huerfanas_tabla2"
)]
#>        cardinalidad n_valores_comunes cobertura_tabla2_en_tabla1 n_filas_huerfanas_tabla2
#> 1 sin_coincidencias                 0                          0                       20
```

The coverage of the visits in the registry is 0 %: none of the 20 visit
keys matches the 8 keys of the registry. It is not that every person is
missing: it is that `"01"` and `1` are not the same value for the
comparison.

## Fixing the shape at the source

The leading zero is padding, not part of the code. It is fixed in the
query, before the value reaches R: this way the key travels with the
type that will be compared and the recipe is written in a single place.
`BIGINT` is DuckDB’s name for the integer; each engine has its own.

``` r

claves_visitas <- leer_por_bloques(
  con_visitas,
  paste(
    "SELECT id_visita, CAST(id_persona AS BIGINT) AS id_persona",
    "FROM visitas ORDER BY id_visita"
  ),
  bloque = 10
)
cruce <- detectar_relaciones(
  claves_padron, claves_visitas,
  columnas_candidatas = list("id_persona", "id_persona")
)
cruce[, c(
  "cardinalidad", "n_valores_comunes", "cobertura_tabla2_en_tabla1",
  "n_filas_huerfanas_tabla2"
)]
#>   cardinalidad n_valores_comunes cobertura_tabla2_en_tabla1 n_filas_huerfanas_tabla2
#> 1          1:m                 8                        0.9                        2
```

Now the coverage is 90 %: 18 of the 20 visits find their person. There
remain 2 orphan visits, the ones of people 9 and 10, who are not in the
registry.

``` r

huerfanas <- cruce$huerfanas_tabla2[[1]]
data.frame(
  total = huerfanas$total,
  mostradas = huerfanas$mostrados,
  estado = huerfanas$estado,
  alcance = huerfanas$alcance
)
#>   total mostradas     estado  alcance
#> 1     2         2 disponible completo
claves_visitas$id_visita[huerfanas$indices_fila]
#> [1] 19 20
```

The trace does not publish the key values; it publishes the row
positions. With `id_visita` read next to the key, those positions
translate to visits 19 and 20. In a large table, the read order has to
be stable for the position to keep working: that is why the query
carries `ORDER BY`.

## From the comparison to the metric

A loose coverage is useful for this delivery. Comparing deliveries
requires a measure with a name, a date and an identifier.
`ReglaIntegridadInterEntidad` measures the proportion of non-missing
keys of the second entity that exist in the first one.

``` r

nucleo <- metricas_nucleo()
regla <- instanciar(
  especializar(
    nucleo$ReglaIntegridadInterEntidad,
    nombre_especifico = "VisitaTienePersona"
  ),
  entidad = c("padron", "visitas"),
  atributos = c("id_persona", "id_persona")
)
medicion <- medir(
  modelo(regla),
  list(padron = claves_padron, visitas = claves_visitas),
  id_medicion = "cruce-2026-01",
  fecha = "2026-01-31"
)
medicion[, c("metrica_especifica", "objeto_medible", "resultado")]
#>   metrica_especifica                          objeto_medible resultado
#> 1 VisitaTienePersona visitas$id_persona -> padron$id_persona       0.9
```

The metric returns a single row: the coverage of the foreign key, with
the rule name, the dependent entity and the object measured. The
`id_medicion` and the `fecha` are what make it possible to compare this
run with the next one. An `NA` in the foreign key does not count as an
orphan: it is not a value that was looked up, so it does not enter the
denominator.

The metric does not normalize anything. If it is passed the raw keys, it
measures 0 %, which is the correct figure for that shape. The correction
is made earlier, in the query, and applied to both sides.

## When the keys do not have the same shape

The visits case was a leading zero. There are 2 more that show up often:
uppercase and edge spaces. The in-memory comparison ignores neither.

``` r

codigos_a <- data.frame(codigo = c("A-01", "B-02", " C-03"))
codigos_b <- data.frame(codigo = c("a-01", "b-02", "c-03"))
comparar <- function(a, b) {
  detectar_relaciones(
    a, b, columnas_candidatas = list("codigo", "codigo")
  )$n_valores_comunes
}
c(
  crudo = comparar(codigos_a, codigos_b),
  normalizado = comparar(
    data.frame(codigo = toupper(trimws(codigos_a$codigo))),
    data.frame(codigo = toupper(trimws(codigos_b$codigo)))
  )
)
#>       crudo normalizado 
#>           0           3
```

Text versus number is not a problem of classes, but of spelling. An
integer and a double with the same integer value match, and a text `"1"`
matches the number `1`; but `"01"`, `"1.0"` or `" 1"` do not. The
comparison uses the exact identity of the value: it neither trims spaces
nor changes case. The same happens with dates: `"2026-01-31"` matches a
`Date`, but `"31/01/2026"` does not.

The decision of which shape is canonical belongs to the data, not to the
package. If the leading zero is padding, remove it on read; if it is
part of the code, restore it on both sides. If case does not
distinguish, normalize both tables with the same function; if it does,
normalizing would be an error. And if the spaces are accidental, trim
them on both sides. What you cannot do is fix only one side and call the
result coverage.

## What the package does not do

It does not join two connections in a single query.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
and
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
receive one connection or one collection;
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
and [`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
compare data frames in R. That is the boundary: each engine profiles and
projects its own side, and R brings the keys together.

If the two tables were in the same database,
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
could resolve the rule in the engine, through the
`sql_dedicado_referencial` path. With two connections there is no query
that sees both, and that is why the recipe brings the keys over. That
bounded read also protects the data: the full rows do not travel, only
the identifiers needed for the comparison.

Neither does it normalize for you: the comparison is exact. And it does
not decide whether a relationship is a real foreign key. Coverage is
evidence; the data dictionary and the engine constraints are what
confirm it. Coverage is computed over every key it is given: there is no
implicit sample that could align with the data. If a subset is read, the
coverage is of that subset and that has to be said.

The two reads are from different moments. If the databases change
between them, the coverage mixes two snapshots: it pays to read the keys
as close together as possible and record the run date, as is done with
the metric.

## Summary

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  describes each table in its engine. The declared type of the key and
  the `tipo_declarado_distinto` finding warn when the two sides are not
  written the same way.
- Reading only the keys and in blocks bounds the memory peak:
  `dbSendQuery()` + `dbFetch(n)` walk the table without bringing the
  full rows.
- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  measures coverage in the 2 directions and names the orphan rows;
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) with
  `ReglaIntegridadInterEntidad` turns it into a measure with a date and
  an identifier.
- The comparison is exact: leading zeros, uppercase and spaces are
  distinct values. Normalizing both sides with the same rule, or fixing
  the type in the query, is part of the recipe.
- The package does not cross two connections in a single query: each
  database is read separately and R is the meeting point.

## 🧭 Next step

To choose the block size and understand which dialects and costs each
engine has, [Engines and
scale](https://sebollin.github.io/lupa/articles/engines-and-scale.md)
gathers the detail.

``` r

DBI::dbDisconnect(con_padron)
DBI::dbDisconnect(con_visitas, shutdown = TRUE)
```

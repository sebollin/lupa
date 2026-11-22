# Tablas planas de un perfil de colección

Un perfil de colección reúne el resumen, la cobertura y, si se
conservaron, los perfiles de cientos de tablas.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) lo baja a
tablas planas —sin columnas-lista, listas para
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html)—, y
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
publica las mismas tablas en su sección de la colección.
[`summary()`](https://rdrr.io/r/base/summary.html) es
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html): sin más
argumentos, el índice.

## Usage

``` r
# S3 method for class 'perfil_coleccion'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  parte = c("tablas", "matriz", "dimensiones", "no_medido", "hallazgos"),
  marco = NULL
)

# S3 method for class 'perfil_coleccion'
summary(object, parte = "tablas", marco = NULL, ...)
```

## Arguments

- x, object:

  Objeto de
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).

- row.names, optional:

  Como en
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

- ...:

  Sin uso: un argumento desconocido —`partes` por `parte`— se rechaza en
  vez de ignorarse.

- parte:

  Qué tabla devolver: `"tablas"` (por omisión), `"matriz"`,
  `"dimensiones"`, `"no_medido"` o `"hallazgos"`.

- marco:

  Taxonomía de
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  contra la que se ubica cada tabla en `"matriz"` y `"dimensiones"`.
  `NULL`, por omisión, usa el marco que
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  usa cuando no recibe uno,
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md);
  las demás partes no lo leen.

## Value

Un `data.frame`.

## Details

Ninguna parte calcula un puntaje: no se promedian factores, dimensiones
ni tablas. La jerarquía dimensión-factor del marco es taxonómica. Los
únicos números son los de cada tabla —los agregados SQL de
`resumen_coleccion` y los hallazgos contados por severidad—, y donde no
hubo medición va `NA`, nunca 0.

## Las partes

- `"tablas"`, el índice: una fila por tabla declarada, primero las
  perfiladas —en el orden en que se midieron— y después las que no.
  Lleva la identidad (`tabla`, `esquema`, `identificador`, `tipo`), el
  `estado` y su `motivo` y `como_resolverlo`, tomados de
  `cobertura_coleccion`: `perfilada`; `vacia`, cero filas;
  `medicion_incompleta`, se perfiló pero no se pudo medir la proporción
  de ausentes de ninguna columna; `con_error`, se declaró como tabla y
  no se pudo perfilar —el motivo conserva la respuesta del motor—; y
  `no_medida`, se declaró como otro tipo de objeto (una vista) y no se
  perfila. `alcances_cobertura` nombra todas las filas de la tabla en
  `cobertura_coleccion`, también las de una tabla perfilada con métricas
  no calculadas o sin muestra. Siguen las columnas de
  `resumen_coleccion`; `n_metricas_sin_listar`, las métricas no
  calculadas de la tabla que `cobertura_metricas` no lista porque se
  recortó a `tope_cobertura_metricas` (0 si están todas; `NA` con
  `cobertura_metricas = "ninguna"`, que no lista nada a propósito); y
  `n_hallazgos_error` y `n_hallazgos_sospechoso`, que son `NA` sin
  perfil conservado, sin muestra leída o en una tabla vacía, donde no
  hubo filas que examinar.

- `"matriz"`: una fila por tabla y una columna por factor del `marco`,
  llamada `"dimensión / factor"`, con el `estado` que
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  da al perfil de la tabla: `medida`, `no_declarada`, `no_aplica` o
  `fuera_de_alcance`. Es la «Cobertura del análisis» de la sección de un
  perfil en
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
  tabla por tabla. Necesita los perfiles: sin
  `conservar_perfiles = TRUE`, y en las tablas que no se perfilaron, la
  celda es `NA`.

- `"dimensiones"`: lo mismo en largo, una fila por tabla y factor, con
  `marco`, `dimension`, `factor`, `estado` y el `motivo` de cada celda
  —el alcance parcial de un factor medido en algunas columnas, por qué
  no aplica, por qué es `NA`—.

- `"no_medido"`: una fila por cosa que no se midió, con el motivo que el
  paquete registró: las filas de `cobertura_coleccion`, las métricas de
  `cobertura_metricas` que no se calcularon y, con perfiles conservados,
  los diagnósticos no evaluados de cada perfil de muestra. `origen`
  nombra el campo de donde sale la fila; `alcance`, `columna`,
  `metrica`, `diagnostico` y `estado` son los de ese campo y `NA` en los
  otros. Con `perfilar_coleccion(cobertura_metricas = "ninguna")`, o con
  la tabla recortada por su tope, faltan métricas: el total real queda
  en `meta$n_metricas_no_medidas`, y cuántas faltan de cada tabla, en
  `n_metricas_sin_listar` del índice.

- `"hallazgos"`: los de
  [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  sobre la colección, una fila por tabla y hallazgo, sin la
  columna-lista `trazabilidad`. Sin perfiles conservados, vacía y con el
  mismo aviso.

## See also

[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md),
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  DBI::dbWriteTable(con, "bajas", data.frame(id = integer()))
  base <- coleccion(con, c("personas", "bajas", "pagos"), nombre = "padron")
  perfil <- perfilar_coleccion(base, conservar_perfiles = TRUE)
  print(summary(perfil)[, c("tabla", "estado", "n_filas")])
  print(as.data.frame(perfil, parte = "matriz")[, 1:6])
  print(as.data.frame(perfil, parte = "no_medido")[, c("tabla", "origen", "alcance")])
  DBI::dbDisconnect(con)
}
#>      tabla    estado n_filas
#> 1 personas perfilada       3
#> 2    bajas     vacia       0
#> 3    pagos con_error      NA
#>      tabla esquema identificador estado_tabla Exactitud / Correctitud semántica
#> 1 personas    <NA>      personas    perfilada                      no_declarada
#> 2    bajas    <NA>         bajas        vacia                      no_declarada
#> 3    pagos    <NA>         pagos    con_error                              <NA>
#>   Exactitud / Correctitud sintáctica
#> 1                       no_declarada
#> 2                       no_declarada
#> 3                               <NA>
#>       tabla                 origen     alcance
#> 1  personas     cobertura_metricas        <NA>
#> 2  personas     cobertura_metricas        <NA>
#> 3  personas     cobertura_metricas        <NA>
#> 4  personas     cobertura_metricas        <NA>
#> 5  personas     cobertura_metricas        <NA>
#> 6  personas     cobertura_metricas        <NA>
#> 7  personas     cobertura_metricas        <NA>
#> 8  personas     cobertura_metricas        <NA>
#> 9  personas     cobertura_metricas        <NA>
#> 10 personas cobertura_diagnosticos        <NA>
#> 11    bajas    cobertura_coleccion tabla_vacia
#> 12    bajas     cobertura_metricas        <NA>
#> 13    bajas     cobertura_metricas        <NA>
#> 14    bajas     cobertura_metricas        <NA>
#> 15    bajas     cobertura_metricas        <NA>
#> 16    bajas     cobertura_metricas        <NA>
#> 17    bajas     cobertura_metricas        <NA>
#> 18    bajas     cobertura_metricas        <NA>
#> 19    bajas     cobertura_metricas        <NA>
#> 20    bajas     cobertura_metricas        <NA>
#> 21    bajas     cobertura_metricas        <NA>
#> 22    bajas     cobertura_metricas        <NA>
#> 23    bajas cobertura_diagnosticos        <NA>
#> 24    pagos    cobertura_coleccion       tabla
```

# Planificar una medición de calidad en una base de datos DBI

Clasifica cada métrica del modelo según la vía con que se medirá en el
motor (`sql_consolidado`, `sql_dedicado`, `sql_dedicado_referencial`,
`bloques_r`, `no_medible`, `no_medible_metodo` o `abstencion`) y estima
las consultas necesarias, sin leer filas de ninguna tabla.

## Usage

``` r
plan_medicion_dbi(
  modelo,
  conexion,
  tablas = NULL,
  aplicabilidad = NULL,
  estrategia_no_sql = c("no_medible", "bloques"),
  bloque_filas = NULL,
  tamano_lote = NULL,
  max_valores_in = 500L,
  max_consultas = Inf,
  dialecto = "auto"
)
```

## Arguments

- modelo:

  Modelo de calidad creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

- conexion:

  Conexión DBI activa o colección creada por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- tablas:

  Mapeo opcional de nombres de entidades del modelo a nombres de tablas
  físicas (vector de texto, lista con nombre o
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html)). Si es `NULL`,
  cada entidad se busca por su nombre (o en
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)).

- aplicabilidad:

  Lista con nombre de fórmulas de un solo lado por columna, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- estrategia_no_sql:

  Cómo resolver las métricas que no se compilan a SQL: `"no_medible"`
  (por omisión, no lee filas) o `"bloques"` (lee por bloques sólo las
  columnas necesarias para las métricas separables por fila).

- bloque_filas:

  Cantidad de filas por bloque cuando `estrategia_no_sql = "bloques"`.
  `NULL` elige el tamaño automático.

- tamano_lote:

  Tope opcional de métricas por consulta consolidada. `NULL` usa el
  límite del motor.

- max_valores_in:

  Tope de valores en listas `IN (...)` compiladas a SQL.

- max_consultas:

  Presupuesto máximo de consultas emitidas durante la ejecución,
  incluidas las consultas de esquema.

- dialecto:

  Dialecto SQL: `"auto"` (por omisión) o uno de `"sqlite"`, `"duckdb"`,
  `"postgresql"`, `"mysql"`, `"mariadb"`, `"sqlserver"`.

## Value

Un `data.frame` de clase `plan_medicion_dbi` con una fila por métrica
instanciada (`metrica_instanciada`, `metrica`, `entidad`, `atributos`,
`via`, `motivo`, `columnas_bloque`) y atributos `motor` y `consultas`
(`esquema`, `consolidadas`, `dedicadas`, `bloques`).

## Details

Sólo consulta el esquema (`SELECT * ... WHERE 1 = 0` y metadatos del
motor). No recibe `proteger_datos_personales` y hereda la protección de
quien lo llama, igual que
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md).

## See also

[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  plan_medicion_dbi(mod, con)
  DBI::dbDisconnect(con)
}
```

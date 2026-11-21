# Medir un modelo de calidad dentro de una base de datos DBI

Evalúa un modelo de calidad directamente en el motor SQL (SQLite,
DuckDB, PostgreSQL actual y 9.3, MySQL, MariaDB y SQL Server verificados
según la política de motores, sus verificaciones de metadatos y sus
guardas) y devuelve un resumen de clase `medicion_resumen` con el mismo
contrato de columnas que `medir(..., detalle = "resumen")`.

## Usage

``` r
medir_dbi(
  modelo,
  conexion,
  tablas = NULL,
  id_medicion = NULL,
  fecha = Sys.time(),
  aplicabilidad = NULL,
  estrategia_no_sql = c("no_medible", "bloques"),
  bloque_filas = NULL,
  tamano_lote = NULL,
  max_valores_in = 500L,
  max_consultas = Inf,
  dialecto = "auto",
  proteger_datos_personales = FALSE,
  columnas_personales = character(),
  validadores_personales = NULL,
  instrumentar = TRUE
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

- id_medicion:

  Identificador de la corrida. Si es `NULL`, se genera uno.

- fecha:

  Fecha de la corrida (UTC).

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

- proteger_datos_personales:

  Si es `TRUE`, protege los mensajes del motor y no agrega la marca
  `datos_personales_sin_proteger`.

- columnas_personales:

  Columnas declaradas como datos personales, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- validadores_personales:

  Validadores adicionales de datos personales, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- instrumentar:

  Si es `TRUE` (por omisión), conserva las consultas SQL y el tiempo
  transcurrido en `attr(resultado, "ejecucion_dbi")`.

## Value

Un `data.frame` de clase `medicion_resumen` (sin heredar de `medicion`)
con las 23 columnas del resumen y los atributos de
`medir(detalle = "resumen")` más `ejecucion_dbi`.

## Details

Lo que el motor no puede reproducir con la semántica exacta de R se
declara `no_medible` en `attr(resultado, "cobertura_metricas")` —nunca
se aproxima ni se interpreta como cero— o, cuando la métrica es
separable por fila y se indica `estrategia_no_sql = "bloques"`, se mide
en R proyectando sólo sus columnas y leyendo por bloques.

En la medición en memoria, `aplicable` y `aplicabilidad` conservan la
evaluación publicada de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md); el
resumen resuelve cada `aplicabilidad` una sola vez, igual que la
medición completa. En la vía DBI, una fórmula traducible sólo puede
nombrar columnas de la entidad y literales escritos en la propia
fórmula. Un símbolo externo se declara `no_medible` sin consultar ni
forzar su entorno.

La marca `datos_personales_sin_proteger` (cuando
`proteger_datos_personales = FALSE`) se determina por nombre de columna
y por `columnas_personales`, sin leer filas de la base.

## See also

[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md),
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  res <- medir_dbi(mod, con)
  tablero_calidad(res)
  DBI::dbDisconnect(con)
}
```

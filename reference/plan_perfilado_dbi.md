# Planificar el costo de `perfilar_dbi()` antes de pagarlo

Emite sólo las consultas-portón —contar filas, leer el esquema y sondear
el dialecto— y devuelve cuántas consultas emitiría el perfilado
completo, de qué clase y con qué alcance sobre la tabla. El plan hace
visible cuántos lotes de agregados se emitirán antes de empezar y evita
una sorpresa de costo.

## Usage

``` r
plan_perfilado_dbi(
  conexion,
  tabla,
  muestra = 1000L,
  orden_muestra = NULL,
  modo = c("exacto", "seguro", "conteos", "muestreado", "aproximado"),
  metricas = NULL,
  max_consultas = Inf,
  dialecto = "auto",
  incluir_valores = TRUE,
  tamano_lote = .TAMANO_LOTE_DBI
)
```

## Arguments

- conexion:

  Conexión abierta compatible con DBI.

- tabla:

  Nombre de tabla o un objeto aceptado por
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).

- muestra:

  Cantidad positiva y finita de filas solicitadas.

- orden_muestra:

  Columnas para `ORDER BY`. La salida solo declara orden reproducible
  cuando la combinación es única en toda la tabla. Sin este argumento,
  DBI no garantiza el orden ni la pertenencia de una muestra limitada, y
  `meta` lo declara expresamente.

- modo:

  Conjunto de métricas del resumen: `"exacto"` las calcula todas,
  `"seguro"` evita las que ordenan o agrupan la tabla completa y
  `"conteos"` deja solo el conteo de valores no nulos, `"muestreado"`
  calcula estimaciones sobre filas elegidas por el motor y
  `"aproximado"` usa funciones nativas aproximadas cuando la sonda las
  acepta.

- metricas:

  Selección explícita de grupos de métricas, que tiene prioridad sobre
  `modo`: `"validos"`, `"distintos"`, `"moda"`, `"basicos"`, `"mediana"`
  y `"desvio"`.

- max_consultas:

  Presupuesto declarado de consultas. Al agotarse, las métricas
  restantes quedan en `no_disponible` con ese motivo.

- dialecto:

  Capacidad de acotar filas: `"auto"` la sondea, y `"limit"`, `"top"`,
  `"fetch_first"`, `"rownum"` o `"portable"` la declaran sin sondeo.

- incluir_valores:

  Si el resumen informa valores de celda: moda, mínimo, máximo y
  mediana. Con `FALSE` esas consultas no se emiten.

- tamano_lote:

  Cantidad máxima de columnas por consulta consolidada. Veinte mantiene
  acotado el número de expresiones y se puede reducir para motores con
  límites más estrictos.

## Value

Data frame de clase `plan_perfilado_dbi` con `clase_consulta`,
`n_consultas` y `alcance`, y los atributos `total`, `columnas`,
`columnas_numericas`, `dialecto`, `consultas_emitidas`, `metricas`,
`filas` y `tamano_lote`.

Cuántas consultas se emiten no dice cuánto cuestan: catorce consultas
sobre dos millones de filas son mucho más trabajo que doscientas sobre
mil. Por eso el plan estima además la magnitud, en los atributos
`filas_leidas` (cuántas filas habría que leer), `ordenaciones_completas`
(cuántas veces habría que ordenar la tabla entera), `magnitud`
—`"baja"`, `"media"`, `"alta"` o `"desconocida"` si no se conoce el
número de filas— y `supuesto_costo`, que dice de dónde sale la cuenta.
El método de impresión avisa cuando la magnitud es alta y nombra las
palancas para acotarla.

## See also

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
  plan_perfilado_dbi(con, "ejemplo")
  DBI::dbDisconnect(con)
}
```

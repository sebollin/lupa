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
  muestra = Inf,
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

  Cantidad positiva de filas solicitadas para el perfil de muestra, o
  `Inf` para traer la tabla entera. Con `Inf` la consulta sale sin
  `LIMIT` y `tabla_completa` queda en `TRUE`.

  El resumen de tabla **no** se muestrea: con `modo = "exacto"` se
  calcula en el motor sobre todas las filas. Lo que sale de esta muestra
  son los diagnosticos que necesitan los valores en R -patrones,
  formatos, casi-duplicados-, y sin `orden_muestra` no son una muestra
  aleatoria sino las primeras filas que devuelva el motor. Medido sobre
  una tabla de 200.000 filas con un defecto plantado al final: con el
  valor por omision aparecen tres hallazgos y con `Inf` aparecen cinco,
  a cambio de 10 segundos en vez de 2. Un analisis de calidad no se
  corre todos los dias; si el tiempo no es la restriccion, `Inf` es la
  opcion honesta.

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
`n_consultas` y `alcance`, y los atributos `total`,
`total_lotes_rechazados`, `columnas`, `columnas_numericas`, `dialecto`,
`consultas_emitidas`, `metricas`, `filas` y `tamano_lote`.

El costo no se declara como un número sino como un rango: `total` es el
extremo inferior, alcanzado si el motor no rechaza ningún lote, y
`total_lotes_rechazados` el superior, alcanzado si los rechaza todos y
cada columna se reintenta sola. El costo real cae entre los dos, y
`attr(plan, "supuesto")` dice por qué se mueve en cada dirección.

Cuántas consultas se emiten no dice cuánto cuestan: catorce consultas
sobre dos millones de filas son mucho más trabajo que doscientas sobre
mil. Por eso el plan estima además la magnitud, y la estima en sus dos
mitades, porque el reloj de una corrida no lo decide siempre el motor.

La del motor va en `filas_leidas` (cuántas filas habría que leer) y
`ordenaciones_completas` (cuántas veces habría que ordenar la tabla
entera), y se resume en `magnitud_motor`. La del cliente va en
`columnas_texto` y `pares_texto` —cuántos pares de formas podría
comparar en R el detector de vocabulario sobre la muestra— y se resume
en `magnitud_texto`. `magnitud` es la mayor de las dos: `"baja"`,
`"media"`, `"alta"`, o `"desconocida"` si no se conoce el número de
filas. `supuesto_costo` dice de dónde sale cada cuenta.

Contar sólo el motor daba juicios falsos con números ciertos: una tabla
de 3.912 filas con una columna de geometría en texto pedía 64.592
lecturas —magnitud `"baja"`— y tardaba 35 segundos, porque el trabajo
estaba en la comparación de formas, que no es una lectura de fila. El
método de impresión muestra las dos mitades, avisa cuando la magnitud es
alta y nombra las palancas para acotarla, que no son las mismas de un
lado que del otro.

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

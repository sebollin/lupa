# Perfilar una muestra leída mediante DBI

Calcula en SQL un resumen sobre la tabla completa o sobre una relación
muestreada por el motor, según `modo`, y en un bloque separado ejecuta
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sobre una muestra traída a memoria. El resumen completo de 105 campos no
se presenta como calculado por la base: esos campos pertenecen
exclusivamente a `perfil_muestra` y su universo es la muestra.

## Usage

``` r
perfilar_dbi(
  conexion,
  tabla,
  muestra = 1000L,
  orden_muestra = NULL,
  modo = c("exacto", "seguro", "conteos", "muestreado", "aproximado"),
  metricas = NULL,
  max_consultas = Inf,
  dialecto = "auto",
  incluir_valores = TRUE,
  tamano_lote = .TAMANO_LOTE_DBI,
  ...
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

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para analizar la muestra.

## Value

Objeto de clase `perfil_dbi` con exactamente dos bloques:
`resumen_tabla`, de alcance completo o muestreado según `modo`, y
`perfil_muestra`, un objeto `perfil` cuyo `meta$origen_dbi` declara
tabla, conexión, SQL y alcance, o `NULL` si la muestra no se pudo
obtener.

## Details

Esta función no escribe en la conexión ni crea objetos temporales. `DBI`
es una dependencia opcional. Cada agregado no disponible queda en `NA` y
su consulta, estado y motivo se conservan en `resumen_tabla$sql`. Las
expresiones se ejecutan como capacidades a comprobar, no como un
dialecto SQL universal.

## Fallo parcial

Ningún bloque descarta al otro. Si el motor rechaza la consulta de
muestra, o si la muestra no se puede perfilar, el resultado sale igual
con `perfil_muestra = NULL` y una fila en `resumen_tabla$cobertura` que
declara el motivo y cómo resolverlo. Si el motor rechaza una columna,
esa columna queda con sus métricas en `no_disponible` y las demás se
perfilan enteras. Los [`stop()`](https://rdrr.io/r/base/stop.html) de
esta vía llevan clase de condición propia —todas heredan de
`lupa_error_dbi`— para que se puedan rescatar con
[`tryCatch()`](https://rdrr.io/r/base/conditions.html):
`lupa_error_conexion_dbi`, `lupa_error_tabla_dbi`,
`lupa_error_campos_dbi`, `lupa_error_conteo_dbi`,
`lupa_error_esquema_dbi` y `lupa_error_argumento_dbi`.

## Muestra y aproximaciones

`modo = "muestreado"` sondea las formas declaradas por el adaptador y
usa `TABLESAMPLE` cuando el motor lo acepta, o una función
pseudoaleatoria con el límite del dialecto. Si ninguna forma es
compatible, las métricas SQL quedan en `no_disponible`: no se sustituyen
por resultados de la tabla completa. Cada registro publica `alcance`,
`universo`, `tamano_muestra`, `fraccion`, `metodo` y `error_esperado`.
Los distintos de una muestra se publican como cardinalidad de la
muestra, no como cardinalidad del universo. `modo = "aproximado"` sondea
`APPROX_COUNT_DISTINCT`, `approx_count_distinct` y las formas de
cuantiles del motor; cuando ninguna responde usa el respaldo exacto y lo
registra por métrica. Las cotas de error no documentadas quedan como
`"desconocido"`.

## Dialecto

Acotar filas no es SQL estándar. En vez de suponer `LIMIT`, la función
sondea el motor con una consulta de cero filas y elige la primera
capacidad que acepte: `LIMIT n [OFFSET k]`, `TOP (n)` y
`OFFSET … FETCH NEXT`, `FETCH FIRST … ROWS ONLY`, o `ROWNUM`. Si ninguna
sirve queda la vía portable, que acota en el cliente con `dbSendQuery()`
y `dbFetch(n)`; en ese caso la mediana exacta se declara no disponible
en vez de traer media tabla a memoria. El alias de subconsulta se
escribe con `AS` o sin él según el motor, y los alias de columna van
comillados y se comparan sin distinguir caja, porque hay motores que los
pliegan a mayúsculas.

## Costo

Los agregados de una tabla ancha se emiten por lotes; `muestra` acota lo
que se trae a R, no el trabajo del motor. `modo`, `metricas`,
`tamano_lote` y `max_consultas` sí lo acotan, y
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
dice cuántas consultas se van a emitir antes de emitirlas. Lo que no
entra en el presupuesto queda en `no_disponible` con su motivo, nunca en
cero.

## Datos personales

`proteger_datos_personales` viaja en `...` hacia
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
vale `TRUE` por omisión. La protección alcanza a los dos bloques: en
`resumen_tabla` se reemplaza la moda de las columnas clasificadas como
personales, se ocultan sus estadísticos de orden y sus momentos, se
sanean los datos de conexión y el SQL guardado no contiene ningún valor
derivado de los datos. La clasificación se toma del perfil de la
muestra; si la muestra no se pudo leer, la protección se aplica a todas
las columnas y `meta` lo declara. `incluir_valores = FALSE` va más
lejos: no emite las consultas de moda ni de mediana y no informa mínimo
ni máximo, útil cuando la tabla es un padrón y la moda de un
identificador único es un documento real.

## See also

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "ejemplo", data.frame(id = 1:10, valor = 11:20))
  resultado <- perfilar_dbi(con, "ejemplo", muestra = 5, orden_muestra = "id")
  DBI::dbDisconnect(con)
}
```

# 10. Perfilar una base

Hay una tabla de ventas en una base de datos, quizá demasiado grande
para la memoria de la sesión. ¿Cómo se decide qué calcula el motor,
cuántas filas llegan a R y dónde queda escrito cómo se eligieron?

> **En esta guía:**
>
> - perfilar una tabla con
>   [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
>   y separar sus 2 bloques, `resumen_tabla` y `perfil_muestra`;
> - comparar qué trae a R cada opción: la llamada por omisión,
>   `muestra = n`, `universo = "muestra_motor"` y
>   `bloque_muestra = "solo_agregados"`;
> - encontrar el metadato del muestreo, que vive en 2 lugares según la
>   opción;
> - leer el resultado con los mismos accesores que un perfil en memoria.

## Una tabla en una base

El ejemplo usa SQLite en memoria, con los paquetes `DBI` y `RSQLite`. La
tabla tiene 1.000 filas para que la guía corra en segundos; las
decisiones son las mismas con millones.
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
sólo prepara el ejemplo.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
emite consultas de lectura: no escribe en la base ni crea tablas
temporales.

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

## Perfilar con los valores por omisión

``` r

completo <- perfilar_dbi(conexion, "ventas")
completo
#> Perfil DBI de ventas
#> Resumen de tabla completa: 4 columnas sobre 1000 filas
#> Clave primaria: no declara clave (fuente `pragma`).
#> Métricas: calculado 45, no_aplica 15
#> Consultas emitidas: 20 (dialecto limit)
#> Derrame estimado: no se pudo estimar (No se pudo estimar el derrame: la conexión no fue
#> reconocida como PostgreSQL y no hay un `pg_stats` portable.).
#> Derrame: no disponible (El controlador no fue reconocido como PostgreSQL; no se puede
#> consultar una estadística de bloques temporales portable.).
#> Perfil de muestra: 1000 filas, reproducible: FALSE
#> Diagnósticos no evaluados sobre la muestra: 3, en `perfil_muestra$cobertura_diagnosticos`
#> No se imprime ningún valor de celda: están en `resumen_tabla$columnas`.
```

La impresión muestra los 2 bloques:

- «Resumen de tabla completa» es `resumen_tabla`: agregados que el motor
  calcula en SQL sobre todas las filas.
- «Perfil de muestra» es `perfil_muestra`:
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  corrido en R sobre las filas que se leyeron.

Las líneas de derrame sólo tienen contenido en PostgreSQL.

Por omisión, `universo = "tabla_completa"` y `muestra = Inf`. Eso quiere
decir:

- los agregados recorren la tabla entera;
- `lupa` trae además a R **todas las filas que entran en los topes de
  lectura**. Los topes son `max_celdas_muestra` (1.000.000 celdas por
  omisión) y `max_bytes_muestra` (512 MiB).

Con esta tabla entran las 1.000 filas. La llamada por omisión no es una
muestra barata: es la tabla completa, si cabe.

## Qué trae a R cada opción

Las otras 3 decisiones se prueban sobre la misma tabla. El resultado
tiene una fila por opción, con:

- cuántas filas describe el resumen SQL;
- cuántas se leyeron a R;
- en cuál de los 2 lugares quedó el metadato del muestreo.

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

Cada fila contesta una pregunta distinta:

- **`muestra = n`** acota la lectura a R, no los agregados: el resumen
  sigue describiendo las 1.000 filas, y el perfil por fila, 200.
- **`universo = "muestra_motor"`** con `muestra_motor = n` cambia el
  universo de los agregados. El motor elige la muestra una sola vez y
  `lupa` la guarda en una copia de la sesión de R. Todo lo demás se
  calcula sobre esas mismas filas: resumen y perfil describen las mismas
  150.
- **`bloque_muestra = "solo_agregados"`** no trae ninguna fila:
  `perfil_muestra` queda en `NULL` y sólo hay conteos y resúmenes
  calculados en el motor. Vale para los 2 universos.

[`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve siempre el conteo de la tabla completa, también con
`muestra_motor`.

## Dónde queda escrito cómo se muestreó

Las 2 últimas columnas de la tabla de arriba dicen si existe cada lugar
donde vive el metadato del muestreo:

- `meta_resumen` mira `resumen_tabla$meta$muestreo`;
- `meta_lectura` mira `perfil_muestra$meta$origen_dbi$muestreo`.

Cada uno existe bajo su propia condición:

| ruta | describe | existe cuando |
|----|----|----|
| `perfil_muestra$meta$origen_dbi$muestreo` | las filas leídas a R | hay `perfil_muestra`, con cualquier universo |
| `resumen_tabla$meta$muestreo` | la muestra elegida por el motor | `universo = "muestra_motor"`; si no, es `NULL` |

Para la lectura a R, el metadato dice:

- cuántas filas se pidieron y cuántas llegaron;
- cuántas tiene la tabla;
- cómo se eligieron;
- si la elección se puede repetir.

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

`reproducible` es `TRUE` porque se declaró `orden_muestra = "id_venta"`
y `lupa` comprobó en la tabla completa que ese orden es único. Sin orden
declarado, un `LIMIT` no tiene orden garantizado. Entonces
`reproducible` vale `FALSE`, como en la llamada por omisión:

``` r

sin_orden <- completo$perfil_muestra$meta$origen_dbi$muestreo
sin_orden$reproducible
#> [1] FALSE
writeLines(strwrap(sin_orden$motivo_reproducibilidad, 88))
#> No se declaró `orden_muestra`; SQL no garantiza el orden de las filas.
```

Para la muestra del motor, el metadato está en el resumen. El método
depende de lo que el motor acepte. `lupa` prueba primero las variantes
de `TABLESAMPLE`. Si ninguna sirve, ordena por una función
pseudoaleatoria con el límite del dialecto, como en SQLite.

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

Ahí `universo` es el tamaño de la tabla de la que salió la muestra y
`fraccion` la parte que se tomó. La muestra pseudoaleatoria cambia en
cada corrida, y `reproducible` lo dice. Cada métrica de
`resumen_tabla$sql` declara además su alcance: el conteo de filas es de
la tabla completa y el resto, de la muestra.

``` r

sql <- en_motor$resumen_tabla$sql
sql[sql$columna == "monto", c("columna", "metrica", "estado", "alcance")][1:6, ]
#>    columna        metrica            estado        alcance
#> 3    monto              n         calculado tabla_completa
#> 33   monto      n_validos observado_muestra        muestra
#> 34   monto    n_faltantes observado_muestra        muestra
#> 35   monto prop_faltantes observado_muestra        muestra
#> 36   monto    n_distintos observado_muestra        muestra
#> 37   monto tasa_distintos observado_muestra        muestra
```

El estado `observado_muestra` avisa que el valor describe la muestra. En
particular, `n_distintos` es la cardinalidad observada en esas 150
filas, no una estimación de la cardinalidad de la tabla.

## Los 2 bloques, uno al lado del otro

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
#> 1  resumen_tabla     el motor  1000                15
#> 2 perfil_muestra            R   200               117
```

El resumen tiene 15 campos analíticos por columna. Son los que SQL puede
calcular sin traer los datos: faltantes, distintos, moda, mínimo,
máximo, media, mediana y desvío, entre otros. El perfil de la muestra
tiene los 117 de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
pero sobre las filas leídas. Por eso `lupa` no los mezcla. Llamar
*perfil* al resumen afirmaría una completitud que no tiene; leer el
perfil como si fuera de la tabla entera, también.

## Leer el resultado con los accesores

Los accesores de [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
funcionan igual sobre el resultado de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve el resumen del motor y
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
los del perfil de la muestra:

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
#> [1] 60
```

El motor contó los 20 faltantes de `monto` en las 1.000 filas. En sus
200 filas, el perfil por fila encontró que:

- `fecha` está guardada como texto aunque contiene fechas
  (`tipo_declarado_distinto`);
- casi cada fila trae una fecha distinta (`alta_cardinalidad`).

[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve una fila por columna y métrica, con la consulta que la produjo,
su estado y su motivo.

Cuando no se pidió la muestra, los accesores no inventan lo que no hay:

- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  devuelve una tabla vacía con un aviso;
- [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  declara el bloque como `no_solicitado`, que no es un fallo.

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

## ⚠️ Lo que el resumen no garantiza

Los agregados son consultas separadas, y la tabla puede cambiar entre
una y otra. El resumen lo declara:

``` r

completo$resumen_tabla$meta$snapshot
#> [1] FALSE
writeLines(strwrap(completo$resumen_tabla$meta$nota_snapshot, 88))
#> No hubo lectura instantánea: cada agregado se midió en su propio momento y la tabla
#> pudo cambiar entre consultas. La cobertura declara las comparaciones incoherentes entre
#> grupos de consistencia distintos.
```

Si `n_validos` y `n_distintos` salen incoherentes y vienen de consultas
distintas,
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
agrega una fila `alcance_distinto` con las 2 sentencias. Es evidencia de
que la tabla cambió durante la corrida, no una acusación contra el
motor.

## Antes de conectar a otro motor

El paquete del motor y la biblioteca del sistema que va debajo los
instala quien usa `lupa`. El error que aparece cuando falta alguno rara
vez dice cuál es.
[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
lo dice antes de conectar:

``` r

requisitos_motor()            # el catálogo entero
requisitos_motor("mariadb")   # qué necesita MariaDB y cómo conseguirlo
```

Por cada motor devuelve:

- el paquete de R;
- la biblioteca del sistema, con su nombre en Debian y en Fedora;
- la forma de instalarla sin permisos de administrador, cuando existe;
- el dialecto esperado;
- el estado de prueba del motor.

## ⏱ Para una tabla grande

Antes de perfilar una tabla de millones de filas conviene mirar el plan.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
cuenta las consultas y el trabajo que haría
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
con los mismos argumentos, sin leer datos. El artículo [Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html)
reúne lo que hace falta para decidir:

- el costo de cada métrica y los lotes de consultas;
- la política de costo;
- los presupuestos de memoria;
- la copia de la muestra que guarda la sesión;
- los dialectos;
- la garantía de la clave primaria según el motor;
- la matriz de motores probados.

``` r

DBI::dbDisconnect(conexion)
```

## Resumen

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  devuelve 2 bloques: `resumen_tabla`, calculado en el motor, y
  `perfil_muestra`, calculado en R sobre las filas leídas.
- Por omisión los agregados cubren toda la tabla. El perfil por fila,
  también por omisión, trae todas las filas que entran en los topes de
  lectura.
- `muestra = n` acota la lectura a R; `universo = "muestra_motor"`
  muestrea en el motor; `bloque_muestra = "solo_agregados"` no trae
  filas.
- El muestreo de lo leído está en
  `perfil_muestra$meta$origen_dbi$muestreo`; el del motor, en
  `resumen_tabla$meta$muestreo`, sólo con `muestra_motor`.
- `meta$snapshot = FALSE`: los agregados no son una lectura instantánea.

## 🧭 Paso siguiente

Una base no es una tabla:
[Colecciones](https://sebollin.github.io/lupa/articles/colecciones.md)
perfila varias tablas a la vez y busca las relaciones entre ellas.

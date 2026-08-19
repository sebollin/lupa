# Perfilar una base

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
mantiene separados dos resultados porque provienen de universos
distintos:

- `resumen_tabla` calcula agregados SQL sobre la tabla completa;
- `perfil_muestra` ejecuta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las filas traídas a memoria.

Mezclarlos en una fila, aun con un campo de alcance por resultado,
permitiría comparar cantidades como si pertenecieran al mismo perfil.
Los dos bloques impiden esa lectura.

## Un ejemplo en memoria

Para ejecutar el bloque se necesitan los paquetes opcionales `DBI` y
`RSQLite`, ambos declarados en `Suggests`. Si falta cualquiera de ellos,
knitr omite el bloque y la viñeta se construye sin error.

La llamada a
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
crea la base de demostración en memoria. Pertenece a la preparación del
ejemplo;
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
no escribe en la base, no crea tablas temporales y sólo emite consultas
de lectura.

``` r

library(lupa)

datos_base <- data.frame(
  id = 1:12,
  grupo = rep(c("A", "B", "C"), each = 4),
  valor = c(10, 12, NA, 14, 20, 22, 24, 26, 30, 32, 34, 36),
  fecha = as.character(as.Date("2026-01-01") + 0:11),
  stringsAsFactors = FALSE
)
datos_base
#>    id grupo valor      fecha
#> 1   1     A    10 2026-01-01
#> 2   2     A    12 2026-01-02
#> 3   3     A    NA 2026-01-03
#> 4   4     A    14 2026-01-04
#> 5   5     B    20 2026-01-05
#> 6   6     B    22 2026-01-06
#> 7   7     B    24 2026-01-07
#> 8   8     B    26 2026-01-08
#> 9   9     C    30 2026-01-09
#> 10 10     C    32 2026-01-10
#> 11 11     C    34 2026-01-11
#> 12 12     C    36 2026-01-12

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
creada <- DBI::dbWriteTable(conexion, "entregas", datos_base)

sin_orden <- perfilar_dbi(
  conexion, "entregas", muestra = 5
)
perfil <- perfilar_dbi(
  conexion, "entregas", muestra = 5, orden_muestra = "id"
)
```

La conexión y los dos perfiles ya están preparados. Los bloques
siguientes consultan ese mismo estado: partir la explicación no implica
recrear la base ni volver a medirla.

## Los dos bloques del resultado

`names(perfil)` hace visible la separación que devuelve
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
La tabla siguiente compara el alcance de cada bloque: las filas de
origen y la cantidad de campos analíticos, sin presentarlos como un
único perfil.

``` r


names(perfil)
#> [1] "resumen_tabla"  "perfil_muestra"

data.frame(
  bloque = c("resumen_tabla", "perfil_muestra"),
  filas_fuente = c(
    perfil$resumen_tabla$meta$filas,
    perfil$perfil_muestra$meta$filas_analizadas
  ),
  campos_analiticos = c(
    ncol(perfil$resumen_tabla$columnas) - 1L,
    ncol(perfil$perfil_muestra$columnas) - 1L
  ),
  columnas_del_resultado = c(
    ncol(perfil$resumen_tabla$columnas),
    ncol(perfil$perfil_muestra$columnas)
  )
)
#>           bloque filas_fuente campos_analiticos columnas_del_resultado
#> 1  resumen_tabla           12                15                     16
#> 2 perfil_muestra            5                98                     99
```

## `resumen_tabla`: agregados sobre la tabla completa

`resumen_tabla$columnas` contiene quince campos analíticos, además del
nombre de la columna. Son los conteos y agregados que esta interfaz
puede calcular en SQL sin traer la tabla: faltantes, distintos, moda y
resúmenes cuantitativos, entre otros. La salida se calcula sobre las
doce filas completas de `entregas`.

``` r


perfil$resumen_tabla$columnas[, c(
  "columna", "n", "n_faltantes", "n_distintos", "minimo", "maximo", "media"
)]
#>   columna  n n_faltantes n_distintos minimo maximo    media
#> 1      id 12           0          12      1     12  6.50000
#> 2   grupo 12           0           3     NA     NA       NA
#> 3   valor 12           1          11     10     36 23.63636
#> 4   fecha 12           0          12     NA     NA       NA
```

`perfil_muestra$columnas` contiene los 99 campos del perfil, pero su
universo son cinco filas en este ejemplo. Llamar *perfil* al resumen SQL
afirmaría una completitud que no tiene: el resumen cubre quince de esos
aspectos sobre doce filas; el perfil cubre sus 99 campos sobre las cinco
filas obtenidas. Las consultas, estados y motivos de los agregados SQL
quedan en `resumen_tabla$sql` para que también se vea qué aceptó o
rechazó el motor.

## `perfil_muestra`: el perfil completo de la muestra

El perfil completo se obtiene en memoria sobre las cinco filas
seleccionadas. La salida muestra algunos de sus 99 campos para que se
vean, junto con los conteos, los faltantes, los distintos y el tipo
inferido.

``` r


perfil$perfil_muestra$columnas[, c(
  "columna", "n", "n_faltantes", "n_distintos", "tipo_inferido"
)]
#>   columna n n_faltantes n_distintos tipo_inferido
#> 1      id 5           0           5        entero
#> 2   grupo 5           0           2         texto
#> 3   valor 5           1           4         doble
#> 4   fecha 5           0           5         fecha
```

Los hallazgos de `perfil_muestra` conservan sus unidades declaradas. En
particular, las inconsistencias de mayúsculas, las equivalencias Unicode
y `casi_duplicados_vocabulario` cuentan valores distintos, pero su
trazabilidad enumera por fila todas las filas que contienen esos
valores. En el último caso incluye las formas seleccionadas por el
grupo, incluida la dominante. Por eso la traza puede tener más filas que
`n_afectados`; no es una contradicción de alcance, sino el localizador
del grupo que se debe revisar. El orden de la traza prioriza las formas
no dominantes antes de las dominantes, para que `max_filas_hallazgo`
conserve primero las filas accionables; la evidencia declara cuántas
filas mostradas pertenecen a cada forma. En `patron_raro`, la evidencia
agrega la proporción del patrón dominante y las filas en patrones no
dominantes excluidos por superar `umbral_patron_raro`; si el dominante
no alcanza `umbral_patron_dominante`, la no medición queda en
`cobertura_diagnosticos`.

## El muestreo declarado

`muestra = 5` declara las filas pedidas. La metadata registra además las
filas obtenidas, las doce filas totales, el método y el orden. Sin
`orden_muestra`, un `LIMIT 5` no tiene orden garantizado y
`reproducible` es `FALSE`.

Con `orden_muestra = "id"`, `lupa` verifica en la tabla completa que el
orden sea único. Sólo entonces informa `reproducible = TRUE`. Indicar
una columna que se repite no alcanza: el orden puede quedar declarado y
aun así no determinar qué filas ocupan el límite.

``` r

resumir_muestreo <- function(x, caso) {
  m <- x$perfil_muestra$meta$origen_dbi$muestreo
  data.frame(
    caso = caso,
    filas_pedidas = m$filas_solicitadas,
    filas_obtenidas = m$filas_obtenidas,
    filas_totales = m$filas_totales_fuente,
    metodo = m$metodo,
    orden = if (length(m$orden_muestra)) {
      paste(m$orden_muestra, collapse = ", ")
    } else {
      "no declarado"
    },
    orden_unico_verificado = m$orden_unico_verificado,
    reproducible = m$reproducible,
    stringsAsFactors = FALSE
  )
}

rbind(
  resumir_muestreo(sin_orden, "sin orden"),
  resumir_muestreo(perfil, "orden por id")
)
#>           caso filas_pedidas filas_obtenidas filas_totales
#> 1    sin orden             5               5            12
#> 2 orden por id             5               5            12
#>                                 metodo        orden orden_unico_verificado reproducible
#> 1 primeras_filas_sin_orden_garantizado no declarado                  FALSE        FALSE
#> 2           primeras_filas_segun_orden           id                   TRUE         TRUE

data.frame(
  bloque = c("resumen_tabla", "perfil_muestra"),
  solo_lectura = c(
    perfil$resumen_tabla$meta$solo_lectura,
    perfil$perfil_muestra$meta$origen_dbi$solo_lectura
  ),
  objetos_temporales = c(
    perfil$resumen_tabla$meta$objetos_temporales,
    perfil$perfil_muestra$meta$origen_dbi$objetos_temporales
  )
)
#>           bloque solo_lectura objetos_temporales
#> 1  resumen_tabla         TRUE              FALSE
#> 2 perfil_muestra         TRUE              FALSE

invisible(DBI::dbDisconnect(conexion))
```

La metadata no convierte una muestra en la tabla completa. Permite que
quien consume el resultado compare cada bloque dentro de su propio
alcance.

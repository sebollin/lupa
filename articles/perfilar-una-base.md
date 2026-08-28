# Perfilar una base

Por omisión,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
mantiene separados dos resultados porque provienen de universos
distintos:

- `resumen_tabla` calcula agregados SQL sobre la tabla completa;
- `perfil_muestra` ejecuta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las filas traídas a memoria.

El resumen SQL usa `modo = "exacto"` por omisión. Para tablas grandes se
puede pedir `modo = "muestreado"`: las métricas se calculan sobre una
relación muestreada por el motor, no sobre una tabla traída a R. La
capacidad se sondea antes de medir; PostgreSQL y SQL Server pueden
resolverla con `TABLESAMPLE`, y otros motores con una función
pseudoaleatoria y el límite de su dialecto. Si ninguna forma es
aceptada, las métricas quedan en `NA` y `resumen_tabla$cobertura`
conserva el motivo.

Cada registro de `resumen_tabla$sql` declara `alcance`, `universo`,
`tamano_muestra`, `fraccion`, `metodo` y `error_esperado`. En el modo
muestreado, `n_distintos` se conserva como cardinalidad observada en la
muestra, no como una estimación de la cardinalidad de la tabla completa.
`modo = "aproximado"` sondea las funciones nativas de cardinalidad y
cuantiles; si una no existe, se usa el respaldo exacto y el método queda
registrado.

Además, `id_muestra` identifica la consulta de datos que produjo cada
métrica. Dos métricas con el mismo valor vieron exactamente las mismas
filas y se pueden comparar directamente, sin cruzar `lote` ni
`columnas_compartidas`. Un `NA` declara que esa garantía no se puede
hacer; en particular, moda, frecuencia de la moda y mediana son métricas
por columna y no comparten filas con otras.

Mezclarlos en una fila, aun con un campo de alcance por resultado,
permitiría comparar cantidades como si pertenecieran al mismo perfil.
Los dos bloques impiden esa lectura.

Si sólo se necesitan los agregados del motor,
`bloque_muestra = "solo_agregados"` evita traer filas a R y deja
`perfil_muestra = NULL`. La cobertura lo declara como `no_solicitado`,
que no es un fallo. La lectura habitual se conserva con
`bloque_muestra = "con_muestra"`, que es el valor por omisión.

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
solo_agregados <- perfilar_dbi(
  conexion, "entregas", modo = "conteos", bloque_muestra = "solo_agregados"
)
```

La conexión y los dos perfiles ya están preparados. Los bloques
siguientes consultan ese mismo estado: partir la explicación no implica
recrear la base ni volver a medirla.

## Antes de conectarse: qué hace falta para cada motor

`lupa` tiene dos dependencias obligatorias, `cli` y `data.table`. El
paquete del motor y la biblioteca del sistema que va debajo son cosa de
quien instala, y el error que aparece cuando falta alguna rara vez dice
cuál es.

``` r

requisitos_motor()            # el catálogo entero
requisitos_motor("mariadb")   # qué necesita MariaDB, y cómo conseguirlo
```

Por cada motor devuelve el paquete de R, la biblioteca del sistema con
su nombre en Debian y en Fedora, **la salida sin permisos de
administrador** cuando existe, el dialecto esperado y si está probado
contra motor real o sólo esperado.

Eso último importa y conviene leerlo con cuidado: la columna dice si
**`lupa`** se probó contra ese motor, no si tu instalación funciona. Y
la comprobación de la biblioteca del sistema es honesta sobre su propio
límite — cuando sólo puede decir «el paquete de R no está, y si al
instalarlo falla la compilación lo que falta es esto», dice eso, porque
comprobar de verdad exigiría intentar compilar.

La clave primaria que se lee después de esa conexión conserva dos datos
distintos: `fuente` dice qué catálogo respondió y `garantia` dice si el
estado consultado alcanza para confiar en el motor. Oracle sólo queda
garantizado con `STATUS = ENABLED` y `VALIDATED`; PostgreSQL consulta
`enforced` y la validación, y MySQL `enforced`. MariaDB, SQL Server,
SQLite y DuckDB no ofrecen en esta lectura un estado comparable, así que
allí una clave visible queda con garantía desconocida. No hay que leer
una entrada del catálogo como prueba de los datos existentes cuando el
estado no fue consultable.

## Leer el resultado sin memorizar su forma

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
devuelve un `perfil` plano y
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
un contenedor con dos componentes: el segundo puede ser `NULL` si la
muestra no se pidió o no estuvo disponible. Es una diferencia real —una
tabla en memoria y una tabla remota no son lo mismo— pero convertía la
lectura en un acertijo: `perfil$general$filas` funciona sobre el primero
y devuelve `NULL` sobre el segundo.

``` r

library(lupa)
en_memoria <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
n_filas(en_memoria)
#> [1] 13
nrow(hallazgos(en_memoria))
#> [1] 23
nrow(cobertura(en_memoria))
#> [1] 2
sql_perfil(en_memoria)   # NULL: un perfil en memoria no emitió SQL
#> NULL
```

Los mismos cinco nombres sirven para las cuatro formas de salida. Y no
inventan lo que no hay: un perfil DBI sin muestra leída, o con la
muestra no solicitada, devuelve una tabla de hallazgos vacía **con su
aviso**, no una tabla que aparente que se midió y no había nada.

## Los dos bloques posibles del resultado

`names(perfil)` hace visible la separación que devuelve
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
La tabla siguiente compara el alcance de cada bloque: las filas de
origen y la cantidad de campos analíticos, sin presentarlos como un
único perfil.

``` r


names(perfil)
#> [1] "resumen_tabla"  "perfil_muestra"
solo_agregados$perfil_muestra
#> NULL
solo_agregados$resumen_tabla$cobertura[
  solo_agregados$resumen_tabla$cobertura$bloque == "perfil_muestra", ,
  drop = FALSE
]
#>           bloque elemento        estado
#> 1 perfil_muestra entregas no_solicitado
#>                                                                                                        motivo
#> 1 No se solicito el perfil de muestra: este resultado contiene unicamente los agregados SQL del modo elegido.
#>                                                                                                         como_resolverlo
#> 1 Para obtener diagnosticos sobre valores y hallazgos por fila, volver a perfilar con `bloque_muestra = "con_muestra"`.
#>    sql
#> 1 <NA>

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
#> 2 perfil_muestra            5               109                    110
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

`perfil_muestra$columnas` trae 109 campos analíticos además del nombre
de la columna, pero su universo son cinco filas en este ejemplo. Llamar
*perfil* al resumen SQL afirmaría una completitud que no tiene: el
resumen cubre quince de esos aspectos sobre doce filas; el perfil cubre
sus 109 sobre las cinco filas obtenidas. Las consultas, estados y
motivos de los agregados SQL quedan en `resumen_tabla$sql` para que
también se vea qué aceptó o rechazó el motor.

La instrumentación queda activa por omisión y sólo agrega metadatos.
Cada fila de `resumen_tabla$sql` conserva la métrica que ya representaba
y suma `duracion_ms`, `cpu_ms`, `n_filas_resultado`,
`bytes_resultado_r`, `consulta_id` e `id_muestra`, además de `etapa`. La
duración incluye la ejecución y la lectura del resultado que DBI entregó
a R; el tamaño es
[`object.size()`](https://rdrr.io/r/utils/object.size.html) de ese
resultado. `cpu_ms` es la suma de `user.self` y `sys.self` de
[`proc.time()`](https://rdrr.io/r/base/proc.time.html): cerca de cero
indica espera y cerca de uno al dividirlo por `duracion_ms` indica
trabajo del cliente. Las filas que no emitieron consulta dejan esos
campos en `NA`, que significa «no medido», no `0`.

``` r

perfil$resumen_tabla$sql[, c(
  "metrica", "estado", "duracion_ms", "cpu_ms", "n_filas_resultado",
  "bytes_resultado_r", "consulta_id", "id_muestra", "etapa"
)]
#>            metrica    estado duracion_ms cpu_ms n_filas_resultado bytes_resultado_r
#> 1                n calculado   0.3976822      1                 1              1368
#> 2                n calculado   0.3976822      1                 1              1368
#> 3                n calculado   0.3976822      1                 1              1368
#> 4                n calculado   0.3976822      1                 1              1368
#> 5        n_validos calculado   0.4227161      0                 1              3040
#> 6      n_faltantes calculado   0.4227161      0                 1              3040
#> 7   prop_faltantes calculado   0.4227161      0                 1              3040
#> 8      n_distintos calculado   0.3976822      1                 1              1368
#> 9   tasa_distintos calculado   0.3976822      1                 1              1368
#> 10            moda calculado   0.3981590      0                 1               872
#> 11 frecuencia_moda calculado   0.3981590      0                 1               872
#> 12          minimo calculado   0.4227161      0                 1              3040
#> 13          maximo calculado   0.4227161      0                 1              3040
#> 14           media calculado   0.4227161      0                 1              3040
#> 15         n_ceros calculado   0.4227161      0                 1              3040
#> 16     n_negativos calculado   0.4227161      0                 1              3040
#> 17         mediana calculado   0.3821850      0                 1               736
#> 18          desvio calculado   0.4227161      0                 1              3040
#> 19       n_validos calculado   0.4227161      0                 1              3040
#> 20     n_faltantes calculado   0.4227161      0                 1              3040
#> 21  prop_faltantes calculado   0.4227161      0                 1              3040
#> 22     n_distintos calculado   0.3976822      1                 1              1368
#> 23  tasa_distintos calculado   0.3976822      1                 1              1368
#> 24            moda calculado   0.4055500      0                 1               928
#> 25 frecuencia_moda calculado   0.4055500      0                 1               928
#> 26          minimo no_aplica          NA     NA                NA                NA
#> 27          maximo no_aplica          NA     NA                NA                NA
#> 28           media no_aplica          NA     NA                NA                NA
#> 29         n_ceros no_aplica          NA     NA                NA                NA
#> 30     n_negativos no_aplica          NA     NA                NA                NA
#> 31         mediana no_aplica          NA     NA                NA                NA
#> 32          desvio no_aplica          NA     NA                NA                NA
#> 33       n_validos calculado   0.4227161      0                 1              3040
#> 34     n_faltantes calculado   0.4227161      0                 1              3040
#> 35  prop_faltantes calculado   0.4227161      0                 1              3040
#> 36     n_distintos calculado   0.3976822      1                 1              1368
#> 37  tasa_distintos calculado   0.3976822      1                 1              1368
#> 38            moda calculado   0.3776550      1                 1               872
#> 39 frecuencia_moda calculado   0.3776550      1                 1               872
#> 40          minimo calculado   0.4227161      0                 1              3040
#> 41          maximo calculado   0.4227161      0                 1              3040
#> 42           media calculado   0.4227161      0                 1              3040
#> 43         n_ceros calculado   0.4227161      0                 1              3040
#> 44     n_negativos calculado   0.4227161      0                 1              3040
#> 45         mediana calculado   0.3960133      0                 1               736
#> 46          desvio calculado   0.4227161      0                 1              3040
#> 47       n_validos calculado   0.4227161      0                 1              3040
#> 48     n_faltantes calculado   0.4227161      0                 1              3040
#> 49  prop_faltantes calculado   0.4227161      0                 1              3040
#> 50     n_distintos calculado   0.3976822      1                 1              1368
#> 51  tasa_distintos calculado   0.3976822      1                 1              1368
#> 52            moda calculado   0.3991127      0                 1               936
#> 53 frecuencia_moda calculado   0.3991127      0                 1               936
#> 54          minimo no_aplica          NA     NA                NA                NA
#> 55          maximo no_aplica          NA     NA                NA                NA
#> 56           media no_aplica          NA     NA                NA                NA
#> 57         n_ceros no_aplica          NA     NA                NA                NA
#> 58     n_negativos no_aplica          NA     NA                NA                NA
#> 59         mediana no_aplica          NA     NA                NA                NA
#> 60          desvio no_aplica          NA     NA                NA                NA
#>    consulta_id id_muestra       etapa
#> 1            3          3     conteos
#> 2            3          3     conteos
#> 3            3          3     conteos
#> 4            3          3     conteos
#> 5            6          6     basicos
#> 6            6          6     basicos
#> 7            6          6     basicos
#> 8            3          3     conteos
#> 9            3          3     conteos
#> 10           7         NA        moda
#> 11           7         NA        moda
#> 12           6          6     basicos
#> 13           6          6     basicos
#> 14           6          6     basicos
#> 15           6          6     basicos
#> 16           6          6     basicos
#> 17           8         NA     mediana
#> 18           6          6     basicos
#> 19           6          6     basicos
#> 20           6          6     basicos
#> 21           6          6     basicos
#> 22           3          3     conteos
#> 23           3          3     conteos
#> 24           9         NA        moda
#> 25           9         NA        moda
#> 26          NA         NA resumen_sql
#> 27          NA         NA resumen_sql
#> 28          NA         NA resumen_sql
#> 29          NA         NA resumen_sql
#> 30          NA         NA resumen_sql
#> 31          NA         NA resumen_sql
#> 32          NA         NA resumen_sql
#> 33           6          6     basicos
#> 34           6          6     basicos
#> 35           6          6     basicos
#> 36           3          3     conteos
#> 37           3          3     conteos
#> 38          10         NA        moda
#> 39          10         NA        moda
#> 40           6          6     basicos
#> 41           6          6     basicos
#> 42           6          6     basicos
#> 43           6          6     basicos
#> 44           6          6     basicos
#> 45          11         NA     mediana
#> 46           6          6     basicos
#> 47           6          6     basicos
#> 48           6          6     basicos
#> 49           6          6     basicos
#> 50           3          3     conteos
#> 51           3          3     conteos
#> 52          12         NA        moda
#> 53          12         NA        moda
#> 54          NA         NA resumen_sql
#> 55          NA         NA resumen_sql
#> 56          NA         NA resumen_sql
#> 57          NA         NA resumen_sql
#> 58          NA         NA resumen_sql
#> 59          NA         NA resumen_sql
#> 60          NA         NA resumen_sql

perfil$resumen_tabla$tiempos
#>                         etapa duracion_ms cpu_ms        estado nivel n_ejecuciones
#> 1        ausencia_estructural   0.6446838      2        medido     2             1
#> 2 casi_duplicados_vocabulario   7.9023838     14        medido     2             1
#> 3                dependencias   0.9856224      1        medido     2             1
#> 4      duplicados_aproximados          NA     NA no_solicitado     2             1
#> 5             lectura_muestra   0.5440712      0        medido     1             1
#> 6          perfilado_columnas 285.5246067    285        medido     2             1
#> 7           perfilado_muestra 369.8031902    381        medido     1             1
```

`resumen_tabla$tiempos` reúne en milisegundos las etapas grandes del
lado R, incluidas la lectura y el perfilado de la muestra, el perfilado
por columna y los análisis opcionales. La columna `nivel` dice cuáles se
pueden sumar: las de `nivel = 1` son disjuntas entre sí y las de nivel
mayor están contenidas en alguna de ellas. `perfilado_muestra` es
inclusivo —contiene el perfilado por columna, las dependencias y los
casi-duplicados—, así que sumar la columna entera da más que la corrida
completa. `instrumentar = FALSE` conserva el mismo plan, la misma
cantidad y el mismo orden de consultas, pero deja las duraciones, el CPU
y los metadatos de tamaño en `NA` y marca las etapas como `no_medido`.

## `perfil_muestra`: el perfil completo de la muestra

El perfil completo se obtiene en memoria sobre las cinco filas
seleccionadas. La salida muestra algunos de sus 109 campos analíticos
para que se vean, junto con los conteos, los faltantes, los distintos y
el tipo inferido.

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

Algunos controladores no saben traer ciertas columnas en una lectura
corriente —`TEXT` y `NTEXT` en SQL Server, `CLOB` y `BLOB` en Oracle,
`bytea` en PostgreSQL— y responden con un error al pedirlas junto con el
resto. Una sola columna así se llevaba puesta la muestra entera. Ahora,
cuando la lectura falla, `lupa` **averigua cuáles no se pueden leer y
reintenta sin ellas**: la muestra vuelve con las columnas que sí se
pudieron leer, y `resumen_tabla$cobertura` gana una fila
`alcance_distinto` que nombra las que quedaron afuera y conserva el
motivo textual del motor.

Averiguarlo, y no deducirlo, es lo que hace que esto funcione contra un
controlador cualquiera. La versión anterior miraba el tipo que declara
el motor y lo comparaba contra una lista de nombres. Contra el driver
`{SQL Server}` eso no reconocía **ninguna** columna, porque ese driver
no informa nombres sino códigos numéricos de ODBC —`"-1"` en vez de
`"varchar(max)"`—, y la muestra se perdía igual, en silencio. Ahora el
reintento **le pregunta al motor**: divide el conjunto de columnas y va
podando los subconjuntos que sí se leen, hasta aislar las que fallan. El
nombre del tipo sigue sirviendo de atajo —si se reconoce, se ahorra el
descarte—, pero ya no decide.

Eso cambia también lo que se puede afirmar. Cuando la omisión se
resolvió por descarte, `omision_comprobada` es `TRUE` y el aviso lo
dice: cada una de esas columnas falló por sí sola y el resto se leyó
junto. Cuando la resolvió el atajo, es `FALSE` y el aviso mantiene el
“no se comprobó que sean la causa”, porque el reintento salta ante
cualquier fallo y un corte de red que se recupera en el segundo intento
daría el mismo camino. `sondas_descarte` publica cuántas consultas costó
averiguarlo. El descarte está acotado: como mucho dos sondas por
columna, y nunca más de la mitad del saldo de `max_consultas`.

Si el descarte no aísla nada, no se inventa una culpable: la muestra se
declara no disponible y el motivo dice cuántos subconjuntos se probaron
y cómo terminó. En todos los casos el resumen por columna las cubre
igual, porque los agregados se calculan en el motor; lo que falta es su
perfil por fila. Para incluirlas, conviene convertirlas a texto acotado
en una vista y perfilar la vista.

El bloque de metadatos del muestreo
—`perfil_muestra$meta$origen_dbi$muestreo`— describe lo que **de
verdad** se leyó y no lo que se había planeado leer: `columnas_leidas`
son las columnas que la muestra trae, `sql_muestra` es la consulta que
se emitió, `metodo` y `acotado_en` describen la lectura que salió —no la
que se había planeado, que el reintento puede cambiar—, y
`columnas_omitidas` con `motivo_columnas_omitidas` dicen cuáles quedaron
afuera y qué respondió el motor.

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

## Muestras y aproximaciones para tablas grandes

La muestra usada por `perfil_muestra` y la relación usada por
`modo = "muestreado"` son declaradas por separado. La primera se trae a
R para ejecutar el perfil completo; la segunda permanece en SQL para que
los agregados no recorran la tabla completa. La metadata de ambos
bloques publica el universo y el método efectivo.

``` r

estimado <- perfilar_dbi(
  conexion, "entregas", muestra = 5000,
  modo = "muestreado", metricas = c(
    "validos", "distintos", "moda", "basicos", "mediana", "desvio"
  )
)

estimado$resumen_tabla$sql[
  estimado$resumen_tabla$sql$estado %in% c("estimado", "observado_muestra"),
  c("columna", "metrica", "estado", "universo", "tamano_muestra",
    "metodo", "fraccion", "error_esperado")
]

aproximado <- perfilar_dbi(
  conexion, "entregas", muestra = 5000, modo = "aproximado"
)
aproximado$resumen_tabla$meta$aproximaciones
```

El modo aproximado sólo consolida un conteo de distintos cuando la
capacidad del motor entrega una expresión incrustable. Si la capacidad
sólo construye una consulta completa, el conteo de válidos y el de
distintos se emiten por separado. El método publicado sigue al SQL que
efectivamente se ejecutó; una consulta no emitida o sin un valor
utilizable queda `no_disponible`.

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
emite las sondas de capacidad y predice el total que costará la corrida.
Publica un **rango**, y lo declara en `attr(plan, "supuesto")`: `total`
si no se rechaza ningún lote y `total_lotes_rechazados` si se rechazan
todos; el costo real cae entre los dos. El extremo inferior cuenta una
consulta por lote de agregados planos y mantiene `COUNT(DISTINCT ...)`
en su clase separada; la moda sigue siendo una por columna y la mediana
conserva ese camino como respaldo. Cuando la sonda acepta
`PERCENTILE_CONT(...) WITHIN GROUP`, varias medianas viajan en un solo
`SELECT` por lote. Una columna sin un solo valor válido no emite mediana
ni desvío, y el plan no puede saber cuáles están vacías sin preguntarlo.
El extremo superior suma hasta `2n - 1` sondas adicionales por cada lote
rechazado de `n` columnas: la bisección aísla las culpables y reutiliza
los grupos aceptados. Para decidir si una corrida es viable, saber entre
qué y qué se mueve alcanza. La predicción incluye las sondas aunque una
forma acertada aparezca antes que las demás, porque el costo declarado
no puede depender del motor.

El plan paga un `COUNT(*)` exacto propio antes de los agregados, porque
necesita el total para estimar el trabajo. La corrida lleva ese total en
su primera consulta de agregados; si el lote es rechazado, se repliega a
un `COUNT(*)` solo y continúa con la bisección. Así la completitud sigue
siendo medida, no estimada, y la corrida evita una consulta y un
recorrido separado en los casos sin rechazo. Una forma `TABLESAMPLE` que
necesita el total para construir un porcentaje lo cuenta antes y no
reclama este ahorro.

La decisión de pagar moda y mediana es explícita.
`politica_costo = "todas"` es el valor por omisión y conserva todas las
métricas solicitadas. Con `politica_costo = "por_cardinalidad"`, primero
se miden los valores válidos y distintos; luego se omiten, por columna,
las métricas caras cuando
`n_distintos / n_validos >= umbral_cardinalidad`. El umbral por omisión
es `0.95`, se puede cambiar en la llamada, y cada omisión queda
declarada en `resumen_tabla$sql` como `omitido_por_costo`, con el motivo
y la forma de pedirla igual.

Ahora bien, **cuántas consultas se emiten no dice cuánto cuestan**:
catorce consultas sobre dos millones de filas son mucho más trabajo que
doscientas sobre mil, y quien mira el plan quiere saber si esto tarda
segundos, minutos u horas. Por eso el plan estima además la magnitud, en
cuentas de verdad y no en un índice inventado, y la estima en **dos
mitades**, porque el reloj de una corrida no lo decide siempre el motor.

La del motor son `filas_leidas`, cuántas filas habría que leer, y
`ordenaciones_completas`, cuántas veces habría que ordenar la tabla
entera; se resume en `magnitud_motor`. La del cliente son
`columnas_texto` y `pares_texto`, cuántos pares de formas podría
comparar en R el detector de vocabulario sobre la muestra; se resume en
`magnitud_texto`. `magnitud` es **la mayor de las dos** —`"baja"`,
`"media"`, `"alta"`, o `"desconocida"` si no se conoce el número de
filas—.

Contar sólo el motor daba juicios falsos con números ciertos. Una tabla
del catálogo de PostGIS de 3.912 filas, con una columna de geometría
guardada como texto, pedía 64.592 lecturas de fila y cero ordenaciones:
magnitud `"baja"`. Y tardaba 35 segundos, **ya con el presupuesto de
trabajo calibrado** —la misma tabla tardaba 243 segundos antes, y esa
medición es la que aparece en
[`vignette("escala-y-duplicados")`](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)—.
El trabajo que quedaba estaba del otro lado, comparando formas, que no
es una lectura de fila. Al imprimir el plan se ven las dos mitades, y el
aviso de magnitud alta viene con las palancas concretas para acotarla
—que no son las mismas de un lado que del otro—: `modo = "muestreado"`,
recortar `metricas`, bajar `muestra` o poner `max_consultas` para el
motor, y `max_trabajo_vocabulario` para lo que se hace en R.

Es una estimación y lo dice en `attr(plan, "supuesto_costo")`. La del
motor cuenta las filas que habría que leer **si ningún índice ayudara**,
y cada ordenación completa como `log2(filas)` pasadas; un índice sobre
la columna ordenada, o una tabla que entra en la memoria del motor, la
bajan mucho. La del cliente cuenta pares: el conteo es exacto, pero
**cuánto cuesta cada par depende del largo de los valores, que el plan
no conoce sin leerlos**. Medido acá, entre 660.000 y 1.150.000 pares por
segundo con valores de cuarenta caracteres, y unos 80.000 con valores de
doscientos: con textos muy largos el tiempo real es varias veces el que
sugiere la referencia. Los números publicados no dependen de esos
supuestos, así que quien no los comparta puede rehacer la cuenta.

### Lo que el muestreo en el motor no puede darte

Los agregados planos que comparten lote —`COUNT(col)`, mínimo, máximo,
media, ceros, negativos y desvío— se piden en una sola consulta.
`COUNT(DISTINCT ...)` y la moda conservan consultas separadas por sus
planes propios. La mediana también conserva ese camino como respaldo; un
motor que acepta `PERCENTILE_CONT(...) WITHIN GROUP` puede consolidar
varias en un solo `SELECT` por lote. En `modo = "muestreado"`, una
consulta distinta vuelve a resolver el muestreo: la media y la mediana
de una misma columna describen conjuntos de filas distintos, del mismo
tamaño pero no necesariamente los mismos. Las métricas planas del mismo
lote sí comparten las filas de esa consulta, y `id_muestra` lo deja
comprobable directamente. Un `id_muestra` ausente declara que no se
puede garantizar esa coincidencia; queda ausente para moda, frecuencia
de la moda y mediana, que son métricas por columna.

Por eso las formas candidatas están ordenadas por **previsibilidad del
tamaño** y no por costo. Primero la de cantidad fija,
`TABLESAMPLE RESERVOIR (n ROWS)`; después la de nivel de fila,
`TABLESAMPLE BERNOULLI (p)`; y recién al final las de bloque. Medido
contra PostgreSQL 16, pidiendo el 20 % de una tabla de 5.000 filas en
cuatro corridas:

    TABLESAMPLE SYSTEM (20)      678  904  452  1384
    TABLESAMPLE BERNOULLI (20)  1011 1017  981  1050

`SYSTEM` elige bloques enteros, así que el tamaño de la muestra salta de
un tercio al doble de lo pedido, y sobre una tabla chica puede dar cero.
Con tamaños tan distintos entre consultas, dos métricas del mismo perfil
dejan de ser comparables entre sí, y la guarda de coherencia interna
—que compara, por ejemplo, la frecuencia de la moda contra los válidos—
empieza a declarar no disponible lo que en realidad es incomparable.

Si necesitás que todas las métricas describan exactamente las mismas
filas, el camino es `perfil_muestra`: trae una muestra a R **una sola
vez** y corre sobre ella el perfil completo. Cuesta traer los datos; a
cambio, todo lo que informa habla del mismo conjunto.

# Perfilar una base

Por omisión,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
mantiene separados dos resultados porque provienen de universos
distintos:

- `resumen_tabla` calcula agregados SQL sobre la tabla completa;
- `perfil_muestra` ejecuta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las filas traídas a memoria.

El resumen SQL usa `universo = "tabla_completa"` por omisión. Para
tablas grandes se puede pedir `universo = "muestra_motor"` junto con
`muestra_motor = n`: las métricas se calculan sobre una relación
muestreada por el motor, no sobre una tabla traída a R. La capacidad se
sondea antes de medir; PostgreSQL y SQL Server pueden resolverla con
`TABLESAMPLE`, y otros motores con una función pseudoaleatoria y el
límite de su dialecto. Si ninguna forma es aceptada, las métricas quedan
en `NA` y `resumen_tabla$cobertura` conserva el motivo.

Cada registro de `resumen_tabla$sql` declara `alcance`, `universo`,
`tamano_muestra`, `fraccion`, `metodo` y `error_esperado`. En el
universo muestreado, `error_esperado` distingue `no_estimado` —el error
podría calcularse bajo un plan probabilístico, pero no se calculó— de
`no_estimable` —moda, mediana y cardinalidad observada no tienen una
cota simple sin supuestos adicionales—. `no_aplica` indica que no hubo
muestreo efectivo. El motivo queda en `resumen_tabla$sql$motivo`, junto
con el método, el tamaño y la fracción. `n_distintos` se conserva como
cardinalidad observada en la muestra, no como una estimación de la
cardinalidad de la tabla completa.

El metadato `resumen_tabla$meta$muestreo` conserva `tamano_muestra` por
compatibilidad como tamaño efectivo solicitado a la consulta. Además
publica `filas_solicitadas`, el pedido original, y `filas_obtenidas`,
las filas que devolvió la lectura de `perfil_muestra`; esta última puede
ser `NA` cuando el bloque no se solicitó o falló antes de leer. Si la
consulta de la muestra devuelve cero filas, no hay base para medir las
métricas de alcance `muestra`: quedan en `NA`, con estado
`no_disponible` y un motivo que nombra la muestra vacía. Esto no permite
concluir que la columna esté vacía, por lo que no se publica cero ni se
dispara `sin_valores`; `n` conserva el conteo de la tabla completa.
`estrategia_distintos` es explícita: `"exacta"` es el valor por omisión
y emite `COUNT(DISTINCT)`; `"aproximada_motor"` usa una función nativa
sólo si el motor la acepta; `"catalogo"` lee `pg_stats.n_distinct` en
PostgreSQL y lo publica como estimación, con guardas de herencia y de
modos muestreados; en otros motores queda `no_disponible` con su motivo;
y `"omitida"` no emite la consulta. Una estrategia aproximada sin
capacidad no se convierte en exacta. El resultado distingue la
estrategia solicitada, la resuelta y su estado.

Además, `id_consulta` identifica la consulta de datos que produjo cada
métrica; `muestra_id` queda reservado para la relación materializada en
el spool cliente. Dos métricas con el mismo valor vieron exactamente las
mismas filas y se pueden comparar directamente, sin cruzar `lote` ni
`columnas_compartidas`. Un `NA` declara que esa garantía no se puede
hacer; en particular, moda, frecuencia de la moda y mediana son métricas
por columna y no comparten filas con otras. `consulta_id` identifica la
sentencia que produjo cada medición y permite formar el grupo de
consistencia: las cotas duras sólo comparan valores con el mismo
identificador. La consulta exacta de distintos trae
`COUNT(columna) AS n_validos_guard` junto a `COUNT(DISTINCT columna)`;
si una capacidad aproximada no puede traer ese guardián, la cota se
declara no comprobable y no se atribuye una inconsistencia al motor.

El catálogo de la clave primaria se consulta siempre, aunque la política
de costo no necesite cardinalidad. La respuesta queda en
`resumen_tabla$meta$clave`, con sus `columnas`, `fuente`, `motivo`,
`garantia` y `estado`. La consulta es de metadatos y no recorre la
tabla. La fuente estructural sólo gobierna la medición de cardinalidad
cuando la política lo necesita; no se pierde el conocimiento de una
clave garantizada si `estrategia_distintos` está omitida o no
disponible. Si la fuente estructural no alcanza y la estrategia no
permite medir, la cardinalidad queda desconocida y se aplica la regla
explícita de la política.

El resumen declara `meta$snapshot = FALSE`, porque sus agregados son
sentencias separadas. Cuando `n_validos` y `n_distintos` exactos son
incoherentes y vienen de grupos `consulta_id` distintos, `cobertura`
agrega `alcance_distinto` y el motivo conserva ambas sentencias: es
evidencia de que la tabla cambió durante la corrida, no una acusación
contra el motor.

Mezclarlos en una fila, aun con un campo de alcance por resultado,
permitiría comparar cantidades como si pertenecieran al mismo perfil.
Los dos bloques impiden esa lectura.

Si sólo se necesitan los agregados del motor,
`bloque_muestra = "solo_agregados"` evita traer filas a R y deja
`perfil_muestra = NULL`. La cobertura lo declara como `no_solicitado`,
que no es un fallo. La lectura habitual se conserva con
`bloque_muestra = "con_muestra"`, que es el valor por omisión.

`max_celdas_muestra` y `max_bytes_muestra` acotan sólo el bloque
`perfil_muestra`: el primero se resuelve con las columnas del esquema y
el segundo con una sonda de hasta 100 filas, antes de leer el resto. Los
agregados SQL conservan su alcance y la cobertura declara cuál tope
redujo la muestra.

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
  conexion, "entregas", metricas = "validos",
  bloque_muestra = "solo_agregados"
)
```

La conexión y los dos perfiles ya están preparados. Los bloques
siguientes consultan ese mismo estado: partir la explicación no implica
recrear la base ni volver a medirla.

Cuando el perfil es de una tabla en memoria,
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
proyecta el costo de las celdas antes de iniciar las etapas caras. Desde
100.000 celdas puede mostrar, sólo en una sesion interactiva, un aviso
con la duracion estimada y la fuente de la referencia; en un guion no
interactivo queda en silencio. La proyeccion se conserva en
`meta$costo_tabla_ancha`. Para la proximidad del vocabulario, valores de
mas de 10.000 caracteres quedan fuera de la distancia normalizada y la
cobertura lo declara con el umbral; `Inf` es la forma explicita de
recuperar el comportamiento anterior.

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
estado no fue consultable. Una tabla sin clave declarada queda con
`garantia = "no_declarada"`; una consulta fallida o un catálogo cuya
visibilidad no se puede establecer queda con `garantia = "desconocida"`
y su motivo.

El método de impresión muestra esta línea de metadatos junto al resumen.
Por ejemplo, una tabla sin clave dice «Clave primaria: no declara
clave», mientras que una consulta fallida dice que no se pudo preguntar
al catálogo.

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
#>                                                                                                                   motivo
#> 1 No se solicito el perfil de muestra: este resultado contiene unicamente los agregados SQL de la configuracion elegida.
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
#> 2 perfil_muestra            5               110                    111
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
`bytes_resultado_r`, `consulta_id` e `id_consulta`, además de `etapa` y
`nivel`. La duración incluye la ejecución y la lectura del resultado que
DBI entregó a R; el tamaño es
[`object.size()`](https://rdrr.io/r/utils/object.size.html) de ese
resultado. `cpu_ms` es la suma de `user.self` y `sys.self` de
[`proc.time()`](https://rdrr.io/r/base/proc.time.html): cerca de cero
indica espera y cerca de uno al dividirlo por `duracion_ms` indica
trabajo del cliente. Las filas que no emitieron consulta dejan esos
campos en `NA`, que significa «no medido», no `0`. Como una consulta
puede producir varias filas (una por columna y métrica), su duración se
repite en ellas: `nivel = 1` marca la primera fila de cada `consulta_id`
y `nivel = 2` las repeticiones. Para sumar tiempos SQL se deben usar
sólo las filas de `nivel = 1`.

``` r

perfil$resumen_tabla$sql[, c(
  "metrica", "estado", "duracion_ms", "cpu_ms", "n_filas_resultado",
  "bytes_resultado_r", "consulta_id", "id_consulta", "etapa", "nivel"
)]
#>            metrica    estado duracion_ms cpu_ms n_filas_resultado bytes_resultado_r
#> 1                n calculado   0.2987385      1                 1              3192
#> 2                n calculado   0.2987385      1                 1              3192
#> 3                n calculado   0.2987385      1                 1              3192
#> 4                n calculado   0.2987385      1                 1              3192
#> 5        n_validos calculado   0.2987385      1                 1              3192
#> 6      n_faltantes calculado   0.2987385      1                 1              3192
#> 7   prop_faltantes calculado   0.2987385      1                 1              3192
#> 8      n_distintos calculado   0.2233982      0                 1              1216
#> 9   tasa_distintos calculado   0.2233982      0                 1              1216
#> 10            moda calculado   0.2427101      0                 1              1024
#> 11 frecuencia_moda calculado   0.2427101      0                 1              1024
#> 12          minimo calculado   0.2987385      1                 1              3192
#> 13          maximo calculado   0.2987385      1                 1              3192
#> 14           media calculado   0.2987385      1                 1              3192
#> 15         n_ceros calculado   0.2987385      1                 1              3192
#> 16     n_negativos calculado   0.2987385      1                 1              3192
#> 17         mediana calculado   0.3261566      0                 1               736
#> 18          desvio calculado   0.2987385      1                 1              3192
#> 19       n_validos calculado   0.2987385      1                 1              3192
#> 20     n_faltantes calculado   0.2987385      1                 1              3192
#> 21  prop_faltantes calculado   0.2987385      1                 1              3192
#> 22     n_distintos calculado   0.2233982      0                 1              1216
#> 23  tasa_distintos calculado   0.2233982      0                 1              1216
#> 24            moda calculado   0.2179146      0                 1              1080
#> 25 frecuencia_moda calculado   0.2179146      0                 1              1080
#> 26          minimo no_aplica          NA     NA                NA                NA
#> 27          maximo no_aplica          NA     NA                NA                NA
#> 28           media no_aplica          NA     NA                NA                NA
#> 29         n_ceros no_aplica          NA     NA                NA                NA
#> 30     n_negativos no_aplica          NA     NA                NA                NA
#> 31         mediana no_aplica          NA     NA                NA                NA
#> 32          desvio no_aplica          NA     NA                NA                NA
#> 33       n_validos calculado   0.2987385      1                 1              3192
#> 34     n_faltantes calculado   0.2987385      1                 1              3192
#> 35  prop_faltantes calculado   0.2987385      1                 1              3192
#> 36     n_distintos calculado   0.2124310      0                 1              1216
#> 37  tasa_distintos calculado   0.2124310      0                 1              1216
#> 38            moda calculado   0.5230904      1                 1              1024
#> 39 frecuencia_moda calculado   0.5230904      1                 1              1024
#> 40          minimo calculado   0.2987385      1                 1              3192
#> 41          maximo calculado   0.2987385      1                 1              3192
#> 42           media calculado   0.2987385      1                 1              3192
#> 43         n_ceros calculado   0.2987385      1                 1              3192
#> 44     n_negativos calculado   0.2987385      1                 1              3192
#> 45         mediana calculado  97.6066589     98                 1               736
#> 46          desvio calculado   0.2987385      1                 1              3192
#> 47       n_validos calculado   0.2987385      1                 1              3192
#> 48     n_faltantes calculado   0.2987385      1                 1              3192
#> 49  prop_faltantes calculado   0.2987385      1                 1              3192
#> 50     n_distintos calculado   0.2124310      0                 1              1216
#> 51  tasa_distintos calculado   0.2124310      0                 1              1216
#> 52            moda calculado   0.3089905      1                 1              1088
#> 53 frecuencia_moda calculado   0.3089905      1                 1              1088
#> 54          minimo no_aplica          NA     NA                NA                NA
#> 55          maximo no_aplica          NA     NA                NA                NA
#> 56           media no_aplica          NA     NA                NA                NA
#> 57         n_ceros no_aplica          NA     NA                NA                NA
#> 58     n_negativos no_aplica          NA     NA                NA                NA
#> 59         mediana no_aplica          NA     NA                NA                NA
#> 60          desvio no_aplica          NA     NA                NA                NA
#>    consulta_id id_consulta       etapa nivel
#> 1            8           8     basicos     1
#> 2            8           8     basicos     2
#> 3            8           8     basicos     2
#> 4            8           8     basicos     2
#> 5            8           8     basicos     2
#> 6            8           8     basicos     2
#> 7            8           8     basicos     2
#> 8            9           9     conteos     1
#> 9            9           9     conteos     2
#> 10          11          NA        moda     1
#> 11          11          NA        moda     2
#> 12           8           8     basicos     2
#> 13           8           8     basicos     2
#> 14           8           8     basicos     2
#> 15           8           8     basicos     2
#> 16           8           8     basicos     2
#> 17          15          NA     mediana     1
#> 18           8           8     basicos     2
#> 19           8           8     basicos     2
#> 20           8           8     basicos     2
#> 21           8           8     basicos     2
#> 22           9           9     conteos     2
#> 23           9           9     conteos     2
#> 24          12          NA        moda     1
#> 25          12          NA        moda     2
#> 26          NA          NA resumen_sql     1
#> 27          NA          NA resumen_sql     1
#> 28          NA          NA resumen_sql     1
#> 29          NA          NA resumen_sql     1
#> 30          NA          NA resumen_sql     1
#> 31          NA          NA resumen_sql     1
#> 32          NA          NA resumen_sql     1
#> 33           8           8     basicos     2
#> 34           8           8     basicos     2
#> 35           8           8     basicos     2
#> 36          10          10     conteos     1
#> 37          10          10     conteos     2
#> 38          13          NA        moda     1
#> 39          13          NA        moda     2
#> 40           8           8     basicos     2
#> 41           8           8     basicos     2
#> 42           8           8     basicos     2
#> 43           8           8     basicos     2
#> 44           8           8     basicos     2
#> 45          16          NA     mediana     1
#> 46           8           8     basicos     2
#> 47           8           8     basicos     2
#> 48           8           8     basicos     2
#> 49           8           8     basicos     2
#> 50          10          10     conteos     2
#> 51          10          10     conteos     2
#> 52          14          NA        moda     1
#> 53          14          NA        moda     2
#> 54          NA          NA resumen_sql     1
#> 55          NA          NA resumen_sql     1
#> 56          NA          NA resumen_sql     1
#> 57          NA          NA resumen_sql     1
#> 58          NA          NA resumen_sql     1
#> 59          NA          NA resumen_sql     1
#> 60          NA          NA resumen_sql     1

perfil$resumen_tabla$tiempos
#>                         etapa duracion_ms cpu_ms        estado nivel n_ejecuciones
#> 1        ausencia_estructural   0.3514290      1        medido     2             1
#> 2 casi_duplicados_vocabulario   5.2938461      9        medido     2             1
#> 3                dependencias   0.6263256      1        medido     2             1
#> 4      duplicados_aproximados          NA     NA no_solicitado     2             1
#> 5             lectura_muestra   0.9870529      1        medido     1             1
#> 6          perfilado_columnas 159.7108841    160        medido     2             1
#> 7           perfilado_muestra 204.9534321    213        medido     1             1
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

`resumen_tabla$sql$memoria_trabajo` responde a la pregunta «¿qué trabajo
no debo recomputar incrementalmente sobre una tabla mayor?». Sus tres
valores son `creciente`, `acotado` y `NA` cuando la fila no tiene
medición.

``` r

sql_memoria <- perfil$resumen_tabla$sql
filtro_memoria <- (
  sql_memoria$memoria_trabajo %in% c("acotado", "creciente") |
    is.na(sql_memoria$memoria_trabajo)
) & (
  (sql_memoria$columna == "id" &
     sql_memoria$metrica %in% c("n_validos", "n_distintos")) |
    (sql_memoria$columna == "grupo" & sql_memoria$metrica == "minimo")
)
ejemplos_memoria <- sql_memoria[
  filtro_memoria,
  c("columna", "metrica", "estado", "alcance", "fraccion", "metodo",
    "memoria_trabajo")
]
ejemplos_memoria
#>    columna     metrica    estado        alcance fraccion          metodo memoria_trabajo
#> 5       id   n_validos calculado tabla_completa        1  tabla_completa         acotado
#> 8       id n_distintos calculado tabla_completa        1 COUNT(DISTINCT)       creciente
#> 26   grupo      minimo no_aplica tabla_completa        1  tabla_completa            <NA>
stopifnot(
  identical(
    as.character(ejemplos_memoria$memoria_trabajo),
    c("acotado", "creciente", NA_character_)
  )
)
```

La regla es: alcance efectivo con tope real, `acotado`; si no, el método
resuelto; sin medición, `NA`.

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
`universo = "muestra_motor"` son declaradas por separado. La primera se
trae a R para ejecutar el perfil completo; la segunda permanece en SQL
para que los agregados no recorran la tabla completa. La metadata de
ambos bloques publica el universo y el método efectivo.

``` r

estimado <- perfilar_dbi(
  conexion, "entregas", universo = "muestra_motor", muestra_motor = 5000,
  muestra = 5000, metricas = c(
    "validos", "distintos", "moda", "basicos", "mediana", "desvio"
  )
)

estimado$resumen_tabla$sql[
  estimado$resumen_tabla$sql$estado %in% c("estimado", "observado_muestra"),
  c("columna", "metrica", "estado", "universo", "tamano_muestra",
    "metodo", "fraccion", "error_esperado")
]

aproximado <- perfilar_dbi(
  conexion, "entregas", muestra = 5000,
  estrategia_mediana = "aproximada_motor",
  estrategia_distintos = "aproximada_motor"
)
aproximado$resumen_tabla$meta$aproximaciones
```

Las estrategias de mediana y de distintos son independientes. Para pedir
una función nativa de cardinalidad hay que usar
`estrategia_distintos = "aproximada_motor"`; para habilitar la
aproximación de mediana se usa
`estrategia_mediana = "aproximada_motor"`. La sonda de mediana prueba
primero una forma nativa exacta consolidada, luego la forma exacta por
columna y deja la aproximada para el final. Por eso una mediana exacta
queda `calculado`, aunque se haya pedido la estrategia aproximada; sólo
queda `estimado` cuando esa forma aproximada efectivamente corrió. Una
consulta no emitida o sin un valor utilizable queda `no_disponible`.

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
emite las sondas de capacidad y publica un **rango** de consultas en
`attr(plan, "supuesto")`. No escanea datos para decidir el costo: lee el
esquema, sondea capacidades y, con
`politica_costo = "por_cardinalidad"`, puede usar una garantía
estructural o una fuente de catálogo. Nunca lanza `COUNT(DISTINCT ...)`
sólo para despejar una duda. Por eso el plan no proyecta el costo
temporal de `COUNT(DISTINCT)` antes de correr: la referencia honesta es
el primer lote de distintos medido durante la ejecución, y el plan no
emite consultas de datos.

## La memoria del procesamiento no se estima

El plan declara explícitamente que la memoria del procesamiento **no se
estima**: no escala de forma predecible con las filas ni con las celdas,
según lo medido. Conserva la **magnitud del trabajo** que se conoce
—filas, celdas y pares de texto—, rotulada como magnitud y no como
consumo de memoria.

Las cifras que ayudan a decidir son datos de referencia medidos, **no
una predicción para la tabla del plan, sino una corrida única y fechada
(2026-08-28) contra un motor remoto**: traer la tabla costó
aproximadamente 0,13 GB por millón de filas y procesar en R
aproximadamente 1,0-1,5 MB por cada mil filas. La segunda cifra varió
por 1,62x entre tablas de la misma magnitud; esa variación es justamente
el motivo por el que no se usa para estimar.

Ver todas las filas y tener todas las filas en memoria no son lo mismo.
En corridas de referencia, 4,5 millones de filas entraron en 0,6 GB y
tardaron 25 segundos en llegar, mientras que procesar 4,5 millones ocupó
aproximadamente 7 GB y procesar 12,8 millones aproximadamente 19 GB. El
problema observado está en el procesamiento en R, no en la red ni en el
motor.

El extremo inferior es `total`: si la fuente de cardinalidad queda
desconocida, supone que la política omite la moda. El extremo superior
es `total_maximo` —también reflejado en `total_lotes_rechazados` tras
sumar la bisección— y deja abierto el camino que las ejecuta. Si un lote
es rechazado, se agregan hasta `2n - 1` sondas por lote de `n` columnas.
El costo real cae entre los dos, y el plan lo declara en las dos
direcciones.

`estrategia_distintos` dice cómo se obtiene o se omite `n_distintos`, y
publica la estrategia solicitada, la resuelta y el estado antes y
después de la corrida. `fuente_cardinalidad_costo` dice de dónde sale la
proporción usada por la política de costo; es independiente y no puede
convertir una petición `"aproximada_motor"` o `"catalogo"` en un
agregado exacto.

Cada consulta de agregados planos que trae `n_validos` incluye
`COUNT(*) AS n_total_consulta` en la misma sentencia. La completitud usa
ese denominador local por lote, incluso después de una bisección, y no
cruza el total de otra consulta. El total del universo se mantiene
separado cuando hace falta para una muestra o cuando no hay agregados
planos. Después se ejecutan distintos, moda y mediana.
`tamano_lote_planos` y `tamano_lote_distintos` son independientes; este
último vale 2 por omisión. La medición contra el servidor de referencia
mostró un `Shared Read` constante entre lotes: dos cardinalidades se
amortizan en una pasada de la tabla. El derrame crece con el ancho del
lote; dos mantuvo casi el mismo tiempo por columna que una y derramó
menos que los lotes más anchos.

El costo de `COUNT(DISTINCT ...)` se anuncia después de completar el
primer lote de distintos y antes de iniciar el segundo, cuando hay una
referencia medida. Con `instrumentar = TRUE`, la proyección usa la
mediana de las duraciones de las consultas de ese primer lote en esta
misma corrida y la multiplica por la cantidad total de lotes de
distintos. El mensaje dice que es una estimación y de dónde sale; no usa
`reltuples`. Con un solo lote no hay nada que proyectar ni que evitar,
así que no se publica una proyección. La misma regla funciona si se pide
sólo `metricas = "distintos"`. Sólo se muestra cuando supera 30 segundos
y no pide confirmación, por lo que
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
no queda esperando en un guion.

Los avisos de `moda` y `mediana` tienen interruptores y umbrales
propios: `avisar_costo_moda`/`umbral_segundos_aviso_moda` proyectan por
cardinalidad y `avisar_costo_mediana`/`umbral_segundos_aviso_mediana`
proyectan por filas. Cada aviso llega antes de pagar la consulta y su
proyección queda separada en `meta$costo_moda` o `meta$costo_mediana`;
la fuente de cada cardinalidad se publica, incluso cuando proviene de
una estimación de catálogo.

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
no publica esa proyección temporal: no emite consultas de datos y, por
lo tanto, no puede medir el primer lote. El plan lo declara
explícitamente; la medición y la proyección quedan en
`resumen_tabla$meta$costo_distintos` después de ejecutar.

En PostgreSQL, la preparación puede consultar `pg_stats.n_distinct`,
`pg_stats.avg_width`, `pg_class.reltuples`, `SHOW work_mem` y, desde la
versión 13, `SHOW hash_mem_multiplier`. Con esos metadatos estima el
tamaño del hash de agregación y avisa antes del primer `COUNT(DISTINCT)`
si supera el límite efectivo. `meta$estimacion_derrame` conserva ese
diagnóstico como estimación, no como medición; subir `work_mem` en la
sesión puede evitar el derrame y el paquete nunca modifica ese
parámetro. Si faltan estadísticas, permisos o el motor no es PostgreSQL,
declara que no pudo estimar.

El informe conserva el derrame real sólo cuando PostgreSQL permite
atribuirlo sin ambigüedad mediante `pg_stat_statements`:
`resumen_tabla$sql` publica los bloques temporales escritos y
`meta$derrame` resume las consultas observadas. Si esa evidencia no está
disponible, el estado lo dice y el paquete no deduce un derrame del
tiempo transcurrido ni cambia `work_mem`. La medición real manda sobre
`meta$estimacion_derrame`: un derrame medido se informa aunque la
estimación previa no hubiera superado el límite.

La decisión de pagar moda y mediana es explícita.
`politica_costo = "todas"` es el valor por omisión y conserva todas las
métricas solicitadas. Con `politica_costo = "por_cardinalidad"`, la
corrida resuelve primero las fuentes estructurales y mide los valores
válidos y distintos sólo cuando no hay una fuente exacta y la estrategia
lo permite; luego omite, por columna, sólo la moda cuando
`n_distintos / n_validos >= umbral_cardinalidad`. El umbral por omisión
es `0.5` y gobierna sólo la moda. La mediana se conserva: el barrido
medido queda plano frente a la cardinalidad y su costo lo gobierna el
número de filas. Cada omisión queda declarada en `resumen_tabla$sql` y
`meta$decisiones_costo` explica por separado cada decisión. Una
estrategia omitida, de catálogo o aproximada sin capacidad nunca se
convierte en `COUNT(DISTINCT ...)`.

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
—que no son las mismas de un lado que del otro—:
`universo = "muestra_motor"`, recortar `metricas`, bajar `muestra` o
poner `max_consultas` para el motor, y `max_trabajo_vocabulario` para lo
que se hace en R.

Es una estimación y lo dice en `attr(plan, "supuesto_costo")`. La del
motor cuenta las filas que habría que leer **si ningún índice ayudara**,
y cada ordenación completa como `log2(filas)` pasadas; un índice sobre
la columna ordenada, o una tabla que entra en la memoria del motor, la
bajan mucho. La del cliente cuenta pares: el conteo es exacto, pero
**cuánto cuesta cada par depende del largo de los valores, que el plan
no conoce sin leerlos**. Medido con `benchmark/medir_costo_texto.R`,
entre 660.000 y 1.150.000 pares por segundo con valores de cuarenta
caracteres —la banda cubre dos máquinas—, y entre 70.000 y 80.000 con
valores de doscientos: con textos muy largos el tiempo real es varias
veces el que sugiere la referencia. Los números publicados no dependen
de esos supuestos, así que quien no los comparta puede rehacer la
cuenta.

### Lo que el muestreo en el motor no puede darte

Los agregados planos que comparten lote —`COUNT(col)`, mínimo, máximo,
media, ceros, negativos y desvío— se piden en una sola consulta.
`COUNT(DISTINCT ...)` y la moda conservan consultas separadas por sus
planes propios. La moda intenta traer
`SUM(COUNT(*)) OVER () AS n_validos_guard` junto a su frecuencia y
sondea esa forma antes de emitirla; si el motor la rechaza, conserva la
consulta anterior y lo declara en `resumen_tabla$meta$moda_guardian`. La
mediana también conserva ese camino como respaldo; en PostgreSQL 9.3 y
SQLite el conteo de la mediana queda como subconsulta escalar en la
misma sentencia que ordena y recorta. Un motor que acepta
`PERCENTILE_CONT(...) WITHIN GROUP` puede consolidar varias en un solo
`SELECT` por lote.

Con `universo = "muestra_motor"`, `TABLESAMPLE` o la fuente
pseudoaleatoria resuelta se ejecuta una sola vez y se materializa en un
spool externo de sesión cliente. El trailer conserva y verifica
`muestra_id`, `snapshot_id`, `orden_id`, `n_filas`, `bytes` y checksum;
las pasadas de resumen y diagnóstico releen esa misma relación.
`id_consulta` sigue identificando la sentencia que produjo una medición,
mientras `muestra_id` identifica la materialización. Un chunk que supera
el presupuesto se rechaza antes de escribir y publica
`spool_presupuesto_excedido` y
`muestra_inestable:presupuesto_materializacion`; no se entrega una
muestra híbrida. El spool no escribe en la conexión DBI ni crea objetos
temporales del motor.

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
dejan de ser comparables entre sí. La guarda de coherencia interna
compara la frecuencia de la moda con `n_validos_guard` de su propia
sentencia; si la sonda de la ventana no pasa, deja la cota como no
comprobable y conserva la medición sin culpar al motor.

Si necesitás que todas las métricas describan exactamente las mismas
filas, el camino es `perfil_muestra`: trae una muestra a R **una sola
vez** y corre sobre ella el perfil completo. Cuesta traer los datos; a
cambio, todo lo que informa habla del mismo conjunto.

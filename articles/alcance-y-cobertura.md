# Alcance y cobertura

Cada cifra de un perfil de `lupa` viaja con su alcance: sobre qué filas
se calculó, en qué unidad cuenta y qué quedó sin medir. Este artículo
reúne esos contratos para consultarlos. La lectura de un perfil paso a
paso está en [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md), que
también define los términos (alcance, cobertura, centinela, severidad).
El alcance de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y de cada motor está en [Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html).

Los valores por omisión que se citan acá se leen de la firma de cada
función al construir la página.

## Qué usa todas las filas y qué se muestrea

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
usa todas las filas para:

- los conteos de tabla y de columna;
- los faltantes, reales y disfrazados;
- los valores distintos y los duplicados exactos;
- los resúmenes cuantitativos;
- los hallazgos que se derivan de esas cantidades.

Una muestra acota sólo los pasos que necesitan mirar los valores uno por
uno.

| Función | Argumento | Por omisión | Qué acota |
|----|----|----|----|
| [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) | `muestra` | 100.000 filas | patrones, tipos inferidos, formatos de fecha y la muestra común de las dependencias funcionales |
| [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) | `muestra_validadores` | 1.000 valores | el primer filtro de los validadores de documentos; el validador que lo supera se evalúa sobre toda la columna |
| [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) | `muestra` | 100.000 filas | el perfil, las distribuciones y los niveles observados |
| [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) | `muestra_asociacion` | 10.000 filas | las asociaciones entre columnas |
| [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) | `duplicados_aproximados` | `FALSE` | la comparación aproximada de filas, que se pide de forma explícita |
| [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md) | `muestra` | `Inf` | las filas que se comparan; los pares tienen sus propios topes (`max_pares`, `presupuesto_pares`) |
| [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md) | `muestra` | `Inf` | las filas que se traen a R para el perfil por fila; los agregados SQL usan el universo elegido |

`Inf` en `muestra` o en `muestra_validadores` vuelve completo ese paso.
La muestra que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
toma por su cuenta es sistemática y sin azar, así que el mismo perfil se
rehace sobre los mismos datos.

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
tiene otro punto de partida: el universo por omisión es
`"tabla_completa"` y `muestra` vale `Inf`. Los agregados SQL se calculan
sobre toda la tabla y el perfil por fila intenta traer a R todas las
filas, sujeto a `max_celdas_muestra` y `max_bytes_muestra`. Para acotar
la lectura a R se pasa `muestra = n`; para muestrear en el motor,
`universo = "muestra_motor"`; para no traer filas,
`bloque_muestra = "solo_agregados"`.

### Dónde queda el alcance efectivo

- `meta$muestra`, `meta$filas_analizadas` y `meta$muestreo` describen la
  corrida entera.
- Cada columna publica `n_filas_analizadas_tipo` y
  `muestreado_tipo_inferido`.
- Para los tipos temporales inferidos, `estado_tipo_inferido` distingue
  `confirmado`, `candidato` y `NA`: una fecha ambigua compatible al 100
  % sigue siendo candidata.
- La tabla de dependencias conserva en sus atributos las filas
  analizadas y el muestreo.

``` r

data(datos_operativos)
perfil <- perfilar(datos_operativos)
perfil$meta[c("muestra", "filas_analizadas")]
#> $muestra
#> [1] 1e+05
#> 
#> $filas_analizadas
#> [1] 13
columnas(perfil)[1:4, c("columna", "n_filas_analizadas_tipo", "muestreado_tipo_inferido")]
#>          columna n_filas_analizadas_tipo muestreado_tipo_inferido
#> 1    id_registro                      13                    FALSE
#> 2 codigo_usuario                      13                    FALSE
#> 3   fecha_evento                      13                    FALSE
#> 4          canal                      12                    FALSE
```

### Diagnósticos con alcance propio

`casi_duplicados_vocabulario` se ejecuta por columna. Dos columnas con
el mismo vocabulario conservan cada una su fila de cobertura y sus
hallazgos, y el costo de compararlas puede repetirse: así no se mezclan
frecuencias ni alcances de columnas distintas.

En
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
`pares$tipo_par` vale `exacto` (los textos guardados son iguales),
`exacto_normalizado` (coinciden después de la normalización declarada) o
`aproximado` (siguen siendo similares). `pares$igualo_normalizar` marca
el caso intermedio, y el alcance los cuenta en `n_pares_exactos`,
`n_pares_exactos_normalizados` y `n_pares_aproximados`.

## Lo que un resumen deja afuera

«Todas las filas» es el alcance, no el contenido. Un resumen
cuantitativo deja afuera lo que no vale como número —`NaN`, `Inf`, los
textos que la conversión no puede leer y los centinelas declarados en
`sentinelas_numericos`— y lo dice:

- `n_valores_excluidos_resumen` cuenta cuántos valores quedaron afuera,
  con el desglose de los no finitos en `n_nan`, `n_infinito_positivo` y
  `n_infinito_negativo`;
- `estado_resumen_cuantitativo` deja de decir `"calculados"`;
- `cobertura_diagnosticos` recibe una fila.

Si no queda ningún valor utilizable, el estado distingue dos casos:

- `"sin_valores"`: la columna no tenía valores presentes en el universo
  que el resumen mide (vacía, toda `NA`, o con una `aplicabilidad` que
  deja ese universo sin valores; los de afuera se cuentan en
  `n_presentes_fuera_de_aplicabilidad`);
- `"sin_valores_utilizables"`: los tenía y ninguno servía.

``` r

no_finitos <- data.frame(
  mezcla = c(1:20, NaN, Inf, -Inf),
  infinitos = rep(Inf, 23)
)
p_no_finitos <- perfilar(no_finitos, analizar_dependencias = FALSE)
columnas(p_no_finitos)[, c(
  "columna", "estado_resumen_cuantitativo", "n_valores_excluidos_resumen",
  "n_nan", "n_infinito_positivo", "n_faltantes", "n_distintos"
)]
#>     columna estado_resumen_cuantitativo n_valores_excluidos_resumen n_nan
#> 1    mezcla    calculados_sobre_valores                           3     1
#> 2 infinitos     sin_valores_utilizables                          23     0
#>   n_infinito_positivo n_faltantes n_distintos
#> 1                   1           1          22
#> 2                  23           0           1
```

La columna de veintitrés `Inf` publica `n_faltantes = 0` y
`n_distintos = 1`: los valores están, pero ninguno sirve para un
resumen. La exclusión de los no finitos vale en cualquier clase: una
columna de fechas con un `Inf` no lo cuenta entre las fechas resumidas,
y una columna `integer64` declara lo mismo que una de dobles. Las
longitudes de texto declaran su propio universo en
`n_longitudes_resumidas`, porque
[`nchar()`](https://rdrr.io/r/base/nchar.html) no puede medir un valor
cuyos bytes no son UTF-8 válido.

## Cuatro estados que no son cero

Lo que no se pudo medir se registra con un estado y un motivo, y nunca
se cuenta como cero. Un `0` es una medición: el diagnóstico corrió y no
encontró nada. Un `NA` acompañado de uno de estos estados dice por qué
no hay cifra.

| Estado | Dónde aparece | Qué significa |
|----|----|----|
| `no_aplica` | `estado_resumen_cuantitativo`, el `estado` y el `error_esperado` de una métrica SQL, el estado de un factor en [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md), la trazabilidad | la medida no corresponde: un resumen cuantitativo de una columna sin números, el error muestral de una corrida sin muestreo |
| `no_disponible` | el `estado` de una métrica SQL, la trazabilidad, las tablas de una colección | correspondía y no se pudo obtener: falta una capacidad del motor, la muestra vino vacía o la credencial no lee la tabla; el `motivo` dice cuál |
| `no_estimado` | el `error_esperado` de una métrica sobre una muestra | el error muestral podría calcularse bajo un plan probabilístico, pero la corrida no lo calcula |
| `no_estimable` | el `error_esperado` de una consulta sobre una muestra | la consulta incluye la moda, la mediana o la cardinalidad observada, que no tienen una cota simple sin supuestos ni un estimador declarado |

`error_esperado` se asigna por consulta: cuando una consulta muestreada
incluye la moda, la mediana o un conteo de distintos, todas sus métricas
reciben `no_estimable`, también las que en otra consulta serían
`no_estimado`. En ningún caso se publica una cota numérica sin una
fórmula que la justifique.

En el perfil en memoria,
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve `cobertura_diagnosticos`: una fila por diagnóstico que no se
ejecutó, con `motivo` y `como_resolverlo`.

``` r

diagnosticos <- cobertura(perfil)
diagnosticos[, c("columna", "diagnostico")]
#>        columna            diagnostico
#> 1  id_registro               outliers
#> 2 fecha_evento               outliers
#> 3 fecha_evento   resumen_cuantitativo
#> 4 fecha_evento            patron_raro
#> 5        monto               outliers
#> 6     id_copia               outliers
#> 7         zona proximidad_vocabulario
#> 8     contacto proximidad_vocabulario
#> 9    id_evento proximidad_vocabulario
envolver(diagnosticos$motivo[[1]])
#> No se evaluaron los límites de Tukey: hay 13 valores finitos y se requieren al menos
#> 20. Reunir más observaciones o declarar otro criterio antes de interpretar outliers.
```

En una base, los mismos estados aparecen en cada métrica SQL. Con SQLite
en memoria, una corrida sobre la tabla completa y otra sobre una muestra
del motor:

``` r

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conexion, "eventos", datos_operativos)

completa <- perfilar_dbi(conexion, "eventos", bloque_muestra = "solo_agregados")
table(sql_perfil(completa)$error_esperado)
#> 
#> no_aplica 
#>       150

muestreada <- perfilar_dbi(
  conexion, "eventos",
  universo = "muestra_motor", muestra_motor = 5, bloque_muestra = "solo_agregados"
)
sql <- sql_perfil(muestreada)
table(estado = sql$estado, error_esperado = sql$error_esperado)
#>                    error_esperado
#> estado              no_aplica no_estimable no_estimado
#>   calculado                10            0           0
#>   estimado                  0           13          12
#>   no_aplica                 0           49           0
#>   observado_muestra        20           10          36

DBI::dbDisconnect(conexion)
```

`observado_muestra` describe las filas que trajo la muestra, no la
tabla. Sin filas traídas a R no se puede clasificar una columna como
dato personal, y sus valores quedan `omitido_por_privacidad` (ver el
último apartado).

## Unidades declaradas y trazabilidad por fila

Cada hallazgo declara en qué cuenta: `n_evaluados` y `n_afectados`
vienen con `unidad_conteo`: por ejemplo `fila`, `columna`,
`valor_distinto` o `formato`.

``` r

h <- hallazgos(perfil)
h[h$tipo_hallazgo %in% c(
  "mayusculas_inconsistentes", "formatos_fecha_mixtos", "filas_duplicadas"
), c("columna", "tipo_hallazgo", "n_evaluados", "n_afectados", "unidad_conteo")]
#>         columna             tipo_hallazgo n_evaluados n_afectados  unidad_conteo
#> 6  fecha_evento     formatos_fecha_mixtos           5           5        formato
#> 11        canal mayusculas_inconsistentes           8           2 valor_distinto
#> 20         <NA>          filas_duplicadas          13           2           fila

traza <- h$trazabilidad[[which(h$tipo_hallazgo == "mayusculas_inconsistentes")]]
traza[c("localizador", "total", "indices_fila")]
#> $localizador
#> [1] "indice_fila"
#> 
#> $total
#> [1] 6
#> 
#> $indices_fila
#> [1]  3  1  6 10 11 12
```

`mayusculas_inconsistentes` cuenta 2 valores distintos y su traza
enumera las 6 filas que los contienen. Esa es la regla de las señales
que cuentan valores:

- `mayusculas_inconsistentes` y `normalizacion_unicode` cuentan valores
  distintos (`valor_distinto`) y trazan todas las filas que contienen un
  valor afectado, no sólo las defectuosas.
- `casi_duplicados_vocabulario` cuenta los valores variantes y traza
  todas las filas cuyo valor pertenece a un grupo seleccionado, incluida
  la forma dominante. La traza entrega primero las formas no dominantes
  y la evidencia declara cuántas filas mostradas pertenecen a cada
  grupo. El detector es heurístico: la traza sirve para revisar o
  unificar el grupo en el sistema de origen, no afirma que todas esas
  filas sean errores.
- En `patron_raro`, la evidencia muestra como máximo seis patrones
  raros, pero la traza usa todos los nombres de patrones raros hasta un
  límite separado de 5.000; si se alcanza, el alcance de la traza es
  parcial y `cobertura_diagnosticos` publica el límite. Cada hallazgo
  informa la proporción del patrón dominante y cuántas filas quedaron en
  patrones excluidos por superar `umbral_patron_raro`. Si ningún patrón
  alcanza `umbral_patron_dominante`, no se emite el hallazgo y la no
  medición queda en `cobertura_diagnosticos`, con la proporción
  observada.
- `filas_duplicadas` cuenta todas las filas que participan en grupos
  duplicados; la evidencia informa aparte los excedentes.

Por eso una traza puede tener más filas que `n_afectados`. La traza usa
la misma comparación que el conteo en las dos direcciones, también en
una columna `integer64`, así que `n_afectados` y `trazabilidad$total`
describen el mismo conjunto. Si un hallazgo y su traza no coinciden,
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
conserva el hallazgo y emite una advertencia de clase
`lupa_trazabilidad_incoherente`.

Con `clave`, la traza trae los valores que identifican cada fila en
lugar de su índice; `trazabilidad$localizador` dice cuál de los dos se
usó. Está explicado en [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md).

## Numeraciones: densidad, Benford y Tukey

La ley de Benford supone un proceso multiplicativo y los límites de
Tukey suponen una distribución. Una numeración —un identificador, un
código correlativo— no es ninguna de las dos cosas: que un código quede
lejos de la mediana no dice nada de su calidad. Por eso
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
reconoce las numeraciones y no les aplica esas pruebas.

Una numeración se reconoce por tres rasgos juntos:

- ocupa un tramo compacto de los enteros: al menos 50 % del tramo entre
  su mínimo y su máximo, con 5 valores distintos como mínimo;
- casi no repite valores: al menos un 90 % de valores distintos;
- no tiene un salto de escala: un valor que abre un hueco mucho mayor
  que el típico entre valores vecinos.

La densidad sola no alcanza: una magnitud entera acotada, como una edad,
también llena su tramo cuando hay filas suficientes, pero repitiendo
valores. Y la unicidad sola tampoco, porque un monto también es casi
único. El salto de escala devuelve las pruebas a una columna donde un
dato malo abre un hueco.

``` r

set.seed(118)
numeraciones <- data.frame(
  numeracion = sample(5000L, 3000L),
  edad = c(sample(18:70, 2999L, replace = TRUE), 120L),
  codigo = c(sample(60L, 2700L, replace = TRUE), sample(61:284, 300L, replace = TRUE))
)
p_num <- perfilar(numeraciones, analizar_dependencias = FALSE)
columnas(p_num)[, c(
  "columna", "densidad_secuencia_entera", "tasa_distintos",
  "salto_de_escala_secuencia_entera", "n_outliers"
)]
#>      columna densidad_secuencia_entera tasa_distintos salto_de_escala_secuencia_entera
#> 1 numeracion                 0.6000000     1.00000000                            FALSE
#> 2       edad                 0.5242718     0.01800000                             TRUE
#> 3     codigo                 0.8156028     0.07666667                            FALSE
#>   n_outliers
#> 1          0
#> 2          1
#> 3        243
cobertura(p_num)[, c("columna", "diagnostico")]
#>      columna diagnostico
#> 1 numeracion ley_benford
#> 2       edad ley_benford
#> 3     codigo ley_benford
```

`numeracion` no recibe límites de Tukey ni Benford. Las tres columnas
tienen una fila `ley_benford` en la cobertura, cada una con su motivo, y
el de `numeracion` dice que parece un identificador. `edad` repite
valores, así que no es una numeración: los límites de Tukey corren y el
`120` aparece como atípico. `codigo` también repite: es un código, pero
su forma no lo distingue de una magnitud, y el paquete prefiere hablar
de más sobre él. Quien conoce la columna lo declara en `columnas_codigo`
(apartado siguiente). Lo que no se corre no se apaga en silencio: deja
su fila en `cobertura_diagnosticos` con el motivo medido.

``` r

envolver(cobertura(p_num)$motivo[cobertura(p_num)$columna == "numeracion"][[1]])
#> No aplica la ley de Benford. Precondiciones fallidas: parece un identificador: el
#> perfil publico `posible_identificador` y los valores forman una secuencia correlativa.
```

El campo `secuencia_entera_densa` contesta otra pregunta, de cobertura:
si la columna llena su tramo con el umbral que publica
`umbral_densidad_secuencia_entera` y al menos
`min_distintos_secuencia_entera` valores distintos. Junto a él se
publican `densidad_secuencia_entera`, `n_huecos_secuencia_entera`,
`hueco_maximo_secuencia_entera` y `salto_de_escala_secuencia_entera`. La
señal `moda_sobresale_secuencia_entera` es independiente y no desactiva
las comprobaciones que usan la numeración.

Para una columna `integer64`, `bit64` se carga de manera diferida cuando
está instalado, para registrar sus métodos antes de medir. Si no está
instalado, las cinco medidas de la secuencia quedan en `NA` y
`cobertura_diagnosticos` registra que `secuencia_entera` no se pudo
evaluar.

La regla está fijada por `tests/testthat/test-regresion-118.R`:
numeraciones que no reciben pruebas de magnitud, magnitudes con un dato
malo que se siguen viendo, y un código que se repite sobre el que el
paquete habla de más, que es el error barato.

## Centinelas

Un centinela es un valor que se usa como «sin dato»: `-99`, `9999`,
`"S/D"`. La prosa dice *centinela*; los argumentos se llaman
`sentinelas_numericos` y `sentinelas_naniar`, y el hallazgo,
`posible_centinela_numerico`.

El paquete separa lo que se declara de lo que sospecha:

- **La lista por omisión** de `sentinelas_numericos`
  (`c(-9, -99, -999, -9999, 999)`) informa y no toca los números: sus
  valores aparecen como `faltantes_disfrazados`, pero siguen dentro de
  la media. Es una conjetura, y una conjetura no mueve un promedio.
- **Una lista declarada** excluye: sus valores salen de `media`,
  `mediana`, `minimo`, `maximo` y `desvio`, y
  `n_valores_excluidos_resumen` dice cuántos quedaron afuera, igual que
  los `NA` y las filas que `aplicabilidad` deja fuera del universo.
  También salen de la ley de Benford y de las relaciones aritméticas: la
  misma celda no puede ser ausencia en el resumen y valor en un
  diagnóstico. `moda` y `n_distintos` siguen describiendo lo que está
  guardado.
- **`posible_centinela_numerico`** informa un valor que reúne tres
  rasgos: queda fuera de los límites, se repite y tiene forma de dígito
  repetido. No lo cuenta como ausencia: eso lo decide quien conoce la
  columna, agregándolo a la lista.

Un `9999` puede ser una edad imposible o un código postal válido, y por
eso no está en la lista por omisión. Un código postal `9999` repetido no
es extremo en su columna; un monto real de `9999` no se repite; un año
`1999` no tiene la forma.

``` r

set.seed(4)
montos <- data.frame(monto = c(round(rlnorm(500, 5, 1), 2), rep(9999, 30)))
sospecha <- perfilar(montos, analizar_dependencias = FALSE)
centinela <- hallazgos(sospecha)
centinela <- centinela[centinela$tipo_hallazgo == "posible_centinela_numerico", ]
envolver(centinela$evidencia)
#> Valor: 9999; repeticiones: 30; valores fuera de los límites: 52
envolver(centinela$sugerencia)
#> Si es un código de ausencia, agregarlo a `sentinelas_numericos`: pasa a contarse como
#> faltante y sale de `media`, `mediana`, `minimo`, `maximo` y `desvio`, que declaran
#> cuantos valores dejaron afuera en `n_valores_excluidos_resumen`. `moda` y `n_distintos`
#> siguen describiendo lo que hay guardado. Si es un dato real, no hay nada que hacer.

declarado <- perfilar(montos, analizar_dependencias = FALSE, sentinelas_numericos = 9999)
rbind(
  sospecha = columnas(sospecha)[, c("media", "n_valores_excluidos_resumen", "moda")],
  declarado = columnas(declarado)[, c("media", "n_valores_excluidos_resumen", "moda")]
)
#>              media n_valores_excluidos_resumen moda
#> sospecha  783.3186                           0 9999
#> declarado 230.3778                          30 9999
```

`sentinelas_naniar` trae los centinelas numéricos que publica el paquete
naniar, para pasarlos de una vez:
`perfilar(datos, sentinelas_numericos = sentinelas_naniar)`. La política
de centinelas se compara como un conjunto numérico: cambiar el orden,
entregar enteros en vez de dobles o repetir un valor no cambia su
significado, ni en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) ni
en
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).

## Columnas que son códigos: `columnas_codigo`

Un código numérico declarado en `columnas_codigo` no recibe límites de
Tukey, ni Benford, ni `media`, `mediana` y `desvio`. Conserva su tramo
(`minimo` y `maximo`), su moda y sus conteos, y la cobertura dice por
qué no se evaluó lo demás.

``` r

como_codigo <- perfilar(
  numeraciones["codigo"], analizar_dependencias = FALSE, columnas_codigo = "codigo"
)
rbind(
  sin_declarar = subset(columnas(p_num), columna == "codigo",
    c("n_outliers", "media", "moda", "minimo", "maximo")),
  declarado = columnas(como_codigo)[, c("n_outliers", "media", "moda", "minimo", "maximo")]
)
#>              n_outliers  media moda minimo maximo
#> sin_declarar        243 43.882   59      1    282
#> declarado            NA     NA   59      1    282
cobertura(como_codigo)$diagnostico
#> [1] "outliers"                     "relacion_aritmetica_columnas"
#> [3] "ley_benford"
```

Pasar la columna como texto o como factor no alcanza: el paquete la lee
como número de todos modos. Dentro de
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
la declaración llega también a `variables` (escala nominal, rol de
categoría) y a `asociaciones`, donde el código se asocia como categoría
y no con una correlación.

## Cardinalidad alta: `alta_cardinalidad`

Una columna de texto o factor con una proporción de valores distintos
mayor que `umbral_alta_cardinalidad` (por omisión 0,5), menor que uno y
con al menos 10 valores distintos recibe `alta_cardinalidad`, con
severidad `sospechoso`. El hallazgo no afirma que haya un defecto:
ofrece tres lecturas para que decida quien conoce la columna.

``` r

alta <- h[h$tipo_hallazgo == "alta_cardinalidad", ]
alta[, c("columna", "evidencia")]
#>           columna                        evidencia
#> 2  codigo_usuario Tasa de valores distintos: 0.846
#> 4    fecha_evento Tasa de valores distintos: 0.923
#> 14       contacto Tasa de valores distintos: 0.923
envolver(alta$sugerencia[[1]])
#> Revisar si es texto libre, un identificador o una categoría mal normalizada.
```

La evidencia de `contacto` sale entera. Con
`proteger_datos_personales = TRUE` saldría protegida, porque la columna
parece un dato personal (ver el último apartado).

Se informa siempre, también cuando los valores son largos: el largo no
distingue un catálogo de la prosa. Un catálogo de etiquetas largas con
cardinalidad alta es justo la señal de una normalización rota, y un
comentario libre puede ser corto.

## Concentración de un valor: `valor_concentrado`

`valor_concentrado` considera sólo columnas numéricas con al menos 20
valores válidos y al menos 10 valores distintos. Emite un hallazgo
`sospechoso` cuando la moda es al menos 5 veces más frecuente que el
segundo valor y representa al menos 15 % de los válidos. La evidencia
publica el valor modal, las dos frecuencias, el cociente y la fracción.
Un valor legítimo puede dominar, y por eso nunca es un `error`.

``` r

set.seed(5)
concentrados <- data.frame(
  un_cuarto = c(rep(250, 40), round(runif(120, 1, 1000))),
  un_octavo = c(rep(250, 20), round(runif(140, 1, 1000)))
)
p_conc <- perfilar(concentrados, analizar_dependencias = FALSE)
conc <- hallazgos(p_conc)
conc <- conc[conc$tipo_hallazgo == "valor_concentrado", ]
conc$columna
#> [1] "un_cuarto"
envolver(conc$evidencia)
#> Valor modal: 250; frecuencia de la moda: 40; frecuencia del segundo valor: 3; cociente
#> moda/segundo: 13.333; fraccion de la moda sobre validos: 0.250
```

La columna donde el valor modal ocupa un octavo de los válidos no recibe
el hallazgo: está por debajo de la fracción mínima. Ese es uno de los
puntos ciegos de la regla, que forman parte de su contrato:

- no detecta concentraciones por debajo de la fracción mínima;
- en columnas enteras pequeñas, los empates naturales pueden dejar el
  cociente por debajo del factor;
- las columnas no elegibles no reciben una fila de
  `cobertura_diagnosticos`: las distribuciones categóricas quedan fuera
  del alcance de la señal.

## Qué hace en detalle

- Perfila una entrega y muestra faltantes, tipos, patrones, fechas y
  evidencia de datos personales.
- Perfila geometrías `sf` y declara CRS, familias geométricas, vacíos,
  validez planar, dominio de coordenadas y alcance de la caja
  envolvente; no hace análisis espacial. Sobre PostGIS lee el SRID de
  cada valor EWKB o EWKT: una columna con SRID mixto queda declarada
  como tal y su dominio se evalúa por grupos, porque una columna
  [`sf::sfc`](https://r-spatial.github.io/sf/reference/sfc.html) sólo
  puede tener un CRS. Un valor que no se puede convertir no borra la
  columna: se mide el resto y la pérdida se declara con su cifra.
- Evalúa la ley de Benford sólo cuando se cumplen sus precondiciones y
  registra los casos no aplicables en `cobertura_diagnosticos`.
- Informa `unidades_mixtas` y `monedas_mixtas` en una columna sin
  convertir valores ni suponer tasas de cambio.
- Informa `celdas_multivaluadas` sólo cuando las partes homogéneas
  coinciden con los patrones de la columna.
- Encuentra `relacion_aritmetica_columnas` como identidades o
  proporciones observadas entre columnas numéricas, no como reglas del
  dominio, y `relacion_orden_columnas` entre columnas comparables, con
  el alcance de la comparación.
- Encuentra claves, relaciones, dependencias y granularidades de
  medición que nunca fueron declaradas.
- Informa `casi_clave` cuando una columna no temporal tiene al menos 100
  filas, es casi única y sus colisiones se concentran en pocos valores:
  eso separa una clave con duplicados de un texto libre de cardinalidad
  alta.
- Permite que cada proyecto defina su marco de calidad, sin imponer un
  puntaje global.
- Mide y evalúa métricas, escalas, reglas de validez y dominios
  referenciales explícitos. Un contrato al que le falta un campo se
  abstiene y declara cuál en `cobertura_metricas`, sin abortar la
  medición de las demás. Una métrica cuyo método falla sobre los datos
  queda `no_medible`, con el mensaje del método y un aviso. Una métrica
  que compara la columna con un valor temporal declarado avisa si las
  dos puntas no son de la misma clase: un `Date` se ancla a la
  medianoche UTC y un `POSIXct` vale por su instante.
- Produce planes de limpieza editables, aplica sólo las acciones
  elegidas sobre una copia y conserva un registro de lo aplicado.
- Encuentra duplicados aproximados con teselas exactas, MinHash/LSH
  determinista, bloqueo, estimación previa del costo y lotes en disco
  ([Escala y
  duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)).
- Repara codificación dañada en R, incluidos el mojibake repetido y
  CESU-8, y rechaza las conversiones inseguras con pérdida
  ([Codificación y
  locales](https://sebollin.github.io/lupa/articles/codificacion-y-locales.html)).
- Sigue la calidad en el tiempo y crea informes HTML autocontenidos.

## El vocabulario completo de `tipo_hallazgo`

El paquete emite 59 nombres de `tipo_hallazgo`. Están en castellano
porque forman parte de la API pública: cambiar uno es un cambio
incompatible.

La lista se calcula al construir esta página a partir del código del
paquete. Se recorren las funciones de `lupa` y se juntan los nombres que
llegan como tipo al constructor interno de hallazgos, más los que se
asignan a un campo `tipo_hallazgo` o `hallazgo` (los tipos de pares de
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
y las dos reglas de fechas personales). La prueba
`tests/testthat/test-vocabulario-publicado.R` lleva una lista explícita
de los tipos emitidos; si las dos no coinciden, la página no se
construye.

``` r

mitad <- ceiling(length(tipos) / 2)
izquierda <- tipos[seq_len(mitad)]
derecha <- c(tipos[-seq_len(mitad)], rep("", 2 * mitad - length(tipos)))
writeLines(paste0(formatC(izquierda, width = -40), derecha))
#> alta_cardinalidad                       formatos_fecha_mixtos
#> anio_de_dos_digitos                     geometria_invalida
#> bloqueo_por_con_perdida                 geometria_vacia
#> casi_clave                              integer64_fuera_precision_double
#> casi_duplicados_vocabulario             mayusculas_inconsistentes
#> celdas_multivaluadas                    monedas_mixtas
#> ceros_no_permitidos                     negativos_no_permitidos
#> clave_con_ausentes                      nombres_columnas_problematicos
#> clave_no_unica                          normalizacion_unicode
#> codificacion_invalida                   numero_como_texto
#> codificacion_rota                       outliers
#> columnas_duplicadas                     patron_raro
#> constante                               posible_ausencia_estructural
#> controles_invisibles                    posible_centinela_numerico
#> coordenada_fuera_dominio                posible_identificador
#> crs_no_declarado                        regla_silencia_ausencia
#> dato_personal_posible                   relacion_aritmetica_columnas
#> desviacion_benford                      relacion_orden_columnas
#> duplicados_aproximados                  separadores_en_campo
#> duplicados_exactos_columnas             tipo_compuesto_no_analizado
#> duplicados_exactos_normalizados         tipo_declarado_distinto
#> entidades_html                          tipos_geometria_mixtos
#> espacios_sobrantes                      unidades_mixtas
#> faltantes                               valor_concentrado
#> faltantes_disfrazados                   valor_fuera_de_aplicabilidad
#> fecha_fallecimiento_fuera_rango         valores_no_finitos
#> fecha_nacimiento_fuera_rango            valores_subnormales
#> fecha_partida_columnas                  variantes_equifrecuentes_vocabulario
#> filas_duplicadas                        zona_horaria_fecha_hora
#> formato_fecha_ambiguo
```

## Qué se enmascara y con qué alcance

La protección de datos personales complementa el perfil: sirve para
medir la calidad sin publicar los valores de una persona. Es un
complemento que se pide (`proteger_datos_personales` vale `FALSE` por
omisión en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
en
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)):
publicar o no los valores de una base es decisión de quien la usa. Con
la protección pedida, una columna se protege cuando el paquete la
clasifica como posible dato personal o cuando se la declara en
`columnas_personales`; lo declarado gana sobre lo inferido, porque
ningún léxico de nombres de columna es completo.

En una columna protegida:

- se suprimen los estadísticos que podrían ser valores de una persona
  (mínimo, máximo, mediana, moda, media y desvío), y la tabla lo marca;
- la descripción, la evidencia y la sugerencia de cada hallazgo, el
  motivo y el `como_resolverlo` de la cobertura, los parámetros de una
  acción del plan y los ejemplos que
  [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md)
  imprime por consola reemplazan el valor;
- de una geometría protegida no se publica la caja envolvente: los
  cuatro `bbox_*` quedan en `NA` con
  `bbox_alcance = "no_publicado_por_geometria_protegida"`;
- una columna de la `clave` clasificada como dato personal vuelve
  enmascarada en la trazabilidad, y `claves_protegidas` dice cuál;
- una métrica referencial contra un padrón de personas publica
  `[valor protegido]` en lugar del candidato más cercano y conserva la
  distancia.

En todos los casos se conserva la señal y se oculta el valor: el ejemplo
sigue mostrando qué filas coinciden y en qué columnas. El alcance es
**por columna**: se reemplaza lo que describe a la columna protegida, no
toda aparición de ese texto. Lo que no tiene columna a la que
atribuirse, como un hallazgo de filas duplicadas, queda enmascarado en
todas partes.

El reemplazo reconoce tres formas del mismo valor: la exacta, la que
sólo difiere en separadores o en la caja de sus letras ASCII, y la que
comparte con un valor protegido una corrida de seis dígitos o más (un
documento sin su dígito verificador). Los valores se reemplazan de más
largo a más corto, así que un valor protegido que es prefijo de otro no
deja publicada la cola del más largo, y los valores que una celda de
`ejemplos` publica unidos se separan antes de buscarlos.

En el histórico, lo que la protección tapa no se compara, pero queda
declarado en el atributo `campos_protegidos`, y un cambio de protección
entre entregas sale con su propia fila (`aspecto = "proteccion"`).

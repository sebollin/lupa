# Perfilar una muestra leída mediante DBI

Calcula en SQL un resumen sobre la tabla completa o sobre una relación
muestreada por el motor, según `modo`, y, por omisión, en un bloque
separado ejecuta
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sobre una muestra traída a memoria. El resumen completo de 105 campos no
se presenta como calculado por la base: esos campos pertenecen
exclusivamente a `perfil_muestra` y su universo es la muestra.
`bloque_muestra = "solo_agregados"` permite omitir esa lectura y pedir
sólo los agregados SQL.

## Usage

``` r
perfilar_dbi(
  conexion,
  tabla,
  muestra = Inf,
  orden_muestra = NULL,
  modo = c("exacto", "seguro", "conteos", "muestreado", "aproximado"),
  metricas = NULL,
  max_consultas = Inf,
  dialecto = "auto",
  incluir_valores = TRUE,
  tamano_lote = NULL,
  tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
  tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
  bloque_muestra = c("con_muestra", "solo_agregados"),
  instrumentar = TRUE,
  politica_costo = c("todas", "ninguna", "por_cardinalidad", "cardinalidad"),
  umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
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
  `meta` lo declara expresamente. No se usa cuando
  `bloque_muestra = "solo_agregados"`.

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

  Cantidad máxima de columnas por consulta consolidada. Se conserva por
  compatibilidad y, si se informa, fija el tamaño de las dos familias.
  Para control separado, usar `tamano_lote_planos` y
  `tamano_lote_distintos`.

- tamano_lote_planos:

  Cantidad máxima de columnas por consulta de agregados planos. El valor
  por omisión es 20.

- tamano_lote_distintos:

  Cantidad máxima de columnas por consulta de cardinalidades exactas. El
  valor por omisión es 1, deliberadamente conservador hasta contar con
  mediciones comparables: una sola cardinalidad puede forzar un agregado
  pesado y derramar mucho más que un lote plano.

- bloque_muestra:

  Qué bloques se solicitan: `"con_muestra"` (por omisión) calcula
  también `perfil_muestra`, o `"solo_agregados"` omite su lectura y
  devuelve sólo los agregados SQL. La segunda opción no cambia el
  alcance de esos agregados: eso lo decide `modo`.

- instrumentar:

  Si se cronometra cada consulta y las etapas grandes de R. Por omisión
  es `TRUE`; agrega `duracion_ms`, `cpu_ms`, `n_filas_resultado`,
  `bytes_resultado_r`, `consulta_id` y `etapa` a `resumen_tabla$sql`, y
  el resumen `resumen_tabla$tiempos`. Con `FALSE` se conserva el mismo
  plan, la misma cantidad y el mismo orden de consultas, pero los campos
  medibles quedan en `NA`. `id_muestra` **no** depende de esta opcion:
  no es una medicion sino un hecho estructural sobre que consulta
  produjo cada metrica, y se publica igual con `FALSE`. Las duraciones
  usan [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) y el CPU del
  cliente usa la suma de `proc.time()[c("user.self", "sys.self")]`.
  `cpu_ms` es cero cuando el proceso no consumió CPU; `NA` significa que
  no se pudo medir. Los intervalos que el reloj no puede resolver no se
  publican como cero.

- politica_costo:

  Política optativa para las métricas caras. El valor por omisión,
  `"todas"`, conserva moda y mediana para todas las columnas
  solicitadas. `"ninguna"` es un alias de `"todas"`;
  `"por_cardinalidad"` (también `"cardinalidad"`) mide primero valores
  válidos y distintos cuando no hay una fuente exacta utilizable y
  omite, por columna, moda y mediana cuando la proporción de distintos
  alcanza `umbral_cardinalidad`.

- umbral_cardinalidad:

  Proporción entre valores distintos y válidos que activa
  `politica_costo = "por_cardinalidad"`. El valor por omisión es `0.95`
  sólo cuando esa política se pide explícitamente; se puede mover en
  cada llamada. Para pedir todas las métricas use
  `politica_costo = "todas"`.

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para analizar la muestra.

## Value

Objeto de clase `perfil_dbi` con dos componentes: `resumen_tabla`, de
alcance completo o muestreado según `modo`, y `perfil_muestra`, un
objeto `perfil` cuyo `meta$origen_dbi` declara tabla, conexión, SQL y
alcance. `perfil_muestra` es `NULL` si la muestra no se pudo obtener o
si se pidió `bloque_muestra = "solo_agregados"`;
`resumen_tabla$cobertura` distingue esos casos con `no_disponible` y
`no_solicitado`, respectivamente.

## Details

Esta función no escribe en la conexión ni crea objetos temporales. `DBI`
es una dependencia opcional. Cada agregado no disponible queda en `NA` y
su consulta, estado y motivo se conservan en `resumen_tabla$sql`. Las
expresiones se ejecutan como capacidades a comprobar, no como un
dialecto SQL universal.

## Dos tablas se llaman cobertura

El resultado trae dos, y cubren cosas distintas.
`resumen_tabla$cobertura` habla de **métricas SQL**: qué pidió esta
función al motor y qué pasó, con `bloque`, `elemento`, `estado`
—`no_disponible`, `no_solicitado`, `degradado`, `presupuesto_agotado`,
`alcance_distinto`— y la consulta en `sql`.
`perfil_muestra$cobertura_diagnosticos` habla de **diagnósticos**: qué
comprobación no se corrió sobre la muestra y por qué, con `diagnostico`,
`columna`, `motivo` y `como_resolverlo`, el mismo esquema que devuelve
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
Un motor que rechaza una columna aparece en la primera; una prueba
estadística que no corresponde a esa columna, en la segunda. Comparten
la palabra y no el vocabulario, así que conviene mirar cuál se está
leyendo. Si se omite el bloque con `bloque_muestra = "solo_agregados"`,
la cobertura usa el estado `no_solicitado`: no es un fallo ni se cuenta
como una métrica no disponible.

## Fallo parcial

Ningún bloque descarta al otro. Si se pide la muestra pero el motor
rechaza su consulta, o si la muestra no se puede perfilar, el resultado
sale igual con `perfil_muestra = NULL` y una fila en
`resumen_tabla$cobertura` que declara el estado `no_disponible`, el
motivo y cómo resolverlo. Si se pide sólo agregados,
`perfil_muestra = NULL` se acompaña de una fila `no_solicitado`: no se
intentó leer la muestra. Si el motor rechaza una columna, esa columna
queda con sus métricas en `no_disponible` y las demás se perfilan
enteras. Los [`stop()`](https://rdrr.io/r/base/stop.html) de esta vía
llevan clase de condición propia —todas heredan de `lupa_error_dbi`—
para que se puedan rescatar con
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
En `resumen_tabla$meta$muestreo`, `tamano_muestra` conserva el nombre
historico y declara el tamano efectivo solicitado a la consulta;
`filas_solicitadas` declara el pedido original y `filas_obtenidas` las
filas que devolvio la lectura del bloque `perfil_muestra`. Esta ultima
puede ser `NA` si el bloque no se solicito o fallo antes de leer.

En una muestra, `error_esperado` vale `no_estimado` para metricas cuyo
error podria calcularse bajo un plan probabilistico pero no se calculo,
`no_estimable` para la moda, la mediana y la cardinalidad observada, que
no tienen una cota simple sin supuestos o un estimador declarado, y
`no_aplica` cuando no hubo muestreo efectivo. El `motivo` de cada
registro explica la distincion. `metodo`, `tamano_muestra` y `fraccion`
conservan las condiciones de la corrida; no se publica una cota numerica
sin una formula justificada. Los distintos de una muestra se publican
como cardinalidad de la muestra, no como cardinalidad del universo.
`modo = "aproximado"` sondea `APPROX_COUNT_DISTINCT`,
`approx_count_distinct` y las formas de cuantiles del motor; cuando
ninguna responde usa el respaldo exacto y lo registra por metrica. Las
cotas de error no documentadas de una aproximacion nativa quedan como
`"desconocido"`. Una aproximacion solo se consolida cuando entrega una
expresion que se puede incrustar en el `SELECT`; si solo construye una
consulta completa, se emite por separado. Una consulta no emitida o sin
un valor utilizable queda `no_disponible`.

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
que se trae a R, no el trabajo del motor. `bloque_muestra` decide si se
trae esa muestra; `modo`, `metricas`, `tamano_lote_planos`,
`tamano_lote_distintos` y `max_consultas` acotan el trabajo SQL.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
dice cuántas consultas se van a emitir antes de emitirlas. El orden de
degradación es agregados planos, total exacto fusionado, distintos, moda
y mediana. Los agregados planos sobre la misma tabla y filtro
—`COUNT(col)`, mínimos, máximos, medias, ceros, negativos y desvío—
comparten una consulta por lote; cuando la fuente no necesita el total
por adelantado, la primera consulta de agregados lleva además el
`COUNT(*)` exacto con el alias `lupa_n_total`. Si el lote completo es
rechazado, se emite un `COUNT(*)` solo como repliegue obligatorio y sus
mitades se sondean por bisección: los grupos aceptados se reutilizan
como mediciones y las columnas culpables se reintentan por métrica. El
denominador de completitud nunca se estima a partir de un catálogo ni de
un lote parcial. Las fuentes `TABLESAMPLE` que necesitan el total para
escribir un porcentaje lo cuentan antes y no reclaman este ahorro.
`COUNT(DISTINCT ...)` queda en una clase separada y usa su propio tamaño
de lote, conservador por omisión porque una cardinalidad puede derramar
mucho más que veinte agregados planos. Lo que no entra en el presupuesto
queda en `no_disponible` con su motivo, nunca en cero.
`meta$tamano_lote_funciono` conserva el mayor lote aceptado durante esa
corrida; no se guarda estado global asociado a la conexión.

## Instrumentación

`resumen_tabla$sql` conserva una fila por métrica y agrega la duración
de la consulta que la respalda, las filas devueltas y los bytes que ese
resultado ocupa en R. `consulta_id` identifica el intento dentro de la
corrida y `id_muestra` identifica la consulta de datos que produjo la
medición: dos métricas con el mismo identificador vieron exactamente las
mismas filas y se pueden comparar directamente. `NA` declara que esa
garantía no se puede hacer; en particular, las métricas por columna
—moda, frecuencia de la moda y mediana— no comparten filas con otras
métricas. `etapa` permite agruparlo (`conteos`, `moda`, `basicos`,
`mediana`, `desvio`, `lectura_muestra` y las sondas). Las métricas no
solicitadas o que no emitieron consulta conservan esos campos y los
dejan en `NA`; en particular, `NA` no significa cero.

`resumen_tabla$tiempos` reúne las etapas grandes del cliente en las
mismas unidades (`duracion_ms`): `lectura_muestra`, `perfilado_muestra`,
`perfilado_columnas`, y los análisis opcionales cuando se solicitan. Una
etapa apagada queda con estado `no_solicitado`; si la instrumentación se
apaga queda con estado `no_medido` y duración `NA`. La columna `nivel`
dice cuáles se pueden sumar: las de `nivel = 1` son disjuntas entre sí,
y las de nivel mayor están contenidas en alguna de ellas.
`perfilado_muestra` es inclusivo -contiene el perfilado por columna, las
dependencias y los casi-duplicados-, así que sumar la columna entera da
más que la corrida.

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

## Progreso

Traer la tabla entera puede tardar minutos sobre una tabla grande, y una
corrida callada no se distingue de una colgada. En una sesión
interactiva se muestra una barra que avanza contra las consultas que el
plan dice que se van a emitir —un total conocido, no una estimación—, y
no aparece cuando la corrida es de menos de una docena de consultas,
porque termina antes de que sirva.

`options(lupa.progreso = TRUE)` la fuerza y `FALSE` la apaga; fuera de
una sesión interactiva está apagada, para que la salida de un guion no
traiga ruido que despues haya que filtrar. No cambia ningún valor de lo
que se mide: hay una prueba que compara el perfil con la barra y sin
ella.

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

# Perfilar una muestra leída mediante DBI

Calcula en SQL un resumen sobre la tabla completa o sobre una relación
muestreada por el motor, según `modo`, y, por omisión, en un bloque
separado ejecuta
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sobre una muestra traída a memoria. El resumen completo de 109 campos
analíticos además del nombre de la columna no se presenta como calculado
por la base: esos campos pertenecen exclusivamente a `perfil_muestra` y
su universo es la muestra. `bloque_muestra = "solo_agregados"` permite
omitir esa lectura y pedir sólo los agregados SQL.

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
  estrategia_distintos = "exacta",
  politica_costo = c("todas", "ninguna", "por_cardinalidad", "cardinalidad"),
  umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
  avisar_costo_distintos = TRUE,
  umbral_segundos_aviso_distintos = .UMBRAL_SEGUNDOS_AVISO_DISTINTOS_DBI,
  avisar_costo_moda = TRUE,
  umbral_segundos_aviso_moda = .UMBRAL_SEGUNDOS_AVISO_MODA_DBI,
  avisar_costo_mediana = TRUE,
  umbral_segundos_aviso_mediana = .UMBRAL_SEGUNDOS_AVISO_MEDIANA_DBI,
  avisar_derrame_estimado = TRUE,
  umbral_bytes_aviso_derrame_estimado = .UMBRAL_BYTES_AVISO_DERRAME_ESTIMADO_DBI,
  max_celdas_muestra = .MAX_CELDAS_MUESTRA,
  max_bytes_muestra = .MAX_BYTES_MUESTRA,
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
  `Inf` para traer la tabla entera. El valor por omisión ya es `Inf`: no
  representa una elección distinta de `Inf`, sino la tabla completa. Con
  `Inf` la consulta sale sin `LIMIT` y `tabla_completa` queda en `TRUE`.

  El resumen de tabla **no** se muestrea: con `modo = "exacto"` se
  calcula en el motor sobre todas las filas. Lo que sale de esta muestra
  son los diagnosticos que necesitan los valores en R -patrones,
  formatos, casi-duplicados y dependencias funcionales-, y sin
  `orden_muestra` no son una muestra aleatoria sino las primeras filas
  que devuelva el motor. El limite también alcanza la muestra común con
  que se buscan dependencias. Use un entero finito para acotar ese
  trabajo; `Inf` es el valor por omisión y trae la tabla entera cuando
  el tiempo no es la restricción.

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
  `"aproximado"` usa funciones nativas aproximadas para las métricas que
  ese modo define.

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
  valor por omisión es 2, medido sobre el servidor de referencia: el
  `Shared Read` fue constante entre lotes y el costo por columna fue
  casi igual para uno y dos, mientras el lote de dos derramó menos que
  los lotes mayores. Una sola cardinalidad todavía puede forzar un
  agregado pesado y derramar mucho más que un lote plano.

- bloque_muestra:

  Qué bloques se solicitan: `"con_muestra"` (por omisión) calcula
  también `perfil_muestra`, o `"solo_agregados"` omite su lectura y
  devuelve sólo los agregados SQL. La segunda opción no cambia el
  alcance de esos agregados: eso lo decide `modo`.

- instrumentar:

  Si se cronometra cada consulta y las etapas grandes de R y, en
  PostgreSQL, se intenta atribuir el uso de bloques temporales de los
  `COUNT(DISTINCT)` exactos mediante `pg_stat_statements`. Por omisión
  es `TRUE`; agrega `duracion_ms`, `cpu_ms`, `n_filas_resultado`,
  `bytes_resultado_r`, `consulta_id`, `etapa` y `nivel` a
  `resumen_tabla$sql`, y el resumen `resumen_tabla$tiempos`. Con `FALSE`
  se conserva el mismo plan, la misma cantidad y el mismo orden de
  consultas, pero los campos medibles quedan en `NA`. `id_muestra`
  **no** depende de esta opcion: no es una medicion sino un hecho
  estructural sobre que consulta produjo cada metrica, y se publica
  igual con `FALSE`. Las duraciones usan
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) y el CPU del
  cliente usa la suma de `proc.time()[c("user.self", "sys.self")]`.
  `cpu_ms` es cero cuando el proceso no consumió CPU; `NA` significa que
  no se pudo medir. Los intervalos que el reloj no puede resolver no se
  publican como cero.

- estrategia_distintos:

  Procedencia explícita para `n_distintos`: `"exacta"` (por omisión)
  emite `COUNT(DISTINCT)`; `"aproximada_motor"` usa una función nativa
  aceptada por el motor y deja la métrica en `no_disponible` si no
  existe; `"catalogo"` lee `pg_stats.n_distinct` en PostgreSQL y publica
  el resultado como `estimado_catalogo`, nunca como medición, cuando el
  modo mide la relación entera (`exacto`, `seguro` o `conteos`). En
  `muestreado` y `aproximado` queda `no_disponible`, porque el catálogo
  describe la relación entera y la corrida mide un subconjunto; y
  `"omitida"` no emite ninguna consulta. No hay repliegue automático
  entre estrategias. El resultado publica `estrategia_solicitada`,
  `estrategia_resuelta` y `estado` en `meta$estrategia_distintos`, y las
  dos primeras también en `resumen_tabla$sql`. En `pg_stats`, un valor
  positivo es el conteo estimado y uno negativo es una fracción de las
  filas. Cuando la relación tiene descendientes se elige
  `inherited = TRUE`, porque esa fila describe lo que lee una consulta
  sin `ONLY`; una relación sin hijas usa su única fila propia. Las
  fracciones se convierten con la suma de `pg_class.reltuples` de la
  jerarquía. Si no hay una fila utilizable —por ejemplo, antes de
  `ANALYZE`— o hay ambigüedad, la métrica queda `no_disponible`, no en
  cero.

- politica_costo:

  Política optativa para las métricas caras. El valor por omisión,
  `"todas"`, conserva moda y mediana para todas las columnas
  solicitadas. `"ninguna"` es un alias de `"todas"`;
  `"por_cardinalidad"` (también `"cardinalidad"`) resuelve primero las
  fuentes estructurales y mide valores válidos y distintos sólo cuando
  hace falta y la estrategia lo permite. Luego omite, por columna, sólo
  la moda cuando la proporción de distintos alcanza
  `umbral_cardinalidad`; la mediana se conserva porque las mediciones
  disponibles muestran que su costo depende de las filas y no de la
  cardinalidad. Una estrategia no disponible no se convierte en una
  medición exacta.

- umbral_cardinalidad:

  Proporción entre valores distintos y válidos que activa la omisión de
  la moda con `politica_costo = "por_cardinalidad"`. El valor por
  omisión es `0.5` sólo cuando esa política se pide explícitamente; se
  puede mover en cada llamada. Este argumento no gobierna la mediana:
  `meta$decisiones_costo` explica la decisión de cada métrica por
  separado. Para pedir todas las métricas use
  `politica_costo = "todas"`.

- avisar_costo_distintos:

  Si es `TRUE`, avisa, después de medir el primer lote y antes del
  segundo, cuando la proyección del costo de `COUNT(DISTINCT)` alcanza
  `umbral_segundos_aviso_distintos`. Por omisión es `TRUE`. Este aviso
  no depende de
  [`interactive()`](https://rdrr.io/r/base/interactive.html): también
  llega en guiones porque el costo se paga en el servidor y puede durar
  decenas de segundos.

- umbral_segundos_aviso_distintos:

  Segundos estimados a partir de los cuales se emite el aviso del costo
  de `COUNT(DISTINCT)`, después del primer lote. Por omisión es `30`, el
  umbral histórico; `Inf` lo desactiva explícitamente. Con un solo lote
  no se publica una proyección porque el costo ya se pagó. El valor no
  cambia la proyección ni la medición que se publica.

- avisar_costo_moda:

  Si es `TRUE`, avisa antes de ejecutar las modas pendientes cuando su
  proyección alcanza `umbral_segundos_aviso_moda`. Por omisión es
  `TRUE`. La tasa se mide con modas anteriores de esta corrida; si falta
  cardinalidad, se declara en la proyección y no se supone.

- umbral_segundos_aviso_moda:

  Segundos estimados a partir de los cuales se emite el aviso de la
  moda. Por omisión es `30`; `Inf` lo desactiva explícitamente. El valor
  no cambia la medición ni la proyección publicada.

- avisar_costo_mediana:

  Si es `TRUE`, avisa antes de ejecutar las medianas pendientes cuando
  su proyección alcanza `umbral_segundos_aviso_mediana`. Por omisión es
  `TRUE`. La proyección sigue las filas, no la cardinalidad.

- umbral_segundos_aviso_mediana:

  Segundos estimados a partir de los cuales se emite el aviso de la
  mediana. Por omisión es `30`; `Inf` lo desactiva explícitamente.
  Cuando no existe una primera medición local, una sola mediana total
  usa la referencia de banco declarada de 68 ms por millón de filas de
  otra corrida. Si la consulta inicial que obtuvo las filas fue medida y
  da una cota mayor, se publica como cota de lectura, no como medición
  de mediana.

- avisar_derrame_estimado:

  Si es `TRUE`, avisa cuando un lote de `COUNT(DISTINCT)` supera la
  memoria efectiva y su tamaño estimado alcanza
  `umbral_bytes_aviso_derrame_estimado`. Por omisión es `TRUE`. Este
  aviso sólo puede aparecer cuando PostgreSQL permite estimar el hash.

- umbral_bytes_aviso_derrame_estimado:

  Tamaño estimado del hash, en bytes, a partir del cual se avisa un
  derrame potencial entre los lotes que ya superan la memoria efectiva.
  Por omisión es `0`, que conserva el aviso para cualquier lote que la
  supere; `Inf` lo desactiva explícitamente. El valor no cambia la
  estimación ni la medición posterior del derrame.

- max_celdas_muestra:

  Máximo de celdas que puede contener el bloque `perfil_muestra`. Por
  defecto es `1000000`; se calcula antes de leer como filas por columnas
  del esquema. Si reduce la muestra,
  `perfil_muestra$cobertura_diagnosticos` usa la misma declaración que
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  con las celdas solicitadas, el umbral y el tope que mandó. `Inf`
  desactiva este tope. No modifica los agregados SQL.

- max_bytes_muestra:

  Máximo de bytes de la muestra materializada que alimenta
  `perfil_muestra`. Por defecto es `512 MiB`. Como el tamaño no se
  conoce desde el esquema, primero se lee una sonda de hasta cien filas
  y con ella se fija el límite final en SQL o en `dbFetch(n)`, antes de
  leer el resto. Si reduce la muestra, `cobertura_diagnosticos` informa
  los bytes observados, el umbral y cuál tope mandó. `Inf` desactiva
  este tope. No modifica los agregados SQL.

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
`no_solicitado`, respectivamente. `resumen_tabla$meta$clave` conserva
siempre la respuesta del catálogo de la clave primaria: `columnas`,
`fuente`, `motivo`, `garantia` y `estado`. `garantia` puede ser
`garantizada`, `declarada_no_garantizada`, `desconocida` o
`no_declarada`; `estado` conserva, cuando el motor los expone,
`visible`, `restriccion_diferible`, `universo_incluye_descendientes` e
`indice_no_unico`. Una consulta fallida queda diferenciada de una clave
no declarada.

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
`alcance_distinto`— y la consulta en `sql`. `alcance_distinto` declara
que dos valores exactos incoherentes salieron de grupos de consistencia
distintos: es evidencia de que la tabla cambio durante la corrida, no
una acusacion contra el motor. `perfil_muestra$cobertura_diagnosticos`
habla de **diagnósticos**: qué comprobación no se corrió sobre la
muestra y por qué, con `diagnostico`, `columna`, `motivo` y
`como_resolverlo`, el mismo esquema que devuelve
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
puede ser `NA` si el bloque no se solicito o fallo antes de leer. Si la
consulta de la muestra devuelve cero filas, no hay base para medir las
metricas de alcance `muestra`: se publican con valor `NA`, estado
`no_disponible` y un motivo que nombra la muestra vacia. Esto no permite
concluir que la columna este vacia, por lo que no se publica cero ni se
dispara la cascada `sin_valores`. `n` conserva el conteo de la tabla
completa.

En una muestra, `error_esperado` vale `no_estimado` para metricas cuyo
error podria calcularse bajo un plan probabilistico pero no se calculo,
`no_estimable` para la moda, la mediana y la cardinalidad observada, que
no tienen una cota simple sin supuestos o un estimador declarado, y
`no_aplica` cuando no hubo muestreo efectivo. El `motivo` de cada
registro explica la distincion. `metodo`, `tamano_muestra` y `fraccion`
conservan las condiciones de la corrida; no se publica una cota numerica
sin una formula justificada. Los distintos de una muestra se publican
como cardinalidad de la muestra, no como cardinalidad del universo.
`estrategia_distintos` es explicita y vale `"exacta"` por omision:
calcula `COUNT(DISTINCT)` sobre las filas de la corrida.
`"aproximada_motor"` sondea una funcion nativa y, si no hay una
capacidad aceptada, deja la metrica `no_disponible`; nunca ejecuta el
conteo exacto como repliegue. `"catalogo"` lee `pg_stats.n_distinct` y
publica `estimado_catalogo` sólo cuando el modo mide la relación entera
(`exacto`, `seguro` o `conteos`). En `muestreado` y `aproximado` queda
`no_disponible`, porque el catálogo describe la relación entera y la
corrida mide un subconjunto; no se inventa una equivalencia entre
universos. `"omitida"` no emite la consulta. Cada resultado y el
atributo `meta$estrategia_distintos` separan `estrategia_solicitada`,
`estrategia_resuelta` y `estado`.

Las comparaciones que tienen una cota dura usan solo valores del mismo
grupo de consistencia. En esta version, el grupo queda probado por el
`consulta_id` que ya se registra en `resumen_tabla$sql`: dos metricas
con el mismo identificador salieron de la misma sentencia. La consulta
exacta de distintos trae `COUNT(columna) AS n_validos_guard` junto a
`COUNT(DISTINCT columna)`. Si una capacidad aproximada sólo construye
una consulta completa y no puede traer ese guardian, la cota no se
comprueba y el motivo lo declara; no se atribuye un valor imposible al
motor. La consulta de la moda intenta traer
`SUM(COUNT(*)) OVER () AS n_validos_guard` junto a su frecuencia. La
forma se sondea antes de usarla; si el motor la rechaza, se conserva la
consulta anterior y `meta$moda_guardian` publica el repliegue. Cuando la
sonda pasa, la cota `frecuencia_moda <= n_validos` se comprueba dentro
de la misma sentencia y el motivo de la métrica lo declara.
`meta$snapshot` queda en `FALSE`, siguiendo la declaracion de
colecciones: no hubo lectura instantanea. La cobertura agrega una
entrada concreta solo si `n_validos` y `n_distintos` son exactos,
incoherentes y provienen de grupos distintos; su motivo conserva ambas
sentencias.

Las cotas de error no documentadas de una aproximacion nativa quedan
como `"desconocido"`. Una aproximacion solo se consolida cuando entrega
una expresion que se puede incrustar en el `SELECT`; si solo construye
una consulta completa, se emite por separado. Una consulta no emitida o
sin un valor utilizable queda `no_disponible`.

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
pliegan a mayúsculas. Para los motores del dialecto `limit`, la mediana
exacta usa una sola sentencia: el `COUNT` queda como subconsulta escalar
de la consulta que ordena y recorta. La forma se sondea antes de usarla;
en SQLite y PostgreSQL se usan `%` y `/` con division entera. Los
dialectos que no declaran esa forma conservan las dos consultas y lo
publican en el metodo de `resumen_tabla$sql`; `PERCENTILE_CONT` no
cambia.

## Costo

Los agregados de una tabla ancha se emiten por lotes; `muestra` acota lo
que se trae a R, no el trabajo del motor. `bloque_muestra` decide si se
trae esa muestra; `modo`, `metricas`, `tamano_lote_planos`,
`tamano_lote_distintos` y `max_consultas` acotan el trabajo SQL.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
dice cuántas consultas se van a emitir antes de emitirlas. El orden de
degradación es agregados planos, total del universo cuando hace falta,
distintos, moda y mediana. Los agregados planos sobre la misma tabla y
filtro —`COUNT(col)`, mínimos, máximos, medias, ceros, negativos y
desvío— comparten una consulta por lote y cada consulta que trae
`n_validos` lleva además `COUNT(*) AS n_total_consulta` en la misma
sentencia. La completitud usa ese denominador local, no el total de otro
lote. La fusión conserva la medición, no una identidad bit a bit entre
agrupamientos: la media y el desvío pueden diferir en el último bit
según cómo se agrupen las sumas, porque la suma en punto flotante no es
asociativa.

El total del universo se conserva por separado cuando el perfil se
calcula sobre una muestra. Si el lote completo es rechazado, sus mitades
se sondean por bisección: los grupos aceptados se reutilizan como
mediciones y las columnas culpables se reintentan por métrica, con su
denominador local. Las fuentes `TABLESAMPLE` que necesitan el total del
universo para escribir un porcentaje lo cuentan antes.
`COUNT(DISTINCT ...)` queda en una clase separada y usa su propio tamaño
de lote, conservador por omisión porque una cardinalidad puede derramar
mucho más que veinte agregados planos; la consulta exacta trae su
`n_validos_guard` compañero. La proyección temporal no usa esos
agregados planos: si hay más de un lote y `instrumentar = TRUE`, se mide
el primer lote de distintos y, después de ejecutarlo, se multiplica su
mediana por la cantidad total de lotes. El aviso llega antes del segundo
lote, en la unidad que se va a evitar; con un solo lote no hay nada que
proyectar. Si la duración no se pudo medir, el resultado declara la
proyección como no disponible. La moda tiene otro canal: después de cada
moda medida se obtiene una tasa en ms por distinto y se usa para
proyectar las modas pendientes. La cardinalidad se toma del agregado de
la corrida, de una clave garantizada o de la estimación de catálogo que
esté disponible; si falta, `meta$costo_moda` lo declara y no inventa un
número. El aviso llega antes de la siguiente moda. La mediana se
proyecta en ms por fila. La primera mediana medida en esta corrida sirve
para proyectar las restantes y el aviso precede a ese trabajo. Si no
existe una primera medición local para una mediana total, usa la
referencia declarada de otra corrida de 68 ms por millón de filas. Si la
consulta inicial que obtuvo las filas fue medida y resulta una cota
mayor, se publica también esa cota de lectura —no como medición de
mediana— para no subestimar una tabla grande recién cargada. Las dos
proyecciones quedan separadas en `meta$costo_moda` y
`meta$costo_mediana`; apagar el aviso no apaga su medición ni su
metadata. Antes de la primera consulta exacta se estima, cuando
PostgreSQL expone `pg_stats`, el tamaño de los hashes con `n_distinct`,
`avg_width` y `pg_class.reltuples`; `SHOW work_mem` y, desde PostgreSQL
13, `SHOW hash_mem_multiplier` dan el límite efectivo.
`meta$estimacion_derrame` y `attr(meta$plan, "estimacion_derrame")`
conservan el diagnóstico, siempre rotulado como estimación y nunca como
derrame medido. Si supera el límite se avisa antes de pagar
`COUNT(DISTINCT)` y se recomienda subir `work_mem` en la sesión; el
paquete no lo modifica. Una estadística ausente, un permiso insuficiente
o una versión sin el parámetro dejan la parte correspondiente como no
disponible. `n_distinct` es una estimación de muestra y puede quedar
corta; si luego `pg_stat_statements` mide un derrame, esa medición
manda.

Los avisos de esta vía son deliberadamente distintos de los de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
`perfilar_dbi()` los emite también en guiones no interactivos porque el
costo relevante ocurre en el servidor y puede consumir decenas de
segundos antes de que el llamador pueda hacer algo. En cambio, el aviso
de tabla ancha de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
estima trabajo local sobre una tabla ya en R y queda limitado a sesiones
interactivas para no convertir la salida de un guion en ruido. Cada
aviso DBI tiene su propio interruptor y umbral porque sus unidades no
son comparables —segundos frente a bytes— y silenciar uno no debe
ocultar el otro. Apagar un aviso no apaga ninguna medición:
`meta$costo_distintos`, `meta$costo_moda`, `meta$costo_mediana`,
`meta$derrame` y `meta$estimacion_derrame` se publican igual. Lo que no
entra en el presupuesto queda en `no_disponible` con su motivo, nunca en
cero. `meta$tamano_lote_funciono` conserva el mayor lote aceptado
durante esa corrida; no se guarda estado global asociado a la conexión.

## Instrumentación

`resumen_tabla$sql` conserva una fila por métrica y agrega la duración
de la consulta que la respalda, las filas devueltas y los bytes que ese
resultado ocupa en R. `consulta_id` identifica el intento dentro de la
corrida y `duracion_ms` es la duración de la consulta, repetida en cada
fila que esa consulta produjo; no es una duración por métrica. La
columna `nivel` marca qué filas se pueden sumar: la primera fila de cada
`consulta_id` queda en `nivel = 1` y sus repeticiones en `nivel = 2`.
Por eso una suma segura usa sólo `duracion_ms[nivel == 1]` (con
`na.rm = TRUE` si corresponde), no la columna completa. `id_muestra`
identifica la consulta de datos que produjo la medición: dos métricas
con el mismo identificador vieron exactamente las mismas filas y se
pueden comparar directamente. `NA` declara que esa garantía no se puede
hacer; en particular, las métricas por columna —moda, frecuencia de la
moda y mediana— no comparten filas con otras métricas. `etapa` permite
agruparlo (`conteos`, `moda`, `basicos`, `mediana`, `desvio`,
`lectura_muestra` y las sondas). Las métricas no solicitadas o que no
emitieron consulta conservan esos campos y los dejan en `NA`; en
particular, `NA` no significa cero.

En PostgreSQL, con `instrumentar = TRUE`, se toma una foto de
`pg_stat_statements` antes y después de los `COUNT(DISTINCT)` exactos.
Sólo se publica un derrame cuando una consulta coincide y su contador
aumentó en exactamente una llamada atribuible a esta corrida. En ese
caso, `resumen_tabla$sql` agrega `derrame`, `bloques_temporales_leidos`,
`bloques_temporales_escritos` y `fuente_derrame`;
`resumen_tabla$meta$derrame` conserva el resumen y la fuente. Si la
extensión no está disponible, la consulta fue concurrente o la
instrumentación está apagada, el estado queda `no_disponible` o
`no_medido` con el motivo: el paquete no deduce un derrame del tiempo y
no modifica `work_mem`.

`resumen_tabla$meta$estimacion_derrame` es un diagnóstico distinto:
conserva la estimación de memoria y siempre la rotula como no medida.
Puede quedar parcial o no disponible por permisos, falta de `ANALYZE`,
particiones sin estadísticas o un motor que no sea PostgreSQL. Si ambos
diagnósticos existen, `meta$derrame` es la evidencia posterior y
prevalece sobre la estimación; una estimación que no superó el límite no
contradice un derrame medido.

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

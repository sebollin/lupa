# Perfilar una muestra leída mediante DBI

Calcula en SQL un resumen sobre la tabla completa o sobre una relación
muestreada por el motor, según `universo`. Con
`universo = "muestra_motor"` y `bloque_muestra = "con_muestra"`, ejecuta
una sola selección y la materializa en un spool externo de la sesión
cliente; el resumen y
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
leen esa misma materialización. Con `bloque_muestra = "solo_agregados"`,
el resumen usa la relación muestreada sin seleccionar filas para el
cliente. `bloque_muestra = "solo_agregados"` sigue omitiendo el objeto
`perfil_muestra`, pero no vuelve a seleccionar filas.

## Usage

``` r
perfilar_dbi(
  conexion,
  tabla,
  universo = c("tabla_completa", "muestra_motor"),
  muestra_motor = NULL,
  muestra = Inf,
  orden_muestra = NULL,
  metricas = .METRICAS_DBI,
  estrategia_distintos = "exacta",
  estrategia_mediana = c("exacta", "aproximada_motor"),
  politica_costo = c("todas", "por_cardinalidad"),
  bloque_muestra = c("con_muestra", "solo_agregados"),
  max_consultas = Inf,
  dialecto = "auto",
  incluir_valores = TRUE,
  tamano_lote = NULL,
  tamano_lote_planos = .TAMANO_LOTE_PLANOS_DBI,
  tamano_lote_distintos = .TAMANO_LOTE_DISTINTOS_DBI,
  instrumentar = TRUE,
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
  bloque_filas = NULL,
  max_bytes_procesamiento = .MAX_BYTES_MUESTRA,
  max_bytes_materializacion = .MAX_BYTES_MUESTRA,
  columnas_opcionales = character(),
  aplicabilidad = NULL,
  sentinelas_numericos = c(-9, -99, -999, -9999, 999),
  ...
)
```

## Arguments

- conexion:

  Conexión abierta compatible con DBI.

- tabla:

  Nombre de tabla o un objeto aceptado por
  [`DBI::dbQuoteIdentifier()`](https://dbi.r-dbi.org/reference/dbQuoteIdentifier.html).

- universo:

  Universo sobre el que se calculan los agregados SQL:
  `"tabla_completa"` (por omisión) o `"muestra_motor"`. En el segundo
  caso, todas las métricas SQL usan la relación muestreada por el motor
  y no se reemplazan silenciosamente por resultados de la tabla
  completa.

- muestra_motor:

  Cantidad positiva y finita de filas que el motor debe tomar cuando
  `universo = "muestra_motor"`. Es obligatorio en ese universo y debe
  quedar `NULL` para `"tabla_completa"`; la función rechaza temprano
  valores no enteros, no positivos o `Inf`. Si una vista falla en una
  fila, el resumen conserva el alcance de la materialización que sí se
  pudo leer: las columnas legibles se perfilan y la columna rota queda
  `no_disponible`; la cobertura no presenta esa lectura parcial como una
  lectura completa de la vista.

- muestra:

  Cantidad positiva de filas solicitadas para el perfil de muestra que
  se trae a R, o `Inf` para traer la tabla entera. Este límite es
  independiente de `universo`: en `muestra_motor`, `muestra_motor`
  decide las filas del resumen SQL y `muestra` decide las filas del
  bloque `perfil_muestra`. En `tabla_completa`, `muestra` no cambia los
  agregados. Cuando `universo = "muestra_motor"`, el resumen usa toda la
  materialización del motor y este argumento solo recorta el diagnóstico
  en R; por eso `meta$materializacion$n_filas` puede ser mayor que
  `perfil_muestra$meta$filas`.

  Sin `orden_muestra`, las filas del bloque en R no son una muestra
  aleatoria garantizada sino las primeras que devuelva el motor. El
  límite también alcanza la muestra común con que se buscan
  dependencias. Use un entero finito para acotar ese trabajo cuando el
  tiempo no sea la restricción.

- orden_muestra:

  Columnas para `ORDER BY`. La salida solo declara orden reproducible
  cuando la combinación es única en toda la tabla. Sin este argumento,
  DBI no garantiza el orden ni la pertenencia de una muestra limitada, y
  `meta` lo declara expresamente. No se usa cuando
  `bloque_muestra = "solo_agregados"`. Con la lectura por bloques, la
  identidad de la fuente por bloques gobierna el recorrido y este pedido
  queda declarado en `meta$orden_muestra` con el motivo estable de que
  no gobierna esa via. Si el motor falla al evaluar una columna del
  `ORDER BY`, la vía prueba cada componente, recupera las columnas
  legibles sin ese orden cuando puede y publica `alcance_distinto` con
  el motivo de que el orden pedido no se pudo aplicar; no lo presenta
  como si ninguna columna fallara.

- metricas:

  Selección explícita de grupos de métricas: `"validos"`, `"distintos"`,
  `"moda"`, `"basicos"`, `"mediana"` y `"desvio"`. El valor por omisión
  solicita las seis. `c("validos", "basicos", "desvio")` omite
  distintos, moda y mediana, y `"validos"` sólo cuenta los no nulos.

- estrategia_distintos:

  Procedencia explícita para `n_distintos`: `"exacta"` (por omisión)
  emite `COUNT(DISTINCT)`; `"aproximada_motor"` usa una función nativa
  aceptada por el motor y deja la métrica en `no_disponible` si no
  existe; `"catalogo"` lee `pg_stats.n_distinct` en PostgreSQL y publica
  el resultado como `estimado_catalogo`, nunca como medición, cuando
  `universo = "tabla_completa"`. En `muestra_motor` queda
  `no_disponible`, porque el catálogo describe la relación entera y la
  corrida mide un subconjunto; y `"omitida"` no emite ninguna consulta.
  No hay repliegue automático entre estrategias. El resultado publica
  `estrategia_solicitada`, `estrategia_resuelta` y `estado` en
  `meta$estrategia_distintos`, y las dos primeras también en
  `resumen_tabla$sql`. En `pg_stats`, un valor positivo es el conteo
  estimado y uno negativo es una fracción de las filas. Cuando la
  relación tiene descendientes se elige `inherited = TRUE`, porque esa
  fila describe lo que lee una consulta sin `ONLY`; una relación sin
  hijas usa su única fila propia. Las fracciones se convierten con la
  suma de `pg_class.reltuples` de la jerarquía. Si no hay una fila
  utilizable —por ejemplo, antes de `ANALYZE`— o hay ambigüedad, la
  métrica queda `no_disponible`, no en cero.

- estrategia_mediana:

  Preferencia para resolver `mediana`: `"exacta"` (por omisión) o
  `"aproximada_motor"`. La sonda prueba siempre primero una forma nativa
  exacta consolidada, luego una forma exacta por columna y deja las
  funciones nativas aproximadas para el final. Por eso esta opción
  describe la estrategia habilitada, no garantiza el método ejecutado:
  `meta$estrategia_mediana` y `resumen_tabla$sql$metodo` publican el
  método que efectivamente corrió. Una mediana resuelta por una forma
  exacta queda `estado = "calculado"` y `error_esperado = "no_aplica"`,
  aunque se haya pedido `"aproximada_motor"`; sólo una aproximación
  ejecutada queda `estado = "estimado"`. `universo = "muestra_motor"`
  combinado con `"aproximada_motor"` se rechaza temprano.

- politica_costo:

  Política optativa para las métricas caras. El valor por omisión,
  `"todas"`, conserva moda y mediana para todas las columnas
  solicitadas. `"por_cardinalidad"` resuelve primero las fuentes
  estructurales y mide valores válidos y distintos sólo cuando hace
  falta y la estrategia lo permite. Luego omite, por columna, sólo la
  moda cuando la proporción de distintos alcanza `umbral_cardinalidad`;
  la mediana se conserva porque las mediciones disponibles muestran que
  su costo depende de las filas y no de la cardinalidad. Los únicos
  valores aceptados son `"todas"` y `"por_cardinalidad"`; no hay alias
  históricos.

- bloque_muestra:

  Qué bloques se solicitan: `"con_muestra"` (por omisión) calcula
  también `perfil_muestra`, o `"solo_agregados"` omite su lectura y
  devuelve sólo los agregados SQL. La decisión vale para los dos
  universos: con `muestra_motor`, los agregados siguen usando la
  relación muestreada, pero no se abre el spool ni se leen filas; la
  corrida publica el estado `no_solicitado` y
  `meta$bloques$filas_vistas = 0`. La segunda opción no cambia el
  alcance de esos agregados: eso lo decide `universo`.

- max_consultas:

  Presupuesto declarado de consultas. Al agotarse, las métricas
  restantes quedan en `no_disponible` con ese motivo.

- dialecto:

  Capacidad de acotar filas: `"auto"` la sondea, y `"limit"`, `"top"`,
  `"fetch_first"`, `"rownum"` o `"portable"` la declaran sin sondeo.

- incluir_valores:

  Si publica valores de celda. Con `FALSE` no se emiten las consultas de
  moda, extremos y mediana, y se limpian esos campos en el resumen, la
  muestra, los bloques, la cobertura y los motivos; también se retiran
  ejemplos de patrones, familias de acumuladores y cualquier muestra
  cruda. Los conteos, proporciones y agregados que no son valores de
  celda pueden seguir publicándose. La elección queda en
  `resumen_tabla$meta$incluir_valores` y `perfil_muestra$meta`.

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
  valor por omisión es 2, un supuesto del código medido sobre un
  PostgreSQL de referencia: el motor hace una sola pasada por la tabla
  para todos los conteos de una consulta, así que un lote de uno cuesta
  por columna bastante más que uno de dos; uno de dos cuesta por columna
  casi lo mismo que uno de cuatro y derrama menos, y los lotes mayores
  derraman más porque los conteos compiten por `work_mem`. Una sola
  cardinalidad todavía puede forzar un agregado pesado y derramar mucho
  más que un lote plano.

- instrumentar:

  Si se cronometra cada consulta y las etapas grandes de R y, en
  PostgreSQL, se intenta atribuir el uso de bloques temporales de los
  `COUNT(DISTINCT)` exactos mediante `pg_stat_statements`. Por omisión
  es `TRUE`; agrega `duracion_ms`, `cpu_ms`, `n_filas_resultado`,
  `bytes_resultado_r`, `consulta_id`, `etapa` y `nivel` a
  `resumen_tabla$sql`, y el resumen `resumen_tabla$tiempos`. Con `FALSE`
  se conserva el mismo plan, la misma cantidad y el mismo orden de
  consultas, pero los campos medibles quedan en `NA`. `id_consulta`
  **no** depende de esta opcion: no es una medicion sino un hecho
  estructural sobre que consulta produjo cada metrica, y se publica
  igual con `FALSE`. Las duraciones usan
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) y el CPU del
  cliente usa la suma de `proc.time()[c("user.self", "sys.self")]`.
  `cpu_ms` es cero cuando el proceso no consumió CPU; `NA` significa que
  no se pudo medir. Los intervalos que el reloj no puede resolver no se
  publican como cero. En PostgreSQL, la atribución del derrame lee
  `pg_stat_statements` dos veces —antes y después—, limitado al usuario
  y la base de la conexión, y atribuye solo las consultas cuyas llamadas
  crecieron en esa ventana; una entrada que el catálogo repite —la misma
  consulta de otro usuario o anidada— no se atribuye. Medido con unas
  4.900 filas en el catálogo: sobre una tabla de 23 filas, 0,3 s con
  `TRUE` contra 0,15 s con `FALSE`.

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

- bloque_filas:

  Entero positivo que activa el recorrido de `tabla_completa` por
  bloques. Se abre un unico result set con `dbSendQuery()` y cada bloque
  se lee con `dbFetch(n = bloque_filas)` antes de liberar la entrada;
  `NULL` conserva la via existente. El resultado publica
  `meta$alcance$orden`, `meta$bloques`, `meta$bytes` y los eventos del
  vigilante. Los drivers sin retencion incremental demostrada (incluido
  DuckDB en esta matriz) quedan `no_disponible` con su motivo, sin caer
  en `dbGetQuery()`. Si se pide `moda` o `mediana`, la lectura por
  bloques inicia tambien el mapa central de distintos aunque
  `n_distintos` no se haya pedido: la mediana depende de ese mapa y la
  fila `n_distintos` conserva `no_solicitado`. Bajo `bloque_filas`,
  `perfil_muestra` sigue siendo la muestra diagnóstica acotada por
  `muestra`, `max_celdas_muestra` y `max_bytes_muestra`; su
  `general$filas` no es la cobertura, que se publica en
  `meta$bloques$filas_vistas`. Si el recorrido por bloques no puede leer
  una columna de una vista, se vuelve a leer la muestra diagnóstica por
  columnas: las columnas sanas conservan su perfil y sus magnitudes, y
  la columna rota queda en `no_disponible` con el motivo del motor. No
  se publica como privacidad ni como `no_solicitado`.

- max_bytes_procesamiento:

  Límite de bytes del estado retenido por el procesamiento por bloques y
  por la lectura del spool en R. Se comprueba antes de publicar un
  bloque o una pasada; `Inf` desactiva este límite.

- max_bytes_materializacion:

  Presupuesto de bytes del spool externo de `muestra_motor`. Se mide por
  chunk antes de cada escritura, dejando espacio para el trailer; si no
  alcanza, se publica `spool_presupuesto_excedido` y
  `muestra_inestable:presupuesto_materializacion` sin mezclar una
  muestra parcial con resultados completos. `Inf` desactiva este límite.

- columnas_opcionales:

  Nombres de columnas que sólo son aplicables a parte de las filas. Esta
  política se aplica al `perfil_muestra`; el resumen SQL no la traduce y
  lo declara en `resumen_tabla$cobertura`. Los dos bloques cuentan
  `n_faltantes` sobre universos distintos, así que la corroboración no
  lo compara: ver «Lo que el motor guarda distinto que R».

- aplicabilidad:

  Lista nombrada de fórmulas de un solo lado, con una regla por columna.
  Se reenvía al `perfil_muestra`; el resumen SQL no la traduce y lo
  declara en `resumen_tabla$cobertura`, y la corroboración no compara lo
  que la regla cambia.

- sentinelas_numericos:

  Vector completo de valores numéricos que se declaran como faltantes en
  el `perfil_muestra`. Se normaliza como un conjunto numérico, por lo
  que el orden, el tipo entero o doble y los duplicados no cambian el
  resultado. El resumen SQL no los excluye de sus agregados y lo declara
  en `resumen_tabla$cobertura`; la corroboración no compara las métricas
  de magnitud de una columna de la que la muestra excluyó alguno. En una
  columna `real`, una representacion float32 del centinela fraccionario
  se alinea antes de perfilar la muestra; la corroboración reconoce esa
  representacion como el mismo centinela.

  Rige la misma reserva que en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  un valor de la lista **por omisión** que aparece una sola vez y dentro
  del rango de los demás no se señala. Declarar el valor lo vuelve a
  contar.

- ...:

  Argumentos enviados a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  para analizar la muestra. `columnas_codigo` vale además en
  `resumen_tabla`: `media`, `mediana` y `desvio` de una columna
  declarada quedan en `NA` y `resumen_tabla$cobertura` lo declara con
  estado `no_aplica`. Sus nombres se validan contra las columnas de la
  tabla aunque no haya muestra. Un nombre abreviado vale lo mismo que el
  entero, como lo completa R al llamar a
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  `proteger = FALSE` es `proteger_datos_personales = FALSE` también para
  `resumen_tabla`. La bandera, `fecha` y los nombres se validan al
  entrar, también con `bloque_muestra = "solo_agregados"`: un nombre que
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  no tiene es un error, no un fallo parcial. Los demás valores los
  valida
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  al perfilar la muestra —sin muestra no se validan—, y uno inválido
  deja `perfil_muestra = NULL` con el error de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  como motivo, legible también con la protección: un error que se repite
  sobre la muestra sin filas no cita ninguna celda.

## Value

Objeto de clase `perfil_dbi` con dos componentes: `resumen_tabla`, de
alcance completo o muestreado según `universo`, y `perfil_muestra`, un
objeto `perfil` cuyo `meta$origen_dbi` declara tabla, conexión, SQL y
alcance. `perfil_muestra` es `NULL` si la muestra no se pudo obtener o
si se pidió `bloque_muestra = "solo_agregados"`;
`resumen_tabla$cobertura` distingue esos casos con `no_disponible` y
`no_solicitado`, respectivamente. Cuando `universo = "muestra_motor"` y
no se solicitó el bloque, la metadata además declara
`meta$materializacion$estado = "no_solicitado"` y
`meta$bloques$filas_vistas = 0`: las filas no se materializaron y luego
se descartaron. `resumen_tabla$meta$clave` conserva siempre la respuesta
del catálogo de la clave primaria: `columnas`, `fuente`, `motivo`,
`garantia` y `estado`. `garantia` puede ser `garantizada`,
`declarada_no_garantizada`, `desconocida` o `no_declarada`; `estado`
conserva, cuando el motor los expone, `visible`,
`restriccion_diferible`, `universo_incluye_descendientes` e
`indice_no_unico`. Una consulta fallida queda diferenciada de una clave
no declarada.

## Details

La función no escribe en la conexión ni crea objetos temporales del
motor; `muestra_motor` puede crear un spool externo temporal de sesión,
con bytes, checksum, presupuesto y estado. `DBI` es una dependencia
opcional. Cada agregado no disponible queda en `NA` y su consulta,
estado y motivo se conservan en `resumen_tabla$sql`. Las expresiones se
ejecutan como capacidades a comprobar, no como un dialecto SQL
universal.

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
leyendo. Si una columna declarada como entero de 64 bits o mas ancho, o
como decimal -`numeric`, `decimal`, con o sin precision-, llega como
doble y contiene un valor con valor absoluto de al menos 2^53,
`resumen_tabla` deja sus metricas de magnitud en `no_disponible`. **Las
siete**: `minimo`, `maximo`, `media`, `n_ceros`, `n_negativos`,
`mediana` y `desvio`. La condicion es de la columna y no de la metrica,
aunque las dos ultimas se calculen en otras consultas; lo que el motor
cuenta con exactitud -`n_distintos`, la moda- se sigue publicando. Lo
decide el valor: un decimal desde 2^53 ya no cabe en un doble
-90071992547409001 y 90071992547409002 son el mismo-, y el tipo solo
dice que viene de un decimal: RPostgres lo publica como `numeric` sin
precision y odbc con su codigo ODBC (2 y 3), asi que una regla que
esperara `numeric(20,0)` no se ejerceria. Para la muestra, la seleccion
SQL trae esas columnas como texto y, si `bit64` esta instalado, las
convierte a `integer64` antes de perfilar: `n_distintos`, la moda y los
diagnosticos por identidad conservan los valores exactos. Los
estadisticos de magnitud por encima de 2^53 siguen usando
`estado_resumen_cuantitativo = "omitidos_precision"`, como en memoria.
Si falta `bit64`, la columna queda como texto y `cobertura_diagnosticos`
explica la limitacion y recomienda instalarlo o pedir `integer64` al
conectar. Las columnas declaradas como `double`, `real` o `float` no
entran en esta guarda. Con `bloque_filas` rige la misma regla, sobre los
valores que el controlador entrega como `integer64` —un `BIGINT` o
`bigserial` de PostgreSQL, un `INTEGER` de SQLite que no entra en 32
bits—: por debajo de 2^53 se opera con el valor exacto, y desde 2^53 las
siete métricas quedan `no_disponible` con su motivo. Un decimal o un
entero ancho que llega como doble desde 2^53 deja ademas `n_distintos`,
`tasa_distintos`, la moda y su frecuencia en `no_disponible`: con
bloques se contarian en R sobre esos dobles, que no distinguen los
valores. Las cifras de `perfil_muestra` de esa columna salen de los
mismos dobles; sin bloques, la corroboracion avisa la diferencia en
`n_distintos` y la moda contra las del motor. Un decimal por debajo de
2^53 con mas cifras de las que guarda un doble -`numeric(30,20)`- no
entra en esta guarda. Ninguna columna cambia la clase de un campo de la
tabla: `minimo`, `maximo`, `media`, `mediana` y `desvio` de
`resumen_tabla$columnas` son siempre dobles. Cuando
`incluir_valores = FALSE`, la promesa de privacidad alcanza las tres
vias de salida: resumen SQL, `perfil_muestra` y `bloque_filas`. Se
omiten los valores de celda -incluidos moda, extremos, medianas,
centinelas y ejemplos de patrones-, se limpian cobertura y motivos, y no
se conservan familias de acumuladores ni una muestra cruda en el
resultado. La limpieza recorre también los atributos de cada hoja del
resultado, incluidos los resúmenes de patrones. Los conteos,
proporciones y agregados que no son valores de celda pueden seguir
publicandose. La marca queda registrada como
`meta$incluir_valores = FALSE` en ambos resultados. Si se omite el
bloque con `bloque_muestra = "solo_agregados"`, la cobertura usa el
estado `no_solicitado`: no es un fallo ni se cuenta como una métrica no
disponible. En `muestra_motor`, la selección materializada se hace una
sola vez antes de las pasadas; el spool se relee y se verifica mediante
su trailer. Un trailer ausente publica
`spool_incompleto:trailer_ausente`; un trailer o checksum que no
coincide publica `spool_checksum_invalido`. Si la lectura materializada
falla, se cierran el resultado DBI y el archivo del spool aun cuando el
error deshaga una transacción; no queda una conexión de archivo abierta
para un finalizador posterior.

## Fallo parcial

Ningún bloque descarta al otro. Si se pide la muestra pero el motor
rechaza su consulta, o si la muestra no se puede perfilar, el resultado
sale igual con `perfil_muestra = NULL` y una fila en
`resumen_tabla$cobertura` que declara el estado `no_disponible`, el
motivo y cómo resolverlo. Si se pide sólo agregados,
`perfil_muestra = NULL` se acompaña de una fila `no_solicitado`: no se
intentó leer la muestra. Si el motor rechaza una columna, esa columna
queda con sus métricas en `no_disponible` y las demás se perfilan
enteras. Lo mismo con una columna cuyo nombre la sesión no puede
escribir: odbc pasa la consulta por la codificación nativa, y bajo el
locale `C` —el de un `Rscript` de cron en Linux— un nombre con tilde no
se puede mandar; mandarlo dejaría la conexión rota y la consulta
siguiente haría caer a R. La consulta no se envía: la columna queda en
`no_disponible` con ese motivo, y una tabla con un nombre así da
`lupa_error_codificacion_dbi`. Para leerlas, correr la sesión con un
locale UTF-8 (`LC_ALL=C.UTF-8`). RPostgres, RSQLite y duckdb reciben la
consulta en UTF-8 y leen esos nombres en cualquier locale. Una columna
que la credencial no puede leer —un `GRANT SELECT` por columnas o un
`DENY SELECT` sobre una columna— se sabe antes de leer, por el catálogo
del motor: en PostgreSQL `pg_attribute` con `has_column_privilege()`,
legibles por cualquier rol (`dbListFields()` de RPostgres no lista esas
columnas), y en SQL Server `sys.columns` con `HAS_PERMS_BY_NAME()`. La
columna aparece en `resumen_tabla$columnas` con sus métricas en
`no_disponible` y una fila de cobertura con el motivo y el remedio, no
se nombra en ninguna consulta, y las demás se perfilan enteras. SQL
Server rechaza además cualquier consulta sobre la tabla, también
`COUNT(*)`, cuando hay una columna sin permiso: la lectura va sobre una
tabla derivada con las columnas legibles, y el SQL publicado lo muestra.
Con `universo = "muestra_motor"` la columna se declara igual, en
`resumen_tabla$columnas` y en la cobertura; `TABLESAMPLE` no se puede
escribir sobre la tabla derivada, así que la muestra del motor usa otra
forma que el motor acepte o se degrada con su motivo (limitación
conocida). Con odbc las consultas se ejecutan sin preparar
(`immediate = TRUE`, como el `dbGetQuery()` de odbc): preparada, una
consulta rechazada por un permiso de columna dejaría la conexión rota y
la siguiente haría caer a R. Lo que el catálogo no ve —un sinónimo, una
tabla de otra base, una tabla temporal— se intenta leer: el rechazo del
motor se declara y la conexión sigue. Con enmascaramiento dinámico de
SQL Server (`MASKED WITH`, 2016+) y una credencial sin `UNMASK` sobre la
columna o la base, el motor devuelve la máscara y no el dato. La
completitud de esa columna se publica —los no nulos se cuentan con una
expresión que el motor no enmascara—; distintos, moda, extremos, media,
mediana y desvío quedan `no_disponible` con el motivo, la columna no
entra en `perfil_muestra` (una fila `alcance_distinto` lo dice) y
`resumen_tabla$cobertura` la declara. Con `UNMASK` se perfila entera.
SQL Server tampoco admite `COUNT(DISTINCT)`, `GROUP BY`, `ORDER BY` ni
`MIN`/`MAX` sobre `text`, `ntext`, `image`, `xml` y `geography`. El
catálogo reconoce esos tipos antes de armar las consultas: `validos` se
conserva con la expresión de nulos, mientras `n_distintos` y `moda`
quedan `no_disponible` con su motivo y no se manda una sentencia que el
motor rechazaría. Si `orden_muestra` pide una de esas columnas, la
llamada se rechaza antes de enviar el orden. En una vista SQL Server,
`universo = "muestra_motor"` usa el límite pseudoaleatorio compatible en
vez de `TABLESAMPLE`, que sólo acepta tablas locales. Limitación: el
catálogo no resuelve el objeto al que apunta un sinónimo. Si apunta a
una vista, `muestra_motor` prueba `TABLESAMPLE`, el motor lo rechaza y
la vía pasa al límite pseudoaleatorio; si apunta a una tabla con `text`,
`ntext`, `image`, `xml` o `geography`, la vía pide los distintos y la
moda de esas columnas, el motor los rechaza y quedan sin medir. Con
`XACT_ABORT ON` y una transacción abierta, en los dos casos el motor
deshace la transacción, y la vía lo informa con
`lupa_error_transaccion_dbi`. Si la lectura de la muestra falla, la
columna que la hace fallar se busca por descarte leyendo las MISMAS
filas que la muestra con menos columnas —un fallo que está en una fila
que no es la primera, como el `CAST` de un texto sucio en una vista,
también se encuentra—; la muestra sale con las demás y una fila
`alcance_distinto` nombra la culpable. Si una vista falla en una fila
durante `universo = "muestra_motor"` o `bloque_filas`, la vía intenta el
mismo resumen SQL sobre la tabla completa: así conserva las métricas de
las columnas sanas y declara la muestra o el recorrido de bloques como
`no_disponible`, en vez de convertir todas las columnas en `NA`. Si el
resumen completo tampoco puede ejecutarse sobre la vista, sólo se
conservan las métricas que el motor alcanzó a contestar y las demás
quedan `no_disponible`; la cobertura identifica la columna o la consulta
que impidió completar el resumen. El nombre de la tabla se normaliza
antes de la primera llamada a la conexión, también la que comprueba que
existe: un nombre marcado `latin1` —el que da
`read.csv(encoding = "latin1")`— se convierte a UTF-8 y se lee, y uno
con bytes que no son UTF-8 válido y no declaran su codificación
—[`read.csv()`](https://rdrr.io/r/utils/read.table.html) sin
`fileEncoding` sobre un archivo latin1— no se adivina: con odbc da
`lupa_error_codificacion_dbi` antes de tocar la conexión, que así no
queda rota. Una columna de `orden_muestra` que la sesión no puede
escribir da también `lupa_error_codificacion_dbi`, con ese remedio: no
la rechazó el motor, no se le mandó. Los **valores** de texto tienen el
mismo origen. Bajo un locale que no es UTF-8, FreeTDS entrega las
columnas de texto no Unicode (`varchar`, `char`, `text`) en latin1 y
odbc las marca UTF-8 sin convertirlas; las `nvarchar` llegan en UTF-8.
Un valor así se lee como latin1 —lo que es—, y no como texto UTF-8 roto:
no se publica el hallazgo de bytes que no forman UTF-8 válido, que sería
del controlador y no del dato, y los valores entran en los análisis de
texto y en las comparaciones de
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).
Lo que no se puede recuperar es un carácter que no está en latin1 —el
signo del euro—: el controlador lo entrega como `?`. Por eso cada
columna de texto no Unicode leída bajo una sesión así queda en
`resumen_tabla$cobertura` con estado `degradado`, el motivo y el remedio
—correr con un locale UTF-8—, salvo que el controlador haya entregado en
ella texto UTF-8 no ASCII (está configurado en UTF-8). Bajo una sesión
UTF-8, la columna que el controlador entregue en latin1 —un
`ClientCharset` configurado así— se lee igual y se declara igual. Los
[`stop()`](https://rdrr.io/r/base/stop.html) de esta vía llevan clase de
condición propia —todas heredan de `lupa_error_dbi`— para que se puedan
rescatar con [`tryCatch()`](https://rdrr.io/r/base/conditions.html):
`lupa_error_conexion_dbi`, `lupa_error_tabla_dbi`,
`lupa_error_campos_dbi`, `lupa_error_conteo_dbi`,
`lupa_error_esquema_dbi`, `lupa_error_codificacion_dbi`,
`lupa_error_argumento_dbi`, `lupa_error_transaccion_dbi` y
`lupa_error_integer64`. La última sale cuando la lectura por bloques
recibe una columna `integer64` con un `bit64` anterior a 4.6.0: sin el
método `mtfrm.integer64`, [`match()`](https://rdrr.io/r/base/match.html)
de R iguala entre sí todos los valores negativos. Sin bloques, los
agregados que calcula el motor se devuelven y la muestra no, con un
aviso que dice por qué.

## Dentro de una transacción

La vía se puede correr dentro de una transacción abierta por el usuario
—es lo que recomienda el motivo de `consistencia` para una lectura
consistente—. En PostgreSQL un error cualquiera aborta la transacción, y
la vía manda consultas que pueden fallar a propósito —las sondas de
dialecto— o por el dato —una métrica que el motor no calcula sobre un
tipo, una vista rota—: con una transacción abierta, cada sentencia de la
vía va entre un `SAVEPOINT` propio y su `RELEASE`, y si falla se vuelve
con `ROLLBACK TO SAVEPOINT`. La transacción se reconoce por RPostgres
(`dbBegin()`, `dbWithTransaction()`) o por el motor, para un `BEGIN`
mandado a mano. El paquete nunca manda `BEGIN`, `COMMIT` ni `ROLLBACK`;
fuera de una transacción no agrega ninguna sentencia de ese tipo. Lo
mismo vale para
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
y
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).
SQL Server puede deshacer la transacción completa ante un error de
ejecución, incluso con `XACT_ABORT` apagado: antes y después de cada
envío y lectura la vía mide `@TRANCOUNT` y `XACT_STATE()`. Si cambian,
levanta `lupa_error_transaccion_dbi` y no continúa leyendo en autocommit
como si la instantánea siguiera viva. Si la sesión tiene
`IMPLICIT_TRANSACTIONS ON`, el motivo lo declara como una transacción
implícita de la sesión, no como una transacción abierta por el usuario.
`SAVE TRANSACTION` no evita los errores del motor que deshacen la
transacción completa. La etapa publicada en el motivo es la que detectó
el cambio: `dbSendQuery`, `dbFetch` o `dbClearResult`. Las sondas de
sintaxis de otros motores no se ejecutan cuando el controlador ya fue
reconocido; así se evita que `XACT_ABORT ON` aborte la transacción por
una sentencia que no corresponde a ese motor. Un desbordamiento de datos
que el motor no puede aislar puede abortarla igualmente y queda
documentado en `lupa_error_transaccion_dbi`. MySQL, MariaDB y SQLite no
abortan la transacción por el error de una consulta. duckdb sí la aborta
por un error al preparar, como el de una sonda de dialecto— y no tiene
`SAVEPOINT`: dentro de una transacción de duckdb, un objeto que falla al
leerse —una vista que convierte un texto sucio— la deja abortada, y hay
que cerrarla con `ROLLBACK`.

## Muestra y aproximaciones

`universo = "muestra_motor"` sondea las formas declaradas por el
adaptador y usa `TABLESAMPLE` cuando el motor lo acepta, o una función
pseudoaleatoria con el límite del dialecto. Si ninguna forma es
compatible, las métricas SQL quedan en `no_disponible`: no se sustituyen
por resultados de la tabla completa. Cada registro publica `alcance`,
`universo`, `tamano_muestra`, `fraccion`, `metodo` y `error_esperado`.
En `resumen_tabla$meta$muestreo`, `tamano_muestra` conserva el nombre
historico y declara el tamano efectivo solicitado a la consulta;
`filas_solicitadas` y `filas_pedidas` declaran el pedido original y
`filas_obtenidas` las filas que devolvio el spool. La materializacion
publica `muestra_id`, `snapshot_id`, `orden_id`, `n_filas`, `bytes`,
`checksum`, backend, version y presupuesto en `meta$materializacion`;
las pasadas publican el mismo `muestra_id`. El contrato de
`perfil_muestra` es campo por campo: `meta$filas_analizadas` es la
cantidad efectivamente entregada a los diagnósticos; `hallazgos` solo
contiene familias disponibles; y `cobertura_diagnosticos` tiene una fila
por familia no evaluada. `meta$origen_dbi$muestreo` conserva filas
solicitadas, entregadas, reproducibilidad, `muestra_id`, `snapshot_id` y
checksum. El método de impresión remite a esa cobertura cuando el campo
no está disponible; no reemplaza la ausencia con `NULL` silencioso.
Cuando el orden de la muestra del motor no se demostró estable, cada
traza de `hallazgos` se publica `no_disponible` con
`alcance = "orden_muestra_no_estable"`, sin índices ni claves y con su
total; los patrones raros conservan su cantidad y retiran sus nombres.
Bajo `bloque_filas`, `perfil_muestra` sigue siendo la muestra
diagnóstica acotada por `muestra`, `max_celdas_muestra` y
`max_bytes_muestra`; no es la cobertura completa. Por eso
`perfil_muestra$general$filas` es el tamaño de esa muestra y la
cobertura se lee en `meta$bloques$filas_vistas`. En `muestra_motor`,
todas las metricas SQL salvo `n` describen la relacion muestreada; `n`
es el total de la tabla completa y esta marcado con
`alcance = "tabla_completa"`, `metodo = "conteo_universo"` y
`error_esperado = "no_aplica"`. Los conteos `n_validos`, `n_faltantes`,
`prop_faltantes`, `n_distintos`, `tasa_distintos`, `n_ceros`,
`n_negativos` y `frecuencia_moda` son observaciones de esa muestra, no
extrapolaciones al universo. Sus motivos declaran la escala local y el
denominador `n_total_consulta` cuando corresponde. `TABLESAMPLE SYSTEM`
es la fuente preferida cuando el adaptador la declara y la sonda la
acepta; publica `metodo_muestreo = "tablesample_system"`,
`sesgo_muestreo = "por_bloques"` y el motivo estable
`sesgo_muestreo:tablesample_system_por_bloques`. Si se usa el fallback
`random_limit` con `NEWID()`, la metadata publica
`metodo_muestreo = "random_limit"`, `funcion_muestreo = "newid"`,
`sesgo_muestreo = "por_fila"` y
`sesgo_muestreo:random_limit_newid_por_fila`. Ese fallback se acepta
solo si la politica de costo conserva `n_total`, la proyeccion, la tasa
y sus tres umbrales; si no, la mediana queda `no_disponible` con
`capacidad_no_aceptada:newid_costo_excede_presupuesto` aunque se
silencien los avisos. Si la consulta de la muestra devuelve cero filas,
no hay base para medir las metricas de alcance `muestra`: se publican
con valor `NA`, estado `no_disponible` y el motivo estable
`muestra_vacia:random_limit_sin_filas` (o el metodo efectivo
equivalente). Esto no permite concluir que la columna este vacia, por lo
que no se publica cero ni se dispara la cascada `sin_valores`. Una
muestra no vacia sin valores validos usa
`muestra_inestable:sin_valores_validos`; una capacidad no aceptada usa
`capacidad_no_aceptada:...`. `n` conserva el conteo de la tabla
completa.

En una muestra, `error_esperado` vale `no_estimado` para metricas cuyo
error podria calcularse bajo un plan probabilistico pero no se calculo,
`no_estimable` para la moda, la mediana y la cardinalidad observada, que
no tienen una cota simple sin supuestos o un estimador declarado, y
`no_aplica` cuando no hubo muestreo efectivo. El `motivo` de cada
registro explica la distincion. `metodo`, `tamano_muestra` y `fraccion`
conservan las condiciones de la corrida; no se publica una cota numerica
sin una formula justificada. Si la materialización completa falla y sólo
se puede construir un perfil aislado de las columnas legibles, ese
perfil queda como diagnóstico. Las métricas SQL del resumen pasan a
`no_disponible`, con `tamano_muestra` y `fraccion` en `NA`, y la
cobertura nombra que la muestra aislada no representa la materialización
solicitada. Los distintos de una muestra se publican como cardinalidad
de la muestra, no como cardinalidad del universo. `estrategia_distintos`
es explicita y vale `"exacta"` por omision: calcula `COUNT(DISTINCT)`
sobre las filas de la corrida. `"aproximada_motor"` sondea una funcion
nativa y, si no hay una capacidad aceptada, deja la metrica
`no_disponible`; nunca ejecuta el conteo exacto como repliegue.
`"catalogo"` lee `pg_stats.n_distinct` y publica `estimado_catalogo`
sólo cuando `universo = "tabla_completa"`. Con
`universo = "muestra_motor"` queda `no_disponible`, porque el catálogo
describe la relación entera y la corrida mide un subconjunto; no se
inventa una equivalencia entre universos. `"omitida"` no emite la
consulta. Cada resultado y el atributo `meta$estrategia_distintos`
separan `estrategia_solicitada`, `estrategia_resuelta` y `estado`.

Las comparaciones que tienen una cota dura usan solo valores del mismo
grupo de consistencia. El grupo queda probado por el `consulta_id` que
ya se registra en `resumen_tabla$sql`: dos metricas con el mismo
identificador salieron de la misma sentencia. La consulta exacta de
distintos trae el conteo de no nulos `AS n_validos_guard` junto a
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
colecciones: no hubo lectura instantanea del motor. `snapshot_id`
identifica positivamente la materializacion cliente, pero solo se
publica evidencia positiva de snapshot transaccional cuando el adaptador
la demuestra. La cobertura agrega una entrada concreta solo si
`n_validos` y `n_distintos` son exactos, incoherentes y provienen de
grupos distintos; su motivo conserva ambas sentencias. La mediana
muestral que no tiene una forma consolidada o escalar usa una sola CTE
con `COUNT(*) OVER ()`, `ROW_NUMBER() OVER` y el promedio de las
posiciones centrales. La sonda barata repite esa construccion sobre
`VALUES (1.0), (2.0), (3.0), (4.0)`, exige `2.5` y un control negativo
que falle; no se sondea la tabla de produccion. Si esa capacidad falla,
la mediana se publica como `no_disponible` con
`capacidad_no_aceptada:sonda_mediana_cte_ventana`, nunca como
`dos_consultas` bajo `muestra_motor`.

Las cotas de error no documentadas de una aproximacion nativa quedan
como `"desconocido"`. Una aproximacion solo se consolida cuando entrega
una expresion que se puede incrustar en el `SELECT`; si solo construye
una consulta completa, se emite por separado. Una consulta no emitida o
sin un valor utilizable queda `no_disponible`.

## Motores medidos

El camino principal es PostgreSQL por RPostgres, en la 9.3 y en la 16, y
SQL Server 2022 por odbc con el controlador FreeTDS, en sesiones UTF-8 y
bajo el locale `C` de un `Rscript` de cron. Los siete motores
—PostgreSQL 9.3 y 16, SQL Server 2022, MySQL y MariaDB (RMariaDB),
SQLite (RSQLite) y duckdb— se verifican contra el motor real con el
guion `benchmark/verificar_motor.R`, y SQLite y duckdb se ejercitan
además en las pruebas del paquete. Con otro motor, otro controlador u
otra versión la función corre por la vía portable (ver «Dialecto») y
declara en la cobertura lo que no pudo medir, pero sus cifras no están
comprobadas contra ese motor. Las configuraciones avanzadas —una
transacción abierta por el usuario, permisos por columna,
enmascaramiento dinámico, `universo = "muestra_motor"`, `bloque_filas`—
tienen sus limitaciones conocidas en «Fallo parcial» y «Dentro de una
transacción».

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
en SQLite y PostgreSQL se usan `%` y `/` con division entera. Bajo
`muestra_motor`, los dialectos sin una forma escalar ni consolidada usan
la CTE de ventanas descrita arriba y no degradan a `dos_consultas`. En
`tabla_completa` se conserva el camino previo, incluido `dos_consultas`
si es el que resuelve ese dialecto; `PERCENTILE_CONT` no cambia. La
mediana consolidada tambien se sondea antes de usarla; en SQL Server
requiere nivel de compatibilidad \>= 110. Se sondea; estos son los
motivos conocidos de que no se active: la funcion no esta disponible o
el motor rechaza la sonda. En ese caso el mensaje del motor queda en
`meta$mediana_consolidada$motivo` —sin sus literales con la protección
activa, entero sin ella, como todo mensaje del motor: ver «Datos
personales»—, y se conserva la mediana por columna.

## Lo que el motor guarda distinto que R

El resumen SQL describe lo que el motor tiene delante, y en estos casos
eso no es lo mismo que la tabla en R:

- **Fechas en SQLite.** SQLite no tiene tipo fecha: RSQLite guarda un
  `Date` como número de días desde 1970 y un `POSIXct` como segundos, y
  sin más información el motor los describe como números
  —`minimo = 17956`—. Con
  `DBI::dbConnect(RSQLite::SQLite(), ..., extended_types = TRUE)` la
  columna se declara temporal y `perfilar_dbi()` no le aplica agregados
  cuantitativos, y lo dice.

- **`NaN` en duckdb.** duckdb conserva `NaN` como un valor: lo cuenta
  como presente y distinto, y responde `MAX` y `AVG` con `NaN`. En R,
  `NaN` cuenta como ausente. Los conteos pueden diferir por eso, y una
  métrica que el motor respondió con `NaN` no se publica: queda
  `no_disponible` con su motivo.

- **Infinitos.** SQLite, PostgreSQL y duckdb admiten `Inf` y `-Inf` en
  una columna de punto flotante, y el motor los cuenta en `MIN`, `MAX`,
  `AVG` y el orden de la mediana. Los estadísticos de magnitud se
  calculan sólo con valores finitos, como en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  y con `bloque_filas`: cuando el motor responde un infinito, `minimo`,
  `maximo`, `media`, `mediana`, `desvio`, `n_ceros` y `n_negativos` se
  vuelven a pedir sobre una subconsulta que deja afuera los infinitos, y
  su `sql` lo muestra. Donde la consulta no sabe escribir el infinito, o
  sobre una muestra del motor, esas métricas quedan `no_disponible` con
  ese motivo. Un infinito es un número: no se informa como un valor que
  no se pudo leer como número.

- **`money` y `real` en PostgreSQL.** RPostgres lee `0` en toda celda
  `money`, sin avisar, y un `real` por el texto con que lo escribe el
  servidor, mientras el motor opera sobre el binario guardado
  —21.299999237060547—. Las dos columnas se leen convertidas en el
  motor: `money` como `CAST(x AS NUMERIC)` y `real` por la forma decimal
  más corta que vuelve al valor guardado —`21.3`, la cifra que lee R
  desde PostgreSQL 12—, que se calcula en el motor redondeando a 6, 7, 8
  y 9 cifras. No se toma del texto de la sesión: antes de PostgreSQL 12,
  o con `extra_float_digits` de 0 o menos en cualquier versión, ese
  texto tiene seis cifras y 1234501 se escribe `1.2345e+06` —200 valores
  distintos se leerían como 21, con un mínimo que no está en la
  columna—. Así la lectura da el valor guardado en cualquier versión y
  con cualquier `extra_float_digits` desde -6, sin tocar el estado de la
  sesión; en un empate de la última cifra puede elegir la otra forma
  corta, que vuelve al mismo `real`. Por debajo de -6 el servidor
  escribe todo doble con menos de nueve cifras, y ninguna lectura
  —tampoco la de un `double precision`— llega entera a R. La conversión
  vale para toda lectura —la muestra, `bloque_filas`, `muestra_motor`— y
  el resumen SQL calcula sus agregados sobre esa misma lectura, así que
  las vías publican la misma cifra. Las columnas convertidas y su
  expresión quedan en `resumen_tabla$meta$lectura_convertida`. Se
  reconocen por el tipo que publica
  [`DBI::dbColumnInfo()`](https://dbi.r-dbi.org/reference/dbColumnInfo.html)
  de RPostgres; con otro controlador no se convierte nada. Para
  `double precision`, PostgreSQL 12 o posterior con
  `extra_float_digits >= 1` conserva la lectura nativa: el servidor ya
  escribe una representación redonda de 17 cifras y no se agrega una
  reconstrucción que pueda cambiar el valor ni encarecer cada consulta.
  En PostgreSQL 9.3, en versiones anteriores a la 12 o con
  `extra_float_digits <= 0`, la lectura usa `float8send()` para
  decodificar los ocho bytes IEEE-754 y formar el valor exacto como
  `NUMERIC`; esto conserva también los dobles vecinos bajo una potencia
  de dos, los valores desde `2^53` y los extremos finitos. `NaN` e
  infinitos se representan por sentinelas numéricos que RPostgres vuelve
  a leer como sus valores originales. La expresión de lectura se
  mantiene sin alias para poder entrar en una consulta derivada; sólo la
  lista de proyección le agrega el alias original. Esa misma distinción
  se usa en el resumen SQL, la muestra, los bloques, las expresiones
  anidadas y las relaciones de una colección. Los agregados de esa misma
  lectura excluyen `NaN` e infinitos con sentinelas de `NUMERIC`; la
  media se escala en `NUMERIC` para no desbordar ni perder subnormales
  por underflow. La lectura nativa de PostgreSQL 12+ con 17 cifras no
  pasa por esa reconstrucción. En PostgreSQL 9.3 y en sesiones con pocas
  cifras la decodificación exacta es más costosa que la lectura nativa;
  se conserva ese costo para no publicar cifras falsas y se aplica sólo
  cuando la sesión no garantiza los 17 dígitos. Cuando la media o el
  desvío de esa lectura vuelven por una sesión de PostgreSQL 9.3 o con
  `extra_float_digits <= 0`, el resultado se decodifica por sus ocho
  bytes antes de publicarse. Así no queda recortado a 15 cifras por el
  transporte; la aritmética del motor todavía puede diferir de R en unos
  ulps, y esa diferencia se conserva como tal en la corroboración.

  **`datetimeoffset` en SQL Server.** FreeTDS puede entregar a `odbc` un
  `datetimeoffset` con desplazamiento distinto de `+00:00` ya convertido
  a UTC. Una vez perdido el desplazamiento, Lupa no puede reconstruirlo
  desde el valor recibido: la corroboración lo deja como divergencia de
  una fecha, no como diferencia de cotejamiento. Para conservar el
  desplazamiento hay que leer la columna como texto o normalizarla en la
  consulta de origen.

- **Extremos y equivalencia entre motores.** SQL Server puede desbordar
  al agregar un `float` que contiene `-DBL_MAX` o `+DBL_MAX`; en ese
  caso sus métricas de magnitud quedan `no_disponible` con el mensaje
  del motor, aunque la muestra conserve el valor. El mismo `real` puede
  mostrar una última cifra distinta entre PostgreSQL y SQL Server porque
  RPostgres lee la forma decimal corta y FreeTDS/odbc entrega el
  `float32`; cada resumen es coherente con los valores que su
  controlador devolvió, pero la comparación entre motores puede
  declararlo `materialmente_distinto`. En un `real`, un centinela
  fraccionario se alinea a `float32`; si el valor declarado tiene más
  precisión que la representación binaria almacenada, no se considera el
  mismo centinela y no se cuentan faltantes disfrazados. La lista
  explícita de centinelas conserva además la reserva documentada de los
  valores por omisión: declarar un valor lo vuelve a contar incluso si
  aparece una sola vez. En PostgreSQL anterior a la 12, o con
  `extra_float_digits <= 0`, una combinación de `+DBL_MAX` o `-DBL_MAX`
  con infinitos se calcula sobre los finitos que sobreviven la limpieza.
  En PostgreSQL 12 o posterior con la sesión por omisión, esa misma
  combinación hace desbordar el motor y el mínimo, el máximo, la media y
  el desvío quedan `no_disponible`, con el error del motor como motivo:
  se declara, no se calcula. Si una columna que conserva ambos extremos
  finitos hace desbordar la suma de cuadrados, el desvío puede quedar
  `Inf`: es una limitación aritmética declarada.

- **La media en coma flotante.** Cada lado suma a su modo; sobre una
  columna que mezcla magnitudes muy distintas —`1e20` junto a `1`— las
  medias pueden diferir entre motores y con la de R, y ninguna es la
  exacta. La corroboración cruzada lo declara cuando compara las dos. La
  media y el desvío se le piden al motor como doble donde su aritmética
  decimal los recortaría: en SQL Server `AVG(CAST(x AS FLOAT))`, porque
  `AVG` de un `numeric` devuelve seis decimales, y en PostgreSQL
  `STDDEV_SAMP(CAST(x AS DOUBLE PRECISION))`, porque sobre magnitudes
  grandes el desvío de un `numeric` sale con cuatro; en MySQL y MariaDB
  `AVG(x * 1e0)`, porque `AVG` de un `DECIMAL` devuelve cinco decimales.
  En los demás motores, `x * 1.0`.

- **El desvío de una sola pasada.** `STDEV` de SQL Server —y, con menos
  pérdida, el desvío nativo de SQLite, duckdb, MariaDB y MySQL— acumula
  en una sola pasada, y cuando la media es muy grande frente al ancho de
  la columna pierde las cifras del desvío: sobre marcas de tiempo en
  milisegundos de una ventana de un segundo, SQL Server responde `0`. El
  desvío no cambia si a cada valor se le resta una constante, así que se
  le pide al motor sobre los valores centrados en la media ya calculada:
  siempre en la consulta por columna, y en la consulta compartida —donde
  la media sale de la misma sentencia— se vuelve a pedir centrado cuando
  la media supera diez veces el desvío, o cuando respondió `0` sobre una
  columna con valores distintos, con una consulta más que el plan no
  anticipa porque depende de los datos. Su fila de `sql` lo dice. La
  resta se hace en un tipo que no pierde cifras: en SQL Server y duckdb
  una columna `real` (`FLOAT` en duckdb) restaría en 24 bits —en T-SQL
  `real - decimal` es `real`— y el desvío centrado se apartaría del de
  los valores guardados, así que se convierte a doble antes de restar;
  en SQL Server el centro de una columna de coma flotante va como
  literal `float`, que no tiene el tope de 38 cifras de un decimal. Una
  columna decimal o entera resta en su tipo, que es exacto. Con
  `bloque_filas` los momentos se acumulan centrados. Bajo
  `muestra_motor` no se vuelve a pedir, porque otra consulta vería otras
  filas; ahí, como red en todas las vías, un desvío `0` sobre una
  columna con dos valores distintos —un mínimo menor que el máximo— es
  imposible y queda `no_disponible` con su motivo.

- **El cotejamiento.** El resumen SQL agrupa y ordena con el
  cotejamiento de la columna: sobre una `COLLATE NOCASE` de SQLite o una
  colación ICU no determinista de PostgreSQL, "Montevideo" y
  "montevideo" son un valor, y `n_distintos`, la moda y su frecuencia
  salen de esa cuenta. Con `bloque_filas` las filas se leen y se
  comparan en R, byte a byte, como el perfil de muestra. Cada
  `resumen_tabla` lo dice en `meta$criterio_comparacion` y
  `meta$criterio_moda`. La corroboración no llama divergencia a la
  diferencia que ese criterio explica —menos distintos en el resumen
  SQL, con la moda y la frecuencia que de ahí se siguen, o una moda
  empatada que cada lado desempata con su orden—: la deja en una fila
  `no_corroborada` con ese motivo. En SQL Server, el resumen SQL aplica
  la igualdad de texto que ignora los espacios finales; la vía por
  bloques compara en R los valores que recibió y, por eso, puede contar
  `"A1"` y `"A1 "` como distintos en `n_distintos`. Esa diferencia de
  alcance se publica como criterio de comparación y no se corrige
  inventando una equivalencia en R.

- **`datetimeoffset` en SQL Server.** FreeTDS/odbc puede entregar un
  `datetimeoffset` con desplazamiento distinto de `+00:00` como
  `POSIXct` en UTC y perder el desplazamiento original. La lectura no lo
  puede recuperar después; la corroboración no lo excusa como un
  cotejamiento y deja la divergencia publicada. Para conservar el
  instante y el desplazamiento, leer la columna como texto con el
  desplazamiento o normalizarla en el origen antes de perfilar.

- **Una columna que mezcla tipos de almacenamiento.** SQLite admite
  texto en una columna de afinidad numérica, y RSQLite la lee como
  número: convierte ese texto en `0` y lo avisa ("mixed type"). El
  resumen SQL ve los valores guardados —la moda puede ser el texto, y
  las métricas de magnitud quedan `no_disponible` porque el motor
  devolvió algo que no se lee como número—, y `perfil_muestra` ve lo que
  entregó el controlador: `minimo = 0`, una moda `0` que nadie escribió.
  `perfilar_dbi()` no relee la columna como texto: cuenta con
  [`typeof()`](https://rdrr.io/r/base/typeof.html) los valores guardados
  como texto o blob y declara cada divergencia con esa causa medida en
  `resumen_tabla$cobertura`, y dice que las cifras de magnitud de
  `perfil_muestra` para esa columna no describen lo guardado. Para
  perfilar los valores guardados, leer la columna como texto
  (`CAST(columna AS TEXT)` en una vista) o limpiarla en el origen.
  **Limitación conocida:** un texto que R sí lee como número —`'0x10'`,
  hexadecimal, que [`as.numeric()`](https://rdrr.io/r/base/numeric.html)
  lee 16— pasa la sonda de magnitud como un número, y el resumen SQL
  publica `maximo = 16` sobre una columna `NUMERIC` cuyo máximo numérico
  es otro; las divergencias de la corroboración son la señal.

- **Lo que la corroboración compara.** Sólo las métricas que los dos
  bloques calculan con la misma semántica. No compara lo que una
  declaración o una guarda hacen distinto a propósito, porque esa
  diferencia no es del motor, del controlador ni de un snapshot: la
  `media`, la `mediana` y el `desvio` de una columna de
  `columnas_codigo`, que no se calculan; `n_faltantes` de una columna de
  `columnas_opcionales` o de `aplicabilidad`, que la muestra cuenta
  sobre su universo —y todas las métricas de la columna con
  `aplicabilidad` si la muestra dejó afuera valores presentes—; las
  métricas de magnitud de una columna de la que la muestra excluyó los
  `sentinelas_numericos` declarados; y las de un entero de 64 bits desde
  2^53, que el resumen no publica. Tampoco lo que un bloque no calculó:
  una métrica que `resumen_tabla$sql` declara `no_disponible` —el motor
  rechazó la consulta sobre el tipo (`json`, `xml` o `point` de
  PostgreSQL; `text`, `ntext`, `image` o `geography` de SQL Server), una
  guarda la retuvo—, la moda que la muestra no pudo contar —una columna
  de blobs— y las métricas de una columna compuesta, que la muestra no
  analiza: su motivo ya está publicado, y una relectura daría lo mismo.
  Un `NaN` o un infinito sí se compara, y un valor guardado como texto
  en una columna numérica también: es la diferencia del motor que la
  corroboración existe para declarar. Lo que no se comparó queda en
  `resumen_tabla$meta$corroboracion_bloques$no_comparadas`, una fila por
  columna y métrica con su causa. La tolerancia está en
  `meta$corroboracion_bloques$tolerancia`: `1e-10` absoluta más `64`
  épsilon de máquina relativos para los valores leídos —`minimo`,
  `maximo`, `mediana`, la moda—, y `1e-10` relativa para `media` y
  `desvio`, que cada lado acumula con su algoritmo y en su orden. Medido
  en SQL Server, la diferencia de la acumulación llega a 2e-13; una
  divergencia real es de otro orden y sigue saliendo. Limitación
  conocida: la tolerancia absoluta se suma siempre, tambien a `desvio`;
  por eso puede ocultar diferencias en columnas cuyos valores no son
  pequeños pero cuyo desvio es menor que `1e-4`. En valores menores que
  `1e-10` —p-valores, concentraciones en unidades del sistema
  internacional— la corroboración tampoco distingue dos lecturas.

## Costo

Los agregados de una tabla ancha se emiten por lotes; `muestra` acota lo
que se trae a R, no el trabajo del motor. `bloque_muestra` decide si se
trae esa muestra; `universo`, `metricas`, `tamano_lote_planos`,
`tamano_lote_distintos` y `max_consultas` acotan el trabajo SQL.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
dice cuántas consultas se van a emitir antes de emitirlas. El orden de
degradación es agregados planos, total del universo cuando hace falta,
distintos, moda y mediana. Los agregados planos sobre la misma tabla y
filtro —el conteo de no nulos, mínimos, máximos, medias, ceros,
negativos y desvío— comparten una consulta por lote y cada consulta que
trae `n_validos` lleva además `COUNT(*) AS n_total_consulta` en la misma
sentencia. La completitud usa ese denominador local, no el total de otro
lote. Los no nulos se cuentan en PostgreSQL con `COUNT(col)`, para no
contar como faltante un tipo compuesto cuyos campos sean todos nulos. En
SQL Server y en los motores que rechazan `COUNT()` sobre `text`, `ntext`
e `image` se usa una expresión CASE; la pregunta por el nulo se contesta
sin publicar el valor enmascarado. La fusión conserva la medición, no
una identidad bit a bit entre agrupamientos: la media y el desvío pueden
diferir en el último bit según cómo se agrupen las sumas, porque la suma
en punto flotante no es asociativa.

El total del universo se conserva por separado cuando el perfil se
calcula sobre una muestra. Si el lote completo es rechazado, sus mitades
se sondean por bisección: los grupos aceptados se reutilizan como
mediciones y las columnas culpables se reintentan por métrica, con su
denominador local. Las fuentes `TABLESAMPLE` que necesitan el total del
universo para escribir un porcentaje lo cuentan antes.
`COUNT(DISTINCT ...)` queda en una clase separada y usa su propio tamaño
de lote, conservador por omisión porque una cardinalidad puede derramar
mucho más que veinte agregados planos; la consulta exacta trae su
`n_validos_guard` compañero. Cuando se pide `mediana` o `desvio` sin
pedir `basicos`, cada columna que el esquema expone como numérica paga
además una sonda propia de magnitud. Usa `MIN` para comprobar que el
motor no está coaccionando texto a cero; la fila `sonda_magnitud` queda
declarada en `resumen_tabla$sql`, con su estado y la etapa
`sonda_magnitud`. Si la sonda no entra en `max_consultas`, las métricas
dependientes quedan `no_disponible` con el motivo del presupuesto.
Cuando `incluir_valores = FALSE`, el motivo no publica el valor que
devolvió el motor. La proyección temporal no usa esos agregados planos:
si hay más de un lote y `instrumentar = TRUE`, se mide el primer lote de
distintos y, después de ejecutarlo, se multiplica su mediana por la
cantidad total de lotes. El aviso llega antes del segundo lote, en la
unidad que se va a evitar; con un solo lote no hay nada que proyectar.
Si la duración no se pudo medir, el resultado declara la proyección como
no disponible. La moda tiene otro canal: después de cada moda medida se
obtiene una tasa en ms por distinto y se usa para proyectar las modas
pendientes. La cardinalidad se toma del agregado de la corrida, de una
clave garantizada o de la estimación de catálogo que esté disponible; si
falta, `meta$costo_moda` lo declara y no inventa un número. El aviso
llega antes de la siguiente moda. La mediana se proyecta en ms por fila.
La primera mediana medida en esta corrida sirve para proyectar las
restantes y el aviso precede a ese trabajo. Si no existe una primera
medición local para una mediana total, usa la referencia declarada de
otra corrida de 68 ms por millón de filas. Si la consulta inicial que
obtuvo las filas fue medida y resulta una cota mayor, se publica también
esa cota de lectura —no como medición de mediana— para no subestimar una
tabla grande recién cargada. Las dos proyecciones quedan separadas en
`meta$costo_moda` y `meta$costo_mediana`; apagar el aviso no apaga su
medición ni su metadata. Antes de la primera consulta exacta se estima,
cuando PostgreSQL expone `pg_stats`, el tamaño de los hashes con
`n_distinct`, `avg_width` y `pg_class.reltuples`; `SHOW work_mem` y,
desde PostgreSQL 13, `SHOW hash_mem_multiplier` dan el límite efectivo.
`meta$estimacion_derrame` y `attr(meta$plan, "estimacion_derrame")`
conservan el diagnóstico, siempre rotulado como estimación y nunca como
derrame medido. Si supera el límite se avisa antes de pagar
`COUNT(DISTINCT)` y se recomienda subir `work_mem` en la sesión; el
paquete no lo modifica. Una estadística ausente, un permiso insuficiente
o una versión sin el parámetro dejan la parte correspondiente como no
disponible. `n_distinct` es una estimación de muestra y puede quedar
corta; si luego `pg_stat_statements` mide un derrame, esa medición
manda. La misma corrida publica `meta$estimacion_derrame_moda` y
`meta$estimacion_derrame_mediana`. La moda deriva `metodo` del plan
exacto mediante `EXPLAIN (FORMAT JSON, COSTS OFF)` sin `ANALYZE`; la
mediana usa siempre un sort contra `work_mem`, con pisos de 32 bytes
para tipos fijos y 42 para `numeric`. El plan usa `n_validos` de
catálogo. En la meta y en el aviso, cuando se pidió la familia
`validos`, ese denominador se reemplaza por el medido en los agregados
planos y se conserva también el valor de catálogo; si no se midió, el
motivo declara que se retuvo el catálogo. Ese reemplazo alcanza solo al
denominador: si la estimación publicada marca `supera_memoria = TRUE`,
el aviso se emite también con catálogo y declara su fuente. La
consolidada decide por el máximo de columna y publica la suma de tapes
sólo como `estado_io_total_bytes` informativo.

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
`na.rm = TRUE` si corresponde), no la columna completa. `id_consulta`
identifica la consulta de datos que produjo la medición: dos métricas
con el mismo identificador vieron exactamente las mismas filas y se
pueden comparar directamente. `NA` declara que esa garantía no se puede
hacer; en particular, las métricas por columna —moda, frecuencia de la
moda y mediana— no comparten filas con otras métricas. `etapa` permite
agruparlo (`conteos`, `moda`, `basicos`, `mediana`, `desvio`,
`lectura_muestra` y las sondas). Las métricas no solicitadas o que no
emitieron consulta conservan esos campos y los dejan en `NA`; en
particular, `NA` no significa cero.

`resumen_tabla$sql$memoria_trabajo` agrega el eje del estado de trabajo
del motor: se deriva en orden como `NA` para una fila sin medición,
`acotado` para una muestra con `fraccion < 1` y, en los demás alcances
efectivos, según el método resuelto del registro; una muestra saturada
(`fraccion = 1`) cae a este último caso porque su tope es vacuo y se
clasifica como tabla completa. Sus únicos valores son `"creciente"`,
`"acotado"` y `NA`.

En PostgreSQL, con `instrumentar = TRUE`, se toma una foto de
`pg_stat_statements` antes y después de la ventana que cubre los
agregados exactos instrumentables de distintos, moda y mediana. Las
firmas se filtran por texto y se normalizan en el paquete: cada literal
numérico o de cadena se reemplaza por su parámetro posicional y los
identificadores entre comillas dobles se conservan. Sólo se publica un
derrame cuando una consulta coincide y su contador aumentó en al menos
una llamada dentro de la ventana. En ese caso, `resumen_tabla$sql`
agrega `derrame`, `bloques_temporales_leidos`,
`bloques_temporales_escritos` y `fuente_derrame`, además de
`llamadas_en_ventana`; `resumen_tabla$meta$derrame` conserva el resumen
y la fuente. Cuando el contador aumentó más de una vez, el motivo
declara que los bloques son un agregado de ese texto exacto y pueden
incluir otra sesión o llamada concurrente. Si la extensión no está
disponible o la instrumentación está apagada, el estado queda
`no_disponible` o `no_medido`: el paquete no deduce un derrame del
tiempo ni modifica `work_mem`.

`resumen_tabla$meta$estimacion_derrame` es un diagnóstico distinto:
conserva la estimación de memoria y siempre la rotula como no medida.
Puede quedar parcial o no disponible por permisos, falta de `ANALYZE`,
particiones sin estadísticas o un motor que no sea PostgreSQL. Si ambos
diagnósticos existen, `meta$derrame` es la evidencia posterior y
prevalece sobre la estimación; una estimación que no superó el límite no
contradice un derrame medido. `meta$estimacion_derrame_moda` y
`meta$estimacion_derrame_mediana` siguen la misma regla, con el método
del plan de la moda y el sort de la mediana; `n_validos_medido` sólo
aparece con valor cuando la familia `validos` fue solicitada y sus
agregados planos lo pudieron medir.

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
las columnas y `meta` lo declara. Lo que se oculta no queda `calculado`:
su fila de `resumen_tabla$sql` dice `omitido_por_privacidad`, con un
motivo que distingue la columna clasificada como personal de la que se
cerró porque, sin muestra, no se pudo clasificar. Una columna que no se
protege publica sus estadísticos aunque comparta el tipo personal con
una protegida, como en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
un importe entero de siete cifras o un número de factura clasificado
`documento_identidad` débil. Una copia del documento la tapa el piso por
valor, que corre con la tabla entera, y la que lo copia sin su
verificador se clasifica como columna personal con la regla de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
sobre la muestra. Por la vía por bloques eso incluye `meta$familias`,
que guarda cada valor distinto de toda columna no protegida: una columna
`debil` —las cédulas sin su verificador de OTRAS personas, como las de
un garante— publica ahí cada uno de sus valores, donde
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
publica sus dos extremos. Limitación conocida: como la clasificación es
sobre la muestra, con una muestra que ve menos de tres valores distintos
de la copia —chica y ordenada sobre una tabla agrupada por persona— la
copia no se clasifica, y el resumen de la tabla entera publica sus
extremos, que pueden ser documentos de personas que la muestra no vio;
por bloques, cada valor distinto en `meta$familias`. Una muestra mayor o
aleatoria lo evita, y también declararla en `columnas_personales`. Lo
mismo vale para una columna cuya palabra el resto del nombre califica y
que decide por sus valores —`celular_estado`, `contacto_alta`,
`usuario_baja`, `nombre_departamento`—: rala, con una muestra que no
trae ninguno de sus valores o unos pocos sin su forma, no se protege, y
por bloques publica cada valor en `meta$familias`. No se cierra
protegiendo por la palabra a la columna sin valores, porque en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
taparía la evidencia de toda columna vacía con un nombre así. Un
documento por el nombre, en cambio, se protege aunque la muestra no
traiga valores —una columna rala de documentos, `cedula_conyuge`,
`ci_tutor`—: se publica sólo si la muestra trae veinte valores o más, de
dos distintos, que lo desmientan. El desvío se oculta como la media,
como en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md);
también en `resumen_tabla$meta$familias`, donde la vía por bloques
(`bloque_filas`) guarda lo que devolvió cada acumulador: ahí se tapan
los extremos, la media y el desvío de las columnas protegidas, y cada
valor distinto. Y por la vía por bloques corre el mismo piso por valor
que sin ella —con los valores de la muestra y, si la muestra no cubre la
tabla, con los de la tabla entera—, sobre **todas** las columnas: una
cédula copiada en una columna de códigos o un nombre citado en un texto
libre se tapan en `resumen_tabla$columnas`, en la corroboración y en
`meta$familias` igual que sin `bloque_filas`; la misma tabla publica lo
mismo por las dos vías. Lo que el piso tapa en la tabla de columnas —un
extremo que es un documento, y la media y el desvío que lo reconstruyen—
se tapa también en `meta$familias`. Si la muestra se recorta, la
clasificación personal sólo cubre esas filas: las cifras de valor del
resumen SQL completo quedan marcadas en `resumen_tabla$cobertura` y, con
la protección activa, se ocultan hasta que la clasificación cubra el
mismo alcance. La corroboración no repite esas cifras dudosas en su
anotación. Y cuando la muestra no cubre la tabla, se traen los valores
distintos de las columnas protegidas de la tabla entera —de texto o
numéricas, una consulta por columna— y se protege con ellos lo que se
publica, con la misma regla que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) en
memoria: tildes, caja, separadores, el documento parcial, los campos
numéricos y la evidencia de los hallazgos. Un valor de una persona que
sólo está fuera de la muestra se tapa igual. El **relleno** que sale del
piso en memoria —el dígito repetido cinco veces o más en una columna que
sin él es casi una clave— se decide sobre la tabla entera, no sobre la
muestra: en la muestra de una tabla de movimientos los clientes que se
repiten casi no se repiten, y la cédula válida de un cliente con
seiscientos movimientos saldría como moda de otra columna. Con la tabla
leída completa decide la muestra, que es la tabla; si no, la confirman
los conteos exactos de la tabla —`n_distintos` y `n_validos`
calculados—, y sin ellos el relleno sigue en el piso. Con
`universo = "muestra_motor"` no hay tabla entera que lo confirme —los
conteos son los de la muestra del motor, y con `muestra = Inf` la
muestra en R es esa misma relación—: el relleno sigue en el piso. Y sale
del piso sólo en su columna, como en memoria: si otra columna protegida
tiene el mismo valor —el `1.111.111-1` de relleno de `cedula` es la
cédula de un cónyuge—, ese valor se sigue tapando. Y la protección es
**por tabla**: un valor protegido de otra tabla escrito en ésta no se
conoce;
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
los cruza. Se traen hasta dos millones de valores; si hay más, o si la
consulta no se puede hacer, se tapan todos los valores de celda de las
columnas no personales —moda, extremos, ejemplos y, por la vía por
bloques, cada valor distinto y los extremos de `meta$familias`— y la
evidencia de sus hallazgos, y
`resumen_tabla$meta$proteccion_personal$fuera_de_muestra` dice por qué.
Salvo un valor igual al nombre de la tabla o de su esquema, que es
estructura: se publica en `meta`, en cada SQL guardada y en
[`print()`](https://rdrr.io/r/base/print.html), y un valor de celda
igual a él no se tapa —taparlo taparía el nombre en todo el objeto—; lo
mismo cuando sin muestra se cierran todas las columnas. Limitación
conocida: un valor igual al nombre de una **columna** sí se tapa, porque
un nombre de columna puede ser el de una persona —una planilla con una
columna por docente—, y tapa también ese nombre en la descripción de las
sondas de dialecto (`meta$dialecto$sondas`) y, con `bloque_filas`, en la
de la fuente por bloques (`meta$fuente_bloques`, `meta$plan`); en
`columnas$columna` y en la SQL guardada queda. En SQLite los valores se
leen como texto, porque una columna de afinidad numérica guarda texto y
el controlador lo convertiría en número; también sin muestreo, cuando la
muestra es la tabla entera: ahí se releen como texto, con una consulta,
las columnas protegidas que el controlador no leyó como texto. Y el
motivo de una cifra que no se pudo leer como número no cita el valor de
una columna protegida. Tampoco el **mensaje de un error del motor**, que
es texto ajeno y cita la celda que hizo fallar la consulta
—`invalid input syntax for type bigint: "4.685.467-9"`, en una vista que
castea una cédula escrita con puntos—: ese valor nunca se leyó, así que
el piso no lo conoce y puede ser de cualquier columna. Con la protección
activa, todo mensaje de error que se publica —en `sql`, `cobertura`,
`meta`, en la colección y en los avisos— lleva sus literales entre
comillas —simples, dobles, tipográficas, bajas como las del polaco,
angulares— cambiados por `[literal omitido]`; también un identificador
entre comillas, salvo el nombre de la tabla o de una de sus columnas. Y
fuera de las comillas el mensaje publica **sólo lo permitido**: el
vocabulario de los mensajes de los motores, en inglés y en castellano,
las palabras de SQL, los nombres de tipo, los nombres de la tabla y de
sus columnas, los códigos SQLSTATE, los signos y los números de menos de
seis cifras —el piso de la protección: la línea, la posición, un
código—; todo lo demás sale `[literal omitido]`. Un número se cuenta con
sus separadores: `4.685.467-9` es uno de ocho cifras, y se lee como lo
lee el piso de la protección, con los mismos separadores —un espacio
duro o uno fino, el punto medio, una raya, `" - "`, dos espacios, un
tabulador o un salto de línea entre los grupos: `"099 - 123 - 456"` es
uno de nueve—. El mensaje se lee como lo dijo el motor: odbc lo arma con
cli, que pone una viñeta después de cada salto de línea y corta las
líneas al ancho de la consola, y un número partido en dos líneas
—`"2901\n1234"`— quedaría partido por la viñeta; se toman las líneas que
guarda la condición, unidas por el salto que puso el motor, y así se
publican, sin las viñetas. Tampoco llevan los colores ni los enlaces que
cli pone en una consola a color —la de RStudio—: las secuencias ANSI
quedarían guardadas en el motivo y, con protección, pegadas a los
marcadores. Lo que cli agrega dentro de una línea no se distingue del
texto del motor y queda: sin color, cli cita entre comillas el archivo
del controlador (`'nanodbc/nanodbc.cpp:1802'`) y con protección esa cita
sale `[literal omitido]`; con color se publica. Así se tapa también el
valor que el motor cita sin comillas —duckdb:
`Type INT64 with value 59899123456 can't be cast`, y la cadena que
`strptime` repite en otra línea, que se tapa entera; SQL Server con la
sesión en sueco o neerlandés— y la cola de un valor que lleva una
comilla adentro. Y el mensaje pasa también por el piso con los valores
protegidos que la llamada ya leyó —en
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
los de todas sus tablas—, antes de las dos pasadas, con el valor entero
a la vista, y otra vez al final: un valor escrito sólo con lo permitido
—los apellidos `"Campos Cadena"`, dos palabras del vocabulario, o un
nombre cuyas palabras son columnas de otra tabla— se tapa entero, y el
nombre de una columna que es también uno de esos valores —la docente
`"Florencia"` de una planilla con una columna por docente— deja de
exceptuarse, con la comparación del piso. En
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
cada mensaje se vuelve a leer al final, desde como llegó, con todo lo
que la colección leyó: con una vista perfilada antes que su tabla base,
el piso todavía no conoce el nombre cuando llega el mensaje, y releer el
texto ya tapado dejaría su resto —`"De Los Campos"` de
`"María De Los Campos"`—. Lo que queda abierto: un valor que la llamada
**no** leyó —la celda que hizo fallar la lectura de la muestra, de una
columna que la vista castea— escrito sólo con lo permitido, o igual al
nombre de una columna, se publica, porque el mensaje no tiene cómo
distinguirlo del vocabulario ni de un nombre. Un mensaje en otro idioma
conserva su SQL, sus tipos y sus nombres y se lee peor, que es el lado
cerrado. Quedan una palabra de SQL entre comillas
—`Incorrect syntax near 'LIMIT'` dice que el motor no acepta ese
dialecto— y un signo suelto. El texto que PostgreSQL cita sin comillas
al no poder leer un JSON (`CONTEXT: JSON data, line 1: ...`) se tapa
hasta el fin de la línea, en PostgreSQL, en duckdb y en SQL Server.
**Sin protección** (`proteger_datos_personales = FALSE`) el mensaje del
motor se publica entero, en todas esas salidas: el literal que hizo
fallar la lectura y el SQL de esa lectura, sólo sin las viñetas, los
cortes ni los colores que agrega cli. El diagnóstico es el cometido del
paquete
—`Conversion failed when converting the nvarchar value 'N/D' to data type int`
dice que la columna trae un faltante disfrazado—, y sin protección ya se
publican valores de todas las columnas —modas, ejemplos, patrones—:
omitirlo ahí no protegería nada y quitaría el diagnóstico. Vale igual en
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
que pasa la bandera a cada tabla y la aplica a sus propios motivos.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
y
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
no tienen la bandera: llamadas por su cuenta, tapan el mensaje como con
la protección activa. `incluir_valores = FALSE` va más lejos: no emite
las consultas de moda ni de mediana y no informa mínimo ni máximo, útil
cuando la tabla es un padrón y la moda de un identificador único es un
documento real. Si se pidió `desvio`, la sonda de magnitud que ese
cálculo necesita puede emitirse, pero su motivo nunca publica el valor
devuelto por el motor.

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

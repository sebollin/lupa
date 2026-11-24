# Planificar el costo de `perfilar_dbi()` antes de pagarlo

Emite sólo consultas de preparación —leer el esquema y sondear
capacidades— y devuelve cuántas consultas emitiría el perfilado
completo, de qué clase y con qué alcance sobre la tabla. No escanea
datos para decidir el costo. Cuando
`politica_costo = "por_cardinalidad"`, una clave estructural exacta
puede cerrar la decisión; si no hay una fuente de catálogo utilizable,
el plan publica el rango entre omitir y ejecutar la moda; la mediana no
entra en ese criterio proporcional. Nunca lanza `COUNT(DISTINCT ...)`
para despejar esa incertidumbre. Las fuentes estructurales se resuelven
cuando la política necesita la cardinalidad, aunque
`estrategia_distintos` no permita medirla. La disponibilidad de la
estrategia gobierna la medición, no el conocimiento que ya da el
catálogo. Para el `COUNT(DISTINCT)` exacto, la preparación puede
consultar además las estadísticas de PostgreSQL (`pg_stats`,
`pg_class.reltuples`, `SHOW work_mem` y, desde PostgreSQL 13,
`SHOW hash_mem_multiplier`) para estimar el tamaño del hash y avisar un
posible derrame. Esa consulta de metadatos no publica cardinalidad
medida ni reemplaza la medición posterior. Para la moda y la mediana se
publican, además, los atributos `estimacion_derrame_moda` y
`estimacion_derrame_mediana`. La moda deriva su `metodo` (`"hash"` o
`"sort"`) del primer `Aggregate` de un
`EXPLAIN (FORMAT JSON, COSTS OFF)` de la consulta exacta, sin `ANALYZE`
ni lectura de datos. La mediana siempre modela un sort y distingue su
huella de decisión de la magnitud del tape. Ambas usan `n_validos` de
catálogo en el plan, pisos de 32 bytes para tipos fijos y 42 para
`numeric`, y dejan el objeto como `no_disponible` cuando el catálogo o
el motor no permiten una estimación. Cuando esa lectura de catálogo trae
`pg_class.reltuples` positivo, el plan lo reutiliza como una estimación
declarada del número de filas. La magnitud y las proyecciones de trabajo
de moda y mediana quedan entonces disponibles y dicen explícitamente
`estimado_catalogo`; no son duraciones ni mediciones. Un valor cero o
negativo —típico de una relación sin `ANALYZE`— conserva el estado
`sin dato filas`. Los demás motores conservan ese estado si no tienen
una lectura de catálogo ya disponible.

## Usage

``` r
plan_perfilado_dbi(
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
  instrumentar = FALSE,
  umbral_cardinalidad = .UMBRAL_CARDINALIDAD_COSTO_DBI,
  max_celdas_muestra = .MAX_CELDAS_MUESTRA,
  max_bytes_muestra = .MAX_BYTES_MUESTRA,
  bloque_filas = NULL,
  max_bytes_procesamiento = .MAX_BYTES_MUESTRA,
  max_bytes_materializacion = .MAX_BYTES_MUESTRA,
  columnas_opcionales = character(),
  aplicabilidad = NULL,
  sentinelas_numericos = c(-9, -99, -999, -9999, 999),
  cadenas_ausencia = NULL,
  clave = NULL,
  compilar = FALSE,
  centinelas_fecha = NULL,
  dominios = NULL,
  periodos = NULL,
  resumen_fechas = FALSE,
  fecha = NULL,
  estrategia_no_sql = c("no_medible", "bloques")
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
  cuando se recorta una muestra sobre `universo = "tabla_completa"` y
  existe una clave primaria o única de grado `total` en `meta$catalogo`,
  o se declara `clave =` (vía `...`), el motor muestrea automáticamente
  por una huella determinista de esa clave (`md5()` en PostgreSQL,
  MySQL, MariaDB y DuckDB; `HASHBYTES('MD5', ...)` en SQL Server; huella
  aritmética determinista en SQLite) con desempate por la clave y
  prefiltro por umbral en tablas grandes, declarando
  `metodo = "muestra_huella_motor"` y `reproducible = TRUE`. Cuando el
  motor calcula MD5 sobre UTF-8 (PostgreSQL, MySQL, MariaDB, DuckDB y
  SQL Server 2019 o posterior) y todas las columnas de la clave son de
  tipos canónicos (enteros, decimales sin ceros de escala, fechas ISO,
  UUID en minúsculas o texto UTF-8), se publica
  `alcance_reproducibilidad = "entre_motores_md5"`; en SQLite, en SQL
  Server anterior a 2019 o con otros tipos dependientes del motor se
  publica `alcance_reproducibilidad = "motor"` (reproducible en ese
  motor, sin prometer la misma selección en otros motores). Si no hay
  clave disponible ni `clave =`, DBI no garantiza el orden ni la
  pertenencia de la muestra limitada y `meta` lo declara expresamente.
  No se usa cuando `bloque_muestra = "solo_agregados"`. Con la lectura
  por bloques, la identidad de la fuente por bloques gobierna el
  recorrido y este pedido queda declarado en `meta$orden_muestra` con el
  motivo estable de que no gobierna esa vía. Si el motor falla al
  evaluar una columna del `ORDER BY`, la vía prueba cada componente,
  recupera las columnas legibles sin ese orden cuando puede y publica
  `alcance_distinto` con el motivo de que el orden pedido no se pudo
  aplicar; no lo presenta como si ninguna columna fallara. Además,
  `resumen_tabla$meta$estabilidad` informa si la tabla cambió durante la
  lectura cuando la corrida emite dos o más consultas de datos sobre la
  tabla completa (con o sin recorte de muestra): usa contadores
  síncronos de modificaciones del motor cuando están activos (`xmin` en
  tablas base/particionadas de PostgreSQL, `rowversion` o
  `sys.dm_db_index_operational_stats` en SQL Server,
  `performance_schema` verificando `setup_instruments`,
  `setup_consumers`, `setup_objects`, `setup_actors` (comodín activo sin
  exclusiones) y `threads` (sin hilos no instrumentados) en MySQL,
  `INNODB_SYS_TABLESTATS` y `UPDATE_TIME` en MariaDB) y emite el cierre
  más barato posible (`"sin_cambio_observado"` / `"cambio_detectado"`).
  En PostgreSQL y SQL Server un `UPDATE` que reescribe el mismo valor —y
  en PostgreSQL 9.3 un `VACUUM FREEZE`, que reescribe `xmin`— se
  informan de forma conservadora como `"cambio_detectado"` porque avanza
  la versión física o el contador de escrituras. En PostgreSQL sobre
  tablas de un millón de filas, la selección por huella y la
  verificación por `xmin` agregan en conjunto alrededor de 6 a 10 % al
  tiempo total del perfil completo. Cuando no existe un contador activo
  de modificaciones por tabla (por ejemplo en SQLite, DuckDB, vistas,
  MySQL con `performance_schema` parcial o con hilos/actores sin
  instrumentar, o sin permiso de DMV), el cierre consulta únicamente
  `COUNT(*)` y el máximo de la clave indexada de una columna (nunca
  agregados que barran columnas no indexadas) y publica
  `"sin_cambio_en_los_controles"` con la lista exacta de controles en
  `controles` y `cambio_detectado = NA`, porque un cambio fuera de esos
  controles no se detecta; cuando no se pudo verificar, declara
  `"no_verificada"` con su motivo. Si la consulta con huella falla en el
  motor en cualquier etapa (preparar, leer filas o cerrar), la vía no
  pierde `perfil_muestra`: reintenta la lectura sin orden garantizado
  (y, si hace falta, el descarte por columnas sin huella) con
  `reproducible = FALSE` y el motivo del motor. Como límite de la
  representación
  [`bit64::integer64`](https://bit64.r-lib.org/reference/bit64-package.html)
  en R, el entero de 64 bits `-9223372036854775808` (`-2^63`) comparte
  el patrón de bits de `NA_integer64_`: al materializarse en
  `perfil_muestra` se trata como valor ausente tanto en columnas comunes
  como al evaluar `clave =` sobre la muestra.

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
  describe la estrategia habilitada, no garantiza el método ejecutado.
  El método que efectivamente corrió está en `resumen_tabla$sql$metodo`,
  en la fila de `metrica = "mediana"` de cada columna, y es la única
  fuente por columna. `meta$estrategia_mediana` publica la resolución de
  la tabla: lo pedido (`estrategia_solicitada`), la forma que el motor
  aceptó para la tabla (`estrategia_resuelta`), su `estado` y su
  `motivo`. No es el método de cada columna: una columna que no puede
  usar esa forma —un `float8` de PostgreSQL, que la deja por la lectura
  exacta, o una columna con extremos— corre por otra (`dos_consultas` en
  la tabla completa, la CTE de ventanas en la muestra) y lo declara en
  su fila de `resumen_tabla$sql`, aunque `meta$estrategia_mediana` siga
  nombrando la consolidada. El `estado` de `meta$estrategia_mediana`
  describe la forma, no la fila: en `universo = "muestra_motor"` la fila
  de cada columna queda `estimado`. Las columnas
  `estrategia_solicitada`, `estrategia_resuelta` y `estado_estrategia`
  de `resumen_tabla$sql` son las de `n_distintos`
  (`meta$estrategia_distintos`), repetidas en todas las filas: en la
  fila de la mediana no describen la mediana. Una mediana resuelta por
  una forma exacta queda `estado = "calculado"` y
  `error_esperado = "no_aplica"`, aunque se haya pedido
  `"aproximada_motor"`; sólo una aproximación ejecutada queda
  `estado = "estimado"`. `universo = "muestra_motor"` combinado con
  `"aproximada_motor"` se rechaza temprano.

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
  restantes quedan en `no_disponible` con ese motivo. Cuenta toda
  sentencia que el perfilado manda al motor —la preparación, las
  lecturas de catálogo, el recorrido por bloques y, por el spool de la
  muestra del motor, la preparación que la corrida repite para publicar
  su plan—, la misma cuenta de `plan_perfilado_dbi()` y de
  `resumen_tabla$meta$consultas$emitidas`. No cuentan las sentencias de
  la guarda de transacción (`SAVEPOINT` y, en SQL Server, la pregunta
  por la transacción abierta antes de cada consulta), ni las lecturas de
  `pg_stat_statements` de `instrumentar`, ni las que algunos
  controladores mandan para contestar
  [`DBI::dbGetInfo()`](https://dbi.r-dbi.org/reference/dbGetInfo.html) o
  [`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html):
  duckdb 1.1 pregunta su versión y manda `SELECT 1`, una o dos veces por
  corrida. En DuckDB sí cuentan las dos sentencias con que la llamada
  averigua si hay una transacción abierta (ver la sección Dentro de una
  transacción). La preparación y el recorrido por bloques se cuentan
  pero no se omiten: sin ellos no hay perfil. Con
  `max_consultas = attr(plan, "total_maximo")` ninguna métrica queda
  afuera por el presupuesto.

- dialecto:

  Capacidad de acotar filas: `"auto"` la sondea, y `"limit"`, `"top"`,
  `"fetch_first"`, `"rownum"` o `"portable"` la declaran sin sondeo.

- incluir_valores:

  Si publica valores de celda. Con `FALSE` no se emiten las consultas de
  moda, extremos y mediana, y se limpian esos campos en el resumen, la
  muestra, los bloques, la cobertura y los motivos; también se retiran
  ejemplos de patrones, familias de acumuladores y cualquier muestra
  cruda. El mensaje de un error del motor, que cita la celda que lo
  provocó, se publica sin sus literales, como con
  `proteger_datos_personales = TRUE`. Los conteos, proporciones y
  agregados que no son valores de celda pueden seguir publicándose. La
  elección queda en `resumen_tabla$meta$incluir_valores` y
  `perfil_muestra$meta`.

- tamano_lote:

  Cantidad máxima de columnas por consulta consolidada. Se conserva por
  compatibilidad y, si se informa, fija el tamaño de las dos familias.
  Para control separado, usar `tamano_lote_planos` y
  `tamano_lote_distintos`.

- tamano_lote_planos:

  Cantidad máxima de columnas por consulta de agregados planos. En SQL
  Server y en columnas `float8` de PostgreSQL, el desvío conserva su
  cálculo centrado en una segunda pasada del mismo tamaño. El valor por
  omisión es 20.

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

  En el plan, si es `TRUE`, cronometra las consultas de preparación. No
  habilita consultas de datos ni agrega mediciones al objeto devuelto:
  sus costos siguen siendo predicciones. Por omisión es `FALSE`.

- umbral_cardinalidad:

  Proporción entre valores distintos y válidos que activa la omisión de
  la moda con `politica_costo = "por_cardinalidad"`. El valor por
  omisión es `0.5` sólo cuando esa política se pide explícitamente; se
  puede mover en cada llamada. Este argumento no gobierna la mediana:
  `meta$decisiones_costo` explica la decisión de cada métrica por
  separado. Para pedir todas las métricas use
  `politica_costo = "todas"`.

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
  `perfil_muestra`. Por defecto es `512 MiB`. En `muestra_motor`, el
  límite se cruza con el presupuesto de materialización y se respeta
  antes de cada `dbFetch(n)`: cuando la cota de tipos es demasiado
  conservadora se lee una fila inicial, se mide lo que ocupa en la
  materialización y los bloques siguientes se piden con el doble de
  margen sobre la fila más grande medida; el spool nunca pasa el tope.
  Si reduce la muestra, `meta$muestreo`, `meta$materializacion` y
  `cobertura_diagnosticos` informan las filas solicitadas, efectivas y
  cuál tope mandó. `meta$muestreo$filas_por_bytes` publica la cantidad
  obtenida de la medicion y `meta$muestreo$filas_por_bytes_fuente` lo
  declara. `Inf` desactiva este tope. No modifica los agregados SQL.

- bloque_filas:

  Entero positivo que activa la fuente por bloques de `tabla_completa`.
  Cada bloque se obtiene con un unico result set (`dbSendQuery()` +
  `dbFetch(n = bloque_filas)`) y se absorbe antes de liberar la entrada.
  `NULL` conserva el plan historico. El plan publica la proyeccion, el
  orden, los bloques previstos y el costo declarado sin leer las filas.
  Cuenta la preparación, las pruebas de la fuente, el recorrido, la
  segunda pasada del localizador, la moda exacta de binarios y decimales
  anchos, la media de los decimales en el decimal y la lectura de
  `perfil_muestra`; la mediana de un decimal cuyas centrales son dobles
  distintos, su desvío centrado y la moda de un entero leído como doble
  los piden los datos y van al extremo superior.

- max_bytes_procesamiento:

  Límite de bytes del estado retenido por el procesamiento por bloques y
  por la lectura del spool en R. Se comprueba antes de publicar un
  bloque o una pasada; si la materialización cabe pero el objeto que R
  necesita para perfilarla supera el límite, el perfil se descarta y se
  emite el mismo aviso de alcance degradado, con
  `tope_que_mando = "procesamiento"`. `Inf` desactiva este límite.

- max_bytes_materializacion:

  Presupuesto de bytes del spool externo de `muestra_motor`. Se mide por
  chunk antes de cada escritura, dejando espacio para el trailer; si no
  alcanza, se publica `spool_presupuesto_excedido` y se conserva la
  parte que entró como muestra recortada, con
  `muestra_inestable:presupuesto_materializacion`. En `muestra_motor`,
  el primer bloque se acota antes de pedirlo y los siguientes se adaptan
  a los bytes medidos; el resultado conserva `meta$bytes_muestra` y
  `meta$tiempos`. `Inf` desactiva este límite.

- columnas_opcionales:

  Nombres de columnas que no se cuentan como políticas exactas de
  faltantes porque sólo son aplicables a parte de las filas.

- aplicabilidad:

  Lista nombrada de reglas de aplicabilidad. Sus nombres se excluyen de
  la predicción de consultas exactas de faltantes.

- sentinelas_numericos:

  Valores numéricos que se consideran faltantes al anticipar las
  consultas de la métrica nueva. Acepta la lista por columna de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  resuelta contra las columnas de la tabla.

- cadenas_ausencia:

  Cadenas adicionales que se consideran ausencia textual al anticipar
  las consultas de la métrica nueva. Acepta la lista por columna de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- clave:

  Vector opcional con nombres de columnas clave para el muestreo
  reproducible por huella en el motor cuando no se especifica
  `orden_muestra`. Si el catálogo ya garantiza su unicidad no agrega
  consultas; si no, el plan cuenta la consulta de verificación de
  unicidad.

- compilar:

  Si es `TRUE`, le pide al motor que compile, sin ejecutarlas ni traer
  filas, las consultas SQL que el plan construye, y publica una fila por
  sentencia en `attr(plan, "compilacion")`. Ver la sección Compilación.

- centinelas_fecha, dominios, periodos, resumen_fechas, fecha:

  Los mismos que recibe
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  por `...` (ver
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)).
  Con `resumen_fechas = TRUE` o con `periodos` el plan cuenta las
  consultas de fechas que la corrida emite, también con `bloque_filas`.
  En el total va lo que no depende de los datos: la consulta de cuentas
  de fechas, la de periodos y, con `estrategia_no_sql = "bloques"`, una
  lectura por cada columna y por cada periodo que el motor no cuenta
  como R. En el extremo superior, lo que los datos pueden activar: la
  reserva de las centinelas incorporadas, la grilla de cada columna con
  fechas futuras y, con `"bloques"`, la lectura de cada texto que la
  muestra lea como fecha y la de cada columna o periodo que la consulta
  mande a R. Una columna con `aplicabilidad` se declara y no consulta.
  La corrida concilia estas consultas con las demás en
  `resumen_tabla$meta$consultas`. Fuera de SQLite, DuckDB y MariaDB sólo
  consulta la lectura por bloques, si se pide; con
  `universo = "muestra_motor"`, nada.

- estrategia_no_sql:

  Qué hacer, con `resumen_fechas = TRUE`, con una columna de fecha que
  el SQL no cuenta como R la lee: `"no_medible"` (por omisión) la
  declara `no_disponible` en `resumen_tabla$sql` con el motivo, y
  `"bloques"` la lee en R por bloques, con el mismo controlador, y la
  cuenta con la misma regla. Son tres casos: una columna de SQLite que
  guarda las fechas como texto (lo que deja un CSV importado; se mide
  con [`typeof()`](https://rdrr.io/r/base/typeof.html)), instantes a
  menos de un segundo de una fecha centinela lejana con resolución de
  microsegundos (R los lee como esa fecha) y una columna de texto que la
  muestra lee como fecha, cuyos formatos se deciden entonces con la
  columna entera. Rige igual para un `TIMESTAMP` de MariaDB (se lee en
  el huso de la sesión), para los motores donde la cuenta contra
  literales no está medida (fuera de SQLite, DuckDB y MariaDB: toda
  columna de fecha), para una conexión de DuckDB abierta con
  `tz_out_convert = "force"` y un huso distinto de UTC (R recibe
  instantes que no son los que guarda el motor) y para `periodos`:
  columnas que el motor guarda como texto, fechas guardadas como texto
  en SQLite, una columna que no corresponde a todas las filas
  (`aplicabilidad` o `columnas_opcionales`), que la consulta del motor
  no traduce, o, en DuckDB, un `infinity` en un periodo cuyas dos
  columnas son de tipos distintos (R lee el infinito de cada tipo como
  una fecha lejana distinta; con las dos del mismo tipo el motor lo
  cuenta como R lo lee). Un periodo contado en R lo cuenta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las columnas leídas enteras, con las mismas declaraciones: da lo
  que da la memoria o no se publica. Si
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  no lee las dos columnas como fechas, o la tabla pasa de cinco millones
  de filas —ahí se deja de leer—, el periodo queda `no_disponible` con
  su motivo. Leer por bloques es lo que R lee en cualquier motor. Mismo
  nombre y valores que en
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md).

## Value

Data frame de clase `plan_perfilado_dbi` con `clase_consulta`,
`n_consultas`, `n_consultas_max` y `alcance`, y los atributos `total`,
`total_minimo`, `total_maximo`, `total_lotes_rechazados`, `columnas`,
`columnas_numericas`, `dialecto`, `consultas_emitidas`, `metricas`,
`metricas_ejecucion`, `politica_costo`, `estrategia_distintos`,
`fuente_cardinalidad_costo`, `moda_guardian`, `mediana_consolidada`,
`filas`, `filas_fuente`, `estimacion_filas`, `proyecciones`,
`mediana_escalar`, `tamano_lote_planos`, `tamano_lote_distintos`,
`estimacion_derrame`, `estimacion_derrame_moda`,
`estimacion_derrame_mediana`, `celdas`, `memoria_procesamiento`,
`max_celdas_muestra`, `max_bytes_muestra`, `tope_muestra` y `muestreo`,
y, cuando se pide `distintos`, `supuesto_costo_distintos`.
`memoria_procesamiento` siempre tiene `estado = "no_estimada"`: no es
una estimación de consumo, sino la declaración de su ausencia, el
motivo, la magnitud del trabajo y referencias medidas de otras corridas.
El atributo `estimacion_derrame` es independiente: sólo describe la
estimación del hash en el motor para `COUNT(DISTINCT)` y no la memoria
del procesamiento en R. `estimacion_derrame_moda` y
`estimacion_derrame_mediana` describen, respectivamente, el nodo real de
la moda y el sort de la mediana. Sus lotes deciden por el máximo de
columna; en una mediana consolidada, `estado_io_total_bytes` es sólo la
suma informativa de los tapes. `muestreo` declara si la forma muestreada
se pudo construir sin emitir una consulta de datos. En
`universo = "muestra_motor"`, cuando su `estado` es `"no_disponible"`,
el plan excluye las métricas SQL de esa muestra y `supuesto` conserva el
motivo. Cuando se pide `bloque_muestra = "solo_agregados"`, también
conserva ese valor en el atributo `bloque_muestra` y no incluye la fila
de la lectura de muestra.

Los atributos `max_celdas_muestra`, `max_bytes_muestra` y `tope_muestra`
declaran la cota que se aplicará al bloque `perfil_muestra`. El plan no
lee datos: resuelve la cota de celdas con el ancho del esquema y declara
que la cota de bytes requiere una sonda de hasta cien filas durante la
corrida. Si la muestra pedida supera la cota de celdas, `tope_muestra` y
[`print()`](https://rdrr.io/r/base/print.html) lo dicen antes de correr.
La fila de consulta de muestra conserva su conteo separado de los
agregados; los topes no cambian ninguna consulta SQL de resumen.

El costo no se declara como un número sino como un rango: `total_minimo`
es el extremo inferior, que supone que la política omite la moda cuya
cardinalidad no se conoce, y `total_maximo` el superior, que supone que
la ejecuta. `total` conserva el extremo inferior para no cambiar a los
consumidores que usan la cuenta esperada;
[`print()`](https://rdrr.io/r/base/print.html) y los atributos
`total_minimo` y `total_maximo` publican el intervalo completo cuando
una condicional puede agregar consultas. La mediana queda fuera de ese
supuesto proporcional y se cuenta según las columnas numéricas
solicitadas. Ambos extremos incluyen la preparación y el perfilado; el
rechazo de lotes puede agregar las sondas de bisección declaradas por
`total_lotes_rechazados`. El costo real cae entre los extremos cuando
`universo` es `"tabla_completa"` o `attr(plan, "muestreo")$estado` es
`"disponible"`; en `"no_sondeado"` la forma sólo se construyó localmente
y el intervalo queda condicionado a que la sonda de la corrida la
acepte. Si la forma muestreada no se puede construir, el plan declara
ese caso, excluye sus métricas del rango y `attr(plan, "supuesto")` dice
por qué. El extremo superior incluye además, con
`universo = "muestra_motor"` y una forma `TABLESAMPLE`, el reintento del
resumen cuando la muestra vuelve con menos de la mitad de lo pedido, que
depende de las páginas que sortee el motor. La cuenta es la de
`max_consultas` y la de `resumen_tabla$meta$consultas$emitidas`: toda
sentencia que el perfilado manda al motor, salvo las de la guarda de
transacción, las lecturas de `pg_stat_statements` de `instrumentar` y
las que algunos controladores mandan para contestar
[`DBI::dbGetInfo()`](https://dbi.r-dbi.org/reference/dbGetInfo.html) o
[`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
(duckdb 1.1, una o dos por corrida). En DuckDB el plan cuenta además,
con la clase `guarda de transaccion abierta (DuckDB)`, las dos
sentencias con que la llamada averigua si hay una transacción abierta.
Con `max_consultas` igual a `total_maximo` ninguna métrica queda afuera
por el presupuesto; con `total`, sólo las consultas que piden los datos
pueden quedar afuera.

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

El plan previo no publica duraciones, CPU, ni filas o bytes medidos, ni
agrega una proyección temporal de `COUNT(DISTINCT)`. Puede publicar
filas estimadas por `pg_class.reltuples` y proyecciones de trabajo de
moda/mediana, siempre rotuladas como estimación de catálogo y no como
medición. Aunque el plan de una corrida conserve el atributo
`supuesto_costo_distintos`, la medición y la proyección sólo aparecen en
`resumen_tabla$meta$costo_distintos`, después de ejecutar el primer
lote. El atributo sólo declara por qué esa proyección no existe antes de
correr; no es una duración ni una estimación temporal.

Si se pide `politica_costo = "por_cardinalidad"`, el plan busca primero
una garantía estructural o una fuente de catálogo. Si la fuente queda
desconocida, no emite un agregado para aclararla: `n_consultas` omite la
moda y `n_consultas_max` deja abierto el camino que la ejecuta. La
mediana se conserva porque su costo medido es plano frente a la
cardinalidad y está gobernado por las filas. La corrida mide `distintos`
sólo si la política lo necesita. La política por omisión es `"todas"`:
el paquete no elige por el usuario. Una fuente estructural se resuelve
aunque la estrategia de distintos este omitida o no disponible; esta
ultima solo gobierna si se puede medir. El catálogo se consulta siempre
por la vía del controlador para publicar `resumen_tabla$meta$clave`,
`resumen_tabla$meta$catalogo` y `attr(plan, "catalogo")` (clave
primaria, restricciones e índices únicos, claves foráneas y columnas
`NOT NULL`); es una lectura de metadatos y no un recorrido de la tabla.
Las garantías de grado `total` permiten omitir consultas redundantes
(`COUNT(col)` en `NOT NULL`, `COUNT(DISTINCT)` en unicidad simple,
verificación de `orden_muestra` y conteo de duplicados exactos en tabla
completa) y quedan reflejadas en el conteo de consultas.

Contar sólo el motor daría juicios falsos con números ciertos: una tabla
chica con una columna de geometría en texto pide pocas lecturas
—magnitud `"baja"` del lado del motor— y puede tardar mucho más de lo
que eso sugiere, porque el trabajo está en la comparación de formas, que
no es una lectura de fila. El método de impresión muestra las dos
mitades, avisa cuando la magnitud es alta y nombra las palancas para
acotarla, que no son las mismas de un lado que del otro.

## Details

`estrategia_distintos` declara la procedencia de `n_distintos` antes de
la corrida y conserva por separado lo pedido, lo resuelto y el estado.
No hay `auto`: `"exacta"` es el valor por omisión, `"aproximada_motor"`
queda `no_disponible` si el motor no ofrece una función aceptada,
`"catalogo"` lee `pg_stats.n_distinct` en PostgreSQL y publica una
estimación con estado `estimado_catalogo` cuando
`universo = "tabla_completa"`, y `"omitida"` no emite el agregado. Con
`universo = "muestra_motor"`, `catalogo` queda `no_disponible`: sus
estadísticas describen la relación entera y no el subconjunto de la
corrida. Un `n_distinct` positivo es un conteo y uno negativo una
fracción de las filas. Si la relación tiene descendientes se usa la fila
`inherited = TRUE`, que describe la consulta sin `ONLY`; si no tiene
hijas se usa la única fila propia. La fracción se convierte con la suma
de `pg_class.reltuples` de la jerarquía. Si falta una estimación
utilizable —por ejemplo, antes de `ANALYZE`— o hay filas ambiguas, se
conserva `no_disponible`, nunca cero. `fuente_cardinalidad_costo` sigue
siendo independiente y sólo describe el número usado por la política de
costo cuando esa política se pide.

El plan previo no proyecta segundos para `COUNT(DISTINCT)`: no lee los
datos y, por tanto, no tiene una referencia medida. La única referencia
honesta es el primer lote de distintos de la corrida, pero obtenerla
cuesta una consulta que este planificador no emite. Durante
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
cuando hay más de un lote y la instrumentación está activa, esa primera
medición se multiplica por la cantidad de lotes y se publica en
`resumen_tabla$meta$costo_distintos`. El aviso temporal llega después
del primer lote y antes del segundo; con un solo lote se declara que no
hay nada que proyectar.

La memoria del procesamiento no se estima: no escala de forma predecible
con las filas ni con las celdas, y eso se midió. El atributo
`memoria_procesamiento` conserva esa declaración, la magnitud conocida
del trabajo (`filas`, `celdas` y `pares_texto`) y referencias medidas de
otras corridas. Esas referencias no son una predicción para la tabla del
plan: traer costó aproximadamente 0,13 GB por millón de filas y procesar
en R aproximadamente 1,0-1,5 MB por cada mil filas, pero esta segunda
cifra varió 1,62x entre tablas de la misma magnitud.

Ver todas las filas y tener todas las filas en memoria no son lo mismo.
En corridas de referencia, 4,5 millones de filas entraron en 0,6 GB y
tardaron 25 segundos, mientras que procesar 4,5 millones ocupó
aproximadamente 7 GB y procesar 12,8 millones aproximadamente 19 GB. El
problema observado está en el procesamiento en R, no en la red ni en el
motor.

Para `universo = "muestra_motor"`, el plan declara una única selección
que se pagará para cerrar el spool externo de sesión cliente.
`attr(plan, "materializacion")` publica `pagado = FALSE`, backend,
versión, presupuesto, result set, fetches esperados, filas y bytes;
`attr(plan, "pasadas")` declara que valor, índice y LSH leerán el mismo
spool. La referencia medida para justificar este costo es PostgreSQL 16,
2 millones de filas: 10.000, 100.000 y 500.000 filas dieron
0,448/1,684/5,598 s con spool frente a 0,814/2,178/3,265 s reordenando
cada pasada; el cruce está entre 100.000 y 500.000, y la elección
prioriza identidad. Con el bloque de filas, esa vía no corre los
agregados SQL de la muestra: el plan cuenta la preparación, la
estimación de bytes por fila del catálogo, el conteo del universo cuando
la preparación no lo hace, la selección de la muestra —dos en el extremo
superior, si un `TABLESAMPLE` corto se reintenta— y la preparación que
la corrida repite para publicar su plan en `meta$plan`.

## Compilación

Con `compilar = TRUE` el plan, además de contar, le pide al motor que
compile cada consulta que construye, sin ejecutarla ni traer filas, y
publica una fila por sentencia en `attr(plan, "compilacion")`, con
`consulta_id`, `etapa`, `sql`, `estado`, `motivo` y `metodo`. `estado`
es uno de:

- `compilada`: el motor la aceptó.

- `no_compilada`: el motor la rechazó, y `motivo` trae su mensaje: una
  columna o una función que no existe, un permiso que falta.

- `candidata_rechazada`: una forma candidata de un sondeo de capacidad
  que el motor no conoce. El plan de perfilado prueba el desvío como
  `STDDEV_SAMP` y como `STDEV` en todo motor, y la corrida usa la que el
  motor acepte. Las candidatas no se compilan: se mandan como las manda
  la corrida, sobre una constante y sin leer filas, y la rechazada queda
  con `metodo` vacío; es la respuesta del sondeo, no una falla del plan.
  Un plan sano no tiene ninguna fila `no_compilada`.

- `no_verificada`: no se pudo saber, porque la forma de la consulta
  depende de un resultado anterior, de una capacidad del controlador o
  de una lectura de filas, porque el motor no tiene una vía de
  compilación, o porque venció el tope de tiempo de PostgreSQL (ver
  abajo).

En
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md)
hay una fila por sentencia compilada, así que su número no tiene por qué
ser el de `attr(plan, "consultas")`: la consulta de la sesión se ejecuta
al planificar, como sin compilar, y las métricas por bloques se calculan
en R sobre una sola lectura por entidad, la fila `bloques`.

`metodo` dice cómo se compiló: `EXPLAIN QUERY PLAN` en SQLite (los
`PRAGMA` de lectura del catálogo se mandan tal cual, `dbSendQuery`),
`EXPLAIN` en DuckDB y PostgreSQL, `PREPARE` y su `DEALLOCATE` en MySQL y
MariaDB —su `EXPLAIN` ejecuta las subconsultas en `FROM`, `JOIN` y
`WITH`; `PREPARE` no ejecuta ninguna y rechaza lo mismo—, con un nombre
único por llamada que no pisa las sentencias preparadas del usuario, y
`SET NOEXEC ON` en SQL Server.

Lo que compilar no garantiza:

- Que la consulta corra. Los errores que dependen de los datos aparecen
  sólo al ejecutar: un `CAST` de un texto que no es un número compila y
  falla en la corrida. Y no todos los motores comprueban los permisos al
  compilar: SQL Server, bajo `SET NOEXEC ON`, no mira ninguno —una
  columna, una función o una tabla negada compilan—, y PostgreSQL 9.3 no
  mira el `EXECUTE` de una función, tampoco dentro de una vista. En esos
  casos la sentencia queda `compilada` y falla en la corrida.

- Que no se ejecute nada. Las consultas de preparación del plan —las del
  esquema, las que llevan `WHERE 1 = 0` o `LIMIT 0`, las pruebas sobre
  literales como `SELECT ... FROM (SELECT 1 AS ...)`— se ejecutan como
  en el plan sin compilar, sin leer filas de la tabla y sin tope de
  tiempo. Y en PostgreSQL el planificador pliega las constantes: una
  función declarada `IMMUTABLE` con argumentos constantes se ejecuta al
  planificar, aunque lea datos, también en esa preparación, como sin
  compilar. Por eso cada sentencia compilada lleva un tope de 2 segundos
  (`SET LOCAL statement_timeout`, o el de la sesión si es menor) dentro
  de una transacción propia que se revierte —o de un punto de guardado,
  dentro de una transacción del usuario—: la que lo supera queda
  `no_verificada` con ese motivo, y la sesión vuelve al tope que tenía.
  `PREPARE` no planificaría, pero en PostgreSQL tampoco comprueba los
  permisos, y `EXPLAIN` sí.

- En SQL Server, que exista una tabla: bajo `SET NOEXEC ON` la
  resolución de nombres de tablas es diferida y una tabla que no existe
  compila.

Compilar nunca toca la transacción del usuario, y si no lo puede
garantizar no compila: falla con `lupa_error_transaccion_dbi` antes de
mandar nada. Así pasa en DuckDB con una transacción abierta —cualquier
error del motor, también el de una sentencia que no compila, la aborta
sin aviso, un
[`DBI::dbCommit()`](https://dbi.r-dbi.org/reference/transactions.html)
posterior pierde lo hecho en ella, y DuckDB no tiene `SAVEPOINT`—, y en
PostgreSQL cuando no se puede saber si hay una abierta (una abierta a
mano y ya abortada). Si la hay se averigua antes de empezar —la marca de
RPostgres, o `now()`, que dentro de una transacción no cambia— y no por
si un `SAVEPOINT` falla. SQLite, MySQL y MariaDB no abortan la
transacción por el error de una sentencia.

La conexión queda como estaba. En SQL Server `NOEXEC` es un estado de la
sesión que, prendido, hace que nada de lo que se mande se ejecute, sin
error: se apaga después de cada sentencia y se comprueba con un
`SELECT 1`. Si no se puede apagar, la llamada falla con
`lupa_error_conexion_dbi` y dice que la conexión quedó inutilizable.
Dentro de una transacción abierta con `XACT_ABORT ON`, una sentencia que
nombra una columna que no existe deshace la transacción aun bajo
`NOEXEC`: `XACT_ABORT` se apaga mientras se compila y se vuelve a
prender.

Una interrupción —el tope de
[`setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html), Ctrl-C— no
es una sentencia que no compila: la compilación deja la sesión como
estaba y la vuelve a señalar, sin devolver el plan. En SQL Server, una
que llega en medio de un envío puede dejar al controlador sin poder
apagar `NOEXEC`; entonces la llamada falla con
`lupa_error_conexion_dbi`, dice que la conexión quedó inutilizable y que
la compilación se interrumpió, y la conexión hay que cerrarla.

## See also

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md),
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)

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

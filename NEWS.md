# lupa 0.1.0

Primera versión publicada.

## Perfilado

- `perfilar()` describe cada columna de un `data.frame`: el tipo implícito, los
  faltantes reales y los disfrazados (códigos y centinelas), los patrones de
  formato, los formatos de fecha mixtos, los valores extremos, las filas
  duplicadas, las dependencias funcionales y las claves. Lo que no puede medir lo
  declara, con su motivo, en vez de dejarlo en cero.
- `perfilar_dbi()` hace el perfil dentro del motor, por DBI, con una consulta por
  lote y no una por columna, muestras del motor y presupuestos de trabajo
  declarados. Está probado contra PostgreSQL (16 y 9.3), SQL Server 2022, MySQL
  8.4, MariaDB 11.8, SQLite y DuckDB. `perfilar_coleccion()` y
  `relaciones_coleccion()` perfilan varias tablas y proponen las relaciones entre
  ellas, y `estimar_costo()` anticipa lo que va a costar una corrida.
- `analizar()` hace el recorrido completo: el perfil, las distribuciones, las
  asociaciones, el análisis temporal, la clasificación de las variables y una
  propuesta de modelo de medición.
- Cada diagnóstico también se puede pedir por separado:
  `detectar_duplicados_aproximados()`, `detectar_claves()`,
  `detectar_relaciones()`, `detectar_dependencias()`,
  `detectar_formatos_fecha()`, `descubrir_patrones()`, `inferir_tipo()` y
  `distribucion_valores()`.
- `columnas_duplicadas` publica un hallazgo por **grupo** de columnas con el
  mismo contenido, no uno por par: `columna` es la primera del grupo en el orden
  de los datos, la evidencia las nombra a todas (`A = B = C; comparadas sobre 5
  filas`) y `n_afectados` las cuenta a todas, como `filas_duplicadas` con sus
  filas. Con dos columnas el hallazgo es el de siempre. Si un nombre no se puede
  leer suelto —vacío, repetido, con espacios al borde o con `=`, `;`, `,` o
  comillas—, la evidencia cita todos los del grupo y el repetido lleva su
  posición (`"X" (columna 1) = "X" (columna 2)`). Dos columnas son iguales en lo
  que el perfil lee como contenido: dos factores sin orden con los mismos
  niveles en otro orden son la misma columna, y un entero y un doble no. Sin
  filas no se compara: no hay hallazgo, y `general$columnas_duplicadas`, que
  tiene una fila por columna repetida junto a la primera de su grupo, queda
  vacía. `planificar_limpieza()` propone una decisión por grupo —marcarlo, o
  eliminar las repetidas y conservar la primera— y `comparar_perfiles()` compara
  los grupos por las igualdades que afirman: si C deja de ser igual a `A = B`,
  publica un resuelto y no dos; si un grupo se separa sólo porque una columna
  cambió de tipo declarado, publica `no_evaluado`, porque el diagnóstico no
  volvió a compararlas; y un perfil guardado con la forma por pares se compara
  sin deriva falsa. Con 1.000 columnas iguales eran 499.500 hallazgos:
  `planificar_limpieza()`, `comparar_perfiles()` y `analizar()` no terminaban en
  5 minutos y ahora tardan 2, 3 y 22 s, y `print()` del perfil pasa de 37 s a
  0,6 s.

## Medición de la calidad

- Marcos de calidad con dimensiones y factores que declara el usuario
  (`marco_calidad()`), y los de ISO/IEC 25012, CEPAL y AGESIC listos para usar.
- Métricas reutilizables con su granularidad explícita (`metrica()`,
  `metricas_nucleo()`), modelos de medición (`modelo()`, `especializar()`,
  `instanciar()`), mediciones (`medir()`), agregación tipada (`agregar()`) y
  perfiles de evaluación (`perfil_evaluacion()`, `evaluar()`).
- Validadores de formato: la cédula y el RUT de Uruguay, el correo, la URL, ISO
  3166, ISO 4217, Luhn y mod 97.
- Históricos versionados (`acumular_historico()`, `guardar_historico()`), la
  comparación de dos perfiles (`comparar_perfiles()`) y la detección de deriva
  (`detectar_deriva_calidad()`).

## Limpieza

- `planificar_limpieza()` propone un plan editable de acciones, cada una con su
  justificación y la severidad de lo que la motivó. `aplicar()` lo ejecuta sobre
  una copia y deja el registro de lo que cambió, y `guiar_limpieza()` muestra
  ejemplos para decidir. `lupa` nunca modifica los datos que recibe.

## Informes

- `reportar()` escribe un informe HTML autocontenido, sin depender de software
  externo para generarlo.

## Documentación

- El paquete trae las guías 1 a 6, el camino básico: de la primera tabla al plan
  de limpieza. Las guías 7 a 12 (métricas, marcos, histórico, bases de datos,
  colecciones y escala) y los 4 artículos de referencia están en el sitio,
  <https://sebollin.github.io/lupa/>.
- Los guiones de `benchmark/` que publican cifras dejan su CSV en
  `benchmark/datos/` con fecha, entorno y huella del código; el de Raha agrega el
  SHA-256 de cada insumo, y el del tope de largo mide Jaro-Winkler con el mismo
  `p` que el paquete (el margen publicado para `max_largo_valor = 10000` era
  mayor que el real).
- El README muestra una captura del informe HTML de `reportar()`, rehecha con
  `scripts/capturar-informe-html.R`. «El vacío por diseño» y «Marcos y modelos
  de calidad» empiezan por el ejemplo y dejan la explicación para después.
- La guía «Escala y duplicados aproximados» termina en los lotes sin pérdida y
  resume cada tope en una línea. El detalle de los topes y del aviso de tabla
  ancha pasa a «Motores y escala», y el de los hilos a «Evidencia y rendimiento».
- Las 6 guías que trae el paquete y los 10 artículos del sitio tienen frases más
  cortas, cifras en dígitos y listas donde había enumeraciones, con la misma
  información y el mismo código. Los artículos van en impersonal.
  `CONTRIBUTING.md` reúne las reglas de redacción de la documentación.
- Las cifras de los artículos que el paquete define (umbrales, motores de la
  matriz, dependencias, métricas, topes) se calculan al construir el sitio, no se
  escriben a mano. Al atarlas salieron 2 frases falsas en «Alcance y cobertura»:
  sin `bit64` el perfil de una columna `integer64` se detiene con un error, no
  deja 5 medidas en `NA`; y un documento sin su dígito verificador necesita 7
  cifras, no 6. La tabla de motores del README nombra también PostgreSQL 9.3.
- El sitio tiene una versión en inglés de 4 guías («Guides in English»):
  *Getting started*, *Profiling a database*, *Engines and scale* y *From
  findings to metrics*. Viven sólo en el sitio, y `data-raw/comparar-guias.R`
  comprueba antes de construirlo que cada una tenga los mismos trozos, el mismo
  código y las mismas cifras en línea que su pareja en castellano.
- El logo y los íconos del sitio usan el hexágono regular: lados iguales y
  bordes a 30°. El anterior era más alto que ancho de la cuenta.

## Rendimiento

- La tabla de formatos de fecha se arma una vez, al instalar el paquete, y no en
  cada columna; el enmascarado de datos personales pasa a pedirse (ver abajo); y
  en los caminos que más se repiten se evitan las llamadas a expresiones regulares
  PCRE, que en Windows cuestan unos 47 µs cada una aunque el vector sea corto.
- Las tablas chicas se arman sin el costo por columna de `data.frame()`, la
  detección de fechas no prueba los formatos que no pueden casar con ningún
  valor, y `analizar()` clasifica una sola vez los datos personales de la tabla
  que mide. Con la protección pedida, el barrido que decide qué campos guardan
  nombres de columna no intenta partir un valor sin coma, y la fila de una
  columna del perfil se arma sin pasar por `[.data.frame`. El pliegue con que
  la protección compara los valores protegidos con el texto publicado ya no
  arma su tabla de tres mil caracteres en cada llamada: un texto ASCII sólo
  baja la caja, y uno corto usa la parte del mapa que trae. Sobre la tabla de
  ejemplo de 13 filas, `perfilar()` tarda 0,11 s y
  `analizar()` 0,21 s en una sesión ya iniciada, y 0,20 s y 0,34 s en la primera
  llamada de una sesión nueva. Con la protección pedida, `perfilar()` tarda
  0,25 s y 0,39 s, y `analizar()` 0,62 s y 0,79 s. Las cifras son de `benchmark/medir_costo_fijo.R` y quedan
  en `benchmark/datos/medir-costo-fijo.csv`.
- `perfilar()` sobre tablas anchas con muchas columnas iguales o de códigos de
  texto es varias veces más rápido. La búsqueda de casi duplicados del
  vocabulario no calcula la mitad de la matriz de distancias que no lee, y
  `columnas_duplicadas` publica un hallazgo por grupo (ver «Perfilado»). Las columnas iguales se buscan agrupándolas
  por una huella y comparando cada una con un representante, no par por par.
  Con 500 filas y 1.000 columnas iguales, de 379 a 387 s pasa a 28 a 30 s (los
  2 CSV fechados de `benchmark/medir_topes_valores_ancho.R` en
  `benchmark/datos/`).

## Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64, desde
  la 4.0.5. Con bit64 anterior a 4.6.0, `match()` de R iguala entre sí todos los
  valores negativos: `lupa` empareja esos valores por su texto, con su propio
  `match()` y su `%in%`, y da el mismo perfil que con bit64 nuevo. Sin bit64 se
  niega con la condición `lupa_error_integer64` en lugar de publicar distintos,
  modas y medianas equivocados. Las pruebas de `integer64` corren todas con
  bit64 4.0.5 en R 4.2.0, y una prueba falla si alguna función de base que
  compara por debajo (`merge()`, `factor()`, `setdiff()`, `order()`…) recibe
  del paquete una columna `integer64`.

## Datos personales

- La clasificación de posibles datos personales (documentos, nombres, fechas de
  nacimiento) corre siempre, como diagnóstico. Su reemplazo en lo que el paquete
  publica es un complemento que se pide con `proteger_datos_personales = TRUE`:
  publicar o no los valores de una base es decisión de quien la usa.

## Correcciones

- `planificar_limpieza()` no convierte un número escrito como texto que el
  perfil no lee: un teléfono como `099.123.456` entre importes con coma decimal,
  que el perfil deja fuera del resumen, hacía que el plan propusiera la
  conversión y `aplicar()` escribiera 99123456. Ahora la conversión queda
  `bloqueada`, como con cualquier valor que no responde al formato, hasta que se
  decida qué hacer con él. Con columnas iguales en las que algún nombre se
  repite en la tabla, el plan no propone acción —proponía dos `bloqueada` sin
  saber sobre cuál columna actuar— y el hallazgo queda en
  `hallazgos_sin_accion_por_columna_ambigua`, con el remedio.
- `inferir_tipo()` y `perfilar()` leen como número un importe con los miles
  agrupados (`1.234,56`, `1,234.56`, `12.500.000`) con la convención de la
  columna: quedaba `texto` y sin resumen, aunque la fila publicaba la
  convención, y la misma columna sin los puntos de miles se resumía. Si la
  columna aporta las dos convenciones, cada valor se lee con la que trae. Sin
  evidencia de convención —una columna de `1.234.567`— sigue como texto, y un
  primer grupo de miles que empieza con cero (`099.123.456`, la forma de un
  teléfono) no se lee como número en ninguna. El resumen lee sólo lo que la
  inferencia cuenta como número, también fuera de su muestra: lo demás —un
  teléfono, `0x1A`, `5.`— queda afuera y lo declara
  `n_valores_excluidos_resumen`. Un importe con moneda o unidad (`USD 1,234.56`,
  `12 kg`) sigue siendo un número escrito como texto: `numero_como_texto` lo
  señala y `planificar_limpieza()` lo convierte.
- La evidencia de `tipo_declarado_distinto` ya no dice que la conversión «no
  pierde ningún valor» cuando pierde algo: nombra el cero inicial de un código
  (`007`), la precisión, la convención sin decidir o las fechas de lectura
  ambigua, las mismas razones por las que el plan no convierte.
  `fecha_nacimiento_fuera_rango` cuenta las filas fuera de rango, que publicaba
  como `NA`.
- `detectar_formatos_fecha()` reconoce la `t` y la `z` minúsculas de RFC 3339
  y cuenta la fecha civil distinta en UTC de un `POSIXlt` como la de su
  `POSIXct` (daba 0).
- `validar_ci_uy()` completa con ceros la cédula guardada como número (`1236`
  es `00001236`, y daba `FALSE`) y rechaza el cuerpo `0000000`, el relleno de
  muchas planillas. `validar_url()` rechaza la contrabarra y los invisibles de
  formato (U+200B, U+FEFF, U+200D, marcas de dirección): un escape de su clase
  de caracteres dejaba pasar `https://ejemplo.com\@otro.com`.
- Las relaciones aritméticas aditivas entre columnas con decimales fijos cuentan
  como rota una diferencia de medio cuanto o más mientras los valores tengan
  menos de 15 cifras significativas en ese cuanto: con centavos, importes de
  hasta 1e12; con enteros, hasta 1e14. Con la tolerancia relativa por omisión,
  `comision + neto = bruto` con 30 filas rotas por centavos salía `ok` con 0
  filas sobre importes de 1e8, y contaba 21 de 30 sobre importes de 1e6. Por
  encima de ese límite un decimal guardado no se distingue del redondeo de un
  texto de 15 cifras: la tolerancia relativa manda y la evidencia dice cuánto
  tolera. Con más de la mitad de las filas rotas por un cuanto la identidad no
  se publica como sana: se descarta y se cuenta en
  `meta$aritmetica_columnas$identidades_descartadas_cuanto`. En una suma, la
  tolerancia se mide también sobre los sumandos: `a = 1e9 + x`, `b = -1e9 + y`,
  `c = x + y` acusaba 16 de 300 filas sanas. La evidencia declara la cota, y las
  proporcionalidades siguen con la tolerancia relativa.
- `desviacion_benford` dice si la desviación la producen los faltantes
  disfrazados que no se declararon, con la prueba sin ellos: 200 valores `999`
  entre 4.000 importes daban p = 1e-45, y sin ellos p = 0,22.
- Con `sentinelas_numericos` declarado, `valor_concentrado` deja afuera lo
  declarado, como el resumen: publicaba como moda el `-99` que el resumen
  excluía.
- `detectar_dependencias()` no publica un par cuyo determinante no acierta más
  filas que la moda del dependiente (con un `estado` al 99,66 % salían cinco
  dependencias que no decían nada), y lo que encuentra en la muestra lo
  recalcula sobre la tabla completa: una muestra alineada con la estructura de
  los datos publicaba 0,996 de cumplimiento donde la tabla da 0,899.
  `detectar_relaciones()` calcula las coberturas sobre las columnas completas
  (con una muestra alineada publicaba 1 donde era 0,5) y `muestra` ya no
  recorta. `clasificar_variables()` cuenta `n_niveles_observados` sobre la
  columna entera también sin perfil, y `meta$aritmetica_columnas` se publica
  también cuando la única identidad candidata se descarta.
- La identidad de filas ya no desborda en R anterior a 4.6. Con dos o más
  columnas de decenas de miles de valores distintos, `aplicar()` con
  `conservar_primera_duplicada` podía borrar filas legítimas (23.673 de 60.100
  en una tabla con 200 repetidas), y `EntidadDuplicada`, `referencial()` y las
  acciones de duplicados contaban grupos equivocados. Una matriz o una tabla
  anidada como columna se aplana para esa identidad, como en el perfil.
- `medir()` ya no se cae con una columna lista —lo que un motor devuelve para
  una columna binaria— cuando una métrica no puede producir medidas: la métrica
  queda `no_medible`. Una tabla anidada, una matriz de listas o un entorno en una
  celda tampoco la hacen caer, y `perfilar()` ya no publica más filas completas
  que filas con una matriz de listas.
- `NoNulo` compara `valores_nulos` con la misma normalización que `perfilar()`:
  sin los blancos de los bordes y sin distinguir mayúsculas. Una celda de solo
  blancos o un tabulador cuentan como nulos para los dos.
- `agregar()` de `instanciaEntidad` a `entidad` conserva la instancia en el
  nombre (`agregada:ratio:ClaveDuplicada@t.a.b`): dos claves compuestas de la
  misma tabla ya no se funden en una sola fila con un resultado mezclado. De
  `instanciaAtributo` a `atributo`, dos instancias de una métrica sobre la misma
  columna llevan la instancia en el nombre solo en ese caso. Más arriba, el
  nombre conserva la instancia que traen todas las partes, y `agregar()` se
  niega a promediar dos instancias de una misma tabla o columna, que la harían
  pesar doble, y dice cómo seguir.
- `medir()` arma su tabla de medidas de una sola vez, sin una tabla intermedia
  por métrica: sobre una tabla de 80.764 filas y 153 columnas de texto con 100
  métricas, bajó de 133 s a 56 s y su pico de memoria de 4,9 GB a 3,5 GB, que es
  del orden de la tabla de medidas que devuelve (8 millones de filas).
- En `perfilar_dbi()` con `universo = "tabla_completa"`, el desvío de las
  columnas de SQL Server y de `double precision` de PostgreSQL se calcula en
  lotes, centrado en la media que ya trajeron los básicos, y no en una consulta
  por columna. Sobre una tabla de un millón de filas y 27 columnas numéricas,
  el desvío pasó de 27 consultas a 2 y de 1,42 millones de lecturas lógicas a
  105 mil en SQL Server, con las mismas cifras. En un motor que lee del disco,
  cada consulta de desvío recorría la tabla entera.
- El desvío de una columna cuyo mayor valor absoluto queda fuera de
  `[1e-150, 1e150]` se calcula sobre sus valores divididos por una potencia de
  dos y centrados en su propia media, en los cinco motores y en `perfilar()`.
  Cerca del máximo del doble (±1,8e308), SQL Server, MySQL y MariaDB dejaban el
  desvío en `NA` sin avisar y `perfilar()` publicaba `Inf`; con valores del
  orden de `1e-160` todos perdían cifras (error de hasta 7e-4), con `1e-200`
  `perfilar()` publicaba `0`, y en PostgreSQL 16, por encima de `1e150`, el
  desvío pasaba por un `numeric` de 15 cifras (error de 4e-5). Ahora queda a
  menos de 1e-14 de la cifra exacta, o `no_disponible` con su falla y el aviso.
  Las columnas de magnitud común conservan la consulta y la cifra.
- En MySQL y MariaDB, el desvío de una columna con media entre `1e65` y `1e150`
  perdía cifras (error de hasta 9e-5): el centro se escribía como un decimal de
  más de 65 cifras. Y cuando la suma desbordaba, MySQL publicaba la media en `0`
  y MariaDB en `NA`, sin avisar; ahora la media también se calcula escalada.
- Con `RMySQL`, un perfil ya no suelta decenas de avisos del controlador
  (`Decimal MySQL column imported as numeric`): lupa pide en el SQL el tipo que
  lee, con las mismas cifras. Un `error while fetching rows` del controlador
  cuenta como falla de la consulta y no como un resultado vacío.
- En MySQL y MariaDB, un `DECIMAL` con valores de 2^53 o más ya no publica
  mínimo, máximo, media, mediana ni desvío con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): quedan `no_disponible`
  con su motivo, como en PostgreSQL y SQL Server. Con `metricas` explícitas, ese
  motivo decía «no se pidió».
- La moda de una columna binaria se publica como su largo y su MD5
  (`400:63f9…`), la misma representación que usa la muestra, en minúsculas en
  todos los motores; la corroboración ya no la marca `no_corroborada`, y en el
  perfil de la muestra la columna figura como `blob`. Con
  `universo = "muestra_motor"`, una columna `char(n)` de SQL Server ya no
  publica la moda con los espacios de relleno.
- Una muestra que el tope de bytes deja en cero filas publica
  `filas_obtenidas = 0` y el aviso dice cero filas, no `NA`. Con `TABLESAMPLE`
  o `RANDOM()`, `meta$orden_muestra` declara que `orden_muestra` no gobierna la
  selección.
- Una moda de frecuencia 1 sobre dos o más valores ya no se publica cuando
  `n_distintos` no se midió —no se pidió, o su consulta se cortó por tiempo—:
  ningún valor se repite.
- Con `universo = "muestra_motor"`, un `max_consultas` chico ya no hace
  abortar el perfil. Con valores de 1 a 12, según el motor, la corrida terminaba
  en error: el presupuesto se agotaba al leer el esquema, o al contar la tabla
  que `TABLESAMPLE` necesita para escribir su porcentaje. Ahora degrada y lo
  declara en `meta$consultas$agotado`, con las métricas que no entraron como no
  calculadas, la cobertura y el aviso.
- Con un tope de bytes bajo, la muestra ya no queda vacía por una cota que no
  es la real: para `text` o `bytea` sin largo declarado se estimaban unos 125 KB
  por fila, y con `max_bytes_muestra = 100000` sobre filas de 3,7 KB llegaba
  una. Ahora se lee una fila, se mide lo que ocupa y se sigue en bloques sin
  pasar el tope —en ese caso, unas 25 filas—, y `meta$muestreo$filas_por_bytes_fuente`
  lo declara.
- Con `universo = "muestra_motor"`, el desvío se pide centrado en la media de
  las mismas filas y en la misma consulta: una relación derivada sobre la
  muestra calcula esa media con una ventana (`AVG(x) OVER ()`). Sin centrar, en
  SQL Server unos montos de un millón daban el desvío con un error de 2,3e-3, y
  en PostgreSQL 16 valores casi constantes, de 5e-3; en MySQL y MariaDB pasaba
  también con `metricas = "desvio"` en `tabla_completa`. Esa misma evaluación de
  la muestra da las filas obtenidas, decide el reintento y entra en los
  agregados: con `TABLESAMPLE SYSTEM`, algunas corridas dejaban la media y el
  desvío no disponibles mientras declaraban 300 filas obtenidas.
- El desvío extremo se escala aunque no se pidan los básicos ni se publiquen
  los valores (`incluir_valores = FALSE`), y también cuando el rango es tan
  chico que sus cuadrados caerían en subnormales. Antes MariaDB llegaba a
  publicar `1.7976931348623157e+308` como desvío y MySQL una media de 0. Con un
  solo valor válido, el desvío es `no_aplica` y no dispara un repliegue.
- En SQL Server, la media de una columna cuya suma desborda, y en los cinco
  motores la mediana de dos centrales enormes, se calculan escaladas en lugar
  de quedar no disponibles; MySQL publicaba esa mediana en 0.
- Con `bloque_filas`, el desvío y la media de valores extremos ya no salen
  `Inf`, `0` ni `NA` como calculados: el estado se reescala por una potencia de dos
  cuando llega un bloque que la escala vigente no cubre. La moda de una columna
  binaria también sale en esa vía, como largo y MD5, y una columna `image` de
  SQL Server publica su moda.
- Sin el paquete opcional `stringdist`, el diagnóstico de proximidad del
  vocabulario deja una sola fila de cobertura por columna, la que dice que falta
  el paquete. Antes agregaba otra que hablaba de un presupuesto agotado («se
  comparó entre sí a las 0 primeras») y recomendaba subir
  `max_trabajo_vocabulario`, que no arreglaba nada.
- Con `bloque_filas` y un controlador sin lectura incremental demostrada
  —DuckDB, o uno que no está en la matriz—, el perfil sigue sin caer en
  `dbGetQuery()`, pero ahora lo avisa (`lupa_aviso_muestra_motor`): antes todas
  las métricas quedaban `no_disponible` y el motivo solo estaba en la cobertura.
- Con `universo = "muestra_motor"`, `estado` y `error_esperado` se deciden por
  métrica y con la misma regla en las dos vías, como dice la ayuda. Antes, con
  `bloque_muestra = "solo_agregados"`, los distintos y la mediana de la muestra
  salían `error_esperado = "no_aplica"`, lo que se publica cuando no hubo
  muestreo; y con `"con_muestra"` todas las métricas salían `no_estimable` —también
  la media y los conteos— y los estadísticos `observado_muestra` en vez de
  `estimado`. Los valores no cambian.
- Los mensajes, avisos, evidencias e informes concuerdan en número («1 fila»,
  «1 valor») en todo el paquete, llevan sus tildes y dan las indicaciones en forma
  impersonal («revisar», «aumentar») en lugar de «usted». Los valores que el
  paquete publica como dato —estados, motivos codificados, unidades, las marcas
  de protección y las clases de consulta del plan— no cambian.
- `n_filas()` de una colección devuelve el vector con el nombre de cada tabla.
- El aviso de tabla ancha proyecta con 5.000 celdas por segundo, por debajo de
  todas las mediciones de `benchmark/medir_referencias.R`; los 10.000 de antes
  eran más rápidos que lo medido (unas 5.200 con 50 columnas), así que la espera
  anunciada se quedaba corta. `meta$costo_tabla_ancha$fuente` nombra los 2
  guiones que la sostienen: ese y `benchmark/medir_topes_valores_ancho.R`, con
  columnas iguales de códigos de texto.
- La ayuda de `estrategia` en `detectar_duplicados_aproximados()` dice
  exactamente cuándo `"auto"` pasa a LSH (más de 10.000 filas, `muestra = Inf` y
  `max_pares` de 50.000.000 o más), y el aviso de LSH ya no llama «piso» a una
  referencia de la comparación aislada.
- Con `bloque_filas`, las columnas binarias se leen como largo y MD5, igual
  que en la muestra: con RMySQL el controlador ya no las trunca (un aviso por
  fila, y largos y distintos falsos), y en PostgreSQL, SQL Server y MariaDB los
  distintos y los largos dejan de quedar no disponibles.
- La moda de un entero de 2^53 o más que el controlador entrega como doble —en
  DuckDB `BIGINT`, `UBIGINT`, `HUGEINT` y `UHUGEINT`, y con RMySQL— ya no sale
  con cifras falsas (`1234567890123456768` por `1234567890123456789`): se pide
  como texto exacto, también con `bloque_filas`. En DuckDB, un `UHUGEINT` de esa
  magnitud ya no publica mínimo, máximo, media ni mediana falsos, y la mediana
  de un `HUGEINT` cerca de su tope, cuya consulta de básicos desborda en el
  motor, queda no disponible en vez de publicarse como un doble.
- Con `bloque_filas`, una columna de un solo valor o vacía ya no publica `NA`
  como calculado: el desvío queda `no_aplica` y las métricas de una columna
  vacía, `sin_valores`. Y con RMariaDB un `bigint unsigned` mayor que 2^63, que
  el controlador entrega dado vuelta a negativo, ya no se publica como
  calculado: queda no disponible con su motivo.
- En MariaDB, la mediana de valores extremos ya no sale `100000000` o `0` como
  calculada.
- La moda de un decimal de 2^53 o más ya no sale con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): se le pide al motor como
  texto exacto en PostgreSQL, SQL Server, MySQL, MariaDB y DuckDB. Con
  `incluir_valores = FALSE`, el motivo de precisión de esas columnas ya no se
  reemplaza por uno de privacidad.
- En PostgreSQL, un `NaN` en la columna ya no deja el mínimo, el máximo, la
  media y el desvío sin calcular: se miden sobre los finitos, como en
  `perfilar()`. Con el presupuesto agotado, `n` queda `no_disponible` con su
  motivo y no `NA` como calculado.
- En R anterior a 4.6, `match()` sobre fechas era cientos de veces más lento, y
  una columna `Date` hacía tardar minutos el perfil: 100.000 fechas pasaron de
  171 s a 1,3 s en R 4.5.2. `Date`, `POSIXct`, `difftime`, los factores e
  `integer64` se comparan por una clave sin clase, con el mismo resultado.
- Los valores de texto de más de `max_largo_valor_vocabulario` (10.000
  caracteres o bytes) quedan fuera solo de las operaciones textuales caras
  —normalización, patrones, reconocedores de datos personales, casi
  duplicados—, y la cobertura lo declara con `texto_largo_no_evaluado` y una
  fila por diagnóstico afectado, para que `comparar_perfiles()` diga
  `no_evaluado` y no `resuelto`. Los conteos, la inferencia de tipo, los
  blancos, los centinelas y las dependencias siguen leyendo el valor entero.
  Sobre 700 filas de cuatro columnas de 14 KB, el perfil pasó de 23 s a 5 s.
- Un nombre de columna con coma ya no rompe la traza: los hallazgos de pares de
  columnas llevan las columnas como vector en `trazabilidad$columnas`, y la
  cobertura en una columna `columnas`. Una clave con un nombre así se pide con un
  vector.
- `cobertura_analisis()` acepta el resultado de `perfilar_dbi()`: analiza la
  muestra y dice que la cobertura es de la muestra; con `bloque_muestra =
  "solo_agregados"`, usa el resumen de la tabla.
- Con `universo = "muestra_motor"`, los topes de bytes y de celdas
  (`max_bytes_materializacion`, `max_bytes_muestra`, `max_celdas_muestra`)
  mandan antes de leer. Cada `dbFetch()` se pide con un tamaño máximo de fila
  conocido de antemano: los textos de más de `max_largo_valor_vocabulario` y las
  columnas binarias (`bytea`, `varbinary`, `image`, `blob`) se piden como largo
  y resumen MD5, con los distintos, los duplicados, los faltantes y los largos
  exactos. Si el presupuesto se agota, se conserva lo que entró. Antes, una
  columna binaria o de textos largos podía traer varios GB a R antes de mirar
  el tope.
- Una muestra del motor que vuelve corta se reintenta una vez, con `BERNOULLI`
  en PostgreSQL o una fracción mayor por bloques, sin ordenar la tabla entera;
  el aviso sale solo si la muestra final sigue corta o la recortó un tope.
- Las consultas pedidas que el motor rechaza o cancela quedan en
  `resumen_tabla$meta$consultas$fallas` y en la cobertura como `degradado`, con
  un aviso de clase `lupa_aviso_muestra_motor` que nombra ese camino completo
  (antes decía `meta$consultas$fallas`, que sobre el resultado da `NULL`); las
  consultas internas del paquete (capacidades, catálogo, configuración) no
  avisan. La muestra del motor publica
  también sus tiempos, sus bytes y el tope que mandó.
- Por JDBC, SQL Server se reconoce como motor por los metadatos de la conexión.
- `perfilar_dbi()` avisa (`lupa_aviso_costo_dbi`) antes de recorrer con
  agregados una tabla que el catálogo estima en 100 millones de filas o más, y
  sugiere `universo = "muestra_motor"`.
- La mediana de MariaDB ya no sale de `PERCENTILE_CONT`, que ese motor devuelve
  con 10 decimales y un tope en 1e8: con `perfilar_dbi()` por omisión, una
  columna de montos del orden de 1e12 publicaba 1e8, una de valores del orden de
  1e-12 publicaba 0 y un doble común perdía desde el décimo decimal, todo como
  `calculado`. La prueba de capacidad del percentil exige ahora que vuelvan todas
  las cifras, y MariaDB calcula la mediana por columna (tabla completa) o con la
  CTE de ventanas (muestra), como MySQL. Vale también con
  `estrategia_mediana = "aproximada_motor"` y con RMySQL contra un servidor
  MariaDB.
- Con `universo = "muestra_motor"` y `bloque_muestra = "solo_agregados"`, la
  mediana sale en MySQL, en un `float8` de PostgreSQL y en una columna con
  valores extremos de cualquier motor: la calcula la CTE de ventanas sobre la
  misma muestra, como con `con_muestra`. La consulta de prueba de esa CTE usaba
  una construcción que MySQL rechaza.
- Con `metricas` sin la cuenta de valores válidos, la mediana de la tabla
  completa cuenta esos valores en una consulta propia y sale igual que con todas
  las métricas. Con `dialecto = "portable"`, que no tiene salto de filas, sale
  de la CTE de ventanas si el motor la acepta.
- Ningún registro de `perfilar_dbi()` afirma un valor ausente: un estado
  `calculado`, `estimado` u `observado_muestra` con el valor en `NA` pasa a decir
  por qué falta. Es `sin_valores` si la columna está vacía (en la muestra,
  `no_disponible` con `muestra_inestable:sin_valores_validos`), y `no_aplica`
  para una métrica numérica sobre una columna que no lo es, para el desvío de
  menos de 2 valores y para la moda de una columna sin repetidos (en la muestra,
  `no_disponible`, porque la tabla puede tener moda).
- En SQL Server, la mediana de un `decimal` con 36 cifras de precisión o más
  ya no pierde decimales: `AVG(valor * 1.0)` recortaba la escala a 6, y en un
  `decimal(38,7)` la mediana de 0,0000001 y 0,0000003 salía 0. Con
  compatibilidad menor que 110, sin `PERCENTILE_CONT`, ese era el camino de todo
  `decimal`. Ahora el motor suma las una o dos centrales en el tipo de la
  columna, y R divide esa suma por cuántas son: la mediana es el doble más
  cercano de la exacta. Convertirlas a doble en el servidor la dejaba a un ulp,
  y promediar en R el doble de cada central tampoco es exacto (la de 0,10 y
  0,20 daba 0,15000000000000002). Lo mismo en MySQL y MariaDB, donde
  `AVG(valor * 1.0)` necesita una cifra de escala más que la columna y en la
  escala máxima del motor (30 y 38) se recortaba: la mediana de 1e-30 y 2e-30
  salía 2e-30.
- En PostgreSQL 9.3 con `dialecto = "portable"`, y en la muestra con la
  subconsulta escalar, la mediana de una cantidad par de `float8` coincide con
  `median()`: difería en 1 ulp en 427 de 3.000 pares. Usa el mismo promedio que
  la vía de dos consultas.
- En `perfilar()`, una errata de texto (`Montevideo`/`Montevido`) ya no queda
  callada porque la misma columna tenga una familia de códigos numerados con
  frecuencias desparejas. La familia sigue declarando que el diagnóstico no
  aplica a las formas con números; los grupos sin números se juzgan con el mismo
  límite y contra el mismo vocabulario entero de la columna: salen igual que si
  la familia no se encadenara.
- La búsqueda de casi duplicados del vocabulario ya no depende de que la
  columna tenga, antes de una pareja, dos formas que se normalizan igual (`ALFA`
  y `Alfa`). Con una de esas clases delante, el filtro de marcas de formato leía
  otro par: en una columna de horas publicaba como casi duplicados pares
  `a.m.`/`p.m.` que tiene que descartar y perdía el hallazgo real; la fila de
  cobertura «N grupos de formas cercanas no se formaron…» desaparecía, y el
  diagnóstico de variantes equifrecuentes nombraba formas que no son pareja. Sin
  esas clases, o con la clase después de la pareja, la salida es la misma.
- En `perfilar()`, una pareja cercana cuyas formas normalizadas empatan en
  frecuencia ya no queda sin declarar porque uno de sus lados junte grafías que
  se normalizan igual (`Montevideo` y `MONTEVIDEO`, 25 cada una, contra
  `Montevido`, 50). Se publicaba sólo el grupo de esas grafías, y la pareja no
  aparecía en la fila de cobertura «N grupos de formas cercanas no se
  formaron…» ni, con `variantes_equifrecuentes_vocabulario = TRUE`, como
  hallazgo. Ahora se declara igual que sin esas grafías, y el grupo
  equifrecuente las lista todas con su frecuencia; su asimetría es la de las
  formas normalizadas (`MONTEVIDEO` 10 y `Montevideo` 40 contra `Montevido` 50
  da 1, no 5).
- En `perfilar()`, una pareja que `casi_duplicados_vocabulario` ya publica no se
  cuenta además en la fila de cobertura «N grupos de formas cercanas no se
  formaron…», ni sale otra vez como `variantes_equifrecuentes_vocabulario` con
  el detector encendido. Con los valores por omisión pasaba en el borde:
  `Montevideo` (50) contra `Montevido` (25) tiene asimetría 2, que llega al piso
  de `min_asimetria_vocabulario` y no pasa el tope de
  `max_asimetria_equifrecuente_vocabulario`, y la salida publicaba la pareja y
  decía a la vez que no se había formado. Lo mismo con las variantes de igual
  frecuencia de un grupo publicado (`Zona B` … `Zona O` alrededor de `Zona A`).
  Lo que el piso de asimetría deja afuera se sigue contando. Y la cantidad de
  pares que esa fila declara («Se identificaron M pares cercanos…») es la de los
  grupos que cuenta: sumaba también los de los grupos ya publicados y los de las
  cadenas que el tope de asimetría o el límite del vocabulario dejan afuera.
- En `perfilar()`, cada par cercano del vocabulario se declara una sola vez
  también cuando el grupo de formas cercanas toca, por otra de sus formas, un
  grupo que `casi_duplicados_vocabulario` publica. Con `Montevideo` (100),
  `Montevido` (50) y `Montividoo` (50) se publicaba [Montevideo / Montevido] y
  la fila de cobertura decía a la vez «1 grupo de formas cercanas no se formó…
  Se identificaron 2 pares», contando esa misma pareja; con
  `variantes_equifrecuentes_vocabulario = TRUE` la pareja salía otra vez dentro
  de [Montevideo / Montevido / Montividoo]. Ahora la fila cuenta 1 par y el
  grupo equifrecuente es [Montevido / Montividoo]: la forma que comparten se
  nombra en los dos grupos y cada uno declara sus pares. Un par que quedaba sin
  declarar porque la pareja publicada llevaba la asimetría del grupo por encima
  de `max_asimetria_equifrecuente_vocabulario` ahora se cuenta, y dos grupos que
  sólo se tocaban por una pareja publicada salen separados. Los grupos de
  `casi_duplicados_vocabulario` no cambian.
- En la fila de cobertura de `proximidad_vocabulario`, los pares cercanos que
  forman cadenas cuya asimetría de punta a punta supera
  `max_asimetria_equifrecuente_vocabulario` se declaran aparte: con
  `Montevideo` (25), `Montevido` (20) y `Montividoo` (10), a 1,25 y 2 de a
  pares y a 2,5 de punta a punta, la fila dice «…2 pares cercanos, con
  asimetría <= 2.0 entre ellos, forman cadenas cuya asimetría de punta a punta
  supera 2.0 y no se agrupan». Antes no se declaraban en ningún lado. El tope
  por grupo no cambia.
- La mediana de una columna entera o decimal con valores de 2^53 o más se decide
  por su valor y no por la columna, con la misma regla en los siete motores: con
  una cantidad impar se publica si la central queda por debajo de 2^53 (la de
  `{1, 2, 2^63 - 1}` en `BIGINT` es 2, y quedaba `no_disponible`); con una par,
  si las dos centrales quedan entre -2^52 y 2^52, lo que en la tabla completa
  cuesta una consulta más. En DuckDB, la de un `HUGEINT` cerca de su tope con una
  cantidad par ya no sale del promedio de dos dobles redondeados (2,5 por 2).
  La misma regla siguen `perfilar()` sobre `integer64` o sobre enteros escritos
  como texto, `perfil_muestra` y la muestra del motor, que tienen las centrales
  a la vista: la corroboración ya no acusa una divergencia entre el resumen SQL,
  que publicaba la mediana, y la muestra, que la negaba por la columna.
- En DuckDB, la mediana de un `DECIMAL` ya no se trunca a la escala de la
  columna: la de 1,00 y 1,01 en `DECIMAL(10,2)` salía 1,00.
- En PostgreSQL, la mediana de una cantidad par cuyas centrales distan más de
  2^53 ya no se publica con el redondeo de `PERCENTILE_CONT` (-1 por -0,5).
- Con `bloque_filas` en SQLite, una columna `INTEGER` cuyos primeros bloques
  entran en 32 bits y otros no ya no cuenta mal `n_distintos`, la moda ni la
  mediana (`{1, 2, 3e9, 3e9}` daba 4 distintos, ninguna moda y mediana 0,5).
- En SQL Server con compatibilidad 110 o más, el desvío de un `decimal(38, s)`
  ya no sale mal cuando la media termina en la mitad de la última unidad:
  `{1, 2, 3, 4}` en `decimal(38,0)` daba 1,8257 por 1,2910.
- Con `bloque_filas`, y por la muestra del motor, el desvío de un decimal que el
  doble no representa ya no sale de esos dobles: un `numeric(18,2)` de 5e14 con
  desvío 3 daba 3,1115 por 3,1047, y con la muestra leída en R
  (`bloque_muestra = "con_muestra"`) se publicaba movido en una parte en 300.
  Por bloques, si el redondeo de cada valor puede moverlo más de una parte en
  mil millones, el motor lo calcula restando a cada valor la media en el tipo
  de la columna, en una consulta para todas esas columnas; con la muestra leída
  en R sale del texto de cada valor, que la muestra ya traía. Por la muestra del
  motor con `bloque_muestra = "solo_agregados"`, en SQL Server, MySQL, MariaDB y
  DuckDB, que centran en doble, queda `no_disponible`. Un decimal de escala 0
  con valores por debajo de 2^53 se publica en todas las vías: ahí los dobles
  son los valores, y la escala la dicen los catálogos de PostgreSQL, MySQL,
  MariaDB, DuckDB y SQL Server, que ahora publica la precisión y la escala de un
  decimal (`decimal(18,0)` y no `decimal`).
- Con `bloque_filas` y con la muestra leída en R, la media y la mediana de un
  decimal que el controlador entrega como doble son el doble más cercano de las
  del decimal, y no las de esos dobles: la de 0,0001 y 0,0002 daba
  0,00015000000000000001, a un ulp en 14 de 112 medias y 9 de 112 medianas de
  SQL Server y en 4 y 2 de 46 de PostgreSQL, MySQL, MariaDB y DuckDB, y con
  valores de hasta 1e12 con seis decimales que se cancelan, una media por
  bloques de `numeric(20,6)` en PostgreSQL quedaba a 1.447 ulps. Por bloques el motor suma la columna en su tipo, en una consulta para
  todas las columnas decimales, y devuelve como texto las dos centrales cuando
  caen en dobles distintos (una consulta ordenada por columna; `money` de SQL
  Server siempre, porque odbc no lo lee al doble más cercano). La muestra leída
  en R ya traía cada decimal como texto.
- `plan_perfilado_dbi()` cuenta la consulta de prueba de la CTE de ventanas, y
  su control negativo, que la muestra del motor corre cuando una columna
  decimal, o un `float8` de PostgreSQL, deja la mediana consolidada: dos
  consultas que el plan no anticipaba.
- `plan_perfilado_dbi()` anuncia lo que `perfilar_dbi()` manda al motor en cada
  vía, contado sentencia por sentencia en los siete motores. Por `bloque_filas`
  contaba sólo el recorrido —1 consulta donde se mandaban de 8 a 24— y ahora
  cuenta la preparación, las consultas de prueba de la fuente, la segunda pasada
  del localizador, la moda exacta, la media de los decimales y la muestra, con la
  mediana por centrales y el desvío centrado de un decimal en `total_maximo`,
  porque los piden los datos. Por la muestra del motor sin bloque de filas
  contaba consultas del desvío que no se emiten, y no la lectura de comprobación
  de la muestra; con el bloque de filas contaba los agregados SQL, que el spool
  no corre. En PostgreSQL y SQL Server la estimación de filas del catálogo del
  aviso de costo, y en PostgreSQL `SHOW extra_float_digits`, se mandaban sin
  contarse. `max_consultas` y `meta$consultas$emitidas` cuentan lo mismo que el
  plan, y por bloques lo que el total anuncia tiene su lugar reservado: con
  `max_consultas = attr(plan, "total")` la media de un decimal y
  `perfil_muestra` ya no quedan afuera; con `total_maximo` no queda afuera nada.
  Con un `max_consultas` más chico que el plan pueden entrar menos métricas que
  antes: esas consultas ya llegaban al motor, sin contarse.
- `planificar_limpieza()` sobre un perfil guardado por una versión anterior, con
  un hallazgo por par de columnas iguales, ya no compara cada hallazgo con todas
  las columnas: con 1.000 columnas iguales (499.500 hallazgos), de 87 s a 4 s,
  con el mismo plan.
- El desvío de un decimal que la muestra lee como doble (5e14 con centavos) ya
  no se publica en `perfil_muestra`, en ninguna vía, y la corroboración no lo
  cuenta como divergencia: queda en `perfil_muestra$meta$desvio_decimal_doble`
  y en las no comparadas con la causa `decimal_doble`. Pasaba por la vía por
  omisión en los seis motores con decimales, y `perfilar_coleccion()` subía la
  tabla a `alcance = "divergencias"`. Un desvío que el motor calcula mal sigue
  siendo divergencia.
- Con `universo = "muestra_motor"`, una muestra pedida mayor que la tabla ya no
  se juzga corta: lo efectivo es lo pedido acotado por el total que trae la
  primera consulta. Con `bloque_muestra = "solo_agregados"` se reintentaba y
  se avisaba una degradación falsa en SQLite, MySQL, MariaDB y PostgreSQL 9.3;
  en DuckDB el reintento escribía `TABLESAMPLE BERNOULLI (100)`, que el motor
  rechaza (ahí un número sin unidad es una cantidad de filas), y todas las
  métricas de la tabla quedaban `no_disponible`. `BERNOULLI` se escribe en
  DuckDB con `PERCENT`.
- Con `incluir_valores = FALSE`, el mensaje de un error del motor (una
  conversión fallida, un desborde) ya no cita la celda en `motivo`,
  `meta$consultas$fallas`, la muestra ni los bloques: pasa por el mismo piso
  que con la protección pedida. PostgreSQL, SQL Server y DuckDB lo publicaban
  entero.
- La media de `perfilar()`, de `perfil_muestra`, de `bloque_filas` y de la
  muestra del motor es el doble más cercano de la media exacta. Se acumulaba
  en doble, centrada en la media del primer bloque, y se equivocaba sin aviso
  cuando los valores se cancelan: `c(-2^51, 1, 2, 2^51)` daba 0,6875 por 0,75;
  quince mil valores de 2^51, quince mil de -2^51 y `1:3`, una media de signo
  cambiado; y en `perfil_muestra` la corroboración lo contaba como divergencia
  contra el `AVG` del motor. También sin cancelación: `rnorm(2e6)` quedaba a 19
  ulps, y cuarenta enteros chicos por `bloque_filas = 7`, a ocho. Cada bloque se
  suma ahora sin redondeo, sin depender del `long double` de `sum()`, con el
  mismo costo.
- El desvío de `perfilar()`, de `perfil_muestra`, de `bloque_filas` y de la
  muestra del motor es el doble más cercano del desvío exacto (con `n - 1`). Se
  acumulaba con Welford en doble y quedaba a uno, dos o tres ulps:
  `((1:25) %% 7 - 3) * 0.3` a uno en memoria (como `sd()`), y por
  `bloque_filas = 7` a dos. Ahora la suma de los cuadrados es exacta o tiene una
  cota demostrada, y el redondeo se decide comparando contra los puntos medios
  con aritmética exacta, sin depender del `long double` de `sum()`; el resumen
  de una columna común de 2 millones de filas no tarda más. La media también es
  la más cercana en una columna con valores de más de 2^900 junto a otros
  diminutos (con 2^1000, -2^1000 y valores de 2^-1000 salía 0). Y la rama de
  texto de un entero que pasa de 2^53 saca los centinelas declarados como
  `integer64` —no entraban en los extremos exactos, los conteos ni la mediana—
  y lee `"-0"` como el cero.
- La mediana de un decimal en SQL Server (compatibilidad 110 o más),
  PostgreSQL 9.4 o más y DuckDB es el doble más cercano de la exacta, por la
  tabla completa y por la muestra del motor: el motor suma sus una o dos
  centrales, como ya en MySQL, MariaDB y SQL Server con compatibilidad 100, en
  lugar de `PERCENTILE_CONT`, que interpola en doble y la dejaba a un ulp. En
  DuckDB la suma se lee por su texto (el controlador convierte un decimal de
  más de 18 cifras a un doble que no es el más cercano), y en SQL Server el
  `money` se suma como `decimal(38,4)`, que no desborda. Cuesta una consulta por
  columna decimal, que `plan_perfilado_dbi()` cuenta.
- En SQL Server, la muestra del motor lee el texto de un decimal de más de 30
  cifras (`CAST(... AS VARCHAR)` es `VARCHAR(30)`, y la lectura de toda la
  muestra abortaba) y `money` con sus cuatro decimales: 0,0001 se leía 0, y el
  mínimo, la media y el desvío de una columna de 0,0001 a 0,0004 salían 0.
- En DuckDB, cuando la media de un `HUGEINT` o `UHUGEINT` cerca de su tope
  desborda, la consulta de mínimo, máximo y conteos se repite sin el promedio:
  esas métricas se juzgan como en cualquier columna que pasa de 2^53, y la media
  queda `no_disponible` con su propio motivo. Antes caían todas con el error del
  motor por motivo, que citaba el valor de la celda aun con
  `incluir_valores = FALSE`.
- En SQLite, el desvío de la muestra del motor ya no pierde cifras con valores
  grandes (`{2^53 - 2, 2^53 - 1}` daba 1 por 0,7071).
- `requisitos_motor()` declara PostgreSQL «9.3 y 16», las dos versiones que se
  miden.

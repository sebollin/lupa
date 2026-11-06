# Changelog

## lupa 0.1.0

Primera versión publicada.

### Perfilado

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  describe cada columna de un `data.frame`: el tipo implícito, los
  faltantes reales y los disfrazados (códigos y centinelas), los
  patrones de formato, los formatos de fecha mixtos, los valores
  extremos, las filas duplicadas, las dependencias funcionales y las
  claves. Lo que no puede medir lo declara, con su motivo, en vez de
  dejarlo en cero.
- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  hace el perfil dentro del motor, por DBI, con una consulta por lote y
  no una por columna, muestras del motor y presupuestos de trabajo
  declarados. Está probado contra PostgreSQL (16 y 9.3), SQL Server
  2022, MySQL 8.4, MariaDB 11.8, SQLite y DuckDB.
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  y
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  perfilan varias tablas y proponen las relaciones entre ellas, y
  [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
  anticipa lo que va a costar una corrida.
- [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  hace el recorrido completo: el perfil, las distribuciones, las
  asociaciones, el análisis temporal, la clasificación de las variables
  y una propuesta de modelo de medición.
- Cada diagnóstico también se puede pedir por separado:
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
  [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md),
  [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md),
  [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
  [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md),
  [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md),
  [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md)
  y
  [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md).

### Medición de la calidad

- Marcos de calidad con dimensiones y factores que declara el usuario
  ([`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)),
  y los de ISO/IEC 25012, CEPAL y AGESIC listos para usar.
- Métricas reutilizables con su granularidad explícita
  ([`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  [`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)),
  modelos de medición
  ([`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  [`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  [`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)),
  mediciones
  ([`medir()`](https://sebollin.github.io/lupa/reference/medir.md)),
  agregación tipada
  ([`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md))
  y perfiles de evaluación
  ([`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md),
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)).
- Validadores de formato: la cédula y el RUT de Uruguay, el correo, la
  URL, ISO 3166, ISO 4217, Luhn y mod 97.
- Históricos versionados
  ([`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md),
  [`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)),
  la comparación de dos perfiles
  ([`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md))
  y la detección de deriva
  ([`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)).

### Limpieza

- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  propone un plan editable de acciones, cada una con su justificación y
  la severidad de lo que la motivó.
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  lo ejecuta sobre una copia y deja el registro de lo que cambió, y
  [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md)
  muestra ejemplos para decidir. `lupa` nunca modifica los datos que
  recibe.

### Informes

- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  escribe un informe HTML autocontenido, sin depender de software
  externo para generarlo.

### Documentación

- El paquete trae las guías 1 a 6, el camino básico: de la primera tabla
  al plan de limpieza. Las guías 7 a 12 (métricas, marcos, histórico,
  bases de datos, colecciones y escala) y los 4 artículos de referencia
  están en el sitio, <https://sebollin.github.io/lupa/>.
- Los guiones de `benchmark/` que publican cifras dejan su CSV en
  `benchmark/datos/` con fecha, entorno y huella del código; el de Raha
  agrega el SHA-256 de cada insumo, y el del tope de largo mide
  Jaro-Winkler con el mismo `p` que el paquete (el margen publicado para
  `max_largo_valor = 10000` era mayor que el real).
- El README muestra una captura del informe HTML de
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
  rehecha con `scripts/capturar-informe-html.R`. «El vacío por diseño» y
  «Marcos y modelos de calidad» empiezan por el ejemplo y dejan la
  explicación para después.
- La guía «Escala y duplicados aproximados» termina en los lotes sin
  pérdida y resume cada tope en una línea. El detalle de los topes y del
  aviso de tabla ancha pasa a «Motores y escala», y el de los hilos a
  «Evidencia y rendimiento».
- Las 6 guías que trae el paquete y los 10 artículos del sitio tienen
  frases más cortas, cifras en dígitos y listas donde había
  enumeraciones, con la misma información y el mismo código. Los
  artículos van en impersonal. `CONTRIBUTING.md` reúne las reglas de
  redacción de la documentación.
- Las cifras de los artículos que el paquete define (umbrales, motores
  de la matriz, dependencias, métricas, topes) se calculan al construir
  el sitio, no se escriben a mano. Al atarlas salieron 2 frases falsas
  en «Alcance y cobertura»: sin `bit64` el perfil de una columna
  `integer64` se detiene con un error, no deja 5 medidas en `NA`; y un
  documento sin su dígito verificador necesita 7 cifras, no 6. La tabla
  de motores del README nombra también PostgreSQL 9.3.
- El logo y los íconos del sitio usan el hexágono regular: lados iguales
  y bordes a 30°. El anterior era más alto que ancho de la cuenta.

### Rendimiento

- La tabla de formatos de fecha se arma una vez, al instalar el paquete,
  y no en cada columna; el enmascarado de datos personales pasa a
  pedirse (ver abajo); y en los caminos que más se repiten se evitan las
  llamadas a expresiones regulares PCRE, que en Windows cuestan unos 47
  µs cada una aunque el vector sea corto.
- Las tablas chicas se arman sin el costo por columna de
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html), la detección
  de fechas no prueba los formatos que no pueden casar con ningún valor,
  y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  clasifica una sola vez los datos personales de la tabla que mide.
  Sobre la tabla de ejemplo de 13 filas,
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  tarda 0,11 s y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  0,22 s en una sesión ya iniciada, y 0,19 s y 0,34 s en la primera
  llamada de una sesión nueva (0,28 s y 0,43 s para
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  con la protección pedida). Las cifras son de
  `benchmark/medir_costo_fijo.R` y quedan en
  `benchmark/datos/medir-costo-fijo.csv`.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre tablas anchas con muchas columnas iguales o de códigos de texto
  es varias veces más rápido, con la misma salida. Los hallazgos
  `columnas_duplicadas` se arman, trazan y revisan de una vez y no par
  por par, y la búsqueda de casi duplicados del vocabulario no calcula
  la mitad de la matriz de distancias que no lee. Con 500 filas y 1.000
  columnas iguales, de 379 a 387 s pasa a 37 a 38 s (los 2 CSV fechados
  de `benchmark/medir_topes_valores_ancho.R` en `benchmark/datos/`).

### Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64
  4.6.0 o posterior: con versiones anteriores,
  [`match()`](https://rdrr.io/r/base/match.html) de R iguala entre sí
  todos los valores negativos, y `lupa` se niega con la condición
  `lupa_error_integer64` en lugar de publicar distintos, modas y
  medianas equivocados.

### Datos personales

- La clasificación de posibles datos personales (documentos, nombres,
  fechas de nacimiento) corre siempre, como diagnóstico. Su reemplazo en
  lo que el paquete publica es un complemento que se pide con
  `proteger_datos_personales = TRUE`: publicar o no los valores de una
  base es decisión de quien la usa.

### Correcciones

- La identidad de filas ya no desborda en R anterior a 4.6. Con dos o
  más columnas de decenas de miles de valores distintos,
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  con `conservar_primera_duplicada` podía borrar filas legítimas (23.673
  de 60.100 en una tabla con 200 repetidas), y `EntidadDuplicada`,
  [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md)
  y las acciones de duplicados contaban grupos equivocados. Una matriz o
  una tabla anidada como columna se aplana para esa identidad, como en
  el perfil.
- [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) ya no
  se cae con una columna lista —lo que un motor devuelve para una
  columna binaria— cuando una métrica no puede producir medidas: la
  métrica queda `no_medible`. Una tabla anidada, una matriz de listas o
  un entorno en una celda tampoco la hacen caer, y
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  ya no publica más filas completas que filas con una matriz de listas.
- `NoNulo` compara `valores_nulos` con la misma normalización que
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  sin los blancos de los bordes y sin distinguir mayúsculas. Una celda
  de solo blancos o un tabulador cuentan como nulos para los dos.
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) de
  `instanciaEntidad` a `entidad` conserva la instancia en el nombre
  (`agregada:ratio:ClaveDuplicada@t.a.b`): dos claves compuestas de la
  misma tabla ya no se funden en una sola fila con un resultado
  mezclado. De `instanciaAtributo` a `atributo`, dos instancias de una
  métrica sobre la misma columna llevan la instancia en el nombre solo
  en ese caso. Más arriba, el nombre conserva la instancia que traen
  todas las partes, y
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) se
  niega a promediar dos instancias de una misma tabla o columna, que la
  harían pesar doble, y dice cómo seguir.
- [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) arma
  su tabla de medidas de una sola vez, sin una tabla intermedia por
  métrica: sobre una tabla de 80.764 filas y 153 columnas de texto con
  100 métricas, bajó de 133 s a 56 s y su pico de memoria de 4,9 GB a
  3,5 GB, que es del orden de la tabla de medidas que devuelve (8
  millones de filas).
- En
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  con `universo = "tabla_completa"`, el desvío de las columnas de SQL
  Server y de `double precision` de PostgreSQL se calcula en lotes,
  centrado en la media que ya trajeron los básicos, y no en una consulta
  por columna. Sobre una tabla de un millón de filas y 27 columnas
  numéricas, el desvío pasó de 27 consultas a 2 y de 1,42 millones de
  lecturas lógicas a 105 mil en SQL Server, con las mismas cifras. En un
  motor que lee del disco, cada consulta de desvío recorría la tabla
  entera.
- El desvío de una columna cuyo mayor valor absoluto queda fuera de
  `[1e-150, 1e150]` se calcula sobre sus valores divididos por una
  potencia de dos y centrados en su propia media, en los cinco motores y
  en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Cerca del máximo del doble (±1,8e308), SQL Server, MySQL y MariaDB
  dejaban el desvío en `NA` sin avisar y
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  publicaba `Inf`; con valores del orden de `1e-160` todos perdían
  cifras (error de hasta 7e-4), con `1e-200`
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  publicaba `0`, y en PostgreSQL 16, por encima de `1e150`, el desvío
  pasaba por un `numeric` de 15 cifras (error de 4e-5). Ahora queda a
  menos de 1e-14 de la cifra exacta, o `no_disponible` con su falla y el
  aviso. Las columnas de magnitud común conservan la consulta y la
  cifra.
- En MySQL y MariaDB, el desvío de una columna con media entre `1e65` y
  `1e150` perdía cifras (error de hasta 9e-5): el centro se escribía
  como un decimal de más de 65 cifras. Y cuando la suma desbordaba,
  MySQL publicaba la media en `0` y MariaDB en `NA`, sin avisar; ahora
  la media también se calcula escalada.
- Con `RMySQL`, un perfil ya no suelta decenas de avisos del controlador
  (`Decimal MySQL column imported as numeric`): lupa pide en el SQL el
  tipo que lee, con las mismas cifras. Un `error while fetching rows`
  del controlador cuenta como falla de la consulta y no como un
  resultado vacío.
- En MySQL y MariaDB, un `DECIMAL` con valores de 2^53 o más ya no
  publica mínimo, máximo, media, mediana ni desvío con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): quedan
  `no_disponible` con su motivo, como en PostgreSQL y SQL Server. Con
  `metricas` explícitas, ese motivo decía «no se pidió».
- La moda de una columna binaria se publica como su largo y su MD5
  (`400:63f9…`), la misma representación que usa la muestra, en
  minúsculas en todos los motores; la corroboración ya no la marca
  `no_corroborada`, y en el perfil de la muestra la columna figura como
  `blob`. Con `universo = "muestra_motor"`, una columna `char(n)` de SQL
  Server ya no publica la moda con los espacios de relleno.
- Una muestra que el tope de bytes deja en cero filas publica
  `filas_obtenidas = 0` y el aviso dice cero filas, no `NA`. Con
  `TABLESAMPLE` o `RANDOM()`, `meta$orden_muestra` declara que
  `orden_muestra` no gobierna la selección.
- Una moda de frecuencia 1 sobre dos o más valores ya no se publica
  cuando `n_distintos` no se midió —no se pidió, o su consulta se cortó
  por tiempo—: ningún valor se repite.
- Con `universo = "muestra_motor"`, un `max_consultas` chico ya no hace
  abortar el perfil. Con valores de 1 a 12, según el motor, la corrida
  terminaba en error: el presupuesto se agotaba al leer el esquema, o al
  contar la tabla que `TABLESAMPLE` necesita para escribir su
  porcentaje. Ahora degrada y lo declara en `meta$consultas$agotado`,
  con las métricas que no entraron como no calculadas, la cobertura y el
  aviso.
- Con un tope de bytes bajo, la muestra ya no queda vacía por una cota
  que no es la real: para `text` o `bytea` sin largo declarado se
  estimaban unos 125 KB por fila, y con `max_bytes_muestra = 100000`
  sobre filas de 3,7 KB llegaba una. Ahora se lee una fila, se mide lo
  que ocupa y se sigue en bloques sin pasar el tope —en ese caso, unas
  25 filas—, y `meta$muestreo$filas_por_bytes_fuente` lo declara.
- Con `universo = "muestra_motor"`, el desvío se pide centrado en la
  media de las mismas filas y en la misma consulta: una relación
  derivada sobre la muestra calcula esa media con una ventana
  (`AVG(x) OVER ()`). Sin centrar, en SQL Server unos montos de un
  millón daban el desvío con un error de 2,3e-3, y en PostgreSQL 16
  valores casi constantes, de 5e-3; en MySQL y MariaDB pasaba también
  con `metricas = "desvio"` en `tabla_completa`. Esa misma evaluación de
  la muestra da las filas obtenidas, decide el reintento y entra en los
  agregados: con `TABLESAMPLE SYSTEM`, algunas corridas dejaban la media
  y el desvío no disponibles mientras declaraban 300 filas obtenidas.
- El desvío extremo se escala aunque no se pidan los básicos ni se
  publiquen los valores (`incluir_valores = FALSE`), y también cuando el
  rango es tan chico que sus cuadrados caerían en subnormales. Antes
  MariaDB llegaba a publicar `1.7976931348623157e+308` como desvío y
  MySQL una media de 0. Con un solo valor válido, el desvío es
  `no_aplica` y no dispara un repliegue.
- En SQL Server, la media de una columna cuya suma desborda, y en los
  cinco motores la mediana de dos centrales enormes, se calculan
  escaladas en lugar de quedar no disponibles; MySQL publicaba esa
  mediana en 0.
- Con `bloque_filas`, el desvío y la media de valores extremos ya no
  salen `Inf`, `0` ni `NA` como calculados: el estado se reescala por
  una potencia de dos cuando llega un bloque que la escala vigente no
  cubre. La moda de una columna binaria también sale en esa vía, como
  largo y MD5, y una columna `image` de SQL Server publica su moda.
- Sin el paquete opcional `stringdist`, el diagnóstico de proximidad del
  vocabulario deja una sola fila de cobertura por columna, la que dice
  que falta el paquete. Antes agregaba otra que hablaba de un
  presupuesto agotado («se comparó entre sí a las 0 primeras») y
  recomendaba subir `max_trabajo_vocabulario`, que no arreglaba nada.
- Con `bloque_filas` y un controlador sin lectura incremental demostrada
  —DuckDB, o uno que no está en la matriz—, el perfil sigue sin caer en
  `dbGetQuery()`, pero ahora lo avisa (`lupa_aviso_muestra_motor`):
  antes todas las métricas quedaban `no_disponible` y el motivo solo
  estaba en la cobertura.
- Con `universo = "muestra_motor"`, `estado` y `error_esperado` se
  deciden por métrica y con la misma regla en las dos vías, como dice la
  ayuda. Antes, con `bloque_muestra = "solo_agregados"`, los distintos y
  la mediana de la muestra salían `error_esperado = "no_aplica"`, lo que
  se publica cuando no hubo muestreo; y con `"con_muestra"` todas las
  métricas salían `no_estimable` —también la media y los conteos— y los
  estadísticos `observado_muestra` en vez de `estimado`. Los valores no
  cambian.
- Los mensajes, avisos, evidencias e informes concuerdan en número («1
  fila», «1 valor») en todo el paquete, llevan sus tildes y dan las
  indicaciones en forma impersonal («revisar», «aumentar») en lugar de
  «usted». Los valores que el paquete publica como dato —estados,
  motivos codificados, unidades, las marcas de protección y las clases
  de consulta del plan— no cambian.
- [`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  de una colección devuelve el vector con el nombre de cada tabla.
- El aviso de tabla ancha proyecta con 5.000 celdas por segundo, por
  debajo de todas las mediciones de `benchmark/medir_referencias.R`; los
  10.000 de antes eran más rápidos que lo medido (unas 5.200 con 50
  columnas), así que la espera anunciada se quedaba corta.
  `meta$costo_tabla_ancha$fuente` nombra los 2 guiones que la sostienen:
  ese y `benchmark/medir_topes_valores_ancho.R`, con columnas iguales de
  códigos de texto.
- La ayuda de `estrategia` en
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
  dice exactamente cuándo `"auto"` pasa a LSH (más de 10.000 filas,
  `muestra = Inf` y `max_pares` de 50.000.000 o más), y el aviso de LSH
  ya no llama «piso» a una referencia de la comparación aislada.
- Con `bloque_filas`, las columnas binarias se leen como largo y MD5,
  igual que en la muestra: con RMySQL el controlador ya no las trunca
  (un aviso por fila, y largos y distintos falsos), y en PostgreSQL, SQL
  Server y MariaDB los distintos y los largos dejan de quedar no
  disponibles.
- La moda de un entero de 2^53 o más que el controlador entrega como
  doble —en DuckDB `BIGINT`, `UBIGINT`, `HUGEINT` y `UHUGEINT`, y con
  RMySQL— ya no sale con cifras falsas (`1234567890123456768` por
  `1234567890123456789`): se pide como texto exacto, también con
  `bloque_filas`. En DuckDB, un `UHUGEINT` de esa magnitud ya no publica
  mínimo, máximo, media ni mediana falsos, y la mediana de un `HUGEINT`
  cerca de su tope, cuya consulta de básicos desborda en el motor, queda
  no disponible en vez de publicarse como un doble.
- Con `bloque_filas`, una columna de un solo valor o vacía ya no publica
  `NA` como calculado: el desvío queda `no_aplica` y las métricas de una
  columna vacía, `sin_valores`. Y con RMariaDB un `bigint unsigned`
  mayor que 2^63, que el controlador entrega dado vuelta a negativo, ya
  no se publica como calculado: queda no disponible con su motivo.
- En MariaDB, la mediana de valores extremos ya no sale `100000000` o
  `0` como calculada.
- La moda de un decimal de 2^53 o más ya no sale con cifras falsas
  (`12345678901234567168` por `12345678901234567890`): se le pide al
  motor como texto exacto en PostgreSQL, SQL Server, MySQL, MariaDB y
  DuckDB. Con `incluir_valores = FALSE`, el motivo de precisión de esas
  columnas ya no se reemplaza por uno de privacidad.
- En PostgreSQL, un `NaN` en la columna ya no deja el mínimo, el máximo,
  la media y el desvío sin calcular: se miden sobre los finitos, como en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Con el presupuesto agotado, `n` queda `no_disponible` con su motivo y
  no `NA` como calculado.
- En R anterior a 4.6, [`match()`](https://rdrr.io/r/base/match.html)
  sobre fechas era cientos de veces más lento, y una columna `Date`
  hacía tardar minutos el perfil: 100.000 fechas pasaron de 171 s a 1,3
  s en R 4.5.2. `Date`, `POSIXct`, `difftime`, los factores e
  `integer64` se comparan por una clave sin clase, con el mismo
  resultado.
- Los valores de texto de más de `max_largo_valor_vocabulario` (10.000
  caracteres o bytes) quedan fuera solo de las operaciones textuales
  caras —normalización, patrones, reconocedores de datos personales,
  casi duplicados—, y la cobertura lo declara con
  `texto_largo_no_evaluado` y una fila por diagnóstico afectado, para
  que
  [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
  diga `no_evaluado` y no `resuelto`. Los conteos, la inferencia de
  tipo, los blancos, los centinelas y las dependencias siguen leyendo el
  valor entero. Sobre 700 filas de cuatro columnas de 14 KB, el perfil
  pasó de 23 s a 5 s.
- Un nombre de columna con coma ya no rompe la traza: los hallazgos de
  pares de columnas llevan las columnas como vector en
  `trazabilidad$columnas`, y la cobertura en una columna `columnas`. Una
  clave con un nombre así se pide con un vector.
- [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  acepta el resultado de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):
  analiza la muestra y dice que la cobertura es de la muestra; con
  `bloque_muestra = "solo_agregados"`, usa el resumen de la tabla.
- Con `universo = "muestra_motor"`, los topes de bytes y de celdas
  (`max_bytes_materializacion`, `max_bytes_muestra`,
  `max_celdas_muestra`) mandan antes de leer. Cada `dbFetch()` se pide
  con un tamaño máximo de fila conocido de antemano: los textos de más
  de `max_largo_valor_vocabulario` y las columnas binarias (`bytea`,
  `varbinary`, `image`, `blob`) se piden como largo y resumen MD5, con
  los distintos, los duplicados, los faltantes y los largos exactos. Si
  el presupuesto se agota, se conserva lo que entró. Antes, una columna
  binaria o de textos largos podía traer varios GB a R antes de mirar el
  tope.
- Una muestra del motor que vuelve corta se reintenta una vez, con
  `BERNOULLI` en PostgreSQL o una fracción mayor por bloques, sin
  ordenar la tabla entera; el aviso sale solo si la muestra final sigue
  corta o la recortó un tope.
- Las consultas pedidas que el motor rechaza o cancela quedan en
  `resumen_tabla$meta$consultas$fallas` y en la cobertura como
  `degradado`, con un aviso de clase `lupa_aviso_muestra_motor` que
  nombra ese camino completo (antes decía `meta$consultas$fallas`, que
  sobre el resultado da `NULL`); las consultas internas del paquete
  (capacidades, catálogo, configuración) no avisan. La muestra del motor
  publica también sus tiempos, sus bytes y el tope que mandó.
- Por JDBC, SQL Server se reconoce como motor por los metadatos de la
  conexión.
- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  avisa (`lupa_aviso_costo_dbi`) antes de recorrer con agregados una
  tabla que el catálogo estima en 100 millones de filas o más, y sugiere
  `universo = "muestra_motor"`.
- La mediana de MariaDB ya no sale de `PERCENTILE_CONT`, que ese motor
  devuelve con 10 decimales y un tope en 1e8: con
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  por omisión, una columna de montos del orden de 1e12 publicaba 1e8,
  una de valores del orden de 1e-12 publicaba 0 y un doble común perdía
  desde el décimo decimal, todo como `calculado`. La prueba de capacidad
  del percentil exige ahora que vuelvan todas las cifras, y MariaDB
  calcula la mediana por columna (tabla completa) o con la CTE de
  ventanas (muestra), como MySQL. Vale también con
  `estrategia_mediana = "aproximada_motor"` y con RMySQL contra un
  servidor MariaDB.
- Con `universo = "muestra_motor"` y
  `bloque_muestra = "solo_agregados"`, la mediana sale en MySQL, en un
  `float8` de PostgreSQL y en una columna con valores extremos de
  cualquier motor: la calcula la CTE de ventanas sobre la misma muestra,
  como con `con_muestra`. La consulta de prueba de esa CTE usaba una
  construcción que MySQL rechaza.
- Con `metricas` sin la cuenta de valores válidos, la mediana de la
  tabla completa cuenta esos valores en una consulta propia y sale igual
  que con todas las métricas. Con `dialecto = "portable"`, que no tiene
  salto de filas, sale de la CTE de ventanas si el motor la acepta.
- Ningún registro de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  afirma un valor ausente: un estado `calculado`, `estimado` u
  `observado_muestra` con el valor en `NA` pasa a decir por qué falta.
  Es `sin_valores` si la columna está vacía (en la muestra,
  `no_disponible` con `muestra_inestable:sin_valores_validos`), y
  `no_aplica` para una métrica numérica sobre una columna que no lo es,
  para el desvío de menos de 2 valores y para la moda de una columna sin
  repetidos (en la muestra, `no_disponible`, porque la tabla puede tener
  moda).

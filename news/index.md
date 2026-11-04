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

### Rendimiento

- La tabla de formatos de fecha se arma una vez, al instalar el paquete,
  y no en cada columna; el enmascarado de datos personales pasa a
  pedirse (ver abajo); y en los caminos que más se repiten se evitan las
  llamadas a expresiones regulares PCRE, que en Windows cuestan unos 47
  µs cada una aunque el vector sea corto. Sobre la tabla de ejemplo de
  13 filas,
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  bajó de 0,63 s a 0,16 s (0,26 s con la protección pedida) y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  de 1,36 s a 0,33 s.

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
- En SQL Server, el desvío de una columna con valores cercanos al máximo
  del doble (±1,8e308) quedaba en `NA`; ahora, si el motor desborda, esa
  columna se recalcula sobre sus valores divididos por una potencia de
  dos. Las demás columnas y las tablas comunes conservan la consulta de
  siempre.
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
  `meta$consultas$fallas` y en la cobertura como `degradado`, con un
  aviso de clase `lupa_aviso_muestra_motor`; las consultas internas del
  paquete (capacidades, catálogo, configuración) no avisan. La muestra
  del motor publica también sus tiempos, sus bytes y el tope que mandó.
- Por JDBC, SQL Server se reconoce como motor por los metadatos de la
  conexión.
- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  avisa (`lupa_aviso_costo_dbi`) antes de recorrer con agregados una
  tabla que el catálogo estima en 100 millones de filas o más, y sugiere
  `universo = "muestra_motor"`.

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

## Rendimiento

- La tabla de formatos de fecha se arma una vez, al instalar el paquete, y no en
  cada columna; el enmascarado de datos personales pasa a pedirse (ver abajo); y
  en los caminos que más se repiten se evitan las llamadas a expresiones regulares
  PCRE, que en Windows cuestan unos 47 µs cada una aunque el vector sea corto.
  Sobre la tabla de ejemplo de 13 filas, `perfilar()` bajó de 0,63 s a 0,16 s
  (0,26 s con la protección pedida) y `analizar()` de 1,36 s a 0,33 s.

## Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64 4.6.0 o
  posterior: con versiones anteriores, `match()` de R iguala entre sí todos los
  valores negativos, y `lupa` se niega con la condición `lupa_error_integer64`
  en lugar de publicar distintos, modas y medianas equivocados.

## Datos personales

- La clasificación de posibles datos personales (documentos, nombres, fechas de
  nacimiento) corre siempre, como diagnóstico. Su reemplazo en lo que el paquete
  publica es un complemento que se pide con `proteger_datos_personales = TRUE`:
  publicar o no los valores de una base es decisión de quien la usa.

## Correcciones

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

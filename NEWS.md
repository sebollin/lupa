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

## Datos personales

- La clasificación de posibles datos personales (documentos, nombres, fechas de
  nacimiento) y su reemplazo en lo que el paquete publica son un complemento: se
  apagan con `proteger_datos_personales = FALSE`.

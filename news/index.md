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

### Requisitos

- R 4.2.0 o posterior. Las columnas `integer64` necesitan además bit64
  4.6.0 o posterior: con versiones anteriores,
  [`match()`](https://rdrr.io/r/base/match.html) de R iguala entre sí
  todos los valores negativos, y `lupa` se niega con la condición
  `lupa_error_integer64` en lugar de publicar distintos, modas y
  medianas equivocados.

### Datos personales

- La clasificación de posibles datos personales (documentos, nombres,
  fechas de nacimiento) y su reemplazo en lo que el paquete publica son
  un complemento: se apagan con `proteger_datos_personales = FALSE`.

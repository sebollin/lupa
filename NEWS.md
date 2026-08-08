# lupa 0.1.0

## Reparación de texto y licencia

- Cierra el motor de reparación de texto: `decode_inconsistent_utf8` trabaja
  por subcadenas con el detector de [ftfy 6.3.1](https://github.com/rspeer/python-ftfy),
  conserva los estados parciales con U+FFFD y agrega tres extensiones
  deliberadas sobre ftfy 6.3.1: la regla de inicio del issue [#222](https://github.com/rspeer/python-ftfy/issues/222),
  la tabla KOI8-R del issue [#231](https://github.com/rspeer/python-ftfy/issues/231)
  y la regla específica para `â` del issue [#233](https://github.com/rspeer/python-ftfy/issues/233).
- Incorpora un motor R puro para detectar y reparar mojibake en varias
  codificaciones, inspirado en el diseño y las tablas de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy) de [Robyn
  Speer](https://github.com/rspeer). Los resultados distinguen reparaciones completas, parciales y casos
  irrecuperables; los estados llegan al hallazgo, al plan y al registro.
- Completa el port de las reglas de detección y de los transcodificadores de
  [ftfy](https://github.com/rspeer/python-ftfy): las transformaciones de bytes
  se encadenan antes de decodificar, las pérdidas quedan como U+FFFD y estado
  `reparado_parcialmente`, y nunca se introduce un control invisible nuevo.
- Completa `restore_byte_a0` de [ftfy 6.3.1](https://github.com/rspeer/python-ftfy):
  conserva la frontera de la palabra `à`, respeta las excepciones portuguesas
  y cubre las seis formas de bytes alterados, sin partir ni pegar palabras.
- La licencia del paquete pasa de `GPL-2 | GPL-3` a `GPL-3`; las partes
  derivadas del diseño de [ftfy](https://github.com/rspeer/python-ftfy) se
  atribuyen en `LICENSE.note` bajo Apache-2.0.
- La estrategia nueva se registra como `reparar_codificacion`; se conserva
  `reparar_codificacion_latin1` únicamente como alias para planes guardados.

## Recursos de comparación

- Fija por omisión en dos los hilos que [`stringdist`](https://cran.r-project.org/package=stringdist) puede usar en las
  comparaciones aproximadas y declara el valor efectivo en `alcance`.
- El aviso interactivo del camino LSH identifica `nucleos` como la perilla que
  puede acortar la etapa de comparación, sin prometer una ganancia fija.
- La viñeta de escala documenta el rendimiento observado entre dos y treinta y
  un hilos y deja explícito que después de dieciséis no hubo una mejora medida.
- Documenta que el piso de tiempo de LSH cubre sólo la comparación de cadenas,
  no la firma, las cubetas ni el troceo; los resultados no dependen de la
  cantidad de hilos.
- Actualiza las mediciones de escala para anotar la configuración de hilos y
  evita presentar tiempos dependientes de la máquina como cifras exactas.

## Marcos declarables y alcance internacional

- Incorpora validadores vectorizados de ISO 3166, ISO 4217, correo, Luhn y
  módulo 97, junto con un pack uruguayo de cédula y RUT. Los packs territoriales
  se pueden extender sin registrar estado global ni modificar el núcleo.
- Separa clasificar de proteger datos personales: las formas numéricas poco
  discriminantes se informan sin suprimir estadísticos, mientras nombres
  semánticos, correos y documentos verificados conservan la protección.
- Documenta los contratos de todos los puntos de extensión y añade
  `propiedades_metrica()` para consultar la configuración admitida sin
  inspeccionar closures.
- Incorpora `marco_iso25012()` como adaptación opcional y explícita de las
  quince características de ISO/IEC 25012:2008.
- Identifica el marco activo en cada fila de `cobertura_analisis()`.
- Permite declarar taxonomías dimensión-factor con `marco_calidad()`, validar
  modelos contra ellas y calcular cobertura con AGESIC sólo como valor de
  fábrica mediante `marco_agesic()`.
- Permite construir familias de madurez con nombres y umbrales propios sin
  cambiar los tres perfiles incluidos.
- Hace que el vector de sentinelas numéricos sea una política completa:
  `numeric()` los desactiva explícitamente.
- Reconoce coma y punto decimal, separadores de miles simétricos, símbolos
  monetarios y prefijos con forma de código ISO 4217.
- Clasifica RUT, DNI y otros documentos con la etiqueta neutral
  `documento_identidad`.
- Permite conectar packs personales territoriales al mismo clasificador de
  `perfilar()`, con tolerancia explícita de errores de digitación; los nombres
  semánticos (`telefono`, `fecha_nacimiento`, entre otros) conservan prioridad
  sobre formas numéricas genéricas.

## Examinar datos

- Protege los estadísticos de orden y cuantiles de columnas personales, marca
  cada supresión en el objeto y conserva alertas de plausibilidad para fechas de
  nacimiento sin publicar sus extremos.
- Añade `datos_operativos`, un segundo conjunto sintético y neutral, reproducible
  desde `data-raw/`, con problemas de calidad sembrados.
- Añade `analizar()` como puerta de entrada al recorrido descriptivo, con
  cobertura conceptual y advertencias de alcance en el propio objeto.
- Incorpora distribuciones de valores acotadas, cuantiles, asociaciones de
  Pearson, V de Cramér y eta cuadrado, además de regularidad, duplicación,
  monotonicidad, cobertura, días de semana y huecos temporales.
- Propone escalas de medición y roles sin confirmar lo que sólo se infiere de
  los valores; conserva niveles declarados, observados y ausentes.
- Perfila tablas administrativas con métricas generales y por columna,
  proporciones en `[0, 1]` y hallazgos filtrables.
- Descubre patrones de formato, tipos implícitos, formatos de fecha mixtos y
  ambiguos, años de dos dígitos, números regionales y problemas de codificación.
- Detecta claves candidatas, relaciones, cobertura referencial, columnas y
  filas duplicadas, y dependencias funcionales exactas o aproximadas.
- Conserva la ambigüedad día/mes con barra, guion y punto; reconoce fracciones
  de segundo y offsets ISO 8601.
- Distingue NaN e infinitos, evita aproximar `integer64` fuera del rango exacto
  de `double` y cuenta valores distintos en columnas de listas y geometrías.
- Clasifica posibles datos personales sin juzgar su presencia y protege por
  defecto los valores concretos cuando la evidencia es discriminante.
- Normaliza factores a texto sólo en la operación, conserva `factor` en el
  perfil y devuelve texto al transformar columnas factor con `aplicar()`.
- Mantiene claves históricas estables en R 3.6 y fija explícitamente en UTC las
  fechas convertidas desde `Date`.
- Las claves históricas tratan el texto ilegible (UTF-8 inválido) como ausente:
  comparte con `NA` la marca `~`, en vez de intentar codificarlo como texto
  literal.
- Añade conteos explícitos de evaluados y afectados, con la unidad de conteo,
  a cada hallazgo; conserva NA cuando el alcance no permite conocerlos.
- Añade trazabilidad acotada por hallazgo mediante índices de fila, con estados
  explícitos para lo disponible, truncado, no aplicable y no disponible; el
  reporte resume el estado sin imprimir los índices.

## Medir y evaluar calidad

- Declara métricas genéricas, específicas e instanciadas con tipo de resultado
  y granularidad explícitos.
- Incluye veintiuna métricas automatizables, tres métricas tabulares basadas en
  referenciales y una correspondencia verificable con las 49 entradas del
  catálogo de AGESIC.
- Separa en el catálogo la disponibilidad de cada métrica de la causa o el
  matiz de esa disponibilidad, y documenta las 49 correspondencias sin vacíos.
- Ajusta las métricas oficiales de oportunidad al resultado booleano del marco
  y conserva la fórmula continua del curso CPAP bajo nombres `GradoOportunidad*`.
- Incorpora contratos explícitos `vigencia()` y `escala()`, y una tabla de
  cobertura que distingue lo medido, no declarado, no aplicable y fuera de
  alcance.
- Implementa las cuatro agregaciones del marco y la cadena de evaluación de
  medidas, reglas y perfiles de madurez. No calcula un índice global.
- Propone modelos editables a partir del perfil sin convertir observaciones de
  una sola entrega en requisitos silenciosos.
- Estima el costo antes de comparar, aplica un presupuesto de pares en los
  caminos exhaustivo y LSH, y publica el alcance de la estimación.
- Incorpora MinHash y LSH deterministas para generar candidatos a escala,
  con deduplicación por banda, garantía declarada y degradación explícita.
- Permite bloquear por una columna elegida por el usuario y estima los pares
  que el bloqueo puede dejar fuera, incluidos los ausentes como bloque propio.

## Mejorar y monitorear

- Construye planes de limpieza editables con alternativas mutuamente
  excluyentes, justificación, modo guiado opcional y consentimiento adicional
  para eliminaciones.
- Aplica sólo acciones activas sobre una copia, conserva un registro y permite
  imputaciones confirmadas mediante dependencias funcionales exactas.
- Acumula evaluaciones en un histórico plano y versionado; detecta deriva del
  modelo y cambios estructurales entre perfiles.
- Procesa comparaciones exhaustivas por lotes con parciales en un directorio
  declarado, cruza los lotes sin pérdida de pares y deja constancia de que no
  son reanudables.

## Informar

- Guarda y recupera análisis versionados sin datos de entrada por omisión y sin
  serializar entornos completos de reglas funcionales.
- Genera un único HTML autocontenido, en español, sin navegador, LaTeX ni
  recursos externos; los valores se escapan y la evidencia personal se
  enmascara por defecto.

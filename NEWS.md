# lupa 0.1.0

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
- Añade conteos explícitos de evaluados y afectados, con la unidad de conteo,
  a cada hallazgo; conserva NA cuando el alcance no permite conocerlos.

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

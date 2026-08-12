# lupa 0.1.0

## Fechas con meses escritos

- `detectar_formatos_fecha()` reconoce fechas con meses escritos en español
  (incluye `setiembre` y `set`) y en inglés, además de los formatos numéricos
  existentes. La tabla de nombres es propia y no depende de `LC_TIME`, y sólo
  acepta la estructura completa de una fecha o de un mes con año: encontrar
  `marzo` dentro de una oración no convierte el texto en fecha. Los meses
  escritos desambiguan el día y el mes; los años de dos dígitos siguen siendo
  candidatos y no se les asigna un siglo en silencio.
- Los períodos expresados sólo como mes y año declaran `granularidad = "mes"`
  y no inventan el día 1 para calcular mínimos, medias o conversiones; esos
  resúmenes quedan en `NA` con estado `granularidad_incompleta`. Los años
  escritos en meses también se limitan al rango 1800--2100, como las fechas
  compactas.
- La detección de meses sólo ejecuta sus expresiones regulares sobre los
  valores candidatos y reutiliza ese resultado al calcular el resumen de la
  columna. Así el texto libre que menciona meses no paga el costo completo ni
  se vuelve a analizar.
- El hallazgo de variantes del vocabulario sólo atribuye el límite de proporción
  cuando existe un grupo compatible que retener; si todas las cercanías fueron
  descartadas por secuencias numéricas incompatibles, lo informa con ese
  motivo.

## Variantes del vocabulario

- `perfilar()` agrega el hallazgo `casi_duplicados_vocabulario`: agrupa, por
  columna, variantes que la normalización funde o que quedan bajo el umbral de
  Jaro--Winkler y conserva la frecuencia de cada forma. La unidad es el valor
  distinto, no la fila; no se elige una forma canónica ni se modifica el dato.
  Las aristas de distancia forman estrellas alrededor de un valor de frecuencia
  estrictamente mayor y único; los empates no se fuerzan y no se cierra
  transitivamente una cadena de vecinos. Cada grupo declara su distancia mínima
  y máxima, y `max_proporcion_grupo_vocabulario`
  permite declarar que el diagnóstico no aplica cuando un componente abarca
  demasiado vocabulario; el filtro se activa desde 20 valores distintos o
  cuando el grupo mayor tiene al menos 10 variantes, y sólo suprime si la
  proporción también supera el umbral. Así no oculta grupos pequeños, pero
  tampoco entrega una columna entera como una sola familia. Cuando hay pares
  cercanos pero no una frecuencia central única, el alcance declara la falta de
  asimetría y apunta a `detectar_duplicados_aproximados()`. Las aristas de
  distancia con secuencias numéricas distintas se descartan; los ceros de
  relleno y separadores de miles se consideran equivalentes, pero una errata
  dentro de un número puede quedar sin agrupar deliberadamente. El alcance
  informa los pares descartados por números y separa el tamaño potencial del
  componente del tamaño que queda compatible con esa regla.
  `casi_duplicados_vocabulario = FALSE` lo desactiva.
  El alcance declara los valores y pares comparados, los recortes y la ausencia
  de [`stringdist`](https://cran.r-project.org/package=stringdist); las
  fusiones exactas se siguen informando sin ese paquete.
  Los resultados del perfil pueden cambiar porque ahora se señalan estas
  variantes como evidencia para una revisión de vocabulario.

## Referenciales

- Las métricas de referenciales heredan el perfil de `normalizar` declarado en
  `referencial()` (o aceptan uno explícito), por lo que variantes de caja,
  acentos y espacios pueden pasar a reconocerse como presentes. Esto cambia
  los resultados de correctitud y cobertura de forma deliberada; las claves
  siguen evaluándose por identidad exacta.
- `CorrectitudSemFuerte` y `CorrectitudSemDebil` pueden agregar, sin cambiar el
  veredicto, el candidato más cercano y su distancia como evidencia. La
  proximidad usa Jaro--Winkler por omisión (`p = 0.1`, umbral `0.10`), sólo se
  calcula para fallos y declara sus límites o la ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist). Se calcula
  sobre los valores fallidos distintos y se reparte a las filas repetidas; el
  alcance distingue filas fallidas, valores distintos y valores comparados.

## Perfil de normalización para comparar

- `normalizar` deja de ser sólo un interruptor lógico: `TRUE` conserva el caso
  común con minúsculas, espacios, acentos protegidos y comillas; `FALSE`
  desactiva esos pasos configurables; `"amplio"`, [normalizacion()] y una lista
  nombrada permiten elegirlos por columna. La representación normalizada sólo
  decide qué valores se comparan: nunca modifica los datos guardados.
- `perfilar()` conserva el perfil resuelto y los análisis de duplicados y claves
  lo heredan cuando reciben `normalizar = NULL`. Los resultados pueden cambiar
  porque el umbral se aplica sobre la cadena normalizada; el perfil informa,
  por vocabulario, cuántos valores fundió cada paso.
- La comparación aplica siempre descomposición y orden canónicos en el
  subconjunto latino cubierto; no reordena palabras ni aplica abreviaturas de
  vías. Las claves siguen descubriéndose por identidad exacta y agregan la
  unicidad normalizada como métrica informativa.
- El informe de fusiones compara el perfil completo con una versión que apaga
  cada paso por separado: sus cifras no son aditivas y el total normalizado se
  informa aparte. Ahora usa el vocabulario completo (las fusiones son una
  propiedad de pares que una muestra de valores puede ocultar) y la
  normalización se aplica de forma vectorizada; `n_usados` y el estado `exacto`
  dejan explícito el alcance real.
- El informe de fusiones no se calcula cuando `normalizar = FALSE`, porque no
  hay pasos configurables que evaluar. Cuando `perfilar()` ya lo calculó,
  `detectar_duplicados_aproximados(perfil = ...)` lo reutiliza en lugar de
  recorrer de nuevo el vocabulario.
- `proteger` acepta grafemas compuestos y el valor predeterminado conserva
  `g̃` además de `ñ` y `ü`, para no borrar letras guaraníes al comparar.

## Diagnósticos de texto invisible

- Amplía la detección a los espacios Unicode, marcas direccionales, BOM y
  otros invisibles de transporte. Los espacios Unicode se pueden colapsar a
  espacio ASCII sólo mediante una acción explícita y destructiva; ZWJ/ZWNJ se
  informan pero se conservan. La comparación normalizada usa estas mismas
  clases sin borrar caracteres semánticamente significativos.
- El hallazgo de separadores en campo, su acción y su conteo usan nombres
  específicos para cubrir tabulaciones, saltos, avances de página y tabulaciones
  verticales.
- `perfilar()` identifica controles C0/C1 e invisibles Unicode, entidades HTML
  reconocibles y separadores dentro de campos. La evidencia escapa esos
  caracteres (`<U+200B>`, `\\t`, `\\n`, `\\r`, `\\f`, `\\v`) y conserva los
  conteos por fila.
- Los controles invisibles que no son separadores se pueden eliminar y se
  recomiendan por defecto; decodificar entidades HTML y reemplazar separadores
  de línea quedan como acciones explícitas porque pueden cambiar contenido
  legítimo. Las tres dejan el número de valores cambiados en el registro.

## Reparación de texto y licencia

- La medida predeterminada de duplicados ahora aplica Jaro--Winkler con
  `p = 0.1` (el valor anterior era Jaro puro por `p = 0`) y el umbral pasa de
  `0.12` a `0.10`. Las dos decisiones pueden cambiar los pares informados al
  actualizar; el cambio es deliberado y queda declarado en la ayuda.

- Declara `cli (>= 3.0.0)`. El motor usa la interfaz de barras de progreso
  (`cli_progress_bar()` y sus compañeras), que existe recién desde esa
  versión; antes el requisito estaba supuesto y no escrito.

- Clasifica los duplicados exactos comparando los textos que realmente entran a
  la medida, después de normalizarlos, y no mediante igualdad exacta de un
  flotante. Esto hace el resultado independiente de la arquitectura y mantiene
  como `aproximado` un par de textos distintos aunque `soundex` devuelva
  distancia cero.

- Cierra el motor de reparación de texto: `decode_inconsistent_utf8` trabaja
  por subcadenas con el detector de [ftfy 6.3.1](https://github.com/rspeer/python-ftfy),
  conserva los estados parciales con U+FFFD y agrega tres extensiones
  deliberadas de badness sobre ftfy 6.3.1: la regla de inicio del issue
  [#222](https://github.com/rspeer/python-ftfy/issues/222), también discutida en
  el [PR #232](https://github.com/rspeer/python-ftfy/pull/232); la regla de caja
  que detecta mojibake de KOI8-R del issue
  [#231](https://github.com/rspeer/python-ftfy/issues/231); y la regla específica
  para `â` del issue [#233](https://github.com/rspeer/python-ftfy/issues/233).
  La tabla de bytes KOI8-R es la cuarta extensión y la puerta literal `Ã ` para
  formas portuguesas y francesas es la quinta.
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
- Conserva los espacios no separables y agrega el decodificador R puro de
  variantes UTF-8 de [ftfy](https://github.com/rspeer/python-ftfy): combina
  pares CESU-8 y reconoce `C0 80`, e incorpora la tabla de bytes KOI8-R
  adicional, con los estados y pérdidas ya declarados.
- Declara como quinta extensión deliberada la puerta adicional para la secuencia
  literal `Ã `, que conserva las formas portuguesas y francesas observadas en
  padrones; el decodificador de variantes rechaza secuencias que producirían un
  NUL, en vez de omitir un carácter al materializar el texto.
- La licencia del paquete pasa de `GPL-2 | GPL-3` a `GPL-3`; las partes
  derivadas del diseño de [ftfy](https://github.com/rspeer/python-ftfy) se
  atribuyen en `LICENSE.note` bajo Apache-2.0.
- La estrategia de reparación de texto se registra como `reparar_codificacion`.

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

- Detecta relaciones de orden sospechosas entre columnas numéricas o temporales
  comparables (por ejemplo, `inicio <= fin` y `monto_bruto <= monto_neto`).
  El hallazgo conserva los conteos y las filas fuera de orden, sugiere
  formalizar la regla con `ReglaIntegridadIntraEntidad` y declara en
  `meta$orden_columnas` las columnas y pares efectivamente comparados. Expone
  un filtro opcional de solapamiento intercuartil para tablas anchas;
  está apagado por omisión (umbral `0`) porque activarlo puede ocultar
  relaciones reales entre magnitudes de rangos distintos. Los pares
  descartados quedan contados en el alcance.

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

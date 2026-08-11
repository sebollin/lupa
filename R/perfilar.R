#' Perfilar un conjunto de datos
#'
#' Examina un `data.frame`, `tibble` o `data.table` y devuelve estadísticas
#' generales, métricas por columna, patrones, formatos de fecha y hallazgos
#' accionables. Todas las proporciones se expresan en `[0, 1]`.
#'
#' Los umbrales de faltantes se aplican a la suma de ausentes reales y
#' faltantes disfrazados y son estrictos: la proporción debe superar el umbral
#' para generar el nivel correspondiente. La lista de cadenas está congelada con referencia a
#' [naniar](https://github.com/njtierney/naniar)::common_na_strings 1.1.0 y suma extensiones habituales en datos
#' administrativos uruguayos. Las entradas que [naniar](https://github.com/njtierney/naniar) expresa como patrones
#' escapados se adaptan a los signos literales de interrogación, asterisco y
#' punto porque aquí se comparan por igualdad. La lista no depende de la
#' versión instalada.
#' Los sentinelas numéricos predeterminados son `-9`, `-99`, `-999`, `-9999` y
#' `999`. La lista es deliberadamente más corta que
#' [naniar](https://github.com/njtierney/naniar)::common_na_numbers 1.1.0:
#' `66`, `77`, `88` y `9999` también pueden
#' ser edades, códigos o años legítimos. `sentinelas_numericos` representa la
#' política completa, no una lista que se agrega silenciosamente: use
#' `numeric()` para desactivar todos los sentinelas numéricos, o
#' `sentinelas_naniar` para solicitar explícitamente la lista de naniar.
#'
#' `muestra` limita sólo el descubrimiento de patrones, la inferencia de tipos y
#' la detección de formatos de fecha. Las demás métricas y hallazgos se calculan
#' sobre todas las filas. Por eso `meta$filas_analizadas` describe el máximo
#' usado por los análisis muestreados, no el alcance del perfil completo.
#'
#' Una columna cuyo año se expresa con dos dígitos se informa con su
#' `tipo_inferido` —`"fecha"` o `"fecha-hora"`— pero deja `minimo_fecha`,
#' `maximo_fecha`, `media_fecha` y `mediana_fecha` en `NA`. No es una omisión:
#' `23` puede ser 1923 o 2023, y elegir el siglo para calcular un rango sería
#' inventarlo. El hallazgo `anio_de_dos_digitos` señala esas columnas, y el
#' rango aparece una vez que el usuario resuelve la ambigüedad.
#'
#' Para números ordinarios, los estadísticos cuantitativos se calculan sólo con
#' valores finitos; `n_nan`, `n_infinito_positivo` y `n_infinito_negativo`
#' declaran lo excluido. En columnas `integer64` que exceden el entero máximo
#' representable exactamente por `double`, `minimo` y `maximo` quedan en `NA` y
#' los extremos exactos se conservan en `minimo_exacto` y `maximo_exacto`.
#' Una columna de listas intenta contar sus valores distintos; si la clase no
#' admite comparación, informa `NA` en lugar de afirmar cero.
#' Las columnas matriciales se conservan como una unidad por fila: `n` informa
#' las filas de la tabla, pero los estadísticos por valor quedan en `NA` y un
#' hallazgo explica que deben separarse en columnas con semántica explícita.
#' Los valores de texto que no forman UTF-8 válido tampoco se convierten: se
#' cuentan, se excluyen de los análisis textuales y generan un hallazgo con sus
#' posiciones. Los diagnósticos de invisibles incluyen controles C0/C1,
#' espacios Unicode, marcas direccionales, BOM y otros caracteres de transporte.
#' La evidencia los muestra como puntos de código; los espacios Unicode se
#' detectan aunque sólo se normalizan mediante una acción explícita, y ZWJ/ZWNJ
#' se informan pero se conservan porque pueden ser semánticos. La comparación
#' de duplicados con `normalizar = TRUE` aplica estas mismas clases sin borrar
#' ZWJ/ZWNJ.
#'
#' Los resúmenes de fecha-hora se expresan siempre en UTC y llevan el sufijo
#' `UTC` en el texto para hacer visible la zona aplicada. El instante se
#' conserva aunque la columna de entrada use otra zona horaria.
#'
#' El diagnóstico de formas Unicode compara sin modificar el texto y puede usar
#' el paquete opcional `stringi` para enriquecer la evidencia cuando existen
#' caracteres no ASCII. El perfil de comparación es completamente R base.
#' El argumento `normalizar` declara el perfil de comparación que se conserva
#' en `meta$normalizacion`; cambia sólo la representación usada para comparar,
#' no el texto guardado. `TRUE` usa el perfil predeterminado, `FALSE` desactiva
#' sus pasos configurables, `"amplio"` activa los tres pliegues optativos y
#' [normalizacion()] permite declararlos. También admite una lista nombrada por
#' columna. `meta$normalizacion_fusiones` informa las colisiones por paso sobre
#' el vocabulario de cada columna. Cuando el vocabulario completo cabe en el
#' límite se informa el estado `exacto`; en vocabularios mayores se toma una
#' muestra determinista de hasta 500 valores y se informa
#' `estimado_sobre_muestra`, junto con `n_distintos` y `n_usados`. Así el
#' informe conserva una red de seguridad acotada también para columnas de alta
#' cardinalidad, sin presentar una estimación como conteo exacto.
#'
#' La clasificación de posibles datos personales es más amplia que la
#' protección. Cada clasificación declara `poder_discriminante` y `proteger`:
#'
#' - `debil`: una forma genérica, como siete a doce dígitos, coincide también
#'   con importes, facturas y códigos; se informa pero no se ocultan valores;
#' - `medio`: el nombre de la columna expresa una categoría personal (por
#'   ejemplo `telefono` o `fecha_nacimiento`); se protege aunque sus valores no
#'   se puedan validar. El nombre tiene prioridad sobre una forma numérica
#'   genérica y también determina la etiqueta de tipo;
#' - `alto`: una forma muy específica, como un correo, o nombre y forma se
#'   apoyan mutuamente; se protege;
#' - `verificado`: al menos tres valores distintos y al menos el 90% cumple uno
#'   de los validadores personales configurados; se protege incluso sin un
#'   nombre orientador. El pack uruguayo es el predeterminado, pero puede
#'   reemplazarse por un `pack_validadores()` de otro país o desactivarse con
#'   `FALSE`. La tolerancia del 10% permite tipeos aislados sin convertir una
#'   columna real en una salida pública; el umbral es configurable.
#'
#' La forma genérica de siete a doce dígitos tiene poder discriminante débil:
#' también describe importes, teléfonos, facturas e identificadores. Las formas
#' con separadores sólo se aceptan cuando tienen una estructura de documento
#' reconocible (por ejemplo, una cédula con grupos y guion o un RUT con grupos
#' de tres y cuatro dígitos); una fecha ISO, una fecha con puntos o guiones y
#' separadores arbitrarios no se consideran documentos. Un validador de dígito
#' que supera el umbral aporta evidencia verificable y eleva la clasificación;
#' una forma sola nunca se trata como prueba de identidad.
#'
#' Este criterio mide capacidad de discriminación, no juzga si la presencia del
#' dato es correcta. La protección sustituye modas, ejemplos, evidencia y
#' extremos o medianas que corresponden a observaciones reales. Las medias y
#' desvíos se conservan como síntesis no ligadas a una fila;
#' `detalle_proteccion_personal` hace visible la supresión. En fechas de
#' nacimiento, un hallazgo separado conserva el diagnóstico de valores
#' anteriores a 1900 o posteriores a la corrida sin publicar las fechas.
#' Los números escritos como texto reconocen tanto coma como punto decimal y
#' sus separadores de miles simétricos. Los prefijos de tres letras separados
#' del número, con forma de código ISO 4217, y los símbolos monetarios se
#' conservan como evidencia; una columna sin datos suficientes para desambiguar
#' un separador de tres dígitos no se convierte automáticamente.
#'
#' @param datos Objeto que hereda de `data.frame`.
#' @param nombre Nombre descriptivo del objeto.
#' @param fecha Fecha y hora de la corrida. Se puede fijar para construir series
#'   reproducibles; se normaliza a UTC.
#' @param muestra Máximo de filas usadas para patrones e inferencia de tipos.
#'   Use `Inf` para analizar todas las filas.
#' @param max_patrones Máximo de patrones mostrados por columna.
#' @param distinguir_mayusculas Si se distinguen mayúsculas y minúsculas.
#' @param expandir Si se emite un token por carácter en los patrones.
#' @param umbral_alta_cardinalidad Umbral para columnas categóricas.
#' @param umbral_faltantes_sospechoso Umbral inferior de faltantes. El
#'   hallazgo se activa al superarlo en sentido estricto.
#' @param umbral_faltantes_error Umbral por encima del cual los faltantes son
#'   un error; la igualdad conserva la severidad sospechosa.
#' @param umbral_patron_raro Máxima frecuencia de un patrón raro.
#' @param umbral_patron_dominante Frecuencia mínima del patrón dominante.
#' @param columnas_sin_ceros Nombres de columnas donde cero no es admisible.
#' @param columnas_no_negativas Nombres de columnas que deben ser no negativas.
#' @param sentinelas_numericos Vector completo de valores numéricos que se
#'   interpretan como ausencia. `numeric()` los desactiva; las cadenas de
#'   ausencia se siguen evaluando por separado.
#' @param analizar_dependencias Si se buscan dependencias funcionales entre
#'   pares de columnas. Se aplica una sola muestra común a toda la tabla.
#' @param umbral_dependencia Cumplimiento mínimo para informar una dependencia.
#' @param umbral_casi_clave_dependencia Tasa de valores distintos a partir de
#'   la cual un determinante se descarta como casi-clave antes de agrupar.
#' @param max_columnas_dependencias Máximo de columnas que intervienen en la
#'   búsqueda, cuyo costo crece cuadráticamente.
#' @param datos_personales_permitidos Si la entrega admite datos personales.
#'   El valor predeterminado no juzga su presencia: la clasificación se informa
#'   con severidad `"ok"`. Use `FALSE` sólo cuando el contrato de la entrega
#'   declare que no deben existir.
#' @param proteger_datos_personales Si se reemplazan modas, ejemplos, evidencia
#'   y estadísticos de orden concretos cuando `poder_discriminante` es medio,
#'   alto o verificado. Las clasificaciones débiles se conservan como aviso pero
#'   no suprimen estadísticos. Para conservar todo en el objeto debe desactivarse
#'   explícitamente; [reportar()] aplica además su propia protección
#'   predeterminada.
#' @param validadores_personales Pack o lista nombrada de funciones que reciben
#'   un vector de texto y devuelven un lógico de igual longitud. `NULL` usa
#'   `validadores_uruguay()` por compatibilidad; `FALSE` o `numeric()` desactiva
#'   la verificación de documentos. El nombre del mejor validador queda en el
#'   fundamento de la clasificación.
#' @param umbral_documento_verificado Proporción mínima de valores que debe
#'   aceptar un validador para clasificar una forma de documento como
#'   `verificado`. Por defecto es `0.9`.
#' @param muestra_validadores Máximo de valores usados en el filtro preliminar
#'   de cada validador. Si la proporción preliminar ya queda bajo el umbral no
#'   se valida la columna completa; use `Inf` para revisar todos desde el inicio.
#' @param duplicados_aproximados `FALSE` por omisión. Use `TRUE` o una lista de
#'   argumentos para ejecutar [detectar_duplicados_aproximados()] y añadir sus
#'   pares y hallazgos al perfil. Es un análisis acotado y opcional porque no
#'   afirma identidad ni debe encarecer todas las corridas.
#' @param normalizar Perfil de comparación que se conserva en `meta$normalizacion`
#'   y que heredan los análisis de duplicados y claves cuando no reciben uno
#'   explícito. Cambia sólo la representación usada para comparar.
#' @param max_filas_hallazgo Tope de índices de fila que conserva cada
#'   trazabilidad disponible. Por defecto es `1000`; cuando se supera, el
#'   estado queda como `truncada` y el total se conserva. Use `Inf` sólo si
#'   necesita desactivar explícitamente el tope.
#' @param umbral_orden_columnas Cumplimiento mínimo de una relación de orden
#'   entre columnas comparables. Se usa `0.95` por omisión; con menos de 20
#'   filas comparables se permite una sola inversión para no descartar tablas
#'   pequeñas. El alcance efectivo queda en `meta$orden_columnas`.
#' @param max_columnas_orden Máximo de columnas numéricas o temporales que se
#'   comparan entre sí para detectar relaciones de orden. Las columnas que
#'   exceden el límite se conservan en `meta$orden_columnas$columnas_omitidas`.
#' @param umbral_solapamiento_orden Solapamiento mínimo de los rangos
#'   intercuartiles para considerar que dos columnas representan magnitudes
#'   comparables. Por defecto es `0`, por lo que el filtro está apagado y no se
#'   descartan pares. Un valor mayor resulta útil en tablas anchas con columnas
#'   de escalas muy distintas, donde el detector puede avisar de más, pero
#'   también puede ocultar relaciones reales entre magnitudes de rangos
#'   distintos (por ejemplo, nacimiento y solicitud). Los pares descartados se
#'   cuentan en `meta$orden_columnas$pares_descartados_magnitud`.
#'
#' @return Objeto S3 de clase `perfil`. Cada fila de hallazgos incluye
#'   n_evaluados, n_afectados y unidad_conteo: son conteos de las unidades
#'   declaradas (por ejemplo fila, columna, formato o par). Cuando el camino
#'   no puede conocer un conteo, informa NA, nunca cero. La columna de lista
#'   `trazabilidad` distingue `disponible`, `truncada`, `no_aplica` y
#'   `no_disponible`; cuando corresponde conserva índices de fila acotados por
#'   `max_filas_hallazgo`, el total conocido y el alcance (completo o parcial).
#'   Los índices no contienen valores. Usarlos para extraer filas de los datos
#'   originales puede volver a exponer datos personales; el paquete no realiza
#'   esa extracción y la protección de salidas no sustituye el control de acceso
#'   a los datos de entrada.
#' @export
#' @seealso [descubrir_patrones()], [detectar_dependencias()],
#'   [proponer_modelo()], [planificar_limpieza()]
#'
#' @examples
#' perfil <- perfilar(datos_administrativos)
#' perfil
#' summary(perfil)
perfilar <- function(datos,
                     nombre = deparse(substitute(datos)),
                     fecha = Sys.time(),
                     muestra = 1e5,
                     max_patrones = 20,
                     distinguir_mayusculas = TRUE,
                     expandir = FALSE,
                     umbral_alta_cardinalidad = 0.5,
                     umbral_faltantes_sospechoso = 0.1,
                     umbral_faltantes_error = 0.4,
                     umbral_patron_raro = 0.05,
                     umbral_patron_dominante = 0.5,
                     columnas_sin_ceros = character(),
                     columnas_no_negativas = character(),
                     sentinelas_numericos = c(-9, -99, -999, -9999, 999),
                     analizar_dependencias = TRUE,
                     umbral_dependencia = 0.995,
                     umbral_casi_clave_dependencia = 0.8,
                     max_columnas_dependencias = 100L,
                     datos_personales_permitidos = TRUE,
                     proteger_datos_personales = TRUE,
                     validadores_personales = NULL,
                     umbral_documento_verificado = 0.9,
                     muestra_validadores = 1000L,
                     duplicados_aproximados = FALSE,
                     normalizar = TRUE,
                     max_filas_hallazgo = 1000L,
                     umbral_orden_columnas = 0.95,
                     max_columnas_orden = 20L,
                     umbral_solapamiento_orden = 0) {
  if (!inherits(datos, "data.frame")) {
    stop("`datos` debe ser un data.frame, tibble o data.table.", call. = FALSE)
  }
  normalizacion_resuelta <- .resolver_normalizacion(normalizar)
  fecha_hora <- tryCatch(.fecha_utc(fecha), error = function(e) NA)
  if (length(fecha_hora) != 1L || is.na(fecha_hora) ||
      !is.finite(as.numeric(fecha_hora))) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  umbrales <- c(
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro, umbral_patron_dominante
  )
  if (anyNA(umbrales) || any(umbrales < 0 | umbrales > 1)) {
    stop("Todos los umbrales deben estar entre 0 y 1.", call. = FALSE)
  }
  if (umbral_faltantes_error < umbral_faltantes_sospechoso) {
    stop("El umbral de error no puede ser menor que el sospechoso.", call. = FALSE)
  }
  if (!is.numeric(sentinelas_numericos) || anyNA(sentinelas_numericos) ||
      any(!is.finite(sentinelas_numericos))) {
    stop("`sentinelas_numericos` debe ser un vector num\u00e9rico finito.", call. = FALSE)
  }
  if (!is.logical(analizar_dependencias) || length(analizar_dependencias) != 1L ||
      is.na(analizar_dependencias)) {
    stop("`analizar_dependencias` debe ser un l\u00f3gico escalar sin NA.",
         call. = FALSE)
  }
  for (argumento in c("datos_personales_permitidos", "proteger_datos_personales")) {
    valor <- get(argumento)
    if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
      stop("`", argumento, "` debe ser TRUE o FALSE.", call. = FALSE)
    }
  }
  if (!is.numeric(umbral_documento_verificado) ||
      length(umbral_documento_verificado) != 1L ||
      is.na(umbral_documento_verificado) ||
      umbral_documento_verificado < 0 || umbral_documento_verificado > 1) {
    stop("`umbral_documento_verificado` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(muestra_validadores) || length(muestra_validadores) != 1L ||
      is.na(muestra_validadores) || muestra_validadores < 1 ||
      (!is.infinite(muestra_validadores) &&
       muestra_validadores != floor(muestra_validadores))) {
    stop("`muestra_validadores` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  muestra_validadores <- if (is.infinite(muestra_validadores)) Inf else {
    as.integer(muestra_validadores)
  }
  if (!is.numeric(max_filas_hallazgo) || length(max_filas_hallazgo) != 1L ||
      is.na(max_filas_hallazgo) || max_filas_hallazgo < 1 ||
      (!is.infinite(max_filas_hallazgo) &&
       max_filas_hallazgo != floor(max_filas_hallazgo))) {
    stop("`max_filas_hallazgo` debe ser un entero positivo o Inf.", call. = FALSE)
  }
  max_filas_hallazgo <- if (is.infinite(max_filas_hallazgo)) {
    Inf
  } else as.integer(max_filas_hallazgo)
  if (!is.numeric(umbral_orden_columnas) ||
      length(umbral_orden_columnas) != 1L ||
      is.na(umbral_orden_columnas) || umbral_orden_columnas <= 0 ||
      umbral_orden_columnas > 1) {
    stop("`umbral_orden_columnas` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.numeric(max_columnas_orden) || length(max_columnas_orden) != 1L ||
      is.na(max_columnas_orden) || max_columnas_orden < 2 ||
      max_columnas_orden != floor(max_columnas_orden)) {
    stop("`max_columnas_orden` debe ser un entero de al menos 2.", call. = FALSE)
  }
  max_columnas_orden <- as.integer(max_columnas_orden)
  if (!is.numeric(umbral_solapamiento_orden) ||
      length(umbral_solapamiento_orden) != 1L ||
      is.na(umbral_solapamiento_orden) ||
      umbral_solapamiento_orden < 0 || umbral_solapamiento_orden > 1) {
    stop("`umbral_solapamiento_orden` debe estar entre 0 y 1.", call. = FALSE)
  }
  if (!is.logical(duplicados_aproximados) &&
      !is.list(duplicados_aproximados)) {
    stop("`duplicados_aproximados` debe ser FALSE, TRUE o una lista de argumentos.",
         call. = FALSE)
  }
  if (is.logical(duplicados_aproximados) &&
      (length(duplicados_aproximados) != 1L ||
       is.na(duplicados_aproximados))) {
    stop("`duplicados_aproximados` debe ser FALSE o TRUE.", call. = FALSE)
  }
  if (is.list(duplicados_aproximados) &&
      any(names(duplicados_aproximados) %in% c(
        "datos", "clasificacion", "perfil", "proteger_datos_personales",
        "normalizar"
      ))) {
    stop("`duplicados_aproximados` no puede reemplazar argumentos coordinados por perfilar().",
         call. = FALSE)
  }
  validadores_personales <- .normalizar_validadores_personales(
    validadores_personales
  )

  nombres <- names(datos)
  if (is.null(nombres)) {
    nombres <- paste0("V", seq_len(ncol(datos)))
  }
  nombres_lista <- make.unique(nombres)
  resultados <- lapply(seq_len(ncol(datos)), function(i) {
    .perfilar_columna(
      datos[[i]], nombres[[i]], muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )
  })

  columnas <- if (length(resultados)) {
    do.call(rbind, lapply(resultados, `[[`, "fila"))
  } else {
    .perfilar_columna(
      character(), "", muestra, max_patrones,
      distinguir_mayusculas, expandir, umbral_patron_raro,
      sentinelas_numericos
    )$fila[0, , drop = FALSE]
  }
  rownames(columnas) <- NULL
  patrones <- lapply(resultados, `[[`, "patrones")
  formatos_fecha <- lapply(resultados, `[[`, "formatos")
  names(patrones) <- nombres_lista
  names(formatos_fecha) <- nombres_lista
  dependencias <- if (analizar_dependencias) {
    detectar_dependencias(
      datos, umbral = umbral_dependencia, muestra = muestra,
      max_columnas = max_columnas_dependencias,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  } else {
    detectar_dependencias(
      datos[0, 0, drop = FALSE], umbral = umbral_dependencia,
      max_columnas = 1L,
      umbral_casi_clave = umbral_casi_clave_dependencia
    )
  }

  n_filas_duplicadas <- tryCatch(
    sum(duplicated(datos)),
    error = function(e) NA_integer_
  )
  n_filas_en_grupos_duplicados <- tryCatch(
    sum(duplicated(datos) | duplicated(datos, fromLast = TRUE)),
    error = function(e) NA_integer_
  )
  filas_completas <- tryCatch(
    sum(stats::complete.cases(datos)),
    error = function(e) NA_integer_
  )
  duplicadas <- .columnas_duplicadas(datos, nombres)
  relaciones_orden <- .detectar_orden_columnas(
    datos, columnas, resultados, formatos_fecha,
    umbral = umbral_orden_columnas, max_columnas = max_columnas_orden,
    umbral_solapamiento = umbral_solapamiento_orden
  )
  tipos <- table(vapply(seq_along(datos), function(i) {
    .tipo_declarado(datos[[i]])
  }, character(1L)))
  tipos_columnas <- data.frame(
    tipo = names(tipos), n = as.integer(tipos), stringsAsFactors = FALSE
  )
  general <- list(
    filas = nrow(datos),
    columnas = ncol(datos),
    celdas = as.numeric(nrow(datos)) * as.numeric(ncol(datos)),
    memoria_bytes = as.numeric(utils::object.size(datos)),
    filas_completas = filas_completas,
    filas_duplicadas = n_filas_duplicadas,
    filas_en_grupos_duplicados = n_filas_en_grupos_duplicados,
    tipos_columnas = tipos_columnas,
    columnas_duplicadas = duplicadas
  )
  hallazgos <- .construir_hallazgos(
    datos, resultados, nombres, duplicadas,
    umbral_alta_cardinalidad, umbral_faltantes_sospechoso,
    umbral_faltantes_error, umbral_patron_raro,
    umbral_patron_dominante, columnas_sin_ceros,
    columnas_no_negativas,
    if (is.na(n_filas_duplicadas)) 0L else n_filas_duplicadas,
    relaciones_orden = relaciones_orden$hallazgos
  )
  datos_personales <- .detectar_datos_personales(
    datos, nombres, resultados,
    validadores = validadores_personales,
    umbral_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores
  )
  indice_personal <- match(columnas$columna, datos_personales$columna)
  columnas$dato_personal_posible <- !is.na(indice_personal)
  columnas$tipo_dato_personal <- datos_personales$tipo[indice_personal]
  columnas$proporcion_dato_personal <-
    datos_personales$proporcion_compatible[indice_personal]
  columnas$poder_discriminante_dato_personal <-
    datos_personales$poder_discriminante[indice_personal]
  columnas$dato_personal_protegido <- ifelse(
    is.na(indice_personal), FALSE, datos_personales$proteger[indice_personal]
  )
  hallazgos_personales <- .hallazgos_datos_personales(
    datos_personales, datos_personales_permitidos
  )
  hallazgos_personales <- c(
    hallazgos_personales,
    .hallazgos_rango_nacimiento(columnas, datos_personales, fecha_hora)
  )
  if (length(hallazgos_personales)) {
    hallazgos <- do.call(rbind, c(list(hallazgos), hallazgos_personales))
    hallazgos$severidad <- factor(
      as.character(hallazgos$severidad),
      levels = c("ok", "sospechoso", "error"), ordered = TRUE
    )
    rownames(hallazgos) <- NULL
  }
  aproximados <- if (is.logical(duplicados_aproximados) &&
      !duplicados_aproximados) {
    NULL
  } else {
    configuracion <- if (isTRUE(duplicados_aproximados)) {
      list()
    } else duplicados_aproximados
    do.call(
      .detectar_duplicados_aproximados,
      c(
        list(
          datos = datos, clasificacion = datos_personales,
          normalizar = normalizacion_resuelta,
          proteger_datos_personales = proteger_datos_personales
        ),
        configuracion
      )
    )
  }
  if (!is.null(aproximados) && nrow(aproximados$hallazgos)) {
    hallazgos <- rbind(hallazgos, aproximados$hallazgos)
    rownames(hallazgos) <- NULL
  }
  hallazgos <- .agregar_trazabilidad_hallazgos(
    hallazgos, datos, nombres, resultados, expandir = expandir,
    aproximados = aproximados, limite = max_filas_hallazgo,
    distinguir_mayusculas = distinguir_mayusculas
  )
  meta <- list(
    nombre = nombre,
    fecha_hora = fecha_hora,
    version = .version_paquete(),
    filas_totales = nrow(datos),
    filas_analizadas = min(nrow(datos), floor(muestra)),
    muestreo = nrow(datos) > muestra,
    muestra = muestra,
    max_patrones = max_patrones,
    distinguir_mayusculas = distinguir_mayusculas,
    expandir = expandir,
    umbral_patron_raro = umbral_patron_raro,
    analizar_dependencias = analizar_dependencias,
    umbral_dependencia = umbral_dependencia,
    umbral_casi_clave_dependencia = umbral_casi_clave_dependencia,
    max_columnas_dependencias = max_columnas_dependencias,
    sentinelas_numericos = .numeros_na(sentinelas_numericos),
    datos_personales_permitidos = datos_personales_permitidos,
    proteger_datos_personales = proteger_datos_personales,
    validadores_personales = names(validadores_personales),
    umbral_documento_verificado = umbral_documento_verificado,
    muestra_validadores = muestra_validadores,
    max_filas_hallazgo = max_filas_hallazgo,
    umbral_orden_columnas = umbral_orden_columnas,
    max_columnas_orden = max_columnas_orden,
    umbral_solapamiento_orden = umbral_solapamiento_orden,
    orden_columnas = relaciones_orden$alcance,
    normalizacion = normalizacion_resuelta,
    normalizacion_resumen = .normalizacion_resumen(normalizacion_resuelta),
    normalizacion_fusiones = .normalizacion_fusiones_tabla(
      datos, normalizacion_resuelta
    )
  )
  estructura <- list(
    general = general,
    columnas = columnas,
    patrones = patrones,
    formatos_fecha = formatos_fecha,
    dependencias = dependencias,
    hallazgos = hallazgos,
    datos_personales = datos_personales,
    meta = meta
  )
  if (!is.null(aproximados)) {
    estructura$duplicados_aproximados <- aproximados
    estructura <- estructura[c(
      "general", "columnas", "patrones", "formatos_fecha", "dependencias",
      "duplicados_aproximados", "hallazgos", "datos_personales", "meta"
    )]
  }
  class(estructura) <- "perfil"
  if (proteger_datos_personales) estructura <- .proteger_perfil(estructura)
  estructura
}

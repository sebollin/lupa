.deriva_perfil_vacia <- function() {
  resultado <- data.frame(
    columna = character(), aspecto = character(), cambio = character(),
    severidad = character(), valor_anterior = character(),
    valor_actual = character(), delta = numeric(),
    cambio_relativo = numeric(), significativo = logical(),
    fecha_anterior = as.POSIXct(character(), tz = "UTC"),
    fecha_actual = as.POSIXct(character(), tz = "UTC"),
    descripcion = character(), evidencia = character(),
    stringsAsFactors = FALSE
  )
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

.validar_perfil_deriva <- function(x, nombre) {
  if (!inherits(x, "perfil") || !inherits(x$columnas, "data.frame") ||
      !is.list(x$patrones) || !inherits(x$hallazgos, "data.frame") ||
      is.null(x$meta$fecha_hora)) {
    stop("`", nombre, "` debe ser un objeto producido por perfilar().",
         call. = FALSE)
  }
  x
}

.mapa_columnas_perfil <- function(perfil) {
  nombres <- as.character(perfil$columnas$columna)
  nombres[is.na(nombres) | !nzchar(nombres)] <- "<sin_nombre>"
  data.frame(
    clave = .nombres_unicos(.nombres_para_operar(nombres)), original = nombres,
    indice = seq_along(nombres), stringsAsFactors = FALSE
  )
}

.distinto_deriva <- function(a, b) {
  if (length(a) != 1L || length(b) != 1L) return(TRUE)
  if (is.na(a) && is.na(b)) return(FALSE)
  if (is.na(a) || is.na(b)) return(TRUE)
  !isTRUE(all.equal(a, b, check.attributes = FALSE))
}

.texto_deriva <- function(x) {
  if (!length(x) || is.na(x)) return(NA_character_)
  if (is.numeric(x)) return(format(x, digits = 8L, trim = TRUE))
  texto <- as.character(x)
  # Una diferencia que el canal no muestra es una diferencia que el lector no
  # puede ver. Recortar espacios cambia el patron de `a+\a+9\a+9a ` a
  # `a+\a+9\a+9a`, y la deriva informaba seis cambios con severidad `error`
  # cuyos `valor_anterior` y `valor_actual` se imprimian IDENTICOS: el espacio
  # final no se ve. Se cita el valor solo cuando hace falta -espacio al borde,
  # cadena vacia, o algo que haya que escapar-, para que la diferencia se vea
  # sin agregar ruido al resto.
  # Los dos motivos para citar son distintos y no se pueden mezclar. Si hay algo
  # que escapar, se rinde con el renderizador del paquete. Si lo unico que pasa
  # es que el valor tiene un espacio al borde o esta vacio, se lo encierra entre
  # comillas TAL CUAL: pasarlo por `encodeString()` duplicaria las barras del
  # propio patron y la fila citada dejaria de casar con la que no lo esta.
  if (.hay_escapes_al_publicar(texto)) return(.citar_publicable(texto))
  if (!nzchar(texto) || !identical(texto, trimws(texto))) {
    return(paste0('"', texto, '"'))
  }
  texto
}

.configuracion_patrones_perfil <- function(perfil) {
  campos <- c(
    "muestra", "max_patrones", "distinguir_mayusculas", "expandir",
    "umbral_patron_raro"
  )
  if (!all(campos %in% names(perfil$meta))) return(NULL)
  perfil$meta[campos]
}

# La normalizacion es la cuarta politica que redefine lo que se mide, y la
# unica que no se declaraba. Medido: con los MISMOS datos y `normalizar = FALSE`
# desaparece `casi_duplicados_vocabulario`, y la deriva informaba "Un hallazgo
# del perfil anterior ya no esta presente" con severidad `ok`. En monitoreo,
# apagar la normalizacion se leia como "los datos mejoraron". El `meta` guardaba
# la politica y la comparacion no la miraba.
# Las varas que deciden si un hallazgo se emite y con que severidad. Se comparan
# como CONJUNTO y en una sola fila: seis filas separadas serian ruido, y lo que
# el lector necesita saber es que la vara cambio, no cual de los seis umbrales.
.proteccion_declarada_perfil <- function(perfil) {
  # `perfilar()` declara su politica en `meta$proteger_datos_personales`. La via
  # DBI usa `meta$proteccion_personal$aplicada`, asi que se miran las dos y se
  # devuelve NA cuando ninguna esta: sin declaracion no se afirma nada.
  if (!is.list(perfil)) return(NA)
  meta <- perfil$meta
  if (!is.null(meta$proteger_datos_personales)) {
    valor <- meta$proteger_datos_personales[[1L]]
    if (is.logical(valor) && !is.na(valor)) return(valor)
  }
  aplicada <- meta$proteccion_personal$aplicada
  if (length(aplicada) == 1L && is.logical(aplicada) && !is.na(aplicada)) {
    return(aplicada)
  }
  NA
}

.configuracion_umbrales_perfil <- function(perfil) {
  campos <- c(
    "umbral_faltantes_sospechoso", "umbral_faltantes_error",
    "umbral_alta_cardinalidad", "umbral_patron_dominante",
    "umbral_patron_raro", "columnas_sin_ceros", "columnas_no_negativas"
  )
  presentes <- intersect(campos, names(perfil$meta))
  if (!length(presentes)) return(NULL)
  valores <- lapply(presentes, function(campo) {
    valor <- perfil$meta[[campo]]
    if (is.null(valor) || !length(valor)) "ninguna"
    else {
      texto <- as.character(valor)
      texto <- texto[!duplicated(.clave_bytes(texto))]
      paste(.clave_bytes(.ordenar_por_bytes(texto)), collapse = "/")
    }
  })
  stats::setNames(unlist(valores, use.names = FALSE), presentes)
}

.configuracion_normalizacion_perfil <- function(perfil) {
  if (!"normalizacion" %in% names(perfil$meta)) return(NULL)
  general <- perfil$meta$normalizacion$general
  # Las politicas son CONJUNTOS: el orden en que se escriben es un artefacto de
  # la declaracion. Sin ordenar, `normalizacion(proteger = c("n", "u"))` contra
  # `normalizacion(proteger = c("u", "n"))` producia una fila
  # "modificado / error" cuyo valor anterior y actual eran IDENTICOS: la fila se
  # contradecia sola. Es la misma regla que el paquete ya fija para los pesos de
  # `agregar()`: la misma declaracion escrita en otro orden da el mismo numero.
  if (is.list(general) && !is.null(general$proteger)) {
    proteger <- as.character(general$proteger)
    proteger <- proteger[!duplicated(.clave_bytes(proteger))]
    general$proteger <- .clave_bytes(.ordenar_por_bytes(proteger))
  }
  general
}

.configuracion_aplicabilidad_perfil <- function(perfil) {
  if (!"declaracion_aplicabilidad" %in% names(perfil$meta)) return(NULL)
  valores <- as.character(perfil$meta$declaracion_aplicabilidad)
  valores <- valores[!duplicated(.clave_bytes(valores))]
  .clave_bytes(.ordenar_por_bytes(valores))
}

.configuracion_sentinelas_perfil <- function(perfil) {
  if (!"sentinelas_numericos" %in% names(perfil$meta)) return(NULL)
  # Un conjunto, no una secuencia: `c(999, -9)` y `c(-9, 999)` son la misma
  # politica y se declaraban como un cambio de vara con severidad `error`, que
  # es justo la fila que una serie de calidad lee como transicion de politica.
  # Los NA del enmascarado se conservan para que la guarda de politica oculta
  # los siga viendo.
  valores <- perfil$meta$sentinelas_numericos
  if (is.null(valores) || all(is.na(valores))) return(valores)
  sort(unique(valores), na.last = TRUE)
}

.rango_perfil <- function(fila) {
  if (is.finite(fila$minimo) && is.finite(fila$maximo)) {
    return(list(
      tipo = "numerico", minimo = fila$minimo, maximo = fila$maximo,
      texto = paste0("[", .texto_deriva(fila$minimo), ", ",
                     .texto_deriva(fila$maximo), "]")
    ))
  }
  if (!is.na(fila$minimo_fecha) && !is.na(fila$maximo_fecha)) {
    limites <- tryCatch(
      suppressWarnings(as.POSIXct(
        c(fila$minimo_fecha, fila$maximo_fecha), tz = "UTC"
      )),
      error = function(e) as.POSIXct(c(NA, NA), tz = "UTC")
    )
    if (length(limites) != 2L || anyNA(limites)) return(NULL)
    a <- as.numeric(limites[[1L]])
    b <- as.numeric(limites[[2L]])
    return(list(
      tipo = "fecha", minimo = a, maximo = b,
      texto = paste0("[", fila$minimo_fecha, ", ", fila$maximo_fecha, "]")
    ))
  }
  NULL
}

.magnitud_rango <- function(anterior, actual) {
  if (is.null(anterior) || is.null(actual) ||
      anterior$tipo != actual$tipo) return(Inf)
  ancho <- abs(anterior$maximo - anterior$minimo)
  escala <- if (is.finite(ancho) && ancho > 0) ancho else {
    max(abs(c(anterior$minimo, anterior$maximo)), 1)
  }
  max(
    abs(actual$minimo - anterior$minimo),
    abs(actual$maximo - anterior$maximo)
  ) / escala
}

.patrones_perfil <- function(perfil, indice) {
  x <- perfil$patrones[[indice]]
  if (!inherits(x, "data.frame") || !all(c("patron", "proporcion") %in% names(x))) {
    return(data.frame(
      patron = character(), proporcion = numeric(), stringsAsFactors = FALSE
    ))
  }
  x[c("patron", "proporcion")]
}

.clave_patron <- function(x) {
  valores <- ifelse(is.na(x), "<NA>", as.character(x))
  .clave_bytes(valores)
}

# Los diagnosticos que el perfil declino, como `columna tipo`, para poder
# distinguir un hallazgo resuelto de uno que no se volvio a evaluar.
# Un diagnostico tambien deja de evaluarse cuando desaparece la DECLARACION que
# lo habilitaba, y eso no queda en `cobertura_diagnosticos`: no es que el
# diagnostico se haya declinado sobre una columna, es que nunca se le pidio.
#
# Medido el 2026-09-05: la misma tabla perfilada con `clave = "id"` y despues sin
# `clave` informaba `clave_no_unica` como **resuelto, severidad ok**. La clave
# seguia estando duplicada; lo unico que cambio fue el argumento. El propio
# `man/comparar_perfiles.Rd` lo declara al reves de lo que pasaba: "dejar de
# mirar no es lo mismo que arreglar".
#
# Se mira `meta$declaracion_clave`, que se agrego para esto: `meta$clave` guarda el
# RESULTADO de comprobar la clave y queda `NULL` cuando sale limpia, asi que no
# distingue "no se declaro" de "se declaro y estaba bien". `clave` es la unica
# declaracion que HABILITA hallazgos propios -medido: `clave_no_unica` y
# `clave_con_ausentes` aparecen con ella y no sin ella-; el resto de los
# argumentos cambia como se mide, no si se mide.
.hallazgos_por_declaracion <- list(
  declaracion_clave = c("clave_no_unica", "clave_con_ausentes")
)

# Diagnosticos que comparan valores ENTRE SI y que solo corren sobre texto.
#
# Cuando una columna pasa de texto a numero, los diagnosticos de texto dejan de
# correr sobre ella. Para la mayoria eso es una resolucion de verdad: un numero
# no puede tener espacios sobrantes, mayusculas inconsistentes ni controles
# invisibles, asi que el problema dejo de existir. Pero un diagnostico que
# mira la RELACION entre valores no desaparece con el tipo: `1200` y `1201`
# siguen ahi despues de convertirlos, solo que nadie los volvio a comparar.
#
# Medido el 2026-09-21: el plan recomendado convertia `"1200%"`/`"1201%"` a
# numero -ninguna accion atendia los casi duplicados- y la deriva informaba
# `casi_duplicados_vocabulario` como **resuelto, severidad ok**.
#
# La direccion opuesta no tiene este problema, y se midio: una columna que pasa
# de numero a texto se vuelve a inferir como numerica y `outliers` sigue
# corriendo.
.diagnosticos_relacion_textual <- c("casi_duplicados_vocabulario")

# Los hallazgos de esos diagnosticos, en columnas que en el perfil nuevo ya no
# son texto, como `columna tipo`.
.declinados_por_cambio_de_tipo <- function(anterior, actual) {
  columnas_a <- anterior$columnas
  columnas_b <- actual$columnas
  if (!inherits(columnas_a, "data.frame") || !inherits(columnas_b, "data.frame") ||
      !all(c("columna", "tipo_declarado") %in% names(columnas_a)) ||
      !all(c("columna", "tipo_declarado") %in% names(columnas_b))) {
    return(character())
  }
  es_texto <- function(tipo) {
    tolower(as.character(tipo)) %in% c("texto", "character", "factor", "categoria")
  }
  nombres_a <- .nombres_para_operar(columnas_a$columna)
  nombres_b <- .nombres_para_operar(columnas_b$columna)
  comunes <- intersect(nombres_a, nombres_b)
  dejaron <- comunes[
    es_texto(columnas_a$tipo_declarado[match(comunes, nombres_a)]) &
      !es_texto(columnas_b$tipo_declarado[match(comunes, nombres_b)])
  ]
  if (!length(dejaron)) return(character())
  as.vector(outer(dejaron, .diagnosticos_relacion_textual, paste))
}

.declinados_por_declaracion_retirada <- function(anterior, actual) {
  declinados <- character()
  for (declaracion in names(.hallazgos_por_declaracion)) {
    tenia <- length(anterior$meta[[declaracion]]) > 0L
    tiene <- length(actual$meta[[declaracion]]) > 0L
    if (tenia && !tiene) {
      declinados <- c(
        declinados,
        paste("<tabla>", .hallazgos_por_declaracion[[declaracion]])
      )
    }
  }
  declinados
}

.clave_declinacion_deriva <- function(columna, tipo) {
  paste(.nombres_para_operar(columna), as.character(tipo))
}


# Lee un campo numerico de la fila de una columna, tolerando que no exista -un
# perfil de otra version puede no traerlo-.
.valor_columna_deriva <- function(fila, campo) {
  if (is.null(fila) || !campo %in% names(fila)) return(NA_real_)
  suppressWarnings(as.numeric(fila[[campo]][[1L]]))
}

# Un perfil guardado por otra version de lupa puede no traer un campo que esta
# version compara. Medido antes de arreglarlo: sin `tasa_distintos`, sin
# `prop_faltantes_totales`, sin `minimo` o sin `maximo`, comparar_perfiles()
# abortaba con el mensaje interno de R -"valor ausente donde TRUE/FALSE es
# necesario"-, que no le dice a nadie que el problema es la version del perfil.
# En otros dos campos era peor que abortar: sin `tipo_inferido` publicaba una
# fila de cambio "modificado" atribuida a los datos, cuando lo que habia
# cambiado era la forma del perfil; sin `n_distintos` la evidencia salia con el
# valor anterior en blanco.
#
# Se compara la interseccion y lo ausente se declara, que es el mismo mecanismo
# que ya usan la politica de patrones, la de centinelas y la de aplicabilidad.
# Los nombres de los grupos son los `aspecto` que ya publica la comparacion, asi
# que la fila de no comparabilidad cae junto a las filas que reemplaza.
.campos_comparables_deriva <- function(anterior, actual) {
  requeridos <- list(
    tipo_declarado = "tipo_declarado",
    tipo_inferido = "tipo_inferido",
    faltantes = "prop_faltantes_totales",
    cardinalidad = c("tasa_distintos", "n_distintos"),
    rango = c("minimo", "maximo", "minimo_fecha", "maximo_fecha")
  )
  nombres_a <- names(anterior$columnas)
  nombres_b <- names(actual$columnas)
  ausentes <- lapply(requeridos, function(campos) {
    list(
      anterior = setdiff(campos, nombres_a),
      actual = setdiff(campos, nombres_b)
    )
  })
  falta_alguno <- vapply(
    ausentes,
    function(x) length(x$anterior) > 0L || length(x$actual) > 0L,
    logical(1)
  )
  list(
    comparables = names(requeridos)[!falta_alguno],
    ausentes = ausentes[falta_alguno]
  )
}

.diagnosticos_declinados_deriva <- function(perfil) {
  cobertura <- perfil$cobertura_diagnosticos
  if (!inherits(cobertura, "data.frame") || !nrow(cobertura) ||
        !all(c("columna", "diagnostico") %in% names(cobertura))) {
    return(character())
  }
  .clave_declinacion_deriva(
    as.character(cobertura$columna), as.character(cobertura$diagnostico)
  )
}

.resumir_hallazgos_deriva <- function(perfil) {
  x <- perfil$hallazgos
  if (!nrow(x)) {
    return(data.frame(
      clave = character(), columna = character(), tipo_hallazgo = character(),
      severidad = character(), evidencia = character(), stringsAsFactors = FALSE
    ))
  }
  columna <- ifelse(is.na(x$columna), "<tabla>", as.character(x$columna))
  clave <- .clave_bytes(paste(
    .clave_bytes(columna), .clave_bytes(x$tipo_hallazgo), sep = "\034"
  ))
  grupos <- split(seq_len(nrow(x)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    nivel <- match(as.character(x$severidad[indices]),
                   c("ok", "sospechoso", "error"))
    elegido <- indices[which.max(nivel)]
    data.frame(
      clave = clave[[elegido]], columna = columna[[elegido]],
      tipo_hallazgo = x$tipo_hallazgo[[elegido]],
      severidad = as.character(x$severidad[[elegido]]),
      evidencia = x$evidencia[[elegido]], stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, unname(partes))
  rownames(resultado) <- NULL
  resultado
}

# La regla de "alcanza el umbral", escrita UNA vez. Estaba escrita de dos
# maneras en este mismo archivo: los faltantes y la cardinalidad comparaban con
# tolerancia y el rango y los patrones sin ella, asi que dos pares que publican
# la misma magnitud recibian veredictos distintos segun el aspecto. La
# documentacion publica un unico umbral para todos.
#
# Dos condiciones, y la primera importa tanto como la segunda:
#   - una magnitud por debajo del ruido de la coma flotante NO es un cambio,
#     por chico que sea el umbral. Sin esto, un umbral menor que la tolerancia
#     hacia que un delta CERO entrara por la rama de "significativo".
#   - y el umbral se compara con tolerancia, porque 0,70 - 0,65 da
#     0,049999999999999933 y 0,75 - 0,70 da 0,050000000000000044.
.alcanza_umbral_deriva <- function(magnitud, umbral) {
  # La tolerancia es RELATIVA al umbral: absorbe el error de la resta -0,70
  # menos 0,65 da 0,049999999999999933- sin mover el corte cuando el umbral es
  # chico o cero.
  # Escala con las MAGNITUDES en juego, no con un piso de 1: el error de la
  # resta es del orden de `eps * max(|a|, |b|)`, y un piso fijo se traga
  # cambios reales cuando el umbral es diminuto.
  tolerancia <- sqrt(.Machine$double.eps) * max(abs(magnitud), abs(umbral))
  if (length(magnitud) != 1L || is.na(magnitud)) return(FALSE)
  if (is.infinite(magnitud)) return(TRUE)
  # Una diferencia EXACTAMENTE nula no es un cambio, con ningun umbral. Eso es
  # lo unico que hay que excluir: la primera version usaba un piso ABSOLUTO
  # -`abs(magnitud) > sqrt(.Machine$double.eps)`- y con eso borraba cambios
  # legitimamente diminutos, como un 1e-10 con `umbral = 0`, que la API deja
  # pedir. La tolerancia esta para estabilizar la comparacion CONTRA EL CORTE,
  # no para redefinir como cero toda magnitud pequena.
  if (magnitud == 0) return(FALSE)
  abs(magnitud) >= umbral - tolerancia
}

#' Comparar dos perfiles y detectar deriva estructural
#'
#' Compara entregas sin exigir que tengan las mismas columnas. Devuelve cambios
#' de esquema, tipos, faltantes, cardinalidad, rango, patrones y hallazgos como
#' un objeto de datos filtrable.
#'
#' @param anterior,actual Objetos producidos por [perfilar()].
#' @param umbral_cambio Diferencia mínima para considerar significativo un
#'   cambio de proporción, cardinalidad o rango relativo. Cinco puntos
#'   porcentuales evita elevar variaciones pequeñas a hallazgo.
#' @param umbral_error Diferencia a partir de la cual un aumento de faltantes o
#'   un patrón nuevo se clasifica como `error`.
#'
#' @return Data frame `deriva_perfil`. `severidad` usa el factor ordenado
#'   `ok < sospechoso < error`; los cambios menores permanecen como filas `ok`
#'   para que la serie sea exportable sin ocultar diferencias.
#'
#' @details
#' Los patrones se comparan sobre el resumen acotado que conserva cada perfil,
#' no sobre los valores originales ni una distribución completa. Si las dos
#' corridas usaron configuraciones de patrones diferentes, se informa un error
#' de comparabilidad y esa parte de la comparación se omite.
#'
#' Los nombres de columnas y las configuraciones que contienen texto del usuario
#' se comparan por sus bytes, no por la marca de codificación ni por la
#' intercalación del locale.
#'
#' Eso alcanza mientras los bytes lleguen intactos, y hay un caso en que no
#' llegan y **no depende de este paquete**: el formato RDS 3 —el de [saveRDS()]
#' por omisión— anota la codificación nativa de quien escribe, y al leer
#' *traduce* desde ella el texto que no declara la suya. Un perfil guardado bajo
#' `LC_CTYPE=C` y releído bajo otro locale puede volver con el texto cambiado;
#' en Windows el cambio es silencioso, porque ahí la conversión no falla.
#'
#' Cuando eso pasa, el perfil releído **está** alterado, y conviene saber qué
#' alcanza a ver esta función. Compara las **propiedades medidas** de cada
#' columna, los hallazgos, el alcance del resumen y las configuraciones
#' declaradas —no los valores uno por uno—: la columna `aspecto` del resultado
#' nombra, fila por fila, qué se comparó, y es la enumeración viva. **Un cambio de texto que no
#' altera ninguna propiedad medida no aparece**: por ejemplo, si la moda de una
#' columna pasa de `"Basico"` a `"Casico"` —mismo largo, misma forma de patrón,
#' mismos conteos—, la comparación devuelve cero filas. Por eso el remedio ante
#' una relectura sospechosa no es confiar en la comparación para detectarla.
#'
#' El remedio es una línea, `saveRDS(perfil, archivo, version = 2)`, porque ese
#' formato no anota codificación nativa y por lo tanto no traduce nada. La
#' persistencia propia del paquete —[guardar_historico()] y
#' [guardar_analisis()]— ya lo usa.
#' Las claves internas que agrupan hallazgos también se normalizan por bytes;
#' por eso dos perfiles idénticos no emiten avisos espurios bajo C.
#'
#' Las columnas que aparecen o desaparecen generan cambios estructurales de
#' severidad `error`, pero no impiden comparar las columnas compartidas. Un
#' hallazgo de una columna retirada no se presenta como resuelto.
#'
#' Un hallazgo que ya no aparece se informa como `resuelto` sólo si el
#' diagnóstico volvió a evaluarse. Si el perfil nuevo lo declinó —y lo dice en
#' su `cobertura_diagnosticos`—, el cambio se informa como `no_evaluado` con
#' severidad `sospechoso`, porque no se sabe si el hallazgo sigue: dejar de
#' mirar no es lo mismo que arreglar.
#'
#' @export
#' @seealso [perfilar()], [detectar_deriva_calidad()], [reportar()],
#'   [comparar_equivalencia()]
#'
#' @examples
#' anterior <- perfilar(data.frame(codigo = c("AA1", "AA2")),
#'                      fecha = as.POSIXct("2026-01-01", tz = "UTC"))
#' actual <- perfilar(data.frame(codigo = c("AA1", "B-2"), nueva = 1:2),
#'                    fecha = as.POSIXct("2026-02-01", tz = "UTC"))
#' comparar_perfiles(anterior, actual)
comparar_perfiles <- function(anterior, actual, umbral_cambio = 0.05,
                              umbral_error = 0.20) {
  anterior <- .validar_perfil_deriva(anterior, "anterior")
  actual <- .validar_perfil_deriva(actual, "actual")
  umbrales <- c(umbral_cambio, umbral_error)
  if (!is.numeric(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      umbral_error < umbral_cambio) {
    stop(
      "Los umbrales deben estar en [0, 1] y `umbral_error` no puede ser menor.",
      call. = FALSE
    )
  }
  fecha_anterior <- .fecha_utc(anterior$meta$fecha_hora)
  fecha_actual <- .fecha_utc(actual$meta$fecha_hora)
  mapa_a <- .mapa_columnas_perfil(anterior)
  mapa_b <- .mapa_columnas_perfil(actual)
  cambios <- list()
  k <- 0L
  agregar <- function(columna, aspecto, cambio, severidad, valor_anterior,
                      valor_actual, delta = NA_real_, cambio_relativo = NA_real_,
                      significativo = TRUE, descripcion, evidencia = "") {
    k <<- k + 1L
    original <- if (length(columna) && !is.na(columna)) {
      indice <- match(columna, c(mapa_a$clave, mapa_b$clave))
      if (!is.na(indice)) {
        fuente <- c(mapa_a$original, mapa_b$original)
        fuente[[indice]]
      } else columna
    } else columna
    cambios[[k]] <<- data.frame(
      columna = original, aspecto = aspecto, cambio = cambio,
      severidad = severidad, valor_anterior = .texto_deriva(valor_anterior),
      valor_actual = .texto_deriva(valor_actual), delta = delta,
      cambio_relativo = cambio_relativo, significativo = significativo,
      fecha_anterior = fecha_anterior, fecha_actual = fecha_actual,
      descripcion = descripcion, evidencia = evidencia,
      stringsAsFactors = FALSE
    )
  }

  campos <- .campos_comparables_deriva(anterior, actual)
  for (aspecto in names(campos$ausentes)) {
    lados <- campos$ausentes[[aspecto]]
    texto_lado <- function(x) {
      if (!length(x)) "los trae todos" else paste(x, collapse = ", ")
    }
    agregar(
      NA_character_, aspecto, "no_comparable", "error",
      texto_lado(lados$anterior), texto_lado(lados$actual),
      descripcion = paste0(
        "Uno de los dos perfiles no trae los campos que esta versi\u00f3n necesita ",
        "para comparar ", gsub("_", " ", aspecto, fixed = TRUE),
        "; esa parte no se compara."
      ),
      evidencia = paste0(
        "Campos ausentes en el perfil anterior: ", texto_lado(lados$anterior),
        "; en el actual: ", texto_lado(lados$actual),
        ". Suele pasar al comparar contra un perfil guardado por otra ",
        "versi\u00f3n de lupa."
      )
    )
  }

  desaparecidas <- setdiff(mapa_a$clave, mapa_b$clave)
  aparecidas <- setdiff(mapa_b$clave, mapa_a$clave)
  for (columna in desaparecidas) {
    agregar(
      columna, "columna", "desaparecida", "error", columna, NA_character_,
      descripcion = "La columna desapareci\u00f3 de la entrega actual.",
      evidencia = "No se comparan sus m\u00e9tricas, patrones ni rango."
    )
  }
  for (columna in aparecidas) {
    agregar(
      columna, "columna", "aparecida", "error", NA_character_, columna,
      descripcion = "Apareci\u00f3 una columna que no estaba en la entrega anterior.",
      evidencia = "No existe una base anterior para sus m\u00e9tricas."
    )
  }

  comunes <- intersect(mapa_a$clave, mapa_b$clave)
  for (columna in comunes) {
    indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
    indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
    a <- anterior$columnas[indice_a, , drop = FALSE]
    b <- actual$columnas[indice_b, , drop = FALSE]
    tipos_comparables <- intersect(
      c("tipo_declarado", "tipo_inferido"), campos$comparables
    )
    for (campo in tipos_comparables) {
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        agregar(
          columna, campo, "modificado", "error", a[[campo]], b[[campo]],
          descripcion = paste0(
            "Cambi\u00f3 el ", gsub("_", " ", campo, fixed = TRUE),
            " de la columna."
          )
        )
      }
    }
    for (especificacion in list(
      c(campo = "prop_faltantes_totales", aspecto = "faltantes"),
      c(campo = "tasa_distintos", aspecto = "cardinalidad")
    )) {
      campo <- especificacion[["campo"]]
      aspecto <- especificacion[["aspecto"]]
      if (!aspecto %in% campos$comparables) next
      if (.distinto_deriva(a[[campo]], b[[campo]])) {
        delta <- b[[campo]] - a[[campo]]
        # Con tolerancia, como `detectar_deriva_calidad()` en `historico.R` y
        # `tablero-calidad.R` con los pesos. Sin ella la resta en coma flotante
        # decide el veredicto: 0,65 -> 0,70 da 0,049999999999999933 y
        # 0,70 -> 0,75 da 0,050000000000000044, asi que dos pares que publican
        # el mismo `delta = 0,05` recibian veredictos distintos. El comentario
        # de `historico.R` ya decia que compararlo con tolerancia en un archivo
        # y sin ella en otro no es una decision sino una inconsistencia, y este
        # archivo era el otro.
        significativo <- .alcanza_umbral_deriva(delta, umbral_cambio)
        severidad <- if (!significativo || delta < 0) {
          "ok"
        } else if (aspecto == "faltantes" &&
                   .alcanza_umbral_deriva(delta, umbral_error)) {
          "error"
        } else {
          "sospechoso"
        }
        evidencia <- if (aspecto == "cardinalidad") {
          paste0(
            "Valores distintos: ", a$n_distintos, " -> ", b$n_distintos,
            "; tasas: ", .formatear_decimal_publicado(a[[campo]], 4L),
            " -> ", .formatear_decimal_publicado(b[[campo]], 4L)
          )
        } else {
          paste0(
            "Cambio de ", .formatear_decimal_publicado(delta, 4L, signo = TRUE),
            " en escala [0, 1]."
          )
        }
        descripcion <- if (aspecto == "faltantes") {
          "Cambi\u00f3 la proporci\u00f3n de faltantes de la columna."
        } else {
          "Cambi\u00f3 la tasa de valores distintos de la columna."
        }
        agregar(
          columna, aspecto, "modificado", severidad, a[[campo]], b[[campo]],
          delta = delta, significativo = significativo,
          descripcion = descripcion,
          evidencia = evidencia
        )
      }
    }
    # Dos resumenes calculados sobre subconjuntos distintos no son comparables
    # sin decirlo. Medido: la misma columna, una vez numerica y otra como texto
    # con un valor que no convierte, producia una fila "Cambio el rango
    # observado de la columna" -[10, 100] contra [10, 90]- atribuida a los
    # datos, cuando el 100 seguia ahi y lo que cambio fue que quedo fuera del
    # resumen.
    #
    # Es el mismo mecanismo que ya declara la comparabilidad de la politica de
    # centinelas y de la de aplicabilidad, una escala mas abajo: por columna, y
    # no por corrida.
    excluidos_a <- .valor_columna_deriva(a, "n_valores_excluidos_resumen")
    excluidos_b <- .valor_columna_deriva(b, "n_valores_excluidos_resumen")
    if (isTRUE(is.finite(excluidos_a)) && isTRUE(is.finite(excluidos_b)) &&
        !identical(excluidos_a, excluidos_b)) {
      agregar(
        columna, "alcance_resumen", "modificado", "sospechoso",
        as.character(excluidos_a), as.character(excluidos_b),
        descripcion = paste(
          "Los dos resumenes se calcularon sobre distinta cantidad de valores,",
          "asi que las diferencias de media, rango o cardinalidad pueden venir",
          "del alcance y no de los datos."
        ),
        evidencia = paste0(
          "Valores excluidos del resumen: ", excluidos_a, " antes, ",
          excluidos_b, " ahora."
        )
      )
    }
    rango_a <- if ("rango" %in% campos$comparables) .rango_perfil(a) else NULL
    rango_b <- if ("rango" %in% campos$comparables) .rango_perfil(b) else NULL
    if ((!is.null(rango_a) || !is.null(rango_b)) &&
        (is.null(rango_a) || is.null(rango_b) ||
         rango_a$texto != rango_b$texto || rango_a$tipo != rango_b$tipo)) {
      magnitud <- .magnitud_rango(rango_a, rango_b)
      significativo <- .alcanza_umbral_deriva(magnitud, umbral_cambio)
      agregar(
        columna, "rango", "modificado",
        if (significativo) "sospechoso" else "ok",
        if (is.null(rango_a)) NA_character_ else rango_a$texto,
        if (is.null(rango_b)) NA_character_ else rango_b$texto,
        cambio_relativo = magnitud, significativo = significativo,
        descripcion = "Cambi\u00f3 el rango observado de la columna."
      )
    }
  }

  config_a <- .configuracion_patrones_perfil(anterior)
  config_b <- .configuracion_patrones_perfil(actual)
  patrones_comparables <- !is.null(config_a) && !is.null(config_b) &&
    isTRUE(all.equal(config_a, config_b, check.attributes = FALSE))
  if (!patrones_comparables) {
    agregar(
      NA_character_, "configuracion_patrones", "no_comparable", "error",
      paste(unlist(config_a), collapse = "; "),
      paste(unlist(config_b), collapse = "; "),
      descripcion = paste0(
        "Las corridas no declaran la misma configuraci\u00f3n de patrones; ",
        "sus patrones no se comparan."
      )
    )
  } else {
    for (columna in comunes) {
      indice_a <- mapa_a$indice[match(columna, mapa_a$clave)]
      indice_b <- mapa_b$indice[match(columna, mapa_b$clave)]
      pa <- .patrones_perfil(anterior, indice_a)
      pb <- .patrones_perfil(actual, indice_b)
      claves_a <- .clave_patron(pa$patron)
      claves_b <- .clave_patron(pb$patron)
      # La clave APAREA; lo que se publica es el patron tal como lo publica el
      # perfil. Publicando la clave, la fila de deriva sacaba el patron con la
      # barra invertida duplicada -el escape que hace inyectiva a la clave- y
      # no coincidia con el mismo patron en `perfil$patrones`. Contar y mostrar
      # son dos preguntas distintas, tambien aca.
      publicable <- function(clave, claves, tabla) {
        posicion <- match(clave, claves)
        if (is.na(posicion)) return(clave)
        as.character(tabla$patron[[posicion]])
      }
      for (patron in setdiff(claves_b, claves_a)) {
        proporcion <- pb$proporcion[match(patron, claves_b)]
        patron <- publicable(patron, claves_b, pb)
        agregar(
          columna, "patron", "aparecido",
          if (.alcanza_umbral_deriva(proporcion, umbral_error)) {
            "error"
          } else {
            "sospechoso"
          },
          NA_character_, patron, delta = proporcion, significativo = TRUE,
          descripcion = "Apareci\u00f3 un patr\u00f3n de formato nuevo.",
          evidencia = paste0(
            "Proporci\u00f3n actual: ",
            .formatear_decimal_publicado(proporcion, 4L)
          )
        )
      }
      for (patron in setdiff(claves_a, claves_b)) {
        proporcion <- pa$proporcion[match(patron, claves_a)]
        patron <- publicable(patron, claves_a, pa)
        agregar(
          columna, "patron", "desaparecido",
          if (.alcanza_umbral_deriva(proporcion, umbral_error)) {
            "error"
          } else {
            "sospechoso"
          },
          patron, NA_character_, delta = -proporcion, significativo = TRUE,
          descripcion = "Desapareci\u00f3 un patr\u00f3n de formato anterior.",
          evidencia = paste0(
            "Proporci\u00f3n anterior: ",
            .formatear_decimal_publicado(proporcion, 4L)
          )
        )
      }
    }
  }

  sentinelas_a <- .configuracion_sentinelas_perfil(anterior)
  sentinelas_b <- .configuracion_sentinelas_perfil(actual)
  # Una lista enteramente ausente no es una politica distinta: es una politica
  # OCULTA. La proteccion de datos personales reemplaza `meta$sentinelas_
  # numericos` por NA cuando no puede decidir que centinela pertenece a que
  # columna, y comparar un perfil protegido contra uno sin proteger daba
  # "Cambio la politica de centinelas numericos" con severidad `error` sobre
  # dos corridas que usaban exactamente la misma politica por omision.
  oculta <- function(x) !is.null(x) && length(x) && all(is.na(x))
  sentinelas_comparables <- !is.null(sentinelas_a) &&
    !is.null(sentinelas_b) &&
    isTRUE(all.equal(sentinelas_a, sentinelas_b, check.attributes = FALSE))
  # Dos listas enmascaradas del mismo modo comparan iguales y no hay nada que
  # declarar: la guarda es para cuando DIFIEREN y esa diferencia se explica por
  # el enmascarado, no por la politica.
  sentinelas_ocultos <- !sentinelas_comparables &&
    (oculta(sentinelas_a) || oculta(sentinelas_b))
  texto_configuracion <- function(x) {
    if (is.null(x)) return(NA_character_)
    if (oculta(x)) return("[pol\u00edtica protegida]")
    paste(unlist(x), collapse = "; ")
  }
  if (sentinelas_ocultos) {
    agregar(
      NA_character_, "configuracion_sentinelas_numericos", "no_comparable",
      "sospechoso", texto_configuracion(sentinelas_a),
      texto_configuracion(sentinelas_b),
      descripcion = paste(
        "Una de las dos corridas public\u00f3 su pol\u00edtica de centinelas",
        "enmascarada por la protecci\u00f3n de datos personales: no se puede",
        "saber si la pol\u00edtica cambi\u00f3."
      ),
      evidencia = paste(
        "No se afirma que haya cambiado. Para comparar la pol\u00edtica,",
        "correr las dos con la misma configuraci\u00f3n de protecci\u00f3n."
      )
    )
  } else if (!sentinelas_comparables) {
    agregar(
      NA_character_, "configuracion_sentinelas_numericos", "modificado",
      "error", texto_configuracion(sentinelas_a),
      texto_configuracion(sentinelas_b),
      descripcion = paste(
        "Cambi\u00f3 la pol\u00edtica de centinelas num\u00e9ricos; se mantienen las",
        "comparaciones para que las corridas siguientes sigan detectando",
        "deriva con la pol\u00edtica vigente."
      ),
      evidencia = paste(
        "Las diferencias de m\u00e9tricas o hallazgos pueden atribuirse a esta",
        "pol\u00edtica en la transici\u00f3n; revisar la configuraci\u00f3n publicada."
      )
    )
  }

  # Comparar un perfil protegido contra uno sin proteger publicaba los valores
  # que el lado protegido oculta, y la diferencia se leia como deriva del dato
  # cuando lo que cambio es la politica. No es una fuga -quien corre esa
  # comparacion ya tiene el perfil sin proteger- pero es la misma forma que ya
  # tienen `configuracion_umbrales` y `configuracion_normalizacion`: un cambio de
  # politica se declara con su propia fila en vez de disfrazarse de deriva.
  proteccion_a <- .proteccion_declarada_perfil(anterior)
  proteccion_b <- .proteccion_declarada_perfil(actual)
  if (!is.na(proteccion_a) && !is.na(proteccion_b) &&
      !identical(proteccion_a, proteccion_b)) {
    agregar(
      NA_character_, "configuracion_proteccion", "modificado", "error",
      if (proteccion_a) "protegido" else "sin proteger",
      if (proteccion_b) "protegido" else "sin proteger",
      descripcion = paste(
        "Un lado oculta los valores de las columnas personales y el otro no."
      ),
      evidencia = paste(
        "Los valores que aparecen de un solo lado pueden venir de esta",
        "diferencia de pol\u00edtica y no de los datos."
      )
    )
  }

  umbrales_a <- .configuracion_umbrales_perfil(anterior)
  umbrales_b <- .configuracion_umbrales_perfil(actual)
  if (!is.null(umbrales_a) && !is.null(umbrales_b) &&
      !identical(umbrales_a, umbrales_b)) {
    difieren <- names(umbrales_a)[
      !names(umbrales_a) %in% names(umbrales_b) |
        umbrales_a[names(umbrales_a)] !=
          umbrales_b[match(names(umbrales_a), names(umbrales_b))]
    ]
    difieren <- difieren[!is.na(difieren)]
    texto_umbral <- function(x, cuales) {
      paste(paste0(cuales, "=", x[cuales]), collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_umbrales", "modificado", "error",
      texto_umbral(umbrales_a, difieren), texto_umbral(umbrales_b, difieren),
      descripcion = paste(
        "Cambiaron los umbrales que deciden si un hallazgo se emite y con",
        "qu\u00e9 severidad."
      ),
      evidencia = paste(
        "Un hallazgo que aparece, desaparece o cambia de severidad entre las",
        "dos corridas puede venir de esta configuraci\u00f3n y no de los datos."
      )
    )
  }

  normalizacion_a <- .configuracion_normalizacion_perfil(anterior)
  normalizacion_b <- .configuracion_normalizacion_perfil(actual)
  if (!is.null(normalizacion_a) && !is.null(normalizacion_b) &&
      !isTRUE(all.equal(normalizacion_a, normalizacion_b,
                        check.attributes = FALSE))) {
    texto_normalizacion <- function(x) {
      activos <- names(x)[vapply(x, function(z) isTRUE(z[[1L]]), logical(1))]
      if (!length(activos)) "ninguno" else paste(activos, collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_normalizacion", "modificado", "error",
      texto_normalizacion(normalizacion_a),
      texto_normalizacion(normalizacion_b),
      descripcion = paste(
        "Cambi\u00f3 la normalizaci\u00f3n del texto, que decide qu\u00e9 valores se",
        "consideran el mismo."
      ),
      evidencia = paste(
        "Las diferencias de cardinalidad, variantes y duplicados pueden venir",
        "de esta pol\u00edtica y no de los datos; un hallazgo que desaparece al",
        "apagarla no es una mejora de la entrega."
      )
    )
  }

  # La misma declaracion de comparabilidad que ya existia para los centinelas,
  # para la tercera politica que redefine lo que se mide.
  aplicabilidad_a <- .configuracion_aplicabilidad_perfil(anterior)
  aplicabilidad_b <- .configuracion_aplicabilidad_perfil(actual)
  if (!is.null(aplicabilidad_a) && !is.null(aplicabilidad_b) &&
      !identical(aplicabilidad_a, aplicabilidad_b)) {
    texto_aplicabilidad <- function(x) {
      if (!length(x)) "ninguna" else paste(x, collapse = "; ")
    }
    agregar(
      NA_character_, "configuracion_aplicabilidad", "modificado",
      "error", texto_aplicabilidad(aplicabilidad_a),
      texto_aplicabilidad(aplicabilidad_b),
      descripcion = paste(
        "Cambiaron las columnas con regla de aplicabilidad; esa regla redefine",
        "el universo aplicable, asi que las diferencias de faltantes,",
        "cardinalidad y rango pueden ser del metodo y no de los datos."
      ),
      evidencia = paste(
        "Se mantienen las comparaciones para que las corridas siguientes sigan",
        "detectando deriva con la politica vigente."
      )
    )
  }

  hallazgos_a <- .resumir_hallazgos_deriva(anterior)
  hallazgos_b <- .resumir_hallazgos_deriva(actual)
  nuevos <- setdiff(hallazgos_b$clave, hallazgos_a$clave)
  resueltos <- setdiff(hallazgos_a$clave, hallazgos_b$clave)
  nombres_actuales <- .nombres_para_operar(unique(mapa_b$original))
  for (clave in nuevos) {
    x <- hallazgos_b[match(clave, hallazgos_b$clave), , drop = FALSE]
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo", "aparecido", x$severidad, NA_character_, x$tipo_hallazgo,
      descripcion = "Apareci\u00f3 un hallazgo que no estaba en el perfil anterior.",
      evidencia = x$evidencia
    )
  }
  # Un hallazgo que ya no esta puede haberse resuelto o puede no haberse
  # evaluado, y son cosas distintas: la primera es una mejora y la segunda es
  # una medicion que falta. Se distinguen mirando la cobertura del perfil nuevo,
  # que es donde el paquete declara lo que declino. Sin esta consulta se
  # informaba "resuelto" con severidad `ok` sobre un diagnostico que decia, en
  # esa misma tabla, "no se evaluaron los limites de Tukey".
  declinados_ahora <- c(
    .diagnosticos_declinados_deriva(actual),
    .declinados_por_declaracion_retirada(anterior, actual),
    .declinados_por_cambio_de_tipo(anterior, actual)
  )
  for (clave in resueltos) {
    x <- hallazgos_a[match(clave, hallazgos_a$clave), , drop = FALSE]
    if (x$columna != "<tabla>" &&
        !.nombres_para_operar(x$columna) %in% nombres_actuales) next
    clave_declinacion <- .clave_declinacion_deriva(
      x$columna, x$tipo_hallazgo
    )
    declinado <- clave_declinacion %in% declinados_ahora
    # Los dos motivos de no evaluacion mandan a lugares distintos, y decir el
    # equivocado es peor que no decir ninguno: quien busque en
    # `cobertura_diagnosticos` un diagnostico que nunca se pidio no va a
    # encontrar nada y va a concluir que el aviso esta de mas.
    por_declaracion <- clave_declinacion %in%
      .declinados_por_declaracion_retirada(anterior, actual)
    por_tipo <- clave_declinacion %in%
      .declinados_por_cambio_de_tipo(anterior, actual)
    agregar(
      if (x$columna == "<tabla>") NA_character_ else x$columna,
      "hallazgo",
      if (declinado) "no_evaluado" else "resuelto",
      if (declinado) "sospechoso" else "ok",
      x$tipo_hallazgo, NA_character_,
      descripcion = if (por_tipo) {
        paste(
          "La columna dej\u00f3 de ser texto y este diagn\u00f3stico compara valores",
          "de texto entre s\u00ed, as\u00ed que no se volvi\u00f3 a evaluar: los valores",
          "que lo dispararon pueden seguir ah\u00ed. Dejar de mirar no es lo mismo",
          "que arreglar."
        )
      } else if (por_declaracion) {
        paste(
          "El diagn\u00f3stico no se evalu\u00f3 en el perfil nuevo porque ya no se",
          "declar\u00f3 lo que lo habilita, as\u00ed que no se sabe si el hallazgo sigue:",
          "dejar de mirar no es lo mismo que arreglar."
        )
      } else if (declinado) {
        paste(
          "El diagn\u00f3stico no se evalu\u00f3 en el perfil nuevo, as\u00ed que no se sabe",
          "si el hallazgo sigue: el motivo est\u00e1 en `cobertura_diagnosticos`."
        )
      } else {
        "Un hallazgo del perfil anterior ya no est\u00e1 presente."
      },
      evidencia = x$evidencia
    )
  }
  compartidos <- intersect(hallazgos_a$clave, hallazgos_b$clave)
  for (clave in compartidos) {
    indice_a <- match(clave, hallazgos_a$clave)
    indice_b <- match(clave, hallazgos_b$clave)
    sa <- hallazgos_a$severidad[[indice_a]]
    sb <- hallazgos_b$severidad[[indice_b]]
    if (sa != sb) {
      columna <- hallazgos_b$columna[[indice_b]]
      agregar(
        if (columna == "<tabla>") NA_character_ else columna,
        "severidad_hallazgo",
        if (match(sb, c("ok", "sospechoso", "error")) >
            match(sa, c("ok", "sospechoso", "error"))) "agravado" else "atenuado",
        sb, sa, sb,
        descripcion = "Cambi\u00f3 la severidad de un hallazgo persistente.",
        evidencia = hallazgos_b$tipo_hallazgo[[indice_b]]
      )
    }
  }

  if (!length(cambios)) return(.deriva_perfil_vacia())
  resultado <- do.call(rbind, cambios)
  resultado$severidad <- factor(
    resultado$severidad, levels = c("ok", "sospechoso", "error"),
    ordered = TRUE
  )
  rownames(resultado) <- NULL
  class(resultado) <- c("deriva_perfil", "data.frame")
  resultado
}

# Registro fijo de los campos que el comparador puede interpretar. La clase
# almacenada de una columna no decide el eje: memoria y DBI publican algunos
# conteos con clases distintas, y el mismo campo debe conservar la misma regla.
# Si aparece una métrica nueva, se agrega explícitamente acá antes de compararla.
.registro_campos_equivalencia <- function() {
  list(
    flotante = c("media", "mediana", "desvio", "longitud_media"),
    exacto = c(
      "proporcion_tipo_inferido", "n_filas_analizadas_tipo", "n",
      "n_validos",
      "n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada",
      "n_presentes_fuera_de_aplicabilidad", "n_faltantes",
      "prop_faltantes", "n_faltantes_disfrazados",
      "n_faltantes_disfrazados_textuales", "n_faltantes_disfrazados_numericos",
      "prop_faltantes_disfrazados", "n_faltantes_totales",
      "prop_faltantes_totales", "n_distintos", "tasa_distintos",
      "secuencia_entera_densa", "densidad_secuencia_entera",
      "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
      "hueco_maximo_secuencia_entera", "salto_de_escala_secuencia_entera",
      "moda_sobresale_secuencia_entera",
      "umbral_densidad_secuencia_entera", "min_distintos_secuencia_entera",
      "frecuencia_moda", "longitud_minima", "longitud_maxima",
      "minimo", "maximo", "minimo_exacto", "maximo_exacto",
      "n_fechas_resumidas", "n_fechas_excluidas_granularidad",
      "n_valores_excluidos_resumen", "n_ceros",
      "n_negativos", "n_outliers", "centinela_repeticiones",
      "densidad_sin_centinela", "n_nan", "n_infinito_positivo",
      "n_infinito_negativo", "n_filas_fecha_civil_distinta_utc", "n_blancos",
      "n_espacios_borde", "n_variantes_mayusculas", "n_variantes_unicode",
      "n_codificacion_rota", "n_codificacion_reparable",
      "n_codificacion_reparable_parcialmente", "n_codificacion_irreparable",
      "n_codificacion_no_se_pudo", "n_codificacion_invalida",
      "n_controles_invisibles", "n_invisibles_eliminables",
      "n_espacios_invisibles", "n_invisibles_significativos",
      "n_entidades_html", "n_separadores_en_campo", "n_numeros_texto",
      "proporcion_numeros_texto"
    ),
    fecha = c(
      "minimo_fecha", "maximo_fecha", "media_fecha", "mediana_fecha"
    ),
    valor = c("moda", "centinela_valor")
  )
}

.campos_magnitud_equivalencia <- function(registro) {
  # Estos son los campos cuyo numero representa una magnitud de la columna.
  # Los conteos y proporciones quedan fuera: que la columna haya pasado de
  # fecha a numero no cambia la unidad de contar filas o valores distintos.
  unique(intersect(
    c(registro$flotante, "minimo", "maximo", "centinela_valor"),
    unlist(registro, use.names = FALSE)
  ))
}

.tipo_declarado_equivalencia <- function(columnas, indice) {
  # Los guardas de tipo consultan `.tipo_columna_equivalencia()`, que prefiere
  # el tipo *inferido*. Si el texto contiene numeros o fechas, esa inferencia
  # borra justo la diferencia de almacenamiento que el guarda existe para ver.
  # Aca se lee el tipo declarado, el unico que dice como estan guardados los
  # valores. Devuelve NA cuando el vocabulario no es el de memoria -- un tipo
  # SQL, una clase `sfc` -- y entonces ningun guarda de almacenamiento afirma
  # nada. El vocabulario es el que produce `.tipo_declarado()`, y vive aca una
  # sola vez para que los dos clasificadores de abajo no puedan divergir.
  if (!"tipo_declarado" %in% names(columnas)) return(NA_character_)
  valor <- as.character(columnas[["tipo_declarado"]][[indice]])
  if (length(valor) != 1L || is.na(valor) || !nzchar(trimws(valor))) {
    return(NA_character_)
  }
  tipo <- tolower(trimws(valor))
  conocidos <- c(
    "texto", "factor", "factor-ordenado", "doble", "entero", "logico",
    "fecha", "fecha-hora", "integer64", "lista", "matriz"
  )
  if (!tipo %in% conocidos) return(NA_character_)
  tipo
}

.nombre_tipo_motivo <- function(declarado, inferido) {
  # El motivo describe un cambio de TIPO, y quien lo lee entiende el tipo
  # declarado. `.tipo_columna_equivalencia()` devuelve el INFERIDO, asi que un
  # texto de "1","2","3" se nombraba `entero`: la pareja texto contra doble se
  # publicaba como `tipo_cambiado:entero_vs_doble`, donde ningun lado es entero.
  # Se prefiere el declarado y se cae al inferido solo cuando no se conoce -por
  # ejemplo con un tipo SQL-, que es mejor que no decir nada.
  if (!is.na(declarado)) return(declarado)
  if (!is.na(inferido)) return(inferido)
  "desconocido"
}

.almacenamiento_caracter_equivalencia <- function(columnas, indice) {
  tipo <- .tipo_declarado_equivalencia(columnas, indice)
  if (is.na(tipo)) return(NA)
  tipo %in% c("texto", "factor", "factor-ordenado")
}

.almacenamiento_con_zona_equivalencia <- function(columnas, indice) {
  # Solo `fecha-hora` lleva zona horaria. `fecha` no la tiene, y una fecha
  # escrita como texto tampoco: por eso el desfasaje se pierde al guardarla.
  tipo <- .tipo_declarado_equivalencia(columnas, indice)
  if (is.na(tipo)) return(NA)
  identical(tipo, "fecha-hora")
}

.campos_representacion_equivalencia <- function(registro) {
  # Estos son los campos cuyo numero se calcula sobre la *representacion* de
  # los valores y no sobre los valores. Cambian con solo cambiar el
  # almacenamiento, aunque el dato sea identico: medidos sobre pares del mismo
  # dato guardado de las dos formas -- doble, entero, fecha, logico, negativos,
  # notacion cientifica, factor y fecha-hora -- son los unicos que difieren
  # siempre. Los conteos de codificacion y de invisibles quedan fuera a
  # proposito: ahi una diferencia habla del dato, no de como se guarda.
  unique(intersect(
    c("longitud_minima", "longitud_maxima", "longitud_media",
      "n_variantes_unicode", "n_numeros_texto", "proporcion_numeros_texto"),
    unlist(registro, use.names = FALSE)
  ))
}

.campos_zona_horaria_equivalencia <- function(registro) {
  # Campos que solo puede medir un almacenamiento que lleva zona horaria. Del
  # otro lado no dan un valor distinto: no dan ninguno, porque no hay zona que
  # comparar contra UTC. Los extremos de fecha quedan FUERA a proposito: ahi la
  # diferencia es real -- al guardarse como texto la columna pierde la zona y
  # los instantes que denota son otros -- y es el hallazgo mas importante de
  # ese escenario.
  unique(intersect(
    c("n_filas_fecha_civil_distinta_utc", "fecha_civil_distinta_utc"),
    unlist(registro, use.names = FALSE)
  ))
}

.tipo_columna_equivalencia <- function(columnas, indice) {
  nombres <- c(
    "tipo_inferido", "clase_temporal", "tipo_temporal", "clase",
    "tipo_declarado"
  )
  nombres <- intersect(nombres, names(columnas))
  for (nombre in nombres) {
    valor <- as.character(columnas[[nombre]][[indice]])
    if (length(valor) == 1L && !is.na(valor) && nzchar(trimws(valor))) {
      return(tolower(trimws(valor)))
    }
  }
  NA_character_
}

.es_temporal_equivalencia <- function(columnas, indice) {
  tipo <- .tipo_columna_equivalencia(columnas, indice)
  if (is.na(tipo)) return(FALSE)
  tipo %in% c(
    "fecha", "fecha-hora", "fecha_hora", "date", "datetime", "timestamp",
    "posixct", "posixlt", "temporal"
  ) || grepl("^(fecha|date|datetime|timestamp|posix)", tipo)
}

.campos_protegidos_equivalencia_vacios <- function() {
  data.frame(
    columna = character(), campo = character(), lado = character(),
    stringsAsFactors = FALSE
  )
}

.detalle_proteccion_campo_equivalencia <- function(detalle, campo) {
  if (length(detalle) != 1L || is.na(detalle) || !nzchar(detalle)) {
    return(FALSE)
  }
  if (identical(campo, "moda") && grepl("moda", detalle, fixed = TRUE)) {
    return(TRUE)
  }
  if (campo %in% c("media", "media_fecha") &&
      grepl("momentos", detalle, fixed = TRUE)) {
    return(TRUE)
  }
  campos_orden <- c(
    "minimo", "maximo", "mediana", "minimo_exacto", "maximo_exacto",
    "minimo_fecha", "maximo_fecha", "mediana_fecha", "centinela_valor",
    "n_posiciones_secuencia_entera", "n_huecos_secuencia_entera",
    "hueco_maximo_secuencia_entera", "densidad_secuencia_entera",
    "densidad_sin_centinela"
  )
  campo %in% campos_orden && grepl("orden", detalle, fixed = TRUE)
}

.campo_protegido_equivalencia <- function(columnas, campo, indice) {
  protegido <- if ("dato_personal_protegido" %in% names(columnas)) {
    isTRUE(columnas$dato_personal_protegido[[indice]])
  } else {
    FALSE
  }
  valor <- .valor_equivalencia(columnas, campo, indice)
  if (protegido || (is.character(valor) && length(valor) == 1L &&
                    identical(valor, "[valor protegido]"))) {
    return(TRUE)
  }
  if (!.faltante_equivalencia(valor) ||
      !"detalle_proteccion_personal" %in% names(columnas)) {
    return(FALSE)
  }
  .detalle_proteccion_campo_equivalencia(
    columnas$detalle_proteccion_personal[[indice]], campo
  )
}

.columnas_equivalencia <- function(x, nombre) {
  if (inherits(x, "perfil")) {
    columnas <- x$columnas
  } else if (inherits(x, "perfil_dbi")) {
    columnas <- x$resumen_tabla$columnas
  } else if (is.data.frame(x)) {
    columnas <- x
  } else {
    stop(
      "`", nombre, "` debe ser un `perfil`, un `perfil_dbi` o un frame `columnas`.",
      call. = FALSE
    )
  }
  if (!is.data.frame(columnas) || !"columna" %in% names(columnas)) {
    stop(
      "`", nombre, "` no contiene un frame `columnas` con el campo `columna`.",
      call. = FALSE
    )
  }
  nombres <- as.character(columnas$columna)
  if (anyNA(nombres) || anyDuplicated(.nombres_para_operar(nombres))) {
    stop(
      "El frame `columnas` de `", nombre,
      "` debe tener nombres de columna presentes y sin duplicados.", call. = FALSE
    )
  }
  columnas
}

.valor_equivalencia <- function(columnas, campo, indice) {
  columnas[[campo]][[indice]]
}

.igualdad_equivalencia <- function(a, b) {
  resultado <- tryCatch(a == b, error = function(e) FALSE)
  isTRUE(resultado)
}

.faltante_equivalencia <- function(x) {
  resultado <- tryCatch(is.na(x), error = function(e) FALSE)
  isTRUE(resultado)
}

.infinito_equivalencia <- function(x) {
  if (length(x) != 1L || !is.numeric(x)) return(FALSE)
  isTRUE(is.infinite(x))
}

.comparar_valor_equivalencia <- function(a, b, tipo_eje, tolerancia) {
  if (length(a) != 1L || length(b) != 1L) {
    stop(
      "Cada valor de un campo comparable debe ser escalar.", call. = FALSE
    )
  }
  faltante_a <- .faltante_equivalencia(a)
  faltante_b <- .faltante_equivalencia(b)
  if (faltante_a && faltante_b) {
    return(list(
      veredicto = if (identical(a, b)) "identico" else "equivalente",
      motivo = if (identical(a, b)) "igualdad_exacta" else
        "faltante_misma_clase",
      diferencia_relativa = NA_real_
    ))
  }
  if (xor(faltante_a, faltante_b)) {
    return(list(
      veredicto = "materialmente_distinto", motivo = "faltante_un_lado",
      diferencia_relativa = NA_real_
    ))
  }
  if (.infinito_equivalencia(a) || .infinito_equivalencia(b)) {
    igual <- .igualdad_equivalencia(a, b)
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else "infinito",
      diferencia_relativa = NA_real_
    ))
  }

  igual <- .igualdad_equivalencia(a, b)
  if (tipo_eje != "flotante") {
    return(list(
      veredicto = if (igual) "identico" else "materialmente_distinto",
      motivo = if (igual) "igualdad_exacta" else paste0("eje_", tipo_eje),
      diferencia_relativa = NA_real_
    ))
  }
  if (!is.numeric(a) || !is.numeric(b) ||
      !isTRUE(is.finite(a)) || !isTRUE(is.finite(b))) {
    stop(
      "Los campos del eje `flotante` deben contener numeros finitos o faltantes.",
      call. = FALSE
    )
  }
  diferencia_relativa <- suppressWarnings(
    abs(a - b) / pmax(1, abs(a), abs(b))
  )
  if (igual) {
    return(list(
      veredicto = "identico", motivo = "igualdad_exacta",
      diferencia_relativa = as.numeric(diferencia_relativa)
    ))
  }
  dentro <- .dentro_tolerancia_aritmetica(a, b, tolerancia)
  list(
    veredicto = if (isTRUE(dentro)) "equivalente" else
      "materialmente_distinto",
    motivo = if (isTRUE(dentro)) "dentro_de_tolerancia" else
      "fuera_de_tolerancia",
    diferencia_relativa = as.numeric(diferencia_relativa)
  )
}

#' Comparar la equivalencia de dos resúmenes de perfiles
#'
#' Compara por intersección los campos registrados de dos perfiles y devuelve
#' una fila por cada par de columna y campo. Los campos que no tienen un eje
#' registrado no se comparan y quedan declarados en `campos_no_comparables`.
#' Los campos bajo protección tampoco se comparan: se declaran por columna,
#' campo y lado en `campos_protegidos`.
#'
#' @param anterior,actual Un objeto `perfil` de [perfilar()], un objeto
#'   `perfil_dbi` de [perfilar_dbi()] o directamente un frame `columnas`.
#' @param tolerancia Número escalar no negativo y finito, declarado por quien
#'   llama. No tiene valor por omisión y se publica en cada fila.
#'
#' @return Un frame de clase `equivalencia_perfiles` con `columna`, `campo`,
#'   `valor_anterior`, `valor_actual`, `diferencia_relativa`, `veredicto`,
#'   `motivo`, `tipo_eje` y `tolerancia`. `veredicto` es un factor ordenado con
#'   niveles `identico < equivalente < materialmente_distinto`. Los atributos
#'   `campos_no_comparables`, `detalle_campos_no_comparables`,
#'   `columnas_no_comparables`, `cobertura_diagnosticos`, `campos_protegidos`
#'   y `resumen` declaran, respectivamente, los campos omitidos, los motivos
#'   estructurales de esos campos, las columnas presentes en un solo lado o
#'   con tipos incompatibles, los diagnósticos que no se pudieron evaluar, los
#'   campos omitidos por protección y el conteo de cada veredicto.
#'   `campos_protegidos` es un data frame con las columnas `columna`, `campo` y
#'   `lado`; este último toma los valores `anterior` y `actual`.
#'
#' @details
#' El registro fijo asigna tolerancia sólo a `media`, `mediana`, `desvio` y
#' `longitud_media`. Los conteos, proporciones de conteos y extremos por
#' selección se comparan en el eje `exacto`; las fechas canónicas en `fecha` y
#' `moda` y `centinela_valor` en `valor`. Los ejes `exacto`, `fecha` y `valor`
#' son binarios por construcción.
#'
#' El comparador devuelve datos, no decisiones: no alimenta hallazgos,
#' severidades ni puntajes. La tolerancia es del llamador y jamás entra en una
#' regla del paquete; sólo se aplica al eje flotante finito y queda publicada
#' para que el llamador decida cómo usarla.
#'
#' Si una columna es temporal en exactamente uno de los perfiles, los campos
#' de magnitud numérica se omiten por el cambio de esquema y su motivo queda en
#' `detalle_campos_no_comparables` como
#' `tipo_cambiado:temporal_vs_no_temporal`. Así no se comparan duraciones en
#' segundos contra magnitudes numéricas sin unidad común. Cuando ambos lados
#' son temporales, `desvio` sí se compara en flotante porque ambas puertas lo
#' expresan en segundos.
#'
#' Si una columna guarda caracteres en exactamente uno de los perfiles, los
#' campos calculados sobre la representación —longitudes, variantes unicode y
#' números escritos como texto— se omiten y su motivo queda en
#' `detalle_campos_no_comparables` como `tipo_cambiado:texto_vs_no_texto`. Esos
#' campos cambian con sólo cambiar el almacenamiento, aunque el dato sea el
#' mismo, de modo que compararlos publicaría el síntoma y callaría la causa.
#' `factor` cuenta como almacenamiento de caracteres. Este guarda lee el tipo
#' *declarado* y no el inferido: si el texto contiene números o fechas, la
#' inferencia borra justo la diferencia que hay que ver. Cuando el tipo
#' declarado viene de un vocabulario ajeno al de memoria —un tipo SQL, por
#' ejemplo— no se afirma nada y la comparación sigue como siempre.
#'
#' Por la misma razón, si una columna lleva zona horaria en exactamente uno de
#' los perfiles, los campos que sólo ese almacenamiento puede medir se omiten
#' con el motivo `tipo_cambiado:con_zona_vs_sin_zona`. Del otro lado no dan un
#' valor distinto: no dan ninguno, porque no hay zona que comparar contra UTC.
#' Los extremos de fecha **sí** se siguen comparando: al guardarse como texto
#' la columna pierde la zona y los instantes que denota son realmente otros,
#' que es lo más importante que hay para informar en ese caso.
#'
#' @name comparar_equivalencia
#' @usage comparar_equivalencia(anterior, actual, tolerancia)
#' @export
#' @seealso [comparar_perfiles()], [perfilar()], [perfilar_dbi()]
#'
#' @examples
#' anterior <- data.frame(
#'   columna = "monto", media = 10, minimo = 1, moda = "a",
#'   stringsAsFactors = FALSE
#' )
#' actual <- data.frame(
#'   columna = "monto", media = 10.00000000001, minimo = 2, moda = "b",
#'   stringsAsFactors = FALSE
#' )
#' comparar_equivalencia(anterior, actual, tolerancia = 1e-9)
comparar_equivalencia <- function(anterior, actual, tolerancia) {
  if (!is.numeric(tolerancia) || length(tolerancia) != 1L ||
      is.na(tolerancia) || !is.finite(tolerancia) || tolerancia < 0) {
    stop(
      "`tolerancia` debe ser un escalar numerico finito, sin NA y mayor o igual a 0.",
      call. = FALSE
    )
  }
  tolerancia <- as.numeric(tolerancia)
  anterior <- .columnas_equivalencia(anterior, "anterior")
  actual <- .columnas_equivalencia(actual, "actual")
  mapa_anterior <- data.frame(
    clave = .nombres_unicos(.nombres_para_operar(anterior$columna)),
    original = as.character(anterior$columna),
    stringsAsFactors = FALSE
  )
  mapa_actual <- data.frame(
    clave = .nombres_unicos(.nombres_para_operar(actual$columna)),
    original = as.character(actual$columna),
    stringsAsFactors = FALSE
  )
  registro <- .registro_campos_equivalencia()
  campos_registrados <- unlist(registro, use.names = FALSE)
  campos_presentes <- unique(c(names(anterior), names(actual)))
  campos_no_comparables <- setdiff(
    setdiff(campos_presentes, "columna"), campos_registrados
  )
  campos <- intersect(
    setdiff(names(anterior), "columna"), setdiff(names(actual), "columna")
  )
  campos <- campos[campos %in% campos_registrados]
  columnas <- intersect(mapa_anterior$clave, mapa_actual$clave)
  campos_magnitud <- .campos_magnitud_equivalencia(registro)
  campos_representacion <- .campos_representacion_equivalencia(registro)
  campos_zona <- .campos_zona_horaria_equivalencia(registro)
  columnas_no_comparables <- data.frame(
    columna = character(), lado = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  detalle_campos_no_comparables <- data.frame(
    columna = character(), campo = character(), motivo = character(),
    stringsAsFactors = FALSE
  )
  cobertura_diagnosticos <- .cobertura_diagnosticos_vacia()
  registrar_no_comparable <- function(columna, lado, motivo, campo = NA_character_) {
    columnas_no_comparables <<- rbind(
      columnas_no_comparables,
      data.frame(columna = columna, lado = lado, motivo = motivo,
                 stringsAsFactors = FALSE)
    )
    detalle_campos_no_comparables <<- rbind(
      detalle_campos_no_comparables,
      data.frame(columna = columna, campo = campo, motivo = motivo,
                 stringsAsFactors = FALSE)
    )
    cobertura_diagnosticos <<- rbind(
      cobertura_diagnosticos,
      .nuevo_diagnostico_no_evaluado(
        "comparar_equivalencia", columna,
        paste0("no_comparable: ", motivo),
        "Asegurar que la columna exista en ambas corridas y conserve un tipo comparable."
      )
    )
  }
  solo_anterior <- setdiff(mapa_anterior$clave, mapa_actual$clave)
  solo_actual <- setdiff(mapa_actual$clave, mapa_anterior$clave)
  for (clave in solo_anterior) {
    columna <- mapa_anterior$original[match(clave, mapa_anterior$clave)]
    registrar_no_comparable(
      columna, "anterior", "columna_solo_en_anterior"
    )
  }
  for (clave in solo_actual) {
    columna <- mapa_actual$original[match(clave, mapa_actual$clave)]
    registrar_no_comparable(
      columna, "actual", "columna_solo_en_actual"
    )
  }
  campos_protegidos <- .campos_protegidos_equivalencia_vacios()
  niveles <- c("identico", "equivalente", "materialmente_distinto")
  salida <- list()
  k <- 0L
  for (clave in columnas) {
    indice_a <- match(clave, mapa_anterior$clave)
    indice_b <- match(clave, mapa_actual$clave)
    columna <- mapa_anterior$original[[indice_a]]
    temporal_a <- .es_temporal_equivalencia(anterior, indice_a)
    temporal_b <- .es_temporal_equivalencia(actual, indice_b)
    tipo_a <- .tipo_columna_equivalencia(anterior, indice_a)
    tipo_b <- .tipo_columna_equivalencia(actual, indice_b)
    declarado_a <- .tipo_declarado_equivalencia(anterior, indice_a)
    declarado_b <- .tipo_declarado_equivalencia(actual, indice_b)
    caracter_a <- .almacenamiento_caracter_equivalencia(anterior, indice_a)
    caracter_b <- .almacenamiento_caracter_equivalencia(actual, indice_b)
    caracter_cambiado <- !is.na(caracter_a) && !is.na(caracter_b) &&
      xor(caracter_a, caracter_b)
    zona_a <- .almacenamiento_con_zona_equivalencia(anterior, indice_a)
    zona_b <- .almacenamiento_con_zona_equivalencia(actual, indice_b)
    zona_cambiada <- !is.na(zona_a) && !is.na(zona_b) && xor(zona_a, zona_b)
    tipo_cambiado <- !is.na(tipo_a) && !is.na(tipo_b) &&
      !identical(tipo_a, tipo_b)
    for (campo in campos) {
      protegido_a <- .campo_protegido_equivalencia(anterior, campo, indice_a)
      protegido_b <- .campo_protegido_equivalencia(actual, campo, indice_b)
      if (protegido_a || protegido_b) {
        registros <- list()
        if (protegido_a) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "anterior",
            stringsAsFactors = FALSE
          )
        }
        if (protegido_b) {
          registros[[length(registros) + 1L]] <- data.frame(
            columna = columna, campo = campo, lado = "actual",
            stringsAsFactors = FALSE
          )
        }
        campos_protegidos <- rbind(campos_protegidos, do.call(rbind, registros))
        next
      }
      if (xor(temporal_a, temporal_b) && campo %in% campos_magnitud) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos", "tipo_cambiado:temporal_vs_no_temporal", campo
        )
        next
      }
      if (caracter_cambiado && campo %in% campos_representacion) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos", "tipo_cambiado:texto_vs_no_texto", campo
        )
        next
      }
      if (zona_cambiada && campo %in% campos_zona) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos", "tipo_cambiado:con_zona_vs_sin_zona", campo
        )
        next
      }
      tipo_eje <- names(registro)[vapply(
        registro, function(campos_eje) campo %in% campos_eje, logical(1L)
      )]
      a <- .valor_equivalencia(anterior, campo, indice_a)
      b <- .valor_equivalencia(actual, campo, indice_b)
      if (tipo_cambiado && campo %in% campos_magnitud &&
          .faltante_equivalencia(a) && .faltante_equivalencia(b)) {
        campos_no_comparables <- unique(c(campos_no_comparables, campo))
        registrar_no_comparable(
          columna, "ambos",
          paste0("tipo_cambiado:", .nombre_tipo_motivo(declarado_a, tipo_a),
                 "_vs_", .nombre_tipo_motivo(declarado_b, tipo_b)),
          campo
        )
        next
      }
      comparacion <- .comparar_valor_equivalencia(
        a, b, tipo_eje[[1L]], tolerancia
      )
      k <- k + 1L
      salida[[k]] <- list(
        columna = columna, campo = campo,
        valor_anterior = a, valor_actual = b,
        diferencia_relativa = comparacion$diferencia_relativa,
        veredicto = comparacion$veredicto, motivo = comparacion$motivo,
        tipo_eje = tipo_eje[[1L]], tolerancia = tolerancia
      )
    }
  }
  if (length(salida)) {
    resultado <- data.frame(
      columna = vapply(salida, `[[`, character(1L), "columna"),
      campo = vapply(salida, `[[`, character(1L), "campo"),
      # Se rinden para publicar: un valor declarado `bytes` dentro de una
      # columna-lista hacia que `print()` del resultado abortara con "width is
      # not computable in bytes encoding". La comparacion corria bien y el
      # objeto no se podia mostrar ni exportar; bastaba con que UNO de los dos
      # lados llevara la marca, aunque los bytes fueran identicos.
      valor_anterior = I(lapply(
        lapply(salida, `[[`, "valor_anterior"), .texto_publicable
      )),
      valor_actual = I(lapply(
        lapply(salida, `[[`, "valor_actual"), .texto_publicable
      )),
      diferencia_relativa = vapply(
        salida, `[[`, numeric(1L), "diferencia_relativa"
      ),
      veredicto = vapply(salida, `[[`, character(1L), "veredicto"),
      motivo = vapply(salida, `[[`, character(1L), "motivo"),
      tipo_eje = vapply(salida, `[[`, character(1L), "tipo_eje"),
      tolerancia = vapply(salida, `[[`, numeric(1L), "tolerancia"),
      stringsAsFactors = FALSE
    )
  } else {
    resultado <- data.frame(
      columna = character(), campo = character(),
      valor_anterior = I(list()), valor_actual = I(list()),
      diferencia_relativa = numeric(), veredicto = character(),
      motivo = character(), tipo_eje = character(), tolerancia = numeric(),
      stringsAsFactors = FALSE
    )
  }
  resultado$veredicto <- factor(
    resultado$veredicto, levels = niveles, ordered = TRUE
  )
  resumen <- stats::setNames(
    as.integer(table(factor(resultado$veredicto, levels = niveles))), niveles
  )
  attr(resultado, "campos_no_comparables") <- campos_no_comparables
  attr(resultado, "detalle_campos_no_comparables") <-
    detalle_campos_no_comparables
  columnas_no_comparables <- columnas_no_comparables[
    !duplicated(columnas_no_comparables), , drop = FALSE
  ]
  cobertura_diagnosticos <- cobertura_diagnosticos[
    !duplicated(cobertura_diagnosticos[c("diagnostico", "columna", "motivo")]),
    , drop = FALSE
  ]
  attr(resultado, "columnas_no_comparables") <- columnas_no_comparables
  attr(resultado, "cobertura_diagnosticos") <- cobertura_diagnosticos
  attr(resultado, "campos_protegidos") <- campos_protegidos
  attr(resultado, "resumen") <- resumen
  rownames(resultado) <- NULL
  class(resultado) <- c("equivalencia_perfiles", "data.frame")
  resultado
}

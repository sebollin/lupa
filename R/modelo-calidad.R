.es_texto_escalar <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.declaracion_metrica <- function(x) {
  attr(x, "declaracion", exact = TRUE)
}

.validar_tipo_resultado <- function(tipo_resultado) {
  opciones <- c("booleano", "real", "numero_real", "entero", "duracion")
  if (!.es_texto_escalar(tipo_resultado) || !tipo_resultado %in% opciones) {
    stop(
      "`tipo_resultado` debe ser 'booleano', 'real', 'numero_real', ",
      "'entero' o 'duracion'.", call. = FALSE
    )
  }
  tipo_resultado
}

.resultados_validos_tipo <- function(resultado, tipo) {
  if (!is.numeric(resultado) || length(resultado) != length(tipo) ||
      anyNA(resultado) || any(!is.finite(resultado))) return(FALSE)
  acotados <- tipo %in% c("booleano", "real")
  if (any(acotados & (resultado < 0 | resultado > 1))) return(FALSE)
  booleanos <- tipo == "booleano"
  if (any(booleanos & !resultado %in% c(0, 1))) return(FALSE)
  enteros <- tipo == "entero"
  if (any(enteros & (resultado < 0 | resultado != floor(resultado)))) {
    return(FALSE)
  }
  duraciones <- tipo == "duracion"
  if (any(duraciones & resultado < 0)) return(FALSE)
  tipos_validos <- c("booleano", "real", "numero_real", "entero", "duracion")
  all(tipo %in% tipos_validos)
}

.validar_propiedades_base <- function(configuracion, propiedades) {
  if (length(configuracion) &&
      (is.null(names(configuracion)) || any(!nzchar(names(configuracion))))) {
    stop("Todas las propiedades de configuraci\u00f3n deben tener nombre.", call. = FALSE)
  }
  desconocidas <- setdiff(names(configuracion), propiedades)
  if (length(desconocidas)) {
    stop(
      "Propiedades no declaradas: ", paste(desconocidas, collapse = ", "), ".",
      call. = FALSE
    )
  }
  faltantes <- setdiff(propiedades, names(configuracion))
  if (length(faltantes)) {
    stop(
      "Faltan propiedades de configuraci\u00f3n: ",
      paste(faltantes, collapse = ", "), ".",
      call. = FALSE
    )
  }
  configuracion
}

.crear_fabrica_especializacion <- function(declaracion, metodo, validador) {
  fabrica <- NULL
  propiedades <- declaracion$propiedades
  argumentos_lista <- stats::setNames(lapply(propiedades, as.name), propiedades)
  llamada_lista <- as.call(c(list(as.name("list")), argumentos_lista))
  cuerpo <- substitute({
    configuracion <- LLAMADA_LISTA
    configuracion <- configuracion[
      !vapply(configuracion, is.null, logical(1L))
    ]
    do.call(
      especializar,
      c(
        list(metrica = fabrica, nombre_especifico = nombre_especifico),
        configuracion
      )
    )
  }, list(LLAMADA_LISTA = llamada_lista))
  formales <- c(
    list(nombre_especifico = NULL),
    stats::setNames(rep(list(NULL), length(propiedades)), propiedades)
  )
  cierre <- eval(call("function", as.pairlist(formales), cuerpo), environment())
  attr(cierre, "declaracion") <- declaracion
  attr(cierre, "metodo_predeterminado") <- metodo
  attr(cierre, "validador_propiedades") <- validador
  class(cierre) <- c("metrica_generica", "function")
  fabrica <- cierre
  cierre
}

.crear_fabrica_instancia <- function(declaracion, nombre_especifico,
                                     configuracion, metodo) {
  fabrica <- NULL
  cierre <- function(entidad, atributos = character(), nombre_instancia = NULL,
                     metodo = NULL, referencial = NULL) {
    instanciar(
      fabrica, entidad = entidad, atributos = atributos,
      nombre_instancia = nombre_instancia, metodo = metodo,
      referencial = referencial
    )
  }
  attr(cierre, "declaracion") <- declaracion
  attr(cierre, "nombre_especifico") <- nombre_especifico
  attr(cierre, "configuracion") <- configuracion
  attr(cierre, "metodo_predeterminado") <- metodo
  class(cierre) <- c("metrica_especifica", "function")
  fabrica <- cierre
  cierre
}

#' Construir métricas y modelos de calidad
#'
#' `metrica()` declara una métrica genérica y devuelve una fábrica de closures.
#' `especializar()` fija sus propiedades de configuración; la closure específica
#' resultante puede instanciarse sobre distintas columnas o tablas.
#' `instanciar()` liga la métrica a objetos concretos y materializa el método de
#' medición. `modelo()` reúne métricas instanciadas sin calcular un índice global.
#'
#' `metricas_nucleo()` devuelve diecinueve métricas automatizables una vez
#' declaradas sus propiedades; [escala()] y [vigencia()] hacen explícitos los
#' insumos expertos que algunas necesitan. [metricas_referencial()] aporta por
#' separado las tres métricas que consumen un padrón tabular. Consulte
#' [catalogo_agesic()] para la correspondencia completa.
#'
#' @param nombre Nombre estable y legible.
#' @param semantica Descripción de lo que mide la métrica.
#' @param granularidad Uno de los niveles devueltos por `granularidades()`.
#' @param tipo_resultado `"booleano"`, `"real"` en `[0, 1]`, `"numero_real"`,
#'   `"entero"` no negativo o `"duracion"` no negativa. Los tres últimos
#'   conservan resultados no acotados del catálogo y no admiten las cuatro
#'   agregaciones normalizadas.
#' @param propiedades Nombres de las propiedades que fija `especializar()`.
#' @param dimension,factor Metadatos taxonómicos; no se usan para calcular
#'   puntuaciones.
#' @param metodo Método predeterminado opcional. Es una función de `tablas` e
#'   `instancia` que cumple el contrato descrito en Detalles.
#' @param validar_propiedades Función opcional que valida y devuelve la lista de
#'   configuración.
#' @param metrica Objeto de clase `metrica_generica`.
#' @param ... En `especializar()`, propiedades con nombre. En `modelo()`,
#'   métricas instanciadas o una única lista que las contenga.
#' @param entidad Nombres de las tablas ligadas, en el orden que espera el
#'   método.
#' @param atributos Nombres de las columnas ligadas, en el mismo orden.
#' @param referencial Objeto opcional creado por [referencial()]. Las métricas
#'   que lo consumen validan su contrato al medir.
#'
#' @return `metrica()` y `especializar()` devuelven closures S3;
#'   `instanciar()` devuelve una `metrica_instanciada`; `modelo()` devuelve un
#'   `modelo_calidad`; `metricas_nucleo()` devuelve una lista de métricas
#'   genéricas.
#'
#' @details
#' El método de medición devuelve un data frame con `resultado`, `entidad`,
#' `atributo`, `fila` y `objeto`. Una métrica booleana produce valores lógicos;
#' una real produce números finitos en `[0, 1]`.
#'
#' En `ReglaIntegridadInterEntidad`, `entidad` y `atributos` se ligan como
#' `c(referencia, dependiente)` y `c(clave_primaria, clave_foranea)`.
#' Pese a su nombre, `ErrorEstandar` sigue literalmente la semántica de la
#' tabla 16.5 del marco y devuelve la desviación estándar muestral sin
#' normalizar; exige al menos dos valores numéricos válidos. Por eso declara
#' `tipo_resultado = "numero_real"` y no admite [agregar()].
#'
#' `Formato` acepta exactamente una de las propiedades `expresion_regular`,
#' `diccionario` o `validador`. Esta última permite conectar validadores
#' externos sin incorporarlos como dependencias.
#' Tanto `Formato` como `ValoresPosiblesPorExtension` omiten los valores `NA`:
#' un ausente no genera una medida en esas métricas y, por lo tanto, tampoco
#' integra el denominador de sus agregaciones. La completitud se mide por
#' separado con `NoNulo`; así un mismo ausente no se penaliza en dos factores.
#' `NoNulo` acepta el vector opcional `valores_nulos` para aplicar de forma
#' deliberada el diccionario de nulos que contempla el marco; sin configurarlo,
#' sólo considera los `NA` reales.
#' `ValoresPosiblesPorComprension` sigue la misma convención y acepta un
#' `predicado` o un rango definido por `minimo`, `maximo` e `inclusivo`.
#'
#' Las métricas de duplicación marcan **todas** las apariciones que participan
#' en un grupo repetido, no sólo la segunda y siguientes. `AtributoDuplicado`
#' omite ausentes. `ConjuntoAtributosDuplicado` compara las columnas ligadas y
#' `EntidadDuplicada` compara la fila completa cuando se instancia sin
#' atributos. Si se ligan atributos de clave, sigue la semántica del marco:
#' marca filas con la misma clave cuyos demás valores son iguales o ausentes en
#' alguna de las dos. En las comparaciones exactas, los `NA` forman parte de la
#' combinación comparada.
#'
#' `DesactualizacionPorFormato` devuelve `TRUE` cuando el valor **no** cumple el
#' formato vigente. `OportunidadAtributoPorFecha` y
#' `OportunidadAtributoPorIntervalo` devuelven
#' `max(0, min(1, 1 - (t1 - t2) / (t3 - t2)))`. Exigen que `t3` sea posterior
#' a `t2`; un intervalo de duración cero se rechaza porque no define el
#' cociente. Se adopta un resultado real continuo para conservar cuánto margen
#' de utilidad queda, aunque las tablas 16.29 y 16.30 del marco lo declaran
#' booleano. Estas tres métricas omiten los valores `NA`: su ausencia corresponde
#' a completitud y no genera una segunda medida de incumplimiento.
#'
#' **Desviación documentada del marco:** en `DensidadPonderada`, un atributo
#' más crítico recibe un coeficiente mayor y, si falta, produce una penalización
#' mayor. El texto del marco indica acercar a cero el coeficiente de mayor
#' gravedad, lo que penalizaría menos el ausente crítico y contradice el sentido
#' de la ponderación. Los coeficientes deben estar en `[0, 1]` y sumar 1.
#'
#' `tipo_resultado` es el contrato canónico que consultan las agregaciones. Las
#' unidades no forman parte de este núcleo porque el marco presenta ambas
#' nociones de forma inconsistente y sólo el tipo permite validar las fórmulas.
#' El argumento referencial se conserva sin transformación dentro de la
#' instancia; [metricas_referencial()] declara y valida el contrato específico
#' de correctitud semántica y cobertura.
#'
#' @references AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad
#'   de Datos en Gobierno Digital*, versión 1.6, Presidencia de la República,
#'   Uruguay.
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' no_nulo <- especializar(
#'   nucleo$NoNulo, nombre_especifico = "NoNuloEdad"
#' )
#' instancia <- instanciar(no_nulo, entidad = "personas", atributos = "edad")
#' modelo_calidad <- modelo(instancia)
#' medir(modelo_calidad, data.frame(edad = c(20, NA, 35)))
#'
#' # Especialización oficial de teléfono fijo según el formato vigente del PNN.
#' telefono_pnn <- especializar(
#'   nucleo$DesactualizacionPorFormato,
#'   nombre_especifico = "TelefonoFijoPNN",
#'   expresion_regular = "^[0-9]{8}$"
#' )
#'
#' # Formato(NumeroDocumento, DNIC) se obtiene conectando un validador externo:
#' # validar_ci <- uyutils::validar_ci
#' # cedula_dnic <- especializar(
#' #   nucleo$Formato, nombre_especifico = "NumeroDocumentoDNIC",
#' #   validador = validar_ci
#' # )
#'
#' @name modelo_calidad
#' @seealso [referencial()], [agregar()], [medir()], [proponer_modelo()]
NULL

#' @rdname modelo_calidad
#' @export
metrica <- function(nombre, semantica, granularidad, tipo_resultado,
                    propiedades = character(), dimension = NA_character_,
                    factor = NA_character_, metodo = NULL,
                    validar_propiedades = NULL) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (!.es_texto_escalar(semantica)) {
    stop("`semantica` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  .validar_granularidad(granularidad)
  tipo_resultado <- .validar_tipo_resultado(tipo_resultado)
  if (!is.character(propiedades) || anyNA(propiedades) ||
      any(!nzchar(propiedades)) || anyDuplicated(propiedades)) {
    stop("`propiedades` debe contener nombres \u00fanicos no vac\u00edos.", call. = FALSE)
  }
  if (any(make.names(propiedades) != propiedades) ||
      "nombre_especifico" %in% propiedades) {
    stop("Las propiedades deben ser nombres sint\u00e1cticos no reservados.",
         call. = FALSE)
  }
  if (!is.null(metodo) && !is.function(metodo)) {
    stop("`metodo` debe ser una funci\u00f3n o NULL.", call. = FALSE)
  }
  if (!is.null(validar_propiedades) && !is.function(validar_propiedades)) {
    stop("`validar_propiedades` debe ser una funci\u00f3n o NULL.", call. = FALSE)
  }
  declaracion <- list(
    nombre = nombre,
    semantica = semantica,
    granularidad = granularidad,
    tipo_resultado = tipo_resultado,
    propiedades = propiedades,
    dimension = as.character(dimension)[[1L]],
    factor = as.character(factor)[[1L]]
  )
  .crear_fabrica_especializacion(
    declaracion, metodo = metodo, validador = validar_propiedades
  )
}

#' @rdname modelo_calidad
#' @param nombre_especifico Nombre de la especialización. Si se omite, conserva
#'   el nombre genérico.
#' @export
especializar <- function(metrica, nombre_especifico = NULL, ...) {
  if (!inherits(metrica, "metrica_generica")) {
    stop("`metrica` debe ser una m\u00e9trica gen\u00e9rica.", call. = FALSE)
  }
  declaracion <- .declaracion_metrica(metrica)
  if (is.null(nombre_especifico)) {
    nombre_especifico <- declaracion$nombre
  }
  if (!.es_texto_escalar(nombre_especifico)) {
    stop("`nombre_especifico` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  configuracion <- list(...)
  validador <- attr(metrica, "validador_propiedades", exact = TRUE)
  if (is.null(validador)) {
    configuracion <- .validar_propiedades_base(
      configuracion, declaracion$propiedades
    )
  } else {
    configuracion <- validador(configuracion)
    if (!is.list(configuracion)) {
      stop("El validador de propiedades debe devolver una lista.", call. = FALSE)
    }
  }
  .crear_fabrica_instancia(
    declaracion = declaracion,
    nombre_especifico = nombre_especifico,
    configuracion = configuracion,
    metodo = attr(metrica, "metodo_predeterminado", exact = TRUE)
  )
}

#' @rdname modelo_calidad
#' @param metrica_especifica Objeto de clase `metrica_especifica`.
#' @param nombre_instancia Nombre de la instancia. Si se omite, se deriva de la
#'   especialización y los objetos ligados.
#' @export
instanciar <- function(metrica_especifica, entidad, atributos = character(),
                       nombre_instancia = NULL, metodo = NULL,
                       referencial = NULL) {
  if (!inherits(metrica_especifica, "metrica_especifica")) {
    stop("`metrica_especifica` debe provenir de especializar().", call. = FALSE)
  }
  if (!is.character(entidad) || !length(entidad) || anyNA(entidad) ||
      any(!nzchar(entidad))) {
    stop("`entidad` debe contener nombres de tabla no vac\u00edos.", call. = FALSE)
  }
  if (!is.character(atributos) || anyNA(atributos) || any(!nzchar(atributos))) {
    stop("`atributos` debe contener nombres de columna no vac\u00edos.", call. = FALSE)
  }
  declaracion <- .declaracion_metrica(metrica_especifica)
  if (!.granularidad_implementada(declaracion$granularidad)) {
    stop(
      "La granularidad '", declaracion$granularidad,
      "' est\u00e1 declarada pero todav\u00eda no se puede instanciar.", call. = FALSE
    )
  }
  if (is.null(metodo)) {
    metodo <- attr(metrica_especifica, "metodo_predeterminado", exact = TRUE)
  }
  if (!is.function(metodo)) {
    stop("La instancia requiere un `metodo` de medici\u00f3n.", call. = FALSE)
  }
  if (is.null(nombre_instancia)) {
    vinculo <- c(entidad, atributos)
    nombre_instancia <- paste0(
      attr(metrica_especifica, "nombre_especifico", exact = TRUE),
      "@", paste(vinculo, collapse = ".")
    )
  }
  if (!.es_texto_escalar(nombre_instancia)) {
    stop("`nombre_instancia` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  estructura <- list(
    nombre = nombre_instancia,
    nombre_especifico = attr(
      metrica_especifica, "nombre_especifico", exact = TRUE
    ),
    declaracion = declaracion,
    configuracion = attr(metrica_especifica, "configuracion", exact = TRUE),
    entidad = entidad,
    atributos = atributos,
    referencial = referencial,
    metodo = metodo
  )
  class(estructura) <- "metrica_instanciada"
  estructura
}

#' @rdname modelo_calidad
#' @export
modelo <- function(...) {
  metricas <- list(...)
  if (length(metricas) == 1L && is.list(metricas[[1L]]) &&
      !inherits(metricas[[1L]], "metrica_instanciada")) {
    metricas <- metricas[[1L]]
  }
  if (!length(metricas) ||
      !all(vapply(metricas, inherits, logical(1L), "metrica_instanciada"))) {
    stop("`modelo()` requiere una o m\u00e1s m\u00e9tricas instanciadas.", call. = FALSE)
  }
  nombres <- vapply(metricas, `[[`, character(1L), "nombre")
  if (anyDuplicated(nombres)) {
    stop("Los nombres de las m\u00e9tricas instanciadas deben ser \u00fanicos.", call. = FALSE)
  }
  names(metricas) <- nombres
  estructura <- list(metricas = metricas)
  class(estructura) <- "modelo_calidad"
  estructura
}

.validar_config_formato <- function(configuracion) {
  permitidas <- c("expresion_regular", "diccionario", "validador")
  desconocidas <- setdiff(names(configuracion), permitidas)
  activas <- intersect(names(configuracion), permitidas)
  activas <- activas[!vapply(configuracion[activas], is.null, logical(1L))]
  if (length(desconocidas) || length(activas) != 1L) {
    stop(
      "Formato exige exactamente una de `expresion_regular`, `diccionario` o `validador`.",
      call. = FALSE
    )
  }
  if (activas == "expresion_regular" &&
      !.es_texto_escalar(configuracion$expresion_regular)) {
    stop("`expresion_regular` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (activas == "diccionario" && !is.atomic(configuracion$diccionario)) {
    stop("`diccionario` debe ser un vector at\u00f3mico.", call. = FALSE)
  }
  if (activas == "validador" && !is.function(configuracion$validador)) {
    stop("`validador` debe ser una funci\u00f3n.", call. = FALSE)
  }
  configuracion
}

.validar_config_no_nulo <- function(configuracion) {
  desconocidas <- setdiff(names(configuracion), "valores_nulos")
  if (length(desconocidas)) {
    stop("NoNulo s\u00f3lo acepta `valores_nulos`.", call. = FALSE)
  }
  if (is.null(configuracion$valores_nulos)) return(list(valores_nulos = NULL))
  if (!is.atomic(configuracion$valores_nulos)) {
    stop("`valores_nulos` debe ser un vector at\u00f3mico.", call. = FALSE)
  }
  configuracion
}

.validar_config_valores <- function(configuracion) {
  configuracion <- .validar_propiedades_base(configuracion, "valores")
  if (!is.atomic(configuracion$valores)) {
    stop("`valores` debe ser un vector at\u00f3mico.", call. = FALSE)
  }
  configuracion
}

.validar_config_regla <- function(configuracion) {
  configuracion <- .validar_propiedades_base(configuracion, "regla")
  if (!is.function(configuracion$regla)) {
    stop("`regla` debe ser una funci\u00f3n.", call. = FALSE)
  }
  configuracion
}

.validar_config_inter <- function(configuracion) {
  desconocidas <- setdiff(names(configuracion), "muestra")
  if (length(desconocidas)) {
    stop("ReglaIntegridadInterEntidad s\u00f3lo acepta `muestra`.", call. = FALSE)
  }
  if (is.null(configuracion$muestra)) {
    configuracion$muestra <- Inf
  }
  .validar_muestra(configuracion$muestra)
  configuracion
}

.obtener_tabla_modelo <- function(tablas, nombre) {
  if (!nombre %in% names(tablas)) {
    stop("No se encontr\u00f3 la entidad ligada: ", nombre, ".", call. = FALSE)
  }
  tablas[[nombre]]
}

.obtener_columna_modelo <- function(tabla, nombre, entidad) {
  if (!nombre %in% names(tabla)) {
    stop(
      "No se encontr\u00f3 el atributo ligado: ", entidad, "$", nombre, ".",
      call. = FALSE
    )
  }
  tabla[[nombre]]
}

.validar_vinculo <- function(instancia, n_entidades, n_atributos) {
  if (length(instancia$entidad) != n_entidades ||
      length(instancia$atributos) != n_atributos) {
    stop(
      "La m\u00e9trica ", instancia$declaracion$nombre, " requiere ",
      n_entidades, " entidad(es) y ", n_atributos, " atributo(s).",
      call. = FALSE
    )
  }
}

.salida_metodo <- function(resultado, entidad, atributo, fila, objeto) {
  n <- length(resultado)
  reciclar <- function(x) {
    if (length(x) == 1L) rep(x, n) else x
  }
  data.frame(
    resultado = resultado,
    entidad = reciclar(entidad),
    atributo = reciclar(atributo),
    fila = reciclar(fila),
    objeto = reciclar(objeto),
    stringsAsFactors = FALSE
  )
}

.metodo_no_nulo <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- seq_along(x)
  ausente <- is.na(x)
  valores_nulos <- instancia$configuracion$valores_nulos
  if (length(valores_nulos)) {
    ausente <- ausente | (!is.na(x) & x %in% valores_nulos)
  }
  .salida_metodo(
    !ausente, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.resultado_validador <- function(resultado, n, nombre) {
  if (!is.logical(resultado) || length(resultado) != n || anyNA(resultado)) {
    stop(
      "El validador de ", nombre,
      " debe devolver un vector l\u00f3gico sin NA de longitud ", n, ".",
      call. = FALSE
    )
  }
  resultado
}

.metodo_formato <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  valores <- x[filas]
  config <- instancia$configuracion
  if (!is.null(config$expresion_regular)) {
    resultado <- grepl(
      config$expresion_regular, as.character(valores), perl = TRUE
    )
  } else if (!is.null(config$diccionario)) {
    resultado <- valores %in% config$diccionario
  } else {
    resultado <- .resultado_validador(
      config$validador(valores), length(valores), "Formato"
    )
  }
  .salida_metodo(
    resultado, entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.metodo_valores_posibles <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  filas <- which(!is.na(x))
  .salida_metodo(
    x[filas] %in% instancia$configuracion$valores,
    entidad, atributo, filas,
    paste0(entidad, "$", atributo, "[", filas, "]")
  )
}

.metodo_regla_intra <- function(tablas, instancia) {
  if (length(instancia$entidad) != 1L || !length(instancia$atributos)) {
    stop(
      "ReglaIntegridadIntraEntidad requiere una entidad y al menos un atributo.",
      call. = FALSE
    )
  }
  entidad <- instancia$entidad[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  faltantes <- setdiff(instancia$atributos, names(tabla))
  if (length(faltantes)) {
    stop("No se encontraron atributos ligados: ", paste(faltantes, collapse = ", "), ".",
         call. = FALSE)
  }
  datos_regla <- tabla[, instancia$atributos, drop = FALSE]
  resultado <- .resultado_validador(
    instancia$configuracion$regla(datos_regla), nrow(tabla),
    "ReglaIntegridadIntraEntidad"
  )
  filas <- seq_len(nrow(tabla))
  .salida_metodo(
    resultado, entidad, NA_character_, filas,
    paste0(entidad, "[", filas, ",]")
  )
}

.metodo_regla_inter <- function(tablas, instancia) {
  .validar_vinculo(instancia, 2L, 2L)
  entidad_referencia <- instancia$entidad[[1L]]
  entidad_dependiente <- instancia$entidad[[2L]]
  atributo_pk <- instancia$atributos[[1L]]
  atributo_fk <- instancia$atributos[[2L]]
  tabla_referencia <- .obtener_tabla_modelo(tablas, entidad_referencia)
  tabla_dependiente <- .obtener_tabla_modelo(tablas, entidad_dependiente)
  .obtener_columna_modelo(tabla_referencia, atributo_pk, entidad_referencia)
  .obtener_columna_modelo(tabla_dependiente, atributo_fk, entidad_dependiente)
  relacion <- detectar_relaciones(
    tabla_referencia[atributo_pk], tabla_dependiente[atributo_fk],
    muestra = instancia$configuracion$muestra
  )
  .salida_metodo(
    relacion$cobertura_tabla2_en_tabla1,
    entidad_dependiente, atributo_fk, NA_integer_,
    paste0(
      entidad_dependiente, "$", atributo_fk, " -> ",
      entidad_referencia, "$", atributo_pk
    )
  )
}

.metodo_error_estandar <- function(tablas, instancia) {
  .validar_vinculo(instancia, 1L, 1L)
  entidad <- instancia$entidad[[1L]]
  atributo <- instancia$atributos[[1L]]
  tabla <- .obtener_tabla_modelo(tablas, entidad)
  x <- .obtener_columna_modelo(tabla, atributo, entidad)
  if (!is.numeric(x) || inherits(x, c("Date", "POSIXt"))) {
    stop("ErrorEstandar requiere un atributo num\u00e9rico.", call. = FALSE)
  }
  valores <- x[is.finite(x)]
  if (length(valores) < 2L) {
    stop("ErrorEstandar requiere al menos dos valores num\u00e9ricos v\u00e1lidos.",
         call. = FALSE)
  }
  resultado <- stats::sd(valores)
  .salida_metodo(
    resultado, entidad, atributo, NA_integer_,
    paste0(entidad, "$", atributo)
  )
}

#' @rdname modelo_calidad
#' @export
metricas_nucleo <- function() {
  c(list(
    NoNulo = metrica(
      "NoNulo", "Indica si una instancia de atributo no es nula.",
      "instanciaAtributo", "booleano", propiedades = "valores_nulos",
      dimension = "Completitud", factor = "Densidad", metodo = .metodo_no_nulo,
      validar_propiedades = .validar_config_no_nulo
    ),
    Formato = metrica(
      "Formato", "Indica si un valor cumple un formato o diccionario.",
      "instanciaAtributo", "booleano",
      propiedades = c("expresion_regular", "diccionario", "validador"),
      dimension = "Exactitud", factor = "Exactitud sint\u00e1ctica",
      metodo = .metodo_formato,
      validar_propiedades = .validar_config_formato
    ),
    ValoresPosiblesPorExtension = metrica(
      "ValoresPosiblesPorExtension",
      "Indica si un valor pertenece al dominio enumerado.",
      "instanciaAtributo", "booleano", propiedades = "valores",
      dimension = "Consistencia", factor = "Integridad de dominio",
      metodo = .metodo_valores_posibles,
      validar_propiedades = .validar_config_valores
    ),
    ReglaIntegridadIntraEntidad = metrica(
      "ReglaIntegridadIntraEntidad",
      "Indica si una instancia de entidad cumple una regla interna.",
      "instanciaEntidad", "booleano", propiedades = "regla",
      dimension = "Consistencia", factor = "Integridad intra-entidad",
      metodo = .metodo_regla_intra,
      validar_propiedades = .validar_config_regla
    ),
    ReglaIntegridadInterEntidad = metrica(
      "ReglaIntegridadInterEntidad",
      "Mide la cobertura de una clave for\u00e1nea respecto de su referencia.",
      "entidad", "real", propiedades = "muestra",
      dimension = "Consistencia", factor = "Integridad inter-entidad",
      metodo = .metodo_regla_inter,
      validar_propiedades = .validar_config_inter
    ),
    ErrorEstandar = metrica(
      "ErrorEstandar",
      "Mide la desviaci\u00f3n est\u00e1ndar muestral del atributo.",
      "atributo", "numero_real", dimension = "Exactitud", factor = "Precisi\u00f3n",
      metodo = .metodo_error_estandar
    )
  ), .metricas_adicionales())
}

.normalizar_tablas_modelo <- function(datos, modelo) {
  if (inherits(datos, "data.frame")) {
    entidades <- unique(unlist(lapply(
      modelo$metricas, `[[`, "entidad"
    ), use.names = FALSE))
    if (length(entidades) != 1L) {
      stop(
        "Para un modelo con varias entidades, `datos` debe ser una lista con nombre.",
        call. = FALSE
      )
    }
    return(stats::setNames(list(datos), entidades))
  }
  if (!is.list(datos) || is.null(names(datos)) || any(!nzchar(names(datos))) ||
      anyDuplicated(names(datos)) ||
      !all(vapply(datos, inherits, logical(1L), "data.frame"))) {
    stop(
      "`datos` debe ser un data.frame o una lista con nombre de data frames.",
      call. = FALSE
    )
  }
  datos
}

.validar_salida_medicion <- function(salida, instancia) {
  requeridas <- c("resultado", "entidad", "atributo", "fila", "objeto")
  if (!inherits(salida, "data.frame") || !all(requeridas %in% names(salida))) {
    stop(
      "El m\u00e9todo de ", instancia$nombre,
      " debe devolver un data frame con: ", paste(requeridas, collapse = ", "), ".",
      call. = FALSE
    )
  }
  resultado <- salida$resultado
  tipo <- instancia$declaracion$tipo_resultado
  if (tipo == "booleano" &&
      (!is.logical(resultado) || anyNA(resultado))) {
    stop("Una m\u00e9trica booleana debe devolver l\u00f3gicos sin NA.", call. = FALSE)
  }
  if (tipo == "real" &&
      (!is.numeric(resultado) || anyNA(resultado) ||
       any(!is.finite(resultado)) || any(resultado < 0 | resultado > 1))) {
    stop("Una m\u00e9trica real debe devolver valores finitos en [0, 1].",
         call. = FALSE)
  }
  if (tipo == "numero_real" &&
      (!is.numeric(resultado) || anyNA(resultado) ||
       any(!is.finite(resultado)))) {
    stop("Una m\u00e9trica numero_real debe devolver n\u00fameros finitos.",
         call. = FALSE)
  }
  if (tipo == "entero" &&
      (!is.numeric(resultado) || anyNA(resultado) ||
       any(!is.finite(resultado)) || any(resultado < 0) ||
       any(resultado != floor(resultado)))) {
    stop("Una m\u00e9trica entera debe devolver enteros no negativos.",
         call. = FALSE)
  }
  if (tipo == "duracion" &&
      (!is.numeric(resultado) || anyNA(resultado) ||
       any(!is.finite(resultado)) || any(resultado < 0))) {
    stop("Una m\u00e9trica de duraci\u00f3n debe devolver valores finitos no negativos.",
         call. = FALSE)
  }
  salida[, requeridas, drop = FALSE]
}

.nuevo_id_medicion <- function(fecha) {
  paste0(
    "medicion-", format(fecha, "%Y%m%dT%H%M%OS6"), "-", Sys.getpid()
  )
}

#' Medir un modelo de calidad
#'
#' Ejecuta todas las métricas instanciadas de un `modelo_calidad`. Cada fila es
#' una medida reutilizable y conserva el identificador y la fecha de la corrida.
#'
#' @param modelo Objeto creado por `modelo()`.
#' @param datos Data frame para una sola entidad o lista con nombre de data
#'   frames para varias entidades.
#' @param id_medicion Identificador de corrida. Si se omite, se genera uno.
#' @param fecha Fecha y hora de la corrida.
#'
#' @return Data frame S3 de clase `medicion`, con una fila por objeto medido.
#'   Los booleanos se almacenan como `0` y `1` en la columna común `resultado`,
#'   y su semántica permanece declarada en `tipo_resultado`.
#' @export
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
#' instancia <- instanciar(especifica, "personas", "edad")
#' medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
medir <- function(modelo, datos, id_medicion = NULL, fecha = Sys.time()) {
  if (!inherits(modelo, "modelo_calidad")) {
    stop("`modelo` debe provenir de modelo().", call. = FALSE)
  }
  if (length(fecha) != 1L || is.na(fecha)) {
    stop("`fecha` debe contener una fecha y hora v\u00e1lida.", call. = FALSE)
  }
  fecha <- as.POSIXct(fecha)
  if (is.null(id_medicion)) {
    id_medicion <- .nuevo_id_medicion(fecha)
  }
  if (!.es_texto_escalar(id_medicion)) {
    stop("`id_medicion` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  tablas <- .normalizar_tablas_modelo(datos, modelo)
  partes <- lapply(modelo$metricas, function(instancia) {
    salida <- .validar_salida_medicion(
      instancia$metodo(tablas, instancia), instancia
    )
    n <- nrow(salida)
    data.frame(
      id_medicion = rep(id_medicion, n),
      fecha = rep(fecha, n),
      metrica = rep(instancia$declaracion$nombre, n),
      metrica_especifica = rep(instancia$nombre_especifico, n),
      metrica_instanciada = rep(instancia$nombre, n),
      dimension = rep(instancia$declaracion$dimension, n),
      factor = rep(instancia$declaracion$factor, n),
      granularidad = rep(instancia$declaracion$granularidad, n),
      tipo_resultado = rep(instancia$declaracion$tipo_resultado, n),
      entidad = as.character(salida$entidad),
      atributo = as.character(salida$atributo),
      fila = as.integer(salida$fila),
      objeto_medible = as.character(salida$objeto),
      resultado = as.numeric(salida$resultado),
      agregacion = rep(NA_character_, n),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado$id_medida <- if (nrow(resultado)) {
    paste0(id_medicion, "-", sprintf("%06d", seq_len(nrow(resultado))))
  } else {
    character()
  }
  resultado <- resultado[c(
    "id_medida", "id_medicion", "fecha", "metrica", "metrica_especifica",
    "metrica_instanciada", "dimension", "factor", "granularidad",
    "tipo_resultado", "entidad", "atributo", "fila", "objeto_medible",
    "resultado", "agregacion"
  )]
  class(resultado) <- c("medicion", "data.frame")
  resultado
}

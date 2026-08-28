#' Reglas y perfiles de evaluación
#'
#' Una regla aplica una condición a los resultados de una o más métricas
#' instanciadas. Un perfil reúne reglas y su evaluación es la media aritmética
#' simple de las evaluaciones de esas reglas; no es un índice de dimensión ni
#' un índice global de calidad.
#'
#' `perfiles_madurez()` crea por omisión los perfiles `Básico`, `Intermedio` y
#' `Avanzado` de AGESIC, con condiciones estrictas `> 0.5`, `> 0.7` y `> 0.9`.
#' El argumento `umbrales` permite construir otra familia con nombres y cortes
#' crecientes propios sobre las mismas métricas instanciadas.
#'
#' @param nombre Nombre de la regla o del perfil.
#' @param condicion Función que recibe el vector `resultado` de las medidas
#'   seleccionadas, en el orden de la tabla, y debe devolver un vector lógico
#'   sin ausentes de la misma longitud. Puede declarar un segundo argumento
#'   `orientacion` para recibir el metadato homónimo de cada medida; las
#'   funciones existentes de un argumento siguen siendo válidas. No modifica
#'   las medidas.
#' @param metricas Nombres de métricas instanciadas a las que se aplica la
#'   regla, es decir, valores de la columna `metrica_instanciada`. `NULL`, el
#'   valor predeterminado, aplica la condición a todas.
#' @param proporcion_minima `NULL`, para conservar una regla por medida, o un
#'   número entre `0` y `1` que declara la proporción mínima de medidas que
#'   deben cumplir `condicion`. En este segundo caso la regla es agregada: el
#'   umbral queda guardado en el objeto y [evaluar()] publica la proporción, el
#'   veredicto y el universo de medidas que la produjo.
#' @param desenlace `NULL`, para limitar la regla a evaluar, o `"suprimir"`
#'   para declarar que las medidas que no cumplen `condicion` no deben
#'   publicarse. No existe un desenlace predeterminado.
#' @param umbrales Vector numérico con nombres, estrictamente creciente y en
#'   `[0, 1]`. `NULL` conserva los tres perfiles incluidos de fábrica.
#' @param ... Reglas creadas por `regla_evaluacion()` o una única lista que las
#'   contenga.
#'
#' @return `regla_evaluacion()` devuelve una `regla_evaluacion`;
#'   `perfil_evaluacion()` devuelve un `perfil_evaluacion`; y
#'   `perfiles_madurez()` devuelve una lista de perfiles.
#' @name reglas_evaluacion
#'
#' @details `regla_evaluacion()` almacena la función sin ejecutarla. [evaluar()]
#'   selecciona las medidas mediante `metricas`, llama una vez a `condicion` y
#'   rechaza resultados que no sean lógicos, que tengan otra longitud o que
#'   contengan `NA`. Si `proporcion_minima` no es `NULL`, calcula sobre esos
#'   mismos lógicos la proporción que cumple y la compara mediante `>=` con el
#'   umbral declarado; no pondera medidas ni construye un puntaje global. Las
#'   evaluaciones cuyas reglas no declaran `desenlace` conservan su estructura
#'   anterior. Cuando una regla declara `desenlace = "suprimir"`, [evaluar()]
#'   añade un plan trazable con una fila por medida incumplida y por regla; no
#'   modifica la medición ni los datos que la originaron. La función expresa un
#'   criterio de evaluación; no es un método de medición ni recibe el data frame
#'   original. Si ningún nombre de
#'   `metricas` coincide, el error enumera tanto los nombres solicitados como
#'   las métricas instanciadas disponibles, que normalmente tienen la forma
#'   `MetricaEspecifica@entidad.atributo`.
#'
#' @examples
#' regla <- regla_evaluacion("Completitud suficiente", function(x) x > 0.9)
#' regla_70 <- regla_evaluacion(
#'   "Al menos 70 %", function(x) x > 0.9, proporcion_minima = 0.7
#' )
#' regla_publicacion <- regla_evaluacion(
#'   "Medida publicable", function(x) x > 0.9, desenlace = "suprimir"
#' )
#' perfil <- perfil_evaluacion("Operativo", regla)
#' madurez <- perfiles_madurez("NoNulo")
#' propios <- perfiles_madurez(
#'   "NoNulo", c(Exploratorio = 0.3, Operativo = 0.65, Consolidado = 0.85)
#' )
#' names(madurez)
#' names(propios)
#' perfil$nombre
NULL

#' @rdname reglas_evaluacion
#' @export
#' @seealso [medir()], [evaluar()], [perfiles_madurez()]
regla_evaluacion <- function(nombre, condicion, metricas = NULL,
                             proporcion_minima = NULL, desenlace = NULL,
                             umbrales = list()) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  if (!is.function(condicion)) {
    stop("`condicion` debe ser una funci\u00f3n.", call. = FALSE)
  }
  if (!is.null(metricas) &&
      (!is.character(metricas) || !length(metricas) || anyNA(metricas) ||
       any(!nzchar(metricas)))) {
    stop("`metricas` debe ser NULL o nombres no vac\u00edos.", call. = FALSE)
  }
  if (!is.null(proporcion_minima) &&
      (!is.numeric(proporcion_minima) || length(proporcion_minima) != 1L ||
       is.na(proporcion_minima) || !is.finite(proporcion_minima) ||
       proporcion_minima < 0 || proporcion_minima > 1)) {
    stop("`proporcion_minima` debe ser NULL o un n\u00famero entre 0 y 1.",
         call. = FALSE)
  }
  if (!is.null(desenlace) &&
      (!.es_texto_escalar(desenlace) || desenlace != "suprimir")) {
    stop("`desenlace` debe ser NULL o 'suprimir'.", call. = FALSE)
  }
  # Los umbrales viajan aparte de la condicion para que se puedan cambiar sin
  # reconstruir la regla y para que queden a la vista en `propiedades_regla()`.
  # Encerrados en el closure quedaban invisibles y obligaban a escribir otra
  # regla para mover un numero.
  if (!is.list(umbrales)) {
    stop("`umbrales` debe ser una lista con nombres.", call. = FALSE)
  }
  if (length(umbrales)) {
    nombres_umbrales <- names(umbrales)
    if (is.null(nombres_umbrales) || anyNA(nombres_umbrales) ||
        !all(nzchar(nombres_umbrales)) || anyDuplicated(nombres_umbrales)) {
      stop("`umbrales` debe tener nombres unicos y no vacios.", call. = FALSE)
    }
    argumentos_condicion <- names(formals(condicion))
    if (!"..." %in% argumentos_condicion) {
      sin_recibir <- setdiff(nombres_umbrales, argumentos_condicion)
      if (length(sin_recibir)) {
        stop(
          "`condicion` no recibe estos umbrales: ",
          paste(sin_recibir, collapse = ", "),
          ". Sus argumentos son: ",
          paste(argumentos_condicion, collapse = ", "), ".",
          call. = FALSE
        )
      }
    }
  }
  estructura <- list(
    nombre = nombre,
    condicion = condicion,
    metricas = unique(metricas)
  )
  if (!is.null(proporcion_minima)) {
    estructura$nivel <- "agregado"
    estructura$proporcion_minima <- as.numeric(proporcion_minima)
  }
  if (!is.null(desenlace)) estructura$desenlace <- desenlace
  if (length(umbrales)) estructura$umbrales <- umbrales
  class(estructura) <- "regla_evaluacion"
  estructura
}

#' Propiedades declaradas de una regla de evaluación
#'
#' Devuelve, en una tabla, lo que una regla declara: a qué métricas se engancha,
#' en qué nivel evalúa, qué desenlace produce y **qué umbrales usa**. Los
#' umbrales viajan aparte de la condición justamente para esto: encerrados en el
#' *closure* quedaban invisibles y obligaban a escribir otra regla para mover un
#' número.
#'
#' Es la contraparte de [propiedades_metrica()], que describe métricas. Un
#' umbral pertenece a una regla, no a una métrica, así que no cabía allí.
#'
#' @param regla Objeto creado por [regla_evaluacion()].
#'
#' @return Data frame con una fila por propiedad: `propiedad`, `valor`.
#' @export
#' @seealso [regla_evaluacion()], [propiedades_metrica()], [evaluar()]
#'
#' @examples
#' regla <- regla_evaluacion(
#'   "cobertura minima",
#'   function(x, minimo) x >= minimo,
#'   umbrales = list(minimo = 0.9)
#' )
#' propiedades_regla(regla)
propiedades_regla <- function(regla) {
  if (!inherits(regla, "regla_evaluacion")) {
    stop("`regla` debe ser una regla creada por regla_evaluacion().",
         call. = FALSE)
  }
  texto <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    paste(format(x, trim = TRUE), collapse = ", ")
  }
  filas <- list(
    data.frame(propiedad = "nombre", valor = regla$nombre,
               stringsAsFactors = FALSE),
    data.frame(propiedad = "metricas", valor = texto(regla$metricas),
               stringsAsFactors = FALSE),
    data.frame(propiedad = "nivel",
               valor = if (is.null(regla$nivel)) "medida" else regla$nivel,
               stringsAsFactors = FALSE),
    data.frame(propiedad = "proporcion_minima",
               valor = texto(regla$proporcion_minima),
               stringsAsFactors = FALSE),
    data.frame(propiedad = "desenlace", valor = texto(regla$desenlace),
               stringsAsFactors = FALSE)
  )
  if (length(regla$umbrales)) {
    filas <- c(filas, lapply(names(regla$umbrales), function(nombre) {
      data.frame(
        propiedad = paste0("umbral:", nombre),
        valor = texto(regla$umbrales[[nombre]]),
        stringsAsFactors = FALSE
      )
    }))
  }
  salida <- do.call(rbind, filas)
  rownames(salida) <- NULL
  salida
}

#' @rdname reglas_evaluacion
#' @export
#' @seealso [regla_evaluacion()], [comparar_evaluaciones()],
#'   [historico_calidad()]
perfil_evaluacion <- function(nombre, ...) {
  if (!.es_texto_escalar(nombre)) {
    stop("`nombre` debe ser una cadena no vac\u00eda.", call. = FALSE)
  }
  reglas <- list(...)
  if (length(reglas) == 1L && is.list(reglas[[1L]]) &&
      !inherits(reglas[[1L]], "regla_evaluacion")) {
    reglas <- reglas[[1L]]
  }
  if (!length(reglas) ||
      !all(vapply(reglas, inherits, logical(1L), "regla_evaluacion"))) {
    stop("Un perfil requiere una o m\u00e1s reglas de evaluaci\u00f3n.", call. = FALSE)
  }
  nombres <- vapply(reglas, `[[`, character(1L), "nombre")
  if (anyDuplicated(nombres)) {
    stop("Los nombres de las reglas del perfil deben ser \u00fanicos.", call. = FALSE)
  }
  names(reglas) <- nombres
  estructura <- list(nombre = nombre, reglas = reglas)
  class(estructura) <- "perfil_evaluacion"
  estructura
}

.regla_umbral <- function(nombre, umbral, metricas) {
  force(umbral)
  regla_evaluacion(
    nombre,
    condicion = function(x) x > umbral,
    metricas = metricas
  )
}

#' @rdname reglas_evaluacion
#' @export
#' @seealso [evaluar()], [detectar_deriva_calidad()]
perfiles_madurez <- function(metricas = NULL, umbrales = NULL) {
  fabrica <- is.null(umbrales)
  if (fabrica) {
    umbrales <- c(Basico = 0.5, Intermedio = 0.7, Avanzado = 0.9)
  }
  if (!is.numeric(umbrales) || !length(umbrales) || anyNA(umbrales) ||
      any(!is.finite(umbrales)) || any(umbrales < 0 | umbrales > 1) ||
      is.null(names(umbrales)) || anyNA(names(umbrales)) ||
      any(!nzchar(names(umbrales))) || anyDuplicated(names(umbrales))) {
    stop(
      "`umbrales` debe ser un vector num\u00e9rico con nombres \u00fanicos en [0, 1].",
      call. = FALSE
    )
  }
  if (length(umbrales) > 1L && any(diff(umbrales) <= 0)) {
    stop("Los umbrales de madurez deben ser estrictamente crecientes.",
         call. = FALSE)
  }
  nombres <- names(umbrales)
  nombres_perfil <- if (fabrica) {
    c(Basico = "B\u00e1sico", Intermedio = "Intermedio", Avanzado = "Avanzado")
  } else {
    nombres
  }
  perfiles <- lapply(seq_along(umbrales), function(i) {
    nombre <- unname(nombres_perfil[[i]])
    umbral <- unname(umbrales[[i]])
    perfil_evaluacion(
      nombre,
      .regla_umbral(paste0("Resultado > ", umbral), umbral, metricas)
    )
  })
  names(perfiles) <- nombres
  perfiles
}

.validar_medicion_evaluacion <- function(medicion) {
  requeridas <- c(
    "id_medida", "id_medicion", "fecha", "metrica_instanciada",
    "tipo_resultado", "resultado"
  )
  if (!inherits(medicion, "data.frame") || !nrow(medicion) ||
      !all(requeridas %in% names(medicion))) {
    stop("`medicion` debe ser un data frame no vac\u00edo producido por medir().",
         call. = FALSE)
  }
  medicion <- .tabla_base(medicion)
  if (!.resultados_validos_tipo(
    medicion$resultado, medicion$tipo_resultado
  )) {
    stop("Los resultados de la medici\u00f3n no respetan su tipo declarado.",
         call. = FALSE)
  }
  .orientacion_medidas(medicion)
  medicion
}

.aplicar_condicion_regla <- function(condicion, resultado, orientacion,
                                     umbrales = NULL) {
  argumentos <- names(formals(condicion))
  extra <- list()
  if ("orientacion" %in% argumentos || "..." %in% argumentos) {
    extra$orientacion <- orientacion
  }
  if (length(umbrales)) {
    admitidos <- if ("..." %in% argumentos) {
      names(umbrales)
    } else {
      intersect(names(umbrales), argumentos)
    }
    extra <- c(extra, umbrales[admitidos])
  }
  do.call(condicion, c(list(resultado), extra))
}

.evaluar_regla_medidas <- function(medicion, perfil, regla) {
  seleccion <- if (is.null(regla$metricas)) {
    rep(TRUE, nrow(medicion))
  } else {
    medicion$metrica_instanciada %in% regla$metricas
  }
  medidas <- medicion[seleccion, , drop = FALSE]
  if (!nrow(medidas)) {
    solicitadas <- setdiff(regla$metricas, medicion$metrica_instanciada)
    disponibles <- unique(medicion$metrica_instanciada)
    stop(
      "La regla '", regla$nombre,
      "' no coincide con ninguna m\u00e9trica instanciada. Solicitadas: ",
      paste(solicitadas, collapse = ", "), ". Disponibles: ",
      paste(disponibles, collapse = ", "), ".",
      call. = FALSE
    )
  }
  orientacion <- .orientacion_medidas(medidas)
  resultado <- .aplicar_condicion_regla(
    regla$condicion, medidas$resultado, orientacion, regla$umbrales
  )
  if (!is.logical(resultado) || length(resultado) != nrow(medidas) ||
      anyNA(resultado)) {
    stop(
      "La condici\u00f3n de la regla '", regla$nombre,
      "' debe devolver l\u00f3gicos sin NA, uno por medida.", call. = FALSE
    )
  }
  data.frame(
    id_medida = medidas$id_medida,
    id_medicion = medidas$id_medicion,
    fecha = medidas$fecha,
    perfil = perfil$nombre,
    regla = regla$nombre,
    metrica_instanciada = medidas$metrica_instanciada,
    orientacion = orientacion,
    resultado = resultado,
    stringsAsFactors = FALSE
  )
}

.resumir_evaluaciones_regla <- function(evaluaciones) {
  clave <- interaction(
    evaluaciones$id_medicion, evaluaciones$perfil, evaluaciones$regla,
    drop = TRUE, lex.order = TRUE
  )
  grupos <- split(seq_len(nrow(evaluaciones)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    primera <- indices[[1L]]
    data.frame(
      id_medicion = evaluaciones$id_medicion[[primera]],
      fecha = evaluaciones$fecha[primera],
      perfil = evaluaciones$perfil[[primera]],
      regla = evaluaciones$regla[[primera]],
      n_medidas = length(indices),
      resultado = mean(evaluaciones$resultado[indices]),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

.declarar_reglas_agregadas <- function(resumen, evaluaciones, perfil) {
  es_agregada <- vapply(
    perfil$reglas, function(regla) !is.null(regla$proporcion_minima),
    logical(1L)
  )
  if (!any(es_agregada)) return(resumen)

  resumen$nivel <- ifelse(
    resumen$regla %in% names(perfil$reglas)[es_agregada],
    "agregado", "medida"
  )
  resumen$n_cumplen <- integer(nrow(resumen))
  resumen$universo <- character(nrow(resumen))
  resumen$proporcion_minima <- NA_real_
  resumen$cumple <- NA
  for (i in seq_len(nrow(resumen))) {
    indices <- evaluaciones$id_medicion == resumen$id_medicion[[i]] &
      evaluaciones$perfil == resumen$perfil[[i]] &
      evaluaciones$regla == resumen$regla[[i]]
    componentes <- evaluaciones[indices, , drop = FALSE]
    resumen$n_cumplen[[i]] <- sum(componentes$resultado)
    metricas <- unique(componentes$metrica_instanciada)
    resumen$universo[[i]] <- paste0(
      nrow(componentes), " medidas seleccionadas: ",
      paste(metricas, collapse = ", ")
    )
    regla <- perfil$reglas[[resumen$regla[[i]]]]
    if (!is.null(regla$proporcion_minima)) {
      resumen$proporcion_minima[[i]] <- regla$proporcion_minima
      resumen$cumple[[i]] <-
        resumen$resultado[[i]] >= regla$proporcion_minima
    }
  }
  resumen[c(
    "id_medicion", "fecha", "perfil", "regla", "nivel", "n_medidas",
    "n_cumplen", "universo", "resultado", "proporcion_minima", "cumple"
  )]
}

.resumir_evaluaciones_perfil <- function(evaluaciones) {
  clave <- interaction(
    evaluaciones$id_medicion, evaluaciones$perfil,
    drop = TRUE, lex.order = TRUE
  )
  grupos <- split(seq_len(nrow(evaluaciones)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    primera <- indices[[1L]]
    data.frame(
      id_medicion = evaluaciones$id_medicion[[primera]],
      fecha = evaluaciones$fecha[primera],
      perfil = evaluaciones$perfil[[primera]],
      n_reglas = length(indices),
      resultado = mean(evaluaciones$resultado[indices]),
      stringsAsFactors = FALSE
    )
  })
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  resultado
}

.desenlaces_vacios <- function() {
  resultado <- data.frame(
    id_medida = character(),
    id_medicion = character(),
    fecha = as.POSIXct(character()),
    perfil = character(),
    regla = character(),
    desenlace = character(),
    motivo = character(),
    metrica_instanciada = character(),
    orientacion = character(),
    granularidad = character(),
    entidad = character(),
    atributo = character(),
    fila = integer(),
    objeto_medible = character(),
    valor_medido = numeric(),
    stringsAsFactors = FALSE
  )
  class(resultado) <- c("plan_desenlaces", "data.frame")
  resultado
}

.planificar_desenlaces <- function(medicion, evaluaciones, perfil) {
  con_desenlace <- vapply(
    perfil$reglas, function(regla) !is.null(regla$desenlace), logical(1L)
  )
  if (!any(con_desenlace)) return(NULL)

  partes <- lapply(perfil$reglas[con_desenlace], function(regla) {
    incumplidas <- evaluaciones$regla == regla$nombre & !evaluaciones$resultado
    if (!any(incumplidas)) return(NULL)
    indices <- match(evaluaciones$id_medida[incumplidas], medicion$id_medida)
    medidas <- medicion[indices, , drop = FALSE]
    data.frame(
      id_medida = medidas$id_medida,
      id_medicion = medidas$id_medicion,
      fecha = medidas$fecha,
      perfil = rep(perfil$nombre, nrow(medidas)),
      regla = rep(regla$nombre, nrow(medidas)),
      desenlace = rep(regla$desenlace, nrow(medidas)),
      motivo = rep(
        paste0(
          "La medida no cumple la condici\u00f3n declarada por la regla '",
          regla$nombre, "'."
        ),
        nrow(medidas)
      ),
      metrica_instanciada = medidas$metrica_instanciada,
      orientacion = .orientacion_medidas(medidas),
      granularidad = medidas$granularidad,
      entidad = medidas$entidad,
      atributo = medidas$atributo,
      fila = medidas$fila,
      objeto_medible = medidas$objeto_medible,
      valor_medido = medidas$resultado,
      stringsAsFactors = FALSE
    )
  })
  partes <- partes[!vapply(partes, is.null, logical(1L))]
  if (!length(partes)) return(.desenlaces_vacios())
  resultado <- do.call(rbind, partes)
  rownames(resultado) <- NULL
  class(resultado) <- c("plan_desenlaces", "data.frame")
  resultado
}

#' Evaluar medidas, reglas y perfiles
#'
#' Ejecuta la cadena formal: condición por medida, proporción de medidas que
#' cumplen cada regla y media aritmética simple de las reglas del perfil.
#'
#' @param medicion Data frame producido por `medir()`. Puede reunir varias
#'   corridas si conserva sus `id_medicion`.
#' @param perfil Objeto creado por `perfil_evaluacion()`.
#'
#' @return Objeto `evaluacion_calidad` con tres data frames filtrables:
#'   `medidas`, `reglas` y `perfiles`. Si alguna regla declara un desenlace,
#'   contiene además `desenlaces`, un plan que identifica las medidas
#'   incumplidas, el valor medido, el motivo y la regla que lo produjo.
#' @export
#'
#' @examples
#' nucleo <- metricas_nucleo()
#' especifica <- especializar(nucleo$NoNulo)
#' instancia <- instanciar(especifica, "personas", "edad")
#' medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#' regla <- regla_evaluacion("Al menos 90%", function(x) x > 0.9)
#' evaluar(medidas, perfil_evaluacion("Avanzado", regla))
evaluar <- function(medicion, perfil) {
  if (inherits(medicion, "data.frame")) medicion <- .tabla_base(medicion)
  medicion <- .validar_medicion_evaluacion(medicion)
  if (!inherits(perfil, "perfil_evaluacion")) {
    stop("`perfil` debe provenir de perfil_evaluacion().", call. = FALSE)
  }
  evaluaciones_medidas <- do.call(rbind, lapply(perfil$reglas, function(regla) {
    .evaluar_regla_medidas(medicion, perfil, regla)
  }))
  rownames(evaluaciones_medidas) <- NULL
  evaluaciones_reglas <- .resumir_evaluaciones_regla(evaluaciones_medidas)
  evaluaciones_reglas <- .declarar_reglas_agregadas(
    evaluaciones_reglas, evaluaciones_medidas, perfil
  )
  evaluaciones_perfiles <- .resumir_evaluaciones_perfil(evaluaciones_reglas)
  class(evaluaciones_medidas) <- c("evaluacion_medidas", "data.frame")
  class(evaluaciones_reglas) <- c("evaluacion_reglas", "data.frame")
  class(evaluaciones_perfiles) <- c("evaluacion_perfiles", "data.frame")
  estructura <- list(
    medidas = evaluaciones_medidas,
    reglas = evaluaciones_reglas,
    perfiles = evaluaciones_perfiles
  )
  desenlaces <- .planificar_desenlaces(
    medicion, evaluaciones_medidas, perfil
  )
  if (!is.null(desenlaces)) estructura$desenlaces <- desenlaces
  class(estructura) <- "evaluacion_calidad"
  estructura
}

#' Comparar evaluaciones de perfil
#'
#' Calcula el cambio de `EvaluacionPerfil` entre dos corridas. Cada objeto debe
#' contener una sola `id_medicion`; no persiste los resultados. Para una serie
#' de N corridas use [historico_calidad()] y [detectar_deriva_calidad()].
#'
#' @param anterior,actual Objetos creados por `evaluar()`.
#'
#' @return Data frame con resultados anterior y actual, y `delta`.
#' @export
#'
#' @examples
#' # Ver ejemplos de evaluar().
comparar_evaluaciones <- function(anterior, actual) {
  if (!inherits(anterior, "evaluacion_calidad") ||
      !inherits(actual, "evaluacion_calidad")) {
    stop("`anterior` y `actual` deben provenir de evaluar().", call. = FALSE)
  }
  a <- anterior$perfiles
  b <- actual$perfiles
  if (length(unique(a$id_medicion)) != 1L ||
      length(unique(b$id_medicion)) != 1L) {
    stop("Cada evaluaci\u00f3n debe contener una sola corrida.", call. = FALSE)
  }
  combinado <- merge(
    a[c("perfil", "id_medicion", "fecha", "resultado")],
    b[c("perfil", "id_medicion", "fecha", "resultado")],
    by = "perfil", suffixes = c("_anterior", "_actual"), all = TRUE,
    sort = FALSE
  )
  combinado$delta <- combinado$resultado_actual - combinado$resultado_anterior
  combinado
}

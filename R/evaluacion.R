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
        !all(nzchar(nombres_umbrales)) ||
        anyDuplicated(.nombres_para_operar(nombres_umbrales))) {
      stop("`umbrales` debe tener nombres unicos y no vacios.", call. = FALSE)
    }
    argumentos_condicion <- names(formals(condicion))
    if (!"..." %in% argumentos_condicion) {
      sin_recibir <- .identificadores_setdiff(
        nombres_umbrales, argumentos_condicion
      )
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
    metricas = if (is.null(metricas)) NULL else {
      .identificadores_unicos(metricas)
    }
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
    filas <- c(filas, lapply(seq_along(regla$umbrales), function(indice) {
      nombre <- names(regla$umbrales)[[indice]]
      data.frame(
        propiedad = paste0("umbral:", nombre),
        valor = texto(regla$umbrales[[indice]]),
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
  if (anyDuplicated(.nombres_para_operar(nombres))) {
    stop("Los nombres de las reglas del perfil deben ser \u00fanicos.", call. = FALSE)
  }
  names(reglas) <- nombres
  estructura <- list(nombre = nombre, reglas = reglas)
  class(estructura) <- "perfil_evaluacion"
  estructura
}

.regla_umbral <- function(nombre, umbral, metricas) {
  force(umbral)
  # La condicion CONSULTA la orientacion, que es para lo que existe el segundo
  # argumento que `regla_evaluacion()` documenta y que esta fabrica ignoraba.
  #
  # Sin esto, una metrica orientada a `defecto` -donde mas alto es peor- quedaba
  # premiada por la regla: una tabla 100 % duplicada da `EntidadDuplicada = 1`, y
  # `Resultado > 0.5` la daba por cumplida en los tres perfiles, mientras la
  # tabla limpia -`0`- no cumplia ninguno. La regla estaba al reves justo donde
  # importa.
  #
  # La inversion es `1 - valor`, la misma convencion que ya usa
  # `tablero_calidad()` para sus componentes de defecto, y que ese tablero
  # publica en `invertidas`. Una sola convencion en el paquete y no dos.
  #
  # `no_aplica` no se invierte ni se premia: una medida que no aplica no es
  # cumplimiento, y se resuelve como `NA` para que la evaluacion la trate como
  # lo que es -no medida- en vez de contarla a favor.
  regla_evaluacion(
    nombre,
    condicion = function(x, orientacion = NULL) {
      valor <- as.numeric(x)
      if (!is.null(orientacion) && length(orientacion) == length(valor)) {
        defecto <- !is.na(orientacion) & orientacion == "defecto"
        valor[defecto] <- 1 - valor[defecto]
        # Una metrica `no_aplica` es, por definicion del paquete, una metrica NO
        # ACOTADA -dias de atraso, por ejemplo-. Un umbral en [0, 1] no puede
        # juzgarla: `30 > 0.5` es cierto y no significa nada, y antes esta
        # fabrica devolvia "cumple" para 30, 60 y 90 dias de atraso por igual.
        #
        # No se inventa un juicio ni se cuenta como incumplimiento -que tampoco
        # es-: se para, nombrando la metrica y el porque, para que quien evalua
        # declare una regla propia con la escala de esa medida.
        no_aplica <- !is.na(orientacion) & orientacion == "no_aplica"
        if (any(no_aplica)) {
          stop(
            "El perfil de madurez usa un umbral en [0, 1] y no puede juzgar una ",
            "metrica no acotada. Declara orientacion `no_aplica`: ",
            paste(unique(seq_along(valor)[no_aplica]), collapse = ", "),
            " de ", length(valor), " medida(s). Declare una regla propia para ",
            "esa metrica, con la escala que le corresponde.",
            call. = FALSE
          )
        }
      }
      valor > umbral
    },
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
      any(!nzchar(names(umbrales))) ||
      anyDuplicated(.nombres_para_operar(names(umbrales)))) {
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

.validar_medicion_evaluacion <- function(medicion,
                                         permitir_suprimidas = FALSE) {
  requeridas <- c(
    "id_medida", "id_medicion", "fecha", "metrica_instanciada",
    "tipo_resultado", "resultado"
  )
  # Un solo mensaje cubria TRES casos distintos, y en uno de ellos afirmaba algo
  # falso: una medicion que si viene de `medir()` y quedo sin filas -porque
  # ninguna metrica fue aplicable, cosa que `medir()` DECLARA en su
  # `cobertura_metricas`- era rechazada con "debe ser ... producido por
  # medir()". El paquete acusaba a la entrada de no ser lo que es, teniendo el
  # motivo verdadero a mano.
  if (!inherits(medicion, "data.frame") ||
      !all(.identificadores_en(requeridas, names(medicion)))) {
    stop("`medicion` debe ser un data frame producido por medir().",
         call. = FALSE)
  }
  if (!nrow(medicion)) {
    .error_medicion_sin_medidas(medicion, "medicion", "`medir()`")
  }
  medicion <- .tabla_base(medicion)
  suprimidas <- if (.identificadores_en("objeto_medible", names(medicion))) {
    !is.na(medicion$objeto_medible) & grepl(
      "[valor suprimido]", medicion$objeto_medible, fixed = TRUE
    )
  } else {
    rep(FALSE, nrow(medicion))
  }
  if (anyNA(medicion$resultado) &&
      any(!suprimidas & is.na(medicion$resultado))) {
    stop("Los resultados de la medici\u00f3n no respetan su tipo declarado.",
         call. = FALSE)
  }
  valores <- if (isTRUE(permitir_suprimidas)) {
    medicion$resultado[!suprimidas]
  } else {
    medicion$resultado
  }
  if (!.resultados_validos_tipo(
    valores, medicion$tipo_resultado[if (isTRUE(permitir_suprimidas)) {
      !suprimidas
    } else {
      rep(TRUE, nrow(medicion))
    }]
  )) {
    stop("Los resultados de la medici\u00f3n no respetan su tipo declarado.",
         call. = FALSE)
  }
  .orientacion_medidas(medicion)
  medicion
}

.configuracion_perfil_evaluacion <- function(perfil) {
  if (!inherits(perfil, "perfil_evaluacion")) return(NULL)
  reglas <- vapply(perfil$reglas, function(regla) {
    .texto_configuracion_calidad(list(
      nombre = regla$nombre,
      metricas = regla$metricas,
      nivel = if (is.null(regla$nivel)) "medida" else regla$nivel,
      proporcion_minima = regla$proporcion_minima,
      desenlace = regla$desenlace,
      umbrales = regla$umbrales,
      condicion = regla$condicion
    ))
  }, character(1L))
  names(reglas) <- vapply(perfil$reglas, `[[`, character(1L), "nombre")
  list(version = 1L, nombre = perfil$nombre, reglas = reglas)
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
      indices_umbrales <- .indice_identificador(names(umbrales), argumentos)
      which(!is.na(indices_umbrales))
    }
    extra_umbrales <- umbrales[admitidos]
    if (is.numeric(admitidos)) {
      nombres_formales <- argumentos[
        .indice_identificador(names(umbrales)[admitidos], argumentos)
      ]
      names(extra_umbrales) <- nombres_formales
    }
    extra <- c(extra, extra_umbrales)
  }
  do.call(condicion, c(list(resultado), extra))
}

.evaluar_regla_medidas <- function(medicion, perfil, regla) {
  seleccion <- if (is.null(regla$metricas)) {
    rep(TRUE, nrow(medicion))
  } else {
    .identificadores_en(medicion$metrica_instanciada, regla$metricas)
  }
  medidas <- medicion[seleccion, , drop = FALSE]
  if (!nrow(medidas)) {
    cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
    faltantes <- if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
      if (is.null(regla$metricas)) cobertura else cobertura[
        .identificadores_en(
          cobertura$metrica_instanciada, regla$metricas
        ), , drop = FALSE
      ]
    } else cobertura
    if (inherits(faltantes, "data.frame") && nrow(faltantes)) {
      return(.evaluaciones_medidas_vacias())
    }
    solicitadas <- .identificadores_setdiff(
      regla$metricas, medicion$metrica_instanciada
    )
    disponibles <- .identificadores_unicos(medicion$metrica_instanciada)
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

.evaluaciones_medidas_vacias <- function() {
  data.frame(
    id_medida = character(), id_medicion = character(),
    fecha = as.POSIXct(character()), perfil = character(), regla = character(),
    metrica_instanciada = character(), orientacion = character(),
    resultado = logical(), stringsAsFactors = FALSE
  )
}

.resumir_evaluaciones_regla <- function(evaluaciones) {
  if (!nrow(evaluaciones)) {
    return(data.frame(
      id_medicion = character(), fecha = as.POSIXct(character()),
      perfil = character(), regla = character(), n_medidas = integer(),
      resultado = numeric(), stringsAsFactors = FALSE
    ))
  }
  clave <- interaction(
    .nombres_para_operar(evaluaciones$id_medicion),
    .nombres_para_operar(evaluaciones$perfil),
    .nombres_para_operar(evaluaciones$regla),
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

.completar_evaluaciones_regla <- function(resumen, medicion, perfil) {
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  if (!inherits(cobertura, "data.frame") || !nrow(cobertura)) {
    return(resumen)
  }
  ids <- .identificadores_unicos(
    c(as.character(medicion$id_medicion), cobertura$id_medicion)
  )
  partes <- list(resumen)
  for (id in ids) {
    faltantes_id <- cobertura[
      .identificadores_en(as.character(cobertura$id_medicion), id), ,
      drop = FALSE
    ]
    if (!nrow(faltantes_id)) next
    for (regla in perfil$reglas) {
      faltantes <- if (is.null(regla$metricas)) {
        faltantes_id
      } else {
        faltantes_id[
        .identificadores_en(
          faltantes_id$metrica_instanciada, regla$metricas
        ), , drop = FALSE
        ]
      }
      if (!nrow(faltantes)) next
      indice <- which(
        .identificadores_en(as.character(resumen$id_medicion), id) &
          .identificadores_en(resumen$perfil, perfil$nombre) &
          .identificadores_en(resumen$regla, regla$nombre)
      )
      if (length(indice)) {
        resumen$n_medidas[indice] <- NA_integer_
        resumen$resultado[indice] <- NA_real_
      } else {
        fecha <- faltantes$fecha[[1L]]
        partes[[length(partes) + 1L]] <- data.frame(
          id_medicion = id, fecha = as.POSIXct(fecha),
          perfil = perfil$nombre, regla = regla$nombre,
          n_medidas = NA_integer_, resultado = NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  partes[[1L]] <- resumen
  salida <- do.call(rbind, partes)
  rownames(salida) <- NULL
  salida
}

.declarar_reglas_agregadas <- function(resumen, evaluaciones, perfil) {
  es_agregada <- vapply(
    perfil$reglas, function(regla) !is.null(regla$proporcion_minima),
    logical(1L)
  )
  if (!any(es_agregada)) return(resumen)

  resumen$nivel <- ifelse(
    .identificadores_en(resumen$regla, names(perfil$reglas)[es_agregada]),
    "agregado", "medida"
  )
  resumen$n_cumplen <- integer(nrow(resumen))
  resumen$universo <- character(nrow(resumen))
  resumen$proporcion_minima <- NA_real_
  resumen$cumple <- NA
  for (i in seq_len(nrow(resumen))) {
    indices <- .identificadores_en(
        evaluaciones$id_medicion, resumen$id_medicion[[i]]
      ) &
      .identificadores_en(evaluaciones$perfil, resumen$perfil[[i]]) &
      .identificadores_en(evaluaciones$regla, resumen$regla[[i]])
    componentes <- evaluaciones[indices, , drop = FALSE]
    resumen$n_cumplen[[i]] <- sum(componentes$resultado)
    metricas <- .identificadores_unicos(componentes$metrica_instanciada)
    resumen$universo[[i]] <- paste0(
      nrow(componentes), " medidas seleccionadas: ",
      paste(metricas, collapse = ", ")
    )
    indice_regla <- .indice_identificador(
      resumen$regla[[i]], names(perfil$reglas)
    )
    regla <- perfil$reglas[[indice_regla]]
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
    .nombres_para_operar(evaluaciones$id_medicion),
    .nombres_para_operar(evaluaciones$perfil),
    drop = TRUE, lex.order = TRUE
  )
  grupos <- split(seq_len(nrow(evaluaciones)), clave, drop = TRUE)
  partes <- lapply(grupos, function(indices) {
    primera <- indices[[1L]]
    data.frame(
      id_medicion = evaluaciones$id_medicion[[primera]],
      fecha = evaluaciones$fecha[primera],
      perfil = evaluaciones$perfil[[primera]],
      n_reglas = if (anyNA(evaluaciones$resultado[indices])) {
        NA_integer_
      } else length(indices),
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
    incumplidas <- .identificadores_en(evaluaciones$regla, regla$nombre) &
      !evaluaciones$resultado
    if (!any(incumplidas)) return(NULL)
    indices <- .indice_identificador(
      evaluaciones$id_medida[incumplidas], medicion$id_medida
    )
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

.desenlaces_de_objeto <- function(x) {
  desenlaces <- if (inherits(x, "evaluacion_calidad")) {
    x$desenlaces
  } else if (inherits(x, "analisis")) {
    x$evaluacion$desenlaces
  } else {
    attr(x, "desenlaces", exact = TRUE)
  }
  if (!inherits(desenlaces, "data.frame") || !nrow(desenlaces) ||
      !"desenlace" %in% names(desenlaces)) {
    return(NULL)
  }
  desenlaces[desenlaces$desenlace == "suprimir", , drop = FALSE]
}

.filas_desenlaces <- function(x, desenlaces) {
  requeridas <- c("id_medida", "id_medicion", "metrica_instanciada")
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(requeridas %in% names(x)) ||
      !all(requeridas %in% names(desenlaces))) {
    return(rep(FALSE, if (inherits(x, "data.frame")) nrow(x) else 0L))
  }
  clave <- function(tabla, incluir_medida = TRUE) {
    campos <- c("id_medicion", "metrica_instanciada")
    if (incluir_medida) campos <- c("id_medida", campos)
    # `unname()`: los nombres de columna son datos del usuario y llegan como
    # argumentos con nombre a `paste`. Una columna llamada `sep` o `recycle0`
    # choca con sus formales y aborta la corrida.
    do.call(paste, c(
      lapply(unname(tabla[campos]), .nombres_para_operar), sep = "\r"
    ))
  }
  exactas <- clave(x) %in% clave(desenlaces)
  if (any(exactas)) return(exactas)
  clave(x, FALSE) %in% clave(desenlaces, FALSE)
}

.proteger_medicion_desenlaces <- function(
    x, desenlaces, reemplazo = "[valor suprimido]", marcar_objeto = FALSE) {
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(c("resultado", "id_medida", "id_medicion",
             "metrica_instanciada") %in% names(x))) {
    return(x)
  }
  suprimidas <- .filas_desenlaces(x, desenlaces)
  if (!any(suprimidas)) return(x)
  if (length(reemplazo) == 1L && is.na(reemplazo)) {
    x$resultado <- as.numeric(x$resultado)
    x$resultado[suprimidas] <- NA_real_
  } else {
    x$resultado <- as.character(x$resultado)
    x$resultado[suprimidas] <- as.character(reemplazo)
  }
  if (isTRUE(marcar_objeto) && "objeto_medible" %in% names(x)) {
    x$objeto_medible <- as.character(x$objeto_medible)
    ya_marcadas <- !is.na(x$objeto_medible) & grepl(
      "[valor suprimido]", x$objeto_medible, fixed = TRUE
    )
    nuevas <- suprimidas & !ya_marcadas
    x$objeto_medible[nuevas] <- paste0(
      x$objeto_medible[nuevas], " [valor suprimido]"
    )
  }
  x
}

.proteger_tablero_desenlaces <- function(x, desenlaces) {
  if (!inherits(x, "data.frame") || !nrow(x) || is.null(desenlaces) ||
      !all(c("metrica", "valor") %in% names(x)) ||
      !"metrica_instanciada" %in% names(desenlaces)) {
    return(x)
  }
  metricas <- sub("@.*$", "", as.character(desenlaces$metrica_instanciada))
  suprimidas <- .identificadores_en(as.character(x$metrica), metricas)
  if (!any(suprimidas)) return(x)
  x$valor <- as.character(x$valor)
  x$valor[suprimidas] <- "[valor suprimido]"
  x
}

.proteger_evaluacion_desenlaces <- function(x, reemplazo = "[valor suprimido]",
                                            incluir_medidas = TRUE) {
  if (!inherits(x$desenlaces, "data.frame") ||
      !nrow(x$desenlaces) || !"valor_medido" %in% names(x$desenlaces)) {
    return(x)
  }
  suprimidas <- x$desenlaces$desenlace == "suprimir"
  if (any(suprimidas)) {
    x$desenlaces$valor_medido <- as.character(x$desenlaces$valor_medido)
    x$desenlaces$valor_medido[suprimidas] <- "[valor suprimido]"
  }
  # `desenlace = "suprimir"` declara que las medidas que no cumplen la
  # condicion **no deben publicarse**, y aca se enmascaraba solo su
  # `valor_medido`. Medido sobre un informe: la tabla de desenlaces mostraba
  # `[valor suprimido]` y, dos secciones mas abajo, la tabla de medidas
  # publicaba el resultado de la MISMA medida -"no"-. Enmascarar el valor y
  # publicar el desenlace es suprimir a medias.
  #
  # Se usa el mismo auxiliar que la medicion, con su mismo idioma: en el objeto
  # que se guarda el resultado va `NA` -para que quien lo consuma siga viendo un
  # numero y no un texto- y en lo que se publica va la marca.
  # Solo en lo que se PUBLICA. En el objeto que se guarda la tabla `medidas`
  # no tiene donde llevar la marca -no trae `objeto_medible`, que es como el
  # historico reconoce una supresion legitima-, asi que enmascarar ahi dejaria
  # un `NA` indistinguible de una medida que falta.
  if (isTRUE(incluir_medidas) && any(suprimidas) &&
      inherits(x$medidas, "data.frame") &&
      "resultado" %in% names(x$medidas)) {
    filas <- .filas_desenlaces(x$medidas, x$desenlaces)
    if (any(filas)) {
      if (length(reemplazo) == 1L && is.na(reemplazo)) {
        # `NA` **del tipo que ya tiene**: la tabla `medidas` valida su contrato
        # y `resultado` es logico; convertirla a numerico -como hace el auxiliar
        # de la medicion, donde el resultado SI es numerico- la rompe al leerla
        # desde el historico.
        x$medidas$resultado[filas] <- NA
      } else {
        x$medidas$resultado <- as.character(x$medidas$resultado)
        x$medidas$resultado[filas] <- as.character(reemplazo)
      }
    }
  }
  x
}

#' @export
print.medicion <- function(x, ...) {
  visible <- .proteger_medicion_desenlaces(
    x, .desenlaces_de_objeto(x)
  )
  .print_data_frame_bytes(visible, ...)
  invisible(x)
}

#' @export
print.evaluacion_calidad <- function(x, ...) {
  original <- x
  # La tabla de medidas se imprime entera, asi que una medida suprimida
  # publicaba su resultado tambien por esta puerta.
  x <- .proteger_evaluacion_desenlaces(x)
  x <- .marcar_objeto_para_exhibir(x)
  cli::cli_h1("Evaluaci\u00f3n de calidad")
  cli::cli_h2("Evaluaciones de medidas")
  .print_data_frame_bytes(x$medidas, row.names = FALSE, ...)
  cli::cli_h2("Evaluaciones de reglas")
  .print_data_frame_bytes(x$reglas, row.names = FALSE, ...)
  cli::cli_h2("Perfiles de madurez")
  .print_data_frame_bytes(x$perfiles, row.names = FALSE, ...)
  desenlaces <- .desenlaces_de_objeto(x)
  if (!is.null(desenlaces)) {
    cli::cli_h2("Plan de desenlaces")
    protegido <- .proteger_evaluacion_desenlaces(x)
    .print_data_frame_bytes(protegido$desenlaces, row.names = FALSE, ...)
  }
  invisible(original)
}

#' Evaluar medidas, reglas y perfiles
#'
#' Ejecuta la cadena formal: condición por medida, proporción de medidas que
#' cumplen cada regla y media aritmética simple, no ponderada, de las reglas
#' del perfil. Los resultados por regla se conservan en `reglas`; el resumen de
#' `perfiles` no sustituye esa distribución.
#'
#' @param medicion **Primer argumento.** Data frame producido por `medir()`;
#'   puede reunir varias corridas si conserva sus `id_medicion`. No es el
#'   `perfil` descriptivo que devuelve [perfilar()].
#' @param perfil **Segundo argumento.** Objeto creado por
#'   `perfil_evaluacion()`, que reúne las reglas que se aplican a la medición.
#'   Es un perfil de evaluación, distinto del objeto `perfil` creado por
#'   [perfilar()].
#'
#' @return Objeto `evaluacion_calidad` con tres data frames filtrables:
#'   `medidas`, `reglas` y `perfiles`. Si alguna regla declara un desenlace,
#'   contiene además `desenlaces`, un plan que identifica las medidas
#'   incumplidas, el valor medido, el motivo y la regla que lo produjo.
#'   Cuando una métrica no pudo medirse, conserva `cobertura_metricas` y deja
#'   en `NA` el resumen afectado, en lugar de tratar la ausencia como éxito.
#'   Conserva además, en atributos, la configuración del modelo, la
#'   aplicabilidad y el perfil de evaluación que produjo el resultado.
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
    recibido <- if (inherits(perfil, "perfil")) {
      "un perfil creado por perfilar()"
    } else {
      paste0("un objeto de clase ", paste(class(perfil), collapse = "/"))
    }
    stop(
      "El segundo argumento `perfil` debe ser un perfil creado por ",
      "perfil_evaluacion(); se recibio ", recibido, ".",
      call. = FALSE
    )
  }
  evaluaciones_medidas <- do.call(rbind, lapply(perfil$reglas, function(regla) {
    .evaluar_regla_medidas(medicion, perfil, regla)
  }))
  rownames(evaluaciones_medidas) <- NULL
  evaluaciones_reglas <- .resumir_evaluaciones_regla(evaluaciones_medidas)
  evaluaciones_reglas <- .completar_evaluaciones_regla(
    evaluaciones_reglas, medicion, perfil
  )
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
  attr(estructura, "configuracion_modelo") <- attr(
    medicion, "configuracion_modelo", exact = TRUE
  )
  attr(estructura, "configuracion_aplicabilidad") <- attr(
    medicion, "configuracion_aplicabilidad", exact = TRUE
  )
  attr(estructura, "configuracion_perfil") <-
    .configuracion_perfil_evaluacion(perfil)
  attr(estructura, "perfil_evaluacion") <- perfil
  cobertura <- attr(medicion, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    estructura$cobertura_metricas <- cobertura
    attr(estructura, "cobertura_metricas") <- cobertura
  }
  desenlaces <- .planificar_desenlaces(
    medicion, evaluaciones_medidas, perfil
  )
  if (!is.null(desenlaces)) estructura$desenlaces <- desenlaces
  class(estructura) <- "evaluacion_calidad"
  # La marca de si la fecha la declaro quien llama viaja con la evaluacion:
  # `comparar_evaluaciones()` la necesita para saber si las dos fechas son una
  # serie o dos relojes de la misma sesion.
  attr(estructura, "fecha_declarada") <-
    isTRUE(attr(medicion, "fecha_declarada", exact = TRUE))
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
  if (length(.identificadores_unicos(a$id_medicion)) != 1L ||
      length(.identificadores_unicos(b$id_medicion)) != 1L) {
    stop("Cada evaluaci\u00f3n debe contener una sola corrida.", call. = FALSE)
  }
  # La misma razon que en `comparar_perfiles()`: con las corridas cambiadas, el
  # delta publicado dice que bajo lo que subio, y la fila llama "anterior" a la
  # fecha mas nueva.
  .exigir_orden_temporal(
    if (nrow(a)) a$fecha[[1L]] else NULL,
    if (nrow(b)) b$fecha[[1L]] else NULL,
    "comparar_evaluaciones()",
    declaradas = isTRUE(attr(anterior, "fecha_declarada", exact = TRUE)) &&
      isTRUE(attr(actual, "fecha_declarada", exact = TRUE))
  )
  a <- a[c("perfil", "id_medicion", "fecha", "resultado")]
  b <- b[c("perfil", "id_medicion", "fecha", "resultado")]
  a$perfil_operativo <- .nombres_para_operar(a$perfil)
  b$perfil_operativo <- .nombres_para_operar(b$perfil)
  combinado <- merge(
    a, b, by = "perfil_operativo", suffixes = c("_anterior", "_actual"),
    all = TRUE, sort = FALSE
  )
  perfil <- combinado$perfil_actual
  presentes <- !is.na(combinado$perfil_anterior)
  perfil[presentes] <- combinado$perfil_anterior[presentes]
  combinado$perfil <- perfil
  combinado <- combinado[c(
    "perfil", "id_medicion_anterior", "fecha_anterior", "resultado_anterior",
    "id_medicion_actual", "fecha_actual", "resultado_actual"
  )]
  combinado$delta <- combinado$resultado_actual - combinado$resultado_anterior
  combinado
}

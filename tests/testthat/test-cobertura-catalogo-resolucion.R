# La cobertura debe recomendar lo que el catalogo implementa.
testthat::skip_on_cran()

.expecta_medicion_receta <- function(x, nombre) {
  testthat::expect_false(
    inherits(x, "error"),
    info = paste(nombre, if (inherits(x, "error")) conditionMessage(x))
  )
  if (inherits(x, "error")) return(invisible(NULL))
  testthat::expect_s3_class(x, "medicion")
  testthat::expect_gt(nrow(x), 0L)
  cobertura <- attr(x, "cobertura_metricas", exact = TRUE)
  if (inherits(cobertura, "data.frame") && nrow(cobertura)) {
    estados <- as.character(cobertura$estado)
    testthat::expect_false(
      any(estados %in% c("contrato_incompleto", "no_medible")),
      info = nombre
    )
  }
}

testthat::test_that("cada especificacion ejecuta una metrica medible", {
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  for (nombre in names(especificaciones)) {
    especificacion <- especificaciones[[nombre]]
    if (!is.null(especificacion$paquete)) {
      testthat::skip_if_not_installed(especificacion$paquete)
    }
    for (indice in seq_along(especificacion$configuraciones)) {
      salida <- tryCatch(
        lupa:::.medir_especificacion_catalogo(especificacion, indice),
        error = identity
      )
      .expecta_medicion_receta(
        salida, paste0(nombre, "[", indice, "]")
      )
    }
  }
})

testthat::test_that("el texto publicado conserva todos los pasos", {
  errores <- lupa:::.errores_textos_catalogo()
  testthat::expect_true(
    !length(errores), info = paste(errores, collapse = " | ")
  )
})

.identificadores_en_codigo <- function(texto) {
  encontrados <- regmatches(
    texto,
    gregexpr(
      "`[^`]+`|[A-Za-z][A-Za-z0-9_]*\\(\\)\\$[A-Za-z][A-Za-z0-9_]*",
      texto, perl = TRUE, useBytes = TRUE
    )
  )[[1L]]
  if (identical(encontrados, -1L)) return(character())
  vapply(encontrados, function(x) {
    if (startsWith(x, "`") && endsWith(x, "`")) {
      substring(x, 2L, nchar(x) - 1L)
    } else {
      x
    }
  }, character(1L))
}

.validar_identificador_publicado <- function(identificador, especificaciones,
                                             nucleo, referenciales) {
  if (!is.null(especificaciones$nombre)) {
    especificaciones <- list(especificaciones)
  }
  exportadas <- getNamespaceExports("lupa")
  metrica <- regexec(
    "^([A-Za-z][A-Za-z0-9_]*)\\(\\)\\$([A-Za-z][A-Za-z0-9_]*)$",
    identificador, perl = TRUE
  )
  partes <- regmatches(identificador, metrica)[[1L]]
  if (length(partes)) {
    funcion <- partes[[2L]]
    nombre <- partes[[3L]]
    catalogo <- if (identical(funcion, "metricas_nucleo")) nucleo else
      if (identical(funcion, "metricas_referencial")) referenciales else NULL
    testthat::expect_false(
      is.null(catalogo), info = identificador
    )
    if (!is.null(catalogo)) {
      testthat::expect_true(nombre %in% names(catalogo), info = identificador)
      testthat::expect_true(
        any(vapply(especificaciones, function(x) {
          identical(nombre, x$nombre)
        }, logical(1L))),
        info = identificador
      )
    }
    return(invisible(NULL))
  }
  funcion <- sub("\\(.*$", "", identificador)
  if (identificador %in% exportadas || funcion %in% exportadas) {
    return(invisible(NULL))
  }
  admitidos <- unique(c(
    unlist(lapply(especificaciones, `[[`, "propiedades"),
           use.names = FALSE),
    unlist(lapply(especificaciones, function(x) x$contrato$formales),
           use.names = FALSE),
    names(formals(instanciar))
  ))
  testthat::expect_true(identificador %in% admitidos, info = identificador)
  invisible(NULL)
}

testthat::test_that("el texto publicado nombra la especificacion real", {
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  catalogo <- catalogo_agesic()
  nucleo <- metricas_nucleo()
  referenciales <- metricas_referencial()
  claves <- lupa:::.clave_par_identificador(
    catalogo$dimension, catalogo$factor
  )
  marco <- marco_agesic()
  for (i in seq_len(nrow(marco$factores))) {
    seleccion <- claves == lupa:::.clave_par_identificador(
      marco$factores$dimension[[i]], marco$factores$factor[[i]]
    ) & as.character(catalogo$estado) %in%
      c("implementada", "via_agregacion")
    nombres <- lupa:::.identificadores_unicos(catalogo$metrica_lupa[seleccion])
    grupo <- unname(especificaciones[nombres])
    texto <- as.character(marco$factores$como_resolverlo[[i]])
    identificadores <- .identificadores_en_codigo(texto)
    for (identificador in identificadores) {
      .validar_identificador_publicado(
        identificador, grupo, nucleo, referenciales
      )
    }
    identificadores_base <- sub("\\(.*$", "", identificadores)
    for (especificacion in grupo) {
      for (token in especificacion$tokens_obligatorios) {
        testthat::expect_true(
          token %in% c(identificadores, identificadores_base),
          info = paste(especificacion$nombre, token, sep = ": ")
        )
      }
      codigo_metrica <- paste0(
        especificacion$catalogo, "()$", especificacion$nombre
      )
      testthat::expect_true(
        codigo_metrica %in% identificadores,
        info = especificacion$nombre
      )
    }
  }
})

testthat::test_that("AGESIC publica una receta completa por metrica", {
  testthat::expect_false(
    exists(".texto_grupo_especificaciones", envir = asNamespace("lupa"),
           inherits = FALSE)
  )
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  recetas <- lupa:::.recetas_metricas_catalogo(especificaciones)
  catalogo <- catalogo_agesic()
  claves <- lupa:::.clave_par_identificador(
    catalogo$dimension, catalogo$factor
  )
  marco <- marco_agesic()
  for (i in seq_len(nrow(marco$factores))) {
    seleccion <- claves == lupa:::.clave_par_identificador(
      marco$factores$dimension[[i]], marco$factores$factor[[i]]
    ) & as.character(catalogo$estado) %in%
      c("implementada", "via_agregacion")
    nombres <- lupa:::.identificadores_unicos(catalogo$metrica_lupa[seleccion])
    esperado <- if (length(nombres)) {
      paste(unname(recetas[nombres]), collapse = " ")
    } else {
      "Requiere un backend o referencial especializado que no integra esta versi\u00f3n."
    }
    testthat::expect_identical(
      as.character(marco$factores$como_resolverlo[[i]]), esperado,
      info = as.character(marco$factores$factor[[i]])
    )
  }
})

testthat::test_that("la publicacion de AGESIC conserva cada cadena de receta", {
  marco <- marco_agesic()
  textos <- as.character(marco$factores$como_resolverlo)
  testthat::expect_false(any(!nzchar(textos)))
  testthat::expect_true(any(grepl(
    "metricas_nucleo()$AtributoDuplicado", textos,
    fixed = TRUE, useBytes = TRUE
  )))
  testthat::expect_true(any(grepl(
    "metricas_nucleo()$ConjuntoAtributosDuplicado", textos,
    fixed = TRUE, useBytes = TRUE
  )))
  testthat::expect_true(any(grepl(
    "metricas_nucleo()$EntidadDuplicada", textos,
    fixed = TRUE, useBytes = TRUE
  )))
  no_contradiccion <- textos[grepl(
    "No-contradicci\u00f3n", as.character(marco$factores$factor),
    fixed = TRUE, useBytes = TRUE
  )]
  testthat::expect_length(no_contradiccion, 1L)
  testthat::expect_match(no_contradiccion, "EntidadContradictoria", fixed = TRUE)
  testthat::expect_match(no_contradiccion, "stringdist", fixed = TRUE)
})

testthat::test_that("cada receta publica el contrato en su llamada receptora", {
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  recetas <- lupa:::.recetas_metricas_catalogo(especificaciones)
  for (nombre in names(especificaciones)) {
    errores <- lupa:::.errores_texto_especificacion(
      recetas[[nombre]], especificaciones[[nombre]]
    )
    testthat::expect_true(!length(errores), info = nombre)
  }
})

testthat::test_that("EntidadContradictoria mide distancia y conserva su ratio", {
  testthat::skip_if_not_installed("stringdist")
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  medicion <- lupa:::.medir_especificacion_catalogo(
    especificaciones$EntidadContradictoria
  )
  testthat::expect_equal(medicion$metrica[[1L]], "EntidadContradictoria")
  testthat::expect_equal(medicion$agregacion[[1L]], "ratio")
  testthat::expect_equal(medicion$resultado[[1L]], 2 / 3)
})

testthat::test_that("cada rama temporal se prueba por separado", {
  especificaciones <- lupa:::.especificaciones_metricas_catalogo()
  fecha <- especificaciones$DesactualizacionPorFecha
  testthat::expect_length(fecha$contrato$alternativas, 2L)
  for (indice in seq_along(fecha$configuraciones)) {
    salida <- lupa:::.medir_especificacion_catalogo(fecha, indice)
    .expecta_medicion_receta(
      salida, paste0("DesactualizacionPorFecha[", indice, "]")
    )
  }
  fuerte <- lupa:::.medir_especificacion_catalogo(
    especificaciones$CorrectitudSemFuerte
  )
  debil <- lupa:::.medir_especificacion_catalogo(
    especificaciones$CorrectitudSemDebil
  )
  .expecta_medicion_receta(fuerte, "CorrectitudSemFuerte")
  .expecta_medicion_receta(debil, "CorrectitudSemDebil")
})

testthat::test_that("los factores implementados no caen en el remedio de ausencia", {
  catalogo <- catalogo_agesic()
  claves <- lupa:::.clave_par_identificador(
    catalogo$dimension, catalogo$factor
  )
  implementadas <- as.character(catalogo$estado) %in%
    c("implementada", "via_agregacion")
  for (clave in lupa:::.identificadores_unicos(claves[implementadas])) {
    partes <- strsplit(clave, "|", fixed = TRUE)[[1L]]
    texto <- lupa:::.resolver_factor(partes[[1L]], partes[[2L]], catalogo)
    testthat::expect_false(grepl(
      "Requiere un backend o referencial especializado",
      texto, fixed = TRUE, useBytes = TRUE
    ), info = clave)
  }
})

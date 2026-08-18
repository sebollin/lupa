if (!exists(".tablas_limpias_r107", mode = "function")) {
  .r107_path <- if (file.exists("test-ronda107.R")) {
    "test-ronda107.R"
  } else {
    file.path("tests", "testthat", "test-ronda107.R")
  }
  .r107_ast <- parse(file = .r107_path)
  eval(.r107_ast[[1L]], envir = environment())
}

.tamanos_r107 <- c(
  0L, 1L, 2L, 3L, 4L, 5L, 9L, 10L, 11L, 19L, 20L, 21L, 24L,
  49L, 50L, 51L, 80L, 99L, 100L, 101L
)

.razon_salto_r107 <- function(nombre, n, filas) {
  if (identical(nombre, "lognormal")) {
    return(paste0(
      "El generador conserva 80 observaciones para esta tabla; ",
      "no representa el tamano solicitado.")
    )
  }
  if (identical(nombre, "faltantes_declarados")) {
    return(paste0(
      "El generador fija las posiciones de ausencia 8 y 24 y extiende ",
      "la tabla hasta la fila 24.")
    )
  }
  paste0(
    "El generador produjo ", filas, " filas para n=", n,
    "; la tabla no es comparable en ese tamano."
  )
}

.preparar_tablas_r107 <- function(n) {
  tablas <- .tablas_limpias_r107(n)
  filas <- vapply(tablas, nrow, integer(1L))
  salteadas <- filas != n
  registro <- if (any(salteadas)) {
    data.frame(
      n = rep.int(n, sum(salteadas)),
      tabla = names(tablas)[salteadas],
      filas_generadas = unname(filas[salteadas]),
      motivo = vapply(
        seq_along(filas[salteadas]),
        function(j) .razon_salto_r107(
          names(tablas)[salteadas][[j]], n, filas[salteadas][[j]]
        ),
        character(1L)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      n = integer(), tabla = character(), filas_generadas = integer(),
      motivo = character(), stringsAsFactors = FALSE
    )
  }
  if (nrow(registro)) {
    message(
      paste(
        paste0(
          "n=", registro$n, ": se saltea ", registro$tabla,
          " (", registro$motivo, ")"
        ),
        collapse = "\n"
      )
    )
  }
  list(
    tablas = tablas[!salteadas],
    salteadas = registro
  )
}

.afirmaciones_base_r107 <- c(
  "booleanos_texto::vigente::tipo_declarado_distinto",
  "decimales_con_coma::importe::numero_como_texto",
  "decimales_con_coma::importe::tipo_declarado_distinto",
  "fechas_como_texto::fecha_texto::tipo_declarado_distinto",
  "moneda_unica::precio::numero_como_texto",
  "porcentajes_texto::porcentaje::numero_como_texto",
  "unidad_unica::peso::numero_como_texto"
)

# En la tabla de factores, `region` toma cuatro valores repartidos con
# `length.out = n`. Cuando el reparto queda disparejo aparece una asimetria de
# frecuencias entre `este` y `oeste`, que estan a una edicion de distancia, y el
# comparador de vocabulario abre el par con severidad `sospechoso`.
#
# Por forma la afirmacion es cierta y la severidad es la honesta, pero son dos
# puntos cardinales distintos: como sugerencia es un falso positivo. Se declara
# aca en vez de esconderlo. Esta anotado en `PENDIENTES.md` seccion 2.22 junto
# con la medicion que hace falta antes de poner un piso de asimetria en la via
# general de distancia.
.tamanos_con_asimetria_cardinal_r107 <- c(11L, 19L, 51L, 99L)

.esperadas_r107 <- function(n) {
  if (n == 0L) return(character())
  salida <- .afirmaciones_base_r107
  if (n == 80L) {
    salida <- c(salida, "lognormal::duracion::outliers")
  }
  if (n %in% .tamanos_con_asimetria_cardinal_r107) {
    salida <- c(salida, "factores::region::casi_duplicados_vocabulario")
  }
  sort(salida)
}

.cobertura_benford_esperada_r107 <- function(n) {
  if (n %in% c(50L, 51L)) {
    return(sort(c(
      "continuas::medicion", "enteros_acotados::nivel",
      "mixta_realista::casos", "mixta_realista::tasa"
    )))
  }
  if (n %in% c(99L, 100L, 101L)) {
    return(sort(c(
      "continuas::medicion", "enteros_acotados::nivel",
      "faltantes_declarados::conteo", "mixta_realista::casos",
      "mixta_realista::tasa"
    )))
  }
  if (n == 80L) {
    return(sort(c(
      "continuas::medicion", "enteros_acotados::nivel",
      "faltantes_declarados::conteo", "mixta_realista::casos",
      "mixta_realista::tasa", "lognormal::duracion"
    )))
  }
  character()
}

.agregar_tabla_r107 <- function(perfiles, tablas) {
  salida <- lapply(names(perfiles), function(nombre) {
    resultado <- perfiles[[nombre]]
    hallazgos <- resultado$hallazgos
    if (!nrow(hallazgos)) return(NULL)
    hallazgos$tabla <- nombre
    hallazgos
  })
  salida <- Filter(Negate(is.null), salida)
  if (!length(salida)) {
    salida <- data.frame(
      tabla = character(), columna = character(), tipo_hallazgo = character(),
      severidad = factor(
        character(), levels = c("ok", "sospechoso", "error"), ordered = TRUE
      ),
      n_evaluados = numeric(), n_afectados = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    salida <- do.call(rbind, salida)
  }
  salida
}

.agregar_cobertura_r107 <- function(perfiles) {
  salida <- lapply(names(perfiles), function(nombre) {
    cobertura <- perfiles[[nombre]]$cobertura_diagnosticos
    if (!nrow(cobertura)) return(NULL)
    cobertura$tabla <- nombre
    cobertura
  })
  salida <- Filter(Negate(is.null), salida)
  if (!length(salida)) {
    return(data.frame(
      tabla = character(), diagnostico = character(), columna = character(),
      motivo = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, salida)
}

test_that("la bateria r107 conserva cobertura y afirmaciones verdaderas", {
  saltos <- lapply(.tamanos_r107, .preparar_tablas_r107)
  registros_salto <- do.call(rbind, lapply(saltos, `[[`, "salteadas"))
  expect_true(all(nzchar(registros_salto$motivo)))

  for (i in seq_along(.tamanos_r107)) {
    n <- .tamanos_r107[[i]]
    tablas <- saltos[[i]]$tablas
    perfiles <- lapply(tablas, function(datos) {
      tryCatch(
        perfilar(datos, analizar_dependencias = FALSE),
        error = function(error) structure(
          list(mensaje = conditionMessage(error)), class = "error_perfil_r107"
        )
      )
    })
    nombres_error <- names(perfiles)[vapply(
      perfiles, inherits, logical(1L), what = "error_perfil_r107"
    )]
    expect_equal(
      nombres_error, character(),
      info = paste0("n=", n)
    )
    perfiles <- perfiles[!vapply(
      perfiles, inherits, logical(1L), what = "error_perfil_r107"
    )]
    if (!length(perfiles)) next

    hallazgos <- .agregar_tabla_r107(perfiles, tablas)
    severidad <- as.character(hallazgos$severidad)
    expect_equal(
      sum(severidad == "error"), 0L,
      info = paste0("n=", n)
    )
    expect_true(is.factor(hallazgos$severidad), info = paste0("n=", n))
    expect_true(is.ordered(hallazgos$severidad), info = paste0("n=", n))
    expect_identical(
      levels(hallazgos$severidad), c("ok", "sospechoso", "error")
    )
    alcance_columnas <- unlist(lapply(
      perfiles, function(perfil) perfil$columnas$n_filas_analizadas_tipo
    ), use.names = FALSE)
    if (n == 0L) {
      # Con cero filas, `n_filas_analizadas_tipo = 0` es el conteo cierto de
      # filas analizadas y no una afirmacion: la ausencia de resultado se
      # declara en `proporcion_tipo_inferido`, que queda `NA`, y en
      # `tipo_inferido = "desconocido"` para el texto. Se exige eso, no que el
      # conteo sea `NA`.
      proporciones <- unlist(lapply(
        perfiles, function(perfil) perfil$columnas$proporcion_tipo_inferido
      ), use.names = FALSE)
      expect_equal(sum(!is.na(proporciones)), 0L)
    } else {
      expect_equal(sum(!is.na(alcance_columnas) & alcance_columnas == 0L), 0L)
    }

    afirmaciones <- if (nrow(hallazgos)) {
      sort(paste(
        hallazgos$tabla, hallazgos$columna, hallazgos$tipo_hallazgo,
        sep = "::"
      )[severidad != "ok"])
    } else character()
    nuevas <- setdiff(afirmaciones, .esperadas_r107(n))
    if (length(nuevas)) {
      message(
        paste0(
          "n=", n, ": afirmaciones nuevas: ", paste(nuevas, collapse = ", ")
        )
      )
    }
    expect_equal(
      afirmaciones, .esperadas_r107(n),
      info = paste0("n=", n)
    )

    cobertura <- .agregar_cobertura_r107(perfiles)
    expect_true(
      all(nzchar(cobertura$motivo)),
      info = paste0("n=", n)
    )
    cobertura_benford <- cobertura[
      cobertura$diagnostico == "ley_benford", , drop = FALSE
    ]
    cobertura_benford_actual <- if (nrow(cobertura_benford)) {
      sort(paste(
        cobertura_benford$tabla, cobertura_benford$columna, sep = "::"
      ))
    } else character()
    expect_equal(
      cobertura_benford_actual,
      .cobertura_benford_esperada_r107(n),
      info = paste0("cobertura Benford, n=", n)
    )

    alcance_cero <- hallazgos[
      !is.na(hallazgos$n_evaluados) & hallazgos$n_evaluados == 0,
      , drop = FALSE
    ]
    expect_equal(
      nrow(alcance_cero), 0L,
      info = paste0("no hay hallazgos con alcance medido cero, n=", n)
    )
    if (nrow(cobertura)) {
      for (j in seq_len(nrow(cobertura))) {
        fila <- cobertura[j, , drop = FALSE]
        mismo <- hallazgos[
          hallazgos$tabla == fila$tabla &
            hallazgos$columna == fila$columna &
            hallazgos$tipo_hallazgo == fila$diagnostico &
            as.character(hallazgos$severidad) == "ok",
          , drop = FALSE
        ]
        expect_equal(
          nrow(mismo), 0L,
          info = paste0(
            "diagnostico no evaluado sin ok falso: ", fila$tabla,
            "::", fila$columna, "::", fila$diagnostico, " n=", n
          )
        )
        if (identical(fila$diagnostico, "ley_benford")) {
          resultado <- perfiles[[fila$tabla]]$meta$benford$resultados[[
            fila$columna
          ]]
          expect_identical(resultado$aplica, FALSE)
          expect_true(length(resultado$precondiciones_fallidas) > 0L)
        }
      }
    }
  }
})

test_that("las relaciones aritmeticas sin soporte no se presentan como medidas", {
  for (n in .tamanos_r107[.tamanos_r107 < 3L]) {
    tablas <- .preparar_tablas_r107(n)$tablas
    tabla_numerica <- names(tablas)[vapply(tablas, function(datos) {
      sum(vapply(datos, is.numeric, logical(1L))) >= 2L
    }, logical(1L))]
    for (nombre in tabla_numerica) {
      perfil <- perfilar(tablas[[nombre]], analizar_dependencias = FALSE)
      cobertura <- perfil$cobertura_diagnosticos
      diagnostico <- cobertura[
        cobertura$diagnostico == "relacion_aritmetica_columnas", ,
        drop = FALSE
      ]
      expect_equal(
        nrow(diagnostico), 1L,
        info = paste0(
          "n=", n, "; tabla=", nombre,
          "; la relacion aritmetica requiere al menos 3 filas"
        )
      )
      if (nrow(diagnostico) == 1L) {
        expect_true(nzchar(diagnostico$motivo[[1L]]))
      }
    }
  }
})

test_that("casi_clave no se afirma en los bordes de su minimo de filas", {
  for (n in c(99L, 100L, 101L)) {
    tablas <- .preparar_tablas_r107(n)$tablas
    perfiles <- lapply(tablas, perfilar, analizar_dependencias = FALSE)
    hallazgos <- .agregar_tabla_r107(perfiles, tablas)
    casi_clave <- hallazgos[
      hallazgos$tipo_hallazgo == "casi_clave", , drop = FALSE
    ]
    expect_equal(nrow(casi_clave), 0L, info = paste0("n=", n))
  }
})

testthat::skip_on_cran()

.fecha_marcas_y_factores <- as.POSIXct("2026-01-01", tz = "UTC")

testthat::test_that("una clave no unica ofrece una marca no destructiva", {
  datos <- data.frame(
    id = c("A", "B", "A", "C"),
    valor = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, clave = "id", fecha = .fecha_marcas_y_factores,
    analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  accion <- plan[plan$estrategia == "marcar_claves_duplicadas", , drop = FALSE]

  testthat::expect_equal(nrow(accion), 1L)
  testthat::expect_true(accion$recomendada[[1L]])
  testthat::expect_true(accion$aplicar[[1L]])
  testthat::expect_false(accion$destructiva[[1L]])
  testthat::expect_true(lupa:::.marcas_del_plan(".clave_duplicada"))
  testthat::expect_equal(accion$parametros[[1L]]$columna_marca,
                         ".clave_duplicada")

  resultado <- aplicar(plan, datos)
  testthat::expect_identical(
    resultado$datos$.clave_duplicada, c(TRUE, FALSE, TRUE, FALSE)
  )
  testthat::expect_equal(resultado$registro$n_cambiadas, 2)
  testthat::expect_identical(resultado$registro$estado, "ejecutada")
  testthat::expect_equal(resultado$registro$n_filas_eliminadas, 0)
  testthat::expect_equal(nrow(attr(plan, "hallazgos_sin_accion")), 0L)
})

testthat::test_that("el resultado tiene resumen y reporte de auditoria", {
  datos <- data.frame(
    "<columna>" = c(" a ", " a "),
    id = c(1, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .fecha_marcas_y_factores, analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  plan$aplicar <- plan$estrategia %in% c(
    "recortar_espacios", "marcar_filas_duplicadas"
  )
  marca <- which(plan$estrategia == "marcar_filas_duplicadas")
  plan$parametros[[marca]] <- list(columna_marca = "")
  resultado <- aplicar(plan, datos)
  resumen <- summary(resultado)

  testthat::expect_s3_class(resumen, "data.frame")
  testthat::expect_true(all(c(
    "estrategia", "columna", "estado", "error", "n_cambiadas",
    "n_filas_eliminadas"
  ) %in% names(resumen)))
  testthat::expect_true(any(resumen$estado == "ejecutada"))
  testthat::expect_true(any(resumen$estado == "fallida"))
  testthat::expect_true(any(nzchar(resumen$error[resumen$estado == "fallida"])))
  testthat::expect_equal(
    resumen$n_cambiadas[resumen$estrategia == "recortar_espacios"], 2
  )

  archivo <- tempfile(fileext = ".html")
  reportar(resultado, archivo = archivo, fecha = .fecha_marcas_y_factores)
  html <- paste(readLines(archivo, encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  testthat::expect_match(html, "Resultado de limpieza", fixed = TRUE)
  testthat::expect_match(html, "Acciones y cambios", fixed = TRUE)
  testthat::expect_false(grepl("<h3>Filas retiradas</h3>", html, fixed = TRUE))
  testthat::expect_match(html, "marcar_filas_duplicadas", fixed = TRUE)
  testthat::expect_false(grepl(
    "<columna>", html, fixed = TRUE, useBytes = TRUE
  ))
  testthat::expect_match(html, "&lt;columna&gt;", fixed = TRUE)

  archivo_completo <- tempfile(fileext = ".html")
  testthat::expect_no_error(reportar(
    perfil, plan, resultado, archivo = archivo_completo,
    fecha = .fecha_marcas_y_factores
  ))
})

testthat::test_that("la limpieza conserva factores y niveles declarados", {
  datos <- data.frame(
    nivel = factor(
      c(" bajo", "medio ", "bajo", "medio "),
      levels = c(" bajo", "medio ", "bajo", "sin uso"), ordered = TRUE
    )
  )
  perfil <- perfilar(datos, fecha = .fecha_marcas_y_factores,
                     analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia == "recortar_espacios")
  testthat::expect_true(plan$recomendada[[indice]])
  testthat::expect_false(plan$aplicar[[indice]])
  testthat::expect_match(plan$justificacion[[indice]], "devuelve texto",
                         fixed = TRUE)
  testthat::expect_match(plan$justificacion[[indice]], "reconstruir el factor",
                         fixed = TRUE)

  plan$aplicar[[indice]] <- TRUE
  resultado <- aplicar(plan, datos)
  limpio <- resultado$datos$nivel
  testthat::expect_false(is.ordered(limpio))
  testthat::expect_s3_class(limpio, "factor")
  testthat::expect_identical(levels(limpio), c("bajo", "medio", "sin uso"))
  testthat::expect_identical(as.character(limpio),
                             c("bajo", "medio", "bajo", "medio"))
})

testthat::test_that("la conversion de tipo declara que pierde el factor", {
  datos <- data.frame(
    fecha = factor(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .fecha_marcas_y_factores, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia == "convertir_tipo")

  testthat::expect_length(indice, 1L)
  testthat::expect_match(
    plan$justificacion[[indice]], "no puede conservar el factor", fixed = TRUE
  )
  resultado <- aplicar(plan, datos)
  testthat::expect_true(inherits(resultado$datos$fecha, "Date"))
  testthat::expect_false(is.factor(resultado$datos$fecha))
})

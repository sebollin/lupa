testthat::skip_on_cran()

.fecha_casos_adversos <- as.POSIXct("2026-01-01", tz = "UTC")

.aplicar_caso_adverso <- function(datos, clave = NULL) {
  perfil <- suppressWarnings(perfilar(
    datos, clave = clave, fecha = .fecha_casos_adversos,
    analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  list(perfil = perfil, plan = plan, resultado = aplicar(plan, datos))
}

testthat::test_that("los factores conservan niveles sin uso", {
  datos <- data.frame(
    categoria = factor(c(" A", "B"), levels = c(" A", "B", "C"))
  )
  resultado <- .aplicar_caso_adverso(datos)$resultado
  esperado <- data.frame(
    categoria = factor(c("A", "B"), levels = c("A", "B", "C"))
  )
  testthat::expect_identical(resultado$datos$categoria, esperado$categoria)
  testthat::expect_identical(levels(resultado$datos$categoria), levels(esperado$categoria))
})

testthat::test_that("los factores conservan un nivel NA explicito", {
  datos <- data.frame(
    categoria = factor(
      c(" a", "b", NA), levels = c(NA, " a", "b"), exclude = NULL
    )
  )
  resultado <- .aplicar_caso_adverso(datos)$resultado
  esperado <- data.frame(
    categoria = factor(c("a", "b", NA), levels = c(NA, "a", "b"),
                       exclude = NULL)
  )
  testthat::expect_identical(resultado$datos$categoria, esperado$categoria)
  testthat::expect_identical(levels(resultado$datos$categoria), levels(esperado$categoria))
  testthat::expect_false(is.na(resultado$datos$categoria[[3L]]))
})

testthat::test_that("una fusion no contigua quita el orden declarado", {
  datos <- data.frame(
    rango = ordered(c(" A", "B", "A"), levels = c(" A", "B", "A"))
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .fecha_casos_adversos, analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  plan$aplicar[] <- TRUE
  resultado <- aplicar(plan, datos)
  esperado <- factor(c("A", "B", "A"), levels = c("A", "B"))
  testthat::expect_identical(resultado$datos$rango, esperado)
  testthat::expect_identical(levels(resultado$datos$rango), levels(esperado))
  testthat::expect_false(is.ordered(resultado$datos$rango))
  testthat::expect_match(
    resultado$registro$justificacion[[1L]],
    "niveles no contiguos", fixed = TRUE
  )
})

testthat::test_that("un factor sin niveles queda como texto faltante", {
  datos <- data.frame(estado = factor(c("s/d", "s/d", "s/d")))
  resultado <- .aplicar_caso_adverso(datos)$resultado
  testthat::expect_identical(resultado$datos$estado, rep(NA_character_, 3L))
  testthat::expect_false(is.factor(resultado$datos$estado))
})

testthat::test_that("una fusion contigua conserva el orden del factor", {
  datos <- data.frame(
    rango = ordered(c(" A", "B"), levels = c(" A", "B"))
  )
  caso <- .aplicar_caso_adverso(datos)
  caso$plan$aplicar[caso$plan$estrategia == "recortar_espacios"] <- TRUE
  resultado <- aplicar(caso$plan, datos)
  esperado <- ordered(c("A", "B"), levels = c("A", "B"))
  testthat::expect_identical(resultado$datos$rango, esperado)
})

testthat::test_that("la marca de clave se calcula sobre la entrega final", {
  datos <- data.frame(
    id = c("B", "B", "A ", "A"), valor = 1:4,
    stringsAsFactors = FALSE
  )
  caso <- .aplicar_caso_adverso(datos, clave = "id")
  accion <- caso$plan[caso$plan$estrategia == "marcar_claves_duplicadas", , drop = FALSE]
  testthat::expect_gt(accion$orden[[1L]], max(caso$plan$orden[caso$plan$estrategia != "marcar_claves_duplicadas"]))
  resultado <- caso$resultado
  testthat::expect_identical(
    resultado$datos$.clave_duplicada, rep(TRUE, nrow(datos))
  )
  resumen <- summary(resultado)
  fila <- resumen[resumen$estrategia == "marcar_claves_duplicadas", , drop = FALSE]
  testthat::expect_identical(fila$n_cambiadas, 0)
  testthat::expect_identical(fila$n_filas_marcadas, 4)
})

testthat::test_that("la marca de clave no conserva una fila ya unica", {
  datos <- data.frame(id = c(1, 1), valor = c("A", "A"))
  perfil <- suppressWarnings(perfilar(
    datos, clave = "id", fecha = .fecha_casos_adversos,
    analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  plan$aplicar[plan$estrategia == "marcar_filas_duplicadas"] <- FALSE
  plan$aplicar[plan$estrategia == "conservar_primera_duplicada"] <- TRUE
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  testthat::expect_equal(nrow(resultado$datos), 1L)
  testthat::expect_false(resultado$datos$.clave_duplicada[[1L]])
})

testthat::test_that("un plan con marca de clave se puede aplicar dos veces", {
  datos <- data.frame(id = c(1, 1, 2), valor = c("A", "B", "C"))
  caso <- .aplicar_caso_adverso(datos, clave = "id")
  segunda <- aplicar(caso$plan, caso$resultado$datos)
  testthat::expect_false(any(segunda$registro$estado == "fallida"))
  testthat::expect_identical(
    segunda$datos$.clave_duplicada, caso$resultado$datos$.clave_duplicada
  )
})

testthat::test_that("el informe separa celdas, marcas y filas retiradas", {
  datos <- data.frame(id = 1:4, valor = c("A", NA, "B", NA))
  perfil <- suppressWarnings(perfilar(
    datos, fecha = .fecha_casos_adversos, analizar_dependencias = FALSE
  ))
  plan <- planificar_limpieza(perfil, datos)
  plan$aplicar[plan$estrategia == "eliminar_filas_ausentes"] <- TRUE
  plan$aplicar[plan$estrategia == "marcar_filas_ausentes"] <- FALSE
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  resumen <- summary(resultado)
  fila <- resumen[resumen$estrategia == "eliminar_filas_ausentes", , drop = FALSE]
  testthat::expect_identical(fila$n_cambiadas, 0)
  testthat::expect_identical(fila$n_filas_eliminadas, 2)
  archivo <- tempfile(fileext = ".html")
  reportar(resultado, archivo = archivo, fecha = .fecha_casos_adversos)
  html <- paste(readLines(archivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  testthat::expect_match(html, "Celdas cambiadas</td><td>0", fixed = TRUE)
  testthat::expect_match(html, "Filas retiradas</h3>", fixed = TRUE)
  testthat::expect_match(html, "<td>2</td>", fixed = TRUE)
})

testthat::test_that("el informe omite retiros cuando no hubo retiros", {
  datos <- data.frame(nombre = c(" Ana ", " Juan "))
  resultado <- .aplicar_caso_adverso(datos)$resultado
  archivo <- tempfile(fileext = ".html")
  reportar(resultado, archivo = archivo, fecha = .fecha_casos_adversos)
  html <- paste(readLines(archivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  testthat::expect_false(grepl("<h3>Filas retiradas</h3>", html, fixed = TRUE))
})

testthat::test_that("la asociacion con signo queda al final", {
  datos <- data.frame(creciente = 1:6, decreciente = 6:1)
  asociaciones <- detectar_asociaciones(datos, umbral = 0)
  testthat::expect_identical(
    names(asociaciones),
    c("columna_1", "columna_2", "tipo_1", "tipo_2", "metodo",
      "supuesto", "asociacion", "n_pares", "asociacion_con_signo")
  )
  inversa <- asociaciones[
    asociaciones$columna_1 == "creciente" &
      asociaciones$columna_2 == "decreciente", , drop = FALSE
  ]
  testthat::expect_identical(inversa$asociacion, 1)
  testthat::expect_identical(inversa$asociacion_con_signo, -1)
})

test_that("la guarda usa el total previo al truncado", {
  set.seed(20260818)
  datos <- data.frame(x = c(seq_len(5000L), rep(1e6, 1200L)))
  expect_no_warning({
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE, max_filas_hallazgo = 1000L,
      casi_duplicados_vocabulario = FALSE
    )
  }, class = "lupa_trazabilidad_incoherente")
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "outliers", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(hallazgo$n_afectados[[1L]], 1200)
  expect_equal(traza$total, 1200)
  expect_equal(traza$mostrados, 1000)
  expect_equal(traza$estado, "truncada")
  expect_equal(traza$alcance, "completo")
})

test_that("la guarda es bidireccional y consciente de la unidad", {
  categoria <- c(
    rep("alto", 20L), rep("altO", 4L),
    rep("bajo", 20L), rep("BAJO", 6L)
  )
  datos <- data.frame(categoria = categoria)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  indice <- which(
    perfil$hallazgos$tipo_hallazgo == "mayusculas_inconsistentes"
  )
  expect_equal(length(indice), 1L)

  hacia_abajo <- perfil$hallazgos
  hacia_abajo$n_afectados[[indice]] <- 1
  expect_warning(
    lupa:::.advertir_incoherencias_trazabilidad(
      hacia_abajo, datos, "categoria"
    ),
    class = "lupa_trazabilidad_incoherente"
  )

  hacia_arriba <- perfil$hallazgos
  hacia_arriba$n_afectados[[indice]] <- 99
  expect_warning(
    lupa:::.advertir_incoherencias_trazabilidad(
      hacia_arriba, datos, "categoria"
    ),
    class = "lupa_trazabilidad_incoherente"
  )

  expect_no_warning(
    lupa:::.advertir_incoherencias_trazabilidad(
      perfil$hallazgos, datos, "categoria"
    ),
    class = "lupa_trazabilidad_incoherente"
  )
})

test_that("la guarda avisa un hallazgo severo sin afectados", {
  datos <- data.frame(x = c(rep(1, 20L), 1000))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  indice <- which(perfil$hallazgos$tipo_hallazgo == "outliers")
  expect_equal(length(indice), 1L)
  alterado <- perfil$hallazgos[indice, , drop = FALSE]
  alterado$n_afectados[[1L]] <- 0
  expect_warning(
    lupa:::.advertir_incoherencias_trazabilidad(alterado, datos, "x"),
    class = "lupa_trazabilidad_incoherente"
  )
})

test_that("la guarda conserva el hallazgo cuando la traza queda separada", {
  datos <- data.frame(x = c(rep(1, 20L), 1000))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
  indice <- which(perfil$hallazgos$tipo_hallazgo == "outliers")
  expect_equal(length(indice), 1L)
  alterado <- perfil$hallazgos[indice, , drop = FALSE]
  alterado$trazabilidad[[1L]]$indices_fila <- integer()
  alterado$trazabilidad[[1L]]$mostrados <- 0L
  expect_warning(
    lupa:::.advertir_incoherencias_trazabilidad(alterado, datos, "x"),
    class = "lupa_trazabilidad_incoherente"
  )
  expect_equal(alterado$n_afectados[[1L]], 1)
})

test_that("las ausencias de una columna de lista se nombran, no solo se cuentan", {
  n <- 60L
  datos <- data.frame(a = seq_len(n))
  contenido <- vector("list", n)
  contenido[seq_len(n / 2L)] <- list(1:2)
  contenido[(n / 2L + 1L):n] <- list(NA)
  datos$contenido <- I(contenido)

  expect_no_warning(
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ),
    class = "lupa_trazabilidad_incoherente"
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "faltantes", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$estado, "disponible")
  expect_equal(hallazgo$n_afectados[[1L]], n / 2)
  expect_equal(sort(traza$indices_fila), which(is.na(contenido)))
})

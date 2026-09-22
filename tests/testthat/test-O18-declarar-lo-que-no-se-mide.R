test_that("Tukey no publica outliers con IQR cero", {
  perfil <- perfilar(
    data.frame(flag = c(rep(0, 90), rep(1, 10))),
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )

  expect_true(is.na(perfil$columnas$n_outliers))
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "outliers"))
  cobertura <- perfil$cobertura_diagnosticos[
    perfil$cobertura_diagnosticos$diagnostico == "outliers", , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$motivo, "intercuartilico")

  sano <- perfilar(
    data.frame(x = c(1:20, 100)),
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE
  )
  expect_equal(sano$columnas$n_outliers, 1L)
  expect_true(any(sano$hallazgos$tipo_hallazgo == "outliers"))
})

test_that("asociaciones conserva pares que no pudo medir", {
  datos <- data.frame(
    x = c(1, 2, NA, NA, NA),
    y = c(NA, NA, NA, 3, 4)
  )
  resultado <- detectar_asociaciones(datos, umbral = 0)
  omitidos <- attr(resultado, "pares_omitidos_medicion", exact = TRUE)

  expect_equal(nrow(resultado), 0L)
  expect_equal(attr(resultado, "pares_posibles"), 1)
  expect_equal(nrow(omitidos), 1L)
  expect_equal(omitidos$columna_1, "x")
  expect_equal(omitidos$columna_2, "y")
  expect_match(omitidos$motivo, "Pocas filas completas")

  integrado <- analizar(
    datos, umbral_asociacion = 0, muestra_asociacion = Inf,
    medir_propuesta = FALSE,
    argumentos_perfil = list(
      analizar_dependencias = FALSE,
      casi_duplicados_vocabulario = FALSE,
      ausencia_estructural = FALSE
    )
  )
  cobertura <- integrado$perfil$cobertura_diagnosticos[
    integrado$perfil$cobertura_diagnosticos$diagnostico == "asociaciones",
    , drop = FALSE
  ]
  expect_equal(nrow(cobertura), 1L)
  expect_match(cobertura$columna, "x / y")

  sano <- detectar_asociaciones(
    data.frame(x = 1:10, y = 2 * (1:10)), umbral = 0
  )
  expect_equal(nrow(sano), 1L)
  expect_equal(nrow(attr(sano, "pares_omitidos_medicion", exact = TRUE)), 0L)
})

test_that("perfilar_por mueve centinelas de integer64 a cobertura", {
  skip_if_not_installed("bit64")
  n <- 2400L
  datos <- data.frame(
    id = bit64::as.integer64(c(seq_len(n - 2L), 999L, 999L)),
    stringsAsFactors = FALSE
  )
  datos$grupo <- ifelse(as.numeric(datos$id) %% 2 == 1, "A", "B")
  datos$id[[n - 1L]] <- bit64::as.integer64(998L)

  resultado <- perfilar_por(datos, por = "grupo", min_filas = 30L)
  expect_false(any(
    resultado$columna == "id" &
      resultado$tipo_hallazgo == "faltantes_disfrazados"
  ))
  cobertura <- attr(resultado, "cobertura_diagnosticos", exact = TRUE)
  expect_true(any(
    cobertura$diagnostico == "centinelas_numericos" &
      cobertura$columna == "id"
  ))

  denso <- data.frame(
    id = bit64::as.integer64(seq_len(n)),
    grupo = rep(c("A", "B"), length.out = n)
  )
  sano <- perfilar_por(denso, por = "grupo", min_filas = 30L)
  cb_sano <- attr(sano, "cobertura_diagnosticos", exact = TRUE)
  expect_false(any(sano$tipo_hallazgo == "faltantes_disfrazados"))
  expect_false(any(cb_sano$diagnostico == "centinelas_numericos"))
})

test_that("las acciones de ausencia respetan el universo por fila", {
  set.seed(14)
  n_t <- 300L
  datos <- data.frame(
    tipo = c(rep("titular", n_t), rep("garante", 700)),
    stringsAsFactors = FALSE
  )
  datos$edad <- as.numeric(c(sample(20:45, n_t, TRUE), rep(NA, 700)))
  datos$edad[sample(1:n_t, 50)] <- NA
  perfil <- perfilar(
    datos, aplicabilidad = list(edad = ~ tipo == "titular"),
    analizar_dependencias = FALSE
  )

  activar <- function(plan, estrategia) {
    plan$aplicar[] <- FALSE
    plan$aplicar[which(plan$estrategia == estrategia)[[1L]]] <- TRUE
    plan
  }

  plan_marca <- activar(planificar_limpieza(perfil, datos), "marcar_filas_ausentes")
  marcada <- aplicar(plan_marca, datos)
  expect_equal(sum(marcada$datos$.ausente_edad, na.rm = TRUE), 50L)
  expect_equal(sum(marcada$datos$.ausente_edad & marcada$datos$tipo == "garante"), 0L)
  expect_equal(marcada$registro$n_cambiadas, 50L)

  plan_elimina <- activar(
    planificar_limpieza(perfil, datos), "eliminar_filas_ausentes"
  )
  eliminada <- aplicar(plan_elimina, datos, permitir_eliminacion = TRUE)
  expect_equal(nrow(eliminada$datos), 950L)
  expect_equal(sum(eliminada$datos$tipo == "garante"), 700L)
  expect_equal(eliminada$registro$n_cambiadas, 50L)

  perfil_sin_regla <- perfilar(datos, analizar_dependencias = FALSE)
  vieja_marca <- aplicar(
    activar(planificar_limpieza(perfil_sin_regla, datos), "marcar_filas_ausentes"),
    datos
  )
  expect_equal(sum(vieja_marca$datos$.ausente_edad, na.rm = TRUE), 750L)
  vieja_elimina <- aplicar(
    activar(planificar_limpieza(perfil_sin_regla, datos), "eliminar_filas_ausentes"),
    datos, permitir_eliminacion = TRUE
  )
  expect_equal(nrow(vieja_elimina$datos), 250L)
})

test_that("una columna sin soporte no propone winsorizar", {
  # `test-plan-aplicar-honesto.R` tenia un caso de columna chica -cuatro
  # valores- que se agrando al exigir soporte para el criterio de Tukey. El
  # caso chico no desaparece: con menos valores que el minimo, los outliers no
  # se publican, la cobertura lo declara y el plan no propone winsorizar sobre
  # un conteo que no se midio.
  datos <- data.frame(x = c(1, 2, 3, 100))
  perfil <- perfilar(datos)
  expect_true(is.na(perfil$columnas$n_outliers))
  expect_true(any(perfil$cobertura_diagnosticos$diagnostico == "outliers"))

  hallazgo <- as.data.frame(hallazgos(perfil))
  expect_false(any(hallazgo$tipo_hallazgo == "outliers"))

  plan <- planificar_limpieza(perfil, datos = datos)
  expect_false(any(as.character(plan$estrategia) %in%
                     c("winsorizar_outliers", "marcar_outliers")))
})

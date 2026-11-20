# Validez de dominio: rangos conservadores, fechas y recorridos de entrada.

testthat::skip_on_cran()

.datos_dominio_O184 <- function() {
  data.frame(
    id = seq_len(4L),
    edad = c(25, -3, 150, 210),
    edad_edificio = c(200, 210, 220, 230),
    edad_arbol = c(140, 160, 180, 200),
    porcentaje = c(10, 120, 105, -5),
    porcentaje_variacion = c(120, 105, 110, 130),
    tasa_cambio = c(-2, -1, 0, 1),
    latitud = c(0, 91, -91, 1),
    longitud = c(0, 181, -181, 1),
    lat = c(300, 301, 302, 303),
    fecha_registro = as.Date(c(
      "2026-01-01", "2026-02-01", "1900-01-01", "1900-01-01"
    )),
    fecha_vencimiento = as.Date(rep("2027-01-01", 4L)),
    fecha_cita = as.Date(rep("2026-03-01", 4L)),
    fecha_unica = as.Date(c("1970-01-01", "2026-01-01", "2026-01-02", "2026-01-03")),
    descripcion = intToUtf8(0x00E1)
  )
}

testthat::test_that("inferencia de dominio acusa imposibles y conserva excepciones", {
  datos <- .datos_dominio_O184()
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  ))
  nuevos <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% .TIPOS_HALLAZGO_DOMINIO,
    , drop = FALSE
  ]
  testthat::expect_setequal(
    unique(as.character(nuevos$tipo_hallazgo)),
    c("valor_fuera_dominio", "fecha_futura", "fecha_centinela")
  )
  testthat::expect_equal(
    nuevos$n_afectados[nuevos$columna == "edad" &
                         nuevos$tipo_hallazgo == "valor_fuera_dominio"],
    3
  )
  testthat::expect_equal(
    nuevos$n_afectados[nuevos$columna == "porcentaje" &
                         nuevos$tipo_hallazgo == "valor_fuera_dominio"],
    3
  )
  testthat::expect_equal(
    nuevos$n_afectados[nuevos$columna == "fecha_registro" &
                         nuevos$tipo_hallazgo == "fecha_futura"],
    1
  )
  testthat::expect_equal(
    nuevos$n_afectados[nuevos$columna == "fecha_registro" &
                         nuevos$tipo_hallazgo == "fecha_centinela"],
    2
  )
  testthat::expect_true(all(nuevos$unidad_conteo == "fila"))
  testthat::expect_true(all(vapply(
    nuevos$trazabilidad, function(x) identical(x$estado, "disponible"),
    logical(1L)
  )))

  columnas_sanas <- c(
    "edad_edificio", "edad_arbol", "porcentaje_variacion", "tasa_cambio",
    "lat", "fecha_vencimiento", "fecha_cita", "fecha_unica"
  )
  testthat::expect_false(any(nuevos$columna %in% columnas_sanas))
})

testthat::test_that("una regla declarada tiene prioridad y el apagado es local", {
  datos <- data.frame(
    nivel_riesgo = c(0, 1, 3, -1),
    edad = c(20, -2, 30, 40),
    fecha = as.Date(c("2026-01-01", "1970-01-01", "2026-01-02", "2026-01-03"))
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    dominios = list(
      nivel_riesgo = list(minimo = 0, maximo = 1),
      fecha = list(centinelas = "1970-01-01")
    ), analizar_dependencias = FALSE
  ))
  riesgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "valor_fuera_dominio" &
      perfil$hallazgos$columna == "nivel_riesgo", , drop = FALSE
  ]
  testthat::expect_equal(nrow(riesgo), 1L)
  testthat::expect_identical(as.character(riesgo$severidad), "error")
  centinela <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "fecha_centinela" &
      perfil$hallazgos$columna == "fecha", , drop = FALSE
  ]
  testthat::expect_equal(centinela$n_afectados, 1)

  apagado <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    dominios = list(edad = FALSE), analizar_dependencias = FALSE
  ))
  testthat::expect_false(any(
    apagado$hallazgos$tipo_hallazgo %in% .TIPOS_HALLAZGO_DOMINIO &
      apagado$hallazgos$columna == "edad"
  ))
  todo_apagado <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    dominios = FALSE, analizar_dependencias = FALSE
  ))
  testthat::expect_false(any(
    todo_apagado$hallazgos$tipo_hallazgo %in% .TIPOS_HALLAZGO_DOMINIO
  ))
})

testthat::test_that("las entradas de perfilado conservan el diagnostico", {
  datos <- data.frame(grupo = c("a", "a", "b", "b"), edad = c(30, -2, 40, 41))
  regla <- list(edad = list(minimo = 0, maximo = 130))
  por_grupo <- suppressWarnings(perfilar_por(
    datos, "grupo", min_filas = 1L, dominios = regla,
    analizar_dependencias = FALSE
  ))
  testthat::expect_equal(nrow(por_grupo), 1L)
  testthat::expect_identical(por_grupo$grupo, "a")
  testthat::expect_identical(por_grupo$tipo_hallazgo, "valor_fuera_dominio")

  analisis <- suppressWarnings(analizar(
    datos["edad"], fecha = as.POSIXct("2026-01-15", tz = "UTC"),
    argumentos_perfil = list(dominios = regla), medir_propuesta = FALSE,
    analizar_dependencias = FALSE
  ))
  testthat::expect_s3_class(analisis, "analisis")
  testthat::expect_identical(
    analisis$perfil$hallazgos$tipo_hallazgo, "valor_fuera_dominio"
  )
})

testthat::test_that("DuckDB y SQLite conservan los hallazgos de la muestra", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("RSQLite")
  motores <- c("duckdb", "RSQLite")
  for (motor in motores) {
    con <- if (motor == "duckdb") {
      DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    } else {
      DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    }
    datos <- data.frame(edad = c(20, -2, 40, 41))
    DBI::dbWriteTable(con, "t_dominio", datos)
    perfil <- tryCatch(
      suppressWarnings(perfilar_dbi(
        con, "t_dominio", muestra = Inf,
        fecha = as.POSIXct("2026-01-15", tz = "UTC"),
        dominios = list(edad = list(minimo = 0, maximo = 130)),
        analizar_dependencias = FALSE, instrumentar = FALSE
      )),
      finally = DBI::dbDisconnect(con, shutdown = motor == "duckdb")
    )
    hallazgos <- perfil$perfil_muestra$hallazgos
    testthat::expect_equal(
      hallazgos$tipo_hallazgo[hallazgos$columna == "edad"],
      "valor_fuera_dominio", info = motor
    )
    testthat::expect_identical(
      as.character(hallazgos$severidad[hallazgos$columna == "edad"]),
      "error", info = motor
    )
  }
})

testthat::test_that("la deriva y el plan hacen visible el dominio sin destruir", {
  datos <- data.frame(edad = c(20, -2, 40, 41))
  fecha_a <- as.POSIXct("2026-01-01", tz = "UTC")
  anterior <- suppressWarnings(perfilar(
    datos, fecha = fecha_a, analizar_dependencias = FALSE
  ))
  actual <- suppressWarnings(perfilar(
    datos, fecha = fecha_a + 86400, dominios = FALSE,
    analizar_dependencias = FALSE
  ))
  deriva <- comparar_perfiles(anterior, actual)
  testthat::expect_true(any(deriva$aspecto == "configuracion_dominios"))
  testthat::expect_true(any(
    deriva$aspecto == "hallazgo" & deriva$columna == "edad"
  ))

  plan <- planificar_limpieza(anterior, datos)
  accion <- plan[plan$hallazgo == "valor_fuera_dominio", , drop = FALSE]
  testthat::expect_equal(nrow(accion), 1L)
  testthat::expect_identical(accion$estrategia, "revisar_dominio")
  testthat::expect_true(accion$recomendada)
  testthat::expect_false(accion$aplicar)
  testthat::expect_identical(as.character(accion$estado), "informativa")
})

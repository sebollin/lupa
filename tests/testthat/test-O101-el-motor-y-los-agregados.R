# El motor contra la memoria, y los agregados encadenados.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.o101_perfil_aviso_opcional <- function(argumentos, espera_aviso = FALSE) {
  ejecutar <- function() suppressMessages(do.call(perfilar_dbi, argumentos))
  if (isTRUE(espera_aviso)) {
    # Que haya aviso depende del controlador, no de lupa: con duckdb 1.x,
    # STDDEV_SAMP sobre un NaN es un error del motor ("out of range") y la
    # consulta queda como no disponible; desde duckdb 2.0 devuelve NaN y no
    # falla ninguna. Lo que se fija es la relacion: hay aviso si y solo si hay
    # fallas registradas, y el aviso es el de la muestra del motor.
    avisos <- list()
    perfil <- withCallingHandlers(ejecutar(), warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
    for (w in avisos) expect_s3_class(w, "lupa_aviso_muestra_motor")
    fallas <- perfil$resumen_tabla$meta$consultas$fallas
    n_fallas <- if (is.data.frame(fallas)) nrow(fallas) else 0L
    expect_identical(length(avisos) > 0L, n_fallas > 0L)
  } else {
    perfil <- expect_no_warning(ejecutar())
  }
  perfil
}

test_that("el alcance de un agregado de segundo nivel es el de cada entidad", {
  nucleo <- metricas_nucleo()
  formato <- function(entidad, atributo) {
    instanciar(especializar(nucleo$Formato, expresion_regular = "^[A-Z]+$"),
               entidad, atributo)
  }
  medicion <- medir(
    modelo(formato("hogares", "depto"), formato("personas", "nombre")),
    list(hogares = data.frame(depto = c("MVD", NA, "3", "zz")),
         personas = data.frame(nombre = c("A", "B", NA, "44")))
  )
  nivel1 <- agregar(medicion, "atributo", "ratio")
  nivel2 <- agregar(nivel1, "entidad", "promedio")
  alcance <- attr(nivel2, "alcance_medidas", exact = TRUE)
  # Antes: "6 de 8" en cada entidad -la suma de las dos-.
  expect_equal(alcance$medidas, c(3, 3))
  expect_equal(alcance$en_el_universo, c(4, 4))
  # Control: el nivel 1 ya estaba bien y sigue igual.
  expect_equal(attr(nivel1, "alcance_medidas", exact = TRUE)$medidas, c(3, 3))
})

test_that("dos corridas con el mismo id y fechas distintas no se mezclan", {
  m <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x"))
  a <- medir(m, data.frame(x = c(1, NA)), id_medicion = "x",
             fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  b <- medir(m, data.frame(x = c(1, 2)), id_medicion = "x",
             fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  expect_error(agregar(rbind(a, b), "atributo", "ratio"), "fecha")
  expect_error(tablero_calidad(rbind(a, b)), "fecha")
  # Control: una sola corrida se agrega.
  expect_equal(agregar(a, "atributo", "ratio")$resultado, 0.5)
})

test_that("un NaN del motor no se publica como una cifra calculada", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(n_nan = c(NaN, 1, 2, 3, NA, 4),
                                          x = c(1, 2, 3, 4, 5, 6)))
  for (metricas in list(NULL, c("validos", "mediana"))) {
    argumentos <- list(con, "d", bloque_muestra = "solo_agregados",
                       proteger_datos_personales = FALSE)
    if (!is.null(metricas)) argumentos$metricas <- metricas
    perfil <- .o101_perfil_aviso_opcional(
      argumentos, espera_aviso = is.null(metricas)
    )
    sql <- as.data.frame(perfil$resumen_tabla$sql)
    de_nan <- sql[sql$columna == "n_nan" &
                    sql$metrica %in% c("minimo", "maximo", "media", "mediana",
                                       "desvio"), , drop = FALSE]
    # Antes: `maximo` y `media` en NA con estado `calculado`, y la mediana
    # contando el NaN como el mayor valor.
    expect_true(all(de_nan$estado %in% c("no_disponible", "no_solicitado")))
    expect_true(any(grepl("NaN", de_nan$motivo, fixed = TRUE)))
    # Control: la columna sin NaN se calcula.
    x <- sql[sql$columna == "x" & sql$metrica == "mediana", , drop = FALSE]
    expect_identical(as.character(x$estado), "calculado")
  }
})

test_that("la sonda de magnitud ve un texto colado en una columna numerica", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (v INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (1), (2), (3), (4), ('texto')")
  perfil <- expect_no_warning(suppressMessages(perfilar_dbi(
    con, "t", metricas = c("validos", "mediana"),
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )))
  sql <- as.data.frame(perfil$resumen_tabla$sql)
  # Con `MIN` la sonda devolvia 1 y la mediana se publicaba calculada sobre una
  # columna con texto.
  expect_identical(as.character(sql$estado[sql$metrica == "mediana"]),
                   "no_disponible")
})

# Las cuentas de cada cifra del resumen: que se publican igual en memoria y en el
# motor, que cada comprobacion dispara en su caso real y no en los legitimos, y
# que el tablero y el indice lo dicen.

.cr_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

.cr_con <- function(motor) {
  if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    DBI::dbConnect(duckdb::duckdb(), ":memory:")
  }
}

.cr_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

.cr_cuentas <- c(
  "resultado", "numerador", "denominador", "n_universo", "n_no_evaluados",
  "motivo_no_evaluados", "n_sin_valor", "n_filas_leidas", "entidad_referida",
  "n_filas_leidas_referida", "estado_cuentas", "motivo_cuentas",
  "diferencia_cuentas"
)

.cr_tabla <- function(x) as.data.frame(unclass(x), stringsAsFactors = FALSE)

.cr_datos <- function() {
  list(
    d = data.frame(
      id = 1:10,
      n = c(1, 2, NA, 4, NaN, 100, 0, NA, 5, 6),
      t = c("x", "y", NA, "x", "z", NA, "x", "y", "x", "x"),
      f = as.Date("2026-01-01") + c(1, 5, NA, 10, 15, 20, NA, 25, 30, 35),
      stringsAsFactors = FALSE
    ),
    ref = data.frame(k = 1:5)
  )
}

.cr_modelo <- function() {
  nuc <- metricas_nucleo()
  vf <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                 fecha_limite = as.Date("2026-01-20"))
  vi <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                 inicio_intervalo = as.Date("2026-01-01"),
                 fin_intervalo = as.Date("2026-01-25"))
  modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$Formato, "Dic", diccionario = c("x", "y")), "d", "t"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t"),
    instanciar(especializar(nuc$OportunidadEntPorFecha, "OEF", vigencia = vf), "d"),
    instanciar(especializar(nuc$OportunidadEntPorIntervalo, "OEI", vigencia = vi), "d"),
    instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"), c("ref", "d"), c("k", "n"))
  )
}

test_that("las cuentas son las mismas en memoria, en DuckDB y por bloques", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  datos <- .cr_datos()
  mod <- .cr_modelo()
  mr <- medir(mod, datos, id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_true(all(lupa:::.COLUMNAS_CUENTAS_RESUMEN %in% names(mr)))
  expect_identical(mr$estado_cuentas, rep("cierran", nrow(mr)))
  for (estrategia in c("no_medible", "bloques")) {
    con <- .cr_con("duckdb")
    DBI::dbWriteTable(con, "d", datos$d)
    DBI::dbWriteTable(con, "ref", datos$ref)
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha,
                    estrategia_no_sql = estrategia)
    .cr_cerrar(con, "duckdb")
    expect_identical(md$metrica_instanciada, mr$metrica_instanciada)
    expect_equal(.cr_tabla(md)[.cr_cuentas], .cr_tabla(mr)[.cr_cuentas],
                 info = estrategia)
  }
  # `n_sin_valor` tambien en `NoNulo`, donde no falta ninguna medida.
  expect_identical(mr$n_sin_valor[mr$metrica_instanciada == "NN@d.n"], 3)
})

test_that("la referencial cuenta el NaN como vacio y su cobertura es 4/7", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  datos <- .cr_datos()
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"),
                           c("ref", "d"), c("k", "n")))
  mr <- medir(mod, datos, id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_identical(c(mr$numerador, mr$denominador), c(4, 7))
  expect_identical(mr$n_no_evaluados, mr$n_sin_valor)
  expect_identical(mr$estado_cuentas, "cierran")
  expect_identical(mr$entidad_referida, "ref")
  expect_identical(mr$n_filas_leidas_referida, 5)
  for (motor in c("sqlite", "duckdb")) {
    con <- .cr_con(motor)
    DBI::dbWriteTable(con, "d", datos$d)
    DBI::dbWriteTable(con, "ref", datos$ref)
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha)
    .cr_cerrar(con, motor)
    expect_identical(c(md$numerador, md$denominador), c(4, 7), info = motor)
    expect_identical(md$estado_cuentas, "cierran", info = motor)
  }
})

test_that("OportunidadEnt* cuentan los vacios de la fecha de su vigencia", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  datos <- .cr_datos()
  mod <- .cr_modelo()
  mr <- medir(mod, datos, id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  ent <- mr[mr$metrica_instanciada %in% c("OEF@d", "OEI@d"), ]
  expect_identical(ent$motivo_no_evaluados, c("sin_valor", "sin_valor"))
  expect_identical(ent$n_sin_valor, c(2, 2))
  al <- attr(mr, "alcance_medidas")
  expect_true(all(grepl("no tienen valor", al$motivo[al$metrica_instanciada == "OEF@d"],
                        fixed = TRUE)))
  con <- .cr_con("duckdb")
  DBI::dbWriteTable(con, "d", datos$d)
  DBI::dbWriteTable(con, "ref", datos$ref)
  md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha)
  .cr_cerrar(con, "duckdb")
  expect_identical(md$motivo_no_evaluados[md$metrica_instanciada %in% c("OEF@d", "OEI@d")],
                   c("sin_valor", "sin_valor"))
})

test_that("un vocabulario truncado por max_valores es parcial, no una cuenta que no cierra", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  nuc <- metricas_nucleo()
  dt <- data.frame(t = c("a", "b", "c", "d", "e", "f", NA, "a", "b", "c"),
                   stringsAsFactors = FALSE)
  m <- medir(modelo(instanciar(especializar(nuc$EntidadContradictoria, "EC", max_valores = 3L),
                               "dt", "t")),
             list(dt = dt), id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_identical(m$estado_cuentas, "parcial")
  expect_identical(m$motivo_cuentas, "truncada")
  expect_identical(m$diferencia_cuentas, 3)
  sin_tope <- medir(modelo(instanciar(especializar(nuc$EntidadContradictoria, "EC"), "dt", "t")),
                    list(dt = dt), id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_identical(sin_tope$estado_cuentas, "cierran")
})

test_that("CorrectitudSemDebil cuenta su universo en filas del par", {
  skip_on_cran()
  rc <- metricas_referencial()
  d <- data.frame(id = c(1:8, NA, 10),
                  v = c("v1", "v2", NA, "x", "v5", NA, "v7", "v8", "v9", "v10"),
                  stringsAsFactors = FALSE)
  refo <- referencial(data.frame(k = 1:30, t = paste0("v", 1:30), stringsAsFactors = FALSE),
                      clave = "k", valor = "t", completo = TRUE, alcance = "prueba")
  inst <- instanciar(especializar(rc$CorrectitudSemDebil, "CSD"), "d", c("id", "v"),
                     referencial = refo)
  m <- medir(modelo(inst), list(d = d), id_medicion = "p", fecha = .cr_fecha,
             detalle = "resumen")
  expect_identical(c(m$denominador, m$n_universo, m$n_sin_valor), c(7, 10, 3))
  expect_identical(m$unidad_conteo, "fila")
  expect_identical(m$estado_cuentas, "cierran")
  mc <- medir(modelo(inst), list(d = d), id_medicion = "p", fecha = .cr_fecha)
  expect_identical(attr(mc, "alcance_medidas")$en_el_universo, 10)
})

.cr_metodo_propio <- function(nombre, filas_de) {
  metrica(
    nombre, "Metodo de prueba.", "instanciaAtributo", "booleano",
    dimension = "Exactitud", factor = "Exactitud sintactica",
    metodo = function(tablas, instancia) {
      x <- tablas[[instancia$entidad[[1L]]]][[instancia$atributos[[1L]]]]
      f <- filas_de(x)
      data.frame(resultado = rep(TRUE, length(f)), entidad = instancia$entidad[[1L]],
                 atributo = instancia$atributos[[1L]], fila = f,
                 objeto = paste0("d$x[", f, "]#", seq_along(f)), stringsAsFactors = FALSE)
    }
  )
}

test_that("C3 y C4 disparan con metodos propios en memoria, y avisan", {
  skip_on_cran()
  d <- data.frame(x = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10))
  doble <- .cr_metodo_propio("Doble", function(x) rep(seq_along(x), each = 2L))
  mitad <- .cr_metodo_propio("Mitad", function(x) {
    f <- which(!is.na(x)); f[f %% 2L == 0L]
  })
  expect_warning(
    m <- medir(modelo(instanciar(especializar(doble, "D"), "d", "x"),
                      instanciar(especializar(mitad, "M"), "d", "x")),
               list(d = d), id_medicion = "p", fecha = .cr_fecha, detalle = "resumen"),
    "no cierran"
  )
  expect_identical(m$estado_cuentas, c("no_cierran", "no_cierran"))
  expect_identical(m$motivo_cuentas, c("medidas_repetidas", "no_evaluados_sin_explicar"))
  # Mitad: 5 medidas de 10 celdas con 2 vacias; 3 celdas con valor sin medir.
  expect_identical(m$diferencia_cuentas[[2L]], 3)
})

test_that("C5: dos lotes que leen la tabla en momentos distintos no cierran", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- data.frame(n = c(1, 2, NA, 4, 2, 100, 3, NA, 5, 6))
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
                instanciar(especializar(nuc$ValoresPosiblesPorComprension, "Rng",
                                        minimo = 0, maximo = 10), "d", "n"))
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  original <- lupa:::.traer_consulta_dbi
  n_lecturas <- 0L
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
    r <- original(conexion, sql, ...)
    if (grepl("AS n_filas", sql, fixed = TRUE)) {
      n_lecturas <<- n_lecturas + 1L
      if (n_lecturas == 1L) DBI::dbAppendTable(conexion, "d", data.frame(n = c(7, 8, 9)))
    }
    r
  })
  expect_warning(
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha, tamano_lote = 1L),
    "no cierran"
  )
  expect_identical(md$n_filas_leidas, c(10, 13))
  expect_identical(md$estado_cuentas, c("no_cierran", "no_cierran"))
  expect_identical(md$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L))
  expect_identical(md$diferencia_cuentas, c(3, 3))
})

test_that("la consulta dedicada sale de una sola lectura: ni -1 ni cuentas mezcladas", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- data.frame(n = c(1, 2, NA, 4, 2, 100, 3, NA, 5, 6))
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
                instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "n"))
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  original <- lupa:::.traer_consulta_dbi
  hecho <- FALSE
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
    r <- original(conexion, sql, ...)
    if (!hecho && grepl("AS n_filas", sql, fixed = TRUE)) {
      hecho <<- TRUE
      DBI::dbAppendTable(conexion, "d", data.frame(n = c(7, 8, 9)))
    }
    r
  })
  expect_warning(
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha),
    "no cierran"
  )
  dup <- md[md$metrica_instanciada == "Dup@d.n", ]
  expect_true(dup$n_no_evaluados >= 0)
  expect_identical(dup$denominador + dup$n_no_evaluados, dup$n_universo)
  expect_identical(dup$n_filas_leidas, 13)
  expect_identical(md$estado_cuentas, c("no_cierran", "no_cierran"))
})

test_that("C5: partida la consulta tras un error, cada mitad conserva sus filas", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- data.frame(a = 1:10, b = 1:10, c = 1:10, e = 1:10)
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "A"), "d", "a"),
    instanciar(especializar(nuc$NoNulo, "B"), "d", "b"),
    instanciar(especializar(nuc$NoNulo, "C"), "d", "c"),
    instanciar(especializar(nuc$NoNulo, "E"), "d", "e")
  )
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  original <- lupa:::.traer_consulta_dbi
  fallo <- FALSE
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
    if (!fallo && grepl("m1_rec", sql, fixed = TRUE) && grepl("m4_rec", sql, fixed = TRUE)) {
      fallo <<- TRUE
      stop("error simulado")
    }
    r <- original(conexion, sql, ...)
    if (fallo && grepl("m1_rec", sql, fixed = TRUE) && !grepl("m4_rec", sql, fixed = TRUE)) {
      DBI::dbAppendTable(conexion, "d", data.frame(a = 11:13, b = 11:13, c = 11:13, e = 11:13))
    }
    r
  })
  expect_warning(
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha),
    "no cierran"
  )
  expect_identical(md$n_filas_leidas, c(10, 10, 13, 13))
  expect_true(all(md$estado_cuentas == "no_cierran"))
})

test_that("C5 ve la tabla referida que cambia entre lecturas", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", data.frame(id = 1:10, k = 1:10))
  DBI::dbWriteTable(con, "ref_tab", data.frame(k = 1:5))
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN_ref"), "ref_tab", "k"),
    instanciar(especializar(nuc$NoNulo, "NN_d"), "d", "k"),
    instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"), c("ref_tab", "d"), c("k", "k"))
  )
  original <- lupa:::.traer_consulta_dbi
  hecho <- FALSE
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
    r <- original(conexion, sql, ...)
    if (!hecho && grepl("AS n_filas", sql, fixed = TRUE) &&
        grepl("FROM `ref_tab`", sql, fixed = TRUE)) {
      hecho <<- TRUE
      DBI::dbAppendTable(conexion, "ref_tab", data.frame(k = 6:10))
    }
    r
  })
  expect_warning(
    md <- medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha),
    "no cierran"
  )
  fk <- md[md$metrica_instanciada == "FK@ref_tab.d.k.k", ]
  expect_identical(fk$n_filas_leidas_referida, 10)
  expect_identical(fk$estado_cuentas, "no_cierran")
  expect_identical(md$estado_cuentas[md$metrica_instanciada == "NN_ref@ref_tab.k"], "no_cierran")
  expect_identical(md$estado_cuentas[md$metrica_instanciada == "NN_d@d.k"], "cierran")
})

test_that("C5 ve la tabla que cambia entre bloques por rowid", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^[a-z]$"),
                           "tb", "t"))
  original <- lupa:::.traer_consulta_dbi
  accion <- "nada"
  bloques <- 0L
  local_mocked_bindings(.traer_consulta_dbi = function(conexion, sql, ...) {
      r <- original(conexion, sql, ...)
      if (grepl("WHERE rowid >=", sql, fixed = TRUE)) {
        bloques <<- bloques + 1L
        if (bloques == 1L && accion == "alta") {
          DBI::dbAppendTable(conexion, "tb", data.frame(t = c("x", "y", "z"), stringsAsFactors = FALSE))
        }
        if (bloques == 1L && accion == "baja") {
          DBI::dbExecute(conexion, "DELETE FROM tb WHERE t IN ('h', 'i')")
        }
      }
      r
  })
  for (accion in c("alta", "baja", "nada")) {
    bloques <- 0L
    con <- .cr_con("duckdb")
    DBI::dbWriteTable(con, "tb", data.frame(t = c("a", "b", "c", "d", NA, "f", "g", "h", "i", "j"),
                                            stringsAsFactors = FALSE))
    md <- suppressWarnings(medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha,
                                     estrategia_no_sql = "bloques", bloque_filas = 4L))
    .cr_cerrar(con, "duckdb")
    esperado <- if (accion == "nada") "cierran" else "no_cierran"
    expect_identical(md$estado_cuentas, esperado, info = accion)
  }
})

test_that("C5 no acusa una aplicabilidad distinta por columna", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  dl <- data.frame(x = c(1, NA, 3, 4, 5, 6), y = c("a", "b", NA, "d", "e", "f"),
                   l = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
  m <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NNx"), "dl", "x"),
                    instanciar(especializar(nuc$NoNulo, "NNy"), "dl", "y")),
             list(dl = dl), id_medicion = "p", fecha = .cr_fecha,
             aplicabilidad = list(x = ~ l), detalle = "resumen")
  expect_identical(m$n_universo, c(4, 6))
  expect_identical(m$n_filas_leidas, c(6, 6))
  expect_identical(m$estado_cuentas, c("cierran", "cierran"))
})

test_that("rbind de dos partes sobre dos versiones de la tabla no cierra", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d1 <- data.frame(x = c(1, 2, NA), y = c("a", "b", "c"), stringsAsFactors = FALSE)
  d2 <- data.frame(x = c(1, 2, NA, 4), y = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  p1 <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN_x"), "d", "x")),
              list(d = d1), id_medicion = "g", fecha = .cr_fecha, detalle = "resumen")
  p2 <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN_y"), "d", "y")),
              list(d = d2), id_medicion = "g", fecha = .cr_fecha, detalle = "resumen")
  expect_warning(u <- rbind(p1, p2), "no cierran")
  expect_identical(u$estado_cuentas, c("no_cierran", "no_cierran"))
  # Las mismas filas y otros datos: lo ve la huella de los datos.
  d3 <- data.frame(x = c(1, 2, NA), y = c("a", "b", "q"), stringsAsFactors = FALSE)
  p3 <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN_y"), "d", "y")),
              list(d = d3), id_medicion = "g", fecha = .cr_fecha, detalle = "resumen")
  expect_warning(u2 <- rbind(p1, p3), "no cierran")
  expect_identical(u2$estado_cuentas, c("no_cierran", "no_cierran"))
  # Sobre la misma tabla, cierra.
  p4 <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN_y"), "d", "y")),
              list(d = d1), id_medicion = "g", fecha = .cr_fecha, detalle = "resumen")
  expect_identical(rbind(p1, p4)$estado_cuentas, c("cierran", "cierran"))
})

test_that("C5 sin contador del motor se publica limitado; con contador ve un UPDATE", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "A"), "d", "x"),
                instanciar(especializar(nuc$ValoresPosiblesPorComprension, "R",
                                        minimo = 0, maximo = 3), "d", "x"))
  correr <- function(con, contador) {
    original <- lupa:::.traer_consulta_dbi
    hecho <- FALSE
    local_mocked_bindings(
      .traer_consulta_dbi = function(conexion, sql, ...) {
        r <- original(conexion, sql, ...)
        if (!hecho && grepl("AS n_filas", sql, fixed = TRUE)) {
          hecho <<- TRUE
          DBI::dbExecute(conexion, "UPDATE d SET x = 9 WHERE x = 1")
        }
        r
      },
      .especificacion_contador_estabilidad_dbi = function(...) contador
    )
    suppressWarnings(medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha,
                               tamano_lote = 1L))
  }
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", data.frame(x = c(1, 2, 3, 2)))
  sin <- correr(con, NULL)
  # El conteo no cambia con un UPDATE: el control lo dice.
  expect_identical(sin$estado_cuentas, c("cierran", "cierran"))
  expect_identical(lupa:::.control_lecturas_filas(sin), rep("solo_filas_agregadas_o_quitadas", 2L))
  DBI::dbExecute(con, "UPDATE d SET x = 1 WHERE x = 9")
  espec <- list(
    motor = "sqlite", mecanismo = "prueba", requiere_tabla_base = FALSE,
    requiere_filas_no_vacias = FALSE, solo_contador_cierre = FALSE,
    campos = list(list(clave = "suma", etiqueta_dif = "suma", alias = "lupa_est_suma",
                       sql = "CAST(SUM(x) AS TEXT)", obligatorio = TRUE))
  )
  con_contador <- correr(con, espec)
  expect_identical(con_contador$estado_cuentas, c("no_cierran", "no_cierran"))
  expect_identical(con_contador$motivo_cuentas, rep("tabla_cambio_entre_lecturas", 2L))
})

test_that("una metrica sin cuentas de su universo queda sin_cuentas", {
  skip_on_cran()
  propia <- metrica(
    "PropiaEntidad", "Una cifra por tabla.", "entidad", "real",
    dimension = "Completitud", factor = "Densidad",
    metodo = function(tablas, instancia) {
      data.frame(resultado = 0.99, entidad = instancia$entidad[[1L]], atributo = NA_character_,
                 fila = NA_integer_, objeto = instancia$entidad[[1L]], stringsAsFactors = FALSE)
    }
  )
  m <- medir(modelo(instanciar(especializar(propia, "P"), "d")),
             list(d = data.frame(x = 1:3)), id_medicion = "p", fecha = .cr_fecha,
             detalle = "resumen")
  expect_identical(m$estado_cuentas, "sin_cuentas")
})

test_that("ninguna metrica del catalogo acusa cuentas que no cierran sobre datos con vacios", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("stringdist")
  set.seed(11)
  n <- 40
  con_na <- function(x, p = 0.15) { x[runif(length(x)) < p] <- NA; x }
  d <- data.frame(
    id = seq_len(n),
    num = con_na(round(rnorm(n, 5, 4), 1)),
    txt = con_na(sample(c("ab", "cd", "ef", "Z9", "", "abx"), n, TRUE)),
    txt2 = con_na(sample(c("u", "v"), n, TRUE)),
    f = con_na(as.Date("2026-01-01") + sample(0:40, n, TRUE)),
    f2 = as.Date("2026-01-03") + sample(0:40, n, TRUE),
    stringsAsFactors = FALSE
  )
  d$num[c(2, 9)] <- NaN
  ref <- data.frame(k = 1:30, t = paste0("v", 1:30), stringsAsFactors = FALSE)
  refo <- referencial(ref, clave = "k", valor = "t", completo = TRUE, alcance = "prueba")
  nuc <- metricas_nucleo()
  rc <- metricas_referencial()
  vf <- vigencia("f", fecha_acceso = as.Date("2026-02-15"), fecha_limite = as.Date("2026-01-20"))
  vi <- vigencia("f", fecha_acceso = as.Date("2026-02-15"),
                 inicio_intervalo = as.Date("2026-01-01"), fin_intervalo = as.Date("2026-01-25"))
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NoNulo"), "d", "num"),
    instanciar(especializar(nuc$NoNulo, "NoNuloTxt", valores_nulos = ""), "d", "txt"),
    instanciar(especializar(nuc$Formato, "FD", diccionario = c("ab", "cd")), "d", "txt"),
    instanciar(especializar(nuc$Formato, "FR", expresion_regular = "^[a-z]+$"), "d", "txt"),
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "VPE", valores = c("ab", "ef")), "d", "txt"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "VPC", minimo = 0, maximo = 10), "d", "num"),
    instanciar(especializar(nuc$AtributoDuplicado, "AD"), "d", "txt"),
    instanciar(especializar(nuc$ConjuntoAtributosDuplicado, "CAD"), "d", c("txt", "txt2")),
    instanciar(especializar(nuc$EntidadDuplicada, "ED"), "d"),
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "RI", regla = ~ is.na(f) | f <= f2),
               "d", c("f", "f2")),
    instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "RIE"), c("ref", "d"), c("k", "num")),
    instanciar(especializar(nuc$EntidadContradictoria, "EC"), "d", "txt"),
    instanciar(especializar(nuc$DesactualizacionPorFormato, "DPF", expresion_regular = "^[a-z]+$"), "d", "txt"),
    instanciar(especializar(nuc$OportunidadAtributoPorFecha, "OAF", fecha_limite = as.Date("2026-01-20")), "d", "f"),
    instanciar(especializar(nuc$OportunidadAtributoPorIntervalo, "OAI", inicio_vigencia = as.Date("2026-01-01"),
                            fin_vigencia = as.Date("2026-01-25")), "d", "f"),
    instanciar(especializar(nuc$GradoOportunidadAtributoPorFecha, "GOF", fecha_solicitud = as.Date("2025-12-01"),
                            fecha_fin_utilidad = as.Date("2026-02-28")), "d", "f"),
    instanciar(especializar(nuc$GradoOportunidadAtributoPorIntervalo, "GOI", inicio_vigencia = as.Date("2026-01-01"),
                            fin_vigencia = as.Date("2026-01-30")), "d", "f"),
    instanciar(especializar(nuc$OportunidadEntPorFecha, "OEF", vigencia = vf), "d"),
    instanciar(especializar(nuc$OportunidadEntPorIntervalo, "OEI", vigencia = vi), "d"),
    instanciar(especializar(nuc$DensidadPonderada, "DP", coeficientes = c(num = 0.5, txt = 0.5)), "d", c("num", "txt")),
    instanciar(especializar(rc$CorrectitudSemFuerte, "CSF"), "d", "id", referencial = refo),
    instanciar(especializar(rc$CorrectitudSemDebil, "CSD"), "d", c("id", "txt2"), referencial = refo)
  )
  mr <- suppressWarnings(medir(mod, list(d = d, ref = ref), id_medicion = "p",
                               fecha = .cr_fecha, detalle = "resumen"))
  expect_gt(nrow(mr), 18L)
  # Que cada fila tenga su estado, y que ninguno acuse: sin la columna, la
  # comparacion de abajo pasaba sin mirar nada.
  expect_identical(length(mr$estado_cuentas), nrow(mr))
  expect_true(all(mr$estado_cuentas %in% c("cierran", "parcial")),
              info = paste(mr$metrica_instanciada[!mr$estado_cuentas %in% "cierran"], collapse = ", "))
  con <- .cr_con("duckdb")
  DBI::dbWriteTable(con, "d", d)
  DBI::dbWriteTable(con, "ref", ref)
  md <- suppressWarnings(medir_dbi(mod, con, id_medicion = "p", fecha = .cr_fecha,
                                   estrategia_no_sql = "bloques"))
  .cr_cerrar(con, "duckdb")
  expect_identical(length(md$estado_cuentas), nrow(md))
  expect_true(all(md$estado_cuentas %in% c("cierran", "parcial")),
              info = paste(md$metrica_instanciada[!md$estado_cuentas %in% "cierran"], collapse = ", "))
})

test_that("el tablero dice que cuentas no cierran y el indice no las combina", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  mitad <- .cr_metodo_propio("Mitad", function(x) {
    f <- which(!is.na(x)); f[f %% 2L == 0L]
  })
  d <- data.frame(x = c(1, 2, NA, 4, 5, 6, NA, 8, 9, 10), y = c(1, 1, 2, NA, 3, 4, 5, 6, 7, 8))
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "y"),
                instanciar(especializar(mitad, "M"), "d", "x"),
                instanciar(especializar(nuc$AtributoDuplicado, "AD"), "d", "y"))
  m <- suppressWarnings(medir(mod, list(d = d), id_medicion = "p", fecha = .cr_fecha,
                              detalle = "resumen"))
  tb <- tablero_calidad(m)
  cuentas <- attr(tb, "cuentas")
  expect_identical(cuentas$metrica_instanciada, "M@d.x")
  expect_identical(cuentas$componente, tb$componente[tb$metrica_instanciada == "M@d.x"])
  ic <- indice_calidad(m, pesos = c(Completitud = 0.5, Unicidad = 0.5))
  expect_false("M@d.x" %in% ic$componentes$metrica_instanciada)
  expect_identical(ic$excluidas$metrica_instanciada, "M@d.x")
  expect_true(grepl("no cierran", ic$excluidas$motivo_exclusion, fixed = TRUE))
  expect_error(
    indice_calidad(m[m$metrica_instanciada != "AD@d.y", ],
                   pesos = c(Completitud = 0.5, Exactitud = 0.5)),
    "no cierran"
  )
  # Un resumen que cierra no lleva el atributo.
  limpio <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "y")),
                  list(d = d), id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_null(attr(tablero_calidad(limpio), "cuentas"))
})

test_that("cobertura_metricas lleva la dimension y el factor de cada metrica", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  d <- data.frame(t = c(NA_character_, NA, NA), x = c(1, 2, 3))
  mod <- modelo(instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^a$"), "d", "t"),
                instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"))
  mr <- medir(mod, list(d = d), id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  mc <- medir(mod, list(d = d), id_medicion = "p", fecha = .cr_fecha)
  cob <- attr(mr, "cobertura_metricas")
  expect_identical(cob$dimension, "Exactitud")
  expect_false(is.na(cob$factor))
  expect_identical(attr(mc, "cobertura_metricas"), cob)
})

test_that("muestra de la referencial avisa que no tiene efecto", {
  skip_on_cran()
  nuc <- metricas_nucleo()
  datos <- .cr_datos()
  expect_warning(
    inst <- instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK", muestra = 3),
                       c("ref", "d"), c("k", "n")),
    "no tiene efecto"
  )
  expect_no_warning(
    sin <- instanciar(especializar(nuc$ReglaIntegridadInterEntidad, "FK"), c("ref", "d"), c("k", "n"))
  )
  con <- suppressWarnings(medir(modelo(inst), datos, id_medicion = "p", fecha = .cr_fecha,
                                detalle = "resumen"))
  sin_m <- medir(modelo(sin), datos, id_medicion = "p", fecha = .cr_fecha, detalle = "resumen")
  expect_identical(.cr_tabla(con)[c("numerador", "denominador", "n_universo")],
                   .cr_tabla(sin_m)[c("numerador", "denominador", "n_universo")])
})

test_that("rbind rechaza dos fechas en una corrida y una metrica medida y no medida", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  nuc <- metricas_nucleo()
  d <- data.frame(x = c(1, 2, 2, NA, 5))
  a <- medir(modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x")), list(d = d),
             id_medicion = "e", fecha = .cr_fecha, detalle = "resumen")
  b <- medir(modelo(instanciar(especializar(nuc$AtributoDuplicado, "AD"), "d", "x")), list(d = d),
             id_medicion = "e", fecha = .cr_fecha + 3600, detalle = "resumen")
  expect_error(rbind(a, b), "fechas distintas", class = "lupa_resumen_no_admitido")
  mod <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "x"),
                instanciar(especializar(nuc$EntidadDuplicada, "ED"), "d"))
  con <- .cr_con("sqlite")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "d", d)
  md <- suppressWarnings(medir_dbi(mod, con, id_medicion = "e", fecha = .cr_fecha))
  expect_identical(attr(md, "cobertura_metricas")$metrica_instanciada, "ED@d")
  ed <- medir(modelo(instanciar(especializar(nuc$EntidadDuplicada, "ED"), "d")), list(d = d),
              id_medicion = "e", fecha = .cr_fecha, detalle = "resumen")
  expect_error(rbind(md, ed), "tiene cifra en una parte", class = "lupa_resumen_no_admitido")
})

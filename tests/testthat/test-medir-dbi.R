# Paso 5: `medir_dbi()` y `plan_medicion_dbi()` en SQLite y DuckDB.

.conectar_prueba_medir_dbi <- function(motor) {
  switch(
    motor,
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    sqlite_ext = DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE),
    duckdb = DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  )
}

test_that("plan_medicion_dbi clasifica vias sin leer filas y valida argumentos", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- .conectar_prueba_medir_dbi("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(
    n = c(1, 2, NA), t = c("a", "b", "c"), cod = c("AB1", "CD2", "EF3"),
    stringsAsFactors = FALSE
  ))
  nuc <- metricas_nucleo()
  mod <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$AtributoDuplicado, "Dup"), "d", "t"),
    instanciar(especializar(nuc$Formato, "Rx", expresion_regular = "^[A-Z]{2}[0-9]$"), "d", "cod"),
    instanciar(especializar(nuc$ErrorEstandar, "SE"), "d", "n")
  )
  # Orden invertido nombra los dos argumentos
  err <- expect_error(plan_medicion_dbi(con, mod))
  expect_match(conditionMessage(err), "modelo", fixed = TRUE)
  expect_match(conditionMessage(err), "conexion", fixed = TRUE)
  # No acepta proteger_datos_personales
  expect_false("proteger_datos_personales" %in% names(formals(plan_medicion_dbi)))

  p <- plan_medicion_dbi(mod, con)
  expect_s3_class(p, "plan_medicion_dbi")
  expect_identical(attr(p, "motor", exact = TRUE), "sqlite")
  expect_identical(p$via, c("sql_consolidado", "sql_dedicado", "no_medible", "no_medible"))
  expect_identical(unname(attr(p, "consultas", exact = TRUE)[c("consolidadas", "dedicadas", "bloques")]),
                   c(1L, 1L, 0L))
  out <- capture.output(ret <- print(p))
  expect_identical(ret, p)

  # Estrategia bloques: Rx pasa a bloques_r con proyeccion solo de cod; SE sigue no_medible
  pb <- plan_medicion_dbi(mod, con, estrategia_no_sql = "bloques")
  expect_identical(pb$via, c("sql_consolidado", "sql_dedicado", "bloques_r", "no_medible"))
  expect_identical(pb$columnas_bloque[[3L]], "cod")
})

test_that("tabla de operadores SQL coincide con memoria y declina lo no traducible (A3, A4)", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    t = c("x", "y", NA, "z", "x", "Y", "x ", NA),
    n = c(1, 2, 3, NA, 5, 2, 0, NA),
    i = c(1L, 2L, NA, 4L, 1L, NA, 3L, 2L),
    x = c(1, NaN, NA, 2.5, 3, 1, 0, NaN),
    l = c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, TRUE),
    f = as.Date(c("2026-01-01", "2026-01-05", NA, "2026-01-10",
                  "2026-01-01", "2026-01-08", NA, "2026-01-03")),
    u = rep(1, 8L),
    stringsAsFactors = FALSE
  )
  umbral <- "x"
  lim <- 2
  formulas <- list(
    eq = ~ t == "x",
    neq = ~ t != "x",
    not_eq = ~ !(t == "x"),
    in_uno = ~ t %in% c("x"),
    not_in_uno = ~ !(t %in% c("x")),
    in_na = ~ t %in% c("x", NA),
    not_in_na = ~ !(t %in% c("x", NA)),
    in_vacio = ~ t %in% character(0),
    not_in_vacio = ~ !(t %in% character(0)),
    es_na = ~ is.na(t),
    no_na = ~ !is.na(t),
    eq_na = ~ t == NA,
    gt_n = ~ n > 2,
    y_or = ~ n > 2 & t == "x",
    neg_or = ~ !(n > 2 | t == "x"),
    na_o_le = ~ is.na(n) | n <= 1,
    in_int_na = ~ i %in% c(1L, NA),
    not_in_int_na = ~ !(i %in% c(1L, NA)),
    ent_txt = ~ t == umbral,
    ent_num = ~ n > lim,
    const_arit = ~ n > 1 + 1,
    letters_in = ~ t %in% letters[24:26],
    nan_gt = ~ x > 1,
    nan_isna = ~ is.na(x),
    nan_in_na = ~ x %in% c(1, NA),
    fecha_ge = ~ f >= as.Date("2026-01-03"),
    logica = ~ l,
    const_true = ~ TRUE,
    orden_texto = ~ t > "x",
    clases_mixtas = ~ n == "2",
    tapa_entorno = local({ u <- "y"; ~ t == u }),
    func_nchar = ~ nchar(t) > 1,
    op_andand = ~ n > 2 && t == "x"
  )

  for (motor in c("sqlite", "sqlite_ext", "duckdb")) {
    con <- .conectar_prueba_medir_dbi(motor)
    datos_m <- d
    if (!identical(motor, "duckdb")) datos_m$x[is.nan(datos_m$x)] <- NA
    DBI::dbWriteTable(con, "d", datos_m)
    leida <- DBI::dbReadTable(con, "d")
    for (nm in names(formulas)) {
      f <- formulas[[nm]]
      for (uso in c("aplicable", "regla")) {
        inst <- if (identical(uso, "aplicable")) {
          instanciar(especializar(nuc$NoNulo, "A", aplicable = f), "d", "n",
                     nombre_instancia = "A")
        } else {
          attrs <- intersect(all.vars(f), names(d))
          if (!length(attrs)) attrs <- "n"
          instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "R", regla = f),
                     "d", attrs, nombre_instancia = "R")
        }
        m <- modelo(inst)
        mem <- suppressWarnings(medir(m, list(d = leida), id_medicion = "op",
                                      fecha = fecha, proteger_datos_personales = TRUE,
                                      detalle = "resumen"))
        dbi <- suppressWarnings(medir_dbi(m, con, id_medicion = "op",
                                          fecha = fecha, proteger_datos_personales = TRUE))
        if (nrow(dbi) > 0L) {
          expect_identical(dbi$numerador, mem$numerador,
                           info = paste(motor, uso, nm))
          expect_identical(dbi$denominador, mem$denominador,
                           info = paste(motor, uso, nm))
          expect_identical(dbi$resultado, mem$resultado,
                           info = paste(motor, uso, nm))
        } else {
          cob_d <- attr(dbi, "cobertura_metricas", exact = TRUE)
          expect_true(is.data.frame(cob_d) && nrow(cob_d) == 1L,
                      info = paste(motor, uso, nm))
          if (!grepl("en la base", cob_d$motivo[[1L]], fixed = TRUE, useBytes = TRUE)) {
            cob_m <- attr(mem, "cobertura_metricas", exact = TRUE)
            expect_identical(cob_d$estado, cob_m$estado,
                             info = paste(motor, uso, nm))
          }
        }
      }
    }
    DBI::dbDisconnect(con)
  }
})

test_that("F1 en plan_medicion_dbi y medir_dbi: boom() nunca se ejecuta en SQLite ni DuckDB", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(t = c("x", "y", NA), n = c(1, 2, NA), stringsAsFactors = FALSE)

  hits <- 0L
  boom <- function() {
    hits <<- hits + 1L
    "x"
  }
  mod_regla <- modelo(
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "B1",
                            regla = ~ t == boom()), "d", "t"),
    instanciar(especializar(nuc$ReglaIntegridadIntraEntidad, "B2",
                            regla = ~ { boom(); t == "x" }), "d", "t"),
    instanciar(especializar(nuc$NoNulo, "B3", aplicable = ~ t == boom()), "d", "n")
  )
  app_boom <- list(n = ~ t == boom())
  mod_nn <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"))

  for (motor in c("sqlite", "duckdb")) {
    con <- .conectar_prueba_medir_dbi(motor)
    DBI::dbWriteTable(con, "d", d)

    hits <- 0L
    p1 <- plan_medicion_dbi(mod_regla, con)
    expect_identical(hits, 0L, info = paste(motor, "plan regla"))
    expect_true(all(p1$via %in% c("no_medible", "no_medible_metodo")))

    hits <- 0L
    r1 <- suppressWarnings(medir_dbi(mod_regla, con, id_medicion = "f1",
                                     fecha = fecha, proteger_datos_personales = TRUE))
    expect_identical(hits, 0L, info = paste(motor, "medir_dbi regla"))
    expect_identical(nrow(r1), 0L)

    hits <- 0L
    p2 <- plan_medicion_dbi(mod_nn, con, aplicabilidad = app_boom)
    expect_identical(hits, 0L, info = paste(motor, "plan aplicabilidad"))
    expect_identical(p2$via, "no_medible")

    hits <- 0L
    r2 <- suppressWarnings(medir_dbi(mod_nn, con, id_medicion = "f1a",
                                     fecha = fecha, aplicabilidad = app_boom,
                                     proteger_datos_personales = TRUE))
    expect_identical(hits, 0L, info = paste(motor, "medir_dbi aplicabilidad"))
    expect_identical(nrow(r2), 0L)

    DBI::dbDisconnect(con)
  }
})

test_that("biseccion, tamano_lote, max_consultas, tabla vacia, NOCASE, guarda UTF-8, coleccion e Id", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  nuc <- metricas_nucleo()
  d <- data.frame(
    n = c(1, 2, NA, 5, 100, 2),
    t = c("a", "b", "a", "c", NA, "A"),
    correo = c("a@b.uy", "c@d.uy", NA, "e@f.uy", "g@h.uy", "i@j.uy"),
    stringsAsFactors = FALSE
  )
  m <- modelo(
    instanciar(especializar(nuc$NoNulo, "NN"), "d", "n"),
    instanciar(especializar(nuc$ValoresPosiblesPorComprension, "R", minimo = 0, maximo = 10), "d", "n"),
    instanciar(especializar(nuc$ValoresPosiblesPorExtension, "E", valores = c("a", "b")), "d", "t"),
    instanciar(especializar(nuc$AtributoDuplicado, "D"), "d", "t")
  )

  for (motor in c("sqlite", "duckdb")) {
    con <- .conectar_prueba_medir_dbi(motor)
    DBI::dbWriteTable(con, "d", d)
    ref <- medir(m, list(d = d), id_medicion = "x", fecha = fecha,
                 proteger_datos_personales = TRUE, detalle = "resumen")
    md <- medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                    proteger_datos_personales = TRUE)
    expect_identical(as.data.frame(unclass(md), stringsAsFactors = FALSE),
                     as.data.frame(unclass(ref), stringsAsFactors = FALSE),
                     info = motor)
    expect_identical(tablero_calidad(md), tablero_calidad(ref), info = motor)

    # tamano_lote = 1 parte la consolidada en lotes separados y da el mismo resultado
    ml <- medir_dbi(m, con, id_medicion = "x", fecha = fecha, tamano_lote = 1L,
                    proteger_datos_personales = TRUE)
    expect_identical(as.data.frame(unclass(ml), stringsAsFactors = FALSE),
                     as.data.frame(unclass(ref), stringsAsFactors = FALSE),
                     info = paste(motor, "tamano_lote"))
    expect_gt(attr(ml, "ejecucion_dbi")$consultas, attr(md, "ejecucion_dbi")$consultas)

    # datos_personales_sin_proteger por nombre cuando proteger_datos_personales = FALSE
    msin <- medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                      proteger_datos_personales = FALSE)
    expect_true("correo" %in% attr(msin, "datos_personales_sin_proteger", exact = TRUE),
                info = motor)
    expect_null(attr(md, "datos_personales_sin_proteger", exact = TRUE))

    # instrumentar = FALSE no guarda SQL ni segundos
    m_ni <- medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                      proteger_datos_personales = TRUE, instrumentar = FALSE)
    expect_null(attr(m_ni, "ejecucion_dbi")$sql)
    expect_null(attr(m_ni, "ejecucion_dbi")$segundos)

    # coleccion() como conexion y DBI::Id en tablas
    col <- coleccion(con, "d")
    mc_col <- medir_dbi(m, col, id_medicion = "x", fecha = fecha,
                        proteger_datos_personales = TRUE)
    expect_identical(as.data.frame(unclass(mc_col), stringsAsFactors = FALSE),
                     as.data.frame(unclass(ref), stringsAsFactors = FALSE),
                     info = paste(motor, "coleccion"))
    mc_id <- medir_dbi(m, con, tablas = list(d = DBI::Id(table = "d")),
                       id_medicion = "x", fecha = fecha,
                       proteger_datos_personales = TRUE)
    expect_identical(as.data.frame(unclass(mc_id), stringsAsFactors = FALSE),
                     as.data.frame(unclass(ref), stringsAsFactors = FALSE),
                     info = paste(motor, "DBI::Id"))

    # Biseccion: romper SQL de una metrica no tumba las otras tres
    local({
    orig_comp <- get(".compilar_especificacion_dbi", envir = asNamespace("lupa"))
    local_mocked_bindings(
      .compilar_especificacion_dbi = function(spec, ...) {
        r <- orig_comp(spec, ...)
        if (identical(spec$metrica, "ValoresPosiblesPorComprension") && !is.null(r$cumple)) {
          r$cumple <- "(columna_inexistente_lupa > 0)"
        }
        r
      },
      .package = "lupa"
    )
    mbis <- suppressWarnings(medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                                       proteger_datos_personales = TRUE))
    cobb <- attr(mbis, "cobertura_metricas", exact = TRUE)
    expect_true("R@d.n" %in% cobb$metrica_instanciada[cobb$estado == "no_medible"],
                info = paste(motor, "biseccion"))
    expect_identical(sort(mbis$metrica_instanciada), c("D@d.t", "E@d.t", "NN@d.n"),
                     info = paste(motor, "biseccion restantes"))
    })

    # Presupuesto max_consultas
    mp <- suppressWarnings(medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                                     max_consultas = 4,
                                     proteger_datos_personales = TRUE))
    cobp <- attr(mp, "cobertura_metricas", exact = TRUE)
    expect_true(any(grepl("max_consultas", cobp$motivo, fixed = TRUE)),
                info = paste(motor, "presupuesto"))

    # Tabla vacia: sin_valores con mismo motivo que memoria
    DBI::dbWriteTable(con, "d", d[0, ], overwrite = TRUE)
    rv <- suppressWarnings(medir(m, list(d = d[0, ]), id_medicion = "x", fecha = fecha,
                                 proteger_datos_personales = TRUE, detalle = "resumen"))
    dv <- suppressWarnings(medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                                     proteger_datos_personales = TRUE))
    expect_identical(nrow(dv), 0L, info = paste(motor, "vacia"))
    expect_identical(
      attr(dv, "cobertura_metricas")[, c("metrica_instanciada", "estado", "motivo")],
      attr(rv, "cobertura_metricas")[, c("metrica_instanciada", "estado", "motivo")],
      info = paste(motor, "vacia motivos")
    )

    # COLLATE NOCASE: dominio y duplicado de texto declinan; NoNulo y rango siguen
    DBI::dbExecute(con, "DROP TABLE d")
    DBI::dbExecute(
      con,
      if (identical(motor, "sqlite")) {
        "CREATE TABLE d (n DOUBLE, t TEXT COLLATE NOCASE, correo TEXT)"
      } else {
        "CREATE TABLE d (n DOUBLE, t VARCHAR COLLATE NOCASE, correo VARCHAR)"
      }
    )
    DBI::dbAppendTable(con, "d", d)
    mn <- suppressWarnings(medir_dbi(m, con, id_medicion = "x", fecha = fecha,
                                     proteger_datos_personales = TRUE))
    cn <- attr(mn, "cobertura_metricas", exact = TRUE)
    expect_true(all(c("E@d.t", "D@d.t") %in% cn$metrica_instanciada),
                info = paste(motor, "NOCASE declina texto"))
    expect_true(all(c("NN@d.n", "R@d.n") %in% mn$metrica_instanciada),
                info = paste(motor, "NOCASE mantiene numerico"))

    DBI::dbDisconnect(con)
  }

  # Guarda de UTF-8 de SQLite: byte invalido que R lee como NA
  con <- .conectar_prueba_medir_dbi("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (t TEXT)")
  DBI::dbExecute(con, "INSERT INTO d VALUES ('a'), ('b'), (CAST(X'61E9' AS TEXT)), (NULL)")
  mu <- modelo(instanciar(especializar(nuc$ValoresPosiblesPorExtension, "E",
                                       valores = c("a", "b")), "d", "t"))
  ru <- suppressWarnings(medir(mu, list(d = DBI::dbReadTable(con, "d")),
                               id_medicion = "u", fecha = fecha,
                               proteger_datos_personales = TRUE, detalle = "resumen"))
  du <- suppressWarnings(medir_dbi(mu, con, id_medicion = "u", fecha = fecha,
                                   proteger_datos_personales = TRUE))
  expect_identical(ru$denominador, 2)
  expect_identical(nrow(du), 0L)
  expect_identical(attr(du, "cobertura_metricas")$estado, "no_medible")
})

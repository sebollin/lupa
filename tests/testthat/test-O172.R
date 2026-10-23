# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

test_that("float8 exacto en PostgreSQL 9.3 y 16", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  sin_sintaxis <- function(objeto) {
    sql <- character()
    if (is.data.frame(objeto) && "sql" %in% names(objeto)) {
      sql <- as.character(objeto$sql)
    } else if (is.list(objeto)) {
      marcos <- Filter(function(x) is.data.frame(x) && "sql" %in% names(x), objeto)
      sql <- unlist(lapply(marcos, function(x) as.character(x$sql)), use.names = FALSE)
    }
    texto <- paste(sql, collapse = "\n")
    expect_false(grepl("syntax error", texto, ignore.case = TRUE), texto)
  }
  columna <- function(perfil, nombre, bloque = "resumen_tabla") {
    perfil[[bloque]]$columnas[perfil[[bloque]]$columnas$columna == nombre, , drop = FALSE]
  }
  valores <- c(
    0.1, 1 / 3, 2.5, 123.456, 1e-5, 1e-300, 5e-324, -0.1,
    1.8e18 + (0:191) * 300, 0.5 + (1:200)
  )
  esperado <- lupa::perfilar(data.frame(f8 = valores, stringsAsFactors = FALSE))$columnas
  esperado <- esperado[esperado$columna == "f8", , drop = FALSE]
  configuraciones <- list(pg93 = c(puerto = 55493L, efd = 0L),
                          pg16 = c(puerto = 55432L, efd = 0L),
                          pg16_efd1 = c(puerto = 55432L, efd = 1L))
  conexiones <- lapply(configuraciones, function(cfg) tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = unname(cfg[["puerto"]]),
      user = "postgres", password = "lupa", dbname = "lupa"
    ), error = function(e) NULL
  ))
  names(conexiones) <- names(configuraciones)
  if (!any(vapply(conexiones, Negate(is.null), logical(1L)))) {
    skip("Ningun PostgreSQL de prueba responde")
  }
  on.exit(lapply(conexiones[!vapply(conexiones, is.null, logical(1L))],
                 function(con) try(DBI::dbDisconnect(con), silent = TRUE)), add = TRUE)

  for (nombre in names(configuraciones)) {
    conexion <- conexiones[[nombre]]
    if (is.null(conexion)) next
    efd <- unname(configuraciones[[nombre]][["efd"]])
    DBI::dbExecute(conexion, paste0("SET extra_float_digits = ", efd))
    tabla <- paste0("r37d_dev_o172_float8_", nombre)
    referencia <- paste0("r37d_dev_o172_ref_", nombre)
    extremos <- paste0("r37d_dev_o172_extremos_", nombre)
    for (objeto in c(referencia, tabla, extremos)) {
      DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", objeto))
    }
    on.exit(lapply(c(referencia, tabla, extremos), function(objeto) {
      try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", objeto)), silent = TRUE)
    }), add = TRUE)
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla,
      " (id integer PRIMARY KEY, f8 double precision, rv real, estado text)",
      " WITH (fillfactor = 10)"
    ))
    filas <- vapply(seq_along(valores), function(i) paste0(
      "(", i, ", ", sprintf("%.17g", valores[[i]]),
      "::double precision, ", if (i <= 20L) "-99.9" else sprintf("%.17g", i / 2),
      "::real, 'ok')"
    ), character(1L))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
    ))
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", referencia,
      " AS SELECT id, 'fila ' || id::text AS etiqueta FROM ", tabla
    ))
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", extremos, " (id integer, f8 double precision)"
    ))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", extremos, " VALUES (1, ", sprintf("%.17g", .Machine$double.xmax),
      "::double precision), (2, ", sprintf("%.17g", -.Machine$double.xmax),
      "::double precision), (3, 0.1::double precision), (4, 5e-324::double precision)"
    ))

    completo <- suppressMessages(perfilar_dbi(
      conexion, tabla, bloque_muestra = "solo_agregados", max_consultas = Inf,
      proteger = FALSE
    ))
    muestra <- suppressMessages(perfilar_dbi(
      conexion, tabla, universo = "muestra_motor", muestra_motor = 200,
      muestra = 200, orden_muestra = "id", sentinelas_numericos = -99.9,
      max_consultas = Inf, proteger = FALSE
    ))
    bloques <- suppressMessages(perfilar_dbi(
      conexion, tabla, bloque_filas = 17, sentinelas_numericos = -99.9,
      max_consultas = Inf, proteger = FALSE
    ))
    solo_agregados <- suppressMessages(perfilar_dbi(
      conexion, tabla, universo = "muestra_motor", muestra_motor = 200,
      orden = "id", bloque_muestra = "solo_agregados",
      sentinelas_numericos = -99.9, max_consultas = Inf, proteger = FALSE
    ))
    perfiles <- list(completo, muestra, bloques, solo_agregados)
    for (perfil in perfiles) {
      sin_sintaxis(perfil$resumen_tabla)
      if (!is.null(perfil$perfil_muestra)) sin_sintaxis(perfil$perfil_muestra)
    }
    f8_completo <- columna(completo, "f8")
    expect_equal(f8_completo$n_distintos, esperado$n_distintos)
    expect_equal(f8_completo$minimo, esperado$minimo)
    expect_equal(f8_completo$maximo, esperado$maximo)
    expect_equal(f8_completo$media, esperado$media, tolerance = 1e-12)
    f8_muestra <- columna(muestra, "f8")
    filas_muestra <- muestra$resumen_tabla$meta$muestreo$filas_obtenidas
    expect_true(length(filas_muestra) == 1L && filas_muestra > 0L &&
                filas_muestra < length(valores))
    expect_lte(f8_muestra$n_distintos, filas_muestra)
    expect_equal(columna(bloques, "f8", "perfil_muestra")$n_distintos,
                 length(unique(valores)))
    expect_true(any(columna(completo, "estado")$moda == "ok", na.rm = TRUE))
    if (efd == 0L) expect_true(any(grepl(
      "f8|float8", completo$resumen_tabla$meta$lectura_convertida$columna,
      ignore.case = TRUE
    )))

    extremos_perfil <- suppressMessages(perfilar_dbi(
      conexion, extremos, bloque_muestra = "solo_agregados",
      max_consultas = Inf, proteger = FALSE
    ))
    extremos_columna <- columna(extremos_perfil, "f8")
    expect_equal(extremos_columna$minimo, -.Machine$double.xmax)
    expect_equal(extremos_columna$maximo, .Machine$double.xmax)
    sin_sintaxis(extremos_perfil$resumen_tabla)

    plan <- plan_perfilado_dbi(conexion, tabla, muestra = 20, max_consultas = Inf)
    sin_sintaxis(plan)
    if (efd == 0L) {
      col <- coleccion(conexion, c(tabla, referencia))
      pares <- data.frame(tabla_1 = referencia, tabla_2 = tabla,
                          stringsAsFactors = FALSE)
      relaciones <- relaciones_coleccion(col, pares, muestra = Inf)
      coleccion_perfiles <- perfilar_coleccion(
        col, muestra = 20, conservar_perfiles = TRUE,
        bloque_muestra = "solo_agregados"
      )
      sin_sintaxis(relaciones)
      expect_true(is.data.frame(relaciones$relaciones))
      expect_true(is.list(coleccion_perfiles$perfiles))
    }
  }
})

test_that("retirar la segunda columna no resuelve un par", {
  datos <- data.frame(
    a = rep(c("aa", "bb", "cc"), length.out = 120L),
    b = rep(c("aa", "bb", "cc"), length.out = 120L),
    stringsAsFactors = FALSE
  )
  anterior <- lupa::perfilar(
    datos, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  actual <- lupa::perfilar(
    datos["a"], fecha = as.POSIXct("2026-02-01", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  deriva <- as.data.frame(lupa::comparar_perfiles(anterior, actual))
  expect_true(any(deriva$columna == "b" & deriva$aspecto == "columna" &
                  deriva$cambio == "desaparecida"))
  expect_false(any(deriva$aspecto == "hallazgo" &
                   deriva$valor_anterior == "columnas_duplicadas" &
                   deriva$cambio == "resuelto"))
})


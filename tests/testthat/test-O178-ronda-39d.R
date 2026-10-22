testthat::test_that("O178: la privacidad recorre atributos y colecciones", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tablas <- c("r39d_dev_o178_f1", "r39d_dev_o178_f2")
  on.exit({
    for (tabla in tablas) {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
    }
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  for (tabla in tablas) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (id integer, txt text)"
    ))
  }
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tablas[[1L]], " VALUES ",
    "(1, 'ZQ7MARCA999'), (2, 'ZQ7MARCA999'), (3, 'otro')"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tablas[[2L]], " VALUES ",
    "(1, 'ZQ7MARCA999'), (2, 'otro'), (3, 'mas')"
  ))
  encontrar <- function(objeto) {
    encontrados <- character()
    recorrer <- function(valor, ruta) {
      atributos <- attributes(valor)
      if (length(atributos)) {
        for (atributo in names(atributos)) {
          recorrer(atributos[[atributo]], paste0(ruta, "@", atributo))
        }
      }
      if (is.data.frame(valor)) {
        for (nombre in names(valor)) {
          recorrer(valor[[nombre]], paste0(ruta, "$", nombre))
        }
      } else if (is.list(valor)) {
        nombres <- names(valor)
        for (i in seq_along(valor)) {
          nombre <- if (is.null(nombres)) as.character(i) else nombres[[i]]
          recorrer(valor[[i]], paste0(ruta, "$", nombre))
        }
      } else if (grepl(
        "ZQ7MARCA999",
        paste(capture.output(dput(valor)), collapse = " "),
        fixed = TRUE
      )) {
        encontrados <<- c(encontrados, ruta)
      }
    }
    recorrer(objeto, "resultado")
    encontrados
  }
  argumentos <- list(
    proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE,
    incluir_valores = FALSE
  )
  tabla <- suppressMessages(do.call(
    lupa::perfilar_dbi,
    c(list(con, tablas[[1L]], bloque_filas = 3L), argumentos)
  ))
  muestra <- suppressMessages(do.call(
    lupa::perfilar_dbi,
    c(list(
      con, tablas[[1L]], universo = "muestra_motor", muestra_motor = 3L
    ), argumentos)
  ))
  col <- lupa::coleccion(con, tablas)
  coleccion <- suppressMessages(do.call(
    lupa::perfilar_coleccion,
    c(list(col, conservar_perfiles = TRUE), argumentos)
  ))
  testthat::expect_length(encontrar(tabla), 0L)
  testthat::expect_length(encontrar(muestra), 0L)
  testthat::expect_length(encontrar(coleccion), 0L)
})

testthat::test_that("O178: faltantes se comparan en dos perfiles DBI muestreados", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tabla <- "r39d_dev_o178_a1"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (id integer, monto double precision)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla,
    " SELECT i, i::double precision FROM generate_series(1, 2000) AS i"
  ))
  args <- list(
    conexion = con, tabla = tabla, muestra = 200L, orden_muestra = "id",
    metricas = c("validos", "basicos"), proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  )
  antes <- suppressMessages(do.call(lupa::perfilar_dbi, args))
  DBI::dbExecute(con, paste0("UPDATE ", tabla, " SET monto = NULL"))
  despues <- suppressMessages(do.call(lupa::perfilar_dbi, args))
  deriva <- suppressMessages(lupa::comparar_perfiles(
    antes$perfil_muestra, despues$perfil_muestra
  ))
  fila <- deriva[deriva$aspecto == "faltantes" & deriva$columna == "monto", ,
                 drop = FALSE]
  testthat::expect_equal(nrow(fila), 1L)
  testthat::expect_equal(as.numeric(fila$valor_anterior), 0)
  testthat::expect_equal(as.numeric(fila$valor_actual), 1)
  testthat::expect_equal(as.numeric(fila$delta), 1)
  testthat::expect_identical(as.character(fila$severidad), "error")
})

testthat::test_that("O178: decimal grande conserva identidad en muestra_motor", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RPostgres")
  testthat::skip_if_not_installed("bit64")
  con <- tryCatch(
    DBI::dbConnect(
      RPostgres::Postgres(), host = "127.0.0.1", port = 55432,
      user = "postgres", password = "lupa", dbname = "lupa"
    ),
    error = function(e) NULL
  )
  if (is.null(con)) testthat::skip("PostgreSQL real no disponible en 55432.")
  tabla <- "r39d_dev_o178_a2"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
        silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }, add = TRUE)
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (idd numeric(18,0), grupo integer)"
  ))
  ids <- as.character(bit64::as.integer64("201903150001234500") + 0:59)
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste(sprintf("(%s, %d)", ids, seq_along(ids) %% 3), collapse = ",")
  ))
  salida <- suppressMessages(lupa::perfilar_dbi(
    con, tabla, universo = "muestra_motor", muestra_motor = 40L,
    clave = "idd", proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE
  ))
  muestra <- salida$perfil_muestra$columnas
  muestra <- muestra[muestra$columna == "idd", , drop = FALSE]
  n_validos <- as.numeric(muestra$n)
  n_distintos <- as.numeric(muestra$n_distintos)
  frecuencia <- as.numeric(muestra$frecuencia_moda)
  testthat::expect_equal(n_distintos, n_validos)
  testthat::expect_equal(frecuencia, 1)
  hallazgos <- salida$perfil_muestra$hallazgos
  testthat::expect_false(any(
    as.character(hallazgos$tipo_hallazgo) %in%
      c("clave_no_unica", "filas_duplicadas") &
      as.character(hallazgos$severidad) == "error"
  ))
})

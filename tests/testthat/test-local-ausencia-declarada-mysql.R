# La politica de ausencia declarada en MySQL y MariaDB. Necesitan RMySQL y
# RMariaDB, que el paquete no declara: viven en un archivo local
# (`test-local-*`, fuera del tarball), como las demas pruebas de esos motores, y
# se saltean solas sin el motor.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.adl_conectar <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  puerto_def <- if (maria) "33306" else "33307"
  puerto_var <- if (maria) "LUPA_MARIADB_PORT" else "LUPA_MYSQL_PORT"
  puerto <- as.integer(Sys.getenv(puerto_var, Sys.getenv("LUPA_DB_PORT", puerto_def)))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = Sys.getenv("LUPA_MARIADB_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MARIADB_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MARIADB_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = Sys.getenv("LUPA_MYSQL_HOST", Sys.getenv("LUPA_DB_HOST", "127.0.0.1")),
        port = puerto,
        dbname = Sys.getenv("LUPA_MYSQL_DB", Sys.getenv("LUPA_DB_NAME", "lupa")),
        username = Sys.getenv("LUPA_MYSQL_USER", Sys.getenv("LUPA_DB_USER", "root")),
        password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

.adl_dbi <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE,
    analizar_dependencias = FALSE, ...
  )))
}

.adl_fila <- function(columnas, columna) {
  columnas[columnas$columna == columna, , drop = FALSE]
}

.adl_borrar <- function(con, tabla) {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
}

.adl_motores <- c(mysql = FALSE, mariadb = TRUE)

test_that("un centinela con fraccion sobre una columna FLOAT sale del resumen y de la muestra", {
  expect_identical(length(.adl_motores), 2L)
  for (motor in names(.adl_motores)) {
    con <- .adl_conectar(.adl_motores[[motor]])
    .adl_borrar(con, "lupa_aus_real")
    DBI::dbExecute(con, "CREATE TABLE lupa_aus_real (id INT PRIMARY KEY, x FLOAT)")
    DBI::dbExecute(con, paste(
      "INSERT INTO lupa_aus_real VALUES (1, -9.9), (2, -9.9), (3, -9.9),",
      "(4, 1.5), (5, 2.5), (6, NULL)"
    ))
    caminos <- list(
      consultas = list(), bloques = list(bloque_filas = 2L),
      muestra_motor = list(universo = "muestra_motor", muestra_motor = 6L)
    )
    expect_identical(length(caminos), 3L)
    for (camino in names(caminos)) {
      info <- paste(motor, camino)
      r <- do.call(.adl_dbi, c(
        list(con, "lupa_aus_real", sentinelas_numericos = -9.9),
        caminos[[camino]]
      ))
      fila <- .adl_fila(r$resumen_tabla$columnas, "x")
      # A mano: sin los tres -9.9 quedan 1,5 y 2,5.
      expect_equal(fila$minimo, 1.5, info = info)
      expect_equal(fila$media, 2, info = info)
      expect_equal(fila$mediana, 2, info = info)
      expect_equal(fila$n_negativos, 0, info = info)
      expect_equal(fila$n_valores_excluidos_resumen, 3, info = info)
      muestra <- .adl_fila(r$perfil_muestra$columnas, "x")
      expect_equal(muestra$minimo, 1.5, info = info)
      expect_equal(muestra$n_faltantes_disfrazados, 3, info = info)
    }
    .adl_borrar(con, "lupa_aus_real")
    DBI::dbDisconnect(con)
  }
})

test_that("en un BIGINT el centinela 2^53 no saca a su vecino del resumen", {
  skip_if_not_installed("bit64")
  expect_identical(length(.adl_motores), 2L)
  for (motor in names(.adl_motores)) {
    con <- .adl_conectar(.adl_motores[[motor]])
    .adl_borrar(con, "lupa_aus_ancho")
    DBI::dbExecute(con, "CREATE TABLE lupa_aus_ancho (id INT PRIMARY KEY, x BIGINT)")
    DBI::dbExecute(con, paste(
      "INSERT INTO lupa_aus_ancho VALUES (1, 9007199254740992),",
      "(2, 9007199254740993), (3, 9007199254740993), (4, 1), (5, 2), (6, 3)"
    ))
    r <- .adl_dbi(con, "lupa_aus_ancho", sentinelas_numericos = 2^53)
    fila <- .adl_fila(r$resumen_tabla$columnas, "x")
    # 2^53 esta una vez; pasada a doble, la columna le suma sus dos vecinos.
    expect_equal(fila$n_valores_excluidos_resumen, 1, info = motor)
    expect_equal(fila$n_validos, 6, info = motor)
    muestra <- .adl_fila(r$perfil_muestra$columnas, "x")
    expect_equal(muestra$n_valores_excluidos_resumen, 1, info = motor)
    .adl_borrar(con, "lupa_aus_ancho")
    DBI::dbDisconnect(con)
  }
})

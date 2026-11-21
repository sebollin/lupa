# MySQL y MariaDB no son dependencias declaradas del paquete; estas pruebas
# locales se mantienen fuera del tarball y se saltan sin su controlador o clave.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.mdt_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  prefijo <- if (maria) "LUPA_MARIADB" else "LUPA_MYSQL"
  clave <- Sys.getenv(paste0(prefijo, "_PWD"), unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", prefijo, "_PWD."))
  puerto <- as.integer(Sys.getenv(
    paste0(prefijo, "_PORT"), if (maria) "33306" else "33307"
  ))
  con <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv(paste0(prefijo, "_HOST"), "127.0.0.1"),
        port = puerto, dbname = Sys.getenv(paste0(prefijo, "_DB"), "lupa"),
        username = Sys.getenv(paste0(prefijo, "_USER"), "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv(paste0(prefijo, "_HOST"), "127.0.0.1"),
        port = puerto, dbname = Sys.getenv(paste0(prefijo, "_DB"), "lupa"),
        username = Sys.getenv(paste0(prefijo, "_USER"), "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(con), paste0(if (maria) "MariaDB" else "MySQL", " no disponible."))
  con
}

test_that("MySQL y MariaDB reproducen TINYINT(1), FLOAT, BIT y texto Unicode", {
  con <- .mdt_mysql(maria = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nuc <- metricas_nucleo()
  mod_tiny <- modelo(.mdt_inst(
    nuc$ReglaIntegridadIntraEntidad, "TINYINT_mysql", "d", "x",
    regla = ~ is.na(x) | x == TRUE
  ))
  .mdt_comparar(
    con, "lupa_medir_mysql_tiny", "x TINYINT(1)",
    "(1), (0), (NULL)", mod_tiny
  )
  mod_float <- modelo(.mdt_inst(
    nuc$ValoresPosiblesPorExtension, "FLOAT_mysql", "d", "x", valores = 0.3
  ))
  .mdt_comparar(
    con, "lupa_medir_mysql_float", "x FLOAT",
    "(0.3), (0.1), (NULL)", mod_float
  )
  mod_bit <- modelo(.mdt_inst(nuc$NoNulo, "BIT_mysql", "d", "x"))
  .mdt_comparar(
    con, "lupa_medir_mysql_bit", "x BIT(1)",
    "(b'1'), (b'0'), (NULL)", mod_bit
  )
  acento <- intToUtf8(0x00E1L)
  enie <- intToUtf8(0x00F1L)
  may_acento <- intToUtf8(0x00C1L)
  may_enie <- intToUtf8(0x00D1L)
  mod_texto <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_mysql", "d", "x", valores_nulos = c(acento, enie)
  ))
  valores <- paste0(
    "('", may_acento, "'), ('", acento, "'), ('", may_enie,
    "'), ('", enie, "'), ('dato'), (NULL)"
  )
  .mdt_comparar(
    con, "lupa_medir_mysql_t", "x VARCHAR(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_bin",
    valores, mod_texto
  )
})

test_that("MariaDB mantiene la equivalencia de TINYINT y texto Unicode", {
  con <- .mdt_mysql(maria = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nuc <- metricas_nucleo()
  mod_tiny <- modelo(.mdt_inst(
    nuc$ReglaIntegridadIntraEntidad, "TINYINT_maria", "d", "x",
    regla = ~ is.na(x) | x == TRUE
  ))
  .mdt_comparar(
    con, "lupa_medir_maria_tiny", "x TINYINT(1)",
    "(1), (0), (NULL)", mod_tiny
  )
  acento <- intToUtf8(0x00E1L)
  enie <- intToUtf8(0x00F1L)
  may_acento <- intToUtf8(0x00C1L)
  may_enie <- intToUtf8(0x00D1L)
  mod_texto <- modelo(.mdt_inst(
    nuc$NoNulo, "TEXTO_maria", "d", "x", valores_nulos = c(acento, enie)
  ))
  valores <- paste0(
    "('", may_acento, "'), ('", acento, "'), ('", may_enie,
    "'), ('", enie, "'), ('dato'), (NULL)"
  )
  .mdt_comparar(
    con, "lupa_medir_maria_t", "x VARCHAR(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_nopad_bin",
    valores, mod_texto
  )
})

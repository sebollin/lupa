# La fecha cero de MySQL y MariaDB (`0000-00-00`, que el `sql_mode` por omision
# de MariaDB admite): RMariaDB la lee como la fecha `-1-11-30` -una fecha valida
# mas-, y el perfil la contaba como dato, la publicaba como moda y callaba los
# faltantes que RMySQL, que entrega el texto, deja ver. En los dos controladores
# tiene que dar lo mismo. Aparte porque los controladores no estan en Suggests.
testthat::skip_on_cran()

.dfc_conectar <- function(maria) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  clave <- Sys.getenv(if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD")
  if (!nzchar(clave)) skip("falta la clave del motor")
  tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_MARIA_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MARIA_PORT", "33306")),
        dbname = Sys.getenv("LUPA_MARIA_NAME", "lupa"),
        username = Sys.getenv("LUPA_MARIA_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_MYSQL_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_MYSQL_PORT", "33307")),
        dbname = Sys.getenv("LUPA_MYSQL_NAME", "lupa"),
        username = Sys.getenv("LUPA_MYSQL_USER", "root"), password = clave
      )
    }
  }, error = function(e) skip("motor local no disponible"))
}

# Tres fechas cero y cuarenta fechas: la forma en que `perfilar()` reconoce el
# centinela sobre texto.
.dfc_crear <- function(con, tabla) {
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
  DBI::dbExecute(con, "SET SESSION sql_mode = ''")
  DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (f DATE, t DATETIME)"))
  fechas <- format(as.Date("2024-01-01") + 0:39)
  horas <- format(as.POSIXct("2024-01-01 08:00:00", tz = "UTC") + 3600 * (0:39),
                  "%Y-%m-%d %H:%M:%S")
  f <- c(rep("0000-00-00", 3), fechas)
  t <- c(rep("0000-00-00 00:00:00", 3), horas)
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ",
    paste0("('", f, "', '", t, "')", collapse = ", ")
  ))
  DBI::dbExecute(con, "SET SESSION sql_mode = DEFAULT")
}

.dfc_columna <- function(r, nombre) {
  col <- r$resumen_tabla$columnas
  col[col$columna == nombre, , drop = FALSE]
}

# La tabla existe mientras corre `codigo(con)`, y se borra y se cierra la
# conexion aunque falle.
.dfc_con_tabla <- function(maria, tabla, codigo) {
  con <- .dfc_conectar(maria)
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  })
  .dfc_crear(con, tabla)
  codigo(con)
}

test_that("la fecha cero se cuenta como fecha centinela y no se publica como fecha ni como moda", {
  for (maria in c(TRUE, FALSE)) {
    tabla <- "lupa_lector_fcero"
    vias <- list(
      sql = list(muestra = 1e6),
      spool = list(universo = "muestra_motor", muestra_motor = 100000L, muestra = 1e6)
    )
    .dfc_con_tabla(maria, tabla, function(con) {
      for (via in names(vias)) {
        r <- do.call(perfilar_dbi, c(
          list(con, tabla, analizar_dependencias = FALSE), vias[[via]]
        ))
        etiqueta <- paste(if (maria) "mariadb" else "mysql", via)
        for (nombre in c("f", "t")) {
          fila <- .dfc_columna(r, nombre)
          # La moda es la fecha cero tal como esta guardada, no una fecha del ano -1.
          expect_false(grepl("^-", as.character(fila$moda)), label = etiqueta)
          expect_match(as.character(fila$moda), "^0000-00-00", label = etiqueta)
          expect_equal(fila$frecuencia_moda, 3, label = etiqueta)
        }
        # Que la columna llegue como texto se dice, con las dos columnas.
        cob <- r$resumen_tabla$cobertura
        fila_cob <- cob[cob$bloque == "perfil_muestra" & cob$estado == "degradado" &
                          grepl("fecha cero", cob$motivo, fixed = TRUE), ]
        expect_identical(nrow(fila_cob), 1L, label = etiqueta)
        expect_true(all(grepl("`f`", fila_cob$motivo, fixed = TRUE) &
                          grepl("`t`", fila_cob$motivo, fixed = TRUE)), label = etiqueta)
        pm <- if (identical(via, "spool")) r$resumen_tabla$columnas else {
          r$perfil_muestra$columnas
        }
        for (nombre in c("f", "t")) {
          fila <- pm[pm$columna == nombre, , drop = FALSE]
          # Las tres fechas cero son faltantes disfrazados, como sobre texto.
          expect_equal(fila$n_faltantes_disfrazados, 3, label = paste(etiqueta, nombre))
        }
      }
    })
  }
})

test_that("la fuente por bloques no cuenta la fecha cero como fecha valida", {
  for (maria in c(TRUE, FALSE)) {
    tabla <- "lupa_lector_fcero_bloques"
    .dfc_con_tabla(maria, tabla, function(con) {
      r <- perfilar_dbi(
        con, tabla, analizar_dependencias = FALSE, bloque_filas = 3L, muestra = 1e6
      )
      fila <- .dfc_columna(r, "f")
      expect_false(grepl("^-", as.character(fila$moda)))
      expect_match(as.character(fila$moda), "^0000-00-00")
    })
  }
})

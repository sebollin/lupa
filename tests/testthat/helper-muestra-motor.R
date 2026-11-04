# Ayudas de las pruebas de la muestra del motor que comparten
# `test-arreglos-muestra-motor.R` y `test-local-mysql.R`.

.arrm_perfil_sin_aviso <- function(conexion, tabla, universo, bloque) {
  avisos <- list()
  resultado <- withCallingHandlers(
    perfilar_dbi(
      conexion, tabla, universo = universo,
      muestra_motor = if (identical(universo, "muestra_motor")) 500L else NULL,
      muestra = if (identical(universo, "muestra_motor")) 500L else Inf,
      bloque_muestra = bloque, proteger_datos_personales = FALSE
    ),
    warning = function(w) {
      # RMySQL avisa por su propia conversion de INTEGER y DECIMAL a numeric;
      # no es una condicion de lupa y no debe tapar sus avisos de degradacion.
      ruido_rmysql <- grepl(
        "MySQL column|Unsigned INTEGER|unrecognized MySQL field type",
        conditionMessage(w),
        fixed = FALSE, useBytes = TRUE
      )
      if (ruido_rmysql) {
        invokeRestart("muffleWarning")
      }
      avisos[[length(avisos) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(avisos, 0L)
  expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
  resultado
}

.arrm_probar_perfil_sin_aviso <- function(conexion, tabla) {
  for (universo in c("tabla_completa", "muestra_motor")) {
    for (bloque in c("solo_agregados", "con_muestra")) {
      resultado <- .arrm_perfil_sin_aviso(
        conexion, tabla, universo = universo, bloque = bloque
      )
      expect_s3_class(resultado, "perfil_dbi")
    }
  }
  invisible(NULL)
}

.arrm_tabla_normal <- function(conexion, tabla) {
  datos <- data.frame(
    id = seq_len(500L), x = seq_len(500L) / 2,
    t = paste0("x", seq_len(500L)),
    f = as.Date("2026-01-01") + seq_len(500L) - 1L,
    stringsAsFactors = FALSE
  )
  try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
  senas <- .senas_conexion_dbi(conexion)
  if (grepl("mysql|maria", senas, perl = TRUE, useBytes = TRUE)) {
    tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla))
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla_sql,
      " (id INTEGER, x DOUBLE, t VARCHAR(64), f DATE)"
    ))
    filas <- vapply(seq_len(nrow(datos)), function(i) paste0(
      "(", datos$id[[i]], ",", format(datos$x[[i]], scientific = FALSE), ",",
      DBI::dbQuoteString(conexion, datos$t[[i]]), ",",
      DBI::dbQuoteString(conexion, as.character(datos$f[[i]])), ")"
    ), character(1L))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla_sql, " (id, x, t, f) VALUES ",
      paste(filas, collapse = ",")
    ))
  } else {
    DBI::dbWriteTable(conexion, tabla, datos)
  }
  invisible(NULL)
}

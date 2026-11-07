.abq_con_pg <- function(puerto) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = puerto, user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), paste0("PostgreSQL no disponible en ", puerto, "."))
  con
}

.abq_con_mssql <- function() {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"),
    TDS_Version = "8.0", TrustServerCertificate = "yes", timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

test_that("bloque_filas escala extremos y conserva la media compensada", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .abq_con_pg(puerto)
    tabla <- paste0("abq_extremos_", puerto)
    tryCatch({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      valores <- 1e307 * (1 + 16 * (0:199) / 199)
      datos <- data.frame(
        x_huge = c(1e300, 0, 0, 0, rep(NA_real_, 196)),
        x_cancel = c(1e200, 1, 1, -1e200, rep(NA_real_, 196)),
        x_tiny = c(1e-160, 2e-160, 3e-160, rep(NA_real_, 197)),
        x_many = valores,
        x_comun = as.numeric(1:200)
      )
      DBI::dbWriteTable(con, tabla, datos, overwrite = TRUE)
      por_bloques <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, bloque_filas = 1L,
        metricas = c("basicos", "desvio"), incluir_valores = TRUE,
        proteger_datos_personales = FALSE
      )))
      columnas <- por_bloques$resumen_tabla$columnas
      for (campo in c("x_huge", "x_cancel", "x_tiny", "x_many")) {
        fila <- columnas[columnas$columna == campo, , drop = FALSE]
        expect_true(is.finite(fila$media[[1L]]), info = campo)
        expect_true(is.finite(fila$desvio[[1L]]), info = campo)
        estados <- por_bloques$resumen_tabla$sql
        estados <- estados[estados$columna == campo &
                             estados$metrica %in% c("media", "desvio"), ,
                           drop = FALSE]
        expect_true(all(estados$estado == "calculado"), info = campo)
      }
      expect_identical(
        columnas$media[columnas$columna == "x_huge"], 2.5e299
      )
      expect_identical(
        columnas$desvio[columnas$columna == "x_huge"], 5e299
      )
      expect_identical(
        columnas$media[columnas$columna == "x_cancel"], 0.5
      )
      expect_equal(
        columnas$desvio[columnas$columna == "x_cancel"],
        8.16496580927726e199, tolerance = 1e-12
      )
      expect_identical(
        columnas$media[columnas$columna == "x_tiny"], 2e-160
      )
      expect_identical(
        columnas$desvio[columnas$columna == "x_tiny"], 1e-160
      )
      expect_equal(
        columnas$desvio[columnas$columna == "x_many"],
        4.653602774991045e307, tolerance = 1e-12
      )
      sin_bloques <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, metricas = c("basicos", "desvio"),
        incluir_valores = TRUE, proteger_datos_personales = FALSE
      )))
      comun_bloques <- columnas[columnas$columna == "x_comun", , drop = FALSE]
      comun_sql <- sin_bloques$resumen_tabla$columnas[
        sin_bloques$resumen_tabla$columnas$columna == "x_comun", , drop = FALSE
      ]
      expect_identical(comun_bloques$media, comun_sql$media)
      expect_identical(comun_bloques$desvio, comun_sql$desvio)
    }, finally = {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    })
  }
})

test_that("modas binarias y decimales son exactas en las tres vias", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .abq_con_pg(puerto)
    tabla <- paste0("abq_modas_", puerto)
    tryCatch({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla, " (id int, d numeric(38,0), b bytea)"
      ))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla,
        " VALUES (1, '12345678901234567890', decode('','hex'))",
        ",(2, '12345678901234567890', decode('','hex'))",
        ",(3, '7', decode('00','hex'))"
      ))
      corridas <- list(
        tabla_completa = list(),
        bloques = list(bloque_filas = 1L),
        muestra = list(universo = "muestra_motor", muestra_motor = 3L,
                       muestra = 3L)
      )
      for (nombre in names(corridas)) {
        resultado <- suppressWarnings(suppressMessages(do.call(
          perfilar_dbi,
          c(list(
            conexion = con, tabla = tabla, metricas = "moda",
            incluir_valores = TRUE, proteger_datos_personales = FALSE
          ), corridas[[nombre]])
        )))
        fila_d <- resultado$resumen_tabla$columnas[
          resultado$resumen_tabla$columnas$columna == "d", , drop = FALSE
        ]
        fila_b <- resultado$resumen_tabla$columnas[
          resultado$resumen_tabla$columnas$columna == "b", , drop = FALSE
        ]
        expect_identical(fila_d$moda, "12345678901234567890", info = nombre)
        expect_equal(fila_d$frecuencia_moda, 2, info = nombre)
        expect_identical(
          fila_b$moda, "0:d41d8cd98f00b204e9800998ecf8427e", info = nombre
        )
        expect_equal(fila_b$frecuencia_moda, 2, info = nombre)
      }
    }, finally = {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    })
  }
})

test_that("la guarda decimal conserva su motivo con privacidad", {
  skip_on_cran()
  for (puerto in c(55432L, 55493L)) {
    con <- .abq_con_pg(puerto)
    tabla <- paste0("abq_guardas_", puerto)
    tryCatch({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbExecute(con, paste0(
        "CREATE TABLE ", tabla,
        " (ancho numeric(38,0), borde numeric(22,2))"
      ))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla,
        " VALUES ('9007199254740992', '9007199254740991.99'),",
        "('9007199254740993', '1.00')"
      ))
      resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, metricas = c("basicos", "desvio"),
        incluir_valores = FALSE, proteger_datos_personales = FALSE
      )))
      sql <- resultado$resumen_tabla$sql
      columnas <- resultado$resumen_tabla$columnas
      # La media y el desvio salen de las sumas exactas en el motor y se
      # publican; los ceros y los negativos, que salen de los dobles, siguen
      # negados, con el motivo del decimal y no el de la privacidad.
      exactos <- list(ancho = c(media = 0x1p+53, desvio = 0x1.6a09e667f3bcdp-1),
                      borde = c(media = 0x1p+52, desvio = 0x1.6a09e667f3bccp+52))
      for (campo in c("ancho", "borde")) {
        for (metrica in c("media", "desvio")) {
          expect_identical(
            sql$estado[sql$columna == campo & sql$metrica == metrica],
            "calculado", info = paste(campo, metrica)
          )
          expect_identical(
            as.numeric(columnas[[metrica]][columnas$columna == campo]),
            exactos[[campo]][[metrica]], info = paste(campo, metrica)
          )
        }
        filas <- sql[sql$columna == campo &
                       sql$metrica %in% c("n_ceros", "n_negativos"), ,
                     drop = FALSE]
        expect_true(nrow(filas) > 0L, info = campo)
        expect_true(all(filas$estado == "no_disponible"), info = campo)
        expect_true(all(grepl(
          "decimal entregado como doble", filas$motivo, fixed = TRUE
        )), info = campo)
        expect_false(any(grepl(
          "incluir_valores", filas$motivo, fixed = TRUE
        )), info = campo)
        extremos <- sql[sql$columna == campo &
                          sql$metrica %in% c("minimo", "maximo"), ,
                        drop = FALSE]
        expect_true(all(extremos$estado == "omitido_por_privacidad"),
                    info = campo)
      }
    }, finally = {
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    })
  }
})

test_that("SQL Server publica la moda de image en tabla completa", {
  skip_on_cran()
  con <- .abq_con_mssql()
  tabla <- "abq_image"
  tryCatch({
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id int, b image)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " VALUES (1, CONVERT(varbinary(max),'0x00',1)),",
      "(2, CONVERT(varbinary(max),'0x00',1)),",
      "(3, CONVERT(varbinary(max),'0x01',1))"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = "moda", incluir_valores = TRUE,
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "b", , drop = FALSE
    ]
    expect_identical(fila$moda, "1:93b885adfe0da089cdf634904fd59f71")
    expect_identical(fila$frecuencia_moda, 2)
    estado <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$columna == "b" &
        resultado$resumen_tabla$sql$metrica == "moda", , drop = FALSE
    ]
    expect_identical(estado$estado, "calculado")
    perfil <- resultado$perfil_muestra$columnas
    perfil <- perfil[perfil$columna == "b", , drop = FALSE]
    expect_identical(
      perfil$moda, "1:93b885adfe0da089cdf634904fd59f71"
    )
  }, finally = {
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  })
})

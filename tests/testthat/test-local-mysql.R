# Pruebas contra MySQL, MariaDB y SQL Server por JDBC en motores locales.
#
# Usan controladores que el paquete no declara (RMySQL, RMariaDB, RJDBC) y
# motores que solo existen en la maquina de desarrollo, asi que no viajan en el
# paquete: `.Rbuildignore` excluye `tests/testthat/test-local-*.R`. La suite de
# desarrollo (`devtools::test()`) las corre igual, desde el arbol fuente.

.adv_sd_referencia_mysql <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  y <- x / escala
  m <- mean(y)
  d <- y - m
  sqrt(max(0, (sum(d^2) - sum(d)^2 / length(y)) / (length(y) - 1L))) * escala
}

.adv_media_referencia_mysql <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  escala <- 2^(floor(log2(max(abs(x)))) - 1)
  sum(x / escala) / length(x) * escala
}

test_that("RJDBC publica el producto del motor cuando esta disponible", {
  skip_on_cran()
  skip_if_not_installed("RJDBC")
  skip_if_not_installed("rJava")
  variables <- c("LUPA_JDBC_DRIVER", "LUPA_JDBC_CLASSPATH", "LUPA_JDBC_URL")
  testthat::skip_if(
    any(!nzchar(Sys.getenv(variables))),
    "se necesitan las variables JDBC de la prueba real"
  )
  driver <- RJDBC::JDBC(
    Sys.getenv("LUPA_JDBC_DRIVER"), Sys.getenv("LUPA_JDBC_CLASSPATH")
  )
  con <- DBI::dbConnect(
    driver, Sys.getenv("LUPA_JDBC_URL"),
    Sys.getenv("LUPA_JDBC_USER", unset = ""),
    Sys.getenv("LUPA_JDBC_PASSWORD", unset = "")
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_match(.senas_conexion_dbi(con), "sql server")
})

.arrm_conexion_mysql <- function(maria = FALSE) {
  paquete <- if (maria) "RMariaDB" else "RMySQL"
  skip_if_not_installed(paquete)
  variable <- if (maria) "LUPA_MARIADB_PWD" else "LUPA_MYSQL_PWD"
  clave <- Sys.getenv(variable, unset = "")
  skip_if(!nzchar(clave), paste0("Sin ", variable, "."))
  conexion <- tryCatch({
    if (maria) {
      DBI::dbConnect(
        RMariaDB::MariaDB(), host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_DB_PORT", if (maria) "33306" else "33307")),
        dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
        username = Sys.getenv("LUPA_DB_USER", "root"), password = clave
      )
    } else {
      DBI::dbConnect(
        RMySQL::MySQL(), host = Sys.getenv("LUPA_DB_HOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("LUPA_DB_PORT", if (maria) "33306" else "33307")),
        dbname = Sys.getenv("LUPA_DB_NAME", "lupa"),
        username = Sys.getenv("LUPA_DB_USER", "root"), password = clave
      )
    }
  }, error = function(e) NULL)
  skip_if(is.null(conexion), paste0(paquete, " no disponible."))
  conexion
}

test_that("una tabla normal no emite avisos en MySQL", {
  skip_on_cran()
  conexion <- .arrm_conexion_mysql()
  tabla <- "arrm_aviso_mysql"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

test_that("una tabla normal no emite avisos en MariaDB", {
  skip_on_cran()
  conexion <- .arrm_conexion_mysql(maria = TRUE)
  tabla <- "arrm_aviso_maria"
  on.exit({
    try(DBI::dbRemoveTable(conexion, tabla), silent = TRUE)
    DBI::dbDisconnect(conexion)
  }, add = TRUE)
  .arrm_tabla_normal(conexion, tabla)
  .arrm_probar_perfil_sin_aviso(conexion, tabla)
})

.arrm_con_mysql <- function(maria = FALSE) {
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

.arrm_prueba_tipos_mysql <- function(maria = FALSE) {
  con <- .arrm_con_mysql(maria)
  nombre <- if (maria) "arrm_maria_tipos" else "arrm_mysql_tipos"
  on.exit({
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", nombre)), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", nombre, " (id INT, txt TEXT, txt_long LONGTEXT, ",
    "bin_fixed BINARY(4), bin_var VARBINARY(16), bin_blob BLOB, ",
    "bin_long LONGBLOB)"
  ))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", nombre,
    " VALUES (1, 'texto corto', REPEAT('l', 100), UNHEX('DEADBEEF'),",
    " UNHEX('DEADBEEF00'), UNHEX('DEADBEEF00'), UNHEX('DEADBEEF00')),",
    " (2, 'otro texto', REPEAT('m', 100), UNHEX('01020304'),",
    " UNHEX('0102030405'), UNHEX('0102030405'), UNHEX('0102030405')),",
    " (3, NULL, NULL, NULL, NULL, NULL, NULL)"
  ))
  resultado <- suppressWarnings(perfilar_dbi(
    con, nombre, universo = "muestra_motor", muestra_motor = 3L,
    muestra = 3L, metricas = "validos", proteger_datos_personales = FALSE
  ))
  binarias <- resultado$perfil_muestra$meta$columnas_binarias
  expect_true(all(c("bin_fixed", "bin_var", "bin_blob", "bin_long") %in% binarias))
  expect_false(any(c("txt", "txt_long") %in% binarias))
  columnas <- resultado$perfil_muestra$columnas
  expect_identical(
    columnas$tipo_declarado[columnas$columna == "txt"], "texto"
  )
  expect_true(is.finite(
    columnas$longitud_media[columnas$columna == "bin_var"]
  ))
}

test_that("MySQL usa data_type del catalogo para texto y binarios", {
  testthat::skip_on_cran()
  .arrm_prueba_tipos_mysql(FALSE)
})

test_that("MariaDB usa data_type del catalogo para texto y binarios", {
  testthat::skip_on_cran()
  .arrm_prueba_tipos_mysql(TRUE)
})

test_that("ambos controladores publican cifras sin avisos y recuperan extremos", {
  skip_on_cran()

  .amy_probar <- function(maria = FALSE) {
    conexion <- .arrm_conexion_mysql(maria = maria)
    sufijo <- if (maria) "maria" else "mysql"
    tabla <- paste0("amy_perfilar_comun_", sufijo)
    tabla_extrema <- paste0("amy_perfilar_extrema_", sufijo)
    tablas <- c(tabla, tabla_extrema)
    on.exit({
      for (nombre in tablas) {
        try(DBI::dbRemoveTable(conexion, nombre), silent = TRUE)
      }
      DBI::dbDisconnect(conexion)
    }, add = TRUE)

    .arrm_tabla_normal(conexion, tabla)
    resultado <- .arrm_perfil_sin_aviso(
      conexion, tabla, universo = "tabla_completa", bloque = "solo_agregados"
    )
    columnas <- resultado$resumen_tabla$columnas
    datos <- data.frame(
      id = seq_len(500L), x = seq_len(500L) / 2,
      stringsAsFactors = FALSE
    )
    for (nombre in c("id", "x")) {
      fila <- columnas[columnas$columna == nombre, , drop = FALSE]
      valores <- datos[[nombre]]
      expect_equal(as.numeric(fila$n_validos), length(valores))
      expect_equal(as.numeric(fila$n_faltantes), 0)
      expect_equal(as.numeric(fila$n_distintos), length(unique(valores)))
      expect_equal(as.numeric(fila$minimo), min(valores))
      expect_equal(as.numeric(fila$maximo), max(valores))
      expect_equal(as.numeric(fila$media), mean(valores), tolerance = 1e-12)
      expect_equal(as.numeric(fila$mediana), median(valores), tolerance = 1e-12)
      expect_equal(as.numeric(fila$desvio), .adv_sd_referencia_mysql(valores),
                   tolerance = 1e-12)
    }

    tabla_sql <- as.character(DBI::dbQuoteIdentifier(conexion, tabla_extrema))
    maximo <- format(.Machine$double.xmax, scientific = TRUE, digits = 17)
    DBI::dbExecute(conexion, paste0(
      "CREATE TABLE ", tabla_sql, " (f8 DOUBLE)"
    ))
    DBI::dbExecute(conexion, paste0(
      "INSERT INTO ", tabla_sql, " (f8) VALUES (", maximo, "), (-",
      maximo, "), (0), (1)"
    ))
    extremo <- .arrm_perfil_sin_aviso(
      conexion, tabla_extrema,
      universo = "tabla_completa", bloque = "solo_agregados"
    )
    fila_extrema <- extremo$resumen_tabla$columnas[
      extremo$resumen_tabla$columnas$columna == "f8", , drop = FALSE
    ]
    expect_equal(
      as.numeric(fila_extrema$desvio), 1.4678102981723262e+308,
      tolerance = 1e-15
    )
  }

  .amy_probar(FALSE)
  .amy_probar(TRUE)
})

test_that("un DECIMAL de 2^53 o mas se declara por precision y no como no pedido", {
  skip_on_cran()
  .adec_decimal <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    tabla <- if (maria) "adec_test_dec_maria" else "adec_test_dec_mysql"
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (id INT, dec38 DECIMAL(38,10), d20 DECIMAL(20,0))"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ",
      "(1,9999999999999999999999999999.1234567890,12345678901234567890),",
      "(2,9999999999999999999999999999.1234567890,12345678901234567891),",
      "(3,NULL,NULL),(4,NULL,NULL)"
    ))
    for (metricas in list(NULL, c("validos", "distintos", "moda", "basicos", "desvio"))) {
      argumentos <- list(
        con, tabla, universo = "tabla_completa",
        bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
      )
      if (!is.null(metricas)) argumentos$metricas <- metricas
      resultado <- suppressWarnings(suppressMessages(
        do.call(perfilar_dbi, argumentos)
      ))
      columnas <- resultado$resumen_tabla$columnas
      sql <- resultado$resumen_tabla$sql
      for (columna in c("dec38", "d20")) {
        fila <- columnas[columnas$columna == columna, , drop = FALSE]
        expect_equal(as.numeric(fila$n_validos), 2)
        # Un doble no distingue esos valores: lo que se publicaria seria otro
        # numero (12345678901234567168 en lugar de 12345678901234567890).
        expect_true(all(is.na(c(fila$minimo, fila$maximo, fila$media,
                                fila$desvio))))
        magnitud <- sql[sql$columna == columna &
                          sql$metrica %in% c("minimo", "maximo", "media",
                                             "desvio"), , drop = FALSE]
        expect_true(nrow(magnitud) >= 4L)
        expect_true(all(magnitud$estado == "no_disponible"))
        expect_true(all(grepl("2^53", magnitud$motivo, fixed = TRUE)))
        expect_false(any(grepl("no se pidio", magnitud$motivo, fixed = TRUE)))
      }
      expect_equal(as.numeric(columnas$n_distintos[columnas$columna == "dec38"]), 1)
      expect_equal(as.numeric(columnas$n_distintos[columnas$columna == "d20"]), 2)
    }
  }
  .adec_decimal(FALSE)
  .adec_decimal(TRUE)
})

test_that("el repliegue escalado de MySQL y MariaDB centra constantes", {
  skip_on_cran()
  .aat_extremo <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    tabla <- if (maria) "aat_test_ext_maria" else "aat_test_ext_mysql"
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (id INT, xmax DOUBLE, casi DOUBLE)"
    ))
    x <- .Machine$double.xmax
    casi <- c(x, x * (1 - 2^-52), x)
    valores <- paste(vapply(seq_len(30L), function(i) {
      casi_i <- if (i <= 3L) sprintf("%.17g", casi[[i]]) else "NULL"
      paste0("(", i, ",", sprintf("%.17g", x), ",", casi_i, ")")
    }, character(1L)), collapse = ",")
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ", valores))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa",
      bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
    )))
    columnas <- resultado$resumen_tabla$columnas
    expect_equal(columnas$desvio[columnas$columna == "xmax"], 0)
    esperado <- .adv_sd_referencia_mysql(casi)
    expect_equal(
      columnas$desvio[columnas$columna == "casi"], esperado,
      tolerance = 1e-12
    )
  }
  .aat_extremo(FALSE)
  .aat_extremo(TRUE)
})

test_that("la moda binaria usa la representacion de largo y hash", {
  skip_on_cran()
  .aat_binaria <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    tabla <- if (maria) "aat_test_bin_maria" else "aat_test_bin_mysql"
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla, " (id INT, bin LONGBLOB)"
    ))
    bytes <- c(rep("01", 40L), rep("02", 30L), rep("03", 20L))
    valores <- paste(vapply(seq_along(bytes), function(i) paste0(
      "(", i, ",UNHEX(REPEAT('", bytes[[i]], "',400)))"
    ), character(1L)), collapse = ",")
    DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES ", valores))
    completa <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa",
      proteger_datos_personales = FALSE
    )))
    muestra <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "muestra_motor", muestra_motor = 1000L,
      muestra = 1000L, proteger_datos_personales = FALSE
    )))
    moda_completa <- completa$resumen_tabla$columnas$moda[
      completa$resumen_tabla$columnas$columna == "bin"
    ]
    moda_muestra <- muestra$resumen_tabla$columnas$moda[
      muestra$resumen_tabla$columnas$columna == "bin"
    ]
    expect_identical(as.character(moda_completa), as.character(moda_muestra))
    expect_match(as.character(moda_completa), "^400:")
    expect_false(any(completa$resumen_tabla$cobertura$estado ==
                     "no_corroborada"))
  }
  .aat_binaria(FALSE)
  .aat_binaria(TRUE)
})

test_that("MySQL y MariaDB escalan media y desvio en los extremos", {
  skip_on_cran()
  .adv_probar <- function(maria = FALSE) {
    conexion <- .arrm_conexion_mysql(maria = maria)
    sufijo <- if (maria) "maria" else "mysql"
    set.seed(20261104)
    casos <- list(
      c37_602 = rep(6.02214076e306, 37L),
      c33_123 = rep(1.2345678901234567e307, 33L),
      c7_neg = rep(-9.87654321e307, 7L),
      alea200 = runif(200L, 1e307, 1.7e308),
      neg200 = -runif(200L, 1e307, 1.7e308),
      mezcla_signo = c(runif(50L, 1e307, 1.7e308),
                       -runif(50L, 1e307, 1.7e308)),
      cancel307 = 1e307 * (1 + runif(60L) * 1e-9),
      casi_xm = c(.Machine$double.xmax,
                  .Machine$double.xmax * (1 - 2^-52),
                  .Machine$double.xmax),
      e100 = 1e100 + runif(100L) * 1e88,
      borde149 = 1e149 + runif(40L) * 1e138
    )
    tablas <- paste0("a59_extremos_", sufijo, "_", names(casos))
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(conexion)
    }, add = TRUE)
    for (i in seq_along(casos)) {
      tabla <- tablas[[i]]
      valores <- casos[[i]]
      DBI::dbExecute(conexion, paste0(
        "CREATE TABLE ", tabla, " (id INT, x DOUBLE)"
      ))
      literal <- sprintf("%.17g", valores)
      DBI::dbExecute(conexion, paste0(
        "INSERT INTO ", tabla, " VALUES ", paste0(
          "(", seq_along(valores), ",", literal, ")", collapse = ","
        )
      ))
      guardado <- DBI::dbGetQuery(conexion, paste0("SELECT x FROM ", tabla))
      avisos <- character()
      resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
        conexion, tabla, universo = "tabla_completa",
        bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
      )), warning = function(w) {
        avisos <<- c(avisos, class(w)[[1L]])
        invokeRestart("muffleWarning")
      })
      fila <- resultado$resumen_tabla$columnas[
        resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
      ]
      esperado_media <- .adv_media_referencia_mysql(guardado$x)
      esperado_desvio <- .adv_sd_referencia_mysql(guardado$x)
      ordenados <- sort(guardado$x)
      esperado_mediana <- if (length(ordenados) %% 2L) {
        ordenados[(length(ordenados) + 1L) / 2L]
      } else {
        ordenados[length(ordenados) / 2L] / 2 +
          ordenados[length(ordenados) / 2L + 1L] / 2
      }
      expect_equal(length(avisos), 0L, info = tabla)
      expect_equal(as.numeric(fila$media), esperado_media,
                   tolerance = 1e-12,
                   info = tabla)
      expect_equal(as.numeric(fila$mediana), esperado_mediana,
                   tolerance = 1e-12,
                   info = tabla)
      expect_equal(as.numeric(fila$desvio), esperado_desvio,
                   tolerance = 1e-12,
                   info = tabla)
      DBI::dbExecute(conexion, paste0("DROP TABLE IF EXISTS ", tabla))
    }
  }
  .adv_probar(FALSE)
  .adv_probar(TRUE)
})

test_that("la mediana de un DECIMAL se promedia exacta y llega como doble", {
  skip_on_cran()
  .adec_mediana <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    tabla <- if (maria) "adec_test_med_maria" else "adec_test_med_mysql"
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (id INT, d DECIMAL(10,2))"))
    # Los dos centrales son -6346.22 y -6346.18: su promedio decimal es
    # -6346.20, y en doble -6346.2000000000007.
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES (1,-9000.00),(2,-6346.22),(3,-6346.18),",
      "(4,15.50)"
    ))
    avisos <- character()
    resultado <- withCallingHandlers(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa",
      bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
    )), warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    columnas <- resultado$resumen_tabla$columnas
    expect_identical(as.numeric(columnas$mediana[columnas$columna == "d"]),
                     -6346.2)
    expect_length(avisos, 0L)
  }
  .adec_mediana(FALSE)
  .adec_mediana(TRUE)
})

test_that("MySQL y MariaDB resuelven bloques extremos y modas exactas", {
  skip_on_cran()
  .abq_probar <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    sufijo <- if (maria) "maria" else "mysql"
    tabla <- paste0("abq_bloques_", sufijo)
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla,
      " (id INT, x DOUBLE, d DECIMAL(38,0), b LONGBLOB)"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " VALUES (1,1e200,'12345678901234567890',UNHEX('')),",
      "(2,2e200,'12345678901234567890',UNHEX('')),",
      "(3,3e200,'7',UNHEX('00'))"
    ))
    por_bloques <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, bloque_filas = 1L,
      metricas = c("basicos", "desvio", "moda"),
      incluir_valores = TRUE, proteger_datos_personales = FALSE
    )))
    columnas <- por_bloques$resumen_tabla$columnas
    fila_x <- columnas[columnas$columna == "x", , drop = FALSE]
    expect_identical(fila_x$media, 2e200)
    expect_identical(fila_x$desvio, 1e200)
    fila_d <- columnas[columnas$columna == "d", , drop = FALSE]
    expect_identical(fila_d$moda, "12345678901234567890")
    expect_equal(fila_d$frecuencia_moda, 2)
    fila_b <- columnas[columnas$columna == "b", , drop = FALSE]
    expect_identical(fila_b$moda, "0:d41d8cd98f00b204e9800998ecf8427e")
    expect_equal(fila_b$frecuencia_moda, 2)
    estados <- por_bloques$resumen_tabla$sql
    estados <- estados[estados$columna == "x" &
                         estados$metrica %in% c("media", "desvio"), ,
                       drop = FALSE]
    expect_true(all(estados$estado == "calculado"))
  }
  .abq_probar(FALSE)
  .abq_probar(TRUE)
})

test_that("MySQL y MariaDB guardan el motivo de precision decimal", {
  skip_on_cran()
  .abq_probar <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    sufijo <- if (maria) "maria" else "mysql"
    tabla <- paste0("abq_guardas_", sufijo)
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tabla,
      " (ancho DECIMAL(38,0), borde DECIMAL(22,2))"
    ))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla,
      " VALUES ('9007199254740992','9007199254740991.99'),",
      "('9007199254740993','1.00')"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, metricas = c("basicos", "desvio"),
      incluir_valores = FALSE, proteger_datos_personales = FALSE
    )))
    sql <- resultado$resumen_tabla$sql
    for (campo in c("ancho", "borde")) {
      filas <- sql[sql$columna == campo &
                     sql$metrica %in% c("media", "desvio"), , drop = FALSE]
      expect_true(all(filas$estado == "no_disponible"), info = campo)
      expect_true(all(grepl(
        "decimal entregado como doble", filas$motivo, fixed = TRUE
      )), info = campo)
      expect_false(any(grepl(
        "incluir_valores", filas$motivo, fixed = TRUE
      )), info = campo)
    }
  }
  .abq_probar(FALSE)
  .abq_probar(TRUE)
})

test_that("MySQL y MariaDB centran el desvio de la muestra", {
  skip_on_cran()
  exactos <- c(
    e6_m8 = 2.8324886649005893e-9,
    epoch_hora = 964.2306713548253,
    e6_u = 0.27123367509526173
  )
  .acn_probar <- function(maria = FALSE) {
    con <- .arrm_conexion_mysql(maria = maria)
    motor <- if (maria) "maria" else "mysql"
    casos <- local({
      set.seed(7)
      list(
        e6_m8 = 1e6 + runif(100L) * 1e-8,
        epoch_hora = 1.7e9 + runif(100L) * 3600,
        e6_u = 1e6 + runif(100L)
      )
    })
    tablas <- c(
      paste0("acn_muestra_", motor), paste0("acn_completa_", motor)
    )
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    for (nombre in names(casos)) {
      tabla <- paste0("acn_muestra_", motor, "_", nombre)
      tablas <- c(tablas, tabla)
      DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                                 " (id INT, x DOUBLE)"))
      valores <- paste0(
        "(", seq_along(casos[[nombre]]), ",",
        sprintf("%.17g", casos[[nombre]]), ")"
      )
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla, " VALUES ", paste(valores, collapse = ",")
      ))
      resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, universo = "muestra_motor", muestra_motor = 100000L,
        bloque_muestra = "solo_agregados",
        metricas = c("basicos", "desvio"),
        proteger_datos_personales = FALSE
      )))
      fila <- resultado$resumen_tabla$columnas[
        resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
      ]
      expect_equal(as.numeric(fila$n), 100, info = paste(motor, nombre))
      expect_true(
        abs(as.numeric(fila$desvio) - exactos[[nombre]]) <= 1e-12,
        info = paste(motor, nombre)
      )
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
    }
    tabla <- paste0("acn_completa_", motor)
    valores <- paste0(
      "(", seq_along(casos$e6_m8), ",",
      sprintf("%.17g", casos$e6_m8), ")"
    )
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                               " (id INT, x DOUBLE)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES ", paste(valores, collapse = ",")
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa",
      bloque_muestra = "solo_agregados", metricas = "desvio",
      proteger_datos_personales = FALSE
    )))
    fila <- resultado$resumen_tabla$columnas[
      resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
    ]
    expect_true(
      abs(as.numeric(fila$desvio) - exactos[["e6_m8"]]) <= 1e-12,
      info = paste(motor, "tabla_completa")
    )
  }
  .acn_probar(FALSE)
  .acn_probar(TRUE)
})

test_that("MySQL y MariaDB escalan sin basicos y sin valores", {
  skip_on_cran()
  es_cifra <- function(observado, esperado) {
    is.finite(observado) && observado != 0 && is.finite(esperado) &&
      esperado != 0 && abs(observado / esperado - 1) <= 1e-12
  }
  set.seed(20261104)
  tiny <- 1e-160 * (1 + runif(30L))
  invisible(runif(80L))
  invisible(runif(40L))
  invisible(runif(40L))
  invisible(runif(30L))
  invisible(runif(40L))
  invisible(runif(30L))
  alea <- runif(200L, 1e307, 1.7e308)
  casos <- list(
    tiny = list(x = tiny, desvio = 3.212636809309274e-161),
    extremos = list(
      x = c(1e300, 0, 0, 0), media = 2.5e299, desvio = 5e299
    ),
    alea = list(
      x = alea, media = 8.621965374052524e307,
      desvio = 4.508407076339053e307
    )
  )
  probar <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    motor <- if (maria) "maria" else "mysql"
    tablas <- paste0("aes_", motor, "_escala_", names(casos))
    on.exit({
      for (tabla in tablas) {
        try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
            silent = TRUE)
      }
      DBI::dbDisconnect(con)
    }, add = TRUE)
    for (nombre in names(casos)) {
      tabla <- paste0("aes_", motor, "_escala_", nombre)
      DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                                 " (id INT, x DOUBLE)"))
      valores <- vapply(casos[[nombre]]$x, function(x) {
        if (is.na(x)) "NULL" else sprintf("%.17g", x)
      }, character(1L))
      DBI::dbExecute(con, paste0(
        "INSERT INTO ", tabla, " VALUES ",
        paste0("(", seq_along(valores), ",", valores, ")", collapse = ",")
      ))
      metricas <- if (nombre == "tiny") "desvio" else c("basicos", "desvio")
      resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
        con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
        metricas = metricas, incluir_valores = FALSE,
        proteger_datos_personales = FALSE
      )))
      fila <- resultado$resumen_tabla$columnas[
        resultado$resumen_tabla$columnas$columna == "x", , drop = FALSE
      ]
      expect_true(es_cifra(as.numeric(fila$desvio), casos[[nombre]]$desvio),
                  info = paste(motor, nombre, "desvio"))
      if (nombre != "tiny") {
        expect_true(es_cifra(as.numeric(fila$media), casos[[nombre]]$media),
                    info = paste(motor, nombre, "media"))
      }
      expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla))
    }
  }
  probar(FALSE)
  probar(TRUE)
})

test_that("MySQL y MariaDB no repliegan un desvio con un solo valido", {
  skip_on_cran()
  probar <- function(maria = FALSE) {
    con <- .arrm_con_mysql(maria)
    motor <- if (maria) "maria" else "mysql"
    tabla <- paste0("aes_", motor, "_un_valido")
    on.exit({
      try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tabla)),
          silent = TRUE)
      DBI::dbDisconnect(con)
    }, add = TRUE)
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla,
                               " (id INT, x DOUBLE)"))
    DBI::dbExecute(con, paste0(
      "INSERT INTO ", tabla, " VALUES (1,1e300),(2,NULL),(3,NULL)"
    ))
    resultado <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, universo = "tabla_completa", bloque_muestra = "solo_agregados",
      metricas = "desvio", proteger_datos_personales = FALSE
    )))
    registro <- resultado$resumen_tabla$sql[
      resultado$resumen_tabla$sql$columna == "x" &
        resultado$resumen_tabla$sql$metrica == "desvio", , drop = FALSE
    ]
    expect_identical(as.character(registro$estado[[1L]]), "no_aplica")
    expect_match(as.character(registro$motivo[[1L]]), "al menos dos")
    expect_equal(nrow(resultado$resumen_tabla$meta$consultas$fallas), 0L)
  }
  probar(FALSE)
  probar(TRUE)
})

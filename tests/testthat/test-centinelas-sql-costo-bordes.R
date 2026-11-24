testthat::skip_on_cran()

.csb_perfil <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, muestra = 5L, metricas = "basicos", ...,
    avisar_costo_distintos = FALSE, avisar_costo_moda = FALSE,
    avisar_costo_mediana = FALSE
  )))
}

testthat::test_that("sin muestra no emite faltantes disfrazados", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "sin_muestra", data.frame(
    a = 1:20, b = rep("ok", 20L), stringsAsFactors = FALSE
  ))
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "sin_muestra", metricas = "basicos",
    avisar_costo_distintos = FALSE, avisar_costo_moda = FALSE,
    avisar_costo_mediana = FALSE
  )))
  testthat::expect_false(
    "n_faltantes_disfrazados" %in% names(perfil$resumen_tabla$columnas)
  )
  testthat::expect_false(any(
    perfil$resumen_tabla$sql$metrica == "n_faltantes_disfrazados"
  ))
})

testthat::test_that("los candidatos viajan sin subconsultas de guarda", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "sin_candidatos", data.frame(
    a = 1:20, b = seq(0.1, 2, length.out = 20L),
    texto = rep("ok", 20L), stringsAsFactors = FALSE
  ))
  perfil <- .csb_perfil(con, "sin_candidatos")
  fd <- perfil$resumen_tabla$sql[
    perfil$resumen_tabla$sql$metrica == "n_faltantes_disfrazados", ,
    drop = FALSE
  ]
  testthat::expect_true(nrow(fd) > 0L)
  testthat::expect_false(any(grepl("GROUP BY", fd$sql, fixed = TRUE)))
  testthat::expect_false(any(
    perfil$resumen_tabla$sql$etapa == "faltantes_guardas"
  ))
})

testthat::test_that(
  "la consolidada normaliza una columna textual pocas veces", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "normaliza_una", data.frame(
      id = 1:20,
      texto = rep(c("ok", "N/D", "sd", "1"), 5L),
      stringsAsFactors = FALSE
    ))
    perfil <- .csb_perfil(con, "normaliza_una")
    filas <- perfil$resumen_tabla$sql[
      perfil$resumen_tabla$sql$metrica == "n_faltantes_disfrazados", ,
      drop = FALSE
    ]
    sql <- unique(as.character(filas$sql[filas$columna == "texto"]))
    testthat::expect_length(sql, 1L)
    apariciones <- gregexpr("LOWER(", sql[[1L]], fixed = TRUE)[[1L]]
    apariciones <- if (identical(apariciones, -1L)) 0L else length(apariciones)
    testthat::expect_lte(apariciones, 3L)
    testthat::expect_match(sql[[1L]], "texto", fixed = TRUE)
  }
)

testthat::test_that(
  "la guarda exacta de PostgreSQL compara la columna double original", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    testthat::local_mocked_bindings(
      .senas_conexion_dbi = function(conexion) "PqConnection rpostgres",
      .package = "lupa"
    )
    policy <- list(
      exacta = TRUE, parcial = FALSE, clase = "numerica",
      candidatos = c(-99, -9.9), numericos = c(-99, -9.9),
      declarados = FALSE, tipo = "float8", motivo = NA_character_
    )
    aliases <- lupa:::.aliases_faltantes_dbi(
      policy, identity, 1L, 1L, con
    )
    sql <- paste(lupa:::.expresiones_faltantes_dbi(
      con, policy, "\"leida\"", aliases, "t",
      columna_original = "\"original\"",
      comparar_double_original = TRUE
    ), collapse = ", ")
    testthat::expect_match(sql, '"original" IN (', fixed = TRUE)
    testthat::expect_match(sql, "-9.9000000000000004", fixed = TRUE)
    testthat::expect_false(grepl(
      'CAST("leida" AS DOUBLE PRECISION)', sql, fixed = TRUE,
      useBytes = TRUE
    ))
  }
)

testthat::test_that("el texto numerico respeta las formas de memoria", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "formas", data.frame(
    coma_final = c("1", "-99,", "2"),
    exponente_coma = c("1", "9,99e2", "2"),
    signo_doble = c("1", "+-99", "2"),
    stringsAsFactors = FALSE
  ))
  perfil <- .csb_perfil(con, "formas")
  valores <- perfil$resumen_tabla$columnas$n_faltantes_disfrazados
  testthat::expect_equal(valores, c(0, 0, 0))
})

testthat::test_that(
  "las formas numericas de texto coinciden con la lectura de memoria", {
    testthat::skip_if_not_installed("DBI")
    formas <- c(
      "-99", "-99.0", "-99.", "-99,", "-99,00", " -99 ", "+-99",
      "9,99e2", "9.99e2", "-99e0", "-99E0", ".99", "-.99", "99."
    )
    datos <- data.frame(id = 1:40)
    for (i in seq_along(formas)) {
      datos[[paste0("v", i)]] <- c(
        rep("1", 10L), rep(formas[[i]], 6L),
        rep(c("2", "3", "4"), length.out = 24L)
      )
    }
    esperado <- perfilar(
      datos, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )$columnas$n_faltantes_disfrazados[-1L]
    motores <- c("RSQLite", "duckdb")
    medidos <- 0L
    for (motor in motores) {
      if (!requireNamespace(motor, quietly = TRUE)) next
      con <- if (identical(motor, "RSQLite")) {
        DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      } else {
        DBI::dbConnect(duckdb::duckdb())
      }
      tryCatch({
        DBI::dbWriteTable(con, "formas_completas", datos)
        perfil <- .csb_perfil(con, "formas_completas")
        obtenido <- perfil$resumen_tabla$columnas$n_faltantes_disfrazados[-1L]
        testthat::expect_equal(obtenido, esperado)
        medidos <- medidos + 1L
      }, finally = DBI::dbDisconnect(con))
    }
    testthat::expect_gt(medidos, 0L)
  }
)

testthat::test_that("SQLite declara texto no ASCII y convenciones ambiguas", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  u <- intToUtf8
  DBI::dbWriteTable(con, "bordes_texto", data.frame(
    acento = c("ok", paste0("SIN INFORMACI", u(211), "N"), "ok"),
    miles = c("1.234,56", "-9.999", "5"),
    stringsAsFactors = FALSE
  ))
  perfil <- .csb_perfil(con, "bordes_texto")
  columnas <- perfil$resumen_tabla$columnas
  testthat::expect_true(all(is.na(columnas$n_faltantes_disfrazados)))
  motivos <- perfil$resumen_tabla$cobertura$motivo
  testthat::expect_true(any(grepl("no ASCII", motivos, fixed = TRUE)))
  testthat::expect_true(any(grepl("convencion", motivos, fixed = TRUE)))
})

testthat::test_that("SQLite no publica cero ante un plegado no reproducible", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  u <- intToUtf8
  base <- c("sd", paste0("v", 1:7))
  datos <- data.frame(
    a = c(
      rep("v1", 10L), rep("sd", 6L),
      rep_len(c(base, paste0("v", u(233)), paste0("v", u(201))), 44L)
    ),
    stringsAsFactors = FALSE
  )
  esperado <- perfilar(
    datos, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )$columnas$n_faltantes_disfrazados
  testthat::expect_equal(esperado, 11)
  DBI::dbWriteTable(con, "acentos_ambiguos", datos)
  perfil <- .csb_perfil(con, "acentos_ambiguos")
  obtenido <- perfil$resumen_tabla$columnas$n_faltantes_disfrazados
  testthat::expect_true(is.na(obtenido))
})

# El `REAL` de DuckDB es su tipo de 4 bytes: guarda el numero de 4 bytes mas
# cercano a -99.9, y el centinela declarado se compara en 4 bytes, como promete
# la ayuda para una columna `real`. Antes se comparaba pasando la columna a
# doble, la cuenta daba 0 y esta prueba lo afirmaba.
testthat::test_that("DECIMAL se compara en doble y el REAL de DuckDB en 4 bytes", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE decimal_sql (x DECIMAL(12,4))")
  DBI::dbExecute(con, "INSERT INTO decimal_sql VALUES (0.1000), (1), (2)")
  perfil <- .csb_perfil(
    con, "decimal_sql", sentinelas_numericos = 0.1
  )
  testthat::expect_equal(
    perfil$resumen_tabla$columnas$n_faltantes_disfrazados, 1
  )

  testthat::skip_if_not_installed("duckdb")
  con2 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  DBI::dbExecute(con2, "CREATE TABLE real_sql (x REAL)")
  DBI::dbExecute(con2, "INSERT INTO real_sql VALUES (-99.9), (1), (2)")
  perfil2 <- .csb_perfil(
    con2, "real_sql", sentinelas_numericos = -99.9
  )
  testthat::expect_equal(
    perfil2$resumen_tabla$columnas$n_faltantes_disfrazados, 1
  )
})

testthat::test_that("la lectura exacta conserva infinitos de PostgreSQL", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  testthat::local_mocked_bindings(
    .senas_conexion_dbi = function(conexion) "PqConnection rpostgres",
    .package = "lupa"
  )
  policy <- list(
    exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = Inf,
    numericos = Inf, declarados = FALSE, tipo = "float8",
    motivo = NA_character_
  )
  aliases <- lupa:::.aliases_faltantes_dbi(
    policy, identity, 1L, 1L, con
  )
  sql <- paste(lupa:::.expresiones_faltantes_dbi(
    con, policy, "float8send(\"x\")", aliases, "x"
  ), collapse = ", ")
  testthat::expect_match(sql, "CAST('1e309' AS NUMERIC)", fixed = TRUE)
  testthat::expect_false(grepl(
    "CAST('Infinity' AS DOUBLE PRECISION)", sql, fixed = TRUE
  ))
})

testthat::test_that("los literales diminutos conservan sus cifras", {
  testthat::expect_match(
    lupa:::.faltantes_dbi_literal_numerico(1e-300, senas = "mysql"),
    "[eE]"
  )
  testthat::expect_match(
    lupa:::.faltantes_dbi_literal_numerico(
      5e-324, senas = "postgres", exacto = TRUE
    ),
    "[eE]"
  )
  testthat::expect_match(
    lupa:::.faltantes_dbi_literal_numerico(1.7e308, senas = "mariadb"),
    "[eE]"
  )
})

testthat::test_that("tabla vacia publica motivo de cuenta no obtenida", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE vacia_sql (x DOUBLE, y VARCHAR(20))")
  perfil <- .csb_perfil(con, "vacia_sql")
  cobertura <- perfil$resumen_tabla$cobertura
  cobertura <- cobertura[grepl("^vacia_sql[.]", cobertura$elemento), ,
                         drop = FALSE]
  testthat::expect_true(nrow(cobertura) > 0L)
  testthat::expect_true(all(grepl(
    "tabla no contiene filas", cobertura$motivo, fixed = TRUE
  )))
  testthat::expect_equal(nrow(perfil$resumen_tabla$meta$consultas$fallas), 0L)
})

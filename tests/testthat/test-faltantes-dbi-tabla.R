# Regresiones de la cuenta de faltantes disfrazados de la tabla completa.

testthat::skip_on_cran()

.cft_fecha <- as.POSIXct("2026-11-19 12:00:00", tz = "UTC")

.cft_dbi <- function(con, tabla, ...) {
  suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, fecha = .cft_fecha, muestra = 5L, orden_muestra = "id",
    proteger_datos_personales = FALSE, analizar_dependencias = FALSE, ...
  )))
}

.cft_fila <- function(perfil, metrica) {
  perfil$resumen_tabla$sql[
    perfil$resumen_tabla$sql$metrica == metrica,
    c("columna", "estado", "alcance", "universo"), drop = FALSE
  ]
}

testthat::test_that(
  "la cuenta SQL cubre la tabla y no reemplaza el perfil de muestra", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    sin_informacion <- intToUtf8(c(115L, 105L, 110L, 32L, 105L, 110L,
                                   102L, 111L, 114L, 109L, 97L, 99L,
                                   105L, 243L, 110L))
    datos <- data.frame(
      id = 1:10,
      monto = c(1:5, -99, 7:10),
      estado = c(rep("ok", 7L), "N/D", sin_informacion, "ok"),
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(con, "t", datos)
    perfil <- .cft_dbi(con, "t", metricas = "basicos")
    columnas <- perfil$resumen_tabla$columnas
    expect_equal(columnas$n_faltantes_disfrazados[columnas$columna == "monto"], 1)
    expect_equal(columnas$n_faltantes_disfrazados[columnas$columna == "estado"], 2)
    expect_equal(perfil$perfil_muestra$columnas$n_faltantes_disfrazados[
      perfil$perfil_muestra$columnas$columna == "monto"], 0)
    expect_equal(perfil$perfil_muestra$columnas$n[
      perfil$perfil_muestra$columnas$columna == "monto"], 5)
    filas <- .cft_fila(perfil, "n_faltantes_disfrazados")
    expect_true(all(filas$estado == "calculado"))
    expect_true(all(filas$alcance == "tabla_completa"))
    expect_true(all(filas$universo == 10))
  }
)

testthat::test_that(
  "comparar_perfiles usa la cuenta SQL completa en E2", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    anterior <- data.frame(id = 1:100, estado = rep("ok", 100L),
                           stringsAsFactors = FALSE)
    actual <- anterior
    actual$estado[80:100] <- "N/D"
    DBI::dbWriteTable(con, "anterior", anterior)
    DBI::dbWriteTable(con, "actual", actual)
    a <- .cft_dbi(con, "anterior")
    b <- .cft_dbi(con, "actual")
    deriva <- as.data.frame(comparar_perfiles(a, b))
    fila <- deriva[deriva$aspecto == "faltantes" & deriva$columna == "estado", ,
                   drop = FALSE]
    expect_equal(nrow(fila), 1L)
    expect_equal(as.numeric(fila$valor_anterior), 0)
    expect_equal(as.numeric(fila$valor_actual), 0.21)
    expect_identical(as.character(fila$universo_anterior), "tabla_completa")
    expect_identical(as.character(fila$universo_actual), "tabla_completa")
  }
)

testthat::test_that(
  "las guardas numericas y el texto numerico conservan la regla de memoria", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    densa <- data.frame(id = seq_along(c(seq_len(1000L), -99)),
                        x = c(seq_len(1000L), -99),
                        stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "densa", densa)
    p <- .cft_dbi(con, "densa", metricas = "basicos")
    x <- p$resumen_tabla$columnas$n_faltantes_disfrazados[
      p$resumen_tabla$columnas$columna == "x"]
    expect_equal(x, 1)

    texto <- data.frame(id = 1:5, codigo = c("1", "2", "-99", "3", "4"),
                        stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "texto", texto)
    q <- .cft_dbi(con, "texto", metricas = "basicos")
    expect_equal(q$resumen_tabla$columnas$n_faltantes_disfrazados[
      q$resumen_tabla$columnas$columna == "codigo"], 1)
  }
)

testthat::test_that("Date no se cuenta como numerico en SQLite", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "fechas", data.frame(
    id = 1:5, fecha = as.Date(rep("2020-01-01", 5L))
  ))
  p <- .cft_dbi(con, "fechas", metricas = "basicos")
  valor <- p$resumen_tabla$columnas$n_faltantes_disfrazados[
    p$resumen_tabla$columnas$columna == "fecha"]
  estado <- p$resumen_tabla$sql$estado[
    p$resumen_tabla$sql$columna == "fecha" &
      p$resumen_tabla$sql$metrica == "n_faltantes_disfrazados"]
  expect_true(is.na(valor))
  expect_identical(as.character(estado), "no_disponible")
})

testthat::test_that("la conversion textual usa el tipo de cada dialecto", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  dialectos <- c(
    postgresql = "postgresql", mysql = "mysql", mariadb = "mariadb",
    sql_server = "microsoft sql server", sqlite = "sqlite", duckdb = "duckdb"
  )
  esperados <- c(
    postgresql = "DOUBLE PRECISION", mysql = "DOUBLE", mariadb = "DOUBLE",
    sql_server = "FLOAT", sqlite = "DOUBLE", duckdb = "DOUBLE"
  )
  fragmentos <- c(
    postgresql = " AS DOUBLE PRECISION)", mysql = " AS DOUBLE)",
    mariadb = " AS DOUBLE)", sql_server = "TRY_CONVERT(FLOAT,",
    sqlite = " AS DOUBLE)", duckdb = " AS DOUBLE)"
  )
  expresion_de <- function(senas) {
    testthat::local_mocked_bindings(
      .senas_conexion_dbi = function(conexion) senas,
      .package = "lupa"
    )
    lupa:::.expresion_numerica_texto_faltantes_dbi(con, "LOWER(x)")
  }
  for (nombre in names(dialectos)) {
    sql <- expresion_de(dialectos[[nombre]])
    tipo <- esperados[[nombre]]
    testthat::expect_match(sql, fragmentos[[nombre]], fixed = TRUE)
    testthat::expect_match(sql, tipo, fixed = TRUE)
    if (nombre %in% c("postgresql")) {
      testthat::expect_match(sql, " ~ '", fixed = TRUE)
    } else if (nombre %in% c("mysql", "mariadb")) {
      testthat::expect_match(sql, " REGEXP '", fixed = TRUE)
    } else if (nombre == "sqlite") {
      testthat::expect_match(sql, " NOT GLOB ", fixed = TRUE)
    }
    if (identical(nombre, "postgresql")) {
      testthat::expect_false(grepl(" AS DOUBLE)", sql, fixed = TRUE))
    }
  }
})

testthat::test_that("SQLite no convierte texto basura en cero", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "texto_cero", data.frame(
    id = 1:5, estado = c("0", "ok", "n/d", "1", "2"),
    stringsAsFactors = FALSE
  ))
  perfil <- .cft_dbi(
    con, "texto_cero", metricas = "basicos", sentinelas_numericos = 0,
    cadenas_ausencia = "n/d"
  )
  valor <- perfil$resumen_tabla$columnas$n_faltantes_disfrazados[
    perfil$resumen_tabla$columnas$columna == "estado"
  ]
  expect_equal(valor, 2)
})

testthat::test_that("una falla de la consulta no publica cero", {
  policy <- list(
    exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = -99,
    numericos = -99, declarados = FALSE, motivo = NA_character_,
    literales = "-99"
  )
  aliases <- lupa:::.aliases_faltantes_dbi(
    policy, function(nombre) nombre, 1L, 1L
  )
  consulta <- lupa:::.resultado_agregado_no_emitido_dbi(
    "basicos", "fallo de preparacion"
  )
  faltantes <- lupa:::.resultado_faltantes_lote_dbi(
    consulta, "SELECT fallo", aliases, list()
  )
  mapa <- lupa:::.faltantes_dbi_mapa(
    list(faltantes = list(monto = faltantes)), list(),
    list(monto = policy), "tabla_completa"
  )
  expect_false(mapa$monto$exacta)
  expect_true(is.na(mapa$monto$n))
  expect_match(mapa$monto$motivo, "fallo de preparacion", fixed = TRUE)
  cobertura <- lupa:::.cobertura_faltantes_dbi(mapa, "t")
  expect_identical(as.character(cobertura$estado), "degradado")
  expect_match(cobertura$motivo, "fallo de preparacion", fixed = TRUE)
})

testthat::test_that("una fila parcial de faltantes publica NA", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  policy <- list(
    exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = -99,
    numericos = -99, declarados = TRUE, motivo = NA_character_,
    literales = "-99"
  )
  aliases <- lupa:::.aliases_faltantes_dbi(
    policy, identity, 1L, 1L
  )
  datos <- data.frame(n_validos = 20, stringsAsFactors = FALSE)
  datos[[aliases$total]] <- 0
  datos[[aliases$candidatos[[1L]]]] <- 0
  datos[[aliases$validos]] <- 20
  for (nombre in c(
    "minimo", "maximo", "minimo_sin", "maximo_sin",
    "frecuencia_moda", "frecuencia_segunda", "n_frecuencias_moda"
  )) {
    datos[[aliases[[nombre]]]] <- 0
  }
  consulta_parcial <- list(
    ok = TRUE, datos = datos, motivo = NA_character_, consulta_id = 1L
  )
  testthat::local_mocked_bindings(
    .consultar_dbi = function(...) consulta_parcial,
    .package = "lupa"
  )
  resultado <- lupa:::.conteos_columna_dbi(
    con, "t", "monto", identity, TRUE, FALSE,
    faltantes = list(aliases = aliases, expresiones = character())
  )
  mapa <- lupa:::.faltantes_dbi_mapa(
    list(faltantes = list(monto = resultado$faltantes)), list(),
    list(monto = policy), "tabla_completa"
  )
  expect_false(resultado$faltantes$ok)
  expect_false(mapa$monto$exacta)
  expect_true(is.na(mapa$monto$n))
})

testthat::test_that(
  "PostgreSQL conserva la cuenta sobre la lectura exacta del double", {
    testthat::skip_if_not_installed("DBI")
    testthat::skip_if_not_installed("RSQLite")
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    testthat::local_mocked_bindings(
      .senas_conexion_dbi = function(conexion) "PqConnection rpostgres",
      .version_servidor_postgresql_dbi = function(conexion) 160015L,
      .extra_float_digits_postgresql_dbi = function(conexion,
                                                    presupuesto = NULL) 0L,
      .package = "lupa"
    )
    lectura <- lupa:::.lectura_convertida_dbi(
      con, "monto", "\"monto\"", "float8"
    )
    testthat::expect_match(lectura$campos_sql[[1L]], "float8send(", fixed = TRUE)
    policy <- list(
      exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = -99,
      numericos = -99, declarados = FALSE, motivo = NA_character_,
      literales = "-99"
    )
    aliases <- lupa:::.aliases_faltantes_dbi(
      policy, identity, 1L, 1L, con
    )
    fuente <- paste0(
      "(SELECT ", lectura$campos_sql[[1L]],
      " AS \"monto\" FROM \"t\") AS \"lupa_lectura\""
    )
    sql <- paste(
      lupa:::.expresiones_faltantes_dbi(
        con, policy, "\"monto\"", aliases, fuente
      ),
      collapse = ", "
    )
    testthat::expect_match(sql, "AS lupa_l1_c1_fd_total", fixed = TRUE)
    testthat::expect_match(sql, "float8send(\"monto\")", fixed = TRUE)
  }
)

testthat::test_that("un agregado faltante ausente no se publica como cero", {
  policy <- list(
    exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = -99,
    numericos = -99, declarados = FALSE, motivo = NA_character_,
    literales = "-99"
  )
  correcto <- list(ok = TRUE, valor = 0, motivo = NA_character_)
  base <- list(
    ok = TRUE, total = correcto, candidatos = list(), distintos = correcto,
    validos = correcto, minimo = correcto, maximo = correcto,
    minimo_sin = correcto, maximo_sin = correcto,
    frecuencia_moda = correcto, frecuencia_segunda = correcto,
    n_frecuencias_moda = correcto
  )
  mapa <- lupa:::.faltantes_dbi_mapa(
    list(faltantes = list(monto = base)), list(),
    list(monto = policy), "tabla_completa"
  )
  testthat::expect_false(mapa$monto$exacta)
  testthat::expect_true(is.na(mapa$monto$n))
  testthat::expect_match(
    mapa$monto$motivo, "no se publica un cero", fixed = TRUE
  )
})

testthat::test_that("una falla conserva el mensaje del motor", {
  policy <- list(
    exacta = TRUE, parcial = FALSE, clase = "numerica", candidatos = -99,
    numericos = -99, declarados = FALSE, motivo = NA_character_,
    literales = "-99"
  )
  aliases <- lupa:::.aliases_faltantes_dbi(
    policy, identity, 1L, 1L
  )
  resultado <- list(
    ok = FALSE, datos = NULL,
    motivo = "operator does not exist: numeric = double precision",
    sql = "SELECT ...", consulta_id = 7L
  )
  faltantes <- lupa:::.resultado_faltantes_desde_resultado_dbi(
    resultado, aliases
  )
  mapa <- lupa:::.faltantes_dbi_mapa(
    list(faltantes = list(monto = faltantes)), list(),
    list(monto = policy), "tabla_completa"
  )
  testthat::expect_true(is.na(mapa$monto$n))
  testthat::expect_match(
    mapa$monto$motivo, "operator does not exist", fixed = TRUE
  )
})

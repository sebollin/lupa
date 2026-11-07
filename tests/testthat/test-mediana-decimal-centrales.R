# La mediana de un decimal es la suma de sus una o dos centrales, en el tipo de
# la columna, dividida en R por cuantas son: el doble mas cercano de la exacta.
# En DuckDB, PostgreSQL desde la 9.4 y SQL Server desde la compatibilidad 110 la
# pedia la consolidada (`PERCENTILE_CONT`), que interpola en doble: las cuatro
# medianas de abajo salian a un ulp, el promedio de los dobles de las centrales.
# Ahora un decimal no entra en la consolidada, y el plan cuenta su consulta.
#
# Las esperadas son la mediana exacta -(a + b) / 2 en fracciones sobre el texto
# de cada valor- redondeada al doble mas cercano, calculada fuera de R y escrita
# en hexadecimal.
testthat::skip_on_cran()

.mdc_pares <- list(
  # 0,15. El promedio de los dobles de 0,10 y 0,20 es 0,15000000000000002.
  centavos = list(tipo = c(18L, 2L), valores = c("0.10", "0.20"),
                  esperada = 0x1.3333333333333p-3),
  largo = list(tipo = c(38L, 20L),
               valores = c("8253.90651511007901151702",
                           "3870.25973563673977909110"),
               esperada = 0x1.7ae1547b45843p+12),
  ancho = list(tipo = c(18L, 2L),
               valores = c("29624339732200.63", "14960762004135.89"),
               esperada = 0x1.44663935cc843p+44),
  treinta = list(tipo = c(38L, 30L),
                 valores = c("0.799050074742554255327429269677",
                             "0.891714778349750377599705592793"),
                 esperada = 0x1.b0d5f72541f83p-1)
)

.mdc_crear <- function(con, tabla, decimal = "DECIMAL", doble = "DOUBLE") {
  try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  columnas <- vapply(names(.mdc_pares), function(k) {
    t <- .mdc_pares[[k]]$tipo
    paste0(k, " ", decimal, "(", t[[1L]], ",", t[[2L]], ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE ", tabla, " (", paste(columnas, collapse = ", "),
    ", f ", doble, ")"
  ))
  filas <- vapply(1:2, function(i) {
    paste0("(", paste(vapply(.mdc_pares, function(p) p$valores[[i]],
                             character(1L)), collapse = ", "),
           ", ", c("1.5", "2.5")[[i]], ")")
  }, character(1L))
  DBI::dbExecute(con, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
}

.mdc_comprobar <- function(con, tabla, ...) {
  perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, tabla, proteger_datos_personales = FALSE, ...
  )))
  columnas <- perfil$resumen_tabla$columnas
  sql <- perfil$resumen_tabla$sql
  for (k in names(.mdc_pares)) {
    fila <- sql[sql$columna == k & sql$metrica == "mediana", , drop = FALSE]
    expect_true(fila$estado %in% c("calculado", "estimado"), info = k)
    expect_false(grepl("PERCENTILE_CONT", fila$sql, fixed = TRUE,
                       useBytes = TRUE), info = k)
    expect_identical(
      as.numeric(columnas$mediana[columnas$columna == k]),
      .mdc_pares[[k]]$esperada, info = k
    )
  }
  # El control: un doble sigue en la consolidada, con su valor.
  expect_identical(as.numeric(columnas$mediana[columnas$columna == "f"]), 2)
  invisible(perfil)
}

test_that("DuckDB: la mediana de un decimal es la exacta, completa y por la muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .mdc_crear(con, "mdc_t")
  perfil <- .mdc_comprobar(con, "mdc_t")
  sql <- perfil$resumen_tabla$sql
  expect_true(grepl(
    "PERCENTILE_CONT", sql$sql[sql$columna == "f" & sql$metrica == "mediana"],
    fixed = TRUE, useBytes = TRUE
  ))
  .mdc_comprobar(con, "mdc_t", universo = "muestra_motor",
                 muestra_motor = 100L, bloque_muestra = "solo_agregados")
  # El plan cuenta una consulta por decimal y un lote para el doble.
  plan <- suppressWarnings(suppressMessages(plan_perfilado_dbi(con, "mdc_t")))
  fila <- plan[grepl("^mediana", plan$clase_consulta), , drop = FALSE]
  expect_identical(as.numeric(fila$n_consultas), 5)
})

test_that("PostgreSQL: la mediana de un numeric es la exacta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  con <- tryCatch(DBI::dbConnect(
    RPostgres::Postgres(), host = Sys.getenv("LUPA_PG_HOST", "127.0.0.1"),
    port = as.integer(Sys.getenv("LUPA_PG_PORT", "55432")),
    user = Sys.getenv("LUPA_PG_USER", "postgres"),
    password = Sys.getenv("LUPA_PG_PASSWORD", "lupa"),
    dbname = Sys.getenv("LUPA_PG_DBNAME", "lupa")
  ), error = function(e) NULL)
  skip_if(is.null(con), "PostgreSQL no disponible.")
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE lupa_mdc_pg"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .mdc_crear(con, "lupa_mdc_pg", decimal = "NUMERIC",
             doble = "DOUBLE PRECISION")
  .mdc_comprobar(con, "lupa_mdc_pg")
  .mdc_comprobar(con, "lupa_mdc_pg", universo = "muestra_motor",
                 muestra_motor = 100L, bloque_muestra = "solo_agregados")
  # Centrales que distan mas de 2^53: `PERCENTILE_CONT` resta en doble y la
  # mediana quedaba `no_disponible`; la suma de un `numeric` es exacta. Un
  # `bigint` sigue negado (control).
  esperadas <- list("numeric(20,0)" = -0.5, "bigint" = NA_real_)
  for (tipo in names(esperadas)) {
    DBI::dbExecute(con, "DROP TABLE lupa_mdc_pg")
    DBI::dbExecute(con, paste0("CREATE TABLE lupa_mdc_pg (v ", tipo, ")"))
    DBI::dbExecute(con, paste(
      "INSERT INTO lupa_mdc_pg VALUES (-9007199254740991), (9007199254740990)"
    ))
    perfil <- suppressWarnings(suppressMessages(perfilar_dbi(
      con, "lupa_mdc_pg", proteger_datos_personales = FALSE
    )))
    sql <- perfil$resumen_tabla$sql
    estado <- sql$estado[sql$columna == "v" & sql$metrica == "mediana"]
    valor <- as.numeric(perfil$resumen_tabla$columnas$mediana)
    if (is.na(esperadas[[tipo]])) {
      expect_identical(estado, "no_disponible", info = tipo)
    } else {
      expect_identical(estado, "calculado", info = tipo)
      expect_identical(valor, esperadas[[tipo]], info = tipo)
    }
  }
})

test_that("SQL Server: la mediana de un decimal es la exacta", {
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
  on.exit({
    try(DBI::dbExecute(con, "DROP TABLE lupa_mdc_mssql"), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)
  .mdc_crear(con, "lupa_mdc_mssql", decimal = "decimal", doble = "float")
  .mdc_comprobar(con, "lupa_mdc_mssql")
  .mdc_comprobar(con, "lupa_mdc_mssql", universo = "muestra_motor",
                 muestra_motor = 100L, bloque_muestra = "solo_agregados")
  # `money`: la suma de dos centrales de 8e14 desborda el tipo, y la de
  # `smallmoney` se lee mal como doble; las dos se suman como `decimal(38,4)`.
  DBI::dbExecute(con, "DROP TABLE lupa_mdc_mssql")
  DBI::dbExecute(con, "CREATE TABLE lupa_mdc_mssql (m money, s smallmoney)")
  DBI::dbExecute(con, paste(
    "INSERT INTO lupa_mdc_mssql VALUES (853934926792751.2961, 201706.4080),",
    "(797410094562274.9358, 185062.0849)"
  ))
  esperadas <- c(m = 0x1.7778ee5991049p+49, s = 0x1.79b41f8bac711p+17)
  for (via in list(list(), list(universo = "muestra_motor",
                                muestra_motor = 100L,
                                bloque_muestra = "solo_agregados"))) {
    perfil <- suppressWarnings(suppressMessages(do.call(perfilar_dbi, c(
      list(con, "lupa_mdc_mssql", proteger_datos_personales = FALSE), via
    ))))
    columnas <- perfil$resumen_tabla$columnas
    for (k in names(esperadas)) {
      expect_identical(
        as.numeric(columnas$mediana[columnas$columna == k]), esperadas[[k]],
        info = k
      )
    }
  }
})

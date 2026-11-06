# En T-SQL, `decimal(p, s) * 1.0` es `decimal(p + 3, s + 1)`, y desde `p = 36`
# pasa de las 38 cifras: SQL Server recorta la escala a 6 decimales. La mediana
# que promedia las dos centrales con `AVG(valor * 1.0)` daba `0` para 0,0000001 y
# 0,0000003 en un `decimal(38,7)`. Con compatibilidad menor que 110 no hay
# `PERCENTILE_CONT`, y toda mediana pasa por ese promedio: la tabla completa por
# las dos consultas y la muestra por la CTE de ventanas. Estas pruebas crean una
# base con compatibilidad 100 y comparan cada mediana con la de R.

# Fuera de CRAN: necesita un SQL Server y crea una base (varios segundos).
testthat::skip_on_cran()

.mdec_con_mssql <- function(base) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("odbc")
  clave <- Sys.getenv("LUPA_MSSQL_PWD", unset = "")
  skip_if(!nzchar(clave), "Sin LUPA_MSSQL_PWD.")
  con <- tryCatch(DBI::dbConnect(
    odbc::odbc(), Driver = "FreeTDS",
    Server = Sys.getenv("LUPA_MSSQL_HOST", "127.0.0.1"),
    Port = as.integer(Sys.getenv("LUPA_MSSQL_PORT", "1433")),
    UID = Sys.getenv("LUPA_MSSQL_USER", "sa"), PWD = clave,
    Database = base, TDS_Version = "8.0", TrustServerCertificate = "yes",
    timeout = 5
  ), error = function(e) NULL)
  skip_if(is.null(con), "SQL Server no disponible.")
  con
}

test_that("la mediana de un decimal ancho no pierde cifras en compatibilidad 100", {
  principal <- .mdec_con_mssql(Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"))
  base <- "lupa_mediana_decimal_c100"
  creada <- tryCatch({
    DBI::dbExecute(principal, paste0(
      "IF DB_ID('", base, "') IS NOT NULL DROP DATABASE ", base
    ), immediate = TRUE)
    DBI::dbExecute(principal, paste0("CREATE DATABASE ", base), immediate = TRUE)
    DBI::dbExecute(principal, paste0(
      "ALTER DATABASE ", base, " SET COMPATIBILITY_LEVEL = 100"
    ), immediate = TRUE)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creada, "No se pudo crear una base con compatibilidad 100.")
  con <- .mdec_con_mssql(base)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbExecute(principal, paste0("DROP DATABASE ", base),
                       immediate = TRUE), silent = TRUE)
    DBI::dbDisconnect(principal)
  }, add = TRUE)

  # Con parametros, FreeTDS aborta R al mandar un `decimal(38,30)`: la tabla se
  # escribe con literales, que en un `decimal` son exactos.
  DBI::dbExecute(con, paste(
    "CREATE TABLE t (d38_7 DECIMAL(38,7), d38_30 DECIMAL(38,30),",
    "d10_1 DECIMAL(10,1), entero INT)"
  ), immediate = TRUE)
  filas <- c(
    "(0.0000001, 0.000000000000000000005055094453, 0.1, 7)",
    "(0.0000003, 0.000000000000000000005055094454, 0.2, 8)",
    "(0.0000001, 0.000000000000000000001000000001, 0.1, 7)",
    "(0.0000003, 0.000000000000000000009000000009, 0.2, 8)"
  )
  DBI::dbExecute(con, paste0("INSERT INTO t VALUES ", paste(filas, collapse = ", ")),
                 immediate = TRUE)
  nivel <- DBI::dbGetQuery(con, paste(
    "SELECT compatibility_level FROM sys.databases WHERE name = DB_NAME()"
  ))[[1L]]
  expect_equal(as.integer(nivel), 100L)
  leidos <- DBI::dbGetQuery(con, "SELECT d38_7, d38_30, d10_1, entero FROM t")
  esperado <- vapply(leidos, function(v) stats::median(as.numeric(v)), numeric(1L))

  comprobar <- function(perfil, metodo) {
    sql <- perfil$resumen_tabla$sql
    columnas <- perfil$resumen_tabla$columnas
    for (columna in names(esperado)) {
      fila <- sql[sql$metrica == "mediana" & sql$columna == columna, , drop = FALSE]
      # Control de que se ejerce el promedio y no la consolidada.
      expect_identical(fila$metodo, metodo, info = columna)
      valor <- as.numeric(columnas$mediana[columnas$columna == columna])
      # En compatibilidad 100 la conversion de `decimal` a `FLOAT` es menos
      # exacta: se admite un ulp. Antes el error era de todo el valor.
      expect_lte(abs(valor - esperado[[columna]]),
                 2 * .Machine$double.eps * abs(esperado[[columna]]))
    }
  }
  completa <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE
  )))
  comprobar(completa, "dos_consultas")
  muestra <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 4L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  )))
  medianas <- muestra$resumen_tabla$sql
  medianas <- medianas[medianas$metrica == "mediana", , drop = FALSE]
  # La muestra que cubre la tabla va por la CTE de ventanas, que promedia igual.
  expect_true(all(grepl("cte_ventana", medianas$metodo, fixed = TRUE,
                        useBytes = TRUE)))
  comprobar(muestra, medianas$metodo[[1L]])
})

# Con compatibilidad 100 el `CAST` de un decimal a `FLOAT` no redondea bien: el
# promedio en el servidor salia a un ulp de `median()` en uno de cada tres
# pares de `decimal(38,30)` y `decimal(38,20)`. Las centrales viajan como las
# lee el controlador y se promedian en R. Estos pares son de los que lo
# mostraban; cada columna tiene dos filas, y la mediana tiene que ser la de R
# hasta el ultimo bit.
test_that("la mediana de un decimal de 38 cifras es la de R en compatibilidad 100", {
  principal <- .mdec_con_mssql(Sys.getenv("LUPA_MSSQL_NAME", "lupa_pruebas"))
  base <- "lupa_mediana_decimal_pares_c100"
  creada <- tryCatch({
    DBI::dbExecute(principal, paste0(
      "IF DB_ID('", base, "') IS NOT NULL DROP DATABASE ", base
    ), immediate = TRUE)
    DBI::dbExecute(principal, paste0("CREATE DATABASE ", base), immediate = TRUE)
    DBI::dbExecute(principal, paste0(
      "ALTER DATABASE ", base, " SET COMPATIBILITY_LEVEL = 100"
    ), immediate = TRUE)
    TRUE
  }, error = function(e) FALSE)
  skip_if(!creada, "No se pudo crear una base con compatibilidad 100.")
  con <- .mdec_con_mssql(base)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
    try(DBI::dbExecute(principal, paste0("DROP DATABASE ", base),
                       immediate = TRUE), silent = TRUE)
    DBI::dbDisconnect(principal)
  }, add = TRUE)
  pares <- list(
    a = c("226992.808529447732050189696242445935",
          "1.293267865383933987390217116380"),
    b = c("135496.464407962477475569423577355872",
          "409.763122013537491611427428874553"),
    c = c("97.543594709466152994394058523550",
          "9.972384056639528202008083859033"),
    d = c("53872.58616771580365152156", "9.95396204220769344315"),
    e = c("163638.03438305067073146678", "779100.83394603776831600912"),
    f = c("8253.90651511007901151702", "3870.25973563673977909110")
  )
  tipos <- c(a = "DECIMAL(38,30)", b = "DECIMAL(38,30)", c = "DECIMAL(38,30)",
             d = "DECIMAL(38,20)", e = "DECIMAL(38,20)", f = "DECIMAL(38,20)")
  DBI::dbExecute(con, paste0(
    "CREATE TABLE t (", paste(names(tipos), tipos, collapse = ", "), ")"
  ), immediate = TRUE)
  for (i in 1:2) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO t VALUES (",
      paste(vapply(pares, `[[`, character(1L), i), collapse = ", "), ")"
    ), immediate = TRUE)
  }
  leidos <- DBI::dbGetQuery(con, paste0(
    "SELECT ", paste(names(tipos), collapse = ", "), " FROM t"
  ))
  esperado <- vapply(leidos, function(v) stats::median(as.numeric(v)),
                     numeric(1L))
  comprobar <- function(perfil) {
    columnas <- perfil$resumen_tabla$columnas
    sql <- perfil$resumen_tabla$sql
    for (columna in names(esperado)) {
      fila <- sql[sql$metrica == "mediana" & sql$columna == columna, ,
                  drop = FALSE]
      expect_true(fila$estado %in% c("calculado", "estimado"), info = columna)
      expect_identical(
        as.numeric(columnas$mediana[columnas$columna == columna]),
        esperado[[columna]], info = columna
      )
    }
  }
  comprobar(suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", proteger_datos_personales = FALSE
  ))))
  comprobar(suppressWarnings(suppressMessages(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 2L,
    bloque_muestra = "solo_agregados", proteger_datos_personales = FALSE
  ))))
})

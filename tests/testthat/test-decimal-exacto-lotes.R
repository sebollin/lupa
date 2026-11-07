# Las sumas exactas de los decimales van por lotes, cada expresion se elige para
# que no pueda desbordar ni redondear, y una columna que el motor rechaza no se
# lleva a las demas.
#
# Lo que pasaba, con una sola consulta para todos los decimales de la tabla:
# * 600 columnas `numeric(18,2)` pasaban el limite de 1.664 expresiones del
#   `SELECT` de PostgreSQL, y por bloques no habia ninguna media;
# * en DuckDB el cuadrado de un `DECIMAL(15,0)` se hacia en 64 bits y
#   desbordaba, y todas las columnas de la tabla quedaban con la media y el
#   desvio del motor;
# * un `decimal(38,30)` de SQL Server cuyos valores difieren en 1e8 no tenia
#   cuadrado exacto, y quedaba el `STDEV` del motor;
# * un decimal con valores de 2^53 o mas, un `NaN` de PostgreSQL por bloques,
#   un `metricas` sin la cuenta o sin la media, un `max_consultas` justo y una
#   tabla temporal de SQL Server dejaban la media o el desvio del motor, o
#   ninguno;
# * con `incluir_valores = FALSE` el SQL del desvio del motor publicaba la media
#   como literal.
#
# Las esperadas son exactas: fracciones sobre el texto de cada valor,
# redondeadas al doble mas cercano fuera de R y escritas en hexadecimal.
testthat::skip_on_cran()

.dlot_centavos <- c("500000000000006.60", "500000000000009.60",
                    "500000000000005.60", "500000000000007.60")
.dlot_centavos_media <- 0x1.c6bf526340076p+48
.dlot_centavos_desvio <- 0x1.b534070e9620cp+0

test_that("DuckDB: un cuadrado que desbordaba en 64 bits no arrastra a las demas columnas", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  # `s` sola sale bien; con `f` en la misma consulta quedaba con el desvio del
  # motor, a un ulp (`0x1.0d9c8987baa49p+0`).
  .dlot_tabla(con, "dlot_arrastre",
              c(s = "DECIMAL(18,2)", f = "DECIMAL(15,0)"),
              list(s = c("1.10", "2.20", "3.30", "1.10"),
                   f = c("999999999999999", "-999999999999998", "5", "7")))
  perfil <- .dlot_perfil(con, "dlot_arrastre")
  .dlot_esperar(perfil, "s", 0x1.ecccccccccccdp+0, 0x1.0d9c8987baa4ap+0)
  .dlot_esperar(perfil, "f", 0x1.ap+1, 0x1.734cb65550766p+49)
})

test_that("una columna que el motor rechaza queda sola: el lote se parte", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dlot_tabla(con, "dlot_rechazo",
              c(a = "DECIMAL(18,2)", b = "DECIMAL(18,2)", c = "DECIMAL(18,2)"),
              list(a = .dlot_centavos, b = .dlot_centavos,
                   c = c("999999999999999.99", "-999999999999999.98", "0.01",
                         "0.02")))
  real <- lupa:::.consultar_dbi
  rechazadas <- 0L
  testthat::local_mocked_bindings(
    .consultar_dbi = function(conexion, sql, ...) {
      if (grepl("lupa_suma_", sql, fixed = TRUE, useBytes = TRUE) &&
          grepl("TRUNC(c)", sql, fixed = TRUE, useBytes = TRUE)) {
        rechazadas <<- rechazadas + 1L
        return(list(ok = FALSE, datos = NULL, motivo = "rechazo de prueba"))
      }
      real(conexion, sql, ...)
    },
    .package = "lupa"
  )
  perfil <- .dlot_perfil(con, "dlot_rechazo")
  .dlot_esperar(perfil, "a", .dlot_centavos_media, .dlot_centavos_desvio)
  .dlot_esperar(perfil, "b", .dlot_centavos_media, .dlot_centavos_desvio)
  # Las sumas van en el lote de agregados: el motor lo rechaza entero, se
  # parte hasta aislar `c`, y `c` se pide sola sin las sumas.
  expect_true(rechazadas >= 2L)
  # La media del motor de `c` -los valores se cancelan- no cabe en la tolerancia
  # de los dobles: no se publica la del motor, y el motivo lo dice.
  sql <- perfil$resumen_tabla$sql
  fila <- sql$columna == "c" & sql$metrica == "media"
  expect_identical(sql$estado[fila], "no_disponible")
  expect_true(grepl("sumas exactas", sql$motivo[fila], fixed = TRUE,
                    useBytes = TRUE))
  expect_true(is.na(perfil$resumen_tabla$columnas$media[
    perfil$resumen_tabla$columnas$columna == "c"
  ]))
})

test_that("DuckDB: la media y el desvio exactos de la muestra del motor sin muestra en R", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dlot_tabla(con, "dlot_muestra", c(x = "DECIMAL(18,2)"),
              list(x = .dlot_centavos))
  perfil <- .dlot_perfil(con, "dlot_muestra", universo = "muestra_motor",
                         muestra_motor = 4L, bloque_muestra = "solo_agregados")
  .dlot_esperar(perfil, "x", .dlot_centavos_media, .dlot_centavos_desvio)
})

test_that("DuckDB: con incluir_valores = FALSE el SQL publicado no lleva la media como literal", {
  con <- .dlot_duck()
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
          add = TRUE)
  .dlot_tabla(con, "dlot_literal",
              c(b = "BIGINT", f = "DOUBLE", d = "DECIMAL(18,2)"),
              list(b = sprintf("%.0f", 731946280000 + c(0, 3, 7, 1, 9)),
                   f = sprintf("%.2f", 481726350 + c(0.25, 3.5, 7.75, 1, 9.5)),
                   d = sprintf("%.2f",
                               5912837400 + c(0.25, 3.5, 7.75, 1, 9.5))))
  for (metricas in list(NULL, "desvio")) {
    argumentos <- if (is.null(metricas)) list() else list(metricas = metricas)
    perfil <- do.call(.dlot_perfil, c(
      list(con, "dlot_literal", incluir_valores = FALSE), argumentos
    ))
    sql <- perfil$resumen_tabla$sql$sql
    for (marca in c("73194628", "48172635", "59128374")) {
      expect_false(any(grepl(marca, sql, fixed = TRUE, useBytes = TRUE)),
                   info = paste(marca, paste(metricas, collapse = "+")))
    }
  }
  # El control: con valores, el SQL del desvio lleva el centro.
  perfil <- .dlot_perfil(con, "dlot_literal")
  expect_true(any(grepl("73194628", perfil$resumen_tabla$sql$sql,
                        fixed = TRUE, useBytes = TRUE)))
})

test_that("DuckDB y PostgreSQL: un decimal con valores de 2^53 o mas tiene media y desvio", {
  casos <- list(
    e0 = list(valores = c("100000000000000000001", "100000000000000000004",
                          "100000000000000000006", "-3"),
              media = 0x1.043561a88293p+66, desvio = 0x1.5af1d78b58c4p+65),
    e2 = list(valores = c("12345678901234567.25", "12345678901234568.50",
                          "12345678901234569.75", "0.01"),
              media = 0x1.0729fa3083c53p+53, desvio = 0x1.5ee2a2eb5a5c4p+52)
  )
  probar <- function(con, tipo, tabla, bloques) {
    .dlot_tabla(con, tabla, c(e0 = sprintf(tipo, 38L, 0L),
                              e2 = sprintf(tipo, 38L, 2L)),
                lapply(casos, `[[`, "valores"))
    vias <- c(list(list()), if (bloques) list(list(bloque_filas = 2L)))
    for (via in vias) {
      perfil <- do.call(.dlot_perfil, c(list(con, tabla), via))
      for (nombre in names(casos)) {
        .dlot_esperar(perfil, nombre, casos[[nombre]]$media,
                      casos[[nombre]]$desvio,
                      info = paste(tabla, names(via)))
      }
    }
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
  }
  con <- .dlot_duck()
  probar(con, "DECIMAL(%d,%d)", "dlot_grandes", bloques = FALSE)
  DBI::dbDisconnect(con, shutdown = TRUE)
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  probar(con, "NUMERIC(%d,%d)", "lupa_dlot_grandes", bloques = TRUE)
})

test_that("PostgreSQL: NaN e Infinity quedan fuera de las sumas por bloques, como en la tabla completa", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  .dlot_tabla(con, "lupa_dlot_nan", c(b = "numeric(18,2)", c = "numeric"),
              list(b = c("1.10", "'NaN'", "3.30", "4.40"),
                   c = c("1.10", "'Infinity'", "3.30", "4.40")))
  on.exit(try(DBI::dbExecute(con, "DROP TABLE lupa_dlot_nan"), silent = TRUE),
          add = TRUE, after = FALSE)
  version <- DBI::dbGetQuery(con, "SHOW server_version_num")[[1L]]
  for (via in list(list(), list(bloque_filas = 2L))) {
    perfil <- do.call(.dlot_perfil, c(list(con, "lupa_dlot_nan"), via))
    .dlot_esperar(perfil, "b", 0x1.7777777777777p+1, 0x1.ae26aed522cc3p+0,
                  info = names(via))
    if (as.numeric(version) >= 140000) {
      .dlot_esperar(perfil, "c", 0x1.7777777777777p+1, 0x1.ae26aed522cc3p+0,
                    info = names(via))
    }
  }
})

test_that("PostgreSQL: una tabla de 600 decimales tiene todas sus medias por bloques", {
  con <- .dlot_pg()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  nombres <- sprintf("c%03d", seq_len(600L))
  columnas <- stats::setNames(rep("numeric(18,2)", 600L), nombres)
  valores <- stats::setNames(
    lapply(seq_len(600L), function(i) .dlot_centavos), nombres
  )
  .dlot_tabla(con, "lupa_dlot_ancha", columnas, valores)
  on.exit(try(DBI::dbExecute(con, "DROP TABLE lupa_dlot_ancha"), silent = TRUE),
          add = TRUE, after = FALSE)
  for (via in list(list(), list(bloque_filas = 2L))) {
    perfil <- do.call(.dlot_perfil, c(
      list(con, "lupa_dlot_ancha", metricas = c("basicos", "desvio")), via
    ))
    columnas <- perfil$resumen_tabla$columnas
    expect_identical(as.numeric(columnas$media),
                     rep(.dlot_centavos_media, 600L), info = names(via))
    expect_identical(as.numeric(columnas$desvio),
                     rep(.dlot_centavos_desvio, 600L), info = names(via))
  }
})

test_that("PostgreSQL y SQL Server: cualquier metricas, sin valores y con el presupuesto justo", {
  for (motor in c("pg", "mssql")) {
    con <- if (motor == "pg") .dlot_pg() else .dlot_mssql()
    tipo <- if (motor == "pg") "NUMERIC(38,30)" else "DECIMAL(38,30)"
    tabla <- paste0("lupa_dlot_met_", motor)
    .dlot_tabla(con, tabla, c(x = tipo), list(x = .dlot_treinta))
    .dlot_probar_metricas(con, tabla, motor)
    .dlot_probar_presupuesto(con, tabla, motor)
    try(DBI::dbExecute(con, paste0("DROP TABLE ", tabla)), silent = TRUE)
    DBI::dbDisconnect(con)
  }
})

test_that("SQL Server: un decimal(38,30) con diferencias de nueve cifras enteras, y una tabla temporal", {
  con <- .dlot_mssql()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # 41 valores de -1e8 a 1e8 con 30 decimales: el `STDEV` del motor quedaba a un
  # ulp (`0x1.a7e4cc69e235dp+25`).
  e <- c(
    "-57007130.166964159257127911748632345409",
    "-75728291.460011539001715533768253334144",
    "-50352947.777944733399551595315258748573",
    "12181117.016858081172835301059343830317",
    "-66494058.200688211203229528100796720288",
    "28690781.379423885021567768723368637089",
    "86459725.707524458692769587772620079404",
    "37725088.534136031571311464998795163733",
    "-14351613.234076051879250412175348964810",
    "-34305136.086618279421977201533603719479",
    "-75746241.166152638919846971740920958608",
    "-69877069.879967457864620821303197128055",
    "-43976101.789464161294083341704491370891",
    "-63509213.915085143227478262538354812199",
    "-27033855.975140339316267232230760483057",
    "-60020443.606732179111095202844917334468",
    "32492284.052555537406918097528393005263",
    "31140459.573105585187593241141997785126",
    "-68605090.108572018894667161423608071097",
    "42176280.305765091102340199851672410150",
    "-21141577.463397542030715977479828702692",
    "74766387.236215781561363782282493918198",
    "-94090475.584067178037258349659988996278",
    "-84364974.515842116794501730148390062936",
    "-64312606.252272229137315849930709592551",
    "-64233929.570115205725466442455537492999",
    "-84024627.804979769172977852142504547938",
    "37433266.381434839480861113048655558504",
    "-25367575.841250326330289733215355530163",
    "-65003892.685611623842701032387640178783",
    "-91029850.508339848288653081887420106917",
    "-94867659.838511679061851256988227046981",
    "-95068194.122069787295909685624027293451",
    "-63858976.461245973755005517687659187130",
    "-50069453.979949705474332054119452425350",
    "38609071.701275574688900633130333630061",
    "-14674400.457759951878366057232914525553",
    "58425354.251783661762044951451784161610",
    "94971013.865866789537996918515996513769",
    "37629863.703488469470483386427537682501",
    "-14510592.568323943496696420105663437964"
  )
  .dlot_tabla(con, "lupa_dlot_ancho", c(e = "decimal(38,30)"), list(e = e))
  on.exit(try(DBI::dbExecute(con, "DROP TABLE lupa_dlot_ancho"), silent = TRUE),
          add = TRUE, after = FALSE)
  for (via in list(list(), list(bloque_filas = 2L))) {
    perfil <- do.call(.dlot_perfil, c(list(con, "lupa_dlot_ancho"), via))
    .dlot_esperar(perfil, "e", -0x1.7bf408c381e01p+24, 0x1.a7e4cc69e235cp+25,
                  info = names(via))
  }
  DBI::dbExecute(con, paste(
    "IF OBJECT_ID('tempdb..#lupa_dlot_temporal') IS NOT NULL",
    "DROP TABLE #lupa_dlot_temporal"
  ))
  DBI::dbExecute(con, "CREATE TABLE #lupa_dlot_temporal (x decimal(18,2))")
  DBI::dbExecute(con, paste0(
    "INSERT INTO #lupa_dlot_temporal VALUES ",
    paste0("(", .dlot_centavos, ")", collapse = ", ")
  ))
  for (via in list(list(), list(bloque_filas = 2L))) {
    perfil <- do.call(.dlot_perfil, c(list(con, "#lupa_dlot_temporal"), via))
    .dlot_esperar(perfil, "x", .dlot_centavos_media, .dlot_centavos_desvio,
                  info = paste("temporal", names(via)))
  }
})

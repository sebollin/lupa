# O157: ronda 33, A-2 (decision D8 de Sebastian: el cometido principal del
# paquete es el diagnostico de la calidad de los datos, y una proteccion que
# cuesta diagnostico importante no va por omision). Con
# `proteger_datos_personales = FALSE` el mensaje de un error del motor se
# publica entero -el literal que rompio la lectura y el SQL- en todas las
# salidas donde se publica; con la proteccion activa no cambia nada. Cada
# bloque rehace a mano la cifra -lo que el motor dice a la misma consulta- y
# falla contra la copia de partida de la ronda 33, donde el literal salia
# `[literal omitido]` con o sin proteccion.

# Una vista que castea a entero una columna de texto con un faltante
# disfrazado, `N/D`: leerla falla, y duckdb cita la celda y el SQL. La vista
# tiene UNA columna: con dos, desde la ronda 35-X la muestra aisla la que falla
# sobre sus mismas filas y se lee la otra (antes la sonda leia una fila, la 25
# no se veia y la muestra entera se perdia); aca lo que se mide es el mensaje
# de una lectura de la muestra que falla.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r33a2_conexion <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  d <- data.frame(id = 1:30, codigo_txt = as.character(1:30),
                  stringsAsFactors = FALSE)
  d$codigo_txt[25] <- "N/D"
  DBI::dbWriteTable(con, "codigos", d)
  DBI::dbExecute(con, paste(
    "CREATE VIEW codigos_v AS",
    "SELECT CAST(codigo_txt AS INTEGER) AS codigo FROM codigos"
  ))
  con
}

# Lo que el motor dice a cada consulta, a mano: su mensaje de error, o `NA` si
# la consulta no falla.
.r33a2_motor_dice <- function(con, sql) {
  vapply(sql, function(s) {
    if (is.na(s)) return(NA_character_)
    e <- tryCatch(DBI::dbGetQuery(con, s), error = identity)
    if (inherits(e, "error")) conditionMessage(e) else NA_character_
  }, character(1L), USE.NAMES = FALSE)
}

.r33a2_con_avisos <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(valor = valor, avisos = avisos)
}

test_that("sin proteccion, perfilar_dbi() publica entero el mensaje del motor", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- .r33a2_conexion()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  r <- .r33a2_con_avisos(
    perfilar_dbi(con, "codigos_v", proteger_datos_personales = FALSE)
  )
  sql <- r$valor$resumen_tabla$sql
  dice <- .r33a2_motor_dice(con, sql$sql)
  fallan <- !is.na(dice)
  # Las consultas de `codigo` leen la celda y fallan.
  expect_gt(sum(fallan), 0L)
  # Cada motivo es, entero, lo que el motor dice a esa misma consulta: la
  # celda, la columna de origen y el SQL.
  expect_identical(sql$motivo[fallan], dice[fallan])
  expect_true(all(grepl("N/D", sql$motivo[fallan], fixed = TRUE)))

  # La lectura de la muestra: `cobertura$motivo` y el aviso de consola.
  cobertura <- r$valor$resumen_tabla$cobertura
  muestra <- cobertura[cobertura$bloque == "perfil_muestra" &
                         !is.na(cobertura$sql), , drop = FALSE]
  expect_identical(nrow(muestra), 1L)
  dice_muestra <- .r33a2_motor_dice(con, muestra$sql)
  expect_false(is.na(dice_muestra))
  expect_true(grepl(dice_muestra, muestra$motivo, fixed = TRUE))
  expect_true(any(grepl(dice_muestra, r$avisos, fixed = TRUE)))
})

test_that("con la proteccion activa, el mismo mensaje sigue sin el literal", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- .r33a2_conexion()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  r <- .r33a2_con_avisos(perfilar_dbi(con, "codigos_v"))
  sql <- r$valor$resumen_tabla$sql
  fallan <- !is.na(.r33a2_motor_dice(con, sql$sql))
  expect_gt(sum(fallan), 0L)
  expect_false(any(grepl("N/D", sql$motivo, fixed = TRUE)))
  expect_true(all(grepl("[literal omitido]", sql$motivo[fallan],
                        fixed = TRUE)))
  expect_false(any(grepl("N/D", r$valor$resumen_tabla$cobertura$motivo,
                         fixed = TRUE)))
  expect_gt(length(r$avisos), 0L)
  expect_false(any(grepl("N/D", r$avisos, fixed = TRUE)))
})

test_that("perfilar_coleccion() sigue la bandera en todas sus salidas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- .r33a2_conexion()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  vista <- coleccion(con, "codigos_v")

  r <- .r33a2_con_avisos(
    perfilar_coleccion(vista, proteger_datos_personales = FALSE)
  )
  lecturas <- r$valor$meta$lecturas
  dice <- .r33a2_motor_dice(con, lecturas$sql)
  fallan <- !is.na(dice)
  expect_gt(sum(fallan), 0L)
  expect_identical(lecturas$motivo[fallan], dice[fallan])
  metricas <- r$valor$cobertura_metricas$motivo
  metricas <- metricas[grepl("N/D", metricas, fixed = TRUE)]
  expect_gt(length(metricas), 0L)
  expect_true(all(metricas %in% dice[fallan]))
  expect_true(any(grepl("N/D", r$valor$cobertura_coleccion$motivo,
                        fixed = TRUE)))
  expect_true(any(grepl("N/D", r$avisos, fixed = TRUE)))

  # Con la proteccion activa, en ninguna.
  p <- .r33a2_con_avisos(perfilar_coleccion(vista))
  textos <- c(p$valor$meta$lecturas$motivo,
              p$valor$cobertura_metricas$motivo,
              p$valor$cobertura_coleccion$motivo, p$avisos)
  expect_true(any(grepl("[literal omitido]", textos, fixed = TRUE)))
  expect_false(any(grepl("N/D", textos, fixed = TRUE)))

  # Una exportada sin la bandera, sola, despues de una llamada sin
  # proteccion: protege.
  pares <- coleccion(con, c("codigos", "codigos_v"))
  rel <- relaciones_coleccion(
    pares, pares = data.frame(tabla_1 = "codigos_v", tabla_2 = "codigos")
  )
  motivos <- rel$cobertura_pares$motivo
  expect_true(any(grepl("[literal omitido]", motivos, fixed = TRUE)))
  expect_false(any(grepl("N/D", motivos, fixed = TRUE)))
})

test_that("la decision llega a la lectura del mensaje por un solo camino", {
  texto <- "Could not convert string 'N/D' to INT32"
  sin_proteccion <- function(condicion) {
    .fijar_proteccion_motor(FALSE)
    .mensaje_motor(condicion)
  }
  # Sin nadie que la fije, protege: el lado cerrado.
  expect_true(.protege_mensaje_motor())
  expect_false(grepl("N/D", .mensaje_motor(simpleError(texto)), fixed = TRUE))
  expect_identical(sin_proteccion(simpleError(texto)), texto)
  # Una condicion propia de la via DBI ya trae el mensaje entero.
  propia <- .condicion_dbi("lupa_error_dbi_r33a2",
                           paste0("No se pudo leer: ", texto))
  expect_identical(sin_proteccion(propia), paste0("No se pudo leer: ", texto))

  # El de odbc, armado con cli: sin las vinetas ni los cortes, y nada mas.
  odbc <- tryCatch(cli::cli_abort(c(
    "!" = "ODBC failed with error 00000 from \\\n  [FreeTDS][SQL Server].",
    x = paste("Conversion failed when converting the nvarchar value 'N/D'",
              "to data type int."),
    "*" = "<SQL> 'SELECT COUNT(\"codigo\") AS \"n\" FROM \"codigos_v\"'"
  ), class = "odbc_database_error", call = NULL), error = identity)
  expect_identical(sin_proteccion(odbc), paste0(
    "ODBC failed with error 00000 from [FreeTDS][SQL Server].\n",
    "Conversion failed when converting the nvarchar value 'N/D' to data ",
    "type int.\n",
    "<SQL> 'SELECT COUNT(\"codigo\") AS \"n\" FROM \"codigos_v\"'"
  ))
  expect_false(grepl("N/D", .mensaje_motor(odbc), fixed = TRUE))

  # Legible en cualquier locale: lo que no es UTF-8 valido se lee como latin1.
  crudo <- rawToChar(as.raw(c(0x76, 0x61, 0x6c, 0x6f, 0x72, 0x20, 0x27, 0x4e,
                              0xe9, 0x44, 0x27)))
  leido <- sin_proteccion(simpleError(crudo))
  expect_true(validUTF8(leido))
  expect_identical(leido, paste0("valor 'N", "\u00e9", "D'"))

  # Vuelve al salir, tambien en error, y una llamada anidada que protege
  # devuelve la de afuera.
  expect_true(.protege_mensaje_motor())
  expect_error((function() {
    .fijar_proteccion_motor(FALSE)
    .detener("corte")
  })(), "corte")
  expect_true(.protege_mensaje_motor())
  dentro <- function() {
    .fijar_proteccion_motor(TRUE)
    .mensaje_motor(simpleError(texto))
  }
  fuera <- function() {
    .fijar_proteccion_motor(FALSE)
    c(dentro(), .mensaje_motor(simpleError(texto)))
  }
  ambos <- fuera()
  expect_false(grepl("N/D", ambos[[1L]], fixed = TRUE))
  expect_identical(ambos[[2L]], texto)
  expect_true(.protege_mensaje_motor())

  # La bandera por su nombre exacto, como la valida el paquete.
  expect_true(.protege_argumentos(list()))
  expect_false(.protege_argumentos(list(proteger_datos_personales = FALSE)))
  expect_true(.protege_argumentos(list(proteger_datos_personales_x = FALSE)))
})

# N1, la misma propiedad en otro sitio: sin proteccion, `perfilar_coleccion()`
# con perfiles retenidos cerraba igual los perfiles de las demas tablas cuando
# una no se perfilaba -aca, declarada `tipo = "vista"`-: la moda de una columna
# de ciudades salia `[valor protegido]`.
test_that("sin proteccion, una tabla sin perfilar no cierra los perfiles de las demas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  d <- data.frame(
    id = 1:30,
    ciudad = rep(c("Montevideo", "Salto", "Paysandu"), c(15L, 10L, 5L)),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con, "ciudades_a", d)
  DBI::dbWriteTable(con, "ciudades_b", d)
  col <- coleccion(con, data.frame(tabla = c("ciudades_a", "ciudades_b"),
                                   tipo = c("tabla", "vista")))
  # La moda, a mano.
  moda <- names(sort(table(d$ciudad), decreasing = TRUE))[[1L]]
  expect_identical(moda, "Montevideo")
  moda_de <- function(pc) {
    columnas <- pc$perfiles[["ciudades_a"]]$resumen_tabla$columnas
    as.character(columnas$moda[columnas$columna == "ciudad"])
  }

  # Solo la moda: la prueba mira un campo, y cada agregado de mas cuesta.
  sin <- suppressWarnings(perfilar_coleccion(
    col, conservar_perfiles = TRUE, metricas = "moda",
    proteger_datos_personales = FALSE
  ))
  expect_identical(moda_de(sin), moda)
  expect_null(
    sin$perfiles[["ciudades_a"]]$resumen_tabla$meta$proteccion_personal$otras_tablas
  )

  # Con la proteccion activa, se cierra y lo dice, como antes.
  con_proteccion <- suppressWarnings(
    perfilar_coleccion(col, conservar_perfiles = TRUE, metricas = "moda")
  )
  expect_false(identical(moda_de(con_proteccion), moda))
  expect_match(
    con_proteccion$perfiles[["ciudades_a"]]$resumen_tabla$meta$proteccion_personal$otras_tablas,
    "No verificada", fixed = TRUE
  )
})

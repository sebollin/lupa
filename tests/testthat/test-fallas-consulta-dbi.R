# Lo que la via DBI dice de una consulta que falla, y lo que hace por eso.
#
# * Una prueba de capacidad que el motor rechaza porque no tiene esa capacidad
#   queda como siempre. La que falla por otra causa -un permiso, un
#   interbloqueo, una cancelacion- y cambia la estrategia lo deja escrito.
# * En DuckDB la corrida se niega dentro de una transaccion abierta, y lo sabe
#   sin depender de como la sesion entrega las fechas.
# * El lugar de la conexion lo dice el motor, en una sentencia y solo cuando
#   hace falta.
# * Un permiso negado se reconoce por lo que el motor dijo, y una interrupcion
#   sale como interrupcion.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.fcd_perfil <- function(abrir, gancho = NULL, ...) {
  .cp_correr(
    abrir, function(con) perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA, ...),
    gancho
  )
}

# Hace fallar la prueba de `STDDEV_SAMP` con una condicion fabricada.
.fcd_gancho_desvio <- function(condicion) {
  function(est, operacion, conexion, sql) {
    if (!is.na(sql) && grepl("STDDEV_SAMP(1.0)", sql, fixed = TRUE)) {
      est$lanzar <- condicion
    }
  }
}

# Las sentencias que llaman a la funcion nativa sobre la tabla (la prueba de
# capacidad la llama sobre una constante).
.fcd_usa_nativa <- function(sql) {
  !is.na(sql) & grepl("STDDEV_SAMP(", sql, fixed = TRUE) &
    !grepl("STDDEV_SAMP(1.0)", sql, fixed = TRUE)
}

.fcd_capacidades <- function(perfil) {
  cobertura <- perfil$resumen_tabla$cobertura
  cobertura[startsWith(cobertura$elemento, "capacidad::"), , drop = FALSE]
}

test_that("una tabla sana no publica nada nuevo sobre sus pruebas de capacidad", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("duckdb")
  for (abrir in list(.cp_abrir_sqlite, .cp_abrir_duckdb)) {
    r <- .fcd_perfil(abrir)
    consultas <- r$valor$resumen_tabla$meta$consultas
    # El motor rechazo al menos una forma candidata: es su respuesta.
    expect_gte(nrow(consultas$sondas_fallidas), 1L)
    expect_identical(unique(consultas$sondas_fallidas$tipo), "rechazada")
    expect_identical(
      names(consultas$sondas_fallidas),
      c("etapa", "consulta_id", "motivo", "sqlstate", "tipo")
    )
    expect_null(consultas$sondas_fallidas_detalle)
    expect_identical(nrow(.fcd_capacidades(r$valor)), 0L)
  }
})

test_that("la prueba que falla por otra causa deja la causa y la estrategia que se uso", {
  skip_if_not_installed("duckdb")
  # DuckDB tiene `STDDEV_SAMP`: sin la falla, la corrida la usa.
  sana <- .fcd_perfil(.cp_abrir_duckdb)
  expect_gte(sum(.fcd_usa_nativa(sana$sql)), 1L)
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  casos <- list(
    permiso = list(
      "Failed to prepare query : ERROR:  permission denied for function stddev_samp\n",
      "rechazada", "la credencial no tiene permiso"
    ),
    transitoria = list(
      "Failed to fetch row : ERROR:  deadlock detected\nDETAIL:  Process 11 waits for ShareLock.\n",
      "rechazada", "una falla transitoria del motor"
    ),
    cancelada = list(
      "Failed to fetch row : ERROR:  canceling statement due to statement timeout\n",
      "cancelada", paste0("la consulta se cancel", .CP_O)
    ),
    transaccion = list(
      "Failed to prepare query : ERROR:  current transaction is aborted, commands ignored until end of transaction block\n",
      "rechazada", paste0("la transacci", .CP_O, "n estaba abortada")
    )
  )
  for (causa in names(casos)) {
    caso <- casos[[causa]]
    r <- .fcd_perfil(
      .cp_abrir_duckdb, .fcd_gancho_desvio(simpleError(caso[[1L]]))
    )
    expect_s3_class(r$valor, "perfil_dbi")
    consultas <- r$valor$resumen_tabla$meta$consultas
    fallida <- consultas$sondas_fallidas[
      consultas$sondas_fallidas$etapa == "sonda_desvio", , drop = FALSE
    ]
    # La de `STDDEV_SAMP`, fabricada, y la de `STDEV`, que DuckDB no tiene.
    expect_identical(nrow(fallida), 2L, info = causa)
    expect_identical(fallida$tipo[[1L]], caso[[2L]], info = causa)
    detalle <- consultas$sondas_fallidas_detalle
    expect_identical(nrow(detalle), 1L, info = causa)
    expect_identical(detalle$etapa, "sonda_desvio")
    expect_identical(detalle$causa, causa)
    expect_identical(detalle$consulta_id, fallida$consulta_id[[1L]])
    # La estrategia publicada es la que la corrida uso de verdad: ninguna
    # sentencia sobre la tabla llama a la funcion nativa.
    expect_identical(detalle$estrategia_elegida, "dos pasadas")
    expect_identical(sum(.fcd_usa_nativa(r$sql)), 0L, info = causa)
    capacidades <- .fcd_capacidades(r$valor)
    expect_identical(nrow(capacidades), 1L, info = causa)
    expect_identical(
      capacidades$elemento,
      paste0("capacidad::sonda_desvio::", detalle$consulta_id)
    )
    expect_identical(capacidades$estado, "advertido")
    expect_true(grepl(caso[[3L]], capacidades$motivo, fixed = TRUE), info = causa)
    expect_true(grepl("`dos pasadas`", capacidades$motivo, fixed = TRUE))
    # El desvio se calculo igual, por la otra via.
    expect_equal(
      r$valor$resumen_tabla$columnas$desvio,
      sana$valor$resumen_tabla$columnas$desvio
    )
  }
})

test_that("el rechazo por capacidad ausente no agrega nada, tampoco fabricado", {
  skip_if_not_installed("duckdb")
  ausente <- structure(
    class = c("duckdb_error", "rlang_error", "error", "condition"),
    list(
      message = "Scalar Function with name stddev_samp does not exist!",
      error_type = "CATALOG",
      raw_message = "Scalar Function with name stddev_samp does not exist!",
      call = NULL
    )
  )
  r <- .fcd_perfil(.cp_abrir_duckdb, .fcd_gancho_desvio(ausente))
  consultas <- r$valor$resumen_tabla$meta$consultas
  expect_identical(sum(consultas$sondas_fallidas$etapa == "sonda_desvio"), 2L)
  expect_null(consultas$sondas_fallidas_detalle)
  expect_identical(nrow(.fcd_capacidades(r$valor)), 0L)
})

test_that("la lectura de la muestra del motor tambien se clasifica por el reconocedor", {
  skip_if_not_installed("duckdb")
  local_mocked_bindings(.controlador_dbi = function(conexion) "RPostgres")
  # Falla la primera lectura de filas de la muestra, con cada condicion.
  gancho <- function(condicion) {
    armada <- FALSE
    vista <- FALSE
    function(est, operacion, conexion, sql) {
      if (identical(operacion, "dbSendQuery") && !is.na(sql) &&
          grepl("TABLESAMPLE", sql, fixed = TRUE)) {
        armada <<- TRUE
      }
      if (armada && !vista && identical(operacion, "dbFetch")) {
        vista <<- TRUE
        est$lanzar <- condicion
      }
    }
  }
  tipo_de <- function(texto) {
    r <- .fcd_perfil(
      .cp_abrir_duckdb, gancho(simpleError(texto)),
      universo = "muestra_motor", muestra_motor = 12
    )
    fallas <- r$valor$resumen_tabla$meta$consultas$fallas
    fallas$tipo[fallas$etapa == "materializacion_muestra"]
  }
  expect_identical(
    tipo_de("Failed to fetch row : ERROR:  canceling statement due to statement timeout\n"),
    "cancelada"
  )
  # El dato que hizo fallar la lectura dice "timeout" y "cancel": no decide.
  expect_identical(
    tipo_de("Failed to fetch row : ERROR:  invalid input syntax for type integer: \"timeout cancel 57014\"\n"),
    "rechazada"
  )
})

# ---- La transaccion abierta de DuckDB ------------------------------------

.FCD_TXID <- "SELECT CAST(txid_current() AS VARCHAR) AS lupa_transaccion"

.fcd_llamadas <- function() {
  modelo_fcd <- .cp_modelo()
  list(
    perfilar_dbi = function(con, col, pc) {
      perfilar_dbi(con, "lupa_t", fecha = .CP_FECHA)
    },
    plan_perfilado_dbi = function(con, col, pc) plan_perfilado_dbi(con, "lupa_t"),
    medir_dbi = function(con, col, pc) medir_dbi(modelo_fcd, con, fecha = .CP_FECHA),
    plan_medicion_dbi = function(con, col, pc) plan_medicion_dbi(modelo_fcd, con),
    conformidad_dbi = function(con, col, pc) conformidad_dbi(con, modelo_fcd),
    coleccion = function(con, col, pc) coleccion(con, c("lupa_t", "lupa_u")),
    perfilar_coleccion = function(con, col, pc) {
      perfilar_coleccion(col, fecha = .CP_FECHA)
    },
    estimar_costo_coleccion = function(con, col, pc) estimar_costo_coleccion(col),
    relaciones_coleccion = function(con, col, pc) {
      relaciones_coleccion(
        col, pares = data.frame(tabla_1 = "lupa_t", tabla_2 = "lupa_u")
      )
    },
    detectar_tablas_codigos = function(con, col, pc) {
      detectar_tablas_codigos(col, pc)
    },
    medir_dbi_coleccion = function(con, col, pc) {
      medir_dbi(modelo_fcd, col, fecha = .CP_FECHA)
    }
  )
}

.fcd_con_transaccion <- function(abrir, llamada) {
  con <- abrir()
  on.exit(suppressWarnings(try(
    DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE
  )), add = TRUE)
  col <- suppressMessages(coleccion(con, c("lupa_t", "lupa_u")))
  pc <- suppressWarnings(suppressMessages(
    perfilar_coleccion(col, fecha = .CP_FECHA)
  ))
  DBI::dbBegin(con)
  DBI::dbExecute(con, "INSERT INTO lupa_u VALUES (999, 'en la transaccion')")
  est <- .cp_estado()
  .cp_imitar_dbi(est)
  valor <- tryCatch(
    suppressWarnings(suppressMessages(llamada(con, col, pc))),
    error = function(e) e
  )
  confirmada <- tryCatch(DBI::dbCommit(con), error = function(e) e)
  filas <- tryCatch(
    as.numeric(DBI::dbGetQuery(
      con, "SELECT COUNT(*) AS n FROM lupa_u WHERE id = 999"
    )$n),
    error = function(e) NA_real_
  )
  list(valor = valor, sql = est$sql, llegaron = est$llegaron,
       confirmada = confirmada, filas = filas)
}

test_that("DuckDB: con una transaccion abierta las once llamadas se niegan y no la tocan", {
  skip_if_not_installed("duckdb")
  llamadas <- .fcd_llamadas()
  expect_identical(length(llamadas), 11L)
  for (nombre in names(llamadas)) {
    r <- .fcd_con_transaccion(.cp_abrir_duckdb, llamadas[[nombre]])
    expect_s3_class(r$valor, "lupa_error_transaccion_dbi")
    expect_true(grepl(
      paste0("transacci", .CP_O, "n abierta de DuckDB"),
      conditionMessage(r$valor), fixed = TRUE
    ), info = nombre)
    # Solo las dos sentencias que lo averiguan; nada despues.
    expect_identical(r$llegaron, 2L, info = nombre)
    expect_identical(r$sql, rep(.FCD_TXID, 2L), info = nombre)
    # La transaccion sigue viva y lo hecho en ella se conserva.
    expect_true(isTRUE(r$confirmada), info = nombre)
    expect_identical(r$filas, 1, info = nombre)
  }
})

test_that("DuckDB: sin transaccion no se niega, entregue como entregue fechas y enteros", {
  skip_if_not_installed("duckdb")
  # Con `tz_out_convert = "force"` dos lecturas seguidas de `now()` llegan
  # iguales, truncadas al segundo, y la conexion se rechazaba sin transaccion.
  aperturas <- list(
    por_omision = function() .cp_abrir_duckdb(),
    fechas_forzadas = function() .cp_abrir_duckdb(tz_out_convert = "force")
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    aperturas$enteros_64 <- function() .cp_abrir_duckdb(bigint = "integer64")
  }
  for (nombre in names(aperturas)) {
    r <- .fcd_perfil(aperturas[[nombre]])
    expect_s3_class(r$valor, "perfil_dbi")
    expect_identical(sum(r$sql %in% .FCD_TXID), 2L, info = nombre)
    plan <- .cp_correr(
      aperturas[[nombre]],
      function(con) plan_perfilado_dbi(con, "lupa_t", compilar = TRUE)
    )
    expect_s3_class(plan$valor, "plan_perfilado_dbi")
    # La compilacion ya no pregunta `now()`: lee la respuesta de la corrida.
    expect_false(any(grepl("now()", plan$sql, fixed = TRUE)), info = nombre)
  }
})

test_that("DuckDB: el plan anuncia las dos sentencias y la cuenta cuadra", {
  skip_if_not_installed("duckdb")
  clase <- "guarda de transaccion abierta (DuckDB)"
  plan <- .cp_correr(
    .cp_abrir_duckdb, function(con) plan_perfilado_dbi(con, "lupa_t")
  )
  fila <- plan$valor[plan$valor$clase_consulta == clase, , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_equal(fila$n_consultas, 2)
  perfil <- .fcd_perfil(.cp_abrir_duckdb)
  conciliacion <- perfil$valor$resumen_tabla$meta$consultas$conciliacion
  expect_identical(conciliacion$estado, "conciliado")
  expect_equal(as.numeric(conciliacion$anunciadas),
               as.numeric(conciliacion$emitidas))
  expect_equal(as.numeric(conciliacion$emitidas),
               as.numeric(perfil$valor$resumen_tabla$meta$consultas$emitidas))
  # El tope las descuenta: con el total del plan alcanza, con dos menos no.
  total <- as.numeric(conciliacion$emitidas)
  justo <- .fcd_perfil(.cp_abrir_duckdb, max_consultas = total)
  expect_false(isTRUE(justo$valor$resumen_tabla$meta$consultas$agotado))
  corto <- .fcd_perfil(.cp_abrir_duckdb, max_consultas = total - 2)
  expect_true(isTRUE(corto$valor$resumen_tabla$meta$consultas$agotado))
  # La medicion las cuenta igual.
  modelo_fcd <- .cp_modelo()
  plan_medicion <- .cp_correr(
    .cp_abrir_duckdb, function(con) plan_medicion_dbi(modelo_fcd, con)
  )
  expect_identical(
    attr(plan_medicion$valor, "consultas")[["transaccion"]], 2L
  )
  # En una coleccion se mandan una vez y no entran en la cuenta de ninguna
  # tabla.
  coleccion_perfilada <- .cp_correr(.cp_abrir_duckdb, function(con) {
    perfilar_coleccion(
      coleccion(con, c("lupa_t", "lupa_u")), fecha = .CP_FECHA,
      conservar_perfiles = TRUE
    )
  })
  # `coleccion()` y `perfilar_coleccion()` abren cada una su corrida: dos y
  # dos, y ninguna de las dos tablas manda las suyas.
  expect_identical(sum(coleccion_perfilada$sql %in% .FCD_TXID), 4L)
  for (perfil_tabla in coleccion_perfilada$valor$perfiles) {
    consultas <- perfil_tabla$resumen_tabla$meta$consultas
    expect_identical(consultas$conciliacion$estado, "conciliado")
    expect_false(clase %in% perfil_tabla$resumen_tabla$meta$plan$clase_consulta)
  }
})

test_that("SQLite no manda nada de mas, con y sin transaccion", {
  skip_if_not_installed("RSQLite")
  suelta <- .fcd_perfil(.cp_abrir_sqlite)
  expect_false(any(grepl("txid_current", suelta$sql, fixed = TRUE)))
  plan <- .cp_correr(
    .cp_abrir_sqlite, function(con) plan_perfilado_dbi(con, "lupa_t")
  )
  expect_false(any(grepl("transaccion", plan$valor$clase_consulta, fixed = TRUE)))
  abrir_en_transaccion <- function() {
    con <- .cp_abrir_sqlite()
    DBI::dbBegin(con)
    con
  }
  dentro <- .fcd_perfil(abrir_en_transaccion)
  expect_s3_class(dentro$valor, "perfil_dbi")
  expect_identical(dentro$sql, suelta$sql)
})

# ---- El lugar de la conexion ---------------------------------------------

test_that("el lugar es el archivo que el motor dice tener abierto", {
  skip_if_not_installed("RSQLite")
  carpeta <- tempfile("lupa_lugar_")
  dir.create(carpeta)
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  archivo <- file.path(carpeta, "base.sqlite")
  otro <- file.path(carpeta, "otra.sqlite")
  lugar_de <- function(ruta) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ruta)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    .lugar_conexion_dbi(con)
  }
  absoluta <- lugar_de(normalizePath(archivo, winslash = "/", mustWork = FALSE))
  con_vueltas <- lugar_de(file.path(carpeta, ".", "base.sqlite"))
  relativa <- local({
    anterior <- setwd(carpeta)
    on.exit(setwd(anterior), add = TRUE)
    lugar_de("base.sqlite")
  })
  expect_identical(con_vueltas, absoluta)
  expect_identical(relativa, absoluta)
  expect_false(identical(lugar_de(otro), absoluta))
  # Abierto por su ruta absoluta, la huella es la que se publicaba.
  ruta <- normalizePath(archivo, winslash = "/", mustWork = FALSE)
  expect_identical(
    absoluta,
    .huella_conformidad_dbi(c(
      "SQLiteConnection",
      .sort_por_bytes(c(paste0("dbname=", ruta), "host=NA", "port=NA"))
    ))
  )
  # Dos bases en memoria no tienen nada que las distinga.
  expect_identical(lugar_de(":memory:"), lugar_de(":memory:"))
})

test_that("el lugar se pregunta una vez, y solo en la llamada que lo publica", {
  skip_if_not_installed("RSQLite")
  es_lugar <- function(sql) sum(sql %in% "PRAGMA database_list")
  perfil <- .fcd_perfil(.cp_abrir_sqlite)
  expect_identical(es_lugar(perfil$sql), 0L)
  modelo_fcd <- .cp_modelo()
  medicion <- .cp_correr(
    .cp_abrir_sqlite,
    function(con) medir_dbi(modelo_fcd, con, fecha = .CP_FECHA)
  )
  expect_identical(es_lugar(medicion$sql), 0L)
  conformidad <- .cp_correr(
    .cp_abrir_sqlite, function(con) conformidad_dbi(con, modelo_fcd)
  )
  expect_false(inherits(conformidad$valor, "condition"))
  expect_identical(es_lugar(conformidad$sql), 1L)
  # Dentro de una corrida, pedirlo otra vez no lo vuelve a preguntar.
  dos_veces <- .cp_correr(.cp_abrir_sqlite, function(con) {
    .con_corrida_dbi(con, "verificar la conformidad", {
      c(.lugar_conexion_dbi(con), .lugar_conexion_dbi(con))
    })
  })
  expect_identical(dos_veces$valor[[1L]], dos_veces$valor[[2L]])
  expect_identical(es_lugar(dos_veces$sql), 1L)
})

# ---- Permiso, interrupcion y conciliacion --------------------------------

test_that("un permiso negado se reconoce por lo que el motor dijo", {
  permisos <- c(
    "SELECT command denied to user 'lupa'@'localhost' for table `lupa`.`x`",
    "SELECT command denied to user 'lupa'@'localhost' for column 'secreto' in table 'x'",
    "execute command denied to user 'lupa'@'localhost' for routine 'lupa.f'",
    "Access denied; you need (at least one of) the PROCESS privilege(s) for this operation",
    paste0("Se deneg", .CP_O, " el permiso SELECT en el objeto 'x', base de datos 'lupa'.")
  )
  for (motivo in permisos) {
    expect_match(
      .mensaje_tabla_inaccesible_dbi(motivo), "no tiene permiso para verla: el",
      fixed = TRUE, info = motivo
    )
    expect_true(.error_permiso_clave(motivo), info = motivo)
  }
  # Un inicio de sesion fallido y un objeto que no existe no son un permiso.
  for (motivo in c(
    "Access denied for user 'lupa'@'localhost' (using password: YES)",
    "ORA-00942: table or view does not exist"
  )) {
    expect_match(
      .mensaje_tabla_inaccesible_dbi(motivo),
      paste0("no existe en la conexi", .CP_O, "n DBI, o la credencial"),
      fixed = TRUE, info = motivo
    )
    expect_false(.error_permiso_clave(motivo), info = motivo)
  }
  # Lo que dijo el reconocedor vale aunque el texto no se entienda.
  expect_match(
    .mensaje_tabla_inaccesible_dbi("texto en otro idioma", "permiso"),
    "no tiene permiso para verla: el", fixed = TRUE
  )
  expect_true(.error_permiso_clave("texto en otro idioma", "permiso"))
  expect_false(.error_permiso_clave("texto en otro idioma", "rechazada"))
  expect_false(.error_permiso_clave(NA_character_))
})

test_that("una interrupcion al comprobar la tabla sale como interrupcion", {
  skip_if_not_installed("RSQLite")
  interrupcion <- simpleError(gettext("reached elapsed time limit", domain = "R"))
  r <- .cp_correr(
    .cp_abrir_sqlite,
    function(con) plan_perfilado_dbi(con, "lupa_t"),
    function(est, operacion, conexion, sql) {
      if (identical(operacion, "dbExistsTable")) est$lanzar <- interrupcion
    }
  )
  expect_s3_class(r$valor, "error")
  expect_false(inherits(r$valor, "lupa_error_dbi"))
  expect_identical(conditionMessage(r$valor), conditionMessage(interrupcion))
  # Y no es una conexion perdida: la conexion sigue sirviendo.
  despues <- .cp_correr(
    .cp_abrir_sqlite, function(con) plan_perfilado_dbi(con, "lupa_t")
  )
  expect_s3_class(despues$valor, "plan_perfilado_dbi")
})

test_that("una tabla vacia se concilia con el plan de la corrida, tambien en DuckDB", {
  skip_if_not_installed("duckdb")
  con <- suppressMessages(DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_vacia (id INTEGER, x DOUBLE, s VARCHAR)")
  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "lupa_vacia", fecha = .CP_FECHA)
  ))
  consultas <- perfil$resumen_tabla$meta$consultas
  expect_identical(consultas$conciliacion$estado, "conciliado")
  expect_equal(as.numeric(consultas$conciliacion$anunciadas),
               as.numeric(consultas$emitidas))
})

test_that("una tabla vacia se concilia con el plan de la corrida, tambien en SQLite", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE lupa_vacia (id INTEGER, x REAL, s TEXT)")
  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "lupa_vacia", fecha = .CP_FECHA)
  ))
  conciliacion <- perfil$resumen_tabla$meta$consultas$conciliacion
  expect_identical(conciliacion$estado, "conciliado")
  expect_equal(as.numeric(conciliacion$anunciadas),
               as.numeric(conciliacion$emitidas))
  previo <- conciliacion$plan_previo
  expect_identical(previo$estado, "desfasado")
  expect_gt(as.numeric(previo$anunciadas), as.numeric(previo$emitidas))
  expect_true(grepl(
    paste0("se anunci", .CP_O, " antes de comprobar que la tabla estaba vac",
           .CP_I, "a. La conciliaci", .CP_O, "n usa"),
    previo$motivo, fixed = TRUE
  ))
})

test_that("el plan rehecho para una tabla sin valores no cuenta el desvio que no se mando", {
  # PostgreSQL y SQL Server calculan el desvio en una sentencia aparte de la de
  # los agregados; sin ningun valor no nulo no se manda. El plan de la corrida
  # la seguia contando: la cuenta de una tabla vacia quedaba desfasada en una
  # consulta, y la diferencia se atribuia a los portones.
  plan <- data.frame(
    clase_consulta = c(
      "portones (esquema y sondas)",
      "agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio)",
      "distintos por lotes (clase separada)"
    ),
    n_consultas = c(2, 2, 1), alcance = "una vez", n_consultas_max = c(2, 2, 1),
    stringsAsFactors = FALSE
  )
  presupuesto <- .presupuesto_dbi(Inf, instrumentar = FALSE)
  # Dos portones, la sentencia de los agregados y la de los distintos: la del
  # desvio no esta.
  for (etapa in c("portones", "esquema", "basicos", "conteos")) {
    .contar_dbi(presupuesto, etapa = etapa)
  }
  ejecutado <- function(sin_valores) {
    .plan_ejecutado_consultas_dbi(
      plan, presupuesto, consultas_antes_resumen = 2L,
      sin_valores = sin_valores
    )$n_consultas
  }
  expect_equal(ejecutado(TRUE), c(2, 1, 1))
  expect_equal(sum(ejecutado(TRUE)), as.numeric(presupuesto$usadas))
  # Con valores la clase no se toca: si falta una consulta, es una falla.
  expect_equal(ejecutado(FALSE), c(2, 2, 1))
})

test_that("el plan rehecho para una tabla sin valores cuenta las medianas que se mandaron", {
  # Sin ningun valor no nulo las medianas no hacen falta. Segun el motor no se
  # manda ninguna, o solo la consolidada: el plan de la corrida las contaba
  # todas salvo en la tabla vacia con el bloque de muestra pedido.
  plan <- data.frame(
    clase_consulta = c(
      "portones (esquema y sondas)",
      "agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio)",
      "mediana (orden total + limite/salto)"
    ),
    n_consultas = c(2, 1, 2), alcance = "una vez", n_consultas_max = c(2, 1, 2),
    stringsAsFactors = FALSE
  )
  ejecutado <- function(etapas, sin_valores) {
    presupuesto <- .presupuesto_dbi(Inf, instrumentar = FALSE)
    for (etapa in etapas) .contar_dbi(presupuesto, etapa = etapa)
    .plan_ejecutado_consultas_dbi(
      plan, presupuesto, consultas_antes_resumen = 2L,
      sin_valores = sin_valores
    )$n_consultas
  }
  sin_medianas <- c("portones", "esquema", "basicos")
  con_la_consolidada <- c(sin_medianas, "medianas_consolidadas")
  expect_equal(ejecutado(sin_medianas, TRUE), c(2, 1, 0))
  expect_equal(ejecutado(con_la_consolidada, TRUE), c(2, 1, 1))
  # Nunca sube: con las dos mandadas queda lo anunciado.
  expect_equal(ejecutado(c(con_la_consolidada, "mediana"), TRUE), c(2, 1, 2))
  # Con valores la clase no se toca: si falta una consulta, es una diferencia.
  expect_equal(ejecutado(sin_medianas, FALSE), c(2, 1, 2))
})

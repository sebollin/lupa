# Arreglos de la via DBI medidos contra bases reales: cuatro consultas-porton
# fatales, el costo no acotado, la caja del alias y el resumen que nunca pasaba
# por la proteccion de datos personales.
#
# La forma de probarlo es un backend simulado: una subclase de la conexion de
# RSQLite que intercepta las consultas y se porta como se portan los motores
# que rompieron el paquete. Rechazar `LIMIT` es SQL Server; plegar los alias a
# mayusculas es Oracle, DB2, Firebird y Snowflake; rechazar una columna es un
# LOB que el driver no materializa o un permiso a nivel de columna.

.dbi_de_prueba <- requireNamespace("DBI", quietly = TRUE) &&
  requireNamespace("RSQLite", quietly = TRUE)

if (.dbi_de_prueba) {
  library(DBI)

  .juguete <- new.env(parent = emptyenv())
  .juguete$modo <- "normal"
  .juguete$sql <- character()
  .juguete$dentro <- FALSE
  .juguete$columna_mala <- "zzz_ninguna"

  .juguete_reiniciar <- function(modo = "normal", columna_mala = "zzz_ninguna") {
    .juguete$modo <- modo
    .juguete$sql <- character()
    .juguete$dentro <- FALSE
    .juguete$columna_mala <- columna_mala
    invisible(NULL)
  }
  .juguete_consultas <- function() .juguete$sql

  .juguete_sabotear <- function(statement) {
    modo <- .juguete$modo
    if (grepl("sqlite_master|sqlite_temp_master|pragma", statement,
              ignore.case = TRUE)) {
      return(invisible(NULL))
    }
    if (modo %in% c("sin_limite", "sin_limite_final") &&
        grepl("LIMIT", statement, ignore.case = TRUE)) {
      if (identical(modo, "sin_limite_final") &&
          grepl("WHERE|COUNT|AVG|LIMIT 0", statement)) {
        return(invisible(NULL))
      }
      stop("Incorrect syntax near 'LIMIT'.", call. = FALSE)
    }
    if (identical(modo, "veneno") && !grepl("LIMIT 0$", statement) &&
        (grepl(.juguete$columna_mala, statement, fixed = TRUE) ||
         grepl("SELECT\\s+\\*", statement))) {
      stop(
        "ORA-00932: inconsistent datatypes: expected - got LONG (column ",
        .juguete$columna_mala, ")", call. = FALSE
      )
    }
    invisible(NULL)
  }

  .juguete_renombrar <- function(datos, statement) {
    if (!is.data.frame(datos) || !length(names(datos))) return(datos)
    modo <- .juguete$modo
    if (identical(modo, "mayusculas")) {
      names(datos) <- toupper(names(datos))
    } else if (identical(modo, "moda_sin_nombre") &&
               grepl("GROUP BY", statement)) {
      names(datos) <- paste0("col_", seq_along(datos))
    } else if (identical(modo, "basicos_sin_nombre") &&
               grepl("n_ceros", statement)) {
      names(datos) <- paste0("col_", seq_along(datos))
    }
    datos
  }

  setClass("ConexionJugueteLupa", contains = "SQLiteConnection")
  setClass("ResultadoJugueteLupa", contains = "SQLiteResult")

  .juguete_envolver <- function(con) {
    obj <- methods::new("ConexionJugueteLupa")
    for (ranura in methods::slotNames(con)) {
      methods::slot(obj, ranura) <- methods::slot(con, ranura)
    }
    obj
  }

  setMethod(
    "dbGetQuery", c("ConexionJugueteLupa", "character"),
    function(conn, statement, ...) {
      if (!.juguete$dentro) {
        .juguete$sql <- c(.juguete$sql, statement)
        .juguete_sabotear(statement)
      }
      previo <- .juguete$dentro
      .juguete$dentro <- TRUE
      on.exit(.juguete$dentro <- previo, add = TRUE)
      .juguete_renombrar(callNextMethod(), statement)
    }
  )

  setMethod(
    "dbSendQuery", c("ConexionJugueteLupa", "character"),
    function(conn, statement, ...) {
      if (!.juguete$dentro) {
        .juguete$sql <- c(.juguete$sql, statement)
        .juguete_sabotear(statement)
      }
      resultado <- callNextMethod()
      if (identical(.juguete$modo, "normal")) return(resultado)
      envuelto <- methods::new("ResultadoJugueteLupa")
      for (ranura in methods::slotNames(resultado)) {
        methods::slot(envuelto, ranura) <- methods::slot(resultado, ranura)
      }
      attr(envuelto, "sql_lupa") <- statement
      envuelto
    }
  )

  setMethod("dbFetch", "ResultadoJugueteLupa", function(res, n = -1, ...) {
    statement <- attr(res, "sql_lupa", exact = TRUE)
    if (is.null(statement)) statement <- ""
    .juguete_renombrar(callNextMethod(), statement)
  })

  # El metodo por omision de DBI para `dbListFields()` emite
  # `SELECT * ... LIMIT 0`: un motor sin LIMIT muere ahi, en la primera
  # consulta, con el texto crudo del driver.
  setMethod(
    "dbListFields", c("ConexionJugueteLupa", "character"),
    function(conn, name, ...) {
      sql <- paste0(
        "SELECT * FROM ", dbQuoteIdentifier(conn, name), " LIMIT 0"
      )
      if (!.juguete$dentro) {
        .juguete$sql <- c(.juguete$sql, sql)
        .juguete_sabotear(sql)
      }
      previo <- .juguete$dentro
      .juguete$dentro <- TRUE
      on.exit(.juguete$dentro <- previo, add = TRUE)
      callNextMethod()
    }
  )
}

.tabla_juguete <- function(n = 12L) {
  data.frame(
    id = seq_len(n),
    monto = rep(c(10, 20, 20, 30, NA, 50), length.out = n),
    texto = rep(c("a", "b", "b", "c", NA, "e"), length.out = n),
    stringsAsFactors = FALSE
  )
}

.conexion_juguete <- function(datos = .tabla_juguete(), tabla = "juguete",
                              modo = "normal", columna_mala = "zzz_ninguna") {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(.dbi_de_prueba, "Falta el backend simulado de DBI.")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, tabla, datos)
  envuelta <- .juguete_envolver(con)
  .juguete_reiniciar(modo, columna_mala)
  list(cruda = con, juguete = envuelta)
}

.argumentos_livianos <- list(
  analizar_dependencias = FALSE,
  casi_duplicados_vocabulario = FALSE
)

.perfilar_juguete <- function(conexion, tabla = "juguete", ...) {
  do.call(
    perfilar_dbi,
    c(list(conexion = conexion, tabla = tabla), list(...), .argumentos_livianos)
  )
}

# ---- 2.39: las cuatro consultas-porton -----------------------------------

test_that("un motor que rechaza LIMIT en la muestra no se lleva el resumen", {
  bases <- .conexion_juguete(modo = "sin_limite_final")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- NULL
  expect_warning(
    resultado <- .perfilar_juguete(
      bases$juguete, muestra = 5L, orden_muestra = "id"
    ),
    class = "lupa_muestra_dbi_no_disponible"
  )
  expect_s3_class(resultado, "perfil_dbi")
  # El resumen completo se devuelve: es lo que ya estaba medido.
  expect_equal(nrow(resultado$resumen_tabla$columnas), 3L)
  expect_gt(sum(resultado$resumen_tabla$sql$estado == "calculado"), 20L)
  expect_identical(resultado$resumen_tabla$meta$alcance, "tabla_completa")
  expect_null(resultado$perfil_muestra)

  cobertura <- resultado$resumen_tabla$cobertura
  fila <- cobertura[cobertura$bloque == "perfil_muestra", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$estado, "no_disponible")
  expect_match(fila$motivo, "LIMIT")
  expect_true(nzchar(fila$como_resolverlo))
})

test_that("cada porton de la via DBI senala con clase de condicion propia", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  expect_error(
    perfilar_dbi(bases$juguete, "no_existe"),
    class = "lupa_error_tabla_dbi"
  )
  expect_error(
    perfilar_dbi(bases$juguete, "juguete", orden_muestra = "inexistente"),
    class = "lupa_error_argumento_dbi"
  )
  expect_error(
    perfilar_dbi(list(a = 1), "juguete"),
    class = "lupa_error_conexion_dbi"
  )
  expect_error(
    perfilar_dbi(bases$juguete, "juguete", metricas = "inventada"),
    class = "lupa_error_argumento_dbi"
  )
  # Todas se pueden rescatar por la clase madre.
  rescate <- tryCatch(
    perfilar_dbi(bases$juguete, "no_existe"),
    lupa_error_dbi = function(e) "rescatado"
  )
  expect_identical(rescate, "rescatado")
})

test_that("dbListFields fallido degrada en vez de tirar el error crudo", {
  # Un driver que no sobrescribe `dbListFields()` cae en el metodo por omision
  # de DBI, que emite `SELECT * ... LIMIT 0`.
  bases <- .conexion_juguete(modo = "sin_limite")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  expect_s3_class(resultado, "perfil_dbi")
  expect_equal(nrow(resultado$resumen_tabla$columnas), 3L)
  cobertura <- resultado$resumen_tabla$cobertura
  expect_true(any(grepl("dbListFields", cobertura$motivo)))
  # Y la muestra igual se obtiene, por la via portable.
  expect_s3_class(resultado$perfil_muestra, "perfil")
})

test_that("el esquema y la muestra enumeran columnas en vez de pedir SELECT *", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, muestra = 5L, orden_muestra = "id"
  )
  esquema <- resultado$resumen_tabla$meta$sql_esquema
  expect_false(grepl("SELECT \\*", esquema))
  expect_match(esquema, "id")
  muestra <- resultado$perfil_muestra$meta$origen_dbi$muestreo$sql_muestra
  expect_false(grepl("SELECT \\*", muestra))
  expect_match(muestra, "ORDER BY")
})

test_that("una columna que el motor rechaza no se lleva puesto el resumen", {
  datos <- data.frame(
    id = 1:20, monto = as.numeric(1:20), texto = rep(letters[1:4], 5L),
    doc_largo = paste0("lob-", 1:20), stringsAsFactors = FALSE
  )
  bases <- .conexion_juguete(
    datos, tabla = "docs", modo = "veneno", columna_mala = "doc_largo"
  )
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, tabla = "docs", muestra = 10L, orden_muestra = "id"
  )
  columnas <- resultado$resumen_tabla$columnas
  # Las cuatro siguen enumeradas: la que no se pudo leer no desaparece.
  expect_equal(nrow(columnas), 4L)
  sanas <- columnas[columnas$columna != "doc_largo", , drop = FALSE]
  expect_true(all(sanas$n_validos == 20))
  mala <- columnas[columnas$columna == "doc_largo", , drop = FALSE]
  # Nunca cero por ausencia de medicion.
  expect_true(is.na(mala$n_validos))
  expect_true(is.na(mala$n_distintos))

  registros <- resultado$resumen_tabla$sql
  estados <- registros$estado[registros$columna == "doc_largo"]
  expect_true(all(estados %in% c("calculado", "no_disponible")))
  expect_gt(sum(estados == "no_disponible"), 10L)
  cobertura <- resultado$resumen_tabla$cobertura
  expect_true(any(cobertura$elemento == "doc_largo"))
  # Y la muestra se lee con las columnas que si se pueden leer.
  expect_s3_class(resultado$perfil_muestra, "perfil")
  expect_setequal(
    resultado$perfil_muestra$meta$origen_dbi$muestreo$columnas_leidas,
    c("id", "monto", "texto")
  )
})

test_that("el adaptador de dialecto escribe cada capacidad como corresponde", {
  dialectos <- lupa:::.dialectos_dbi()
  expect_identical(
    dialectos$limit$limitar("SELECT a FROM t", 5L, 0),
    "SELECT a FROM t LIMIT 5"
  )
  expect_identical(
    dialectos$limit$limitar("SELECT a FROM t", 2L, 19),
    "SELECT a FROM t LIMIT 2 OFFSET 19"
  )
  expect_identical(
    dialectos$top$limitar("SELECT a FROM t", 5L, 0),
    "SELECT TOP (5) a FROM t"
  )
  expect_match(
    dialectos$top$limitar("SELECT a FROM t ORDER BY a", 2L, 19),
    "OFFSET 19 ROWS FETCH NEXT 2 ROWS ONLY", fixed = TRUE
  )
  expect_match(
    dialectos$fetch_first$limitar("SELECT a FROM t ORDER BY a", 2L, 19),
    "OFFSET 19 ROWS FETCH FIRST 2 ROWS ONLY", fixed = TRUE
  )
  expect_match(
    dialectos$rownum$limitar("SELECT a FROM t", 5L, 0), "ROWNUM <= 5",
    fixed = TRUE
  )
  # Oracle no acepta AS para el alias de una subconsulta.
  expect_identical(dialectos$fetch_first$alias_tabla("lupa_mediana"), " lupa_mediana")
  expect_identical(dialectos$limit$alias_tabla("lupa_mediana"), " AS lupa_mediana")
  # La via portable no expresa recorte en SQL: se acota en el cliente.
  expect_null(dialectos$portable$limitar("SELECT a FROM t", 5L, 0))
  expect_null(dialectos$rownum$limitar("SELECT a FROM t", 5L, 19))
})

test_that("sin dialecto de limite la mediana se declara en vez de traer la tabla", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, muestra = 5L, orden_muestra = "id", dialecto = "portable"
  )
  expect_identical(resultado$resumen_tabla$meta$dialecto$nombre, "portable")
  medianas <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica == "mediana", , drop = FALSE
  ]
  fila_monto <- medianas[medianas$columna == "monto", , drop = FALSE]
  expect_identical(fila_monto$estado, "no_disponible")
  expect_match(fila_monto$motivo, "salto de filas")
  expect_true(is.na(
    resultado$resumen_tabla$columnas$mediana[
      resultado$resumen_tabla$columnas$columna == "monto"
    ]
  ))
  # Y el resto de las metricas sigue viniendo entero.
  expect_false(is.na(
    resultado$resumen_tabla$columnas$media[
      resultado$resumen_tabla$columnas$columna == "monto"
    ]
  ))
})

# ---- 2.40: el costo -------------------------------------------------------

test_that("modo y metricas acotan las consultas que se emiten", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  contar <- function(...) {
    .juguete_reiniciar("normal")
    .perfilar_juguete(bases$juguete, muestra = 5L, orden_muestra = "id", ...)
    length(.juguete_consultas())
  }
  exacto <- contar()
  seguro <- contar(modo = "seguro")
  conteos <- contar(modo = "conteos")
  solo_validos <- contar(metricas = "validos")

  expect_gt(exacto, seguro)
  expect_gt(seguro, conteos)
  expect_equal(conteos, solo_validos)
})

test_that("lo que no entra en el presupuesto se declara, y nunca queda en cero", {
  bases <- .conexion_juguete(.tabla_juguete(20L))
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, muestra = 5L, orden_muestra = "id", max_consultas = 8
  )
  expect_lte(resultado$resumen_tabla$meta$consultas$emitidas, 8)
  expect_true(resultado$resumen_tabla$meta$consultas$agotado)
  registros <- resultado$resumen_tabla$sql
  agotadas <- registros[grepl("presupuesto", registros$motivo), , drop = FALSE]
  expect_gt(nrow(agotadas), 0L)
  expect_true(all(agotadas$estado == "no_disponible"))
  cobertura <- resultado$resumen_tabla$cobertura
  expect_true(any(cobertura$estado == "presupuesto_agotado"))
  # Ninguna metrica no medida sale como cero.
  no_medidas <- registros$columna[
    registros$metrica == "n_distintos" & registros$estado != "calculado"
  ]
  columnas <- resultado$resumen_tabla$columnas
  expect_true(all(is.na(columnas$n_distintos[columnas$columna %in% no_medidas])))
})

test_that("el plan previo dice cuantas consultas se van a emitir", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  .juguete_reiniciar("normal")
  plan <- lupa:::plan_perfilado_dbi(
    bases$juguete, "juguete", muestra = 5L, orden_muestra = "id"
  )
  emitidas_al_planificar <- length(.juguete_consultas())
  expect_s3_class(plan, "data.frame")
  expect_true(all(c("clase_consulta", "n_consultas", "alcance") %in% names(plan)))
  expect_equal(attr(plan, "columnas"), 3L)
  expect_gt(attr(plan, "total"), emitidas_al_planificar)

  .juguete_reiniciar("normal")
  resultado <- .perfilar_juguete(
    bases$juguete, muestra = 5L, orden_muestra = "id"
  )
  expect_equal(length(.juguete_consultas()), attr(plan, "total"))
  expect_equal(
    resultado$resumen_tabla$meta$consultas$emitidas, attr(plan, "total")
  )
  # Planificar cuesta mucho menos que medir.
  expect_lt(emitidas_al_planificar, attr(plan, "total"))
})

# ---- 2.41: la caja del alias ---------------------------------------------

test_that("un motor que pliega los alias a mayusculas no inventa un motivo falso", {
  bases <- .conexion_juguete(modo = "mayusculas")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  columnas <- resultado$resumen_tabla$columnas
  monto <- columnas[columnas$columna == "MONTO", , drop = FALSE]
  if (!nrow(monto)) monto <- columnas[columnas$columna == "monto", , drop = FALSE]
  expect_equal(nrow(monto), 1L)
  expect_equal(monto$n_validos, 10)
  expect_equal(monto$moda, "20")
  expect_equal(monto$frecuencia_moda, 4)
  expect_equal(monto$minimo, 10)
  expect_equal(monto$maximo, 50)
  expect_false(is.na(monto$media))
  expect_true(all(resultado$resumen_tabla$sql$estado != "no_disponible"))
})

test_that("la moda nunca sale calculada con el valor vacio", {
  bases <- .conexion_juguete(modo = "moda_sin_nombre")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  registros <- resultado$resumen_tabla$sql
  modas <- registros[registros$metrica == "moda", , drop = FALSE]
  expect_true(all(modas$estado == "no_disponible"))
  expect_true(all(grepl("valor", modas$motivo)))
  expect_true(all(is.na(resultado$resumen_tabla$columnas$moda)))
  expect_true(all(is.na(resultado$resumen_tabla$columnas$frecuencia_moda)))
})

test_that("un agregado sin nombre reconocible no revienta el ensamblado", {
  bases <- .conexion_juguete(modo = "basicos_sin_nombre")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  columnas <- resultado$resumen_tabla$columnas
  expect_equal(nrow(columnas), 3L)
  # `numeric(0)` no llega ni a data.frame() ni a un `if`.
  expect_equal(nrow(unique(columnas["columna"])), 3L)
  expect_true(all(is.na(columnas$media)))
  registros <- resultado$resumen_tabla$sql
  medias <- registros[registros$metrica == "media", , drop = FALSE]
  expect_true(any(medias$estado == "no_disponible"))
  # Y el desvio, que ya no depende de la media traida a R, se calcula igual.
  desvio <- columnas$desvio[columnas$columna == "monto"]
  expect_false(is.na(desvio))
})

test_that("las columnas de orden se resuelven sin distinguir caja", {
  bases <- .conexion_juguete(modo = "mayusculas")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, muestra = 5L, orden_muestra = "id"
  )
  expect_s3_class(resultado, "perfil_dbi")
  muestreo <- resultado$perfil_muestra$meta$origen_dbi$muestreo
  expect_true(muestreo$orden_unico_verificado)
})

# ---- 2.42: datos personales ----------------------------------------------

.padron_de_prueba <- function() {
  data.frame(
    documento_texto = c(
      "39174820", "48213756", "50382614", "57493021", "61847205"
    ),
    ci = c(39174820, 48213756, 50382614, 57493021, 61847205),
    nombre = c(
      "Ada Neri", "Bruno Costa", "Carla Diaz", "Diego Luna", "Eva Rios"
    ),
    correo = paste0(
      c("ada", "bruno", "carla", "diego", "eva"), "@example.invalid"
    ),
    monto = c(10, 20, 20, 30, 40),
    stringsAsFactors = FALSE
  )
}

.valores_crudos_dbi <- function(datos, columnas) {
  valores <- unique(unlist(lapply(datos[columnas], as.character), use.names = FALSE))
  valores[!is.na(valores)]
}

.apariciones_crudas_dbi <- function(x, crudos, ruta = "resumen_tabla") {
  if (is.list(x)) {
    return(unlist(lapply(names(x), function(nombre) {
      .apariciones_crudas_dbi(x[[nombre]], crudos, paste0(ruta, "$", nombre))
    }), use.names = FALSE))
  }
  if (is.atomic(x)) {
    valores <- as.character(x)
    posiciones <- which(!is.na(valores) & valores %in% crudos)
    if (length(posiciones)) {
      return(paste0(ruta, "[", posiciones, "] = ", valores[posiciones]))
    }
  }
  character()
}

test_that("el resumen de tabla completa pasa por la proteccion personal", {
  padron <- .padron_de_prueba()
  bases <- .conexion_juguete(padron, tabla = "padron")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  crudos <- .valores_crudos_dbi(
    padron, c("documento_texto", "ci", "nombre", "correo")
  )

  protegido <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci",
    proteger_datos_personales = TRUE
  )
  crudo <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci",
    proteger_datos_personales = FALSE
  )

  # El control decisivo: los dos bloques ya no son bit a bit identicos.
  expect_false(identical(
    protegido$resumen_tabla$columnas, crudo$resumen_tabla$columnas
  ))
  expect_length(
    .apariciones_crudas_dbi(protegido$resumen_tabla, crudos), 0L
  )
  expect_gt(length(.apariciones_crudas_dbi(crudo$resumen_tabla, crudos)), 0L)

  columnas <- protegido$resumen_tabla$columnas
  personales <- columnas$columna %in%
    c("documento_texto", "ci", "nombre", "correo")
  expect_true(all(columnas$moda[personales] == "[valor protegido]"))
  # Los momentos tambien: la media de las cedulas reconstruye demasiado.
  expect_true(all(is.na(columnas$media[personales])))
  expect_true(all(is.na(columnas$minimo[personales])))
  expect_true(all(is.na(columnas$mediana[personales])))
  # Y la columna que no es personal conserva todo.
  expect_false(is.na(columnas$media[columnas$columna == "monto"]))
  expect_true(protegido$resumen_tabla$meta$proteccion_personal$aplicada)
})

test_that("el SQL guardado no incrusta ningun valor derivado de los datos", {
  padron <- .padron_de_prueba()
  bases <- .conexion_juguete(padron, tabla = "padron")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci"
  )
  registros <- resultado$resumen_tabla$sql
  media_observada <- mean(padron$ci)
  expect_false(any(grepl(
    format(media_observada, scientific = FALSE), registros$sql, fixed = TRUE
  )))
  desvio <- registros$sql[
    registros$columna == "ci" & registros$metrica == "desvio"
  ]
  ## Lo que importa no es que forma se uso, sino que el SQL guardado no lleve
  ## ningun valor derivado de los datos. La forma nativa del motor lo cumple sin
  ## necesidad de la subconsulta, y es la que evita romper SQL Server.
  expect_false(grepl("[0-9]{3,}\\.[0-9]{2,}", desvio))
  # Los datos de conexion tampoco quedan a la vista.
  informacion <- resultado$resumen_tabla$meta$motor$informacion_dbi
  expect_identical(informacion$dbname, "[dato de conexion protegido]")
})

test_that("print.perfil_dbi no imprime ningun valor de celda", {
  padron <- .padron_de_prueba()
  bases <- .conexion_juguete(padron, tabla = "padron")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  crudos <- .valores_crudos_dbi(
    padron, c("documento_texto", "ci", "nombre", "correo")
  )

  resultado <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci"
  )
  salida <- c(
    capture.output(lupa:::print.perfil_dbi(resultado)),
    capture.output(lupa:::print.perfil_dbi(resultado), type = "message")
  )
  expect_gt(length(salida), 2L)
  expect_true(any(grepl("padron", salida)))
  expect_false(any(vapply(
    crudos, function(valor) any(grepl(valor, salida, fixed = TRUE)),
    logical(1L)
  )))
  expect_identical(
    invisible(lupa:::print.perfil_dbi(resultado)), resultado
  )
})

test_that("incluir_valores = FALSE no emite las consultas de valores", {
  padron <- .padron_de_prueba()
  bases <- .conexion_juguete(padron, tabla = "padron")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  .juguete_reiniciar("normal")
  con_valores <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci"
  )
  emitidas_con <- length(.juguete_consultas())
  .juguete_reiniciar("normal")
  sin_valores <- .perfilar_juguete(
    bases$juguete, tabla = "padron", muestra = 5L, orden_muestra = "ci",
    incluir_valores = FALSE
  )
  emitidas_sin <- length(.juguete_consultas())

  expect_lt(emitidas_sin, emitidas_con)
  # La consulta de la moda -la que agrupa y ordena la tabla entera- no se emite.
  expect_false(any(grepl("frecuencia", .juguete_consultas(), fixed = TRUE)))
  columnas <- sin_valores$resumen_tabla$columnas
  expect_true(all(is.na(columnas$moda)))
  expect_true(all(is.na(columnas$minimo)))
  expect_true(all(is.na(columnas$mediana)))
  registros <- sin_valores$resumen_tabla$sql
  expect_true(any(registros$estado == "omitido_por_privacidad"))
  # La media de la columna que no es personal se sigue midiendo.
  expect_false(is.na(columnas$media[columnas$columna == "monto"]))
  expect_equal(nrow(con_valores$resumen_tabla$columnas), nrow(columnas))
})

# ---- 2.52: defectos menores de la misma ronda ----------------------------

test_that("una conexion que no es DBI recibe el mensaje del paquete", {
  # Sin DBI el paquete se queja de que falta DBI, no de la clase del argumento:
  # es otro mensaje y otra comprobacion.
  skip_if_not_installed("DBI")
  expect_error(
    perfilar_dbi(list(a = 1), "tabla"),
    "no hereda de `DBIConnection`", fixed = TRUE
  )
})

test_that("dbExistsTable en FALSE no afirma que la tabla no existe", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  error <- tryCatch(
    perfilar_dbi(bases$juguete, "no_existe"), error = function(e) e
  )
  expect_s3_class(error, "lupa_error_tabla_dbi")
  expect_match(conditionMessage(error), "permiso")
})

test_that("el tipo declarado por el motor habilita los agregados numericos", {
  expect_true(lupa:::.es_numerico_dbi(1.5))
  expect_true(lupa:::.es_numerico_dbi("texto", "DECIMAL(10,2)"))
  expect_true(lupa:::.es_numerico_dbi("texto", "NUMBER"))
  expect_true(lupa:::.es_numerico_dbi("texto", "BIGINT"))
  expect_false(lupa:::.es_numerico_dbi("texto", "VARCHAR(20)"))
  expect_false(lupa:::.es_numerico_dbi("texto"))
  expect_false(lupa:::.es_numerico_dbi(as.Date("2026-01-01")))
  expect_false(lupa:::.es_numerico_dbi(list(raw(1L)), "BLOB"))
})

test_that("la media se pide casteada para no truncar en semantica entera", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  registros <- resultado$resumen_tabla$sql
  medias <- registros$sql[
    registros$metrica == "media" & registros$estado == "calculado"
  ]
  expect_gt(length(medias), 0L)
  expect_true(all(grepl("* 1.0", medias, fixed = TRUE)))
})

test_that("un conteo por encima de la exactitud del doble se declara", {
  bases <- .conexion_juguete()
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  expect_true(resultado$resumen_tabla$meta$conteo_exacto)
})

# ---- 2.51: el reflejo de todo-o-nada -------------------------------------

test_that("ninguna metrica no medida sale como cero", {
  bases <- .conexion_juguete(modo = "moda_sin_nombre")
  on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)

  resultado <- .perfilar_juguete(bases$juguete, muestra = 5L)
  registros <- resultado$resumen_tabla$sql
  columnas <- resultado$resumen_tabla$columnas
  numericas <- setdiff(names(columnas), c("columna", "moda"))
  # Se fija que haya algo que mirar antes de mirarlo. Con `if (!length(fallidas))
  # next` y una sola metrica fallida, trece de las catorce vueltas no aseveraban
  # nada: si la etiqueta `no_disponible` se renombrara, o si el modo del juguete
  # dejara de romper esa metrica, el bloque entero quedaba sin una sola
  # asercion y seguia en verde.
  expect_gt(sum(registros$estado == "no_disponible"), 0L)
  ejercitadas <- 0L
  for (metrica in numericas) {
    fallidas <- registros$columna[
      registros$metrica == metrica & registros$estado == "no_disponible"
    ]
    if (!length(fallidas)) next
    ejercitadas <- ejercitadas + 1L
    valores <- columnas[[metrica]][columnas$columna %in% fallidas]
    expect_true(all(is.na(valores)))
    expect_false(any(valores %in% 0))
  }
  # Y que el bucle haya entrado de verdad en alguna vuelta.
  expect_gt(ejercitadas, 0L)
})

test_that("una metrica internamente imposible se declara en vez de publicarse", {
  ## Una refutacion adversarial falseo el resultado del motor: mas valores
  ## distintos que validos, y una frecuencia de moda mayor que las filas. El
  ## paquete los aceptaba como `calculado`. Un numero imposible no es un dato.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(v = c(1, 1, 2, 2, 3)))

  normal <- perfilar_dbi(con, "t", muestra = 10)
  fila <- normal$resumen_tabla$columnas
  expect_lte(fila$n_distintos, fila$n_validos)
  expect_lte(fila$tasa_distintos, 1)
  expect_lte(fila$frecuencia_moda, fila$n_validos)
})

test_that("un nombre calificado con punto se resuelve como en coleccion()", {
  ## Verificado contra PostgreSQL real: `dbExistsTable()` no resuelve el punto,
  ## asi que `coleccion("esquema.tabla")` funcionaba y `perfilar_dbi()` con el
  ## mismo texto decia que la tabla no existe. Dos puertas del mismo paquete
  ## comportandose distinto ante la misma entrada.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  archivo <- tempfile(fileext = ".sqlite")
  on.exit(unlink(archivo), add = TRUE)
  auxiliar <- DBI::dbConnect(RSQLite::SQLite(), archivo)
  DBI::dbWriteTable(auxiliar, "resumen", data.frame(k = 1:20, v = 21:40))
  DBI::dbDisconnect(auxiliar)
  DBI::dbExecute(con, sprintf("ATTACH DATABASE '%s' AS reportes", archivo))

  expect_false(DBI::dbExistsTable(con, "reportes.resumen"))
  por_texto <- perfilar_dbi(con, "reportes.resumen", muestra = 10)
  expect_s3_class(por_texto, "perfil_dbi")
  expect_equal(nrow(por_texto$resumen_tabla$columnas), 2L)

  por_id <- perfilar_dbi(
    con, DBI::Id(schema = "reportes", table = "resumen"), muestra = 10
  )
  expect_equal(
    por_texto$resumen_tabla$columnas$columna,
    por_id$resumen_tabla$columnas$columna
  )

  ## Una tabla que de verdad no existe sigue dando el error, y no afirma
  ## inexistencia: dice que puede ser permiso.
  expect_error(
    perfilar_dbi(con, "nada.de.nada", muestra = 10),
    class = "lupa_error_tabla_dbi"
  )
})

test_that("el desvio usa la funcion nativa del motor antes que el calculo casero", {
  ## Verificado contra SQL Server 2022 real: la forma de dos pasadas pone la
  ## media como subconsulta escalar —para que el SQL guardado no lleve valores
  ## derivados de los datos— y ese motor rechaza una subconsulta dentro de un
  ## agregado. El arreglo de privacidad habia roto la compatibilidad, y ningun
  ## motor simulado podia mostrarlo.
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  valores <- c(4, 8, 15, 16, 23, 42)
  DBI::dbWriteTable(con, "t", data.frame(v = valores))

  perfil <- perfilar_dbi(con, "t", muestra = 10)
  fila <- perfil$resumen_tabla$columnas
  expect_equal(fila$desvio, stats::sd(valores), tolerance = 1e-8)

  sql <- perfil$resumen_tabla$sql
  registro <- sql[sql$metrica == "desvio" & !is.na(sql$sql), ]
  expect_equal(nrow(registro), 1L)
  expect_equal(registro$estado, "calculado")

  ## Y la razon por la que existe la forma con subconsulta se mantiene: el SQL
  ## guardado no puede llevar ningun valor derivado de los datos.
  expect_false(grepl(format(mean(valores)), registro$sql, fixed = TRUE))
})

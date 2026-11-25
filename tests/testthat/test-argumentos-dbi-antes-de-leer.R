# Un argumento malo de la via DBI sale con la clase que promete `?perfilar_dbi`
# (`lupa_error_argumento_dbi`), con el mismo mensaje que da `perfilar()`, y antes
# de pagar lo que no va a servir: lo que no depende de la tabla, antes de mandar
# una sola sentencia; lo que depende del esquema, apenas se lo conoce y antes del
# resumen SQL; un tope de celdas que no deja una fila dice, ademas, el minimo que
# admite la tabla. Y el plan con costo por cardinalidad no le hace llegar a quien
# llama los avisos de R de un reciclado, ni anuncia la moda de una columna que no
# se lee.
testthat::skip_on_cran()

.vad_datos <- function() {
  data.frame(
    id = 1:12,
    x = c(1.5, 2, NA, 4, 5, 6, 7, 8.25, 9, 10, 11, 12),
    t = c("a", "b", "a", NA, "c", "a", "b", "d", "a", "e", NA, "b"),
    k = c(-9, 1, 2, 3, 999, NA, 1, 2, 3, 4, 5, 6),
    l = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, NA, FALSE, TRUE, TRUE,
          FALSE),
    stringsAsFactors = FALSE
  )
}

.vad_motores <- function() {
  motores <- character()
  if (requireNamespace("DBI", quietly = TRUE)) {
    if (requireNamespace("RSQLite", quietly = TRUE)) motores <- c(motores, "sqlite")
    if (requireNamespace("duckdb", quietly = TRUE)) motores <- c(motores, "duckdb")
  }
  if (!length(motores)) testthat::skip("sin RSQLite ni duckdb")
  motores
}

.vad_abrir <- function(motor) {
  con <- if (identical(motor, "sqlite")) {
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  }
  DBI::dbWriteTable(con, "vad_t", .vad_datos())
  con
}

.vad_cerrar <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

# Lo que llega al controlador mientras corre `codigo`: las sentencias y las
# preguntas de metadatos, por los genericos de DBI. Devuelve el valor o el error
# y los avisos.
.vad_enviadas <- function(codigo) {
  est <- new.env(parent = emptyenv())
  est$ops <- character()
  genericas <- c("dbSendQuery", "dbGetQuery", "dbExecute", "dbSendStatement",
                 "dbListFields", "dbExistsTable", "dbListTables")
  for (generica in genericas) {
    anotar <- bquote(assign(
      "ops", c(get("ops", envir = .(est)), .(generica)), envir = .(est)
    ))
    suppressMessages(trace(generica, tracer = anotar,
                           where = asNamespace("DBI"), print = FALSE))
  }
  on.exit(for (generica in genericas) {
    suppressMessages(untrace(generica, where = asNamespace("DBI")))
  }, add = TRUE)
  avisos <- list()
  valor <- tryCatch(
    withCallingHandlers(codigo, warning = function(w) {
      avisos[[length(avisos) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  list(valor = valor, ops = est$ops, avisos = avisos)
}

# El mensaje con que `perfilar()` rechaza lo mismo sobre la tabla en memoria.
.vad_mensaje_perfilar <- function(argumentos) {
  tryCatch(
    suppressWarnings(do.call(perfilar, c(list(.vad_datos()), argumentos))),
    error = conditionMessage
  )
}

.VAD_MAL_FORMADAS <- list(
  sentinelas = list(sentinelas_numericos = "a"),
  sentinelas_lista = list(sentinelas_numericos = list(x = "a")),
  cadenas = list(cadenas_ausencia = 5),
  personales = list(columnas_personales = 5),
  codigo = list(columnas_codigo = 5)
)

test_that("una declaracion mal formada sale antes de la primera sentencia, con la clase de la via", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    for (caso in names(.VAD_MAL_FORMADAS)) {
      argumentos <- .VAD_MAL_FORMADAS[[caso]]
      esperado <- .vad_mensaje_perfilar(argumentos)
      expect_type(esperado, "character")
      for (bloque in c("con_muestra", "solo_agregados")) {
        etiqueta <- paste(motor, caso, bloque)
        r <- .vad_enviadas(do.call(perfilar_dbi, c(
          list(con, "vad_t", bloque_muestra = bloque), argumentos
        )))
        expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
        if (inherits(r$valor, "error")) {
          expect_identical(conditionMessage(r$valor), esperado, label = etiqueta)
        }
        expect_identical(r$ops, character(), label = etiqueta)
      }
    }
    .vad_cerrar(con, motor)
  }
})

test_that("el plan y la coleccion rechazan lo mismo, antes de la primera sentencia", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    col <- coleccion(con, "vad_t")
    for (caso in names(.VAD_MAL_FORMADAS)) {
      argumentos <- .VAD_MAL_FORMADAS[[caso]]
      esperado <- .vad_mensaje_perfilar(argumentos)
      if (caso %in% c("sentinelas", "sentinelas_lista", "cadenas")) {
        r <- .vad_enviadas(do.call(plan_perfilado_dbi, c(list(con, "vad_t"), argumentos)))
        etiqueta <- paste(motor, caso, "plan")
        expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
        if (inherits(r$valor, "error")) {
          expect_identical(conditionMessage(r$valor), esperado, label = etiqueta)
        }
        expect_identical(r$ops, character(), label = etiqueta)
      }
      # En la coleccion la lista de ausencias es por tabla: la de una columna
      # se declara anidada.
      if (identical(caso, "sentinelas_lista")) {
        argumentos <- list(sentinelas_numericos = list(vad_t = list(x = "a")))
      }
      r <- .vad_enviadas(do.call(perfilar_coleccion, c(list(col), argumentos)))
      etiqueta <- paste(motor, caso, "coleccion")
      expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
      if (inherits(r$valor, "error")) {
        expect_identical(conditionMessage(r$valor), esperado, label = etiqueta)
      }
      expect_identical(r$ops, character(), label = etiqueta)
    }
    .vad_cerrar(con, motor)
  }
})

test_that("medir_dbi() mira la forma de columnas_personales antes de la primera sentencia", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "vad_t", "x"))
    r <- .vad_enviadas(medir_dbi(mod, con, columnas_personales = 5))
    expect_true(inherits(r$valor, "error"), label = motor)
    if (inherits(r$valor, "error")) {
      expect_identical(
        conditionMessage(r$valor),
        .vad_mensaje_perfilar(list(columnas_personales = 5)), label = motor
      )
    }
    expect_identical(r$ops, character(), label = motor)
    .vad_cerrar(con, motor)
  }
})

test_that("columnas_codigo inexistente sale con la clase de la via al conocer el esquema", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    esperado <- .vad_mensaje_perfilar(list(columnas_codigo = "no_existe"))
    for (bloque in c("con_muestra", "solo_agregados")) {
      etiqueta <- paste(motor, bloque)
      # Una declaracion que la via ya rechazaba contra el esquema, con su clase:
      # el mismo punto de la corrida.
      referencia <- .vad_enviadas(perfilar_dbi(
        con, "vad_t", columnas_opcionales = "no_existe", bloque_muestra = bloque
      ))
      expect_true(inherits(referencia$valor, "lupa_error_argumento_dbi"))
      r <- .vad_enviadas(perfilar_dbi(
        con, "vad_t", columnas_codigo = "no_existe", bloque_muestra = bloque
      ))
      expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
      if (inherits(r$valor, "error")) {
        expect_identical(conditionMessage(r$valor), esperado, label = etiqueta)
      }
      expect_identical(length(r$ops), length(referencia$ops), label = etiqueta)
    }
    .vad_cerrar(con, motor)
  }
})

test_that("en una coleccion, la tabla sin la columna de codigo dice que argumento fallo, tambien con la proteccion", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    DBI::dbWriteTable(con, "vad_b", data.frame(id = 1:4, s = c("p", "q", "p", "r"),
                                                stringsAsFactors = FALSE))
    col <- coleccion(con, c("vad_t", "vad_b"))
    motivo <- function(proteger) {
      r <- suppressWarnings(perfilar_coleccion(
        col, columnas_codigo = "k", proteger_datos_personales = proteger,
        fecha = as.POSIXct("2026-01-13 12:00:00", tz = "UTC")
      ))
      m <- r$cobertura_coleccion$motivo
      m[grepl("No se pudo perfilar la tabla", m, fixed = TRUE, useBytes = TRUE)]
    }
    abierto <- motivo(FALSE)
    protegido <- motivo(TRUE)
    expect_length(abierto, 1L)
    # Es un error de argumento del paquete, no un mensaje del motor: la
    # proteccion no tacha sus palabras, como no las tacha en los demas errores de
    # la via.
    expect_identical(protegido, abierto, label = motor)
    expect_true(all(grepl("`columnas_codigo`", protegido, fixed = TRUE, useBytes = TRUE)),
                label = motor)
    .vad_cerrar(con, motor)
  }
})

test_that("un tope de celdas que no deja una fila sale al conocer el esquema, antes del resumen", {
  # Cinco columnas: cuatro celdas no dejan ni una fila.
  esperado <- .vad_mensaje_perfilar(list(max_celdas_muestra = 4))
  expect_type(esperado, "character")
  vias <- list(
    tabla = list(),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 50L,
                         orden_muestra = "id"),
    bloques = list(bloque_filas = 4L)
  )
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    for (via in names(vias)) {
      etiqueta <- paste(motor, via)
      referencia <- .vad_enviadas(do.call(perfilar_dbi, c(
        list(con, "vad_t", columnas_opcionales = "no_existe"), vias[[via]]
      )))
      expect_true(inherits(referencia$valor, "lupa_error_argumento_dbi"))
      r <- .vad_enviadas(do.call(perfilar_dbi, c(
        list(con, "vad_t", max_celdas_muestra = 4), vias[[via]]
      )))
      expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
      if (inherits(r$valor, "error")) {
        expect_identical(conditionMessage(r$valor), esperado, label = etiqueta)
      }
      expect_lte(length(r$ops), length(referencia$ops), label = etiqueta)
    }
    # El plan lo rechaza igual.
    r <- .vad_enviadas(plan_perfilado_dbi(con, "vad_t", max_celdas_muestra = 4))
    expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = paste(motor, "plan"))
    # Sin muestra el tope no se usa; y uno que deja una fila se acepta.
    sin_muestra <- .vad_enviadas(perfilar_dbi(
      con, "vad_t", max_celdas_muestra = 4, bloque_muestra = "solo_agregados"
    ))
    expect_s3_class(sin_muestra$valor, "perfil_dbi")
    una_fila <- .vad_enviadas(perfilar_dbi(con, "vad_t", max_celdas_muestra = 5))
    expect_s3_class(una_fila$valor, "perfil_dbi")
    if (inherits(una_fila$valor, "perfil_dbi")) {
      expect_equal(una_fila$valor$perfil_muestra$meta$muestra_efectiva, 1,
                   label = motor)
    }
    .vad_cerrar(con, motor)
  }
})

# El minimo que dice el mensaje de un tope de celdas que no deja una fila.
.vad_minimo_celdas <- function(condicion) {
  if (!inherits(condicion, "error")) return(NA_real_)
  partes <- regmatches(
    conditionMessage(condicion),
    regexec("el m\u00ednimo es ([0-9]+)\\.", conditionMessage(condicion))
  )[[1L]]
  if (length(partes) == 2L) as.numeric(partes[[2L]]) else NA_real_
}

# Recorre lo que el mensaje recomienda: el minimo que dice se acepta, y uno menos
# no. Devuelve el minimo o NA.
.vad_recorrer_minimo <- function(llamar, etiqueta) {
  r <- .vad_enviadas(llamar(1))
  expect_true(inherits(r$valor, "lupa_error_argumento_dbi"), label = etiqueta)
  minimo <- .vad_minimo_celdas(r$valor)
  expect_false(is.na(minimo), label = etiqueta)
  if (is.na(minimo)) return(NA_real_)
  if (minimo > 1) {
    menos <- .vad_enviadas(llamar(minimo - 1))
    expect_true(inherits(menos$valor, "lupa_error_argumento_dbi"), label = etiqueta)
    expect_identical(.vad_minimo_celdas(menos$valor), minimo, label = etiqueta)
  }
  justo <- .vad_enviadas(llamar(minimo))
  expect_s3_class(justo$valor, "perfil_dbi")
  minimo
}

test_that("el mensaje del tope de celdas dice el minimo que admite la tabla, y ese minimo se acepta", {
  vias <- list(
    tabla = list(),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 50L,
                         orden_muestra = "id"),
    bloques = list(bloque_filas = 4L)
  )
  # `perfilar()` en memoria dice lo mismo sobre las mismas columnas.
  expect_identical(
    .vad_minimo_celdas(tryCatch(
      suppressWarnings(perfilar(.vad_datos(), max_celdas_muestra = 1)),
      error = function(e) e
    )),
    5
  )
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    # Tres columnas y ninguna fila: el tope contradice el ancho de la tabla y se
    # rechaza antes de leer, aunque no haya filas que leer.
    DBI::dbWriteTable(con, "vad_vacia", .vad_datos()[0L, c("id", "x", "t")])
    for (via in names(vias)) {
      for (tabla in c("vad_t", "vad_vacia")) {
        etiqueta <- paste(motor, via, tabla)
        minimo <- .vad_recorrer_minimo(function(tope) {
          do.call(perfilar_dbi, c(
            list(con, tabla, max_celdas_muestra = tope), vias[[via]]
          ))
        }, etiqueta)
        expect_identical(minimo, if (identical(tabla, "vad_t")) 5 else 3,
                         label = etiqueta)
      }
    }
    .vad_cerrar(con, motor)
  }
})

test_that("con una columna enmascarada, el minimo es el de la lectura que corre", {
  if (!requireNamespace("RSQLite", quietly = TRUE)) testthat::skip("sin RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "vad_m", .vad_datos()[, c("id", "x", "t")])
  # El catalogo de un motor con enmascaramiento dinamico (no lo hay en la maquina
  # de pruebas): `t` llega enmascarada. La muestra de la tabla la saca antes de
  # leer; la muestra del motor la materializa y la saca despues, asi que cuenta
  # sus celdas.
  local_mocked_bindings(
    .permisos_columnas_dbi = function(conexion, tabla_sql, presupuesto = NULL) {
      .interpretar_permisos_columnas_dbi(
        data.frame(posicion = 1:3, columna = c("id", "x", "t"), legible = 1L),
        data.frame(columna = "t", sin_mascara_columna = 0L,
                   sin_mascara_base = 0L),
        proyectar = TRUE, sql = "catalogo"
      )
    },
    .package = "lupa"
  )
  vias <- list(
    tabla = list(),
    muestra_motor = list(universo = "muestra_motor", muestra_motor = 50L,
                         orden_muestra = "id"),
    bloques = list(bloque_filas = 4L)
  )
  esperado <- c(tabla = 2, muestra_motor = 3, bloques = 2)
  for (via in names(vias)) {
    minimo <- .vad_recorrer_minimo(function(tope) {
      suppressWarnings(do.call(perfilar_dbi, c(
        list(con, "vad_m", max_celdas_muestra = tope), vias[[via]]
      )))
    }, via)
    expect_identical(minimo, esperado[[via]], label = via)
  }
})

test_that("con una columna aislada, el costo por cardinalidad no recicla decisiones", {
  for (motor in .vad_motores()) {
    con <- .vad_abrir(motor)
    # Una columna de pocos valores primero (conserva la moda), un identificador
    # (no la conserva) y una columna que el motor no puede leer, al final.
    DBI::dbWriteTable(con, "vad_base", data.frame(
      g = c("a", "a", "b", "b", "a", "a", "b", "a"), id = 1:8,
      z = c(1, 1, 2, 2, 1, 3, 1, 2),
      t = c("1", "2", "tres", "4", "5", "seis", "7", "8"),
      stringsAsFactors = FALSE
    ))
    rota <- if (identical(motor, "duckdb")) "CAST(t AS INTEGER)" else "json(t)"
    creada <- tryCatch({
      DBI::dbExecute(con, paste0(
        "CREATE VIEW vad_rota AS SELECT g, id, z, ", rota, " AS j FROM vad_base"
      ))
      TRUE
    }, error = function(e) FALSE)
    if (!creada) {
      .vad_cerrar(con, motor)
      next
    }
    r <- .vad_enviadas(perfilar_dbi(
      con, "vad_rota", universo = "muestra_motor", muestra_motor = 50L,
      orden_muestra = "id", bloque_muestra = "solo_agregados",
      politica_costo = "por_cardinalidad", umbral_cardinalidad = 0.5,
      fecha = as.POSIXct("2026-01-13 12:00:00", tz = "UTC")
    ))
    expect_s3_class(r$valor, "perfil_dbi")
    # La columna se aisla: la via lo avisa con su clase.
    expect_true(length(r$avisos) > 0L, label = motor)
    # Ningun aviso de R llega a quien llama: todos son de la via.
    propios <- vapply(r$avisos, inherits, logical(1L), "lupa_aviso_dbi")
    expect_true(all(propios), label = motor)
    if (inherits(r$valor, "perfil_dbi")) {
      # El plan rehecho anuncia las modas que corrieron: la de `g` y la de `z`,
      # no la de la columna aislada.
      conciliacion <- r$valor$resumen_tabla$meta$consultas$conciliacion
      expect_identical(conciliacion$estado, "conciliado", label = motor)
      expect_equal(conciliacion$diferencia, 0, label = motor)
    }
    .vad_cerrar(con, motor)
  }
})

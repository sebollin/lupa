# En uso por omision sobre una tabla sana, la cuenta de consultas concilia sea
# cual sea la forma de las columnas. Una grilla de tablas -de una columna, por
# tipo, forma y cantidad de filas, y mezclas- en SQLite y DuckDB, con el bloque
# de muestra y sin el.
#
# Lo que la grilla destapo: una columna numerica toda nula en una tabla con
# datos dejaba la cuenta desfasada -su mediana no se manda y el plan la
# contaba-, un entero que no cabe exacto en un doble activaba una consulta que
# el plan no contaba, y el motivo publicado citaba el rechazo esperado de un
# sondeo de capacidad, que no tenia nada que ver.
testthat::skip_on_cran()
skip_if_not_installed("DBI")

.ccf_tipo_sql <- function(motor, tipo) {
  switch(tipo,
    doble = "DOUBLE PRECISION", entero = "INTEGER", entero_ancho = "BIGINT",
    decimal = "DECIMAL(12,2)", texto = "VARCHAR(30)", fecha = "DATE",
    fecha_hora = if (identical(motor, "sqlite")) "DATETIME" else "TIMESTAMP",
    logico = "BOOLEAN"
  )
}

# El literal de la fila `i` para un tipo y una forma.
.ccf_literal <- function(motor, tipo, forma, i) {
  # Un infinito entre valores finitos, o solo infinitos: en doble precision.
  if (forma %in% c("con_infinito", "solo_infinitos") &&
      (i == 1L || identical(forma, "solo_infinitos"))) {
    return(if (identical(motor, "sqlite")) {
      if (i %% 2L == 1L) "9e999" else "-9e999"
    } else {
      paste0("CAST('", if (i %% 2L == 1L) "" else "-", "Infinity' AS DOUBLE PRECISION)")
    })
  }
  k <- switch(forma,
    toda_nula = NA, un_valor = if (i == 1L) 1L else NA,
    dos_valores = (i %% 2L) + 1L, constante = 1L, distintos = i,
    con_infinito = i
  )
  if (is.na(k)) return("NULL")
  dia <- format(as.Date("2024-01-01") + k)
  switch(tipo,
    doble = format(k + 0.5, nsmall = 1), entero = as.character(k * 3L),
    # Mas alla de 2^53: no cabe exacto en un doble.
    entero_ancho = sprintf("%.0f", 9007199254740992 + 2 * k),
    decimal = format(k + 0.25, nsmall = 2), texto = paste0("'v", k, "'"),
    fecha = paste0("'", dia, "'"),
    fecha_hora = paste0("'", dia, " 10:00:", sprintf("%02d", k), "'"),
    logico = if (identical(motor, "sqlite")) {
      as.character(k %% 2L)
    } else if (k %% 2L == 1L) "TRUE" else "FALSE"
  )
}

.ccf_formas <- function(tipo, n) {
  formas <- if (n == 1L) {
    c("toda_nula", "un_valor")
  } else if (n == 2L) {
    c("toda_nula", "un_valor", "dos_valores", "constante")
  } else {
    c("toda_nula", "un_valor", "dos_valores", "constante", "distintos")
  }
  if (identical(tipo, "logico")) setdiff(formas, "distintos") else formas
}

# Cada tabla: la cantidad de filas y sus columnas, cada una un tipo y una forma.
.ccf_grilla <- function() {
  tablas <- list()
  tipos <- c("doble", "entero", "entero_ancho", "decimal", "texto", "fecha",
             "fecha_hora", "logico")
  for (tipo in tipos) {
    for (n in c(1L, 2L, 12L)) {
      for (forma in .ccf_formas(tipo, n)) {
        tablas[[paste(tipo, forma, n, sep = ".")]] <- list(
          n = n, columnas = list(c(tipo, forma))
        )
      }
    }
  }
  for (n in c(2L, 12L)) {
    varios <- if (n == 2L) "dos_valores" else "distintos"
    mezclas <- list(
      todos_los_tipos = list(
        c("doble", varios), c("entero", varios), c("entero_ancho", varios),
        c("decimal", varios), c("texto", varios), c("fecha", varios),
        c("fecha_hora", varios), c("logico", "dos_valores")
      ),
      una_numerica_nula = list(
        c("entero", varios), c("doble", "toda_nula"), c("texto", "dos_valores")
      ),
      dos_numericas_una_nula = list(c("doble", "toda_nula"), c("doble", varios)),
      todas_nulas_menos_texto = list(
        c("doble", "toda_nula"), c("entero_ancho", "toda_nula"),
        c("decimal", "toda_nula"), c("fecha", "toda_nula"),
        c("logico", "toda_nula"), c("texto", "dos_valores")
      ),
      formas_cruzadas = list(
        c("doble", "un_valor"), c("entero_ancho", "constante"),
        c("texto", "toda_nula"), c("fecha", "dos_valores"),
        c("logico", "constante")
      ),
      comun = list(
        c("entero", varios), c("texto", "dos_valores"), c("doble", "un_valor"),
        c("fecha", "constante"), c("decimal", "toda_nula"),
        c("texto", "toda_nula")
      )
    )
    for (nombre in names(mezclas)) {
      tablas[[paste("mezcla", nombre, n, sep = ".")]] <- list(
        n = n, columnas = mezclas[[nombre]]
      )
    }
  }
  tablas
}

.ccf_crear <- function(conexion, motor, tabla, forma) {
  suppressWarnings(try(
    DBI::dbExecute(conexion, paste("DROP TABLE", tabla)), silent = TRUE
  ))
  columnas <- vapply(seq_along(forma$columnas), function(j) {
    paste0("c", j, " ", .ccf_tipo_sql(motor, forma$columnas[[j]][[1L]]))
  }, character(1L))
  DBI::dbExecute(conexion, paste0(
    "CREATE TABLE ", tabla, " (", paste(columnas, collapse = ", "), ")"
  ))
  if (forma$n == 0L) return(invisible(NULL))
  filas <- vapply(seq_len(forma$n), function(i) {
    paste0("(", paste(vapply(forma$columnas, function(columna) {
      .ccf_literal(motor, columna[[1L]], columna[[2L]], i)
    }, character(1L)), collapse = ", "), ")")
  }, character(1L))
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO ", tabla, " VALUES ", paste(filas, collapse = ", ")
  ))
  invisible(NULL)
}

.ccf_perfilar <- function(conexion, tabla, bloque, metricas = NULL) {
  argumentos <- list(
    conexion, tabla, bloque_muestra = bloque,
    fecha = as.POSIXct("2026-03-04 12:00:00", tz = "UTC")
  )
  if (!is.null(metricas)) argumentos$metricas <- metricas
  suppressWarnings(suppressMessages(do.call(perfilar_dbi, argumentos)))
}

.ccf_abrir <- function(motor) {
  if (identical(motor, "sqlite")) {
    skip_if_not_installed("RSQLite")
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    skip_if_not_installed("duckdb")
    suppressMessages(DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:"))
  }
}

.ccf_cerrar <- function(conexion, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(conexion, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(conexion)
  }
}

# Las clases del plan previo que cambiaron, con su diferencia.
.ccf_previo <- function(perfil) {
  previo <- perfil$resumen_tabla$meta$consultas$conciliacion$plan_previo
  if (is.null(previo)) return(NULL)
  detalle <- previo$detalle[previo$detalle$diferencia != 0, , drop = FALSE]
  list(
    estado = previo$estado, detalle = detalle, motivo = previo$motivo,
    anunciadas = as.numeric(previo$anunciadas)
  )
}

for (.ccf_motor in c("sqlite", "duckdb")) {
  local({
    motor <- .ccf_motor
    test_that(paste0("la grilla de formas de columna concilia en ", motor), {
      conexion <- .ccf_abrir(motor)
      on.exit(.ccf_cerrar(conexion, motor), add = TRUE)
      grilla <- .ccf_grilla()
      expect_gte(length(grilla), 90L)
      desfasadas <- character()
      sin_cuadrar <- character()
      for (nombre in names(grilla)) {
        .ccf_crear(conexion, motor, "lupa_forma", grilla[[nombre]])
        for (bloque in c("con_muestra", "solo_agregados")) {
          consultas <- .ccf_perfilar(
            conexion, "lupa_forma", bloque
          )$resumen_tabla$meta$consultas
          rotulo <- paste(nombre, bloque)
          if (!identical(consultas$conciliacion$estado, "conciliado")) {
            desfasadas <- c(desfasadas, rotulo)
          }
          if (!isTRUE(all.equal(as.numeric(consultas$conciliacion$anunciadas),
                                as.numeric(consultas$emitidas)))) {
            sin_cuadrar <- c(sin_cuadrar, rotulo)
          }
        }
      }
      expect_identical(desfasadas, character())
      expect_identical(sin_cuadrar, character())
    })
  })
}

test_that("una columna numerica toda nula en una tabla con datos: concilia, y el plan previo dice que clase y por que", {
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  # Las dos tablas tienen ademas una columna de texto toda nula: no es de las
  # que explican una mediana que falta, y el motivo cuenta una columna, no dos.
  DBI::dbExecute(conexion, "CREATE TABLE lupa_nula (id INTEGER, x DOUBLE PRECISION, s VARCHAR(20), t VARCHAR(20))")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO lupa_nula VALUES ",
    paste0("(", 1:12, ", NULL, 'v", 1:12 %% 3L, "', NULL)", collapse = ", ")
  ))
  DBI::dbExecute(conexion, "CREATE TABLE lupa_sana (id INTEGER, x DOUBLE PRECISION, s VARCHAR(20), t VARCHAR(20))")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO lupa_sana VALUES ",
    paste0("(", 1:12, ", ", 1:12 / 4, ", 'v", 1:12 %% 3L, "', NULL)", collapse = ", ")
  ))
  for (bloque in c("con_muestra", "solo_agregados")) {
    con_nula <- .ccf_perfilar(conexion, "lupa_nula", bloque)
    sana <- .ccf_perfilar(conexion, "lupa_sana", bloque)
    consultas <- con_nula$resumen_tabla$meta$consultas
    expect_identical(consultas$conciliacion$estado, "conciliado", info = bloque)
    expect_equal(as.numeric(consultas$conciliacion$anunciadas),
                 as.numeric(consultas$emitidas), info = bloque)
    expect_equal(
      sum(as.numeric(con_nula$resumen_tabla$meta$plan$n_consultas)),
      as.numeric(consultas$emitidas), info = bloque
    )
    # Una consulta menos que la tabla sin la columna nula: su mediana.
    expect_equal(
      as.numeric(sana$resumen_tabla$meta$consultas$emitidas) -
        as.numeric(consultas$emitidas), 1, info = bloque
    )
    expect_null(sana$resumen_tabla$meta$consultas$conciliacion$plan_previo)
    # Lo anunciado antes de leer los datos queda aparte: la clase es la de las
    # medianas, no una de arrastre, y el motivo es el verdadero.
    previo <- .ccf_previo(con_nula)
    expect_identical(previo$estado, "desfasado", info = bloque)
    expect_identical(nrow(previo$detalle), 1L, info = bloque)
    expect_match(previo$detalle$clase_consulta, "^mediana \\(")
    expect_equal(as.numeric(previo$detalle$diferencia), -1, info = bloque)
    expect_match(previo$detalle$motivo, "sin valores no nulos \\(1\\)")
    expect_false(grepl("STDDEV|STDEV|fall", previo$detalle$motivo), info = bloque)
    expect_match(previo$motivo, "antes de leer los datos")
  }
})

test_that("la cuenta concilia con cualquier seleccion de metricas, tambien sobre la tabla vacia", {
  # Con las metricas por omision la corrida sabe cuantos valores no nulos tiene
  # cada columna. Sin `validos` no lo sabe, y los mismos datos activan otras
  # cosas: el promedio nulo de una columna sin valores hace preguntar por
  # infinitos, y las medianas de la tabla vacia no se mandan sin que el plan
  # de la corrida se rehiciera. Y sin `basicos`, la sentencia de los conteos se
  # cobra en otra clase: descontar "lo que los agregados no mandaron" le sacaba
  # una consulta que si se habia mandado.
  selecciones <- list(
    "validos", "distintos", "moda", "basicos", "mediana", "desvio",
    c("validos", "basicos", "desvio"), c("basicos", "mediana"),
    c("basicos", "desvio"), c("validos", "distintos", "moda")
  )
  formas <- list(
    vacia = list(n = 0L, columnas = list(c("doble", "distintos"))),
    vacia_mixta = list(n = 0L, columnas = list(
      c("entero", "distintos"), c("doble", "distintos"), c("fecha", "distintos"),
      c("texto", "distintos")
    )),
    toda_nula = list(n = 12L, columnas = list(c("doble", "toda_nula"))),
    # Sin la cantidad de valores, la mediana de un entero que pasa de 2^53 no
    # se publicaria: la corrida no paga su consulta.
    entero_ancho = list(n = 12L, columnas = list(c("entero_ancho", "distintos"))),
    # Una columna que guarda infinitos: sus magnitudes se vuelven a pedir
    # sobre los valores finitos.
    con_infinito = list(n = 12L, columnas = list(
      c("entero", "distintos"), c("doble", "con_infinito"), c("texto", "dos_valores")
    )),
    solo_infinitos = list(n = 3L, columnas = list(c("doble", "solo_infinitos"))),
    una_numerica_nula = list(n = 12L, columnas = list(
      c("entero", "distintos"), c("doble", "toda_nula"), c("texto", "dos_valores")
    )),
    fechas = list(n = 12L, columnas = list(
      c("fecha", "distintos"), c("fecha_hora", "dos_valores"), c("fecha", "toda_nula")
    )),
    comun = list(n = 12L, columnas = list(
      c("entero", "distintos"), c("texto", "dos_valores"), c("doble", "un_valor"),
      c("fecha", "constante"), c("decimal", "toda_nula"), c("texto", "toda_nula")
    ))
  )
  for (motor in c("sqlite", "duckdb")) {
    conexion <- .ccf_abrir(motor)
    desfasadas <- character()
    for (nombre in names(formas)) {
      .ccf_crear(conexion, motor, "lupa_forma", formas[[nombre]])
      for (seleccion in selecciones) {
        estado <- .ccf_perfilar(
          conexion, "lupa_forma", "con_muestra", seleccion
        )$resumen_tabla$meta$consultas$conciliacion$estado
        if (!identical(estado, "conciliado")) {
          desfasadas <- c(
            desfasadas, paste(motor, nombre, paste(seleccion, collapse = "+"))
          )
        }
      }
    }
    .ccf_cerrar(conexion, motor)
    expect_identical(desfasadas, character())
  }
})

test_that("una columna sin valores se reconoce tambien por el registro de sus metricas", {
  # Sin la metrica `validos` no hay conteo, pero los basicos o la prueba de
  # magnitud dicen lo mismo y la corrida deja de mandar las mismas consultas:
  # en los servidores, el desvio y la mediana de esa columna.
  columnas <- data.frame(
    columna = c("a", "b", "s"), n_validos = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  registros <- data.frame(
    columna = c("a", "a", "b", "s"),
    metrica = c("media", "mediana", "media", "moda"),
    estado = c("sin_valores", "sin_valores", "calculado", "sin_valores"),
    stringsAsFactors = FALSE
  )
  expect_identical(.columnas_sin_valores_dbi(columnas, c("a", "b")), 0L)
  expect_identical(
    .columnas_sin_valores_dbi(columnas, c("a", "b"), registros), 1L
  )
  # Contada y registrada, es una sola columna.
  columnas$n_validos <- c(0, 3, 0)
  expect_identical(
    .columnas_sin_valores_dbi(columnas, c("a", "b"), registros), 1L
  )
  expect_identical(.columnas_sin_valores_dbi(columnas, NULL, registros), 2L)
})

test_that("SQLite: una fecha guardada como texto se cuenta en sus dos clases, sin compensarse", {
  skip_if_not_installed("RSQLite")
  # La columna es `DATE` -afinidad numerica- y guarda texto ISO, que es como
  # SQLite guarda las fechas. La corrida lo mide con `typeof()`, una consulta
  # que el plan no cuenta, y no manda la mediana, que el plan contaba. Las dos
  # se compensaban: la cuenta "conciliaba" con una consulta en la clase
  # equivocada, y sin la mediana en la seleccion quedaba una de mas.
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE lupa_fecha (c1 DATE)")
  DBI::dbExecute(conexion, "INSERT INTO lupa_fecha VALUES ('2024-01-02'), ('2024-01-03'), ('2024-01-04')")
  perfil <- .ccf_perfilar(conexion, "lupa_fecha", "con_muestra")
  consultas <- perfil$resumen_tabla$meta$consultas
  expect_identical(consultas$conciliacion$estado, "conciliado")
  expect_equal(as.numeric(consultas$conciliacion$anunciadas),
               as.numeric(consultas$emitidas))
  previo <- .ccf_previo(perfil)
  expect_identical(previo$estado, "desfasado")
  cambios <- previo$detalle
  expect_identical(nrow(cambios), 2L)
  de_menos <- cambios[cambios$diferencia < 0, ]
  de_mas <- cambios[cambios$diferencia > 0, ]
  expect_match(de_menos$clase_consulta, "^mediana \\(")
  expect_match(de_menos$motivo, "que guardan texto \\(1\\)")
  expect_equal(as.numeric(de_mas$diferencia), 1)
  expect_match(de_mas$motivo, "`typeof()`", fixed = TRUE)
  # Sin la mediana en la seleccion no hay nada que compensar: concilia igual.
  for (seleccion in list("basicos", "desvio", c("validos", "basicos", "desvio"))) {
    estado <- .ccf_perfilar(
      conexion, "lupa_fecha", "con_muestra", seleccion
    )$resumen_tabla$meta$consultas$conciliacion$estado
    expect_identical(estado, "conciliado", info = paste(seleccion, collapse = "+"))
  }
})

test_that("una columna que guarda infinitos: lo que se vuelve a pedir sobre los finitos se cuenta en su clase", {
  # Las magnitudes de una columna con infinitos se piden de nuevo sobre los
  # valores finitos: consultas que el plan no cuenta, porque lo dicen los
  # datos. Quedaban de mas, sin clase, anotadas en los portones.
  for (motor in c("sqlite", "duckdb")) {
    conexion <- .ccf_abrir(motor)
    .ccf_crear(conexion, motor, "lupa_infinito", list(n = 12L, columnas = list(
      c("entero", "distintos"), c("doble", "con_infinito")
    )))
    .ccf_crear(conexion, motor, "lupa_finito", list(n = 12L, columnas = list(
      c("entero", "distintos"), c("doble", "distintos")
    )))
    for (bloque in c("con_muestra", "solo_agregados")) {
      rotulo <- paste(motor, bloque)
      perfil <- .ccf_perfilar(conexion, "lupa_infinito", bloque)
      finito <- .ccf_perfilar(conexion, "lupa_finito", bloque)
      consultas <- perfil$resumen_tabla$meta$consultas
      expect_identical(consultas$conciliacion$estado, "conciliado", info = rotulo)
      expect_equal(as.numeric(consultas$conciliacion$anunciadas),
                   as.numeric(consultas$emitidas), info = rotulo)
      # Manda mas consultas que la misma tabla sin el infinito, y la tabla sin
      # el infinito no tiene nada que rehacer.
      expect_gt(as.numeric(consultas$emitidas),
                as.numeric(finito$resumen_tabla$meta$consultas$emitidas))
      expect_null(finito$resumen_tabla$meta$consultas$conciliacion$plan_previo)
      previo <- .ccf_previo(perfil)
      expect_identical(previo$estado, "desfasado", info = rotulo)
      cambios <- previo$detalle
      expect_gt(nrow(cambios), 0L)
      # Cada diferencia en su clase -ninguna en la de arrastre- y con su causa.
      expect_false(any(grepl("^portones", cambios$clase_consulta)), info = rotulo)
      expect_true(all(cambios$diferencia > 0), info = rotulo)
      expect_true(all(grepl("guarda infinitos", cambios$motivo)), info = rotulo)
      expect_equal(
        sum(cambios$diferencia),
        as.numeric(consultas$emitidas) - as.numeric(previo$anunciadas),
        info = rotulo
      )
    }
    .ccf_cerrar(conexion, motor)
  }
})

test_that("un entero que no cabe exacto en un doble: la consulta que activa se cuenta en su clase", {
  skip_if_not_installed("duckdb")
  conexion <- suppressMessages(DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:"))
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(conexion, "CREATE TABLE lupa_ancho (c1 BIGINT)")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO lupa_ancho VALUES ",
    paste0("(", sprintf("%.0f", 9007199254740992 + 2 * 1:12), ")", collapse = ", ")
  ))
  DBI::dbExecute(conexion, "CREATE TABLE lupa_chico (c1 BIGINT)")
  DBI::dbExecute(conexion, paste0(
    "INSERT INTO lupa_chico VALUES ", paste0("(", 1:12, ")", collapse = ", ")
  ))
  ancho <- .ccf_perfilar(conexion, "lupa_ancho", "con_muestra")
  chico <- .ccf_perfilar(conexion, "lupa_chico", "con_muestra")
  consultas <- ancho$resumen_tabla$meta$consultas
  expect_identical(consultas$conciliacion$estado, "conciliado")
  expect_equal(as.numeric(consultas$conciliacion$anunciadas),
               as.numeric(consultas$emitidas))
  # Una consulta mas que con valores chicos: la mediana exacta.
  expect_equal(
    as.numeric(consultas$emitidas) -
      as.numeric(chico$resumen_tabla$meta$consultas$emitidas), 1
  )
  expect_null(chico$resumen_tabla$meta$consultas$conciliacion$plan_previo)
  # El maximo del plan de la corrida no queda por debajo de lo que publica.
  plan <- ancho$resumen_tabla$meta$plan
  expect_true(all(plan$n_consultas <= plan$n_consultas_max))
  previo <- .ccf_previo(ancho)
  expect_identical(previo$estado, "desfasado")
  expect_identical(nrow(previo$detalle), 1L)
  expect_match(previo$detalle$clase_consulta, "^mediana \\(")
  expect_equal(as.numeric(previo$detalle$diferencia), 1)
  expect_match(previo$detalle$motivo, "en una consulta aparte")
})

test_that("solo se cuenta por datos lo que la magnitud de los valores activa, y con tope lo que falta", {
  plan <- data.frame(
    clase_consulta = c(
      "portones (esquema y sondas)",
      "agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio)",
      "mediana (orden total + limite/salto)"
    ),
    n_consultas = c(2, 1, 3), alcance = "una vez", n_consultas_max = c(2, 1, 3),
    stringsAsFactors = FALSE
  )
  ejecutado <- function(etapas, columnas_sin_valores = 0, sin_pagar = 0,
                        no_finitos = integer()) {
    presupuesto <- .presupuesto_dbi(Inf, instrumentar = FALSE)
    presupuesto$medianas_sin_pagar <- sin_pagar
    # La posicion entre las emitidas: las dos primeras son las del porton.
    presupuesto$emitidas_por_no_finitos <- no_finitos + 2L
    for (etapa in c("portones", "esquema", etapas)) {
      .contar_dbi(presupuesto, etapa = etapa)
    }
    salida <- .plan_ejecutado_consultas_dbi(
      plan, presupuesto, consultas_antes_resumen = 2L,
      columnas_sin_valores = columnas_sin_valores
    )
    list(
      n = salida$n_consultas,
      ajustes = as.numeric(attr(salida, "ajustes_por_datos", exact = TRUE))
    )
  }
  medianas <- rep("mediana", 3L)
  # El desvio calculado aparte es de las etapas que la magnitud activa.
  de_mas <- ejecutado(c("basicos", "desvio", medianas))
  expect_equal(de_mas$n, c(2, 2, 3))
  expect_equal(de_mas$ajustes, 1)
  # La pregunta por los infinitos tambien la deciden los datos.
  infinitos <- ejecutado(c("basicos", "sonda_infinitos", "sonda_infinitos", medianas))
  expect_equal(infinitos$n, c(2, 3, 3))
  expect_equal(infinitos$ajustes, 2)
  # Lo que se volvio a pedir sobre los valores finitos de una columna con
  # infinitos se cuenta por lo anotado: los basicos de nuevo y su desvio.
  finitos <- ejecutado(c("basicos", "basicos", "desvio", medianas), no_finitos = 2:3)
  expect_equal(finitos$n, c(2, 3, 3))
  expect_equal(finitos$ajustes, 2)
  # Sin la anotacion, de esas dos solo se explica la del desvio.
  sin_anotar <- ejecutado(c("basicos", "basicos", "desvio", medianas))
  expect_equal(sin_anotar$n, c(2, 2, 3))
  # Un sondeo que el plan no anuncia, en la misma clase, no: sigue siendo una
  # diferencia, y no se la explica con un motivo que no es el suyo.
  ajeno <- ejecutado(c("basicos", "sonda_magnitud", "sonda_magnitud", medianas))
  expect_equal(ajeno$n, c(2, 1, 3))
  expect_length(ajeno$ajustes, 0L)
  # Los agregados planos solo se descuentan si la clase mando algo: con una
  # seleccion de metricas sin `basicos` su sentencia se cobra con otra etapa.
  sin_basicos <- ejecutado(c("conteos", medianas), columnas_sin_valores = 2)
  expect_equal(sin_basicos$n, c(2, 1, 3))
  expect_length(sin_basicos$ajustes, 0L)
  # SQLite: una columna numerica que guarda texto. La medicion con `typeof()`
  # se cuenta en la clase donde se cobra y explica una mediana que no se manda.
  texto <- ejecutado(c("basicos", "tipos_guardados", "mediana", "mediana"))
  expect_equal(texto$n, c(3, 1, 2))
  expect_equal(sort(texto$ajustes), c(-1, 1))
  # Una mediana que la corrida decidio no pagar -valores de al menos 2^53 y la
  # cantidad de valores sin contar- se descuenta; sin esa decision, no.
  sin_pagar <- ejecutado(c("basicos", "mediana", "mediana"), sin_pagar = 1)
  expect_equal(sin_pagar$n, c(2, 1, 2))
  expect_equal(sin_pagar$ajustes, -1)
  sin_causa <- ejecutado(c("basicos", "mediana", "mediana"))
  expect_equal(sin_causa$n, c(2, 1, 3))
  expect_length(sin_causa$ajustes, 0L)
  # De menos: no se descuenta mas que la cantidad de columnas numericas sin
  # valores. Con una, de dos medianas que faltan se explica una.
  una <- ejecutado(c("basicos", "mediana"), columnas_sin_valores = 1)
  expect_equal(una$n, c(2, 1, 2))
  expect_equal(una$ajustes, -1)
  dos <- ejecutado(c("basicos", "mediana"), columnas_sin_valores = 2)
  expect_equal(dos$n, c(2, 1, 1))
  # Sin ninguna columna sin valores, la mediana que falta no se explica.
  ninguna <- ejecutado(c("basicos", "mediana", "mediana"))
  expect_equal(ninguna$n, c(2, 1, 3))
  expect_length(ninguna$ajustes, 0L)
})

test_that("el motivo de una diferencia no cita una falla que no es de esa clase", {
  # El rechazo esperado de un sondeo de capacidad -la forma del desvio que el
  # motor no tiene- quedaba citado como causa de cualquier diferencia: "no
  # corrio porque una consulta previa fallo", cuando nada habia fallado.
  clases <- c(
    "portones (esquema y sondas)",
    "capacidad de desvio (formas candidatas)",
    "mediana (orden total + limite/salto)"
  )
  presupuesto <- .presupuesto_dbi(Inf, instrumentar = FALSE)
  campo <- paste0("fallas_", "son", "das")
  presupuesto[[campo]] <- list(list(
    etapa = paste0("son", "da_desvio"), motivo = "no such function: STDDEV_SAMP"
  ))
  expect_identical(
    .fallas_conciliacion_consultas_dbi(presupuesto, clases, clases[[1L]]),
    character()
  )
  expect_identical(
    .fallas_conciliacion_consultas_dbi(presupuesto, clases, clases[[3L]]),
    character()
  )
  # Sin clase se listan todas, y la de su propia clase si se cita.
  expect_length(.fallas_conciliacion_consultas_dbi(presupuesto, clases), 1L)
  expect_length(
    .fallas_conciliacion_consultas_dbi(presupuesto, clases, clases[[2L]]), 1L
  )
})

test_that("el rechazo esperado de un sondeo de capacidad no impide reconocer una consulta condicional", {
  # Una clase que emite mas que lo esperado, dentro del maximo que el plan
  # anuncio, es condicional y la cuenta concilia. Antes dejaba de serlo en
  # cuanto la corrida tenia cualquier falla, aunque fuera de otra clase: y el
  # rechazo de una forma candidata del desvio pasa en toda corrida.
  plan <- data.frame(
    clase_consulta = c(
      "portones (esquema y sondas)",
      "capacidad de desvio (formas candidatas)",
      "duplicados exactos (GROUP BY)"
    ),
    n_consultas = c(3, 2, 1), alcance = "una vez", n_consultas_max = c(3, 2, 2),
    stringsAsFactors = FALSE
  )
  conciliar <- function(duplicados, con_rechazo) {
    presupuesto <- .presupuesto_dbi(Inf, instrumentar = FALSE)
    for (i in seq_len(5L + duplicados)) .contar_dbi(presupuesto, etapa = "portones")
    if (con_rechazo) {
      presupuesto[[paste0("fallas_", "son", "das")]] <- list(list(
        etapa = paste0("son", "da_desvio"), motivo = "no such function: STDEV"
      ))
    }
    emitido <- plan
    emitido$n_consultas[[3L]] <- duplicados
    .conciliar_consultas_dbi(plan, emitido, presupuesto$usadas, list(), presupuesto)
  }
  for (con_rechazo in c(FALSE, TRUE)) {
    rotulo <- paste("con rechazo de sondeo:", con_rechazo)
    expect_identical(conciliar(1L, con_rechazo)$estado, "conciliado", info = rotulo)
    conciliacion <- conciliar(2L, con_rechazo)
    expect_identical(conciliacion$estado, "conciliado", info = rotulo)
    diferencias <- conciliacion$detalle[conciliacion$detalle$diferencia != 0, ]
    expect_identical(nrow(diferencias), 1L, info = rotulo)
    expect_identical(diferencias$direccion, "condicional", info = rotulo)
    expect_match(diferencias$clase_consulta, "^duplicados exactos")
    expect_false(grepl("STDEV|fall", diferencias$motivo), info = rotulo)
    # Por encima del maximo anunciado si es una diferencia.
    expect_identical(conciliar(3L, con_rechazo)$estado, "desfasado", info = rotulo)
  }
})

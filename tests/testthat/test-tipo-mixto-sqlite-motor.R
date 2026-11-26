# La mezcla de tipos de una columna de SQLite se mide en el motor, no en el
# aviso del controlador, y de esa medicion sale una sola forma de leerla: la
# misma en cualquier orden de filas, con cualquier historial de la conexion y
# por cualquier via. Estas pruebas fijan los casos donde la deteccion por el
# aviso fallaba.
testthat::skip_on_cran()
testthat::skip_if_not_installed("RSQLite")

.tmm_con <- function(valores, columna = "x", tipo = "") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  nombre <- paste0("\"", gsub("\"", "\"\"", columna, fixed = TRUE), "\"")
  DBI::dbExecute(con, paste0("CREATE TABLE t (", nombre, " ", tipo, ")"))
  for (valor in valores) {
    DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", valor, ")"))
  }
  con
}

.tmm_sin_avisos <- function(expr) {
  avisos <- character()
  valor <- withCallingHandlers(expr, warning = function(w) {
    avisos <<- c(avisos, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_false(any(grepl("mixed type", avisos, fixed = TRUE)))
  valor
}

.tmm_fecha <- as.Date("2026-01-01")

test_that("un real entero desde 1e15 y desde 2^53 vale lo mismo que el entero", {
  pares <- list(
    c("1000000000000000", "1e15", "'0'"),
    c("9007199254740992", "9007199254740992.0", "'0'"),
    c("-1000000000000000", "-1e15", "'0'")
  )
  for (valores in pares) {
    for (orden in list(valores, rev(valores))) {
      con <- .tmm_con(orden)
      for (via in list(list(), list(universo = "muestra_motor", muestra_motor = 3))) {
        r <- .tmm_sin_avisos(do.call(perfilar_dbi, c(list(con, "t"), via)))
        pm <- r$perfil_muestra$columnas
        expect_equal(pm$n_distintos, 2)
        expect_equal(pm$frecuencia_moda, 2)
        expect_identical(pm$moda, sub("^'|'$", "", valores[[1L]]))
      }
      DBI::dbDisconnect(con)
    }
  }
})

test_that("una columna con tipo declarado que guarda texto sigue igual por bloques y en medir_dbi()", {
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(nuc$Formato, "F", expresion_regular = "^[0-9.]+$"), "t", "x"
  ))
  for (tipo in c("INTEGER", "REAL")) {
    con <- .tmm_con(c("1", "'abc'", "2", "3"), tipo = tipo)
    r <- suppressWarnings(perfilar_dbi(con, "t", bloque_filas = 2))
    cob <- r$resumen_tabla$cobertura
    expect_false(any(cob$bloque == "fuente_bloques" & cob$estado == "no_disponible"))
    expect_false(any(grepl("mezclados", cob$motivo, fixed = TRUE)))
    expect_equal(r$resumen_tabla$columnas$n_validos, 4)
    m <- suppressWarnings(
      medir_dbi(mod, con, bloque_filas = 2, estrategia_no_sql = "bloques")
    )
    expect_identical(nrow(m), 1L)
    expect_equal(m$denominador, 4)
    DBI::dbDisconnect(con)
  }
})

test_that("una columna de solo numeros da lo mismo en la muestra del motor con cualquier historial", {
  perfil_solo <- function(antes) {
    con <- .tmm_con(c("9007199254740993", "2.5", "1"))
    on.exit(DBI::dbDisconnect(con))
    if (antes) suppressWarnings(perfilar_dbi(con, "t", fecha = .tmm_fecha))
    r <- .tmm_sin_avisos(perfilar_dbi(
      con, "t", universo = "muestra_motor", muestra_motor = 3,
      fecha = .tmm_fecha
    ))
    r$perfil_muestra$columnas
  }
  primero <- perfil_solo(FALSE)
  despues <- perfil_solo(TRUE)
  expect_identical(primero$tipo_declarado, "doble")
  # El entero no cabe en un doble: sus magnitudes no se publican.
  expect_true(is.na(primero$maximo))
  for (campo in c("tipo_declarado", "n_distintos", "moda", "minimo", "maximo",
                  "media", "n_negativos")) {
    expect_identical(despues[[campo]], primero[[campo]])
  }
})

test_that("un BLOB mezclado se lee como BLOB en la muestra del motor, en cada corrida y despues de otra", {
  valores <- c("x'61626300646566'", "x'00'", "'zz'", "x'616263'")
  con <- .tmm_con(valores)
  on.exit(DBI::dbDisconnect(con))
  revisar <- function() {
    r <- .tmm_sin_avisos(perfilar_dbi(
      con, "t", universo = "muestra_motor", muestra_motor = 4
    ))
    pm <- r$perfil_muestra$columnas
    expect_identical(pm$tipo_declarado, "blob")
    expect_equal(pm$n_distintos, 4)
    expect_equal(pm$n_faltantes_disfrazados, 0)
    expect_equal(pm$n_blancos, 0)
  }
  for (corrida in 1:12) revisar()
  suppressWarnings(perfilar_dbi(con, "t"))
  revisar()
})

test_that("una columna mixta con un salto de linea en el nombre se mide y se lee de una sola forma", {
  nombre <- paste0("a", intToUtf8(10L), "b")
  salidas <- lapply(list(c("1", "'x'", "2"), c("'x'", "1", "2")), function(orden) {
    con <- .tmm_con(orden, columna = nombre)
    on.exit(DBI::dbDisconnect(con))
    r <- .tmm_sin_avisos(perfilar_dbi(con, "t", fecha = .tmm_fecha))
    cob <- r$resumen_tabla$cobertura
    expect_true(any(cob$elemento == nombre & cob$estado == "degradado"))
    pm <- r$perfil_muestra$columnas
    # El texto no se lee como `0`.
    expect_true(is.na(pm$minimo))
    expect_identical(pm$tipo_declarado, "texto")
    pm[, c("n_distintos", "moda", "minimo", "maximo")]
  })
  expect_identical(salidas[[1L]], salidas[[2L]])
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cita <- paste0("\"", nombre, "\"")
  valores <- list(a = c("1", "'x'", "2", "3"), b = c("1", "0", "2", "3"))
  for (tabla in c("a", "b")) {
    DBI::dbExecute(con, paste0("CREATE TABLE ", tabla, " (", cita, ")"))
    for (v in valores[[tabla]]) {
      DBI::dbExecute(con, paste0("INSERT INTO ", tabla, " VALUES (", v, ")"))
    }
  }
  # La coleccion la compara con la lectura unica: 'x' no es el 0 de `b`, tres
  # de cuatro, lo que cuenta el motor.
  rel <- .tmm_sin_avisos(relaciones_coleccion(
    coleccion(con, c("a", "b")), data.frame(tabla_1 = "a", tabla_2 = "b"),
    muestra = 100, umbral_cobertura = 0
  ))
  expect_identical(nrow(rel$relaciones), 1L)
  expect_equal(rel$relaciones$cobertura_tabla1_en_tabla2, 0.75)
  expect_false(any(rel$cobertura_podas$motivo %in% "tipo_mixto"))
})

test_that("la cobertura dice como quedan los textos que se escriben como un numero", {
  # `'Inf'` y el real `Inf` se escriben igual y cuentan como uno; `'2.0'` y
  # `2` no. La cobertura no promete que no se pierde ningun valor.
  con <- .tmm_con(c("'Inf'", "9e999", "1", "'2.0'", "2", "'NaN'"))
  on.exit(DBI::dbDisconnect(con))
  r <- .tmm_sin_avisos(perfilar_dbi(con, "t"))
  pm <- r$perfil_muestra$columnas
  # Inf (dos), 1, 2.0, 2 y el 'NaN', que cuenta como faltante disfrazado.
  expect_equal(pm$n_distintos, 5)
  expect_equal(pm$frecuencia_moda, 2)
  expect_identical(pm$moda, "Inf")
  fila <- r$resumen_tabla$cobertura
  fila <- fila[fila$elemento == "x" & fila$bloque == "perfil_muestra", , drop = FALSE]
  expect_identical(nrow(fila), 1L)
  expect_false(grepl("sin perder", fila$motivo, fixed = TRUE))
  expect_true(grepl("'Inf'", fila$motivo, fixed = TRUE))
  expect_true(grepl("'2.0'", fila$motivo, fixed = TRUE))
  expect_true(grepl("faltante disfrazado", fila$motivo, fixed = TRUE))
})

test_that("dos reales que solo difieren en la cifra 17 no quedan como uno al leerse como texto", {
  con <- .tmm_con(c("0.30000000000000004", "0.3", "'x'"))
  on.exit(DBI::dbDisconnect(con))
  r <- .tmm_sin_avisos(perfilar_dbi(con, "t"))
  expect_equal(r$perfil_muestra$columnas$n_distintos, 3)
  expect_equal(r$resumen_tabla$columnas$n_distintos, 3)
})

test_that("la sentencia que mide la mezcla la cuenta el plan", {
  con <- .tmm_con(c("1", "'x'", "2"))
  on.exit(DBI::dbDisconnect(con))
  r <- .tmm_sin_avisos(perfilar_dbi(con, "t"))
  conciliacion <- r$resumen_tabla$meta$consultas$conciliacion
  expect_identical(conciliacion$estado, "conciliado")
  plan <- plan_perfilado_dbi(con, "t")
  expect_equal(sum(plan$n_consultas), conciliacion$emitidas)
})

test_that("la muestra del motor al azar da la misma cifra en doce corridas", {
  casos <- list(
    texto = c("-9", "1", "2", "'ND'", "'nd'", "NULL", "2.0"),
    numeros = c("1", "2.5", "3", "NULL", "1e15", "1000000000000000")
  )
  for (valores in casos) {
    con <- .tmm_con(valores)
    perfiles <- lapply(1:12, function(i) {
      r <- .tmm_sin_avisos(perfilar_dbi(
        con, "t", universo = "muestra_motor", muestra_motor = length(valores),
        fecha = .tmm_fecha
      ))
      r$perfil_muestra$columnas[, c("tipo_declarado", "n_distintos", "moda",
                                    "frecuencia_moda", "minimo", "maximo",
                                    "n_faltantes_disfrazados")]
    })
    DBI::dbDisconnect(con)
    for (p in perfiles[-1L]) expect_identical(p, perfiles[[1L]])
  }
})

test_that("una tabla ancha sin tipos declarados se mide entera en una sentencia", {
  n <- 1500L
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  campos <- paste0("c", seq_len(n))
  DBI::dbExecute(con, paste0("CREATE TABLE t (", paste(campos, collapse = ", "), ")"))
  fila <- rep("1", n)
  DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", paste(fila, collapse = ", "), ")"))
  fila[[300L]] <- "2.5"
  fila[[1499L]] <- "'x'"
  DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", paste(fila, collapse = ", "), ")"))
  citas <- vapply(campos, function(x) lupa:::.citar_identificador_dbi(con, x),
                  character(1L), USE.NAMES = FALSE)
  medida <- lupa:::.lectura_tipo_mixto_perfil_dbi(
    con, "t", campos, citas, stats::setNames(rep("", n), campos)
  )
  expect_true(isTRUE(attr(medida, "ok")))
  expect_identical(medida$columna, c("c300", "c1499"))
  expect_false(grepl(";", attr(medida, "sql"), fixed = TRUE))
  expect_identical(medida$clase, c("numeros", "texto"))
})

test_that("una columna del usuario con el nombre de la auxiliar se lee como cualquier otra", {
  reservada <- "__lupa_tipo_mixto_real_1"
  perfil_de <- function(columnas, filas) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, paste0("CREATE TABLE t (", paste(columnas, collapse = ", "), ")"))
    for (f in filas) DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", f, ")"))
    .tmm_sin_avisos(perfilar_dbi(con, "t", fecha = .tmm_fecha))
  }
  mixta <- c("1", "'ND'", "2", "7")
  campos_a <- c("tipo_declarado", "n_distintos", "moda", "minimo", "maximo",
                "n_faltantes_disfrazados")
  for (tipo_a in c("", "TEXT")) {
    base <- perfil_de(paste("a", tipo_a), mixta)
    esperado <- base$perfil_muestra$columnas[, campos_a]
    cob_a <- base$resumen_tabla$cobertura
    cob_a <- cob_a[cob_a$elemento %in% "a", c("bloque", "estado", "motivo")]
    for (primera in c(TRUE, FALSE)) {
      columnas <- c(paste("a", tipo_a), paste0("\"", reservada, "\" REAL"))
      filas <- paste0(mixta, ", ", c("0.5", "1.5", "NULL", "2.5"))
      if (primera) {
        columnas <- rev(columnas)
        filas <- paste0(c("0.5", "1.5", "NULL", "2.5"), ", ", mixta)
      }
      r <- perfil_de(columnas, filas)
      pm <- r$perfil_muestra$columnas
      expect_identical(pm[pm$columna == "a", campos_a, drop = FALSE],
                       esperado[1L, , drop = FALSE], ignore_attr = TRUE)
      cob <- r$resumen_tabla$cobertura
      expect_identical(cob[cob$elemento %in% "a", c("bloque", "estado", "motivo")],
                       cob_a, ignore_attr = TRUE)
      # La del usuario guarda sus reales: 0.5, 1.5 y 2.5.
      expect_equal(pm$maximo[pm$columna == reservada], 2.5)
      expect_equal(pm$n_distintos[pm$columna == reservada], 3)
    }
  }
})

test_that("una columna generada que mezcla tipos se mide, se lee de una sola forma y se declara", {
  salidas <- lapply(list(c(2, 1, 3, 4), c(1, 2, 3, 4), c(4, 3, 1, 2)), function(orden) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, "CREATE TABLE t (a, x AS (CASE WHEN a = 1 THEN 'ND' ELSE a END))")
    for (v in orden) DBI::dbExecute(con, paste0("INSERT INTO t (a) VALUES (", v, ")"))
    r <- .tmm_sin_avisos(perfilar_dbi(con, "t", fecha = .tmm_fecha))
    cob <- r$resumen_tabla$cobertura
    expect_true(any(cob$elemento %in% "x" & cob$estado == "degradado"))
    expect_true("x" %in% r$resumen_tabla$meta$lectura_tipo_mixto$columna)
    pm <- r$perfil_muestra$columnas
    fila <- pm[pm$columna == "x", , drop = FALSE]
    # 'ND' no se lee como 0.
    expect_true(is.na(fila$minimo))
    expect_equal(fila$n_faltantes_disfrazados, 1)
    fila[, c("tipo_declarado", "n_distintos", "moda", "minimo",
             "n_faltantes_disfrazados")]
  })
  expect_identical(salidas[[1L]], salidas[[2L]], ignore_attr = TRUE)
  expect_identical(salidas[[1L]], salidas[[3L]], ignore_attr = TRUE)
})

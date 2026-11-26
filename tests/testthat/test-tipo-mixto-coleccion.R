# Una columna de SQLite que guarda valores de tipos distintos llega a la
# coleccion con la misma lectura unica de `perfilar_dbi()`: ningun texto se lee
# como `0` -el `'ND'` de `a.k` coincidia con el `0` de `b.k` segun la primera
# fila que llegara-, un BLOB solo aparea con el mismo BLOB, y la cobertura es la
# que cuenta el motor, en cualquier orden de filas y de tablas. Solo se poda la
# columna de enteros y reales con un entero que un doble no representa.
testthat::skip_on_cran()
testthat::skip_if_not_installed("RSQLite")

.tmc_con <- function(tablas) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  for (nombre in names(tablas)) {
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", nombre, " (k ", tablas[[nombre]]$tipo, ")"
    ))
    for (v in tablas[[nombre]]$valores) {
      DBI::dbExecute(con, paste0("INSERT INTO ", nombre, " VALUES (", v, ")"))
    }
  }
  con
}

# La parte de los valores no nulos de `t1` que el motor encuentra en `t2`.
.tmc_motor <- function(con, t1, t2) {
  DBI::dbGetQuery(con, paste0(
    "SELECT CAST((SELECT COUNT(*) FROM ", t1, " WHERE k IN (SELECT k FROM ", t2,
    ")) AS REAL) / (SELECT COUNT(k) FROM ", t1, ") AS p"
  ))$p
}

.tmc_relaciones <- function(con, t1, t2) {
  avisos <- character()
  r <- withCallingHandlers(
    relaciones_coleccion(
      coleccion(con, unique(c(t1, t2))),
      data.frame(tabla_1 = t1, tabla_2 = t2), muestra = 100,
      umbral_cobertura = 0
    ),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("mixed type", avisos, fixed = TRUE)))
  r
}

test_that("una columna con numeros y 'ND' no da una relacion falsa y da la cobertura del motor", {
  ordenes <- list(
    c("1", "2", "3", "'ND'", "5"), c("'ND'", "5", "3", "2", "1"),
    c("3", "'ND'", "1", "5", "2")
  )
  for (orden in ordenes) {
    con <- .tmc_con(list(
      a = list(tipo = "", valores = orden),
      b = list(tipo = "INTEGER", valores = c("0", "1", "2", "3", "5"))
    ))
    # 'ND' no es 0: cuatro de cinco en cada sentido.
    expect_equal(.tmc_motor(con, "a", "b"), 0.8)
    for (par in list(c("a", "b"), c("b", "a"))) {
      r <- .tmc_relaciones(con, par[[1L]], par[[2L]])
      rel <- r$relaciones
      expect_identical(nrow(rel), 1L)
      expect_equal(rel$cobertura_tabla1_en_tabla2,
                   .tmc_motor(con, par[[1L]], par[[2L]]))
      expect_equal(rel$cobertura_tabla2_en_tabla1,
                   .tmc_motor(con, par[[2L]], par[[1L]]))
      expect_false(any(r$cobertura_podas$motivo %in% "tipo_mixto"))
    }
    DBI::dbDisconnect(con)
  }
})

test_that("una columna de enteros y reales con un entero ancho se poda y se declara", {
  for (orden in list(c("9007199254740993", "2.5", "1"), c("2.5", "1", "9007199254740993"))) {
    con <- .tmc_con(list(
      a = list(tipo = "", valores = orden),
      b = list(tipo = "", valores = c("9007199254740992", "1", "2.5"))
    ))
    r <- .tmc_relaciones(con, "a", "b")
    DBI::dbDisconnect(con)
    expect_identical(nrow(r$relaciones), 0L)
    podas <- r$cobertura_podas
    expect_true(all(podas$motivo == "tipo_mixto"))
    expect_true(all(grepl("2^53", podas$detalle, fixed = TRUE)))
  }
})

test_that("un BLOB mezclado no aparea con lo que no es igual byte a byte", {
  # `abc\0def`, `abc` como BLOB, el texto 'zz' y el entero 1, contra texto y
  # contra otra columna mezclada: el BLOB `abc\0def` no es el texto 'abc' (un
  # `CAST` a texto lo cortaba en el nulo) ni el BLOB `abc\0`.
  a <- c("x'61626300646566'", "x'616263'", "'zz'", "1")
  for (otra in list(
    list(tipo = "TEXT", valores = c("'abc'", "'zz'", "'q'")),
    list(tipo = "", valores = c("x'616263'", "'abc'", "'zz'", "x'6162630064'"))
  )) {
    for (orden in list(a, rev(a))) {
      con <- .tmc_con(list(a = list(tipo = "", valores = orden), b = otra))
      r <- .tmc_relaciones(con, "a", "b")
      rel <- r$relaciones
      expect_identical(nrow(rel), 1L)
      expect_equal(rel$cobertura_tabla1_en_tabla2, .tmc_motor(con, "a", "b"))
      expect_equal(rel$cobertura_tabla2_en_tabla1, .tmc_motor(con, "b", "a"))
      expect_lt(rel$cobertura_tabla1_en_tabla2, 1)
      expect_false(any(r$cobertura_podas$motivo %in% "tipo_mixto"))
      DBI::dbDisconnect(con)
    }
  }
})

test_that("un par que compara una columna mezclada lo declara en cobertura_pares", {
  # `a.k` guarda el real 2.0 y texto; `b.k` el texto '2'. Se comparan por valor
  # -2.0 y '2' son el mismo valor, como en `detectar_relaciones()`-, y el motor,
  # sin tipo declarado, no los iguala: la fila del par lo dice.
  con <- .tmc_con(list(
    a = list(tipo = "", valores = c(rep("2.0", 19), "'zz'")),
    b = list(tipo = "TEXT", valores = c(rep("'2'", 19), "'qq'"))
  ))
  on.exit(DBI::dbDisconnect(con))
  r <- .tmc_relaciones(con, "a", "b")
  expect_identical(nrow(r$relaciones), 1L)
  expect_equal(r$relaciones$cobertura_tabla1_en_tabla2, 0.95)
  par <- r$cobertura_pares
  expect_identical(nrow(par), 1L)
  expect_identical(c(par$tabla_1, par$tabla_2), c("a", "b"))
  expect_true(grepl("por valor", par$motivo, fixed = TRUE))
  expect_true(grepl("un REAL y un TEXT no son iguales", par$motivo, fixed = TRUE))
  expect_true(grepl("`k` (de `a`)", par$motivo, fixed = TRUE))
  # El par se comparo: no falta.
  expect_identical(r$meta$pares_faltantes, 0L)
  expect_identical(r$meta$pares_comparados, 1L)
  # Un par sin columnas mezcladas no lleva la fila.
  DBI::dbExecute(con, "CREATE TABLE c (z TEXT)")
  DBI::dbExecute(con, "INSERT INTO c VALUES ('2'), ('qq')")
  sin_mezcla <- .tmc_relaciones(con, "b", "c")
  expect_identical(nrow(sin_mezcla$cobertura_pares), 0L)
})

test_that("un texto que se escribe como la clave de un BLOB no aparea con el BLOB", {
  for (texto in c("'<lupa-blob:41>'", "'b:41'", "'b:'")) {
    blob <- if (identical(texto, "'b:'")) "x''" else "x'41'"
    con <- .tmc_con(list(
      a = list(tipo = "", valores = rep(texto, 10)),
      b = list(tipo = "", valores = c(rep(blob, 9), "'zz'"))
    ))
    for (par in list(c("a", "b"), c("b", "a"))) {
      r <- .tmc_relaciones(con, par[[1L]], par[[2L]])
      expect_equal(.tmc_motor(con, par[[1L]], par[[2L]]), 0)
      rel <- r$relaciones
      expect_true(!nrow(rel) || all(rel$n_valores_comunes == 0))
    }
    # Y la columna de texto mezclada contra la de BLOB, en los dos sentidos.
    DBI::dbExecute(con, "INSERT INTO a VALUES (1)")
    r <- .tmc_relaciones(con, "a", "b")
    rel <- r$relaciones
    expect_true(!nrow(rel) || all(rel$n_valores_comunes == 0))
    DBI::dbDisconnect(con)
  }
})

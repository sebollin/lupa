# Una columna de SQLite con tipo declarado puede guardar otro tipo: un texto que
# la afinidad no convierte queda como texto en una columna INTEGER, NUMERIC o
# BOOLEAN, y una columna generada guarda lo que su expresion devuelva. El
# controlador la lee por su tipo declarado y ese texto llegaba como `0` o como
# faltante segun la fila que llegara primero, y la medicion en el motor no la
# miraba. Estas pruebas fijan que se mide `typeof()` tambien ahi y que la
# columna se lee de una sola forma en cualquier orden y por cualquier via, y que
# la columna tipada que guarda un solo tipo sigue como siempre.
testthat::skip_on_cran()
testthat::skip_if_not_installed("RSQLite")

.ttm_fecha <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")

# `valores` sin repetir salvo en la booleana, que solo tiene tres distintos.
# Cada caso: el DDL de `t`, los valores de la columna `x` o de la que la genera
# (`col`), y que se espera de la lectura.
.ttm_casos <- list(
  entero_nd = list(
    ddl = "CREATE TABLE t (x INTEGER)", col = "x",
    valores = c("1", "2", "3", "'ND'", "5"), clase = "texto"
  ),
  numeric_texto = list(
    ddl = "CREATE TABLE t (x NUMERIC)", col = "x",
    valores = c("1", "2.5", "3", "'ND'", "5"), clase = "texto"
  ),
  booleana_si = list(
    ddl = "CREATE TABLE t (x BOOLEAN)", col = "x",
    valores = c("0", "1", "'ND'", "1", "0"), clase = "texto"
  ),
  generada_entera = list(
    ddl = paste0(
      "CREATE TABLE t (a, x INTEGER GENERATED ALWAYS AS ",
      "(CASE WHEN a = 4 THEN 'ND' ELSE a END) VIRTUAL)"
    ),
    col = "a", valores = c("1", "2", "3", "4", "5"), clase = "texto"
  ),
  generada_texto_blob = list(
    ddl = paste0(
      "CREATE TABLE t (a, x TEXT GENERATED ALWAYS AS ",
      "(CASE WHEN a = 4 THEN x'4100' ELSE CAST(a AS TEXT) END) VIRTUAL)"
    ),
    col = "a", valores = c("1", "2", "3", "4", "5"), clase = "blob"
  )
)

.ttm_ordenes <- function(v) list(v, rev(v), v[c(3L, 1L, 5L, 2L, 4L)])
.ttm_ordenes3 <- function(v) list(v, rev(v), v[c(2L, 3L, 1L)])

.ttm_con <- function(caso, valores) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, caso$ddl)
  for (v in valores) {
    DBI::dbExecute(con, paste0(
      "INSERT INTO t (", caso$col, ") VALUES (", v, ")"
    ))
  }
  con
}

.ttm_perfilar <- function(con, ...) {
  avisos <- character()
  r <- withCallingHandlers(
    perfilar_dbi(con, "t", fecha = .ttm_fecha, cadenas_ausencia = "ND", ...),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(r = r, avisos = avisos)
}

.ttm_vias <- list(
  muestra = list(), bloques = list(bloque_filas = 2),
  muestra_motor = list(universo = "muestra_motor", muestra_motor = 5)
)

test_that("la columna con tipo declarado que guarda otro tipo se lee igual en cualquier orden y por todas las vias", {
  for (nombre in names(.ttm_casos)) {
    caso <- .ttm_casos[[nombre]]
    for (via in names(.ttm_vias)) {
      cifras <- lapply(.ttm_ordenes(caso$valores), function(valores) {
        con <- .ttm_con(caso, valores)
        on.exit(DBI::dbDisconnect(con))
        l <- do.call(.ttm_perfilar, c(list(con), .ttm_vias[[via]]))
        # Ni un aviso suelto del controlador: la mezcla se declara en la
        # cobertura.
        expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
        tm <- l$r$resumen_tabla$meta$lectura_tipo_mixto
        expect_identical(as.character(tm$columna), "x")
        expect_identical(as.character(tm$clase), caso$clase)
        cob <- l$r$resumen_tabla$cobertura
        expect_true(any(cob$estado == "degradado" & cob$elemento == "x" &
                          grepl("afinidad", cob$motivo, fixed = TRUE)))
        pm <- l$r$perfil_muestra$columnas
        pm <- pm[pm$columna == "x", , drop = FALSE]
        expect_identical(pm$tipo_declarado, caso$clase)
        expect_equal(pm$n_distintos, length(unique(caso$valores)))
        if (identical(caso$clase, "texto")) {
          # El `'ND'` llega como `'ND'`, y se cuenta como faltante declarado:
          # leido por el tipo declarado era `0` y nadie lo contaba.
          expect_equal(pm$n_faltantes_disfrazados, 1)
        }
        pm[, c("tipo_declarado", "n_distintos", "moda", "minimo", "maximo",
               "n_faltantes_disfrazados")]
      })
      expect_identical(cifras[[1L]], cifras[[2L]])
      expect_identical(cifras[[1L]], cifras[[3L]])
    }
  }
})

test_that("medir_dbi() por bloques mide la regla sobre la columna con tipo declarado leida de una sola forma", {
  nuc <- metricas_nucleo()
  mod <- modelo(instanciar(
    especializar(nuc$Formato, "F", expresion_regular = "^[0-9]+$"), "t", "x"
  ))
  for (nombre in c("entero_nd", "numeric_texto", "booleana_si")) {
    caso <- .ttm_casos[[nombre]]
    resultados <- lapply(.ttm_ordenes(caso$valores), function(valores) {
      con <- .ttm_con(caso, valores)
      on.exit(DBI::dbDisconnect(con))
      avisos <- character()
      r <- withCallingHandlers(
        medir_dbi(mod, con, bloque_filas = 2, estrategia_no_sql = "bloques"),
        warning = function(w) {
          avisos <<- c(avisos, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      expect_false(any(grepl("mixed type", avisos, fixed = TRUE)))
      expect_identical(nrow(r), 1L)
      c(r$numerador, r$denominador)
    })
    expect_identical(resultados[[1L]], resultados[[2L]])
    expect_identical(resultados[[1L]], resultados[[3L]])
    # El `'ND'` no es un numero: no cumple, y no es el `0` que cumplia.
    expect_equal(
      resultados[[1L]][[1L]], length(caso$valores) - 1 -
        if (identical(nombre, "numeric_texto")) 1 else 0
    )
    expect_equal(resultados[[1L]][[2L]], length(caso$valores))
  }
})

test_that("la coleccion compara la columna con tipo declarado por la misma lectura", {
  for (orden in .ttm_ordenes(c("1", "2", "3", "'ND'", "5"))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE a (k INTEGER)")
    DBI::dbExecute(con, "CREATE TABLE b (k INTEGER)")
    for (v in orden) DBI::dbExecute(con, paste0("INSERT INTO a VALUES (", v, ")"))
    for (v in c("0", "1", "2", "3", "5")) {
      DBI::dbExecute(con, paste0("INSERT INTO b VALUES (", v, ")"))
    }
    motor <- DBI::dbGetQuery(con, paste0(
      "SELECT CAST((SELECT COUNT(*) FROM a WHERE k IN (SELECT k FROM b)) AS ",
      "REAL) / (SELECT COUNT(k) FROM a) AS p"
    ))$p
    # El `'ND'` no es el `0` de `b`: cuatro de cinco.
    expect_equal(motor, 0.8)
    avisos <- character()
    r <- withCallingHandlers(
      relaciones_coleccion(
        coleccion(con, c("a", "b")), data.frame(tabla_1 = "a", tabla_2 = "b"),
        muestra = 100, umbral_cobertura = 0
      ),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    DBI::dbDisconnect(con)
    expect_false(any(grepl("mixed type", avisos, fixed = TRUE)))
    rel <- r$relaciones
    expect_identical(nrow(rel), 1L)
    expect_equal(rel$n_valores_comunes, 4)
    expect_equal(rel$cobertura_tabla1_en_tabla2, motor)
    expect_identical(nrow(r$cobertura_pares), 1L)
    expect_true(grepl("`k` (de `a`)", r$cobertura_pares$motivo, fixed = TRUE))
  }
})

test_that("la tabla tipada sin mezcla publica lo mismo que siempre", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, paste0(
    "CREATE TABLE t (id INTEGER, p NUMERIC, r REAL, s TEXT, b BOOLEAN, ",
    "g INTEGER GENERATED ALWAYS AS (id * 2) VIRTUAL)"
  ))
  for (i in 1:8) {
    DBI::dbExecute(con, sprintf(
      "INSERT INTO t (id, p, r, s, b) VALUES (%d, %s, %s, '%s', %d)",
      i, c("1", "2.5")[[1L + i %% 2L]], c("0.5", "1")[[1L + i %% 2L]],
      c("aa", "bb", "cc")[[1L + i %% 3L]], i %% 2L
    ))
  }
  l <- .ttm_perfilar(con)
  expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
  # Ninguna columna se lee como otra cosa que lo declarado, y no hay lectura
  # mezclada que declarar.
  expect_false(NROW(l$r$resumen_tabla$meta$lectura_tipo_mixto) > 0L)
  cob <- l$r$resumen_tabla$cobertura
  expect_false(any(grepl("afinidad|mezclados", cob$motivo)))
  pm <- l$r$perfil_muestra$columnas
  expect_identical(
    pm$tipo_declarado[match(c("id", "p", "r", "s", "b", "g"), pm$columna)],
    c("entero", "doble", "doble", "texto", "entero", "entero")
  )
  expect_equal(pm$maximo[pm$columna == "p"], 2.5)
  # Con la tabla en un solo tipo de almacenamiento por columna, la lectura de
  # muestra no cambia ni lleva auxiliares.
  expect_false(any(grepl("lupa_tipo_mixto|CASE typeof", unlist(
    l$r$resumen_tabla$meta$lecturas$sql
  ))))
})

test_that("un entero que no entra en 32 bits junto a un real se lee como doble en cualquier orden", {
  # El controlador lee `integer64` si el entero llega primero y trunca el real
  # (`2.5` pasaba a `2`).
  for (tipo in c("INTEGER", "NUMERIC", "BOOLEAN", "DATE")) {
    for (valores in .ttm_ordenes3(c("3000000000", "2.5", "2"))) {
      con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      DBI::dbExecute(con, paste0("CREATE TABLE t (x ", tipo, ")"))
      for (v in valores) DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
      for (via in names(.ttm_vias)) {
        l <- do.call(.ttm_perfilar, c(list(con), .ttm_vias[[via]]))
        pm <- l$r$perfil_muestra$columnas
        expect_equal(pm$mediana, 2.5)
        expect_equal(pm$media, (3000000000 + 2.5 + 2) / 3)
        expect_identical(
          as.character(l$r$resumen_tabla$meta$lectura_tipo_mixto$clase), "numeros"
        )
      }
      DBI::dbDisconnect(con)
    }
  }
  # Control: enteros chicos con reales no son una mezcla.
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (x NUMERIC)")
  for (v in c("10", "9.99", "2147483647")) {
    DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
  }
  l <- .ttm_perfilar(con)
  expect_false(NROW(l$r$resumen_tabla$meta$lectura_tipo_mixto) > 0L)
})

test_that("la coleccion lee igual el entero de mas de 32 bits con un real en una columna tipada", {
  for (valores in .ttm_ordenes3(c("3000000000", "2.5", "2"))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE a (k INTEGER)")
    DBI::dbExecute(con, "CREATE TABLE b (k INTEGER)")
    for (v in valores) DBI::dbExecute(con, paste0("INSERT INTO a VALUES (", v, ")"))
    for (v in c("2.5", "7")) DBI::dbExecute(con, paste0("INSERT INTO b VALUES (", v, ")"))
    r <- suppressWarnings(relaciones_coleccion(
      coleccion(con, c("a", "b")), data.frame(tabla_1 = "a", tabla_2 = "b"),
      muestra = 100, umbral_cobertura = 0
    ))
    DBI::dbDisconnect(con)
    # Solo el `2.5` es comun: truncado a `2` no coincidia con nada.
    expect_equal(r$relaciones$n_valores_comunes, 1)
  }
})

test_that("una columna ANY de una tabla STRICT se lee con la afinidad desconocida", {
  skip_if(
    package_version(DBI::dbGetQuery(
      DBI::dbConnect(RSQLite::SQLite(), ":memory:"), "SELECT sqlite_version() AS v"
    )$v) < "3.37.0"
  )
  for (valores in .ttm_ordenes(c("1", "'ND'", "2.5", "3", "'x'"))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE t (x ANY) STRICT")
    for (v in valores) DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
    l <- .ttm_perfilar(con)
    DBI::dbDisconnect(con)
    expect_false(any(grepl("mixed type", l$avisos, fixed = TRUE)))
    tm <- l$r$resumen_tabla$meta$lectura_tipo_mixto
    expect_identical(as.character(tm$afinidad), "desconocida")
    cob <- l$r$resumen_tabla$cobertura
    expect_false(any(grepl("afinidad num", cob$motivo, fixed = TRUE)))
    expect_equal(l$r$resumen_tabla$columnas$n_distintos, 5)
    expect_equal(l$r$perfil_muestra$columnas$n_faltantes_disfrazados, 1)
  }
})

test_that("una columna ANY fuera de STRICT declara su afinidad como desconocida", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TABLE t (x ANY)")
  for (v in c("10", "9.99", "'ND'")) {
    DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
  }
  l <- .ttm_perfilar(con)
  expect_identical(
    as.character(l$r$resumen_tabla$meta$lectura_tipo_mixto$afinidad), "desconocida"
  )
  cob <- l$r$resumen_tabla$cobertura
  expect_false(any(grepl("afinidad", cob$motivo, fixed = TRUE)))
})

test_that("un entero mayor que 2^53 con un real hace no medible la regla de valor por bloques, y 2^53 exacto no es ancho", {
  nuc <- metricas_nucleo()
  mod_formato <- modelo(instanciar(
    especializar(nuc$Formato, "F", expresion_regular = "^[0-9]+$"), "t", "x"
  ))
  mod_nonulo <- modelo(instanciar(especializar(nuc$NoNulo, "NN"), "t", "x"))
  for (tipo in c("INTEGER", "NUMERIC")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, paste0("CREATE TABLE t (x ", tipo, ")"))
    for (v in c("9007199254740993", "2.5", "2")) {
      DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
    }
    m <- suppressWarnings(suppressMessages(medir_dbi(
      mod_formato, con, estrategia_no_sql = "bloques", bloque_filas = 2
    )))
    expect_true(!length(m$resultado) || is.na(m$resultado))
    expect_true(grepl("2^53", attr(m, "cobertura_metricas")$motivo[[1L]], fixed = TRUE))
    # La regla que no mira el valor sigue.
    n <- suppressWarnings(suppressMessages(medir_dbi(
      mod_nonulo, con, estrategia_no_sql = "bloques", bloque_filas = 2
    )))
    expect_equal(n$numerador, 3)
    DBI::dbDisconnect(con)
  }
  for (par in list(c("9007199254740992", FALSE), c("9007199254740993", TRUE))) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, "CREATE TABLE t (x INTEGER)")
    for (v in c(par[[1L]], "2.5")) {
      DBI::dbExecute(con, paste0("INSERT INTO t VALUES (", v, ")"))
    }
    l <- .ttm_perfilar(con)
    DBI::dbDisconnect(con)
    expect_identical(l$r$resumen_tabla$meta$lectura_tipo_mixto$ancho, as.logical(par[[2L]]))
  }
})

test_that("el entero -2^31 solo en una columna tipada no se pierde como el faltante de R", {
  for (tipo in c("INTEGER", "NUMERIC", "BOOLEAN")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbExecute(con, paste0("CREATE TABLE t (x ", tipo, ")"))
    DBI::dbExecute(con, "INSERT INTO t VALUES (-2147483648)")
    for (via in names(.ttm_vias)) {
      l <- do.call(.ttm_perfilar, c(list(con), .ttm_vias[[via]]))
      pm <- l$r$perfil_muestra$columnas
      expect_equal(pm$minimo, -2147483648)
      expect_equal(pm$maximo, -2147483648)
    }
    DBI::dbDisconnect(con)
  }
})

# Una columna BLOB se lee igual en todo el paquete. Las reglas:
#
#   1. es BLOB la clase `blob` o una lista cuyos elementos no `NULL` son `raw`
#      -tambien toda `NULL`-, y en ella el `NULL` es faltante;
#   2. un BLOB nunca es igual a un texto o a un numero, aunque el texto escriba
#      sus bytes como los escribe el paquete por dentro (`b:<hex>`); entre dos
#      BLOB se comparan los bytes;
#   3. todos los sitios que comparan dan la misma respuesta, la del motor;
#   4. un dominio o un referencial que no se puede comparar da la respuesta del
#      motor (ninguna coincidencia) o un rechazo con motivo, nunca un conforme;
#   5. la moda de un BLOB no se publica, pero su frecuencia si, igual en memoria
#      y en SQL, con el motivo en la cobertura;
#   6. la clase `blob` y la lista de `raw` con los mismos bytes son la misma
#      columna;
#   7. lo que no es BLOB queda como estaba.
#
# Los conteos esperados se rehacen con SQLite (`COUNT(DISTINCT)`, `IN`).
testthat::skip_on_cran()

.bp_fecha <- as.POSIXct("2026-01-01", tz = "UTC")

.bp_blob <- function(...) {
  lapply(list(...), function(v) if (is.null(v)) NULL else as.raw(v))
}

.bp_hex <- function(v) paste(sprintf("%02X", as.integer(v)), collapse = "")

.bp_perfil <- function(d) suppressWarnings(perfilar(d, fecha = .bp_fecha))

.bp_col <- function(tabla, columna) {
  tabla[tabla$columna == columna, , drop = FALSE]
}

.bp_medir <- function(d, metrica, columna = "k", config = list(),
                      referencial = NULL) {
  nucleo <- c(metricas_nucleo(), metricas_referencial())
  especifica <- do.call(especializar, c(list(nucleo[[metrica]], "M"), config))
  instancia <- if (is.null(referencial)) {
    instanciar(especifica, "d", columna, nombre_instancia = "M")
  } else {
    instanciar(especifica, "d", columna, nombre_instancia = "M",
               referencial = referencial)
  }
  suppressWarnings(medir(
    modelo(instancia), list(d = d), id_medicion = "m", fecha = .bp_fecha
  ))$resultado
}

# Una tabla del motor con una columna BLOB `k` (y las de texto que se pasen).
.bp_motor <- function(motor, valores, textos = NULL) {
  con <- if (identical(motor, "duckdb")) {
    testthat::skip_if_not_installed("duckdb")
    DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  } else {
    testthat::skip_if_not_installed("RSQLite")
    DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  literal <- function(v) {
    if (is.null(v)) return("NULL")
    if (identical(motor, "duckdb")) {
      paste0("from_hex('", .bp_hex(v), "')")
    } else paste0("X'", .bp_hex(v), "'")
  }
  DBI::dbExecute(con, "CREATE TABLE d (id INTEGER, k BLOB, t VARCHAR)")
  for (i in seq_along(valores)) {
    texto <- if (is.null(textos) || is.na(textos[[i]])) "NULL" else {
      paste0("'", textos[[i]], "'")
    }
    DBI::dbExecute(con, paste0(
      "INSERT INTO d VALUES (", i, ", ", literal(valores[[i]]), ", ", texto, ")"
    ))
  }
  con
}

# La misma lista leida de SQLite, que la entrega con la clase `blob`.
.bp_clase_blob <- function(valores) {
  con <- .bp_motor("sqlite", valores)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT k FROM d ORDER BY id")$k
}

.bp_valores <- .bp_blob(c(1, 2), c(3, 4), integer(0), c(1, 2), NULL,
                        c(9, 0, 9), c(3, 4), c(1, 2))

test_that("1. el NULL de una lista BLOB es faltante, tambien si la columna es toda NULL", {
  d <- data.frame(id = 1:8)
  d$k <- .bp_valores
  f <- .bp_col(.bp_perfil(d)$columnas, "k")
  expect_identical(f$tipo_declarado, "blob")
  expect_identical(f$n_faltantes, 1L)
  expect_identical(f$n_distintos, 4L)
  expect_equal(.bp_medir(d, "NoNulo"), c(1, 1, 1, 1, 0, 1, 1, 1))

  nulos <- data.frame(id = 1:3)
  nulos$k <- list(NULL, NULL, NULL)
  f <- .bp_col(.bp_perfil(nulos)$columnas, "k")
  expect_identical(f$tipo_declarado, "blob")
  expect_identical(f$n_faltantes, 3L)
  expect_identical(f$n_distintos, 0L)
  # Un dominio BLOB no hace presente a un faltante.
  expect_equal(
    .bp_medir(nulos, "NoNulo", config = list(valores_nulos = .bp_blob(1))),
    c(0, 0, 0)
  )
})

test_that("1. una lista de NA del usuario no es BLOB", {
  d <- data.frame(id = 1:3)
  d$k <- list(NA, NA, NA)
  expect_false(identical(.bp_col(.bp_perfil(d)$columnas, "k")$tipo_declarado, "blob"))
})

test_that("1. DuckDB y SQLite: una columna BLOB toda NULL es BLOB con todos sus faltantes", {
  for (motor in c("sqlite", "duckdb")) {
    con <- .bp_motor(motor, list(NULL, NULL, NULL))
    r <- suppressWarnings(perfilar_dbi(con, "d", fecha = .bp_fecha))
    DBI::dbDisconnect(con)
    f <- .bp_col(r$perfil_muestra$columnas, "k")
    expect_identical(f$tipo_declarado, "blob")
    expect_identical(f$n_faltantes, 3L)
    expect_identical(f$n_distintos, 0L)
  }
})

test_that("2. un texto que escribe los bytes no es el BLOB: relaciones, claves, columnas duplicadas", {
  d <- data.frame(id = 1:3, t = c("b:01", "b:02", "b:03"), stringsAsFactors = FALSE)
  d$k <- .bp_blob(1, 2, 3)
  r <- suppressWarnings(detectar_relaciones(d["k"], d["t"]))
  expect_identical(r$cardinalidad, "sin_coincidencias")
  expect_identical(r$n_valores_comunes, 0L)
  claves <- detectar_claves(d)
  expect_true("k" %in% claves$columnas)
  expect_false(any(claves$redundante))
  expect_identical(nrow(.bp_perfil(d)$general$columnas_duplicadas), 0L)
})

test_that("2. un referencial de texto b:<hex> no declara conforme a un BLOB, ni al reves", {
  objetivo <- data.frame(id = 1:2)
  objetivo$k <- .bp_blob(1, 2)
  padron_texto <- referencial(data.frame(k = c("b:01", "b:02")), "k",
                              completo = TRUE, alcance = "x")
  expect_equal(.bp_medir(objetivo, "CorrectitudSemFuerte",
                         referencial = padron_texto), c(0, 0))
  padron <- data.frame(id = 1:2)
  padron$k <- .bp_blob(1, 2)
  padron_blob <- referencial(padron, "k", completo = TRUE, alcance = "x")
  texto <- data.frame(k = c("b:01", "b:02"), stringsAsFactors = FALSE)
  expect_equal(.bp_medir(texto, "CorrectitudSemFuerte",
                         referencial = padron_blob), c(0, 0))
})

test_that("2. un dominio de texto no coincide con un BLOB, ni con la escritura del objeto", {
  d <- data.frame(id = 1:5)
  d$k <- .bp_blob(c(1, 2), c(3, 4), integer(0), c(9, 9), NULL)
  for (texto in c("<raw>", "blob[2 B]", "b:0102", "0102")) {
    expect_equal(
      .bp_medir(d, "ValoresPosiblesPorExtension", config = list(valores = texto)),
      rep(0, 4)
    )
    expect_equal(.bp_medir(d, "Formato", config = list(diccionario = texto)),
                 rep(0, 4))
    expect_equal(.bp_medir(d, "NoNulo", config = list(valores_nulos = texto)),
                 c(1, 1, 1, 1, 0))
  }
  # Un vector `raw` es un byte por posicion: no es un BLOB.
  expect_equal(
    .bp_medir(d, "ValoresPosiblesPorExtension",
              config = list(valores = as.raw(c(1, 2)))),
    rep(0, 4)
  )
})

test_that("2. detectar_discordancias() compara los BLOB por sus bytes, y un BLOB contra un texto discrepa", {
  d <- data.frame(id = 1:3, t = c("b:01", "b:02", "b:03"), stringsAsFactors = FALSE)
  d$a <- .bp_clase_blob(.bp_blob(1, 2, 3))
  d$b <- .bp_clase_blob(.bp_blob(4, 5, 6))
  d$c <- .bp_blob(1, 2, 3)
  d$largo <- .bp_blob(c(1, 0, 1), c(2, 0, 2), c(3, 0, 3))
  discordantes <- function(columnas) {
    suppressWarnings(detectar_discordancias(d, senal_redundante(columnas)))$n_discordantes
  }
  expect_equal(discordantes(c("a", "b")), 3)
  expect_equal(discordantes(c("a", "c")), 0)
  expect_equal(discordantes(c("c", "t")), 3)
  expect_equal(discordantes(c("c", "largo")), 3)
})

test_that("2. comparar_perfiles() no iguala el vocabulario de un texto con el de un BLOB", {
  d <- data.frame(id = 1:40)
  d$k <- .bp_blob(1, 2)[rep(1:2, 20)]
  t <- data.frame(id = 1:40, k = rep(c("b:01", "b:02"), 20), stringsAsFactors = FALSE)
  r <- comparar_perfiles(.bp_perfil(t), .bp_perfil(d))
  expect_true(any(r$columna == "k" & r$aspecto == "tipo_declarado"))
  expect_false(any(r$columna == "k" & r$aspecto == "vocabulario" &
                     r$cambio == "sin_cambio"))
  d2 <- data.frame(id = 1:40)
  d2$k <- .bp_blob(3, 4)[rep(1:2, 20)]
  r <- comparar_perfiles(.bp_perfil(d), .bp_perfil(d2))
  expect_true(any(r$columna == "k" & r$aspecto == "vocabulario" &
                    r$cambio == "modificado"))
})

test_that("3. relaciones, referencial y dominio cuentan lo mismo que el motor", {
  testthat::skip_if_not_installed("RSQLite")
  claves <- .bp_blob(c(1, 2), c(3, 4), integer(0), c(9, 0, 9))
  usos <- .bp_blob(c(1, 2), c(3, 4), c(7, 7), NULL, integer(0), c(1, 2, 0))
  con <- .bp_motor("sqlite", claves)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE u (id INTEGER, fk BLOB)")
  for (i in seq_along(usos)) {
    v <- if (is.null(usos[[i]])) "NULL" else paste0("X'", .bp_hex(usos[[i]]), "'")
    DBI::dbExecute(con, paste0("INSERT INTO u VALUES (", i, ", ", v, ")"))
  }
  en_motor <- DBI::dbGetQuery(con, paste(
    "SELECT CASE WHEN fk IN (SELECT k FROM d) THEN 1 ELSE 0 END AS c",
    "FROM u WHERE fk IS NOT NULL ORDER BY id"
  ))$c
  comunes <- DBI::dbGetQuery(
    con, "SELECT COUNT(DISTINCT fk) AS n FROM u WHERE fk IN (SELECT k FROM d)"
  )$n
  a <- data.frame(id = seq_along(claves))
  a$k <- claves
  b <- data.frame(id = seq_along(usos))
  b$k <- usos
  r <- suppressWarnings(detectar_relaciones(a["k"], b["k"]))
  expect_equal(r$n_valores_comunes, comunes)
  expect_equal(r$cobertura_tabla2_en_tabla1, mean(en_motor))
  padron <- referencial(a, "k", completo = TRUE, alcance = "x")
  d <- b
  expect_equal(.bp_medir(d, "CorrectitudSemFuerte", referencial = padron), en_motor)
  expect_equal(
    .bp_medir(d, "ValoresPosiblesPorExtension", config = list(valores = claves)),
    en_motor
  )
  # La clase `blob` del controlador da lo mismo.
  d$k <- .bp_clase_blob(usos)
  expect_equal(.bp_medir(d, "CorrectitudSemFuerte", referencial = padron), en_motor)
})

test_that("4. medir_dbi() con un dominio BLOB: el conteo del motor por bloques, y sin bloques no publica cifra", {
  valores <- .bp_blob(c(1, 2), c(3, 4), c(1, 2), NULL)
  for (motor in c("sqlite", "duckdb")) {
    con <- .bp_motor(motor, valores)
    lit <- if (motor == "sqlite") "X'0102'" else "from_hex('0102')"
    esperado <- DBI::dbGetQuery(con, paste0(
      "SELECT AVG(CASE WHEN k IN (", lit, ") THEN 1.0 ELSE 0.0 END) AS p ",
      "FROM d WHERE k IS NOT NULL"
    ))$p
    especifica <- especializar(metricas_nucleo()$ValoresPosiblesPorExtension, "M",
                               valores = .bp_blob(c(1, 2)))
    m <- modelo(instanciar(especifica, "d", "k", nombre_instancia = "M"))
    por_bloques <- suppressWarnings(medir_dbi(
      m, con, tablas = "d", id_medicion = "m", fecha = .bp_fecha,
      estrategia_no_sql = "bloques"
    ))
    expect_equal(por_bloques$resultado, esperado)
    sin_bloques <- suppressWarnings(medir_dbi(
      m, con, tablas = "d", id_medicion = "m", fecha = .bp_fecha
    ))
    expect_identical(nrow(sin_bloques), 0L)
    expect_true(any(attr(sin_bloques, "cobertura_metricas")$estado == "no_medible"))
    DBI::dbDisconnect(con)
  }
})

test_that("4. referencial() acepta una clave BLOB y rechaza una lista que no lo es", {
  padron <- data.frame(id = 1:2)
  padron$k <- .bp_blob(1, 2)
  expect_s3_class(referencial(padron, "k"), "referencial")
  padron$k <- list("a", 1)
  expect_error(referencial(padron, "k"), "BLOB")
  padron$k <- .bp_blob(1, 1)
  expect_error(referencial(padron, "k"), "un\u00edvocamente")
})

test_that("5. la moda de un BLOB no se publica y lo dice la cobertura; el resumen SQL publica la frecuencia del motor", {
  valores <- .bp_blob(c(1, 0, 1), c(1, 0, 1), c(1, 0, 2), NULL)
  d <- data.frame(id = 1:4)
  d$k <- valores
  p <- .bp_perfil(d)
  f <- .bp_col(p$columnas, "k")
  expect_true(is.na(f$moda))
  # En R la moda de una lista no se cuenta (test-O164-mediciones.R).
  expect_true(is.na(f$frecuencia_moda))
  expect_true(any(p$cobertura_diagnosticos$diagnostico == "moda" &
                    p$cobertura_diagnosticos$columna == "k"))
  for (motor in c("sqlite", "duckdb")) {
    con <- .bp_motor(motor, valores)
    esperado <- DBI::dbGetQuery(con, paste(
      "SELECT MAX(c) AS n FROM (SELECT COUNT(*) AS c FROM d",
      "WHERE k IS NOT NULL GROUP BY k) s"
    ))$n
    r <- suppressWarnings(perfilar_dbi(con, "d", fecha = .bp_fecha))
    DBI::dbDisconnect(con)
    resumen <- .bp_col(r$resumen_tabla$columnas, "k")
    expect_true(is.na(resumen$moda))
    expect_equal(as.numeric(resumen$frecuencia_moda), as.numeric(esperado))
    expect_true("k::moda" %in% r$resumen_tabla$cobertura$elemento)
    muestra <- .bp_col(r$perfil_muestra$columnas, "k")
    expect_true(is.na(muestra$moda))
    expect_identical(muestra$tipo_declarado, "blob")
    expect_true(any(r$perfil_muestra$cobertura_diagnosticos$diagnostico == "moda" &
                      r$perfil_muestra$cobertura_diagnosticos$columna == "k"))
  }
})

test_that("6. la clase blob y la lista de raw con los mismos bytes son la misma columna", {
  d <- data.frame(id = 1:4)
  d$a <- .bp_blob(1, 2, 3, 4)
  d$b <- .bp_clase_blob(d$a)
  p <- .bp_perfil(d)
  duplicadas <- p$general$columnas_duplicadas
  expect_identical(paste(duplicadas$columna_1, duplicadas$columna_2), "a b")
  claves <- detectar_claves(d)
  expect_identical(claves$equivalente_a[claves$columnas == "a"], "b")
  r <- suppressWarnings(detectar_relaciones(d["a"], d["b"]))
  expect_identical(r$cardinalidad, "1:1")
  expect_equal(r$cobertura_tabla1_en_tabla2, 1)
  expect_identical(.bp_col(p$columnas, "a")$n_distintos,
                   .bp_col(p$columnas, "b")$n_distintos)
})

test_that("6. una clave BINARY(16) unica se detecta y se sugiere en sus dos formas", {
  uuid <- function(i) c(rep(0L, 15L), i)
  d <- data.frame(x = c("a", "a", "b", "b"))
  d$k <- do.call(.bp_blob, lapply(1:4, uuid))
  d$kb <- .bp_clase_blob(d$k)
  claves <- detectar_claves(d)
  expect_true(all(c("k", "kb") %in% claves$columnas))
  expect_true(all(c("k", "kb") %in% sugerir_clave(d)$columna))
})

test_that("7. una columna raw con un dominio raw se mide como siempre", {
  d <- data.frame(id = 1:3)
  d$r <- as.raw(c(1, 2, 3))
  expect_equal(
    .bp_medir(d, "ValoresPosiblesPorExtension", columna = "r",
              config = list(valores = as.raw(c(1, 3)))),
    c(1, 0, 1)
  )
})

test_that("1. AtributoDuplicado y EntidadDuplicada marcan todas las filas de un BLOB repetido, en sus dos formas", {
  uuid <- function(i) c(rep(0L, 15L), i)
  valores <- do.call(.bp_blob, lapply(c(1, 2, 3, 1, 4, 5, 6, 1), uuid))
  con <- .bp_motor("sqlite", valores)
  esperado <- DBI::dbGetQuery(con, paste(
    "SELECT CASE WHEN k IN (SELECT k FROM d GROUP BY k HAVING COUNT(*) > 1)",
    "THEN 1 ELSE 0 END AS c FROM d ORDER BY id"
  ))$c
  DBI::dbDisconnect(con)
  d <- data.frame(id = 1:8)
  d$k <- valores
  expect_equal(.bp_medir(d, "AtributoDuplicado"), esperado)
  d$k <- .bp_clase_blob(valores)
  expect_equal(.bp_medir(d, "AtributoDuplicado"), esperado)
  for (forma in c("lista", "blob")) {
    unica <- data.frame(id = 1:4)
    unica$k <- if (forma == "lista") .bp_blob(1, 1, 2, 1) else .bp_clase_blob(.bp_blob(1, 1, 2, 1))
    unica$id <- NULL
    r <- suppressWarnings(medir(
      modelo(instanciar(especializar(metricas_nucleo()$EntidadDuplicada, "E"),
                        "d", nombre_instancia = "E")),
      list(d = unica), id_medicion = "m", fecha = .bp_fecha
    ))$resultado
    expect_equal(r, c(1, 1, 0, 1))
  }
})

test_that("1. una columna BLOB de una tabla vacia de DuckDB es BLOB, como en SQLite", {
  tipos <- list()
  for (motor in c("sqlite", "duckdb")) {
    con <- .bp_motor(motor, list())
    r <- suppressWarnings(perfilar_dbi(con, "d", fecha = .bp_fecha))
    DBI::dbDisconnect(con)
    tipos[[motor]] <- r
    f <- .bp_col(r$perfil_muestra$columnas, "k")
    expect_identical(f$tipo_declarado, "blob")
    expect_identical(f$tipo_inferido, "blob")
  }
  cambios <- comparar_perfiles(tipos$sqlite, tipos$duckdb)
  expect_false(any(cambios$aspecto %in% c("tipo_declarado", "tipo_inferido")))
  # Una lista vacia sin el tipo del motor sigue sin decir nada.
  vacia <- data.frame(id = integer())
  vacia$k <- list()
  expect_false(identical(.bp_col(.bp_perfil(vacia)$columnas, "k")$tipo_declarado, "blob"))
})

test_that("4. Formato con expresion regular sobre un BLOB no se mide y dice por que", {
  d <- data.frame(id = 1:3)
  d$k <- .bp_blob(1, 2, 3)
  for (forma in c("lista", "blob")) {
    if (forma == "blob") d$k <- .bp_clase_blob(.bp_blob(1, 2, 3))
    especifica <- especializar(metricas_nucleo()$Formato, "F",
                               expresion_regular = "^<raw>$")
    r <- suppressWarnings(medir(
      modelo(instanciar(especifica, "d", "k", nombre_instancia = "F")),
      list(d = d), id_medicion = "m", fecha = .bp_fecha
    ))
    expect_identical(nrow(r), 0L)
    cobertura <- attr(r, "cobertura_metricas")
    expect_identical(cobertura$estado, "no_medible")
    expect_match(cobertura$motivo, "BLOB")
  }
  # Sobre un texto la expresion se aplica como siempre.
  d$t <- c("<raw>", "x", "<raw>")
  expect_equal(.bp_medir(d, "Formato", columna = "t",
                         config = list(expresion_regular = "^<raw>$")),
               c(1, 0, 1))
})

test_that("1. una lista con la clase list escrita es BLOB; con otra clase no", {
  bytes <- .bp_blob(1, 2, 3)
  d <- data.frame(id = 1:3)
  d$plano <- bytes
  d$clase <- structure(bytes, class = "list")
  d$blob <- .bp_clase_blob(bytes)
  p <- .bp_perfil(d)
  expect_identical(.bp_col(p$columnas, "clase")$tipo_declarado, "blob")
  duplicadas <- paste(p$general$columnas_duplicadas$columna_1,
                      p$general$columnas_duplicadas$columna_2)
  expect_true("plano clase" %in% duplicadas)
  r <- suppressWarnings(detectar_relaciones(d["clase"], d["blob"]))
  expect_identical(r$cardinalidad, "1:1")
  expect_true("clase" %in% detectar_claves(d)$columnas)
  otra <- data.frame(id = 1:3)
  otra$k <- structure(bytes, class = "otra_clase")
  expect_false(identical(.bp_col(.bp_perfil(otra)$columnas, "k")$tipo_declarado, "blob"))
})

.bp_cobertura <- function(d, metrica, columna = "k", config = list()) {
  especifica <- do.call(especializar, c(list(metricas_nucleo()[[metrica]], "M"), config))
  r <- suppressWarnings(medir(
    modelo(instanciar(especifica, "d", columna, nombre_instancia = "M")),
    list(d = d), id_medicion = "m", fecha = .bp_fecha
  ))
  list(resultado = r$resultado, cobertura = attr(r, "cobertura_metricas"))
}

test_that("4. DesactualizacionPorFormato, un rango y EntidadContradictoria no se miden sobre un BLOB y dicen por que", {
  uuid <- function(i) c(rep(0L, 15L), i)
  for (forma in c("lista", "blob")) {
    d <- data.frame(id = 1:3)
    d$k <- do.call(.bp_blob, lapply(1:3, uuid))
    if (forma == "blob") d$k <- .bp_clase_blob(d$k)
    for (caso in list(
      list("DesactualizacionPorFormato", list(expresion_regular = "^<raw>$")),
      list("DesactualizacionPorFormato", list(expresion_regular = "^0[1-3]$")),
      list("ValoresPosiblesPorComprension", list(minimo = 0, maximo = 10)),
      list("EntidadContradictoria", list())
    )) {
      r <- .bp_cobertura(d, caso[[1]], config = caso[[2]])
      expect_length(r$resultado, 0L)
      expect_identical(r$cobertura$estado, "no_medible")
      expect_match(r$cobertura$motivo, "BLOB")
    }
  }
  # Sobre un texto se miden como siempre.
  d <- data.frame(id = 1:3, t = c("<raw>", "x", "<raw>"), stringsAsFactors = FALSE)
  expect_equal(.bp_cobertura(d, "DesactualizacionPorFormato", "t",
                             list(expresion_regular = "^<raw>$"))$resultado,
               c(0, 1, 0))
})

test_that("4. el predicado de ValoresPosiblesPorComprension recibe los bytes en las dos formas", {
  uuid <- function(i) c(rep(0L, 15L), i)
  bytes <- do.call(.bp_blob, lapply(1:3, uuid))
  ultimo_par <- function(x) vapply(x, function(v) as.integer(v[[16L]]) %% 2L == 0L, logical(1L))
  for (forma in c("lista", "blob")) {
    d <- data.frame(id = 1:3)
    d$k <- if (forma == "lista") bytes else .bp_clase_blob(bytes)
    expect_equal(.bp_medir(d, "ValoresPosiblesPorComprension",
                           config = list(predicado = ultimo_par)),
                 c(0, 1, 0))
  }
})

test_that("6. EntidadDuplicada con atributos compara un BLOB de 16 bytes entre los demas valores", {
  uuid <- function(i) c(rep(0L, 15L), i)
  for (forma in c("lista", "blob")) {
    d <- data.frame(g = c("a", "a", "b", "b"), stringsAsFactors = FALSE)
    d$k <- do.call(.bp_blob, lapply(c(1, 2, 3, 3), uuid))
    if (forma == "blob") d$k <- .bp_clase_blob(d$k)
    r <- suppressWarnings(medir(
      modelo(instanciar(especializar(metricas_nucleo()$EntidadDuplicada, "E"),
                        "d", "g", nombre_instancia = "E")),
      list(d = d), id_medicion = "m", fecha = .bp_fecha
    ))$resultado
    expect_equal(r, c(0, 0, 1, 1))
    # Un vacio o un faltante entre los demas valores: el vacio es un valor, el
    # faltante es compatible con cualquiera.
    d$k <- .bp_blob(integer(0), 1, NULL, 2)
    if (forma == "blob") d$k <- .bp_clase_blob(d$k)
    r <- suppressWarnings(medir(
      modelo(instanciar(especializar(metricas_nucleo()$EntidadDuplicada, "E"),
                        "d", "g", nombre_instancia = "E")),
      list(d = d), id_medicion = "m", fecha = .bp_fecha
    ))$resultado
    expect_equal(r, c(0, 0, 1, 1))
  }
})

test_that("7. la copia que conserva analizar() es la entrada: el NULL de un BLOB queda NULL", {
  d <- data.frame(id = 1:4, g = c("a", "b", "a", "b"), stringsAsFactors = FALSE)
  d$k <- .bp_blob(1, NULL, c(2, 3), NULL)
  an <- suppressWarnings(suppressMessages(analizar(
    d, conservar_datos = TRUE, fecha = .bp_fecha
  )))
  expect_identical(an$datos$k, d$k)
  expect_identical(.bp_col(an$perfil$columnas, "k")$n_faltantes, 2L)
})

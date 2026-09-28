# Dos cosas del frente DBI, las dos de la misma forma: una LISTA donde deberia haber
# una medicion.

test_that("una columna compuesta cuenta filas y no campos", {
  # `length()` de un `data.frame` es su numero de COLUMNAS. Una columna cuyo valor es
  # un data.frame -lo que devuelve un driver al leer un `STRUCT`- publicaba `n = 2`
  # sobre una tabla de tres filas, mientras `general$filas` decia 3. Por DBI eso
  # dejaba al bloque de muestra discrepando del resumen SQL sin que nada declarara la
  # diferencia. Esta mitad no necesita ningun motor: es de `perfilar()`.
  interior <- data.frame(
    a = c(1L, 1L, 2L), b = c("x", "x", "y"), stringsAsFactors = FALSE
  )
  datos <- data.frame(id = 1:3)
  datos$st <- interior

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- columnas(perfil)
  fila <- fila[fila$columna == "st", , drop = FALSE]

  expect_identical(as.numeric(fila$n), 3)
  expect_identical(as.numeric(perfil$general$filas), 3)
  # Y la columna corriente de la misma tabla no cambia.
  ordinaria <- columnas(perfil)
  expect_identical(
    as.numeric(ordinaria$n[ordinaria$columna == "id"]), 3
  )
})

test_that("un entero de 64 bits que llega como doble no publica mediana ni desvio", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  # El roxygen dice que si una columna declarada entero de 64 bits llega como doble
  # con un valor de al menos 2^53, el resumen deja sus metricas de magnitud en
  # `no_disponible`. La guarda recorria CINCO metricas -`minimo`, `maximo`, `media`,
  # `n_ceros`, `n_negativos`- y `mediana` y `desvio` se calculan en otras consultas:
  # salian `calculado` en la misma corrida. La condicion es de la COLUMNA, no de la
  # metrica.
  archivo <- tempfile(fileext = ".duckdb")
  on.exit(unlink(archivo, force = TRUE), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = archivo)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE bigs (v BIGINT)")
  DBI::dbExecute(con, paste(
    "INSERT INTO bigs VALUES (9007199254740993),(9007199254740994),",
    "(9007199254740995),(9007199254740996)"
  ))

  resultado <- perfilar_dbi(
    con, "bigs", universo = "tabla_completa", muestra = Inf,
    avisar_costo_distintos = FALSE, avisar_costo_moda = FALSE,
    avisar_costo_mediana = FALSE, avisar_derrame_estimado = FALSE
  )
  sql <- resultado$resumen_tabla$sql
  estado_de <- function(metrica) {
    as.character(sql$estado[sql$columna == "v" & sql$metrica == metrica])
  }

  for (metrica in c("minimo", "maximo", "media", "mediana", "desvio")) {
    expect_identical(estado_de(metrica), "no_disponible", info = metrica)
  }
  columna <- resultado$resumen_tabla$columnas
  expect_true(is.na(columna$mediana))
  expect_true(is.na(columna$desvio))
  # Lo que el motor SI cuenta con exactitud se sigue publicando: la guarda es sobre
  # la magnitud, no sobre la columna entera.
  expect_identical(as.numeric(columna$n_distintos), 4)
})

test_that("con enteros chicos, la magnitud se publica como siempre", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  # Mitad de control: la guarda no puede apagar la magnitud de cualquier `BIGINT`.
  archivo <- tempfile(fileext = ".duckdb")
  on.exit(unlink(archivo, force = TRUE), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = archivo)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE chicos (v BIGINT)")
  DBI::dbExecute(con, "INSERT INTO chicos VALUES (10),(20),(30),(40)")

  resultado <- perfilar_dbi(
    con, "chicos", universo = "tabla_completa", muestra = Inf,
    avisar_costo_distintos = FALSE, avisar_costo_moda = FALSE,
    avisar_costo_mediana = FALSE, avisar_derrame_estimado = FALSE
  )
  sql <- resultado$resumen_tabla$sql
  estado_de <- function(metrica) {
    as.character(sql$estado[sql$columna == "v" & sql$metrica == metrica])
  }

  expect_identical(estado_de("media"), "calculado")
  expect_identical(estado_de("mediana"), "calculado")
  expect_identical(estado_de("desvio"), "calculado")
  expect_identical(as.numeric(resultado$resumen_tabla$columnas$media), 25)
})

test_that("una columna STRUCT no discrepa consigo misma entre los dos bloques", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  archivo <- tempfile(fileext = ".duckdb")
  on.exit(unlink(archivo, force = TRUE), add = TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = archivo)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "CREATE TABLE ests (st STRUCT(a INTEGER, b VARCHAR))")
  DBI::dbExecute(con, paste(
    "INSERT INTO ests VALUES ({'a': 1, 'b': 'uno'}),",
    "({'a': 1, 'b': 'uno'}), ({'a': 2, 'b': 'dos'})"
  ))

  resultado <- perfilar_dbi(
    con, "ests", universo = "tabla_completa", muestra = Inf,
    avisar_costo_distintos = FALSE, avisar_costo_moda = FALSE,
    avisar_costo_mediana = FALSE, avisar_derrame_estimado = FALSE
  )

  expect_identical(as.numeric(resultado$resumen_tabla$columnas$n), 3)
  muestra <- columnas(resultado$perfil_muestra)
  expect_identical(as.numeric(muestra$n[muestra$columna == "st"]), 3)
  expect_identical(as.numeric(resultado$perfil_muestra$general$filas), 3)
})

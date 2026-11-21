testthat::skip_on_cran()

# El nombre separa una clave de un conteo. Un numero de algo seguido de un
# sustantivo en plural cuenta (`num_votos`, `number_of_items`); seguido de uno
# en singular, o precedido por el sustantivo (`order_number`), identifica. La
# palabra castellana `orden` es ambigua y queda del lado que no se acusa. La
# lista de dinero es una sola y `costo` pesa lo mismo que `monto`.

.hallazgo_identificador <- function(nombre, valores) {
  d <- data.frame(x = valores, stringsAsFactors = FALSE)
  names(d) <- nombre
  h <- as.data.frame(hallazgos(perfilar(d)))
  h[h$tipo_hallazgo == "identificador_con_ausentes", , drop = FALSE]
}

.valores_con_ausentes <- function(n = 500, desde = 1000L, hasta = 9000L) {
  set.seed(20261120)
  valores <- sample(desde:hasta, n - 10L)
  replace(valores, seq_len(10L), NA_integer_)
}

test_that("un numero de algo en plural es un conteo y no un identificador", {
  # Valores por encima del numero de filas: la unica razon para no acusarlos
  # es el nombre, no el maximo.
  conteos <- c(
    "num_votos", "num_hijos", "numero_visitas", "num_clientes", "num_paises",
    "num_ordenes", "numero_de_hijos", "num_visits", "number_of_items",
    "cantidad_items"
  )
  for (nombre in conteos) {
    expect_equal(
      nrow(.hallazgo_identificador(nombre, .valores_con_ausentes())), 0L,
      info = nombre
    )
  }
})

test_that("un numero de algo en singular es un identificador", {
  identificadores <- c(
    "num_documento", "numero_cuenta", "nro_factura", "num_pedido",
    "invoice_number", "customer_number", "order_number"
  )
  for (nombre in identificadores) {
    valores <- replace(seq_len(500), seq_len(5L), NA_integer_)
    h <- .hallazgo_identificador(nombre, valores)
    expect_equal(nrow(h), 1L, info = nombre)
    expect_equal(h$columna, nombre, info = nombre)
  }
})

test_that("el numero de orden en castellano es ambiguo y no se acusa", {
  # "Numero de orden" puede ser una posicion en una lista o un pedido. Ante la
  # ambiguedad el paquete no afirma; el ingles `order_number` si es un pedido.
  for (nombre in c("orden", "nro_orden", "numero_orden", "num_orden",
                   "orden_numero", "numero_de_orden", "ordenes")) {
    expect_equal(
      nrow(.hallazgo_identificador(nombre, .valores_con_ausentes())), 0L,
      info = nombre
    )
  }
  expect_equal(
    nrow(.hallazgo_identificador("order_number", .valores_con_ausentes())), 1L
  )
})

test_that("una clave numerica con ausentes se informa aunque sea una numeracion", {
  # La clave autoincremental con faltantes: el caso que el aviso tiene que
  # decir. No importa si empieza en 1, en 2 o lejos.
  casos <- list(
    id_cliente = c(1:970, rep(NA_integer_, 30)),
    id_desplazada = replace(2:501, seq_len(5L), NA_integer_),
    id_lejana = replace(1001:1500, seq_len(5L), NA_integer_)
  )
  for (nombre in names(casos)) {
    h <- .hallazgo_identificador(nombre, casos[[nombre]])
    expect_equal(nrow(h), 1L, info = nombre)
    expect_equal(as.character(h$severidad), "ok", info = nombre)
  }
  # El nombre sigue dejando afuera al conteo y a la numeracion declarada.
  expect_equal(
    nrow(.hallazgo_identificador(
      "conteo", replace(seq_len(500), seq_len(5L), NA_integer_)
    )), 0L
  )
  expect_equal(
    nrow(.hallazgo_identificador(
      "id_secuencia", replace(seq_len(500), seq_len(5L), NA_integer_)
    )), 0L
  )
})

test_that("las palabras de dinero comparten una sola lista", {
  for (nombre in c("id_costo", "costo_id", "cost_id", "id_costos")) {
    expect_equal(
      nrow(.hallazgo_identificador(nombre, .valores_con_ausentes())), 0L,
      info = nombre
    )
  }
})

test_that("el segundo intercalar no se declara perdido al convertir una hora", {
  base <- format(
    as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + seq_len(100) * 60,
    "%H:%M:%S"
  )
  d <- data.frame(id = 1:100, hora = base, stringsAsFactors = FALSE)
  d$hora[1] <- "23:59:60"
  h <- as.data.frame(hallazgos(perfilar(d)))
  expect_equal(
    nrow(h[h$tipo_hallazgo == "hora_invalida", , drop = FALSE]), 0L
  )
  tipo <- h[h$tipo_hallazgo == "tipo_declarado_distinto", , drop = FALSE]
  if (nrow(tipo)) {
    expect_false(grepl("valores en NA", tipo$evidencia[[1L]], fixed = TRUE))
    expect_match(tipo$evidencia[[1L]], "no pierde", fixed = TRUE)
  }
})

test_that("el segundo intercalar no se acusa, pero su conversion no se recomienda", {
  # `23:59:60` es una escritura valida y el diagnostico no lo acusa como fecha
  # imposible; pero `POSIXct` no representa el segundo 60, asi que convertir
  # perderia ese valor. El plan lo dice y no convierte la columna solo.
  base <- format(
    as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + seq_len(100) * 3600,
    "%Y-%m-%d %H:%M:%S"
  )
  d <- data.frame(id = 1:100, f = base, stringsAsFactors = FALSE)
  d$f[1] <- "2016-12-31 23:59:60"
  r <- analizar(
    d, fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )
  h <- as.data.frame(r$perfil$hallazgos)
  expect_false(any(h$tipo_hallazgo %in% c("fecha_invalida", "hora_invalida")))
  tipo <- h[h$tipo_hallazgo == "tipo_declarado_distinto", , drop = FALSE]
  expect_equal(nrow(tipo), 1L)
  expect_true(grepl("1 de 100", tipo$evidencia[[1L]], fixed = TRUE))
  plan <- r$plan
  fila <- plan$estrategia == "convertir_tipo" & plan$columna == "f"
  expect_equal(sum(fila), 1L)
  expect_false(plan$recomendada[[which(fila)]])
  expect_false(plan$aplicar[[which(fila)]])
})

test_that("la muestra del motor no nombra un total que no conoce", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not(exists(".duckdb_prueba", mode = "function"))
  con <- DBI::dbConnect(.duckdb_prueba(), dbdir = ":memory:")
  on.exit(try(duckdb::duckdb_shutdown(con), silent = TRUE), add = TRUE)
  d <- data.frame(id_cliente = c(1001:1970, rep(NA_integer_, 30)))
  DBI::dbWriteTable(con, "t", d)
  p <- suppressWarnings(perfilar_dbi(
    con, "t", universo = "muestra_motor", muestra_motor = 500
  ))
  cobertura <- p$perfil_muestra$cobertura_diagnosticos
  motivos <- as.character(cobertura$motivo)
  expect_false(any(grepl("de las NA de la tabla", motivos, fixed = TRUE)))
  fila <- cobertura$diagnostico == "identificador_con_ausentes"
  expect_equal(sum(fila), 1L)
  expect_match(cobertura$motivo[[which(fila)]], "500 filas de la tabla",
               fixed = TRUE)
})

test_that("perfilar_por informa la clave con ausentes en su grupo", {
  d <- data.frame(
    g = rep(c("a", "b"), each = 300),
    id = c(replace(1:300, seq_len(3L), NA_integer_), 301:600)
  )
  pp <- as.data.frame(perfilar_por(d, por = "g"))
  x <- pp[pp$grupo == "a" & pp$tipo_hallazgo ==
            "identificador_con_ausentes", , drop = FALSE]
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$columna), "id")
  expect_equal(
    nrow(pp[pp$grupo == "b" & pp$tipo_hallazgo ==
              "identificador_con_ausentes", , drop = FALSE]), 0L
  )
})

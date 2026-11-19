testthat::skip_on_cran()

# Dos defectos de validez de un codigo que el perfil medido por omision no
# nombraba: el relleno de ceros perdido (`00123` pasa a `123`) y el digito
# verificador que no se cumple. Los dos se prueban con datos sinteticos en
# ASCII y con los casos legitimos que no deben acusarse.

.tipos_de <- function(perfil, tipo, columna = NULL) {
  h <- as.data.frame(perfil$hallazgos)
  h <- h[h$tipo_hallazgo == tipo, , drop = FALSE]
  if (!is.null(columna)) h <- h[h$columna == columna, , drop = FALSE]
  h
}

.filas_traza <- function(perfil, tipo) {
  h <- perfil$hallazgos
  i <- which(h$tipo_hallazgo == tipo)[[1L]]
  sort(h$trazabilidad[[i]]$indices)
}

.codigos_con_ceros_perdidos <- function(n = 400L, perdidos = 24L, semilla = 11L) {
  set.seed(semilla)
  cod <- sprintf("%05d", sample(1:99999, n, TRUE))
  # Con relleno en los que quedan: un cinco por ciento de los valores empieza con
  # cero, que es lo que el paquete exige para llamar "relleno" al largo dominante.
  cod[1:5] <- c("00012", "00345", "04567", "00099", "01234")
  filas <- sort(sample(6:n, perdidos))
  cod[filas] <- sub("^0+", "", sprintf("%05d", sample(100:999, perdidos)))
  list(datos = data.frame(cod = cod, stringsAsFactors = FALSE), filas = filas)
}

test_that("los ceros perdidos de un codigo salen como hallazgo con sus filas", {
  caso <- .codigos_con_ceros_perdidos()
  p <- suppressWarnings(perfilar(caso$datos, nombre = "t"))
  h <- .tipos_de(p, "largo_codigo_inconsistente", "cod")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "sospechoso")
  expect_equal(h$n_afectados, length(caso$filas))
  expect_equal(h$n_evaluados, nrow(caso$datos))
  expect_identical(.filas_traza(p, "largo_codigo_inconsistente"), caso$filas)
  # La evidencia dice largos y conteos, no valores.
  expect_false(grepl("00012|00345", h$evidencia))
})

test_that("el largo inconsistente tambien se ve en un codigo con prefijo", {
  set.seed(3)
  cod <- sprintf("AB-%05d", sample(1:99999, 300L, TRUE))
  cod[1:4] <- c("AB-00007", "AB-00100", "AB-01000", "AB-00033")
  cortos <- c(10L, 50L, 120L)
  cod[cortos] <- c("AB-7", "AB-33", "AB-121")
  p <- suppressWarnings(perfilar(data.frame(cod = cod), nombre = "t"))
  expect_identical(.filas_traza(p, "largo_codigo_inconsistente"), cortos)
})

test_that("`umbral_patron_raro = 0` apaga el aviso de largo", {
  caso <- .codigos_con_ceros_perdidos()
  p <- suppressWarnings(perfilar(caso$datos, nombre = "t", umbral_patron_raro = 0))
  expect_equal(nrow(.tipos_de(p, "largo_codigo_inconsistente")), 0L)
})

test_that("los codigos sanos o de largo legitimamente mixto no se acusan", {
  set.seed(21)
  n <- 400L
  sanos <- list(
    # Un numero sin relleno tiene valores de todos los largos: ninguno esta mal.
    sin_relleno = as.character(sample(1:99999, n, TRUE)),
    secuencia_rellena = sprintf("%05d", seq_len(n)),
    # Dos largos a mitades: no hay uno dominante.
    mitad_y_mitad = sample(c("00123", "123"), n, TRUE),
    # Telefonos con y sin prefijo de area, a partes parecidas.
    telefonos = sample(c(sprintf("09%07d", 1234567L), "21234567"), n, TRUE),
    # Codigos postales de dos paises, sin ceros de relleno.
    postales = sample(c("11000", "11800", "1425", "5000"), n, TRUE,
                      prob = c(0.4, 0.4, 0.1, 0.1)),
    texto_libre = sample(c("Apto 5", "Apto 12", "Apto 301", "Casa 7"), n, TRUE),
    fechas = sprintf("2020-%02d-%02d", sample(1:12, n, TRUE), sample(1:28, n, TRUE)),
    decimales = sprintf("%.2f", stats::runif(n, 0, 100)),
    # Cada prefijo con su propio ancho.
    prefijos = c(sprintf("AB-%05d", sample(1:99999, n / 2, TRUE)),
                 sprintf("XYZ-%03d", sample(1:999, n / 2, TRUE))),
    # Un codigo que desbordo su ancho: los de mas digitos no se acusan.
    desborda = c(sprintf("%04d", sample(1:9999, n - 4L, TRUE)),
                 "10001", "10002", "10003", "10004")
  )
  for (nombre in names(sanos)) {
    p <- suppressWarnings(perfilar(
      data.frame(v = sanos[[nombre]], stringsAsFactors = FALSE), nombre = nombre
    ))
    expect_equal(nrow(.tipos_de(p, "largo_codigo_inconsistente")), 0L, info = nombre)
  }
})

test_that("el largo inconsistente llega por perfilar_por() y por el motor", {
  caso <- .codigos_con_ceros_perdidos()
  datos <- caso$datos
  datos$g <- rep(c("a", "b"), length.out = nrow(datos))
  por <- suppressWarnings(perfilar_por(datos, "g", analizar_dependencias = FALSE))
  expect_true(any(por$tipo_hallazgo == "largo_codigo_inconsistente"))
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", caso$datos)
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(con, "t")))
  hd <- as.data.frame(hallazgos(pd))
  expect_equal(
    sum(hd$n_afectados[hd$tipo_hallazgo == "largo_codigo_inconsistente"]),
    length(caso$filas)
  )
})

test_that("comparar_perfiles() dice que el largo inconsistente aparecio o se resolvio", {
  caso <- .codigos_con_ceros_perdidos()
  arreglado <- caso$datos
  arreglado$cod <- sprintf("%05d", as.integer(arreglado$cod))
  malo <- suppressWarnings(perfilar(caso$datos, nombre = "t"))
  bueno <- suppressWarnings(perfilar(arreglado, nombre = "t"))
  r <- suppressWarnings(comparar_perfiles(malo, bueno))
  fila <- r[r$aspecto == "hallazgo" & r$valor_anterior %in% "largo_codigo_inconsistente", ]
  expect_identical(as.character(fila$cambio), "resuelto")
  r2 <- suppressWarnings(comparar_perfiles(bueno, malo))
  fila2 <- r2[r2$aspecto == "hallazgo" & r2$valor_actual %in% "largo_codigo_inconsistente", ]
  expect_identical(as.character(fila2$cambio), "aparecido")
})

test_that("el plan de limpieza no toca el codigo: declara el hallazgo sin accion", {
  caso <- .codigos_con_ceros_perdidos()
  p <- suppressWarnings(perfilar(caso$datos, nombre = "t"))
  plan <- suppressWarnings(planificar_limpieza(p))
  sin_accion <- attr(plan, "hallazgos_sin_accion")
  expect_true("largo_codigo_inconsistente" %in% sin_accion$hallazgo)
  # Ninguna accion del plan sobre esa columna restituye ni borra ceros sola.
  expect_false(any(plan$estrategia %in% "restituir_ceros"))
  expect_true(all(plan$destructiva[plan$estrategia %in% "convertir_tipo"]))
})

# ---- digito verificador ------------------------------------------------------

# El digito de una cedula uruguaya de siete cifras: pesos 2987634 mod 10.
.digito_ci <- function(base) {
  pesos <- c(2, 9, 8, 7, 6, 3, 4)
  vapply(base, function(b) {
    d <- as.integer(strsplit(sprintf("%07d", b), "")[[1L]])
    (10 - sum(d * pesos) %% 10) %% 10
  }, numeric(1L))
}

.cedulas <- function(n = 400L, malas = integer(), semilla = 5L) {
  set.seed(semilla)
  base <- sample(1000000:4900000, n)
  digito <- .digito_ci(base)
  digito[malas] <- (digito[malas] + 1) %% 10
  sprintf("%d%d", base, digito)
}

test_that("un digito verificador que no se cumple se nombra con sus filas", {
  malas <- c(3L, 40L, 41L, 99L, 200L, 399L)
  d <- data.frame(documento = .cedulas(malas = malas), stringsAsFactors = FALSE)
  p <- suppressWarnings(perfilar(d, nombre = "t"))
  h <- .tipos_de(p, "digito_verificador_invalido", "documento")
  expect_equal(nrow(h), 1L)
  expect_identical(as.character(h$severidad), "sospechoso")
  expect_equal(h$n_afectados, length(malas))
  expect_equal(h$n_evaluados, 400)
  expect_identical(.filas_traza(p, "digito_verificador_invalido"), malas)
  expect_match(h$evidencia, "cedula", fixed = TRUE)
  # La tabla publicada de datos personales no cambia de forma.
  expect_null(attr(p$datos_personales, "validador_por_columna"))
})

test_that("el verificador es el del pack que se recibe, no uno fijo", {
  # Un verificador propio: el ultimo digito es la suma de los demas, modulo 10.
  suma <- function(x) {
    vapply(x, function(v) {
      d <- as.integer(strsplit(v, "")[[1L]])
      length(d) >= 2L && d[[length(d)]] == sum(d[-length(d)]) %% 10L
    }, logical(1L))
  }
  set.seed(9)
  base <- sample(1000000:9999999, 300L)
  doc <- vapply(base, function(b) {
    d <- as.integer(strsplit(as.character(b), "")[[1L]])
    paste0(b, sum(d) %% 10L)
  }, character(1L))
  rotos <- c(7L, 8L, 150L)
  doc[rotos] <- paste0(substr(doc[rotos], 1, 7), (as.integer(substr(doc[rotos], 8, 8)) + 1L) %% 10L)
  p <- suppressWarnings(perfilar(
    data.frame(documento = doc, stringsAsFactors = FALSE), nombre = "t",
    validadores_personales = list(suma_modulo_10 = suma)
  ))
  expect_identical(.filas_traza(p, "digito_verificador_invalido"), rotos)
  expect_match(.tipos_de(p, "digito_verificador_invalido")$evidencia, "suma_modulo_10")
})

test_that("no se acusa verificador donde no hay documentos que verificar", {
  # Todos validos.
  p <- suppressWarnings(perfilar(
    data.frame(documento = .cedulas()), nombre = "t"
  ))
  expect_equal(nrow(.tipos_de(p, "digito_verificador_invalido")), 0L)
  # Mayoria invalida: la columna no es de esos documentos.
  p <- suppressWarnings(perfilar(
    data.frame(documento = .cedulas(malas = 1:300)), nombre = "t"
  ))
  expect_equal(nrow(.tipos_de(p, "digito_verificador_invalido")), 0L)
  # Una columna de enteros de ocho cifras con nombre de documento.
  set.seed(4)
  p <- suppressWarnings(perfilar(
    data.frame(documento = as.character(sample(10000000:90000000, 400L))),
    nombre = "t"
  ))
  expect_equal(nrow(.tipos_de(p, "digito_verificador_invalido")), 0L)
  # Sin validadores no hay con que verificar.
  d <- data.frame(documento = .cedulas(malas = 1:10))
  p <- suppressWarnings(perfilar(d, nombre = "t", validadores_personales = FALSE))
  expect_equal(nrow(.tipos_de(p, "digito_verificador_invalido")), 0L)
})

test_that("documentos de otro pais en la misma columna se nombran sin acusar de mas", {
  ci <- .cedulas()
  set.seed(8)
  extranjeros <- 1:20
  ci[extranjeros] <- sprintf("%d", sample(10000000:99999999, 20L))
  p <- suppressWarnings(perfilar(data.frame(documento = ci), nombre = "t"))
  h <- .tipos_de(p, "digito_verificador_invalido")
  expect_equal(nrow(h), 1L)
  # Los extranjeros fallan casi todos; alguno pasa por azar y no se cuenta.
  expect_true(h$n_afectados >= 15 && h$n_afectados <= 20)
  expect_identical(as.character(h$severidad), "sospechoso")
})

test_that("con proteccion de datos personales el hallazgo no publica valores", {
  malas <- c(3L, 40L, 41L)
  ci <- .cedulas(malas = malas)
  d <- data.frame(documento = ci, stringsAsFactors = FALSE)
  p <- suppressWarnings(perfilar(d, nombre = "t", proteger_datos_personales = TRUE))
  h <- .tipos_de(p, "digito_verificador_invalido")
  expect_equal(nrow(h), 1L)
  texto <- paste(unlist(h[c("descripcion", "evidencia", "sugerencia")]), collapse = " ")
  for (valor in ci[malas]) expect_false(grepl(valor, texto, fixed = TRUE))
  # Y la evidencia sin proteger tampoco los trae.
  sin <- suppressWarnings(perfilar(d, nombre = "t"))
  evidencia <- .tipos_de(sin, "digito_verificador_invalido")$evidencia
  for (valor in ci[malas]) expect_false(grepl(valor, evidencia, fixed = TRUE))
})

test_that("el verificador llega por el motor y se compara entre perfiles", {
  malas <- c(3L, 40L, 41L, 99L)
  d <- data.frame(documento = .cedulas(malas = malas), stringsAsFactors = FALSE)
  malo <- suppressWarnings(perfilar(d, nombre = "t"))
  bueno <- suppressWarnings(perfilar(data.frame(documento = .cedulas()), nombre = "t"))
  r <- suppressWarnings(comparar_perfiles(malo, bueno))
  fila <- r[r$aspecto == "hallazgo" & r$valor_anterior %in% "digito_verificador_invalido", ]
  expect_identical(as.character(fila$cambio), "resuelto")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(con, "t")))
  hd <- as.data.frame(hallazgos(pd))
  expect_equal(
    sum(hd$n_afectados[hd$tipo_hallazgo == "digito_verificador_invalido"]),
    length(malas)
  )
})

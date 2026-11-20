testthat::skip_on_cran()

# Cuatro huecos del perfil: horas y fechas escritas como texto que no existen,
# columnas casi constantes, identificadores con ausentes, y dos falsos positivos
# que la casi constante producia (relaciones funcionales vacias y casi
# duplicados de vocabulario entre sus dos valores).

hallazgos_de <- function(d, tipo, ...) {
  h <- as.data.frame(hallazgos(perfilar(d, ...)))
  h[h$tipo_hallazgo == tipo, , drop = FALSE]
}
filas_de <- function(h) h$trazabilidad[[1L]]$indices_fila

horas_validas <- function(n) {
  sprintf("%02d:%02d", (seq_len(n) * 7L) %% 24L, (seq_len(n) * 13L) %% 60L)
}
fechas_validas <- function(n) {
  format(as.Date("2022-01-01") + (seq_len(n) * 11L) %% 300L)
}

test_that("hora_invalida nombra las filas con minutos o horas que no existen", {
  d <- data.frame(id = 1:300, hora = horas_validas(300), stringsAsFactors = FALSE)
  d$hora[c(3, 9)] <- c("25:10", "12:61")
  d$hora[20] <- "24:00"
  h <- hallazgos_de(d, "hora_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(h$columna, "hora")
  expect_equal(h$n_afectados, 2)
  expect_equal(h$n_evaluados, 300)
  expect_equal(sort(filas_de(h)), c(3L, 9L))
  expect_match(h$evidencia, "24:00", fixed = TRUE)
})

test_that("24:00 sola, como fin del dia, no es una hora invalida", {
  d <- data.frame(id = 1:300, hora = horas_validas(300), stringsAsFactors = FALSE)
  d$hora[c(20, 21, 22)] <- "24:00"
  expect_equal(nrow(hallazgos_de(d, "hora_invalida")), 0L)
})

test_that("una duracion con muchas horas de 24 o mas no se acusa", {
  d <- data.frame(
    id = 1:300,
    dur = sprintf("%02d:%02d", (seq_len(300) * 7L) %% 41L, (seq_len(300) * 13L) %% 60L),
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(hallazgos_de(d, "hora_invalida")), 0L)
})

test_that("umbral_patron_raro = 0 apaga hora_invalida y fecha_invalida", {
  d <- data.frame(id = 1:300, hora = horas_validas(300),
                  fec = fechas_validas(300), stringsAsFactors = FALSE)
  d$hora[3] <- "12:61"
  d$fec[5] <- "2022-02-30"
  expect_equal(nrow(hallazgos_de(d, "hora_invalida")), 1L)
  expect_equal(nrow(hallazgos_de(d, "fecha_invalida")), 1L)
  expect_equal(nrow(hallazgos_de(d, "hora_invalida", umbral_patron_raro = 0)), 0L)
  expect_equal(nrow(hallazgos_de(d, "fecha_invalida", umbral_patron_raro = 0)), 0L)
})

test_that("fecha_invalida distingue el anio bisiesto y no acusa mes/dia ambiguos", {
  d <- data.frame(id = 1:300, fec = fechas_validas(300), stringsAsFactors = FALSE)
  d$fec[c(4, 5, 6)] <- c("2023-02-29", "2022-02-30", "0000-00-00")
  d$fec[7] <- "2024-02-29"
  h <- hallazgos_de(d, "fecha_invalida")
  expect_equal(nrow(h), 1L)
  expect_equal(sort(filas_de(h)), c(4L, 5L, 6L))
  expect_equal(h$n_afectados, 3)

  # Con barras, 02/13/2022 es valida como mes/dia: no se acusa. 31/02/2022 no
  # existe de ninguna de las dos formas.
  e <- data.frame(
    id = 1:300,
    f = format(as.Date("2022-01-01") + (seq_len(300) * 11L) %% 28L, "%d/%m/%Y"),
    stringsAsFactors = FALSE
  )
  e$f[c(1, 2)] <- c("02/13/2022", "31/02/2022")
  he <- hallazgos_de(e, "fecha_invalida")
  expect_equal(nrow(he), 1L)
  expect_equal(filas_de(he), 2L)
})

test_that("un texto con forma de fecha que no es una columna de fechas no se acusa", {
  lote <- c("12.13.2020", "ABC-1", "lote nuevo", "foo bar", "X 22")
  d <- data.frame(id = 1:300, lote = lote[(seq_len(300) %% 5L) + 1L],
                  stringsAsFactors = FALSE)
  expect_equal(nrow(hallazgos_de(d, "fecha_invalida")), 0L)
})

test_that("casi_constante: un valor distinto entre 500 y su severidad", {
  d <- data.frame(id = 1:500, cn = c(rep(5, 499), 6),
                  ct = c(rep("A", 498), "B", "B"), stringsAsFactors = FALSE)
  h <- hallazgos_de(d, "casi_constante")
  expect_equal(sort(h$columna), c("cn", "ct"))
  hn <- h[h$columna == "cn", ]
  expect_equal(hn$n_afectados, 1)
  expect_equal(hn$n_evaluados, 500)
  expect_equal(filas_de(hn), 500L)
  expect_equal(as.character(hn$severidad), "ok")
  expect_equal(sort(filas_de(h[h$columna == "ct", ])), c(499L, 500L))
})

test_that("casi_constante no acusa lo que no llega al umbral ni lo constante", {
  n <- 500
  # 98 % de un valor: por debajo del umbral.
  d <- data.frame(id = 1:n, a = c(rep(1, 490), rep(2, 10)), stringsAsFactors = FALSE)
  expect_equal(nrow(hallazgos_de(d, "casi_constante")), 0L)
  # Menos de 100 valores presentes.
  d <- data.frame(id = 1:99, a = c(rep(1, 98), 2))
  expect_equal(nrow(hallazgos_de(d, "casi_constante")), 0L)
  # Constante: sigue siendo `constante`, no `casi_constante`.
  d <- data.frame(id = 1:n, a = 7)
  expect_equal(nrow(hallazgos_de(d, "casi_constante")), 0L)
  expect_equal(nrow(hallazgos_de(d, "constante")), 1L)
  # La moda ocupa el 99 % y hay muchos valores distintos: `valor_concentrado`
  # ya lo dice, y no se repite.
  d <- data.frame(id = 1:2000, a = c(rep(5, 1985), 100 + 1:15))
  expect_equal(nrow(hallazgos_de(d, "valor_concentrado")), 1L)
  expect_equal(nrow(hallazgos_de(d, "casi_constante")), 0L)
})

test_that("casi_constante es siempre informativo, sea cual sea la codificacion", {
  n <- 1000
  tabla <- data.frame(
    id = 1:n,
    fallecido = c(rep("no", 990), rep("si", 10)),
    sexo = c(rep("M", 995), rep("F", 5)),
    estado = c(rep("ACTIVO", 995), rep("BAJA", 5)),
    anio = c(rep(2022L, 995), 2021L, 2023L, 2020L, 2019L, 2018L),
    stringsAsFactors = FALSE
  )
  tabla$reclamos <- c(rep(0, 995), 1, 2, 1, 3, 1)
  tabla$activo <- rep(c(FALSE, TRUE), c(995, 5))
  h <- hallazgos_de(tabla, "casi_constante")
  expect_setequal(h$columna, c("fallecido", "sexo", "estado", "anio", "reclamos", "activo"))
  expect_true(all(as.character(h$severidad) == "ok"))
  # El texto no califica: dice cuantas filas difieren del valor comun.
  expect_match(h$evidencia[h$columna == "sexo"], "5 de 1.000? filas difieren|5 de 1000 filas difieren")
})

test_that("una casi constante no es el lado determinado de una relacion vacia", {
  n <- 500
  d <- data.frame(
    id = 1:n,
    # Las dos filas con `B` caen en valores de `monto` que no se repiten.
    monto = c(1000L + (seq_len(n - 2L) %% 299L), 5000L, 5001L),
    ct = c(rep("A", n - 2), "B", "B"),
    stringsAsFactors = FALSE
  )
  dd <- detectar_dependencias(d, umbral = 0.99)
  expect_false(any(dd$dependiente == "ct"))
  expect_equal(nrow(hallazgos_de(d, "relacion_funcional_columnas")), 0L)
})

test_that("una relacion real con un dependiente casi constante se conserva", {
  n <- 1000
  ciudad <- paste0("C", (seq_len(n) * 7L) %% 40L)
  pais <- ifelse(ciudad %in% c("C1", "C2"), "AR", "UY")
  d <- data.frame(id = 1:n, ciudad = ciudad, pais = pais, stringsAsFactors = FALSE)
  dd <- detectar_dependencias(d, umbral = 0.99)
  expect_true(any(dd$determinante == "ciudad" & dd$dependiente == "pais"))
  # Con una fila que rompe la relacion dentro de un grupo repetido, el hallazgo
  # sale con esa fila.
  fila_ar <- which(pais == "AR")[1L]
  d$pais[fila_ar] <- "UY"
  h <- hallazgos_de(d, "relacion_funcional_columnas")
  expect_true(any(h$columna == "ciudad,pais"))
  expect_equal(h$n_afectados[h$columna == "ciudad,pais"], 1)
})

test_that("una relacion real con categorias raras que aparecen una vez se conserva", {
  n <- 1000
  ciudad <- paste0("C", (seq_len(n - 2L) * 7L) %% 38L)
  ciudad <- c(ciudad, "C38", "C39")
  pais <- ifelse(ciudad %in% c("C38", "C39"), "AR", "UY")
  d <- data.frame(id = 1:n, ciudad = ciudad, pais = pais, stringsAsFactors = FALSE)
  dd <- detectar_dependencias(d, umbral = 0.99)
  expect_true(any(dd$determinante == "ciudad" & dd$dependiente == "pais"))
})

test_that("dos valores de un caracter de una casi constante no son casi duplicados", {
  d <- data.frame(id = 1:300, ct = c(rep("A", 298), "B", "B"),
                  sexo = c(rep("M", 150), rep("F", 148), "X", "X"),
                  stringsAsFactors = FALSE)
  expect_equal(nrow(hallazgos_de(d, "casi_duplicados_vocabulario")), 0L)
  # Una errata real de un valor dominante sigue saliendo.
  e <- data.frame(id = 1:300, ciudad = c(rep("Montevideo", 296), rep("Montevidoe", 4)),
                  stringsAsFactors = FALSE)
  expect_equal(nrow(hallazgos_de(e, "casi_duplicados_vocabulario")), 1L)
})

test_that("identificador_con_ausentes: casi clave con pocos NA, sin clave declarada", {
  n <- 500
  d <- data.frame(id = 1:n, ident = sprintf("K%05d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  d$ident[c(4, 40, 77)] <- NA
  h <- hallazgos_de(d, "identificador_con_ausentes")
  expect_equal(nrow(h), 1L)
  expect_equal(h$columna, "ident")
  expect_equal(h$n_afectados, 3)
  expect_equal(h$n_evaluados, 500)
  expect_equal(sort(filas_de(h)), c(4L, 40L, 77L))
  # Declarada como clave: la dice `clave_con_ausentes`, no este.
  hc <- suppressWarnings(hallazgos_de(d, "identificador_con_ausentes", clave = "ident"))
  expect_equal(nrow(hc), 0L)
})

test_that("identificador_con_ausentes no acusa un identificador opcional ni repetidos", {
  n <- 500
  d <- data.frame(id = 1:n, pasaporte = sprintf("P%06d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  d$pasaporte[seq(1, n, by = 3)] <- NA
  expect_equal(nrow(hallazgos_de(d, "identificador_con_ausentes")), 0L)
  e <- data.frame(id = 1:n, ident = sprintf("K%05d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  e$ident[c(4, 40, 77)] <- NA
  e$ident[10] <- e$ident[11]
  expect_equal(nrow(hallazgos_de(e, "identificador_con_ausentes")), 0L)
})

test_that("perfilar_por aplica las reglas por grupo", {
  n <- 600
  d <- data.frame(id = 1:n, g = rep(c("a", "b"), each = n / 2),
                  hora = horas_validas(n), stringsAsFactors = FALSE)
  d$hora[5] <- "12:61"
  pp <- perfilar_por(d, por = "g")
  x <- pp[pp$tipo_hallazgo == "hora_invalida", ]
  expect_equal(nrow(x), 1L)
  expect_equal(as.character(x$grupo), "a")
})

test_that("el plan de limpieza no toca los datos por estos hallazgos", {
  n <- 300
  d <- data.frame(id = 1:n, hora = horas_validas(n), cn = c(rep(5, n - 1), 6),
                  stringsAsFactors = FALSE)
  d$hora[3] <- "12:61"
  p <- perfilar(d, nombre = "t")
  plan <- planificar_limpieza(p, d)
  accion <- as.data.frame(plan)
  expect_false(any(accion$hallazgo %in% c("hora_invalida", "casi_constante")))
  sin_accion <- as.data.frame(attr(plan, "hallazgos_sin_accion"))
  # `casi_constante` es informativo (`ok`) y no espera accion.
  expect_true("hora_invalida" %in% sin_accion$hallazgo)
})

test_that("comparar_perfiles ve aparecer el hallazgo nuevo", {
  n <- 300
  limpio <- data.frame(id = 1:n, hora = horas_validas(n), stringsAsFactors = FALSE)
  sucio <- limpio
  sucio$hora[3] <- "12:61"
  cmp <- as.data.frame(comparar_perfiles(
    perfilar(limpio, nombre = "t"), perfilar(sucio, nombre = "t")
  ))
  expect_true(any(grepl("hallazgo que no estaba", cmp$descripcion)))
})

test_that("perfilar_dbi: sobre la tabla completa publica, sobre una muestra declara", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  n <- 500
  d <- data.frame(id = 1:n, hora = horas_validas(n), cn = c(rep(5, n - 1), 6),
                  ident = sprintf("K%05d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  d$hora[3] <- "12:61"
  d$ident[c(4, 40, 77)] <- NA
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  tipos <- function(r) as.character(as.data.frame(hallazgos(r))$tipo_hallazgo)
  completo <- tipos(perfilar_dbi(con, "t"))
  expect_true(all(c("hora_invalida", "casi_constante",
                    "identificador_con_ausentes") %in% completo))
  muestra <- suppressWarnings(perfilar_dbi(con, "t", muestra = 200))
  expect_false(any(c("casi_constante", "identificador_con_ausentes") %in% tipos(muestra)))
  cobertura <- as.data.frame(muestra$perfil_muestra$cobertura_diagnosticos)
  expect_true("identificador_con_ausentes" %in% cobertura$diagnostico)
})

test_that("hora_invalida: una hora de 24 o mas se acusa solo si el nombre dice hora", {
  n <- 300
  d <- data.frame(id = 1:n, horas_cargadas = horas_validas(n), hora = horas_validas(n),
                  stringsAsFactors = FALSE)
  d$horas_cargadas[3] <- d$hora[3] <- "25:10"
  d$hora[4] <- d$horas_cargadas[4] <- "12:61"
  h <- as.data.frame(hallazgos(perfilar(d)))
  h <- h[h$tipo_hallazgo == "hora_invalida", ]
  expect_equal(h$n_afectados[h$columna == "hora"], 2)
  # Una columna que no se llama como una hora del dia: solo los minutos > 59.
  d$duracion <- d$hora
  d$hora <- NULL
  d$horas_cargadas <- NULL
  h2 <- as.data.frame(hallazgos(perfilar(d)))
  h2 <- h2[h2$tipo_hallazgo == "hora_invalida", ]
  expect_equal(h2$n_afectados, 1)
  expect_equal(filas_de(h2), 4L)
})

test_that("hora_invalida cuenta minutos fuera de rango aunque la hora tambien lo este", {
  n <- 300
  d <- data.frame(id = 1:n, hora = horas_validas(n), stringsAsFactors = FALSE)
  d$hora[c(3, 4, 5)] <- c("25:99", "24:99", "12:99")
  h <- hallazgos_de(d, "hora_invalida")
  expect_equal(h$n_afectados, 3)
  expect_match(h$evidencia, "minutos o segundos fuera de 0-59: 3", fixed = TRUE)
})

test_that("los hallazgos nuevos cuentan sobre el universo aplicable", {
  n <- 1000
  d <- data.frame(
    id = 1:n, aplica = rep(c(TRUE, FALSE), each = n / 2),
    hora = horas_validas(n), fec = fechas_validas(n),
    ct = rep("A", n), stringsAsFactors = FALSE
  )
  d$hora[501:510] <- "12:61"
  d$fec[501:510] <- "2022-02-30"
  d$ct[c(496:500, 996:1000)] <- "B"
  reglas <- list(hora = ~ aplica, fec = ~ aplica, ct = ~ aplica)
  h <- as.data.frame(hallazgos(perfilar(d, aplicabilidad = reglas)))
  expect_false(any(h$tipo_hallazgo %in% c("hora_invalida", "fecha_invalida")))
  hc <- h[h$tipo_hallazgo == "casi_constante", ]
  expect_equal(hc$n_afectados, 5)
  expect_equal(hc$n_evaluados, 500)
  expect_equal(sort(hc$trazabilidad[[1L]]$indices_fila), 496:500)
  expect_match(hc$evidencia, "5 de 500 filas difieren", fixed = TRUE)
})

test_that("identificador_con_ausentes es informativo y no se sostiene con blancos ni espacios", {
  n <- 500
  d <- data.frame(id = 1:n, ident = sprintf("K%05d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  d$ident[c(4, 40, 77)] <- NA
  h <- hallazgos_de(d, "identificador_con_ausentes")
  expect_equal(as.character(h$severidad), "ok")
  e <- d
  e$ident[10] <- ""
  expect_equal(nrow(hallazgos_de(e, "identificador_con_ausentes")), 0L)
  f <- d
  f$ident[10] <- paste0(f$ident[11], " ")
  expect_equal(nrow(hallazgos_de(f, "identificador_con_ausentes")), 0L)
})

test_that("perfilar_dbi con muestra_motor declara lo que pide la tabla entera", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  n <- 600
  d <- data.frame(id = 1:n, ct = c("B", "B", rep("A", n - 2)),
                  ident = sprintf("K%05d", (seq_len(n) * 7919L) %% 99991L),
                  stringsAsFactors = FALSE)
  d$ident[c(4, 40, 77)] <- NA
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", d)
  r <- suppressWarnings(perfilar_dbi(con, "t", universo = "muestra_motor",
                                     muestra_motor = 200))
  tipos <- as.character(as.data.frame(hallazgos(r))$tipo_hallazgo)
  expect_false(any(c("casi_constante", "identificador_con_ausentes") %in% tipos))
})

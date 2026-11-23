# Dos referenciales en R distintos con el mismo nombre en una corrida -dos
# `referencial(data.frame(...))` sin `nombre` se llaman los dos `referencial`-.
# A un referencial en R lo identifican su nombre y la huella de sus datos: son
# dos referenciales, se registran los dos y cada cifra cierra con la lectura
# del suyo, vayan en una llamada, en dos partes o en tres. El control de
# lecturas es `desconocido` solo si de verdad no se puede saber cual leyo la
# cifra: el mismo nombre, las mismas filas, otros datos y otro respaldo.

.amb_fecha <- as.POSIXct("2026-06-30", tz = "UTC")

.amb_padron <- function(n, nombre = NULL, valores = letters[seq_len(n)]) {
  referencial(data.frame(k = seq_len(n), v = valores), clave = "k",
              valor = "v", completo = TRUE, alcance = "e", nombre = nombre)
}

.amb_fuerte <- function(nombre, padron, entidad = "d") {
  instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, nombre),
             entidad, "k", referencial = padron)
}

.amb_datos <- function() data.frame(k = c(1, 2, 3, NA), z = c(1, NA, 2, 3))

.amb_medir <- function(..., entidades = "d") {
  datos <- stats::setNames(rep(list(.amb_datos()), length(entidades)), entidades)
  medir(modelo(...), datos, id_medicion = "j", fecha = .amb_fecha,
        detalle = "resumen")
}

.amb_cortes <- list(Exactitud = c(0.5, 0.9), Completitud = c(0.5, 0.9))

.amb_control <- function(x) semaforo_calidad(x, cortes = .amb_cortes)$control_lecturas

.amb_mensajes <- function(x) {
  mensajes <- character()
  withCallingHandlers(
    utils::capture.output(print(x)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  paste(mensajes, collapse = " ")
}

test_that("dos referenciales distintos con un nombre se registran, cada cifra con su lectura", {
  skip_on_cran()
  p <- .amb_medir(
    .amb_fuerte("A", .amb_padron(3)), .amb_fuerte("B", .amb_padron(4)),
    instanciar(especializar(metricas_nucleo()$NoNulo, "Z"), "d", "z")
  )
  l <- attr(p, "lecturas", exact = TRUE)
  propias <- l[l$entidad == "referencial", , drop = FALSE]
  expect_setequal(propias$n_filas, c(3, 4))
  expect_length(unique(propias$instantanea), 2L)
  expect_length(unique(propias$tabla), 2L)
  expect_identical(p$n_filas_leidas_referida[1:2], c(3, 4))
  expect_identical(p$estado_cuentas, rep("cierran", 3L))
  # Cada referencial se leyo una vez, y cada fila sabe cual es el suyo.
  s <- semaforo_calidad(p, cortes = .amb_cortes)
  expect_identical(s$control_lecturas, rep("una_lectura", 3L))
  expect_false(grepl("desconocido", .amb_mensajes(s), fixed = TRUE, useBytes = TRUE))
})

test_that("el estado no depende de como se repartan los referenciales en llamadas y partes", {
  skip_on_cran()
  # Cada metrica sobre su entidad, para que la unica tabla compartida entre
  # las partes sea la del referencial.
  a <- function() .amb_fuerte("A", .amb_padron(3), "d")
  b <- function() .amb_fuerte("B", .amb_padron(4), "e")
  una_llamada <- .amb_medir(a(), b(), entidades = c("d", "e"))
  expect_warning(
    dos_partes <- rbind(.amb_medir(a()), .amb_medir(b(), entidades = "e")), NA
  )
  for (x in list(una_llamada, dos_partes)) {
    expect_identical(x$estado_cuentas, c("cierran", "cierran"))
    expect_identical(x$n_filas_leidas_referida, c(3, 4))
    expect_identical(.amb_control(x), c("una_lectura", "una_lectura"))
  }
  # Con una tercera parte que vuelve a usar los dos, las mismas filas siguen
  # cerrando; cada referencial se leyo dos veces, con la misma huella.
  tercera <- .amb_medir(
    .amb_fuerte("C", .amb_padron(3), "f"), .amb_fuerte("D", .amb_padron(4), "g"),
    entidades = c("f", "g")
  )
  expect_warning(
    tres_partes <- rbind(.amb_medir(a()), .amb_medir(b(), entidades = "e"), tercera),
    NA
  )
  expect_identical(tres_partes$estado_cuentas, rep("cierran", 4L))
  expect_identical(.amb_control(tres_partes), rep("completo", 4L))
})

test_that("con el mismo nombre y las mismas filas el control se declara solo si no se puede saber", {
  skip_on_cran()
  uno <- function() .amb_padron(3)
  otro <- function() .amb_padron(3, valores = c("x", "y", "z"))
  # Los dos en una llamada, cada uno leido una vez: lea el que lea, es una
  # lectura.
  p <- .amb_medir(.amb_fuerte("A", uno()), .amb_fuerte("B", otro()))
  l <- attr(p, "lecturas", exact = TRUE)
  expect_length(unique(l$tabla[l$entidad == "referencial"]), 2L)
  expect_identical(p$estado_cuentas, c("cierran", "cierran"))
  expect_identical(.amb_control(p), c("una_lectura", "una_lectura"))
  # Otra parte vuelve a leer uno de los dos: uno tiene dos lecturas y el otro
  # una, y con el nombre y las filas no se sabe cual leyo cada cifra.
  u <- rbind(p, .amb_medir(.amb_fuerte("C", uno(), "e"), entidades = "e"))
  expect_identical(u$estado_cuentas, rep("cierran", 3L))
  s <- semaforo_calidad(u, cortes = .amb_cortes)
  expect_identical(s$control_lecturas, rep("desconocido", 3L))
  # El motivo lo da la impresion.
  expect_true(grepl("comparte su nombre con otro de las mismas filas",
                    .amb_mensajes(s), fixed = TRUE, useBytes = TRUE))
})

test_that("un referencial con el nombre de una tabla de otra parte no se compara con ella", {
  skip_on_cran()
  padron <- data.frame(k = 1:3, v = c("a", "b", "c"))
  como_entidad <- function(datos) {
    medir(
      modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "Clave"), "padron", "k")),
      list(padron = datos), id_medicion = "j", fecha = .amb_fecha, detalle = "resumen"
    )
  }
  como_referencial <- .amb_medir(.amb_fuerte("A", referencial(
    padron, clave = "k", valor = "v", completo = TRUE, alcance = "e"
  )))
  expect_identical(como_referencial$entidad_referida, "padron")
  # La tabla `padron` de otra parte tiene otras filas: no es un cambio del
  # referencial, y no se sabe cual de las dos leyo la cifra.
  u <- rbind(como_entidad(data.frame(k = 1:5, v = letters[1:5])), como_referencial)
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  expect_identical(.amb_control(u), c("una_lectura", "desconocido"))
  # Con los mismos datos da igual cual leyo.
  u <- rbind(como_entidad(padron), como_referencial)
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  expect_identical(.amb_control(u), c("una_lectura", "completo"))
})

test_that("entre dos referenciales con el mismo nombre, cada cifra lleva el respaldo del suyo", {
  skip_on_cran()
  # El de tres filas se lee en dos partes y el de cuatro en una.
  p <- .amb_medir(.amb_fuerte("A", .amb_padron(3)), .amb_fuerte("B", .amb_padron(4)))
  u <- rbind(p, .amb_medir(.amb_fuerte("C", .amb_padron(3), "e"), entidades = "e"))
  expect_identical(u$estado_cuentas, rep("cierran", 3L))
  expect_identical(u$n_filas_leidas_referida, c(3, 4, 3))
  expect_identical(.amb_control(u), c("completo", "una_lectura", "completo"))
})

test_that("una tabla del motor con el nombre de un referencial en R de otras filas sigue siendo la tabla", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "padron", data.frame(k = 1:4, v = c("a", "b", "c", "d")))
  DBI::dbWriteTable(con, "d", .amb_datos())
  del_motor <- medir_dbi(
    modelo(.amb_fuerte("Motor", referencial(
      DBI::Id(table = "padron"), clave = "k", valor = "v", completo = TRUE,
      alcance = "e"
    ))),
    con, id_medicion = "j", fecha = .amb_fecha
  )
  expect_identical(del_motor$entidad_referida, "padron")
  expect_identical(.amb_control(del_motor), "limitado")
  # Otra parte usa un referencial en R que tambien se llama `padron`, con tres
  # filas: la fila del motor leyo cuatro, asi que leyo la tabla, y su control
  # es el de antes de unir. La fila de memoria no dice cual de los dos leyo.
  en_memoria <- .amb_medir(
    .amb_fuerte("Memoria", .amb_padron(3, "padron"), "e"), entidades = "e"
  )
  u <- rbind(del_motor, en_memoria)
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  expect_identical(.amb_control(u), c("limitado", "desconocido"))
})

test_that("el control: el mismo referencial en dos metricas es una lectura", {
  skip_on_cran()
  padron <- .amb_padron(3)
  cubre <- instanciar(especializar(metricas_referencial()$RatioCobertura, "C"),
                      "d", "k", referencial = padron)
  p <- .amb_medir(.amb_fuerte("A", padron), cubre)
  l <- attr(p, "lecturas", exact = TRUE)
  expect_identical(sum(l$entidad == "referencial"), 1L)
  s <- semaforo_calidad(p, cortes = .amb_cortes)
  expect_identical(s$control_lecturas, c("una_lectura", "una_lectura"))
})

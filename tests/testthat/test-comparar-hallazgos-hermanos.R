# Una columna con dos hallazgos del mismo tipo (por ejemplo dos centinelas de
# fecha, o un valor fuera de dominio por filas y otro de columna) compara la
# cuenta de cada uno con la de su par en la otra entrega. Antes el resumen
# juntaba los dos en uno, no publicaba la cuenta y el empeoramiento quedaba
# invisible.

testthat::skip_on_cran()

.fecha_a_hermanos <- as.Date("2026-11-18")
.fecha_b_hermanos <- as.Date("2026-11-19")

.perfil_hermanos <- function(datos, fecha, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE, ...
  )))
}

# Una columna de fechas con dos centinelas: `n1` veces 9999-12-31 y `n2` veces
# 1900-01-01.
.fechas_hermanos <- function(n1, n2, n = 400L) {
  f <- as.Date("2024-01-01") + seq_len(n)
  f[seq_len(n1)] <- as.Date("9999-12-31")
  f[n1 + seq_len(n2)] <- as.Date("1900-01-01")
  data.frame(id = seq_len(n), fecha_fin = f)
}

.perfil_fechas_hermanos <- function(n1, n2, fecha) {
  .perfil_hermanos(.fechas_hermanos(n1, n2), fecha, clave = "id")
}

.proporcion_hermanos <- function(n_dominio, n_alto = 12L) {
  data.frame(proporcion = c(
    seq(0.01, 0.99, length.out = 100L - n_dominio - n_alto),
    rep(-0.2, n_dominio), seq(200, 500, length.out = n_alto)
  ))
}

.cambios_hermanos <- function(a, b, aspecto) {
  d <- as.data.frame(suppressWarnings(suppressMessages(
    comparar_perfiles(a, b)
  )))
  d[d$aspecto %in% aspecto, , drop = FALSE]
}

.tipo_en_columna <- function(perfil, tipo, columna) {
  h <- as.data.frame(perfil$hallazgos)
  h[h$tipo_hallazgo == tipo & !is.na(h$columna) & h$columna == columna, ,
    drop = FALSE]
}

test_that("dos centinelas de fecha: el que empeora se compara y el otro no", {
  a <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 40L, .fecha_b_hermanos)
  expect_equal(
    nrow(.tipo_en_columna(a, "fecha_centinela", "fecha_fin")), 2L
  )
  d <- .cambios_hermanos(a, b, "conteo_hallazgo")
  expect_equal(nrow(d), 1L)
  expect_identical(d$cambio, "agravado")
  expect_identical(d$valor_anterior, "6 de 400")
  expect_identical(d$valor_actual, "40 de 400")
})

test_that("dos valores fuera de dominio: la cuenta de cada uno se compara", {
  a <- .perfil_hermanos(.proporcion_hermanos(1L), .fecha_a_hermanos)
  b <- .perfil_hermanos(.proporcion_hermanos(30L), .fecha_b_hermanos)
  expect_equal(
    nrow(.tipo_en_columna(a, "valor_fuera_dominio", "proporcion")), 2L
  )
  d <- .cambios_hermanos(a, b, "conteo_hallazgo")
  expect_equal(nrow(d), 1L)
  expect_identical(d$columna, "proporcion")
  expect_identical(d$cambio, "agravado")
  expect_identical(d$valor_anterior, "1 de 100")
  expect_identical(d$valor_actual, "30 de 100")
})

test_that("dos hallazgos iguales en las dos entregas no inventan cambios", {
  a <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 6L, .fecha_b_hermanos)
  d <- .cambios_hermanos(
    a, b, c("hallazgo", "conteo_hallazgo", "severidad_hallazgo")
  )
  expect_equal(nrow(d), 0L)
})

test_that("el orden de los hallazgos no cambia con quien se empareja", {
  a <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 40L, .fecha_b_hermanos)
  b$hallazgos <- b$hallazgos[rev(seq_len(nrow(b$hallazgos))), , drop = FALSE]
  d <- .cambios_hermanos(a, b, "conteo_hallazgo")
  expect_equal(nrow(d), 1L)
  expect_identical(d$valor_anterior, "6 de 400")
  expect_identical(d$valor_actual, "40 de 400")
})

# Un hallazgo no guarda que lo distingue de su hermano, solo su evidencia, y
# esa cambia con los datos: cuando los hermanos de las dos entregas no se
# emparejan sin duda, no se publica ni cuenta ni aparecido ni resuelto (como
# antes), porque un dato falso es peor que el silencio.
.sin_publicar_hermanos <- function(d, tipo) {
  expect_false(any(d$aspecto == "conteo_hallazgo"))
  expect_false(any(d$aspecto == "hallazgo" &
                     (d$valor_anterior %in% tipo | d$valor_actual %in% tipo)))
}

test_that("si las lecturas de los hermanos no son las mismas no se publica nada", {
  # Uno que desaparece o que aparece: no hay modo de saber si es el mismo
  # hallazgo con otra lectura, y un silencio vale mas que un falso.
  a <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 6L, .fecha_b_hermanos)
  quedan <- grepl("Lectura de columna", b$hallazgos$evidencia, fixed = TRUE)
  b$hallazgos <- b$hallazgos[
    b$hallazgos$tipo_hallazgo != "fecha_centinela" | quedan, , drop = FALSE
  ]
  .sin_publicar_hermanos(
    .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo")), "fecha_centinela"
  )
  a2 <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  quedan <- grepl("Lectura de columna", a2$hallazgos$evidencia, fixed = TRUE)
  a2$hallazgos <- a2$hallazgos[
    a2$hallazgos$tipo_hallazgo != "fecha_centinela" | quedan, , drop = FALSE
  ]
  b2 <- .perfil_fechas_hermanos(6L, 6L, .fecha_b_hermanos)
  .sin_publicar_hermanos(
    .cambios_hermanos(a2, b2, c("hallazgo", "conteo_hallazgo")), "fecha_centinela"
  )
})

test_that("un hallazgo de columna que pasa a lectura por filas no se da por resuelto", {
  a <- .perfil_hermanos(.proporcion_hermanos(1L), .fecha_a_hermanos)
  b <- .perfil_hermanos(.proporcion_hermanos(1L), .fecha_b_hermanos)
  tipo <- b$hallazgos$tipo_hallazgo == "valor_fuera_dominio"
  columna <- which(
    tipo & grepl("Lectura de columna", b$hallazgos$evidencia, fixed = TRUE)
  )
  fila <- which(
    tipo & !grepl("Lectura de columna", b$hallazgos$evidencia, fixed = TRUE)
  )
  # En la entrega nueva queda un solo hallazgo, por filas, con las filas del
  # de columna: el de columna de antes no se resolvio.
  b$hallazgos$evidencia[columna] <- sub(
    " Lectura de columna.*$", "", b$hallazgos$evidencia[columna]
  )
  b$hallazgos$severidad[columna] <- "sospechoso"
  b$hallazgos <- b$hallazgos[-fila, , drop = FALSE]
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "valor_fuera_dominio")
})

test_that("la severidad no decide cual es cual entre dos hermanos", {
  a <- .perfil_hermanos(.proporcion_hermanos(30L, 5L), .fecha_a_hermanos)
  b <- .perfil_hermanos(.proporcion_hermanos(30L, 5L), .fecha_b_hermanos)
  i <- which(b$hallazgos$tipo_hallazgo == "valor_fuera_dominio")
  b$hallazgos$severidad[i] <- rev(b$hallazgos$severidad[i])
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "valor_fuera_dominio")
})

test_that("el de filas se resuelve y el de columna sigue: se publica lo de siempre", {
  # Las lecturas de las dos entregas no son las mismas: el grupo se compara
  # junto, como siempre, y no se publica `resuelto` de ninguno.
  a <- .perfil_hermanos(.proporcion_hermanos(1L), .fecha_a_hermanos)
  for (n_alto in c(12L, 30L)) {
    b <- .perfil_hermanos(
      data.frame(proporcion = c(
        seq(0.01, 0.99, length.out = 100L - n_alto),
        seq(101, 112, length.out = n_alto)
      )),
      .fecha_b_hermanos
    )
    d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
    .sin_publicar_hermanos(d, "valor_fuera_dominio")
  }
})

test_that("dos hermanos que intercambian su evidencia no intercambian su cuenta", {
  a <- .perfil_fechas_hermanos(6L, 30L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 30L, .fecha_b_hermanos)
  i <- which(b$hallazgos$tipo_hallazgo == "fecha_centinela")
  b$hallazgos$evidencia[i] <- rev(b$hallazgos$evidencia[i])
  b$hallazgos$severidad[i] <- rev(b$hallazgos$severidad[i])
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "fecha_centinela")
})

test_that("con tres hallazgos del mismo tipo, sin con que distinguirlos, no se publica nada", {
  tres <- function(perfil, n3) {
    h <- perfil$hallazgos
    fila <- which(
      h$tipo_hallazgo == "fecha_centinela" &
        !grepl("Lectura de columna", h$evidencia, fixed = TRUE)
    )
    nuevo <- h[fila, , drop = FALSE]
    nuevo$descripcion <- paste(nuevo$descripcion, "(tercero)")
    nuevo$evidencia <- sub(
      "Centinelas observados: [^;]*; ", "Centinelas observados: 1970-01-01; ",
      sub("filas afectadas: [0-9]+", paste0("filas afectadas: ", n3),
          nuevo$evidencia)
    )
    nuevo$n_afectados <- n3
    perfil$hallazgos <- rbind(h, nuevo)
    perfil
  }
  a <- tres(.perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos), 8L)
  b <- tres(.perfil_fechas_hermanos(6L, 6L, .fecha_b_hermanos), 60L)
  expect_equal(
    nrow(.tipo_en_columna(a, "fecha_centinela", "fecha_fin")), 3L
  )
  # Solo hay dos lecturas posibles (filas y columna): el tercero no se puede
  # emparejar sin duda y el grupo se compara junto, como siempre.
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "fecha_centinela")
})

test_that("una columna con un solo hallazgo por tipo se compara como siempre", {
  a <- .perfil_hermanos(
    data.frame(proporcion = c(seq(0.01, 0.99, length.out = 99L), -0.2)),
    .fecha_a_hermanos
  )
  b <- .perfil_hermanos(
    data.frame(proporcion = c(seq(0.01, 0.99, length.out = 70L),
                              rep(-0.2, 30L))),
    .fecha_b_hermanos
  )
  d <- .cambios_hermanos(a, b, "conteo_hallazgo")
  expect_equal(nrow(d), 1L)
  expect_identical(d$valor_anterior, "1 de 100")
  expect_identical(d$valor_actual, "30 de 100")
})

test_that("un perfil sin unidad de conteo no inventa cambios contra uno nuevo", {
  a <- .perfil_fechas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .perfil_fechas_hermanos(6L, 6L, .fecha_b_hermanos)
  a$hallazgos$unidad_conteo <- NULL
  d <- .cambios_hermanos(
    a, b, c("hallazgo", "conteo_hallazgo", "severidad_hallazgo")
  )
  expect_equal(nrow(d), 0L)
})

.homonimas_hermanos <- function(n_primera, n_segunda, fecha) {
  x <- data.frame(
    a = as.Date("2024-01-01") + seq_len(400L),
    b = as.Date("2024-01-01") + seq_len(400L), id = seq_len(400L)
  )
  x$a[seq_len(n_primera)] <- as.Date("9999-12-31")
  x$a[n_primera + seq_len(n_segunda)] <- as.Date("1900-01-01")
  x$b[1:30] <- as.Date("9999-12-31")
  names(x)[1:2] <- c("fecha_fin", "fecha_fin")
  suppressWarnings(suppressMessages(perfilar(
    x, fecha = fecha, clave = "id", analizar_dependencias = FALSE
  )))
}

test_that("columnas con el mismo nombre no publican cuentas ni resueltos falsos", {
  # La tabla de hallazgos no dice de cual de las columnas homonimas es cada
  # uno, y su orden no es identidad.
  a <- .homonimas_hermanos(6L, 6L, .fecha_a_hermanos)
  b <- .homonimas_hermanos(0L, 6L, .fecha_b_hermanos)
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "fecha_centinela")
  # El mismo dato con las filas de los hallazgos al reves: nada cambio.
  b2 <- .homonimas_hermanos(6L, 6L, .fecha_b_hermanos)
  b2$hallazgos <- b2$hallazgos[rev(seq_len(nrow(b2$hallazgos))), , drop = FALSE]
  d2 <- .cambios_hermanos(
    a, b2, c("hallazgo", "conteo_hallazgo", "severidad_hallazgo")
  )
  expect_equal(nrow(d2), 0L)
})

# Una columna con el 9999 abierto (6 filas) y otro centinela de `n_cent` filas.
.centinelas_hermanos <- function(centinela, n_cent, fecha, ...) {
  f <- as.Date("2024-01-01") + seq_len(400L)
  f[1:6] <- as.Date("9999-12-31")
  if (n_cent > 0L) f[6L + seq_len(n_cent)] <- as.Date(centinela)
  .perfil_hermanos(data.frame(id = seq_len(400L), fecha_fin = f), fecha,
                   clave = "id", ...)
}

test_that("otro valor de centinela no se compara como el mismo hallazgo", {
  a <- .centinelas_hermanos("1900-01-01", 30L, .fecha_a_hermanos)
  b <- .centinelas_hermanos("1970-01-01", 70L, .fecha_b_hermanos)
  d <- .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo"))
  .sin_publicar_hermanos(d, "fecha_centinela")
})

test_that("declarar los centinelas no resuelve el hallazgo que sigue", {
  declarados <- c("9999-12-31", "1900-01-01")
  a <- .centinelas_hermanos("1900-01-01", 30L, .fecha_a_hermanos)
  b <- .centinelas_hermanos(
    "1900-01-01", 30L, .fecha_b_hermanos, centinelas_fecha = declarados
  )
  .sin_publicar_hermanos(
    .cambios_hermanos(a, b, c("hallazgo", "conteo_hallazgo")), "fecha_centinela"
  )
  # Y si ademas el centinela de 1900 desaparece de los datos.
  b2 <- .centinelas_hermanos(
    "1900-01-01", 0L, .fecha_b_hermanos, centinelas_fecha = declarados
  )
  .sin_publicar_hermanos(
    .cambios_hermanos(a, b2, c("hallazgo", "conteo_hallazgo")), "fecha_centinela"
  )
})

.cuenta_con_declaracion <- function(args_a, args_b) {
  a <- do.call(.centinelas_hermanos, c(
    list("1900-01-01", 6L, .fecha_a_hermanos), args_a
  ))
  b <- do.call(.centinelas_hermanos, c(
    list("1900-01-01", 40L, .fecha_b_hermanos), args_b
  ))
  .cambios_hermanos(a, b, "conteo_hallazgo")
}

test_that("la misma declaracion escrita distinto no apaga la comparacion", {
  # Un conjunto en otro orden, o ninguna declaracion contra una vacia, es la
  # misma politica: la cuenta real (6 -> 40) se publica.
  casos <- list(
    list(list(centinelas_fecha = c("2001-01-01", "2002-01-01")),
         list(centinelas_fecha = c("2002-01-01", "2001-01-01"))),
    list(list(cadenas_ausencia = c("S/D", "N/A")),
         list(cadenas_ausencia = c("N/A", "S/D"))),
    list(list(cadenas_ausencia = NULL),
         list(cadenas_ausencia = character()))
  )
  for (caso in casos) {
    d <- .cuenta_con_declaracion(caso[[1L]], caso[[2L]])
    expect_equal(nrow(d), 1L)
    expect_identical(d$valor_anterior, "6 de 400")
    expect_identical(d$valor_actual, "40 de 400")
  }
})

test_that("una declaracion por columna distinta apaga la comparacion", {
  d <- .cuenta_con_declaracion(
    list(), list(cadenas_ausencia = list(fecha_fin = "S/D"))
  )
  expect_equal(nrow(d), 0L)
})

# La comparacion de perfiles y la deriva.
#
# Una prueba por defecto. La cifra esperada se rehace a mano en cada prueba, y
# cada una falla contra el codigo anterior al arreglo. Tablas chicas: el check
# tiene presupuesto.

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.f1_o140 <- as.POSIXct("2026-01-01", tz = "UTC")
.f2_o140 <- as.POSIXct("2026-02-01", tz = "UTC")

# Protege salvo que se diga lo contrario: desde el 2026-10-25 `perfilar()` no
# protege por omision.
.perfil_o140 <- function(datos, fecha, ..., proteger_datos_personales = TRUE) {
  perfilar(datos, fecha = fecha, analizar_dependencias = FALSE,
           proteger_datos_personales = proteger_datos_personales, ...)
}

.con_opciones_o140 <- function(opciones, codigo) {
  viejas <- options(opciones)
  on.exit(options(viejas), add = TRUE)
  force(codigo)
}

.filas_o140 <- function(deriva, aspecto, columna = NULL) {
  x <- as.data.frame(deriva)
  x <- x[x$aspecto == aspecto, , drop = FALSE]
  if (!is.null(columna)) {
    x <- x[!is.na(x$columna) & x$columna == columna, , drop = FALSE]
  }
  x
}

# Cedula uruguaya: siete cifras y un verificador con pesos 2, 9, 8, 7, 6, 3, 4.
.ci_o140 <- function(base, valida = TRUE) {
  cifras <- as.integer(strsplit(sprintf("%07d", base), "")[[1L]])
  verificador <- (10L - sum(cifras * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
  if (!valida) verificador <- (verificador + 1L) %% 10L
  base * 10 + verificador
}

test_that("H1: una columna protegida en los dos lados declara que su rango no se compara", {
  # `telefono` se protege por su nombre. Su rango pasa a setenta veces su ancho
  # y la comparacion no decia nada: ni el cambio, que no puede publicar, ni que
  # no lo habia mirado.
  telefono_a <- 20001066 + (0:19) * 52000
  telefono_b <- 90002001 + (0:19) * 52000
  deriva <- comparar_perfiles(
    .perfil_o140(data.frame(telefono = telefono_a, n = 1:20), .f1_o140),
    .perfil_o140(data.frame(telefono = telefono_b, n = 1:20), .f2_o140)
  )
  expect_equal(nrow(deriva), 0L)
  protegidos <- attr(deriva, "campos_protegidos")
  expect_s3_class(protegidos, "data.frame")
  expect_identical(names(protegidos), c("columna", "campo", "lado"))
  expect_identical(protegidos$columna, c("telefono", "telefono"))
  expect_identical(protegidos$campo, c("rango", "rango"))
  expect_identical(protegidos$lado, c("anterior", "actual"))

  # Control: sin proteccion se publica el cambio. La magnitud es el mayor
  # corrimiento de un extremo sobre el ancho anterior, y no hay nada que
  # declarar.
  abierta <- comparar_perfiles(
    .perfil_o140(data.frame(telefono = telefono_a, n = 1:20), .f1_o140,
                 proteger_datos_personales = FALSE),
    .perfil_o140(data.frame(telefono = telefono_b, n = 1:20), .f2_o140,
                 proteger_datos_personales = FALSE)
  )
  rango <- .filas_o140(abierta, "rango", "telefono")
  ancho <- 20989066 - 20001066
  corrimiento <- max(abs(90002001 - 20001066), abs(90990001 - 20989066))
  expect_equal(nrow(rango), 1L)
  expect_equal(rango$cambio_relativo, corrimiento / ancho)
  expect_equal(nrow(attr(abierta, "campos_protegidos")), 0L)

  # `rbind()` conserva la declaracion de las dos derivas, en los dos ordenes.
  expect_equal(nrow(attr(rbind(abierta, deriva), "campos_protegidos")), 2L)
  expect_equal(nrow(attr(rbind(deriva, abierta), "campos_protegidos")), 2L)
})

test_that("H2: un cambio de proteccion de la columna no se disfraza de cambio de rango", {
  # Cien documentos con los MISMOS extremos en las dos entregas: 88 con
  # verificador valido en la anterior y 92 en la actual. La columna cruza el
  # umbral de verificacion -0,9- y queda protegida con la misma politica. Salia
  # "Cambio el rango observado", `sospechoso`, con magnitud `Inf`.
  bases <- 3100000 + 0:99
  extremos <- c(.ci_o140(bases[[1L]]), .ci_o140(bases[[100L]]))
  anterior_datos <- c(
    extremos, vapply(bases[2:87], .ci_o140, numeric(1)),
    vapply(bases[88:99], .ci_o140, numeric(1), valida = FALSE)
  )
  actual_datos <- c(
    extremos, vapply(bases[2:91], .ci_o140, numeric(1)),
    vapply(bases[92:99], .ci_o140, numeric(1), valida = FALSE)
  )
  expect_identical(range(anterior_datos), range(actual_datos))
  expect_equal(mean(validar_ci_uy(as.character(anterior_datos))), 88 / 100)
  expect_equal(mean(validar_ci_uy(as.character(actual_datos))), 92 / 100)

  for (como_texto in c(FALSE, TRUE)) {
    a <- if (como_texto) as.character(anterior_datos) else anterior_datos
    b <- if (como_texto) as.character(actual_datos) else actual_datos
    deriva <- comparar_perfiles(
      .perfil_o140(data.frame(codigo = a, stringsAsFactors = FALSE), .f1_o140),
      .perfil_o140(data.frame(codigo = b, stringsAsFactors = FALSE), .f2_o140)
    )
    expect_equal(nrow(.filas_o140(deriva, "rango", "codigo")), 0L)
    fila <- .filas_o140(deriva, "proteccion", "codigo")
    expect_equal(nrow(fila), 1L)
    expect_identical(as.character(fila$severidad), "sospechoso")
    expect_identical(fila$valor_anterior, "sin proteger")
    expect_identical(fila$valor_actual, "protegido")
    expect_identical(attr(deriva, "campos_protegidos")$lado, "actual")
  }

  # Si cambia la politica entera, la fila es la de la politica y no se repite
  # por columna; el rango tampoco se compara.
  politica <- comparar_perfiles(
    .perfil_o140(data.frame(codigo = actual_datos), .f1_o140,
                 proteger_datos_personales = FALSE),
    .perfil_o140(data.frame(codigo = actual_datos), .f2_o140)
  )
  expect_equal(nrow(.filas_o140(politica, "configuracion_proteccion")), 1L)
  expect_equal(nrow(.filas_o140(politica, "proteccion")), 0L)
  expect_equal(nrow(.filas_o140(politica, "rango")), 0L)

  # Un perfil de otra version, sin los campos de proteccion y con el rango en
  # NA, no deja afirmar "sin proteger": no hay fila por columna, y lo protegido
  # del otro lado se declara igual.
  viejo <- .perfil_o140(data.frame(codigo = anterior_datos), .f1_o140)
  viejo$columnas$dato_personal_protegido <- NULL
  viejo$columnas$detalle_proteccion_personal <- NULL
  viejo$columnas$minimo <- NA_real_
  viejo$columnas$maximo <- NA_real_
  deriva <- comparar_perfiles(
    viejo, .perfil_o140(data.frame(codigo = actual_datos), .f2_o140)
  )
  expect_equal(nrow(.filas_o140(deriva, "proteccion")), 0L)
  expect_equal(nrow(.filas_o140(deriva, "rango")), 0L)
  expect_identical(attr(deriva, "campos_protegidos")$lado, "actual")
})

test_that("H3: los extremos exactos de un entero que no entra en un doble se comparan", {
  # Un entero escrito como texto por encima de 2^53 publica `minimo` y
  # `maximo` en NA y sus extremos exactos como texto. El ancho pasa de 20 a
  # 1020 y la comparacion daba cero filas.
  textos <- function(...) data.frame(id = c(...), stringsAsFactors = FALSE)
  anterior <- .perfil_o140(
    textos("9007199254740993", "9007199254741003", "9007199254741013"), .f1_o140
  )
  actual <- .perfil_o140(
    textos("9007199254740993", "9007199254741003", "9007199254742013"), .f2_o140
  )
  expect_true(is.na(anterior$columnas$maximo))
  rango <- .filas_o140(comparar_perfiles(anterior, actual), "rango", "id")
  expect_equal(nrow(rango), 1L)
  # El maximo se corre 1000 sobre un ancho de 20.
  expect_equal(rango$cambio_relativo, 1000 / 20)
  expect_identical(as.character(rango$severidad), "sospechoso")
  expect_identical(rango$valor_anterior, "[9007199254740993, 9007199254741013]")
  expect_identical(rango$valor_actual, "[9007199254740993, 9007199254742013]")

  # Negativos, que restan con prestamo: el minimo baja 1000 sobre un ancho de 20.
  rango <- .filas_o140(comparar_perfiles(
    .perfil_o140(textos("-9007199254741013", "-9007199254740993",
                        "-9007199254741003"), .f1_o140),
    .perfil_o140(textos("-9007199254742013", "-9007199254740993",
                        "-9007199254741003"), .f2_o140)
  ), "rango", "id")
  expect_equal(rango$cambio_relativo, 1000 / 20)

  # Un rango que cruza el cero -el ancho suma las dos magnitudes- con un cambio
  # chico: sale `ok`, no significativo, y no se calla.
  rango <- .filas_o140(comparar_perfiles(
    .perfil_o140(textos("-9007199254740993", "0", "9007199254740993"), .f1_o140),
    .perfil_o140(textos("-9007199254740993", "0", "9007199254741993"), .f2_o140)
  ), "rango", "id")
  expect_equal(nrow(rango), 1L)
  expect_identical(as.character(rango$severidad), "ok")
  expect_equal(rango$cambio_relativo, 1000 / 18014398509481986)
})

test_that("H3: un integer64 por encima de 2^53 compara su rango exacto", {
  skip_if_not_installed("bit64", "4.6.0")
  base <- bit64::as.integer64("1152921504606846976")
  anterior <- .perfil_o140(
    data.frame(id = base + bit64::as.integer64(c(0, 10, 20))), .f1_o140
  )
  actual <- .perfil_o140(
    data.frame(id = base + bit64::as.integer64(c(0, 10, 1000))), .f2_o140
  )
  rango <- .filas_o140(comparar_perfiles(anterior, actual), "rango", "id")
  expect_equal(nrow(rango), 1L)
  # El maximo se corre 1000 - 20 sobre un ancho de 20 - 0.
  expect_equal(rango$cambio_relativo, (1000 - 20) / (20 - 0))
  expect_identical(
    rango$valor_actual, "[1152921504606846976, 1152921504606847976]"
  )
})

test_that("H4: el rango se decide por sus valores y se publica igual en cualquier sesion", {
  anterior <- .perfil_o140(
    data.frame(monto = c(123456780, 123456785, 123456789)), .f1_o140
  )
  actual <- .perfil_o140(
    data.frame(monto = c(123456780, 123456785, 123456791)), .f2_o140
  )
  # El maximo se corre 2 sobre un ancho de 9.
  esperado <- (123456791 - 123456789) / (123456789 - 123456780)
  estados <- list(
    list(scipen = 0, digits = 7), list(scipen = -5, digits = 7),
    list(scipen = 100, digits = 7), list(scipen = 0, digits = 3),
    list(scipen = -5, digits = 15)
  )
  for (estado in estados) {
    rango <- .con_opciones_o140(
      estado, .filas_o140(comparar_perfiles(anterior, actual), "rango", "monto")
    )
    etiqueta <- paste(names(estado), unlist(estado), collapse = " ")
    expect_equal(nrow(rango), 1L, info = etiqueta)
    expect_equal(rango$cambio_relativo, esperado, info = etiqueta)
    expect_identical(rango$valor_anterior, "[123456780, 123456789]", info = etiqueta)
    expect_identical(rango$valor_actual, "[123456780, 123456791]", info = etiqueta)
  }

  # Un rango angosto frente a su magnitud, que ocho cifras no distinguen.
  rango <- .filas_o140(comparar_perfiles(
    .perfil_o140(data.frame(v = c(1.000000010, 1.000000020, 1.000000030)), .f1_o140),
    .perfil_o140(data.frame(v = c(1.000000010, 1.000000020, 1.000000044)), .f2_o140)
  ), "rango", "v")
  expect_equal(nrow(rango), 1L)
  expect_equal(
    rango$cambio_relativo,
    (1.000000044 - 1.000000030) / (1.000000030 - 1.000000010)
  )

  # Dos extremos distintos no se imprimen iguales, ni en dos notaciones.
  grande <- .con_opciones_o140(list(scipen = 0), comparar_perfiles(
    .perfil_o140(data.frame(monto = c(210000000011, 210000000500, 210000001000)),
                 .f1_o140),
    .perfil_o140(data.frame(monto = c(210000000011, 210000000500, 210000009000)),
                 .f2_o140)
  ))
  expect_identical(
    .filas_o140(grande, "rango", "monto")$valor_anterior,
    "[210000000011, 210000001000]"
  )
  constante <- .con_opciones_o140(list(scipen = 0), comparar_perfiles(
    .perfil_o140(data.frame(v = c(1e6, 1e6, 1e6)), .f1_o140),
    .perfil_o140(data.frame(v = c(1e6, 1020000, 1040000)), .f2_o140)
  ))
  expect_identical(
    .filas_o140(constante, "rango", "v")$valor_actual, "[1000000, 1040000]"
  )

  # La marca decimal sigue a `OutDec`, y entonces los extremos se separan con
  # punto y coma. La magnitud no cambia: (30 - 3) / (3 - 1,5).
  coma <- .con_opciones_o140(list(OutDec = ","), .filas_o140(comparar_perfiles(
    .perfil_o140(data.frame(v = c(1.5, 2, 3)), .f1_o140),
    .perfil_o140(data.frame(v = c(1.5, 2, 30)), .f2_o140)
  ), "rango", "v"))
  expect_identical(coma$valor_anterior, "[1,5; 3]")
  expect_identical(coma$valor_actual, "[1,5; 30]")
  expect_equal(coma$cambio_relativo, (30 - 3) / (3 - 1.5))
})

test_that("H5: un patron que entra o sale de un resumen lleno no es nuevo ni desaparecido", {
  separadores <- c(
    "-", ".", "/", "_", ":", "#", "+", "=", "*", "&", "%", "$", "!", "?", ";",
    ",", "~", "^", "|", "<", ">", "("
  )
  formas <- paste0("A", separadores, "1")
  valores <- function(frecuencias) {
    data.frame(cod = rep(formas, frecuencias), stringsAsFactors = FALSE)
  }
  # 22 formas y un resumen de 20. Solo se intercambian las frecuencias de `A<1`
  # (5 -> 4) y `A>1` (4 -> 5): las dos siguen en las dos entregas.
  frecuencias_a <- c(rep(30, 19), 5, 4, 3)
  frecuencias_b <- c(rep(30, 19), 4, 5, 3)
  anterior <- .perfil_o140(valores(frecuencias_a), .f1_o140)
  expect_equal(nrow(anterior$patrones[[1L]]), 20L)
  patrones <- .filas_o140(
    comparar_perfiles(anterior, .perfil_o140(valores(frecuencias_b), .f2_o140)),
    "patron", "cod"
  )
  expect_equal(nrow(patrones), 2L)
  expect_false(any(patrones$cambio %in% c("aparecido", "desaparecido")))
  expect_true(all(patrones$cambio == "no_comparable"))
  expect_true(all(as.character(patrones$severidad) == "ok"))
  expect_false(any(patrones$significativo))

  # Lo que se puede garantizar se informa: `A(1` estaba fuera del resumen
  # anterior -a lo sumo 5 de 582, la menor proporcion que guardaba- y en la
  # nueva entrega es 300 de 879.
  frecuencias_c <- c(rep(30, 19), 5, 4, 300)
  crecido <- .filas_o140(
    comparar_perfiles(anterior, .perfil_o140(valores(frecuencias_c), .f2_o140)),
    "patron", "cod"
  )
  fila <- crecido[!is.na(crecido$valor_actual) & crecido$valor_actual == "A(9", ,
                  drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$cambio, "modificado")
  expect_identical(as.character(fila$severidad), "error")
  expect_equal(fila$delta, 300 / 879 - 5 / 582)

  # Control: con el resumen completo, una forma nueva sigue siendo nueva.
  completo_a <- .perfil_o140(valores(frecuencias_a), .f1_o140, max_patrones = 30)
  completo_b <- .perfil_o140(
    rbind(valores(frecuencias_a), data.frame(cod = rep("Z1", 3))), .f2_o140,
    max_patrones = 30
  )
  nuevo <- .filas_o140(comparar_perfiles(completo_a, completo_b), "patron", "cod")
  expect_identical(nuevo$cambio, "aparecido")
  expect_identical(nuevo$valor_actual, "A9")
})

test_that("H6: el cambio de proporcion de un patron que sigue presente se informa", {
  dep <- function(nombres, codigos) {
    data.frame(dep = c(rep("Montevideo", nombres), rep("MVD-01", codigos)),
               stringsAsFactors = FALSE)
  }
  anterior <- .perfil_o140(dep(95, 5), .f1_o140)
  patrones <- .filas_o140(
    comparar_perfiles(anterior, .perfil_o140(dep(5, 95), .f2_o140)),
    "patron", "dep"
  )
  expect_equal(nrow(patrones), 2L)
  expect_true(all(patrones$cambio == "modificado"))
  codigo <- patrones[patrones$valor_actual == "A+-9+", , drop = FALSE]
  expect_equal(codigo$delta, 95 / 100 - 5 / 100)
  expect_identical(as.character(codigo$severidad), "error")
  nombre <- patrones[patrones$valor_actual == "Aa+", , drop = FALSE]
  expect_equal(nombre$delta, 5 / 100 - 95 / 100)

  # Control: un corrimiento menor que `umbral_cambio` -dos puntos- no da fila.
  menor <- .filas_o140(
    comparar_perfiles(anterior, .perfil_o140(dep(93, 7), .f2_o140)),
    "patron", "dep"
  )
  expect_equal(nrow(menor), 0L)
})

test_that("H7: sin tipo de un lado no se afirma un cambio temporal", {
  # El frame de `perfilar_dbi()` no trae ningun campo de tipo. Un `desvio` en
  # segundos contra uno en dias no se compara, pero tampoco se afirma que el
  # tipo cambio.
  memoria <- data.frame(columna = "fecha", tipo_declarado = "fecha",
                        desvio = 318432.16, stringsAsFactors = FALSE)
  sin_tipo <- data.frame(columna = "fecha", desvio = 3.685557,
                         stringsAsFactors = FALSE)
  comparacion <- comparar_equivalencia(memoria, sin_tipo, tolerancia = 1e-9)
  detalle <- attr(comparacion, "detalle_campos_no_comparables")
  motivo <- detalle$motivo[!is.na(detalle$columna) & detalle$campo == "desvio"]
  expect_identical(motivo, "tipo_desconocido:temporal_vs_sin_tipo")
  expect_false(any(comparacion$campo == "desvio"))

  # Control: con el otro tipo conocido, el cambio si se afirma.
  numerico <- sin_tipo
  numerico$tipo_declarado <- "doble"
  detalle <- attr(
    comparar_equivalencia(memoria, numerico, tolerancia = 1e-9),
    "detalle_campos_no_comparables"
  )
  expect_identical(
    detalle$motivo[!is.na(detalle$columna) & detalle$campo == "desvio"],
    "tipo_cambiado:temporal_vs_no_temporal"
  )
})

test_that("H7: fechas que el motor devuelve como fechas no publican un cambio de tipo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- data.frame(fecha = as.Date("2026-01-01") + c(0, 5, 5, 9))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  expect_s3_class(DBI::dbReadTable(con, "t")$fecha, "Date")
  comparacion <- comparar_equivalencia(
    perfilar(datos, analizar_dependencias = FALSE, proteger_datos_personales = TRUE),
    suppressWarnings(perfilar_dbi(con, "t", proteger_datos_personales = TRUE)), tolerancia = 1e-9
  )
  motivos <- attr(comparacion, "detalle_campos_no_comparables")$motivo
  expect_false(any(motivos == "tipo_cambiado:temporal_vs_no_temporal",
                   na.rm = TRUE))
})

test_that("H8: un cambio significativo de cardinalidad es sospechoso en los dos sentidos", {
  ids <- sprintf("T%03d", 1:40)
  repetidos <- ids
  repetidos[seq(2, 40, 2)] <- ids[seq(1, 40, 2)]
  tabla <- function(id) data.frame(id_tramite = id, v = 1:40, stringsAsFactors = FALSE)
  baja <- .filas_o140(
    comparar_perfiles(.perfil_o140(tabla(ids), .f1_o140),
                      .perfil_o140(tabla(repetidos), .f2_o140)),
    "cardinalidad", "id_tramite"
  )
  # 20 distintos de 40 contra 40 de 40.
  expect_equal(baja$delta, 20 / 40 - 40 / 40)
  expect_true(baja$significativo)
  expect_identical(as.character(baja$severidad), "sospechoso")
  sube <- .filas_o140(
    comparar_perfiles(.perfil_o140(tabla(repetidos), .f1_o140),
                      .perfil_o140(tabla(ids), .f2_o140)),
    "cardinalidad", "id_tramite"
  )
  expect_identical(as.character(sube$severidad), "sospechoso")

  # Control: en faltantes, bajar sigue siendo mejorar.
  faltantes <- .filas_o140(
    comparar_perfiles(.perfil_o140(data.frame(x = c(rep(NA, 10), 1:30)), .f1_o140),
                      .perfil_o140(data.frame(x = 1:40), .f2_o140)),
    "faltantes", "x"
  )
  expect_equal(faltantes$delta, 0 - 10 / 40)
  expect_identical(as.character(faltantes$severidad), "ok")
})

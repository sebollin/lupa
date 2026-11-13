# Fuera de CRAN: corre en la suite completa y en la integracion continua, que
# usan NOT_CRAN=true.
testthat::skip_on_cran()

# Las filas huerfanas de una relacion: cuantas son y cuales. Los esperados se
# calculan a mano o con `%in%` de base sobre los datos, nunca con el paquete.

.huerfanas_esperadas <- function(hija, referencia) {
  which(!is.na(hija) & !(hija %in% referencia))
}

.sucursales_y_ventas <- function() {
  sucursales <- data.frame(
    id_sucursal = 1:4, zona = c("N", "S", "E", "O"), stringsAsFactors = FALSE
  )
  # 40 ventas: las filas 37 y 38 son de una sucursal que no existe (la 9) y la
  # fila 5 no tiene sucursal.
  ventas <- data.frame(
    id_venta = 1:40, id_sucursal = rep(1:4, 10), stringsAsFactors = FALSE
  )
  ventas$id_sucursal[c(37L, 38L)] <- 9L
  ventas$id_sucursal[[5L]] <- NA
  list(sucursales = sucursales, ventas = ventas)
}

.par_de_claves <- function(tablas, ...) {
  detectar_relaciones(
    tablas$sucursales, tablas$ventas,
    columnas_candidatas = list("id_sucursal", "id_sucursal"), ...
  )
}

test_that("el par casi cubierto dice cuantas filas quedan huerfanas y cuales", {
  tablas <- .sucursales_y_ventas()
  relacion <- .par_de_claves(tablas)

  # 39 ventas con sucursal, 37 en una sucursal que existe.
  expect_equal(relacion$cobertura_tabla2_en_tabla1, 37 / 39)
  expect_identical(relacion$n_filas_huerfanas_tabla2, 2L)
  expect_identical(relacion$n_valores_huerfanos_tabla2, 1L)
  traza <- relacion$huerfanas_tabla2[[1L]]
  expect_identical(traza$indices_fila, c(37L, 38L))
  expect_identical(traza$estado, "disponible")
  expect_identical(traza$localizador, "indice_fila")
  expect_identical(traza$total, 2)
  expect_identical(traza$mostrados, 2L)
  expect_false(traza$truncado)
  expect_identical(traza$alcance, "completo")
  # Con las filas, el valor esta a una linea para quien tiene la tabla.
  expect_identical(tablas$ventas$id_sucursal[traza$indices_fila], c(9L, 9L))

  # Del otro lado no falta nada: todas las sucursales tienen ventas.
  expect_identical(relacion$n_filas_huerfanas_tabla1, 0L)
  expect_identical(relacion$n_valores_huerfanos_tabla1, 0L)
  expect_identical(relacion$huerfanas_tabla1[[1L]]$indices_fila, integer())
})

test_that("un ausente en la clave foranea no es huerfano", {
  tablas <- .sucursales_y_ventas()
  relacion <- .par_de_claves(tablas)
  # La fila 5 no entra en el denominador de la cobertura, ni en el conteo, ni
  # en la traza.
  expect_false(5L %in% relacion$huerfanas_tabla2[[1L]]$indices_fila)
  expect_equal(
    relacion$cobertura_tabla2_en_tabla1,
    1 - relacion$n_filas_huerfanas_tabla2 / sum(!is.na(tablas$ventas$id_sucursal))
  )

  # El control: con un valor en esa fila que la referencia no tiene, si lo es.
  tablas$ventas$id_sucursal[[5L]] <- 8L
  con_valor <- .par_de_claves(tablas)
  expect_identical(con_valor$n_filas_huerfanas_tabla2, 3L)
  expect_identical(con_valor$n_valores_huerfanos_tabla2, 2L)
  expect_identical(
    con_valor$huerfanas_tabla2[[1L]]$indices_fila, c(5L, 37L, 38L)
  )
})

test_that("los conteos y las filas coinciden con %in% en los dos lados", {
  # Los dos lados casi cubiertos a la vez, con repetidos y ausentes.
  referencia <- c(1:40, 501L, 502L)
  hija <- c(1:40, 1:40, 900L, NA, 901L, 900L)
  relacion <- detectar_relaciones(
    data.frame(clave = referencia), data.frame(clave = hija)
  )
  filas_1 <- .huerfanas_esperadas(referencia, hija)
  filas_2 <- .huerfanas_esperadas(hija, referencia)
  expect_identical(filas_1, c(41L, 42L))
  expect_identical(filas_2, c(81L, 83L, 84L))

  expect_identical(relacion$n_filas_huerfanas_tabla1, length(filas_1))
  expect_identical(relacion$n_filas_huerfanas_tabla2, length(filas_2))
  expect_identical(
    relacion$n_valores_huerfanos_tabla1, length(unique(referencia[filas_1]))
  )
  expect_identical(
    relacion$n_valores_huerfanos_tabla2, length(unique(hija[filas_2]))
  )
  expect_identical(relacion$huerfanas_tabla1[[1L]]$indices_fila, filas_1)
  expect_identical(relacion$huerfanas_tabla2[[1L]]$indices_fila, filas_2)
  # El total de cada traza es el conteo de su lado.
  expect_identical(relacion$huerfanas_tabla1[[1L]]$total, 2)
  expect_identical(relacion$huerfanas_tabla2[[1L]]$total, 3)
})

test_that("cada par nombra sus filas aunque comparta columna con otro", {
  # `fk` se ubica en dos pares, con huerfanos distintos en cada uno, y del otro
  # lado se ubican dos columnas de la misma tabla. Cada traza tiene que salir de
  # su columna y de los huerfanos de su par.
  tabla1 <- data.frame(a = c(1:20, 99L), b = c(3:22, NA))
  tabla2 <- data.frame(fk = 1:21)
  relacion <- detectar_relaciones(tabla1, tabla2)
  expect_identical(relacion$columna_tabla1, c("a", "b"))
  for (i in 1:2) {
    columna <- tabla1[[relacion$columna_tabla1[[i]]]]
    filas_1 <- .huerfanas_esperadas(columna, tabla2$fk)
    filas_2 <- .huerfanas_esperadas(tabla2$fk, columna)
    expect_identical(relacion$huerfanas_tabla1[[i]]$indices_fila, filas_1)
    expect_identical(relacion$huerfanas_tabla2[[i]]$indices_fila, filas_2)
    expect_identical(relacion$n_filas_huerfanas_tabla1[[i]], length(filas_1))
    expect_identical(relacion$n_filas_huerfanas_tabla2[[i]], length(filas_2))
  }
  # Los esperados, a mano: `a` deja afuera su fila 21 (el 99) y `fk` su fila 21
  # (el 21); `b` su fila 20 (el 22) y `fk` sus filas 1 y 2.
  expect_identical(relacion$huerfanas_tabla1[[1L]]$indices_fila, 21L)
  expect_identical(relacion$huerfanas_tabla2[[1L]]$indices_fila, 21L)
  expect_identical(relacion$huerfanas_tabla1[[2L]]$indices_fila, 20L)
  expect_identical(relacion$huerfanas_tabla2[[2L]]$indices_fila, c(1L, 2L))
})

test_that("sin huerfanas la traza sale vacia y bajo el umbral conserva el total", {
  # Cobertura 1: conteo cero y nada que ubicar.
  completa <- detectar_relaciones(
    data.frame(clave = 1:20), data.frame(clave = rep(1:20, 2))
  )
  expect_identical(completa$n_filas_huerfanas_tabla2, 0L)
  expect_identical(completa$n_valores_huerfanos_tabla2, 0L)
  vacia <- completa$huerfanas_tabla2[[1L]]
  expect_identical(vacia$estado, "no_disponible")
  expect_identical(vacia$indices_fila, integer())
  expect_true(is.na(vacia$total))
  expect_identical(vacia$alcance, "completo")

  # Cobertura 0,8 con el umbral en 0,9: se cuentan y no se nombran.
  referencia <- 1:10
  hija <- c(1:8, 98L, 99L)
  baja <- detectar_relaciones(
    data.frame(clave = referencia), data.frame(clave = hija)
  )
  expect_equal(baja$cobertura_tabla2_en_tabla1, 0.8)
  expect_identical(baja$n_filas_huerfanas_tabla2, 2L)
  retenida <- baja$huerfanas_tabla2[[1L]]
  expect_identical(retenida$estado, "no_disponible")
  expect_identical(retenida$indices_fila, integer())
  expect_identical(retenida$total, 2)
  expect_identical(retenida$alcance, "cobertura_bajo_umbral")

  # El control: los mismos datos con el umbral en 0,8 nombran las dos filas.
  nombrada <- detectar_relaciones(
    data.frame(clave = referencia), data.frame(clave = hija),
    umbral_cobertura = 0.8
  )
  expect_identical(nombrada$huerfanas_tabla2[[1L]]$indices_fila, c(9L, 10L))
  expect_identical(nombrada$huerfanas_tabla2[[1L]]$alcance, "completo")
})

test_that("ningun valor comun: todas las filas son huerfanas, con o sin poda por rangos", {
  # Intercalados -los rangos se solapan y el par se compara- y disjuntos -el
  # par se resuelve sin comparar-: la respuesta es la misma.
  casos <- list(
    intercalados = list(a = c(1L, 3L, 5L, 7L, NA), b = c(2L, 4L, 6L, 6L, NA, 2L)),
    disjuntos = list(a = 1:10, b = c(101:130, NA))
  )
  for (nombre in names(casos)) {
    a <- casos[[nombre]]$a
    b <- casos[[nombre]]$b
    relacion <- detectar_relaciones(
      data.frame(clave = a), data.frame(clave = b), umbral_cobertura = 0
    )
    expect_identical(relacion$cardinalidad, "sin_coincidencias", info = nombre)
    expect_identical(
      relacion$n_filas_huerfanas_tabla1, sum(!is.na(a)), info = nombre
    )
    expect_identical(
      relacion$n_filas_huerfanas_tabla2, sum(!is.na(b)), info = nombre
    )
    expect_identical(
      relacion$n_valores_huerfanos_tabla2, length(unique(b[!is.na(b)])),
      info = nombre
    )
    expect_identical(
      relacion$huerfanas_tabla2[[1L]]$indices_fila, which(!is.na(b)),
      info = nombre
    )
  }
})

test_that("la clase con que se guarda la clave no cambia las filas huerfanas", {
  referencia <- 1:30
  hija <- c(1:30, 1:28, 444L, NA, 445L)
  esperadas <- .huerfanas_esperadas(hija, referencia)
  expect_identical(esperadas, c(59L, 61L))
  formas <- list(
    entero = hija,
    doble = as.numeric(hija),
    texto = as.character(hija),
    factor = factor(hija)
  )
  for (nombre in names(formas)) {
    relacion <- detectar_relaciones(
      data.frame(clave = referencia),
      data.frame(clave = formas[[nombre]], stringsAsFactors = FALSE)
    )
    expect_identical(relacion$n_filas_huerfanas_tabla2, 2L, info = nombre)
    expect_identical(relacion$n_valores_huerfanos_tabla2, 2L, info = nombre)
    expect_identical(
      relacion$huerfanas_tabla2[[1L]]$indices_fila, esperadas, info = nombre
    )
  }
})

test_that("un integer64 sobre 2^53 se cuenta por su valor exacto", {
  skip_if_not_installed("bit64")
  # 2^53 + 1 y 2^53 + 2: como dobles serian el mismo numero.
  grande <- bit64::as.integer64("9007199254740993")
  referencia <- c(bit64::as.integer64(1:30), grande)
  hija <- c(bit64::as.integer64(c(1:30, 1:28)), grande, grande + 1L, grande + 1L)
  # El rango de la columna se calcula como doble y bit64 avisa que pierde
  # precision; es anterior a los conteos y no es lo que se mide aca. Cualquier
  # otro aviso sigue a la vista.
  relacion <- withCallingHandlers(
    detectar_relaciones(
      data.frame(clave = referencia), data.frame(clave = hija)
    ),
    warning = function(aviso) {
      if (grepl("precision lost", conditionMessage(aviso), fixed = TRUE,
                useBytes = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  expect_identical(relacion$n_filas_huerfanas_tabla2, 2L)
  expect_identical(relacion$n_valores_huerfanos_tabla2, 1L)
  expect_identical(relacion$huerfanas_tabla2[[1L]]$indices_fila, c(60L, 61L))
})

test_that("otra caja o un espacio al borde son otro valor, y salen huerfanos", {
  referencia <- sprintf("C%02d", 1:30)
  hija <- c(referencia, referencia[1:20], "c05", "C07 ", " C09", "C11")
  relacion <- detectar_relaciones(
    data.frame(clave = referencia, stringsAsFactors = FALSE),
    data.frame(clave = hija, stringsAsFactors = FALSE)
  )
  esperadas <- .huerfanas_esperadas(hija, referencia)
  expect_identical(esperadas, 51:53)
  expect_identical(relacion$n_filas_huerfanas_tabla2, 3L)
  expect_identical(relacion$huerfanas_tabla2[[1L]]$indices_fila, esperadas)
})

test_that("el tope recorta los indices y conserva el total", {
  # 60 huerfanas entre 1.000 filas, intercaladas: cobertura 0,94.
  referencia <- 1:500
  hija <- rep(1:500, 2)
  posiciones <- seq(10L, 1000L, length.out = 60L)
  hija[posiciones] <- 9000L + seq_along(posiciones)
  esperadas <- .huerfanas_esperadas(hija, referencia)
  expect_length(esperadas, 60L)
  tablas <- list(data.frame(clave = referencia), data.frame(clave = hija))

  por_omision <- detectar_relaciones(tablas[[1L]], tablas[[2L]])
  expect_identical(por_omision$huerfanas_tabla2[[1L]]$indices_fila, esperadas)
  expect_identical(por_omision$huerfanas_tabla2[[1L]]$limite, 1000L)

  recortada <- detectar_relaciones(
    tablas[[1L]], tablas[[2L]], max_filas_huerfanas = 25
  )
  traza <- recortada$huerfanas_tabla2[[1L]]
  expect_identical(traza$indices_fila, esperadas[1:25])
  expect_identical(traza$estado, "truncada")
  expect_true(traza$truncado)
  expect_identical(traza$total, 60)
  expect_identical(traza$mostrados, 25L)
  expect_identical(traza$limite, 25L)
  # El conteo no depende del tope.
  expect_identical(recortada$n_filas_huerfanas_tabla2, 60L)

  sin_tope <- detectar_relaciones(
    tablas[[1L]], tablas[[2L]], max_filas_huerfanas = Inf
  )
  expect_identical(sin_tope$huerfanas_tabla2[[1L]]$indices_fila, esperadas)
  expect_identical(sin_tope$huerfanas_tabla2[[1L]]$limite, Inf)

  for (invalido in list(0, -1, NA, "a", 1.5, c(1, 2), NULL)) {
    expect_error(
      detectar_relaciones(
        tablas[[1L]], tablas[[2L]], max_filas_huerfanas = invalido
      ),
      "max_filas_huerfanas"
    )
  }
})

test_that("un par que no se compara no cuenta huerfanos", {
  sin_comparar <- list(
    familias = detectar_relaciones(
      data.frame(clave = 1:5),
      data.frame(clave = letters[1:5], stringsAsFactors = FALSE), podar = TRUE
    ),
    presupuesto = detectar_relaciones(
      data.frame(clave = 1:5), data.frame(clave = 1:5), tope_memoria_mb = 0
    ),
    fecha_e_instante = detectar_relaciones(
      data.frame(clave = as.Date("2020-01-01") + 0:4),
      data.frame(clave = as.POSIXct("2020-01-01", tz = "UTC") + 0:4)
    )
  )
  for (nombre in names(sin_comparar)) {
    relacion <- sin_comparar[[nombre]]
    expect_identical(relacion$cardinalidad, "sin_comparar", info = nombre)
    for (conteo in c(
      "n_filas_huerfanas_tabla1", "n_filas_huerfanas_tabla2",
      "n_valores_huerfanos_tabla1", "n_valores_huerfanos_tabla2"
    )) {
      expect_identical(relacion[[conteo]], NA_integer_, info = nombre)
    }
    for (lado in c("huerfanas_tabla1", "huerfanas_tabla2")) {
      traza <- relacion[[lado]][[1L]]
      expect_identical(traza$estado, "no_disponible", info = nombre)
      expect_identical(traza$alcance, "no_evaluado", info = nombre)
      expect_identical(traza$indices_fila, integer(), info = nombre)
      expect_true(is.na(traza$total), info = nombre)
    }
  }
})

test_that("las columnas que ya se publicaban conservan su lugar, y sin pares el esquema es el mismo", {
  tablas <- .sucursales_y_ventas()
  relacion <- detectar_relaciones(tablas$sucursales, tablas$ventas)
  expect_identical(
    names(relacion)[1:7],
    c(
      "columna_tabla1", "columna_tabla2", "cardinalidad", "n_valores_comunes",
      "cobertura_tabla1_en_tabla2", "cobertura_tabla2_en_tabla1", "motivo_poda"
    )
  )
  expect_identical(
    names(relacion)[8:13],
    c(
      "n_filas_huerfanas_tabla1", "n_filas_huerfanas_tabla2",
      "n_valores_huerfanos_tabla1", "n_valores_huerfanos_tabla2",
      "huerfanas_tabla1", "huerfanas_tabla2"
    )
  )
  # Una traza por par, del lado que corresponde.
  expect_length(relacion$huerfanas_tabla1, nrow(relacion))
  expect_length(relacion$huerfanas_tabla2, nrow(relacion))

  sin_pares <- detectar_relaciones(data.frame(), data.frame())
  expect_identical(nrow(sin_pares), 0L)
  expect_identical(names(sin_pares), names(relacion))
  expect_identical(
    vapply(sin_pares, function(columna) class(columna)[[1L]], character(1L)),
    vapply(relacion, function(columna) class(columna)[[1L]], character(1L))
  )
})

test_that("el resultado no lleva los valores huerfanos", {
  # Los huerfanos son identificadores largos, que no estan en ningun otro lado
  # de las dos tablas.
  referencia <- sprintf("DOC-%08d", 1:40)
  huerfanos <- c("DOC-99911122", "DOC-99933344")
  hija <- c(
    referencia, referencia[1:10], huerfanos[[1L]], referencia[[11L]],
    huerfanos[[2L]]
  )
  relacion <- detectar_relaciones(
    data.frame(documento = referencia, stringsAsFactors = FALSE),
    data.frame(documento = hija, stringsAsFactors = FALSE)
  )
  expect_identical(relacion$huerfanas_tabla2[[1L]]$indices_fila, c(51L, 53L))
  # Ni en las columnas, ni en las trazas, ni en los atributos.
  contenido <- paste(
    utils::capture.output(dput(unclass(relacion))), collapse = "\n"
  )
  for (valor in c(huerfanos, "99911122", "99933344")) {
    expect_false(grepl(valor, contenido, fixed = TRUE, useBytes = TRUE))
  }
  # El control: el volcado si muestra lo que el resultado lleva.
  expect_true(grepl("documento", contenido, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("51L, 53L", contenido, fixed = TRUE, useBytes = TRUE))
  # Las trazas no tienen mas campos que los de la trazabilidad de un hallazgo.
  expect_identical(
    names(relacion$huerfanas_tabla2[[1L]]),
    c(
      "estado", "localizador", "indices_fila", "claves", "total", "mostrados",
      "truncado", "limite", "alcance"
    )
  )
  expect_null(relacion$huerfanas_tabla2[[1L]]$claves)
})

test_that("las trazas cuentan en el presupuesto de memoria", {
  tablas <- .sucursales_y_ventas()
  relacion <- .par_de_claves(tablas)
  ocupado <- attr(relacion, "memoria_resultado_mb") * 1024^2
  trazas <- as.numeric(utils::object.size(relacion$huerfanas_tabla1[[1L]])) +
    as.numeric(utils::object.size(relacion$huerfanas_tabla2[[1L]]))
  # Las trazas ocupan, y lo que el resultado declara ocupar las incluye.
  expect_gt(trazas, 0)
  expect_gt(ocupado, trazas)
})

.con_huerfanas <- function(motor) {
  skip_if_not_installed("DBI")
  if (identical(motor, "sqlite")) {
    skip_if_not_installed("RSQLite")
    return(DBI::dbConnect(RSQLite::SQLite(), ":memory:"))
  }
  skip_if_not_installed("duckdb")
  DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
}

.cerrar_huerfanas <- function(con, motor) {
  if (identical(motor, "duckdb")) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  } else {
    DBI::dbDisconnect(con)
  }
}

test_that("la coleccion publica los conteos de la lectura y ninguna traza", {
  tablas <- .sucursales_y_ventas()
  conteos <- c(
    "n_filas_huerfanas_tabla1", "n_filas_huerfanas_tabla2",
    "n_valores_huerfanos_tabla1", "n_valores_huerfanos_tabla2"
  )
  for (motor in c("sqlite", "duckdb")) {
    con <- .con_huerfanas(motor)
    DBI::dbWriteTable(con, "sucursales", tablas$sucursales)
    DBI::dbWriteTable(con, "ventas", tablas$ventas)
    col <- coleccion(con, c("sucursales", "ventas"), nombre = "ventas")
    resultado <- relaciones_coleccion(
      col,
      pares = data.frame(
        tabla_1 = "sucursales", tabla_2 = "ventas", stringsAsFactors = FALSE
      ),
      columnas_candidatas = list(
        sucursales = "id_sucursal", ventas = "id_sucursal"
      ),
      orden = list(sucursales = "id_sucursal", ventas = "id_venta")
    )
    .cerrar_huerfanas(con, motor)
    relaciones <- resultado$relaciones
    expect_identical(nrow(relaciones), 1L, info = motor)
    # A mano: dos ventas de la sucursal 9, que no existe; la venta sin
    # sucursal no cuenta.
    expect_identical(relaciones$n_filas_huerfanas_tabla2, 2L, info = motor)
    expect_identical(relaciones$n_valores_huerfanos_tabla2, 1L, info = motor)
    expect_identical(relaciones$n_filas_huerfanas_tabla1, 0L, info = motor)
    expect_identical(relaciones$n_valores_huerfanos_tabla1, 0L, info = motor)
    # Los mismos que en memoria.
    en_memoria <- .par_de_claves(tablas)
    for (conteo in conteos) {
      expect_identical(relaciones[[conteo]], en_memoria[[conteo]], info = motor)
    }
    # Los conteos van al final, y las filas no se nombran.
    expect_identical(utils::tail(names(relaciones), 4L), conteos, info = motor)
    expect_false(
      any(c("huerfanas_tabla1", "huerfanas_tabla2") %in% names(relaciones)),
      info = motor
    )
    expect_false(
      any(vapply(relaciones, is.list, logical(1L))), info = motor
    )
    # Contar no costo ninguna consulta: una lectura por tabla.
    expect_identical(nrow(resultado$meta$lecturas), 2L, info = motor)
  }
})

test_that("sin relaciones, la coleccion conserva las columnas de conteo", {
  conteos <- c(
    "n_filas_huerfanas_tabla1", "n_filas_huerfanas_tabla2",
    "n_valores_huerfanos_tabla1", "n_valores_huerfanos_tabla2"
  )
  con <- .con_huerfanas("sqlite")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(clave = 1:10))
  DBI::dbWriteTable(con, "b", data.frame(clave = c(1:10, 1:9, 77L)))
  DBI::dbWriteTable(con, "c", data.frame(clave = 201:230))
  col <- coleccion(con, c("a", "b", "c"), nombre = "tres")

  llena <- relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "a", tabla_2 = "b", stringsAsFactors = FALSE)
  )$relaciones
  vacia <- relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "a", tabla_2 = "c", stringsAsFactors = FALSE)
  )$relaciones
  expect_identical(nrow(llena), 1L)
  expect_identical(nrow(vacia), 0L)
  expect_identical(llena$n_filas_huerfanas_tabla2, 1L)
  expect_identical(utils::tail(names(vacia), 4L), conteos)
  expect_identical(utils::tail(names(llena), 4L), conteos)
  for (conteo in conteos) {
    expect_identical(class(vacia[[conteo]]), class(llena[[conteo]]))
  }
})

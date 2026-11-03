test_that("la identidad de filas conserva particiones sin interaction", {
  skip_on_cran()
  set.seed(42)
  n <- 240L
  acento <- rawToChar(as.raw(c(0xc3, 0xa1)))
  datos <- data.frame(
    texto = sample(c(NA_character_, "", paste0(" ", acento), "A", "a"), n, TRUE),
    factor = factor(sample(c(NA_character_, "uno", "dos"), n, TRUE),
                    exclude = NULL),
    fecha = as.Date("2020-01-01") + sample(c(NA_integer_, 0:2), n, TRUE),
    numero = sample(c(-0, 0, NaN, 1, 2), n, TRUE),
    stringsAsFactors = FALSE
  )
  factores <- lapply(datos, function(x) {
    lupa:::.factor_por_bytes(
      lupa:::.texto_identidad(lupa:::.valores_relacion(x)),
      exclude = NULL
    )
  })
  anterior <- as.integer(do.call(
    interaction, c(factores, list(drop = TRUE, lex.order = TRUE))
  ))
  # Los mismos codigos, no solo la misma particion: quien parte con `split()`
  # recibe los grupos en el mismo orden que con `interaction()`.
  expect_identical(lupa:::.codigos_filas(datos), anterior)

  base <- data.frame(
    a = sample(1:60000, 59900L),
    b = sample(rep_len(1:58000, 59900L)),
    c = sample(1:2, 59900L, TRUE)
  )
  grande <- rbind(base, base[sample.int(59900L, 200L), , drop = FALSE])
  expect_warning(codigos <- lupa:::.codigos_filas(grande), NA)
  expect_false(anyNA(codigos))
  expect_equal(sum(duplicated(grande)), 200L)
  expect_equal(sum(duplicated(codigos)), 200L)
  expect_warning(exactos <- lupa:::.codigos_filas_exactos(grande), NA)
  expect_false(anyNA(exactos))
  expect_equal(sum(duplicated(exactos)), 200L)

  perfil <- suppressWarnings(suppressMessages(perfilar(
    grande, analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(perfil, grande)))
  acciones <- plan[plan$estrategia == "conservar_primera_duplicada", ,
                   drop = FALSE]
  expect_true(nrow(acciones) >= 1L)
  plan$aplicar <- FALSE
  plan$aplicar[match(acciones$id_accion[[1L]], plan$id_accion)] <- TRUE
  limpio <- aplicar(plan, grande, permitir_eliminacion = TRUE)$datos
  expect_equal(nrow(limpio), 59900L)
})

test_that("filas completas admite listas y tipos compuestos", {
  datos <- data.frame(id = 1:3)
  datos$binaria <- I(list(as.raw(1), NULL, raw()))
  datos$fecha <- as.POSIXlt(
    as.POSIXct("2020-01-01", tz = "UTC") + (0:2) * 86400
  )
  datos$matriz <- I(matrix(1:6, nrow = 3L))
  datos$anidada <- I(list(
    data.frame(valor = 1),
    data.frame(valor = NA_real_),
    data.frame(valor = 3)
  ))
  expect_no_error(completas <- lupa:::.filas_completas(datos))
  expect_equal(completas, c(TRUE, FALSE, TRUE))

  # Una tabla anidada como columna: `complete.cases()` no la acepta.
  anidada <- data.frame(id = 1:3)
  anidada$sub <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA))
  expect_equal(lupa:::.filas_completas(anidada), c(TRUE, FALSE, FALSE))

  if (requireNamespace("bit64", quietly = TRUE)) {
    enteros <- data.frame(
      valor = bit64::as.integer64(c(1, 2, NA))
    )
    expect_no_error(completos_64 <- lupa:::.filas_completas(enteros))
    expect_equal(completos_64, c(TRUE, TRUE, FALSE))
  }

  sin_medidas <- function(tablas, instancia) {
    data.frame(
      resultado = logical(), entidad = character(), atributo = character(),
      fila = integer(), objeto = character(), stringsAsFactors = FALSE
    )
  }
  metrica_vacia <- metrica(
    "NoMideLista", "prueba", "instanciaAtributo", "booleano",
    metodo = sin_medidas
  )
  instancia <- instanciar(especializar(metrica_vacia), "t", "binaria")
  expect_no_error(medidas <- suppressWarnings(medir(
    modelo(instancia), list(t = datos[c("id", "binaria")]),
    id_medicion = "lista"
  )))
  cobertura <- attr(medidas, "cobertura_metricas", exact = TRUE)
  expect_equal(cobertura$estado, "no_medible")
})

test_that("NoNulo usa la normalizacion textual del perfil", {
  skip_on_cran()
  datos <- data.frame(
    x = c("dato", "otro", "tercero", "cuarto", "quinto", "sexto",
          "", "   ", "\t", NA_character_),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  fila <- perfil$columnas[perfil$columnas$columna == "x", , drop = FALSE]
  expect_equal(fila$n_faltantes_totales, 4L)
  especifica <- especializar(
    metricas_nucleo()$NoNulo, nombre_especifico = "NoNuloBlancos",
    valores_nulos = ""
  )
  medidas <- medir(
    modelo(instanciar(especifica, "t", "x")), list(t = datos),
    id_medicion = "blancos", fecha = "2026-01-01"
  )
  expect_equal(sum(medidas$resultado), 6L)
})

test_that("agregar conserva la instancia en nombre e identidad", {
  datos <- data.frame(
    a = c(1, 1, 2, 3), b = c("x", "x", "y", "z"),
    c = c(10, 10, 20, 30), d = c("u", "u", "v", "w"),
    stringsAsFactors = FALSE
  )
  nucleo <- metricas_nucleo()
  especifica <- especializar(
    nucleo$ConjuntoAtributosDuplicado, nombre_especifico = "ClaveDuplicada"
  )
  medir_y_agregar <- function(instancias, id) {
    agregar(
      medir(modelo(instancias), list(t = datos), id_medicion = id,
            fecha = "2026-01-01"),
      "entidad", "ratio"
    )
  }
  agregadas <- medir_y_agregar(list(
    instanciar(especifica, "t", c("a", "b")),
    instanciar(especifica, "t", c("c", "d"))
  ), "compuestas")
  expect_equal(nrow(agregadas), 2L)
  expect_setequal(
    agregadas$metrica_instanciada,
    c("agregada:ratio:ClaveDuplicada@t.a.b",
      "agregada:ratio:ClaveDuplicada@t.c.d")
  )
  expect_equal(anyDuplicated(agregadas$id_medida), 0L)

  evaluar_agregadas <- function(x) {
    regla <- regla_evaluacion(
      "pasa", function(valor) valor >= 0,
      metricas = unique(x$metrica_instanciada)
    )
    evaluar(x, perfil_evaluacion("P", regla))
  }
  expect_no_error(evaluar_agregadas(agregadas))
  expect_no_error(tablero_calidad(agregadas))

  simple <- instanciar(
    especializar(nucleo$EntidadDuplicada, nombre_especifico = "ClaveDuplicada"),
    "t"
  )
  compuesta <- instanciar(especifica, "t", c("a", "b"))
  medicion_mixta <- medir(
    modelo(simple, compuesta), list(t = datos), id_medicion = "mixta",
    fecha = "2026-01-01"
  )
  mixta <- rbind(
    agregar(
      medicion_mixta[medicion_mixta$metrica_instanciada == simple$nombre, ,
                     drop = FALSE],
      "entidad", "ratio"
    ),
    agregar(
      medicion_mixta[medicion_mixta$metrica_instanciada == compuesta$nombre, ,
                     drop = FALSE],
      "entidad", "ratio"
    )
  )
  expect_equal(nrow(mixta), 2L)
  expect_equal(anyDuplicated(mixta$id_medida), 0L)
  expect_no_error(evaluar_agregadas(mixta))
  expect_no_error(tablero_calidad(mixta))
})

test_that("las columnas compuestas dan un valor por fila", {
  # Una matriz de listas: un valor por fila, no uno por celda.
  m <- matrix(list(NULL, raw(0), NA, list(NA), 5, list(1)), nrow = 3)
  d <- data.frame(id = 1:3)
  d$m <- m
  expect_equal(lupa:::.filas_completas(d), c(FALSE, TRUE, FALSE))
  # Un entorno en una celda es un valor, y `anyNA()` no lo acepta.
  e <- data.frame(id = 1:3)
  e$v <- I(list(new.env(), 1:2, NULL))
  expect_equal(lupa:::.filas_completas(e), c(TRUE, TRUE, FALSE))
  # Una matriz como columna se aplana: las filas 1 y 3 son la misma entidad.
  d <- data.frame(id = c(1, 2, 1))
  d$m <- matrix(c(1, 3, 1, 2, 4, 2), nrow = 3)
  expect_equal(lupa:::.codigos_filas(d), c(1L, 2L, 1L))
})

test_that("un codigo ausente deja la fila sin codigo, como interaction()", {
  f1 <- factor(c("a", NA, "b", "a"))
  f2 <- factor(c("x", "y", NA, "x"))
  expect_identical(
    lupa:::.codigos_filas_combinados(list(as.integer(f1), as.integer(f2))),
    as.integer(interaction(f1, f2, drop = TRUE, lex.order = TRUE))
  )
})

test_that("agregar conserva el orden numerico de las filas", {
  m <- medir(
    modelo(instanciar(especializar(metricas_nucleo()$NoNulo), "t", "x")),
    list(t = data.frame(x = rep(1, 12))), id_medicion = "orden",
    fecha = "2026-01-01"
  )
  expect_equal(agregar(m, "instanciaEntidad", "ratio")$fila, 1:12)
})

test_that("arriba de la entidad no se mezclan dos instancias de una tabla", {
  base <- especializar(metricas_nucleo()$ConjuntoAtributosDuplicado)
  datos <- data.frame(a = c(1, 1, 2, 2, 3), b = c("x", "x", "y", "y", "z"),
                      c = c(1, 2, 2, 2, 3), stringsAsFactors = FALSE)
  dos <- agregar(medir(
    modelo(instanciar(base, "t", c("a", "b")), instanciar(base, "t", c("a", "c"))),
    list(t = datos), id_medicion = "dos", fecha = "2026-01-01"
  ), "entidad", "ratio")
  expect_equal(dos$resultado, c(0.8, 0.4))
  expect_error(agregar(dos, "conjuntoEntidades", "promedio"),
               "m\u00e1s de una instancia")
  # Una instancia por tabla sigue subiendo.
  una <- agregar(medir(
    modelo(instanciar(base, "t", c("a", "b")), instanciar(base, "u", c("a", "b"))),
    list(t = datos, u = datos[1:3, ]), id_medicion = "una", fecha = "2026-01-01"
  ), "entidad", "ratio")
  expect_equal(nrow(agregar(una, "conjuntoEntidades", "promedio")), 1L)
})

test_that("separadas por instancia, dos claves suben a coleccion y a organizacion", {
  skip_if_not_installed("RSQLite")
  base <- especializar(metricas_nucleo()$ConjuntoAtributosDuplicado)
  datos <- data.frame(a = c(1, 1, 2, 3), b = c(1, 1, 2, 3),
                      x = c("p", "q", "r", "s"), stringsAsFactors = FALSE)
  m <- medir(
    modelo(instanciar(base, "t", c("a", "b")), instanciar(base, "t", c("a", "x"))),
    list(t = datos), id_medicion = "r", fecha = "2026-01-01"
  )
  por_instancia <- split(agregar(m, "entidad", "ratio"),
                         agregar(m, "entidad", "ratio")$metrica_instanciada)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (a INTEGER, b INTEGER, x TEXT)")
  col <- coleccion(con, "t", nombre = "col1")
  partes <- do.call(rbind, lapply(por_instancia, function(x) {
    agregar(x, "coleccion", "promedio_ponderado", pesos = 1, coleccion = col)
  }))
  expect_equal(anyDuplicated(partes$id_medida), 0L)
  expect_true(all(grepl("@t[.]a[.]", partes$metrica_instanciada)))
  org <- organizacion(nombre = "org1", colecciones = list(col))
  expect_no_error(agregar(partes, "organizacion", "promedio_ponderado",
                          pesos = c(0.5, 0.5), organizacion = org))
})

test_that("dos instancias sobre una columna no se funden al subir a atributo", {
  nucleo <- metricas_nucleo()
  datos <- data.frame(x = c("a", "S/D", "NA", NA), y = c("a", "b", "c", "d"),
                      stringsAsFactors = FALSE)
  m <- medir(modelo(
    instanciar(especializar(nucleo$NoNulo, valores_nulos = "S/D"), "t", "x",
               nombre_instancia = "NoNulo.SD@t.x"),
    instanciar(especializar(nucleo$NoNulo, valores_nulos = "NA"), "t", "x",
               nombre_instancia = "NoNulo.NA@t.x"),
    instanciar(especializar(nucleo$NoNulo, valores_nulos = "NA"), "t", "y")
  ), list(t = datos), id_medicion = "r", fecha = "2026-01-01")
  atributos <- agregar(m, "atributo", "ratio")
  expect_equal(nrow(atributos), 3L)
  expect_equal(anyDuplicated(atributos$id_medida), 0L)
  # La columna con una sola instancia conserva el nombre de siempre.
  expect_true("agregada:ratio:NoNulo" %in% atributos$metrica_instanciada)
  expect_error(agregar(atributos, "entidad", "promedio"),
               "m\u00e1s de una instancia")
})

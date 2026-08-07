tipos_de_columna <- function() {
  indice <- c(1L, 2L, 1L, 2L, 3L, 3L)
  data.frame(
    texto = c("A", "B", "A", "B", "C", "C"),
    factor = factor(c("A", "B", "A", "B", "C", "C")),
    ordenado = ordered(c("A", "B", "A", "B", "C", "C")),
    entero = as.integer(indice),
    doble = as.double(indice),
    logico = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
    fecha = as.Date("2020-01-01") + indice,
    fecha_hora = as.POSIXct("2020-01-01", tz = "UTC") + indice * 86400,
    stringsAsFactors = FALSE
  )
}

test_that("el perfilado recorre la matriz de tipos", {
  datos <- tipos_de_columna()
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  declarados <- setNames(
    as.character(perfil$columnas$tipo_declarado), perfil$columnas$columna
  )
  expect_identical(declarados[names(datos)], c(
    texto = "texto", factor = "factor", ordenado = "factor-ordenado",
    entero = "entero", doble = "doble", logico = "logico",
    fecha = "fecha", fecha_hora = "fecha-hora"
  ))
  expect_s3_class(analizar(datos, analizar_dependencias = FALSE), "analisis")
})

test_that("el catalogo y la limpieza aceptan todos los tipos", {
  datos <- tipos_de_columna()
  nucleo <- metricas_nucleo()
  instancias <- lapply(names(datos), function(nombre) {
    instanciar(
      especializar(nucleo$NoNulo, nombre_especifico = paste0("NoNulo_", nombre)),
      "tabla", nombre
    )
  })
  medicion <- medir(modelo(instancias), datos, id_medicion = "tipos")
  expect_equal(nrow(medicion), sum(vapply(datos, length, integer(1L))))

  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil)
  salida <- aplicar(plan, datos)$datos
  expect_true(all(names(datos) %in% names(salida)))
  expect_equal(nrow(salida), nrow(datos))
})

test_that("los marcos aceptan una columna adicional de cada tipo", {
  datos <- tipos_de_columna()
  for (nombre in names(datos)) {
    marco <- marco_calidad("Tipos", data.frame(
      dimension = "D", factor = "F", evidencia = datos[[nombre]][[1L]],
      stringsAsFactors = FALSE
    ))
    expect_no_error(as.data.frame(marco))
  }
})

test_that("duplicados compara cada tipo en teselas, LSH y bloqueo", {
  skip_if_not_installed("stringdist")
  datos <- tipos_de_columna()
  for (nombre in names(datos)) {
    exacto <- detectar_duplicados_aproximados(
      datos, columnas = nombre, estrategia = "teselas",
      max_pares = Inf, max_resultados = 1L
    )
    lsh <- detectar_duplicados_aproximados(
      datos, columnas = nombre, estrategia = "lsh",
      max_resultados = 1L
    )
    bloqueado <- detectar_duplicados_aproximados(
      datos, columnas = nombre, bloquear_por = nombre,
      estrategia = "teselas", max_pares = Inf, max_resultados = 1L
    )
    expect_true(is.data.frame(exacto$pares), info = nombre)
    expect_true(is.data.frame(lsh$pares), info = nombre)
    expect_true(is.data.frame(bloqueado$pares), info = nombre)
    expect_true(isTRUE(bloqueado$alcance$bloqueo_pares_alcanzables > 0),
                info = nombre)
    if (inherits(datos[[nombre]], c("Date", "POSIXt"))) {
      expect_true(any(grepl("2020-", exacto$hallazgos$evidencia, fixed = TRUE)),
                  info = nombre)
      expect_true(any(grepl("2020-", lsh$hallazgos$evidencia, fixed = TRUE)),
                  info = nombre)
    }
  }
  for (estrategia in c("teselas", "lsh")) {
    entre_fechas <- detectar_duplicados_aproximados(
      datos, columnas = c("fecha", "fecha_hora"), estrategia = estrategia,
      max_pares = Inf, max_resultados = 1L
    )
    expect_true(nrow(entre_fechas$hallazgos) > 0L, info = estrategia)
    expect_true(any(grepl("fecha=2020-", entre_fechas$hallazgos$evidencia,
                          fixed = TRUE)), info = estrategia)
  }
})

test_that("el histórico convierte a texto la columna de cada tipo", {
  datos <- tipos_de_columna()
  for (nombre in names(datos)) {
    x <- data.frame(
      fecha = as.Date("2026-01-01") + 0:5,
      resultado = seq_len(nrow(datos)),
      stringsAsFactors = FALSE
    )
    x$metrica <- datos[[nombre]]
    historico <- lupa:::.parte_historico(
      x, "medida", paste0("r", seq_len(nrow(x)))
    )
    expect_type(historico$metrica, "character")
    expect_identical(attr(historico$fecha, "tzone"), "UTC")
  }
})

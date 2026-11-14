# Lo que el perfil calcula sobre una columna se paga una vez por bloque o por
# columna, no una vez por valor: cada prueba cuenta el trabajo y comprueba que
# la respuesta es la de siempre.

test_that("el mapa de distintos mide su estado una vez por bloque, no por valor nuevo", {
  skip_on_cran()
  set.seed(3)
  x <- c(round(stats::rnorm(3000), 6), NA, -0.5, -0.5)
  mediciones <- 0L
  original <- .bytes_estado_acumulador
  local_mocked_bindings(.bytes_estado_acumulador = function(...) {
    mediciones <<- mediciones + 1L
    original(...)
  })
  mapa <- .mapa_distintos_bloques(x)
  presentes <- x[!is.na(x)]
  distintos <- unique(presentes)
  expect_identical(mapa$estado, "calculado")
  expect_identical(mapa$resultado$representante, distintos)
  expect_identical(
    mapa$resultado$frecuencia,
    tabulate(match(presentes, distintos), nbins = length(distintos))
  )
  expect_identical(
    mapa$resultado$primer_ordinal,
    as.numeric(match(distintos, x))
  )
  # Antes se median el estado entero y se lo agrandaba con `c()` por cada uno
  # de los 2.999 valores nuevos: cuadratico en los distintos del bloque.
  expect_lte(mediciones, 3L)
})

test_that("el tope de bytes del mapa corta en el mismo valor que agregando de a uno", {
  skip_on_cran()
  set.seed(4)
  casos <- list(
    dobles = round(stats::rnorm(3000), 6),
    textos = sprintf("C%05d", sample.int(20000L, 3000L)),
    fechas = as.Date("2000-01-01") + sample.int(9000L, 3000L)
  )
  for (nombre in names(casos)) {
    x <- casos[[nombre]]
    # Con bloques de un valor cada paso agrega uno solo y mide el estado: es el
    # corte de referencia.
    de_a_uno <- .ejecutar_vector_bloques(
      x, "distintos", tamano = 1L, max_bytes = 30000
    )
    por_bloques <- .ejecutar_vector_bloques(
      x, "distintos", tamano = 1000L, max_bytes = 30000
    )
    expect_identical(por_bloques$resultado$estado, "cota", info = nombre)
    expect_identical(
      por_bloques$resultado$motivo, "mapa_distintos_truncado:bytes",
      info = nombre
    )
    expect_identical(
      por_bloques$resultado$cota, de_a_uno$resultado$cota, info = nombre
    )
    expect_identical(
      por_bloques$acumulador$estado_familia$representantes,
      de_a_uno$acumulador$estado_familia$representantes, info = nombre
    )
  }
})

test_that("la columna se lee como numero una vez para la secuencia, los faltantes y el resumen", {
  skip_on_cran()
  datos <- data.frame(
    v = c(sprintf("%d,5", 1:60), "abc"), stringsAsFactors = FALSE
  )
  con_centinela <- data.frame(
    v = c(sprintf("%d,5", 1:60), "-999", "abc"), stringsAsFactors = FALSE
  )
  lecturas <- 0L
  original <- .valores_cuantitativos
  local_mocked_bindings(.valores_cuantitativos = function(...) {
    lecturas <<- lecturas + 1L
    original(...)
  })
  p <- perfilar(datos, analizar_dependencias = FALSE)
  n_por_omision <- lecturas
  lecturas <- 0L
  declarados <- perfilar(con_centinela, analizar_dependencias = FALSE,
                         sentinelas_numericos = -999)
  n_declarados <- lecturas
  fila <- p$columnas
  expect_identical(fila$tipo_inferido, "doble")
  expect_identical(as.integer(fila$n_valores_excluidos_resumen), 1L)
  expect_identical(fila$minimo, 1.5)
  expect_identical(fila$maximo, 60.5)
  expect_identical(declarados$columnas$minimo, 1.5)
  expect_identical(declarados$columnas$n_faltantes_disfrazados, 1L)
  # La secuencia, los faltantes disfrazados y el resumen la leian cada uno:
  # tres lecturas iguales. Con centinelas declarados el resumen lee aparte.
  expect_identical(n_por_omision, 1L)
  expect_identical(n_declarados, 2L)
})

test_that("la lectura como numero no vuelve a recortar para buscar centinelas", {
  skip_on_cran()
  x <- c(rep(c(" 12,5", "7,25 ", "3"), 500L), "-999", NA)
  inferencia <- list(tipo = "doble", convencion_numero_texto = "decimal_coma")
  celdas <- 0
  suppressMessages(trace(
    "trimws", where = asNamespace("base"), print = FALSE,
    tracer = bquote(assign(
      "celdas", get("celdas", envir = .(environment())) + length(x),
      envir = .(environment())
    ))
  ))
  on.exit(suppressMessages(untrace("trimws", where = asNamespace("base"))),
          add = TRUE)
  sin_declarar <- .valores_cuantitativos(x, inferencia, NULL)
  celdas_sin_declarar <- celdas
  declarados <- .valores_cuantitativos(x, inferencia, NULL,
                                       sentinelas_numericos = -999)
  expect_identical(sin_declarar$valores[1:3], c(12.5, 7.25, 3))
  expect_identical(sin_declarar$valores[[1501L]], -999)
  expect_identical(which(declarados$mascara_sentinelas_texto), 1501L)
  # El texto recortado ya no se recorta de nuevo para buscar los centinelas: la
  # lectura recortaba tres veces las 1.502 celdas sin centinelas declarados.
  expect_lte(celdas_sin_declarar, 2 * length(x))
})

test_that("la deteccion de listas no descompone entero el numero de cada celda con coma", {
  skip_on_cran()
  x <- c(rep(c("12,5", "1.234,56", "$ 7,25", "USD 3,5", "ABC 4,5"), 180L),
         rep("10, 20, 30", 50L), rep("15", 400L))
  sin_codigo <- x[!startsWith(x, "ABC")]
  descomposiciones <- 0L
  original <- .componentes_numero_texto
  local_mocked_bindings(.componentes_numero_texto = function(...) {
    descomposiciones <<- descomposiciones + 1L
    original(...)
  })
  listas <- .detectar_multivaluados(sin_codigo)
  expect_length(listas, 1L)
  expect_identical(listas[[1L]]$delimitador, ",")
  expect_identical(listas[[1L]]$indices, 721:770)
  expect_identical(listas[[1L]]$n_valores, 150)
  # `ABC` no es un codigo ISO 4217: `ABC 4,5` no es un numero y su coma se mira
  # como delimitador. Partida da una parte de texto, y la columna no es de
  # listas.
  expect_length(.detectar_multivaluados(x), 0L)
  # Para saber si una celda con coma ya es un numero alcanza la forma
  # compatible; la descomposicion entera -unidad, moneda, evidencia de cada
  # separador- se armaba sobre las 720 celdas con coma.
  expect_identical(descomposiciones, 0L)
})

test_that("la trazabilidad de los numeros escritos como texto usa la descomposicion del perfil", {
  skip_on_cran()
  datos <- data.frame(
    imp = c(rep(c("$ 1.234,50", "$ 12,00", "$ 7,25"), 100L), "abc", NA,
            "15 kg", "U$S 3,00"),
    stringsAsFactors = FALSE
  )
  descomposiciones <- 0L
  original <- .componentes_numero_texto
  local_mocked_bindings(.componentes_numero_texto = function(...) {
    descomposiciones <<- descomposiciones + 1L
    original(...)
  })
  p <- perfilar(datos, analizar_dependencias = FALSE)
  h <- p$hallazgos
  traza <- function(tipo) {
    h$trazabilidad[[which(h$tipo_hallazgo == tipo)]]$indices_fila
  }
  expect_identical(traza("numero_como_texto"), c(1:300, 303L, 304L))
  expect_identical(traza("monedas_mixtas"), c(1:300, 304L))
  expect_identical(
    h$n_afectados[h$tipo_hallazgo %in% c("numero_como_texto", "monedas_mixtas")],
    c(302, 301)
  )
  # El perfil descompone la columna para contarlos; la deteccion de listas y la
  # trazabilidad de cada hallazgo la volvian a descomponer: cuatro veces.
  expect_identical(descomposiciones, 1L)
})

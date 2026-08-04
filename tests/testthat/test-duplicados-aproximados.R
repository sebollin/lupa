datos_pares_aproximados <- function() {
  data.frame(
    nombre = c("Juan Pérez", "Juan Peres", "Ana Silva", "Luis Díaz"),
    domicilio = c(
      "Calle 18 de Julio 123", "Calle 18 de Julio 123",
      "Rambla 1", "Camino del Prado 44"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("el resultado siempre declara el alcance", {
  resultado <- lupa:::.vacio_duplicados_aproximados(
    100L, "nombre", "jw", 0.15, 10L, 20L, 5L,
    disponible = FALSE, razon = "paquete ausente"
  )
  expect_false(resultado$disponible)
  expect_equal(resultado$alcance$n_pares_posibles, 4950)
  expect_equal(resultado$alcance$n_pares_comparados, 0)
  expect_equal(resultado$alcance$n_pares_sin_comparar, 4950)
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "No se ejecutó la comparación aproximada", fixed = TRUE)
  expect_match(html, "paquete ausente", fixed = TRUE)
})

test_that("sin stringdist la degradacion es explicita", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  resultado <- detectar_duplicados_aproximados(datos_pares_aproximados())
  expect_false(resultado$disponible)
  expect_match(resultado$razon, "stringdist", ignore.case = TRUE)
  expect_equal(nrow(resultado$pares), 0L)
})

test_that("los pares aproximados declaran distancia y no identidad", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  resultado <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"),
    max_resultados = 10L
  )
  expect_true(resultado$disponible)
  expect_equal(resultado$alcance$n_pares_posibles, 6)
  expect_equal(resultado$alcance$n_pares_comparados, 6)
  expect_equal(resultado$alcance$n_pares_hallados, 1)
  expect_equal(nrow(resultado$pares), 1L)
  expect_true(resultado$pares$distancia[[1L]] > 0)
  expect_lte(resultado$pares$distancia[[1L]], 0.15)
  expect_true(all(as.character(resultado$hallazgos$severidad) == "sospechoso"))
  expect_true(all(grepl("no demuestra identidad", resultado$hallazgos$descripcion,
                         fixed = TRUE)))
  expect_false(any(grepl("Juan|Ana|Calle|Rambla", resultado$pares$evidencia_1,
                         perl = TRUE)))
  expect_true(all(grepl("valor protegido", resultado$pares$evidencia_1,
                        fixed = TRUE)))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(resultado, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Juan|Ana|Calle|Rambla", html, perl = TRUE))
  expect_match(html, "Duplicados aproximados", fixed = TRUE)
  expect_match(html, "n_pares_comparados", fixed = TRUE)
})

test_that("el perfil integra pares aproximados sin proponer eliminacion", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE,
    duplicados_aproximados = list(
      columnas = c("nombre", "domicilio"), max_resultados = 10L
    )
  )
  expect_true(inherits(perfil$duplicados_aproximados,
                       "duplicados_aproximados"))
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "duplicados_aproximados"))
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(grepl("duplicados_aproximados", plan$hallazgo, fixed = TRUE)))
})

test_that("el alcance declara el recorte de pares", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = sprintf("persona %03d", 1:100))
  resultado <- detectar_duplicados_aproximados(
    datos, max_pares = 100L, muestra = Inf, max_resultados = 10L
  )
  expect_true(resultado$alcance$n_pares_comparados <= 100)
  expect_true(resultado$alcance$n_pares_sin_comparar > 0)
  expect_true(resultado$alcance$muestreado)
  expect_match(resultado$alcance$estrategia,
               "muestra_sistematica", fixed = TRUE)
})

test_that("el recorte sistematico conserva los extremos de la tabla", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = paste0("persona", seq_len(1000L)),
    domicilio = paste0("calle", seq_len(1000L))
  )
  datos$nombre[999:1000] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[999:1000] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = 1000L, max_pares = 10000L, max_resultados = Inf
  )
  expect_equal(resultado$alcance$n_filas_muestra, 141)
  expect_match(resultado$alcance$estrategia, "limite_de_pares", fixed = TRUE)
  expect_true(any(
    resultado$pares$fila_1 == 999L & resultado$pares$fila_2 == 1000L
  ))
  solo_muestra <- detectar_duplicados_aproximados(
    datos, muestra = 100L, max_pares = Inf, max_resultados = 1L
  )
  expect_equal(solo_muestra$alcance$estrategia,
               "muestra_sistematica_por_muestra")
  ambos_limites <- detectar_duplicados_aproximados(
    datos, muestra = 500L, max_pares = 100L, max_resultados = 1L
  )
  expect_equal(ambos_limites$alcance$estrategia,
               "muestra_sistematica_por_muestra_y_limite_de_pares")
  primeras <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = 1L, max_resultados = 1L
  )
  expect_match(primeras$alcance$estrategia, "primeras_n_filas", fixed = TRUE)
})

test_that("el valor predeterminado recorre la tabla completa por bloques", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = paste0("persona", seq_len(1000L)),
    domicilio = paste0("calle", seq_len(1000L))
  )
  datos$nombre[500:501] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[500:501] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(datos)
  expect_true(resultado$alcance$comparacion_exhaustiva)
  expect_equal(resultado$alcance$n_filas_muestra, 1000)
  expect_equal(resultado$alcance$n_pares_comparados, 499500)
  expect_equal(resultado$alcance$tamano_bloque, 1000)
  expect_equal(resultado$alcance$modo_comparacion, "exhaustiva_por_bloques")
  expect_true(any(resultado$pares$fila_1 == 500L & resultado$pares$fila_2 == 501L))
})

test_that("el recorrido por bloques conserva exactitud y acota su tesela", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz", "Marta Silva"),
    domicilio = c("Calle 1", "Calle 1", "Calle 2", "Calle 3")
  )
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf, max_resultados = Inf, bloque = 2L
  )
  expect_equal(resultado$alcance$n_pares_comparados, 6)
  expect_equal(resultado$alcance$n_pares_exactos, 1)
  expect_equal(resultado$alcance$n_bloques, 3)
  expect_equal(resultado$alcance$tamano_bloque, 2)
  expect_true(any(resultado$pares$tipo_par == "exacto"))
})

test_that("el acumulador procesa lotes y conserva sólo el límite", {
  acumulador <- lupa:::.nuevo_acumulador_duplicados(2L)
  expect_identical(
    lupa:::.acumular_pares_duplicados(acumulador, integer(), integer(), numeric()),
    acumulador
  )
  expect_identical(
    lupa:::.acumular_pares_duplicados(acumulador, 1L, 2L, Inf), acumulador
  )
  for (inicio in seq(1, 1000, by = 10)) {
    indices <- inicio:(inicio + 9)
    acumulador <- lupa:::.acumular_pares_duplicados(
      acumulador, indices, indices + 1L,
      rep(c(0.08, 0.01, 0.05, 0.03, 0.02), length.out = 10L)
    )
  }
  expect_equal(nrow(acumulador$pares), 2L)
  expect_equal(acumulador$n_hallados, 1000L)
  expect_equal(acumulador$n_exactos, 0L)
  expect_equal(acumulador$n_aproximados, 1000L)
  expect_equal(acumulador$pares$distancia, c(0.01, 0.01))
  expect_error(
    lupa:::.acumular_pares_duplicados(acumulador, 1L, c(2L, 3L), 0.1),
    "igual longitud"
  )
})

test_that("el tope predeterminado alcanza diez mil filas", {
  skip_on_cran()
  skip_if_not_installed("stringdist")
  n <- 10000L
  datos <- data.frame(
    nombre = paste0("persona", seq_len(n)),
    domicilio = paste0("calle", seq_len(n))
  )
  datos$nombre[5000:5001] <- c("Juan Perez", "Juan Peres")
  datos$domicilio[5000:5001] <- "Calle Centro"
  resultado <- detectar_duplicados_aproximados(datos)
  expect_true(resultado$alcance$comparacion_exhaustiva)
  expect_equal(resultado$alcance$n_filas_muestra, n)
  expect_equal(resultado$alcance$n_pares_comparados, 49995000)
  expect_true(any(resultado$pares$fila_1 == 5000L &
                 resultado$pares$fila_2 == 5001L))
})

test_that("el truncamiento conserva los pares de menor distancia", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("Ana Perez ", seq_len(30L)))
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf, max_resultados = 10L,
    umbral = 0.2
  )
  expect_true(resultado$alcance$truncado)
  expect_equal(nrow(resultado$pares), 10L)
  expect_true(all(diff(resultado$pares$distancia) >= 0))
})

test_that("los pares exactos en las columnas elegidas quedan visibles", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    id = c("a", "b"), nombre = c("Ana", "Ana"),
    domicilio = c("Calle 1", "Calle 1")
  )
  resultado <- detectar_duplicados_aproximados(
    datos, muestra = Inf, max_pares = Inf
  )
  expect_equal(resultado$columnas, c("nombre", "domicilio"))
  expect_equal(resultado$pares$tipo_par, "exacto")
  expect_equal(resultado$alcance$n_pares_exactos, 1)
  expect_equal(resultado$alcance$n_pares_aproximados, 0)
  expect_equal(resultado$hallazgos$tipo_hallazgo, "duplicados_exactos_columnas")
  expect_equal(sum(duplicated(datos)), 0L)
})

test_that("el reenmascarado protege hallazgos compuestos nuevos y exactos", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Perez", "Luis Diaz"),
    domicilio = c("Calle Uruguay 123", "Calle Uruguay 123", "Av Italia 900"),
    stringsAsFactors = FALSE
  )
  analisis <- analizar(
    datos, proteger_datos_personales = FALSE,
    argumentos_perfil = list(
      duplicados_aproximados = list(columnas = c("nombre", "domicilio"))
    )
  )
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  invisible(reportar(analisis, archivo = archivo))
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Ana Perez|Calle Uruguay 123", html, perl = TRUE))
  expect_match(html, "evidencia protegida", fixed = TRUE)

  compuesto <- data.frame(
    columna = "nombre, domicilio", tipo_hallazgo = "hallazgo_compuesto_nuevo",
    evidencia = "Ana Perez / Calle Uruguay 123", stringsAsFactors = FALSE
  )
  compuesto$severidad <- factor("sospechoso",
    levels = c("ok", "sospechoso", "error"), ordered = TRUE
  )
  compuesto$descripcion <- "prueba"
  compuesto$sugerencia <- "revisar"
  base <- perfilar(datos, analizar_dependencias = FALSE)
  protegido <- lupa:::.proteger_componentes_perfil(
    base$columnas, base$patrones, base$dependencias, compuesto,
    base$datos_personales
  )
  expect_equal(protegido$hallazgos$evidencia, "[evidencia protegida]")
})

test_that("la seleccion automatica evita mezclar demasiadas columnas", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(a = "x", b = "y", c = "z")
  resultado <- detectar_duplicados_aproximados(datos)
  expect_length(resultado$columnas, 0L)
  expect_match(resultado$razon, "indique `columnas`", fixed = TRUE)
})

test_that("valida entradas y tipos no comparables", {
  expect_length(lupa:::.indices_duplicados_aproximados(0, 10L), 0L)
  expect_length(lupa:::.indices_duplicados_aproximados(10, 3L), 3L)
  expect_equal(lupa:::.indices_duplicados_aproximados(3, 2L), 1:2)
  vacio <- lupa:::.vacio_duplicados_aproximados(
    2L, character(), "jw", 0.12, 2L, 1L, 1L
  )
  expect_identical(
    lupa:::.proteger_duplicados_aproximados(vacio, character()), vacio
  )
  expect_equal(
    lupa:::.evidencia_fila_aproximada(
      data.frame(a = NA_character_), "a", 1L, character()
    ),
    "a=[ausente]"
  )
  expect_error(detectar_duplicados_aproximados(1:3), "data.frame")
  datos <- data.frame(a = c("x", "y"), b = 1:2)
  expect_error(detectar_duplicados_aproximados(datos, columnas = "no"),
               "columnas")
  expect_error(detectar_duplicados_aproximados(datos, columnas = c("a", "a")),
               "columnas")
  matriz <- data.frame(a = I(matrix(1:4, nrow = 2)))
  expect_error(detectar_duplicados_aproximados(matriz, columnas = "a"),
               "matrices")
  expect_error(detectar_duplicados_aproximados(datos, metodo = "inventado"),
               "medida")
  expect_error(detectar_duplicados_aproximados(datos, umbral = -1), "finito")
  expect_error(detectar_duplicados_aproximados(datos, muestra = 1.5), "muestra")
  expect_error(detectar_duplicados_aproximados(datos, normalizar = NA), "lógicos")
  expect_error(detectar_duplicados_aproximados(datos, bloque = Inf), "bloque")
  expect_error(detectar_duplicados_aproximados(datos, bloque = 1.5), "bloque")
  tesela_vacia <- lupa:::.comparar_bloques_duplicados(
    "solo", 1L, "jw", 0.12, 1000L, 1L
  )
  expect_equal(tesela_vacia$n_bloques, 0L)
  expect_length(lupa:::.indices_duplicados_aproximados(3, 10L), 3L)
  expect_error(
    detectar_duplicados_aproximados(
      datos, perfil = perfilar(data.frame(z = 1:2), analizar_dependencias = FALSE)
    ),
    "corresponder"
  )
})

test_that("declara ausencia de columnas, filas o pares comparables", {
  skip_if_not_installed("stringdist")
  sin_texto <- detectar_duplicados_aproximados(data.frame(a = 1:3))
  expect_true(sin_texto$disponible)
  expect_match(sin_texto$razon, "texto comparables", fixed = TRUE)
  sin_filas <- detectar_duplicados_aproximados(
    data.frame(nombre = c(NA_character_, ""))
  )
  expect_match(sin_filas$razon, "dos filas", fixed = TRUE)
  sin_pares <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana", "Luis")), umbral = 0
  )
  expect_equal(nrow(sin_pares$pares), 0L)
  expect_equal(sin_pares$alcance$n_pares_hallados, 0)
  sin_normalizar <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana", "ana")), normalizar = FALSE, umbral = 0.01,
    muestra = Inf, max_pares = Inf
  )
  expect_true(sin_normalizar$disponible)
})

test_that("los bytes UTF-8 inválidos se excluyen sin abortar", {
  skip_if_not_installed("stringdist")
  invalido <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  resultado <- detectar_duplicados_aproximados(
    data.frame(nombre = c(invalido, "Ana")), muestra = Inf
  )
  expect_true(resultado$disponible)
  expect_equal(resultado$alcance$n_filas_validas, 1)
  expect_equal(nrow(resultado$pares), 0L)
})

test_that("limita resultados, protege o expone evidencia según la opción", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = paste0("Ana Perez ", seq_len(6L)))
  truncado <- detectar_duplicados_aproximados(
    datos, max_resultados = 2L, umbral = 0.2
  )
  expect_true(truncado$alcance$truncado)
  expect_equal(nrow(truncado$pares), 2L)
  archivo_truncado <- tempfile(fileext = ".html")
  on.exit(unlink(archivo_truncado), add = TRUE)
  reportar(truncado, archivo = archivo_truncado)
  expect_match(
    paste(readLines(archivo_truncado, encoding = "UTF-8"), collapse = "\n"),
    "tabla de pares mostrados está truncada", fixed = TRUE
  )
  visible <- detectar_duplicados_aproximados(
    data.frame(nombre = c("Ana Pérez", "Ana Peres")),
    proteger_datos_personales = FALSE, umbral = 0.2
  )
  expect_true(any(grepl("Ana", visible$pares$evidencia_1, fixed = TRUE)))
  archivo_protegido <- tempfile(fileext = ".html")
  on.exit(unlink(archivo_protegido), add = TRUE)
  reportar(visible, archivo = archivo_protegido)
  html_protegido <- paste(
    readLines(archivo_protegido, encoding = "UTF-8"), collapse = "\n"
  )
  expect_false(grepl("Ana", html_protegido, fixed = TRUE))
  con_perfil <- perfilar(
    datos_pares_aproximados(), analizar_dependencias = FALSE
  )
  reutilizado <- detectar_duplicados_aproximados(
    datos_pares_aproximados(), perfil = con_perfil
  )
  expect_true(reutilizado$disponible)
})

test_that("la protección del reporte también cubre análisis con evidencia cruda", {
  skip_if_not_installed("stringdist")
  datos <- datos_pares_aproximados()
  analisis <- analizar(
    datos,
    proteger_datos_personales = FALSE,
    argumentos_perfil = list(
      analizar_dependencias = FALSE,
      duplicados_aproximados = list(columnas = c("nombre", "domicilio"))
    )
  )
  expect_true(any(grepl("Juan", analisis$perfil$duplicados_aproximados$pares$evidencia_1,
                     fixed = TRUE)))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(analisis, archivo = archivo)
  html <- paste(readLines(archivo, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("Juan|Calle", html, perl = TRUE))
  expect_match(html, "valores personales protegidos", fixed = TRUE)
})

test_that("perfilar valida la configuración aproximada", {
  datos <- data.frame(nombre = c("Ana", "Ana"))
  expect_error(perfilar(datos, analizar_dependencias = NA), "analizar_dependencias")
  expect_error(perfilar(datos, datos_personales_permitidos = NA), "TRUE o FALSE")
  expect_error(perfilar(datos, muestra_validadores = 0.5), "muestra_validadores")
  expect_error(perfilar(datos, duplicados_aproximados = 1),
               "FALSE, TRUE o una lista")
  expect_error(
    perfilar(datos, duplicados_aproximados = list(proteger_datos_personales = FALSE)),
    "argumentos coordinados"
  )
  if (requireNamespace("stringdist", quietly = TRUE)) {
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE,
      duplicados_aproximados = TRUE
    )
    expect_true(inherits(perfil$duplicados_aproximados,
                         "duplicados_aproximados"))
    archivo <- tempfile(fileext = ".html")
    on.exit(unlink(archivo), add = TRUE)
    reportar(perfil, archivo = archivo)
  }
})

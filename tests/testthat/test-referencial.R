test_that("un referencial declara clave, valores y alcance completo", {
  datos <- data.frame(id = 1:3, nombre = c("A", "B", "C"))
  ref <- referencial(
    datos, "id", "nombre", completo = TRUE, alcance = "universo de prueba",
    nombre = "padrón"
  )

  expect_s3_class(ref, "referencial")
  expect_equal(ref$clave, "id")
  expect_equal(ref$valor, "nombre")
  expect_true(ref$completo)
  expect_output(print(ref), "padrón")
  expect_error(referencial(datos, "id", completo = TRUE), "alcance")
  expect_error(referencial(rbind(datos, datos[1, ]), "id"), "unívocamente")
  expect_error(referencial(transform(datos, id = c(1, NA, 3)), "id"), "ausentes")
  expect_error(referencial(datos, "id", "id"), "compartir")
  expect_error(referencial(datos, "no_existe"), "No se encontraron")
  expect_error(referencial(1:3, "id"), "data.frame")
  nombres_malos <- datos
  names(nombres_malos) <- c("id", "id")
  expect_error(referencial(nombres_malos, "id"), "nombres de columna")
  expect_error(referencial(datos, c("id", "id")), "únicos")
  expect_error(referencial(datos, "id", completo = NA), "lógico escalar")
  expect_error(referencial(datos, "id", alcance = 1), "cadena")
  expect_error(referencial(datos, "id", nombre = ""), "cadena")
  lista <- data.frame(id = 1:2)
  lista$valor <- I(list(1, 2))
  expect_error(referencial(lista, "id", "valor"), "atómicos")
})

test_that("correctitud fuerte y débil conservan semánticas distintas", {
  ref <- referencial(
    data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla")),
    "id", "nombre", completo = TRUE, alcance = "padrón"
  )
  m <- metricas_referencial()
  fuerte <- instanciar(
    especializar(m$CorrectitudSemFuerte), "personas", "id", referencial = ref
  )
  debil <- instanciar(
    especializar(m$CorrectitudSemDebil), "personas", c("id", "nombre"),
    referencial = ref
  )
  datos <- data.frame(
    id = c(1, 2, 4, NA), nombre = c("Ana", "X", "Carla", "Ana")
  )
  medidas <- medir(modelo(fuerte, debil), datos, id_medicion = "r1")

  expect_equal(
    medidas$resultado[medidas$metrica == "CorrectitudSemFuerte"],
    c(1, 1, 0)
  )
  expect_equal(
    medidas$resultado[medidas$metrica == "CorrectitudSemDebil"],
    c(1, 0, 0)
  )
  ratio <- agregar(
    medidas[medidas$metrica == "CorrectitudSemFuerte", ],
    "atributo", "ratio"
  )
  expect_equal(ratio$resultado, 2 / 3)

  sin_valores <- referencial(data.frame(id = 1:2), "id")
  debil_invalida <- instanciar(
    especializar(m$CorrectitudSemDebil), "personas", c("id", "nombre"),
    referencial = sin_valores
  )
  expect_error(medir(modelo(debil_invalida), datos), "valores asociados")
  enlace_malo <- instanciar(
    especializar(m$CorrectitudSemFuerte), "personas", c("id", "nombre"),
    referencial = ref
  )
  expect_error(medir(modelo(enlace_malo), datos), "requiere 1")
  fuerte_faltante <- instanciar(
    especializar(m$CorrectitudSemFuerte), "personas", "ausente",
    referencial = ref
  )
  expect_error(medir(modelo(fuerte_faltante), datos), "No se encontraron")
  debil_faltante <- instanciar(
    especializar(m$CorrectitudSemDebil), "personas", c("id", "ausente"),
    referencial = ref
  )
  expect_error(medir(modelo(debil_faltante), datos), "No se encontraron")
})

test_that("la cobertura exige completitud y no se infla con duplicados", {
  tabla <- data.frame(id = c(1, 1, 3, 8))
  completo <- referencial(
    data.frame(id = 1:4), "id", completo = TRUE, alcance = "cuatro personas"
  )
  parcial <- referencial(data.frame(id = 1:4), "id")
  metrica <- metricas_referencial()$RatioCobertura
  instancia <- instanciar(
    especializar(metrica), "personas", "id", referencial = completo
  )
  expect_equal(medir(modelo(instancia), tabla)$resultado, 0.5)

  instancia_parcial <- instanciar(
    especializar(metrica), "personas", "id", referencial = parcial
  )
  expect_error(medir(modelo(instancia_parcial), tabla), "declarado completo")
  expect_error(
    medir(modelo(instanciar(especializar(metrica), "personas", "id")), tabla),
    "referencial"
  )
  vacio <- referencial(
    data.frame(id = integer()), "id", completo = TRUE, alcance = "universo vacío"
  )
  cobertura_vacia <- instanciar(
    especializar(metrica), "personas", "id", referencial = vacio
  )
  expect_equal(medir(modelo(cobertura_vacia), data.frame(id = integer()))$resultado, 1)
  cobertura_faltante <- instanciar(
    especializar(metrica), "personas", "ausente", referencial = completo
  )
  expect_error(medir(modelo(cobertura_faltante), tabla), "No se encontraron")
})

test_that("los auxiliares referenciales validan dimensiones y tablas vacías", {
  expect_equal(lupa:::.codigos_filas(data.frame(row.names = 1:2)), c(1L, 1L))
  expect_error(
    lupa:::.filas_en_referencial(data.frame(a = 1), data.frame(a = 1, b = 2)),
    "misma cantidad"
  )
  expect_false(lupa:::.filas_en_referencial(
    data.frame(a = 1), data.frame(a = integer())
  ))
  if (requireNamespace("data.table", quietly = TRUE)) {
    ref <- referencial(data.table::data.table(id = 1:2), "id")
    expect_s3_class(ref$datos, "data.frame")
    expect_false(inherits(ref$datos, "data.table"))
  }
})

test_that("EntidadDuplicada admite clave con iguales o ausentes", {
  datos <- data.frame(
    id = c(1, 1, 2, 2, 3),
    nombre = c("Ana", NA, "B", "C", "D"),
    edad = c(20, 20, 30, 30, 40)
  )
  metrica <- especializar(metricas_nucleo()$EntidadDuplicada)
  por_clave <- instanciar(metrica, "personas", "id")
  exacta <- instanciar(metrica, "personas", character(),
                      nombre_instancia = "exacta")

  expect_equal(medir(modelo(por_clave), datos)$resultado,
               c(1, 1, 0, 0, 0))
  expect_true(all(medir(modelo(exacta), datos)$resultado == 0))
})

test_that("los referenciales heredan normalizacion y no inventan presencias", {
  ref <- referencial(
    data.frame(departamento = c("Montevideo", "Canelones", "Río Negro")),
    "departamento"
  )
  metrica <- metricas_referencial()$CorrectitudSemFuerte
  datos <- data.frame(departamento = c(
    "MONTEVIDEO", "montevideo", "Canelones", "Rio Negro", "Montevido", "Rocha"
  ))
  heredada <- instanciar(especializar(metrica), "padron", "departamento",
                         referencial = ref)
  sin_normalizar <- instanciar(
    especializar(metrica, normalizar = FALSE, proximidad = FALSE),
    "padron", "departamento", referencial = ref
  )
  resultado <- medir(modelo(heredada), datos)
  resultado_crudo <- medir(modelo(sin_normalizar), datos)
  expect_equal(resultado$resultado, c(1, 1, 1, 1, 0, 0))
  expect_equal(resultado_crudo$resultado, c(0, 0, 1, 0, 0, 0))
  expect_false(any(resultado$resultado[5:6] == 1))
  evidencia <- resultado$objeto_medible[5]
  if (requireNamespace("stringdist", quietly = TRUE)) {
    expect_match(evidencia, "Montevideo", fixed = TRUE)
    expect_match(evidencia, "0.0200", fixed = TRUE)
  }
})

test_that("la proximidad agrega evidencia pero no cambia el veredicto", {
  skip_if_not_installed("stringdist")
  ref <- referencial(
    data.frame(departamento = c("Montevideo", "Canelones", "Río Negro")),
    "departamento"
  )
  metrica <- metricas_referencial()$CorrectitudSemFuerte
  datos <- data.frame(departamento = c("Montevido", "Rocha", "Montevideo"))
  con <- instanciar(especializar(metrica, proximidad = TRUE), "x", "departamento",
                    referencial = ref)
  sin <- instanciar(especializar(metrica, proximidad = FALSE), "x", "departamento",
                    referencial = ref)
  medida_con <- medir(modelo(con), datos)
  medida_sin <- medir(modelo(sin), datos)
  expect_identical(medida_con$resultado, medida_sin$resultado)
  expect_match(medida_con$objeto_medible[[1L]], "Montevideo", fixed = TRUE)
  expect_match(medida_con$objeto_medible[[1L]], "distancia=0.0200", fixed = TRUE)
  expect_false(grepl("candidato_referencial", medida_con$objeto_medible[[2L]],
                     fixed = TRUE))
})

test_that("la cobertura hereda normalizacion pero excluye proximidad", {
  ref <- referencial(
    data.frame(departamento = c("Montevideo", "Canelones", "Río Negro")),
    "departamento", completo = TRUE, alcance = "departamentos"
  )
  metrica <- metricas_referencial()$RatioCobertura
  instancia <- instanciar(especializar(metrica), "x", "departamento",
                          referencial = ref)
  medida <- medir(modelo(instancia), data.frame(
    departamento = c("MONTEVIDEO", "Rio Negro", "Rocha")
  ))
  expect_equal(medida$resultado, 2 / 3)
  expect_false(medida$resultado > 2 / 3)
  alcance <- attr(medida, "alcance_metricas")[[1L]]
  expect_false(alcance$proximidad$solicitada)
  expect_equal(alcance$n_referencial, 3)
})

test_that("sin stringdist el camino exacto referencial sigue funcionando", {
  local_mocked_bindings(
    .stringdist_disponible = function() FALSE,
    .package = "lupa"
  )
  ref <- referencial(data.frame(departamento = "Montevideo"), "departamento")
  metrica <- especializar(metricas_referencial()$CorrectitudSemFuerte)
  medida <- medir(
    modelo(instanciar(metrica, "x", "departamento", referencial = ref)),
    data.frame(departamento = c("MONTEVIDEO", "Montevido"))
  )
  expect_equal(medida$resultado, c(1, 0))
  alcance <- attr(medida, "alcance_metricas")[[1L]]$proximidad
  expect_false(alcance$disponible)
  expect_match(alcance$motivo, "stringdist", ignore.case = TRUE)
})

test_that("el limite de proximidad queda declarado cuando recorta", {
  skip_if_not_installed("stringdist")
  ref <- referencial(data.frame(valor = c("Montevideo", "Canelones", "Artigas")),
                     "valor")
  metrica <- especializar(
    metricas_referencial()$CorrectitudSemFuerte,
    max_pares = 3L
  )
  medida <- medir(
    modelo(instanciar(metrica, "x", "valor", referencial = ref)),
    data.frame(valor = c("Montevido", "Canelone", "Rocha"))
  )
  proximidad <- attr(medida, "alcance_metricas")[[1L]]$proximidad
  expect_true(proximidad$truncado)
  expect_equal(proximidad$n_fallos_comparados, 1L)
  expect_equal(proximidad$n_pares_sin_comparar, 6)
})

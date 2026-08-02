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

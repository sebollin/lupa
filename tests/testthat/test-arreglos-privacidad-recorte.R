test_that("una columna con forma de documento sin verificar se protege igual", {
  set.seed(23)
  n <- 300
  # Documentos con digito verificador invalido: el caso de una base sucia, que
  # es la poblacion para la que existe el paquete. El nombre de la columna es
  # neutro a proposito, para entrar por la rama de la forma y no la del nombre.
  ced <- sprintf(
    "%d.%03d.%03d-%d", sample(1:5, n, TRUE), sample(0:999, n, TRUE),
    sample(0:999, n, TRUE), sample(0:9, n, TRUE)
  )
  valores <- c(ced, paste0(ced[1:30], " "))
  datos <- data.frame(campo_17 = valores, stringsAsFactors = FALSE)

  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = TRUE
  )

  clasificacion <- perfil$datos_personales
  expect_equal(clasificacion$tipo, "documento_identidad")
  expect_equal(clasificacion$poder_discriminante, "debil")
  expect_true(clasificacion$proteger)

  # Ningun valor real de la columna puede sobrevivir en la evidencia.
  crudos <- unique(trimws(valores))
  aparece <- vapply(
    crudos,
    function(v) any(grepl(v, perfil$hallazgos$evidencia, fixed = TRUE)),
    logical(1L)
  )
  expect_equal(sum(aparece), 0L)

  personal <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "dato_personal_posible",
  ]
  expect_match(personal$evidencia, "proteccion automatica: si")
  expect_match(personal$evidencia, "por precaucion")
})

test_that("una forma verificada sigue declarandose verificada", {
  datos <- data.frame(
    documento = c("1.234.567-2", "4.098.765-4", "3.111.222-6"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = TRUE
  )
  expect_true(perfil$datos_personales$proteger)
  expect_false(identical(perfil$datos_personales$poder_discriminante, "debil"))
})

test_that("el recorte de dependencias se declara en cobertura_diagnosticos", {
  set.seed(2)
  columnas <- lapply(seq_len(120), function(i) sample(letters[1:5], 200, TRUE))
  names(columnas) <- sprintf("v%03d", seq_len(120))
  datos <- as.data.frame(columnas, stringsAsFactors = FALSE)

  perfil <- perfilar(datos, max_columnas_dependencias = 100L)

  expect_true(attr(perfil$dependencias, "truncado"))
  expect_equal(attr(perfil$dependencias, "max_columnas"), 100L)
  expect_length(attr(perfil$dependencias, "columnas_omitidas"), 20L)

  cobertura <- perfil$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "dependencias_funcionales", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "100 columnas")
  expect_match(fila$motivo, "20 fuera del")
  expect_match(fila$como_resolverlo, "max_columnas_dependencias")
  expect_match(fila$como_resolverlo, "por posici")
})

test_that("sin recorte no se declara nada", {
  datos <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, max_columnas_dependencias = 100L)
  expect_false(isTRUE(attr(perfil$dependencias, "truncado")))
  cobertura <- perfil$cobertura_diagnosticos
  expect_equal(sum(cobertura$diagnostico == "dependencias_funcionales"), 0L)
})

test_that("seleccionar columnas para dependencias no reinyecta la geometria", {
  skip_if_not_installed("sf")
  ## Sobre un objeto `sf`, `datos[, seleccion]` vuelve a pegar la columna
  ## geometrica que la seleccion habia excluido. Eso traia al analisis una
  ## columna que no corresponde y dejaba los indices desalineados con los
  ## nombres.
  datos <- sf::st_sf(
    a = c(1, 1, 2, 2, 3, 3),
    b = c("x", "x", "y", "y", "z", "z"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      sf::st_point(c(3, 3)), sf::st_point(c(4, 4)), sf::st_point(c(5, 5))
    )
  )
  dependencias <- detectar_dependencias(datos)
  analizadas <- attr(dependencias, "columnas_analizadas")
  expect_false("geometry" %in% analizadas)
  expect_false("geometry" %in% dependencias$determinante)
  expect_false("geometry" %in% dependencias$dependiente)
})

test_that("el lexico cubre los nombres frecuentes de columnas con personas", {
  ## Una refutacion adversarial mostro que `persona`, `cliente`, `paciente` y
  ## `lugar_residencia` no se clasificaban, y sus valores quedaban visibles en
  ## la moda. El lexico no puede ser completo, pero si cubrir lo frecuente.
  for (nombre in c("persona", "cliente", "paciente", "socio", "beneficiario",
                   "titular", "lugar_residencia")) {
    datos <- data.frame(
      x = c("Juan Perez", "Ana Gomez", "Luis Diaz"), stringsAsFactors = FALSE
    )
    names(datos) <- nombre
    perfil <- perfilar(
      datos, analizar_dependencias = FALSE, proteger_datos_personales = TRUE
    )
    expect_gt(nrow(perfil$datos_personales), 0L)
    expect_false(identical(perfil$columnas$moda, "Juan Perez"))
  }
})

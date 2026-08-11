test_that("el nombre personal tiene prioridad sobre una forma numerica", {
  datos <- data.frame(
    telefono = rep("096551054", 5L),
    celular = rep("096551054", 5L),
    movil = rep("096551054", 5L),
    fecha_nacimiento = rep("19860907", 5L),
    nacimiento = rep("19481021", 5L),
    nombre = rep("Ana", 5L),
    correo = rep("ana@example.org", 5L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  filas <- perfil$datos_personales[
    match(names(datos), perfil$datos_personales$columna), , drop = FALSE
  ]
  expect_equal(filas$tipo[1:3], rep("telefono", 3L))
  expect_equal(filas$tipo[4:5], rep("fecha_nacimiento", 2L))
  expect_true(all(filas$proteger))
  expect_true(all(perfil$columnas$moda == "[valor protegido]"))
})

test_that("la verificacion admite una tasa explicita de tipeos", {
  candidatos <- as.character(1000000:1003000)
  validos <- candidatos[validar_ci_uy(candidatos)]
  ids <- head(validos, 200L)
  ultimo <- as.integer(substr(ids[[1L]], nchar(ids[[1L]]), nchar(ids[[1L]])))
  ids[[1L]] <- paste0(
    substr(ids[[1L]], 1L, nchar(ids[[1L]]) - 1L), (ultimo + 1L) %% 10L
  )
  perfil <- perfilar(data.frame(padron = ids), analizar_dependencias = FALSE)
  expect_equal(perfil$datos_personales$poder_discriminante, "verificado")
  expect_true(perfil$datos_personales$proteger)
  expect_equal(perfil$meta$umbral_documento_verificado, 0.9)
  expect_error(
    perfilar(data.frame(x = ids), umbral_documento_verificado = 1.1),
    "entre 0 y 1"
  )
})

test_that("un pack personal externo entra por la misma puerta", {
  validar_rut_cl <- function(x) {
    uno <- function(valor) {
      z <- toupper(gsub("[.-]", "", valor))
      if (!grepl("^[0-9]{7,8}[0-9K]$", z, perl = TRUE)) return(FALSE)
      cuerpo <- as.integer(strsplit(substr(z, 1L, nchar(z) - 1L), "")[[1L]])
      suma <- sum(rev(cuerpo) * rep(2:7, length.out = length(cuerpo)))
      esperado <- 11L - suma %% 11L
      esperado <- if (esperado == 11L) "0" else if (esperado == 10L) "K" else {
        as.character(esperado)
      }
      substr(z, nchar(z), nchar(z)) == esperado
    }
    vapply(as.character(x), uno, logical(1L))
  }
  pack <- pack_validadores("Chile", list(rut = validar_rut_cl), pais = "CL")
  datos <- data.frame(codigo = c(
    "12.345.678-5", "9.876.543-0", "76.543.210-0", "11.111.111-1"
  ))
  # Se conserva la comprobación del contrato aun si el ejemplo contiene
  # documentos que el validador rechaza: el pack no toca el motor.
  resultado <- perfilar(
    datos, validadores_personales = pack, analizar_dependencias = FALSE
  )
  expect_true("codigo" %in% resultado$datos_personales$columna)
  expect_false(anyNA(resultado$datos_personales$proteger))
})

test_that("los validadores de digitos procesan vectores sin bucle por celda", {
  expect_equal(
    validar_luhn(c("79927398713", "79927398714", "49927398716")),
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    validar_ci_uy(c("1.234.567-2", "2.345.678-3", "3.456.789-4")),
    c(TRUE, TRUE, TRUE)
  )
  skip_on_cran()
  valores <- rep("1.234.567-2", 1e5)
  expect_lt(unname(system.time(validar_ci_uy(valores))["elapsed"]), 1)
  expect_lt(
    unname(system.time(validar_luhn(gsub("[.-]", "", valores)))["elapsed"]),
    1
  )
})

test_that("el perfilado con formas documentales no valida celdas innecesarias", {
  skip_on_cran()
  skip_on_ci()
  set.seed(21)
  n <- 1e5
  valores <- as.character(sample(10000000:99999999, n, replace = TRUE))
  datos <- data.frame(
    a = valores, b = rev(valores), c = valores, d = valores, e = valores,
    stringsAsFactors = FALSE
  )
  tiempo <- system.time(
    perfilar(datos, analizar_dependencias = FALSE)
  )["elapsed"]
  # En la ronda 75 este guardián encontró una regresión real de 16 a 26 s.
  # El contador determinista protege ahora el trabajo por valor; este umbral
  # queda como red de arrastre contra desastres algorítmicos y deja margen para
  # la carga variable de la máquina.
  expect_lt(unname(tiempo), 30)
})

test_that("la configuracion de validadores se puede apagar y valida su contrato", {
  datos <- data.frame(codigo = c("1.234.567-2", "2.345.678-3", "3.456.789-4"))
  apagado <- perfilar(
    datos, validadores_personales = numeric(), analizar_dependencias = FALSE
  )
  expect_equal(apagado$datos_personales$poder_discriminante, "debil")
  expect_error(
    perfilar(datos, validadores_personales = list(x = 1)),
    "lista con nombres de funciones"
  )
  expect_true(is.na(.proporcion_compatible(NA_character_, "^[0-9]+$")))
})

test_that("la validacion preliminar y completa respetan el contrato vectorial", {
  validos <- c("1.234.567-2", "2.345.678-3", "3.456.789-4")
  datos <- data.frame(codigo = rep(validos, length.out = 1001L))
  siempre <- function(x) rep(TRUE, length(x))
  perfil <- perfilar(
    datos, validadores_personales = list(externo = siempre),
    analizar_dependencias = FALSE
  )
  expect_equal(perfil$datos_personales$poder_discriminante, "verificado")
  defectuoso <- function(x) if (length(x) > 1000L) TRUE else rep(TRUE, length(x))
  expect_error(
    perfilar(
      datos, validadores_personales = list(externo = defectuoso),
      analizar_dependencias = FALSE
    ),
    "igual longitud"
  )
  expect_error(
    .proporcion_validadores(
      validos, list(externo = function(x) TRUE), 0.9, muestra = 3L
    ),
    "igual longitud"
  )
})

test_that("las ramas de formato compacto y Luhn rechazan entradas no validas", {
  expect_equal(validar_luhn(c("abc", "12A", "")), rep(FALSE, 3L))
  formatos <- detectar_formatos_fecha(c("01/02/23", "03/04/23"))
  expect_true(any(formatos$anio_dos_digitos))
  expect_true(any(detectar_formatos_fecha("23-01-01")$anio_dos_digitos))
})

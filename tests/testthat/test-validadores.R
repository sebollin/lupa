test_that("los validadores ISO usan listas locales y conservan ausentes", {
  expect_equal(
    validar_iso3166(c("UY", "cl", "ZZ", NA)),
    c(TRUE, TRUE, FALSE, NA)
  )
  expect_equal(
    validar_iso3166(c("URY", "chl", "ZZZ"), "alpha3"),
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    validar_iso3166(c("858", 152, 4, 999, NA), "numerico"),
    c(TRUE, TRUE, TRUE, FALSE, NA)
  )
  expect_equal(
    validar_iso4217(c("UYU", "clp", "EUR", "ZZZ", NA)),
    c(TRUE, TRUE, TRUE, FALSE, NA)
  )
  expect_error(validar_iso3166("UY", "otro"), "arg")
})

test_that("el validador de correo declara un subconjunto sintactico", {
  valores <- c(
    "persona@example.org", "nombre.apellido+tag@sub.example.com",
    "sin-arroba", ".inicio@example.org", "doble..punto@example.org",
    "persona@-example.org", "persona@example", NA
  )
  expect_equal(
    validar_correo(valores),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
  )
  largo <- paste0(strrep("a", 65), "@example.org")
  expect_false(validar_correo(largo))
  expect_error(validar_correo(list("a@example.org")), "atomico")
})

test_that("Luhn y MOD 97 no convierten enteros largos a double", {
  expect_equal(
    validar_luhn(c("79927398713", "79927398714", "12A", "", NA)),
    c(TRUE, FALSE, FALSE, FALSE, NA)
  )
  expect_equal(
    validar_mod97(c(
      "9999123456789012141490", "9999123456789012141491",
      "08686001256515001121751", "ABC81", "ABC?", "", NA
    )),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, NA)
  )
})

test_that("los validadores uruguayos distinguen digitos validos", {
  expect_equal(
    validar_ci_uy(c(
      "1.234.567-2", "1.234.567-3", "123456-1", "123", "abc", NA
    )),
    c(TRUE, FALSE, TRUE, FALSE, FALSE, NA)
  )
  expect_equal(
    validar_rut_uy(c(
      "21 100 342 0017", "211406340011", "21 030 367 0014",
      "231003420017", "210000000017", "211003429997", "abc", NA
    )),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, NA)
  )
})

test_that("los packs se conectan a Formato y son extensibles", {
  internacionales <- validadores_internacionales()
  uruguay <- validadores_uruguay()
  expect_s3_class(internacionales, "pack_validadores")
  expect_equal(attr(uruguay, "pais"), "UY")
  expect_true(uruguay$cedula("1.234.567-2"))
  expect_output(print(uruguay), "cedula, rut")

  formato <- especializar(
    metricas_nucleo()$Formato, "CorreoSintactico",
    validador = internacionales$correo
  )
  medida <- medir(
    modelo(formato("datos", "correo")),
    data.frame(correo = c("a@example.org", "incorrecto", NA))
  )
  expect_equal(medida$resultado, c(1, 0))

  validar_rut_cl <- function(x) {
    uno <- function(valor) {
      z <- toupper(gsub("[.-]", "", valor))
      if (!grepl("^[0-9]{7,8}[0-9K]$", z)) return(FALSE)
      cuerpo <- as.integer(strsplit(substr(z, 1L, nchar(z) - 1L), "")[[1L]])
      suma <- sum(rev(cuerpo) * rep(2:7, length.out = length(cuerpo)))
      esperado <- 11L - suma %% 11L
      esperado <- if (esperado == 11L) "0" else if (esperado == 10L) "K" else {
        as.character(esperado)
      }
      identical(substr(z, nchar(z), nchar(z)), esperado)
    }
    ifelse(is.na(x), NA, vapply(as.character(x), uno, logical(1L)))
  }
  chile <- pack_validadores(
    "Chile", list(rut = validar_rut_cl), pais = "CL",
    descripcion = "Pack del proyecto consumidor."
  )
  expect_equal(chile$rut(c("12.345.678-5", "12.345.678-4", NA)),
               c(TRUE, FALSE, NA))

  expect_error(pack_validadores("", list(x = identity)), "nombre")
  expect_error(pack_validadores("x", list(identity)), "nombres unicos")
  expect_error(pack_validadores("x", list(x = 1)), "funciones")
  expect_error(pack_validadores("x", list(x = identity), pais = "ZZ"), "ISO")
  expect_error(
    pack_validadores("x", list(x = identity), descripcion = ""), "descripcion"
  )
})

test_that("la sospecha numerica debil no suprime estadisticos", {
  base <- 10000000:10000199
  datos <- data.frame(
    importe_pesos = base,
    n_factura = rev(base),
    codigo_producto = base + 2000L,
    id_transaccion = base + 4000L,
    valor_inmueble = base + 6000L,
    poblacion_localidad = seq(21885L, length.out = 200L),
    salario_mensual = seq(25081L, length.out = 200L),
    superficie_m2 = rep(56:255, length.out = 200L)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  debiles <- c(
    "importe_pesos", "n_factura", "codigo_producto", "id_transaccion",
    "valor_inmueble"
  )
  filas <- match(debiles, perfil$columnas$columna)
  clasificacion <- perfil$datos_personales[
    match(debiles, perfil$datos_personales$columna), , drop = FALSE
  ]

  expect_true(all(is.finite(perfil$columnas$minimo[filas])))
  expect_true(all(is.finite(perfil$columnas$maximo[filas])))
  expect_true(all(clasificacion$poder_discriminante == "debil"))
  expect_false(any(clasificacion$proteger))
  expect_false(any(perfil$columnas$dato_personal_protegido[filas]))
  expect_true(all(is.na(perfil$columnas$detalle_proteccion_personal[filas])))
  expect_false(any(c(
    "poblacion_localidad", "salario_mensual", "superficie_m2"
  ) %in% perfil$datos_personales$columna))
})

test_that("nombre correo y documentos con evidencia fuerte siguen protegidos", {
  datos <- data.frame(
    cedula = c("1.234.567-2", "2.345.678-3", "3.456.789-4"),
    documento = c(12345672, 23456783, 34567894),
    nombre = c("Ana", "Beto", "Cora"),
    contacto = c("ana@example.org", "beto@example.org", "cora@example.org"),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(all(perfil$datos_personales$proteger))
  expect_setequal(
    perfil$datos_personales$poder_discriminante,
    c("verificado", "alto", "medio")
  )
  expect_true(all(perfil$columnas$moda == "[valor protegido]"))

  sin_nombre <- perfilar(
    data.frame(codigo = c("1.234.567-2", "2.345.678-3", "3.456.789-4")),
    analizar_dependencias = FALSE
  )
  expect_equal(sin_nombre$datos_personales$poder_discriminante, "verificado")
  expect_true(sin_nombre$datos_personales$proteger)
  expect_true(sin_nombre$columnas$dato_personal_protegido)

  ## Decision cambiada el 2026-08-20, por Sebastian, a favor de la coherencia.
  ## Antes una columna con un solo documento repetido no se protegia, con el
  ## argumento de que no discrimina a nadie DENTRO de la tabla. Es cierto, y
  ## tambien es cierto que identifica a una persona fuera de ella. La evidencia
  ## sigue siendo debil —eso no cambio— pero la debilidad ya no decide por el
  ## lado de mostrar el documento.
  repetido <- perfilar(
    data.frame(codigo = rep("1.234.567-2", 3L)),
    analizar_dependencias = FALSE
  )
  expect_equal(repetido$datos_personales$poder_discriminante, "debil")
  expect_true(repetido$datos_personales$proteger)
  expect_equal(repetido$columnas$moda, "[valor protegido]")

  ## El hallazgo que nombra la columna sobrevive: para saber que es constante no
  ## hace falta ver el valor.
  constante <- repetido$hallazgos[
    repetido$hallazgos$tipo_hallazgo == "constante", , drop = FALSE
  ]
  expect_equal(nrow(constante), 1L)
  expect_equal(constante$n_afectados, 3)

  ## Y el contraste que no debe moverse: digitos pelados sin separadores son
  ## importes, facturas o identificadores de transaccion, y conservan sus
  ## estadisticos. Protegerlos por la sola coincidencia de largo le sacaria el
  ## resumen cuantitativo a media tabla.
  base <- 41000000L
  importes <- perfilar(
    data.frame(
      importe_pesos = base + seq_len(200L),
      n_factura = base + 2000L + seq_len(200L)
    ),
    analizar_dependencias = FALSE
  )
  expect_false(any(importes$datos_personales$proteger))
  expect_true(all(is.finite(importes$columnas$minimo)))

  esquema_anterior <- sin_nombre$datos_personales
  esquema_anterior$proteger <- NULL
  expect_equal(
    lupa:::.columnas_personales_protegidas(esquema_anterior), "codigo"
  )
})

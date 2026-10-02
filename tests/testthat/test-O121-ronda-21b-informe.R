# Ronda 21-B: el informe HTML de `reportar()` -lo que publicaba en claro, lo que
# redondeaba y lo que lo hacia abortar-.

.html_O121 <- function(...) {
  archivo <- tempfile("O121-", fileext = ".html")
  on.exit(unlink(archivo))
  suppressWarnings(suppressMessages(reportar(
    ..., archivo = archivo, sobrescribir = TRUE,
    fecha = as.POSIXct("2026-01-01", tz = "UTC")
  )))
  paste(readLines(archivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

.veces_O121 <- function(html, aguja) {
  lengths(regmatches(html, gregexpr(aguja, html, fixed = TRUE)))
}

.datos_O121 <- function() {
  documentos <- c("48123456", "51987654", "12345678", "34567890", "29876543")
  correos <- c("juan.perez@x.uy", "ana.lopez@y.uy", "pedro.gomez@z.uy",
               "lucia.diaz@w.uy", "marta.suarez@v.uy")
  n <- 40L
  data.frame(
    cedula = rep(c(documentos, "S/D", "4812345 6"), length.out = n),
    correo = rep(c(correos, "juan.perez@x.uy ", "JUAN.PEREZ@X.UY"), length.out = n),
    monto = c(rep(c(100, 200, 100, 300, 100), length.out = n - 2L), -9, 999),
    stringsAsFactors = FALSE
  )
}

.medicion_O121 <- function(proteger, id = "m1") {
  referencia <- referencial(
    data.frame(legajo = c("LEG-0001", "LEG-0002", "LEG-0003"),
               stringsAsFactors = FALSE),
    "legajo"
  )
  datos <- data.frame(legajo = c("LEG-0001", "LEG-000X", "ZZZ-9999"),
                      stringsAsFactors = FALSE)
  instancia <- instanciar(
    especializar(metricas_referencial()$CorrectitudSemFuerte),
    "t", "legajo", referencial = referencia
  )
  medir(modelo(instancia), datos, columnas_personales = "legajo",
        proteger_datos_personales = proteger,
        fecha = as.POSIXct("2026-01-01", tz = "UTC"), id_medicion = id)
}

test_that("un plan o una deriva sin proteccion no se publican en un informe protegido", {
  datos <- .datos_O121()
  perfil <- suppressWarnings(perfilar(datos, proteger_datos_personales = FALSE))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  otros <- datos
  otros$correo[1:10] <- "otro.nombre@q.uy"
  otros$cedula[1:12] <- "48123456"
  deriva <- comparar_perfiles(
    perfil, suppressWarnings(perfilar(otros, proteger_datos_personales = FALSE))
  )
  # Que los objetos traen el valor en claro: si no, la prueba no mide nada.
  expect_true(any(grepl("juan.perez", plan$evidencia, fixed = TRUE)))
  expect_true(any(grepl("51987654", deriva$valor_actual, fixed = TRUE)))

  html <- .html_O121(perfil, plan, deriva)
  # Antes: 14 apariciones del correo en el plan y el rango de la cedula en la
  # deriva; el perfil del mismo archivo, tapado.
  expect_identical(.veces_O121(html, "juan.perez"), 0L)
  expect_identical(.veces_O121(html, "51987654"), 0L)
  expect_identical(.veces_O121(html, "Secci\u00f3n omitida"), 2L)

  # Pedido sin proteccion tambien en el informe, se publica.
  abierto <- .html_O121(plan, proteger_datos_personales = FALSE)
  expect_gt(.veces_O121(abierto, "juan.perez"), 0L)
})

test_that("una medicion sin proteccion no se publica en un informe protegido", {
  # El legajo llega a `objeto_medible` como candidato del referencial, que se
  # calcula con `stringdist`.
  skip_if_not_installed("stringdist")
  medicion <- .medicion_O121(FALSE)
  expect_true(any(grepl("LEG-0001", medicion$objeto_medible, fixed = TRUE)))
  html <- .html_O121(medicion)
  # Antes: el legajo, una vez.
  expect_identical(.veces_O121(html, "LEG-0001"), 0L)
  expect_identical(.veces_O121(html, "Secci\u00f3n omitida"), 1L)
  abierto <- .html_O121(medicion, proteger_datos_personales = FALSE)
  expect_gt(.veces_O121(abierto, "LEG-0001"), 0L)
  # Armada con la proteccion activa, se publica y no trae el valor.
  html_protegida <- .html_O121(.medicion_O121(TRUE))
  expect_identical(.veces_O121(html_protegida, "Secci\u00f3n omitida"), 0L)
  expect_identical(.veces_O121(html_protegida, "LEG-0001"), 0L)
})

test_that("la marca de objeto sin proteger sobrevive a subset, rbind y al historico", {
  medicion <- .medicion_O121(FALSE)
  protegida <- .medicion_O121(TRUE, id = "m0")
  expect_false(is.null(attr(medicion, "datos_personales_sin_proteger")))
  # `[` con columnas nombradas -lo que hace subset()- y `rbind()` con un objeto
  # protegido delante devolvian la misma clase sin la marca.
  sin_marca <- function(x) is.null(attr(x, "datos_personales_sin_proteger"))
  expect_false(sin_marca(subset(medicion, TRUE)))
  expect_false(sin_marca(medicion[1L, names(medicion)]))
  expect_false(sin_marca(rbind(protegida, medicion)))
  expect_identical(.veces_O121(.html_O121(subset(medicion, TRUE)), "LEG-0001"), 0L)

  # El historico armado con una medicion sin proteger publicaba el legajo.
  historico <- historico_calidad(protegida, medicion)
  expect_false(sin_marca(historico))
  expect_false(sin_marca(acumular_historico(historico_calidad(protegida), medicion)))
  expect_identical(.veces_O121(.html_O121(historico), "LEG-0001"), 0L)
  expect_true(sin_marca(historico_calidad(protegida)))
})

test_that("una medida suprimida no se publica en la seccion Historico", {
  instancia <- instanciar(especializar(metricas_nucleo()$NoNulo), "personas", "edad")
  medicion <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)),
                    id_medicion = "enero")
  evaluacion <- evaluar(medicion, perfil_evaluacion("Basico", regla_evaluacion(
    "Presente", function(x) x > 0, desenlace = "suprimir"
  )))
  html <- .html_O121(evaluacion, historico_calidad(medicion))
  inicio <- regexpr("<h2>Hist", html, fixed = TRUE)
  expect_gt(inicio, 0L)
  seccion <- substring(html, inicio)
  seccion <- substr(seccion, 1L, regexpr("</section>", seccion, fixed = TRUE))
  fila <- regmatches(seccion, regexpr("<tr>(?:(?!</tr>).)*enero-000002(?:(?!</tr>).)*</tr>",
                                      seccion, perl = TRUE))
  expect_length(fila, 1L)
  # Antes: <td>0</td>, el valor que la evaluacion del mismo informe tapaba.
  expect_match(fila, "[valor suprimido]", fixed = TRUE)
  expect_false(grepl("<td>0</td>", fila, fixed = TRUE))
})

test_that("un perfil sin proteccion vuelto a proteger sin la tabla lo declara", {
  datos <- data.frame(
    correo = rep(c("juan.perez@x.uy", "ana.diaz@x.uy", "pedro.sosa@x.uy"), 10),
    n = 1:30, stringsAsFactors = FALSE
  )
  aviso <- "protecci\u00f3n de datos personales desactivada"
  abierto <- suppressWarnings(perfilar(datos, proteger_datos_personales = FALSE))
  cerrado <- suppressWarnings(perfilar(datos))
  expect_identical(.veces_O121(.html_O121(abierto), aviso), 1L)
  expect_identical(.veces_O121(.html_O121(cerrado), aviso), 0L)
  # Pedido sin proteccion, no hay nada que declarar.
  expect_identical(
    .veces_O121(.html_O121(abierto, proteger_datos_personales = FALSE), aviso), 0L
  )
})

test_that("el informe no aborta con texto marcado UTF-8 que no es UTF-8", {
  roto <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(roto) <- "UTF-8"
  datos <- data.frame(ciudad = rep(c(roto, "otro", "mas"), 10), n = 1:30,
                      stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos))
  # Antes: "invalid multibyte string" y ningun archivo.
  html <- .html_O121(perfil)
  expect_true(validUTF8(html))
  expect_gt(.veces_O121(html, "caf\\xe9"), 0L)
})

test_that("las cifras del informe son las del objeto, sin redondeo de consola", {
  datos <- data.frame(x = c(123456789.5, 123456789.5, 987654321.25, 1))
  perfil <- suppressWarnings(perfilar(datos))
  expect_identical(perfil$columnas$maximo[[1L]], 987654321.25)
  html <- .html_O121(perfil)
  # Antes: 987654321 -ocho cifras significativas-, que no esta en el objeto.
  expect_gt(.veces_O121(html, "987654321.25"), 0L)
})

test_that("un perfil al que le falta un componente se declara y no aborta", {
  perfil <- perfilar(datos_administrativos)
  for (componente in c("dependencias", "hallazgos", "columnas")) {
    incompleto <- perfil
    incompleto[componente] <- list(NULL)
    for (proteger in c(TRUE, FALSE)) {
      html <- expect_no_error(.html_O121(incompleto, proteger_datos_personales = proteger))
      expect_match(html, "</html>", fixed = TRUE)
    }
  }
})

test_that("un salto de linea en el titulo no rompe el elemento title", {
  html <- .html_O121(perfilar(datos_administrativos), titulo = "Uno\nDos")
  titulo <- regmatches(html, regexpr("<title>[^<]*</title>", html))
  # Antes: <title>Uno<br>Dos</title>, que el navegador muestra literal.
  expect_identical(titulo, "<title>Uno Dos</title>")
})

# Los accesores `hallazgos()` y `cobertura()` devuelven una tabla que se imprime
# con un nombre de columna sin marca que no es UTF-8 valido -lo que deja
# `data.table::fread()` sobre un CSV en latin-1-. Quien imprimia el campo
# directo -`p$hallazgos`, `p$cobertura_diagnosticos`, o el atributo del plan al
# que manda su propia impresion- recibia un `data.frame` y `print()` abortaba al
# formatear sus columnas-lista. Y la frase de la clave declarada pegaba el
# nombre crudo junto a texto marcado UTF-8: una cadena que decia ser UTF-8 y no
# lo era.

testthat::skip_on_cran()

.datos_campos_no_utf8 <- function(nombre_clave = paste0("ciudad", rawToChar(as.raw(0xA0)))) {
  datos <- data.frame(id = c(1:12, NA))
  datos[[nombre_clave]] <- c(
    "Montevideo", "montevideo", "Montevido", "Canelones", "canelones",
    "Canelones", "Maldonado", "Maldonado", "Salto", "Salto", "Montevideo",
    "Canelones", NA
  )
  datos[[paste0("importe", rawToChar(as.raw(0xA0)))]] <- c(
    "12,5", "13,1", "14,2", "15,0", "16,3", "2,2", "3,3", "4,4", "5,5", "6,6",
    "7,7", "8,8", NA
  )
  datos
}

.perfil_campos_no_utf8 <- function(datos, clave = names(datos)[[2L]]) {
  # La clave del fixture repite valores y tiene un ausente: el aviso de la
  # clave declarada es el de siempre y no es lo que se prueba aca.
  expect_warning(
    perfil <- perfilar(datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
                       clave = clave),
    "no es \u00fanica"
  )
  perfil
}

.impresion_campos <- function(x) {
  tryCatch(
    utils::capture.output(print(x)),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
}

test_that("los campos del perfil y el atributo del plan se imprimen con un nombre que no es UTF-8", {
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "solo en una sesion UTF-8")
  datos <- .datos_campos_no_utf8()
  expect_false(validUTF8(names(datos)[[2L]]))
  perfil <- .perfil_campos_no_utf8(datos)
  # Las tablas traen lo que abortaba: el nombre adentro de una columna-lista.
  claves <- Filter(is.data.frame, lapply(perfil$hallazgos$trazabilidad, `[[`, "claves"))
  expect_true(any(vapply(claves, function(k) any(!validUTF8(names(k))), TRUE)))
  expect_true(any(vapply(perfil$cobertura_diagnosticos$columnas,
                         function(n) any(!validUTF8(n)), TRUE)))

  salida <- .impresion_campos(perfil$hallazgos)
  expect_false(any(startsWith(salida, "ERROR:")), info = salida[[1L]])
  # La impresion nombra la columna con su byte escapado, no la calla.
  expect_true(any(grepl("ciudad\\\\240", salida, fixed = TRUE)))
  expect_false(any(startsWith(.impresion_campos(perfil$cobertura_diagnosticos), "ERROR:")))
  # Un subconjunto del campo sigue imprimiendose.
  expect_false(any(startsWith(
    .impresion_campos(perfil$hallazgos[, "trazabilidad", drop = FALSE]), "ERROR:"
  )))

  # El campo es la tabla que devuelve el accesor, con la misma clase.
  expect_identical(perfil$hallazgos, hallazgos(perfil))
  expect_identical(perfil$cobertura_diagnosticos, cobertura(perfil))

  # La impresion del plan manda a leer este atributo.
  plan <- planificar_limpieza(perfil, datos)
  expect_false(any(startsWith(
    .impresion_campos(attr(plan, "cobertura_diagnosticos", exact = TRUE)), "ERROR:"
  )))
  aplicado <- suppressWarnings(aplicar(plan, datos))
  expect_false(any(startsWith(
    .impresion_campos(attr(aplicado$plan, "cobertura_diagnosticos", exact = TRUE)), "ERROR:"
  )))
})

test_that("la frase de la clave declarada no dice ser UTF-8 con bytes que no lo son", {
  datos <- .datos_campos_no_utf8()
  perfil <- .perfil_campos_no_utf8(datos)
  h <- perfil$hallazgos
  filas <- h$tipo_hallazgo %in% c("clave_con_ausentes", "clave_no_unica")
  expect_identical(sum(filas), 2L)
  frases <- h$descripcion[filas]
  expect_true(all(validUTF8(frases)))
  # El nombre va como lo muestra la consola, con el byte escapado.
  expect_true(all(grepl("`ciudad\\xa0`", frases, fixed = TRUE)))

  # Con la clave compuesta, lo mismo en la lista de nombres.
  expect_warning(
    compuesta <- perfilar(datos, fecha = as.POSIXct("2026-01-15", tz = "UTC"),
                          clave = names(datos)[2:3]),
    "ausentes"
  )
  hc <- compuesta$hallazgos
  frase <- hc$descripcion[hc$tipo_hallazgo == "clave_con_ausentes"]
  expect_length(frase, 1L)
  expect_true(validUTF8(frase))
  expect_true(grepl("`ciudad\\xa0`, `importe\\xa0`", frase, fixed = TRUE))
})

test_that("la clase no cambia ningun valor ni la impresion de lo que ya se imprimia", {
  datos <- .datos_campos_no_utf8()
  clave <- names(datos)[[2L]]
  perfil <- .perfil_campos_no_utf8(datos)
  h <- perfil$hallazgos
  # `columna` y la traza guardan el nombre real, byte por byte, y cruzan con
  # los datos.
  i <- which(h$tipo_hallazgo == "mayusculas_inconsistentes")
  expect_length(i, 1L)
  expect_identical(charToRaw(h$columna[[i]]), charToRaw(clave))
  expect_identical(datos[[h$columna[[i]]]], datos[[2L]])
  claves <- Filter(is.data.frame, lapply(h$trazabilidad, `[[`, "claves"))
  expect_true(all(vapply(claves, function(k) identical(names(k), clave), TRUE)))
  # `as.data.frame()` sigue dando la tabla sin la clase de impresion.
  expect_identical(class(as.data.frame(h)), "data.frame")
  expect_identical(class(as.data.frame(perfil$cobertura_diagnosticos)), "data.frame")

  # Con un nombre UTF-8 valido la frase de la clave lo nombra tal cual, y la
  # impresion de cada tabla es la de `print.data.frame()`: escapar de mas
  # tambien seria una diferencia.
  nombre <- paste0("a", intToUtf8(0xF1), "o")
  valido <- .datos_campos_no_utf8(nombre)
  valido[[3L]] <- NULL
  perfil_valido <- .perfil_campos_no_utf8(valido, clave = nombre)
  hv <- perfil_valido$hallazgos
  frases <- hv$descripcion[hv$tipo_hallazgo %in% c("clave_con_ausentes", "clave_no_unica")]
  expect_length(frases, 2L)
  expect_true(all(grepl(paste0("`", nombre, "`"), frases, fixed = TRUE)))
  plan <- planificar_limpieza(perfil_valido, valido)
  for (tabla in list(hv, perfil_valido$cobertura_diagnosticos,
                     attr(plan, "cobertura_diagnosticos", exact = TRUE))) {
    expect_true(nrow(tabla) > 0L)
    expect_identical(utils::capture.output(print(tabla)),
                     utils::capture.output(print(as.data.frame(tabla))))
  }
})

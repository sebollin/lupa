# La persistencia del paquete no puede depender del locale que la escribio.
#
# El formato RDS version 3 -el de `saveRDS()` por omision- anota en la cabecera
# la codificacion nativa de quien escribio, y al leer TRADUCE desde ella las
# cadenas sin marca. Escribiendo bajo `LC_CTYPE=C` la cabecera dice
# `ANSI_X3.4-1968`, y al leer bajo UTF-8 R avisa:
#
#   input string 'B<tilde>sico' cannot be translated from 'ANSI_X3.4-1968'
#   to UTF-8, but is valid UTF-8
#
# En glibc esa traduccion FALLA y R deja los bytes intactos: el dato se salva
# por el camino del error. En Windows `win_iconv` NO falla -apaga el bit alto-,
# asi que el rescate nunca corre y `B<tilde>sico` vuelve como `BC!sico`:
# `c3`->`43`, `a1`->`21`. Texto plausible, silenciosamente distinto. Medido en
# R-hub Windows: `acumular_historico()` rechazaba su propia corrida guardada.
#
# El formato 2 no anota codificacion nativa y por lo tanto no traduce nada.
#
# QUE SE MIDE Y QUE NO. Que un perfil guardado con el formato 3 vuelva corrupto
# NO es algo que el paquete haga: lo hace `readRDS()`. Y que
# `comparar_perfiles()` informe deriva sobre un perfil corrupto es CORRECTO, su
# texto cambio de verdad; hacer que comparara igual seria esconderlo. Lo que el
# paquete garantiza -y lo que esta guarda mide- es que SU propia persistencia
# conserve el texto, y que el remedio que documenta para `saveRDS()` funcione.
#
# La otra salida -declarar UTF-8 el texto al sellar el objeto- se probo y se
# DESCARTO: rompe `test-N52`, que exige que un nombre publicado conserve bytes
# Y MARCA para indexar la tabla del usuario. Bajo `C`, una cadena marcada y la
# misma sin marcar no son iguales ni para `==` ni para `[[`.

.n87_sin_marca <- function(x) rawToChar(charToRaw(x))

.n87_locale_utf8 <- function() {
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)
  for (locale in c("es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "en_US.UTF-8",
                   "C.UTF-8", "C.utf8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (identical(Sys.getlocale("LC_CTYPE"), locale)) return(locale)
  }
  NULL
}

# Devuelve los avisos que R emitio al leer, que es la senal de que INTENTO
# traducir. Donde no intenta, no hay plataforma que pueda romperlo.
.n87_leer_con_avisos <- function(leer) {
  avisos <- character()
  valor <- withCallingHandlers(
    leer(),
    warning = function(condicion) {
      avisos <<- c(avisos, conditionMessage(condicion))
      invokeRestart("muffleWarning")
    }
  )
  list(valor = valor, avisos = avisos)
}

.n87_historico <- function(nombre) {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN87"),
    "personas", "dato"
  )
  medidas <- medir(
    modelo(instancia), data.frame(dato = c(1, 1, NA, NA)),
    id_medicion = "Zebra_2", fecha = as.POSIXct("2026-09-17", tz = "UTC")
  )
  perfil <- perfil_evaluacion(
    nombre, regla_evaluacion("Presente", function(x) x == 1)
  )
  historico_calidad(suppressWarnings(evaluar(medidas, perfil)))
}

test_that("el control: esta guarda distingue el formato 3 del 2", {
  # Un control que solo se vio dar OK no se distingue de uno que no mide. Si el
  # formato 3 tambien pasara en silencio, los expect de abajo no probarian nada.
  locale_utf8 <- .n87_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 en esta maquina")
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  texto <- .n87_sin_marca("B\u00e1sico")
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"),
              "no se pudo fijar LC_CTYPE=C")
  saveRDS(list(x = texto), archivo, version = 3L)
  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  con_formato_3 <- .n87_leer_con_avisos(function() readRDS(archivo))

  # El control NO puede pedir el aviso, y esa correccion la pago una corrida de
  # R-hub: en Windows `win_iconv` no falla al traducir, asi que R NO avisa -y
  # devuelve los bytes cambiados-. La ausencia del aviso ES el defecto, no su
  # ausencia de defecto. Medido sobre `293685f`:
  #
  #   Linux   : 1 aviso, bytes intactos  (la conversion falla y salva el dato)
  #   Windows : 0 avisos, bytes cambiados (la conversion "funciona" y miente)
  #
  # Lo que vale en las dos plataformas es que el formato 3 NO ES SEGURO: o
  # avisa, o cambia los bytes. Si algun dia dejara de hacer las dos cosas, este
  # expect se pone rojo y avisa que la premisa del arreglo caduco.
  formato_3_inseguro <- length(con_formato_3$avisos) > 0L ||
    !identical(charToRaw(con_formato_3$valor$x), charToRaw(texto))
  expect_true(
    formato_3_inseguro,
    info = paste0(
      "el formato 3 no aviso y conservo los bytes: la premisa del arreglo ",
      "-que traduce lo sin marca- ya no se cumple en esta plataforma"
    )
  )

  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  saveRDS(list(x = texto), archivo, version = 2L)
  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  con_formato_2 <- .n87_leer_con_avisos(function() readRDS(archivo))
  expect_identical(length(con_formato_2$avisos), 0L)
  expect_identical(charToRaw(con_formato_2$valor$x), charToRaw(texto))
})

test_that("guardar_historico y leer_historico conservan el texto entre locales", {
  locale_utf8 <- .n87_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 en esta maquina")
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  nombre <- .n87_sin_marca("B\u00e1sico")
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"),
              "no se pudo fijar LC_CTYPE=C")
  guardar_historico(.n87_historico(nombre), archivo, sobrescribir = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  leido <- .n87_leer_con_avisos(function() suppressWarnings(leer_historico(archivo)))
  expect_identical(
    length(leido$avisos), 0L,
    info = paste("R intento traducir al leer:", paste(leido$avisos, collapse = " / "))
  )
  guardado <- leido$valor
  perfil_leido <- unique(guardado$perfil[!is.na(guardado$perfil)])
  expect_identical(charToRaw(perfil_leido[[1L]]), charToRaw(nombre))
  expect_identical(Encoding(perfil_leido[[1L]]), Encoding(nombre))

  # Y la promesa publica: acumular la corrida guardada sobre una construida en
  # este locale no la rechaza ni la duplica.
  expect_no_error(juntos <- acumular_historico(guardado, .n87_historico(nombre)))
  expect_identical(nrow(juntos), nrow(guardado))
})

test_that("el remedio que la documentacion nombra para saveRDS funciona", {
  locale_utf8 <- .n87_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 en esta maquina")
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  columna <- .n87_sin_marca("a\u00f1o_medici\u00f3n")
  datos <- data.frame(
    a = c(1, 2, NA, 4), b = c("x", "y", .n87_sin_marca("B\u00e1sico"), "x"),
    stringsAsFactors = FALSE
  )
  names(datos)[2L] <- columna
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"),
              "no se pudo fijar LC_CTYPE=C")
  guardado <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    columnas_no_negativas = columna
  ))
  saveRDS(guardado, archivo, version = 2L)

  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  leido <- .n87_leer_con_avisos(function() readRDS(archivo))
  expect_identical(length(leido$avisos), 0L)
  expect_identical(
    charToRaw(leido$valor$columnas$columna[[2L]]), charToRaw(columna)
  )
  fresco <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    columnas_no_negativas = columna
  ))
  deriva <- suppressWarnings(as.data.frame(comparar_perfiles(leido$valor, fresco)))
  expect_identical(sum(startsWith(as.character(deriva$aspecto), "configuracion_")), 0L)
})

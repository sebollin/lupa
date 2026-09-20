# El texto que el paquete GUARDA no puede depender del locale que lo guardo.
#
# El formato RDS version 3 -el de `saveRDS()` por omision- anota en la cabecera
# la codificacion nativa de quien escribio, y al leer traduce desde ella las
# cadenas SIN MARCA. Escribiendo bajo `LC_CTYPE=C` la cabecera dice
# `ANSI_X3.4-1968`, y al leer bajo UTF-8 R avisa:
#
#   input string 'B<tilde>sico' cannot be translated from 'ANSI_X3.4-1968'
#   to UTF-8, but is valid UTF-8
#
# En glibc esa traduccion FALLA y R deja los bytes intactos: el dato se salva
# por el camino del error. En Windows `win_iconv` NO falla -apaga el bit alto-,
# asi que el rescate nunca corre y `B<tilde>sico` queda `BC!sico`: `c3`->`43`,
# `a1`->`21`. Texto plausible, silenciosamente distinto. Medido en R-hub
# Windows: `comparar_perfiles()` publicaba una deriva de configuracion
# inexistente y `acumular_historico()` rechazaba su propia corrida guardada.
#
# La guarda no puede pedir el sintoma, porque el sintoma solo aparece en
# Windows. Pide la PROPIEDAD que lo hace imposible en todas partes: ninguna
# cadena guardada tiene bytes no ASCII con la marca en `unknown`. Eso es
# comprobable aca y se pone rojo aca.

.n86_sin_marca <- function(x) rawToChar(charToRaw(x))

# Recorre el objeto entero -listas, data.frames y atributos- y devuelve la ruta
# de cada cadena expuesta a la traduccion. Recorrer y no mirar campos elegidos a
# mano: un campo nuevo que olvide marcarse tiene que aparecer solo.
.n86_fragiles <- function(x, ruta = "") {
  salida <- character()
  if (is.character(x) && length(x)) {
    no_ascii <- !is.na(x) & grepl("[^\001-\177]", x, useBytes = TRUE)
    expuestas <- no_ascii & Encoding(x) == "unknown"
    if (any(expuestas)) {
      salida <- c(salida, paste0(ruta, " -> ", paste(
        unique(substr(x[expuestas], 1L, 24L)), collapse = " | "
      )))
    }
  }
  nombres <- names(x)
  if (is.list(x)) {
    for (i in seq_along(x)) {
      if (is.null(x[[i]])) next
      etiqueta <- if (!is.null(nombres) && nzchar(nombres[[i]])) {
        nombres[[i]]
      } else {
        paste0("[[", i, "]]")
      }
      salida <- c(salida, .n86_fragiles(x[[i]], paste0(ruta, "$", etiqueta)))
    }
  }
  atributos <- attributes(x)
  if (!is.null(atributos)) {
    for (a in setdiff(names(atributos), c("class", "row.names"))) {
      salida <- c(salida, .n86_fragiles(atributos[[a]], paste0(ruta, "@", a)))
    }
  }
  unique(salida)
}

.n86_locale_utf8 <- function() {
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)
  for (locale in c("es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "en_US.UTF-8",
                   "C.UTF-8", "C.utf8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    if (identical(Sys.getlocale("LC_CTYPE"), locale)) return(locale)
  }
  NULL
}

.n86_datos <- function() {
  datos <- data.frame(
    a = c(1, 2, NA, 4),
    b = c("x", "y", .n86_sin_marca("B\u00e1sico"), "x"),
    stringsAsFactors = FALSE
  )
  names(datos)[2L] <- .n86_sin_marca("a\u00f1o_medici\u00f3n")
  datos
}

test_that("ningun objeto que se persiste guarda texto expuesto a la traduccion", {
  datos <- .n86_datos()
  acento <- .n86_sin_marca("B\u00e1sico")

  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    columnas_sin_ceros = "a", columnas_no_negativas = names(datos)[2L]
  ))
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN86"), "personas", "a"
  )
  medidas <- medir(
    modelo(instancia), datos, id_medicion = acento,
    fecha = as.POSIXct("2026-09-17", tz = "UTC")
  )
  perfil_ev <- perfil_evaluacion(
    acento, regla_evaluacion("Presente", function(x) x == 1)
  )
  evaluacion <- suppressWarnings(evaluar(medidas, perfil_ev))
  historico <- historico_calidad(evaluacion)

  objetos <- list(
    perfil = perfil, medicion = medidas, perfil_evaluacion = perfil_ev,
    evaluacion = evaluacion, historico = historico
  )
  for (nombre in names(objetos)) {
    expuestas <- .n86_fragiles(objetos[[nombre]], nombre)
    expect_equal(
      length(expuestas), 0L,
      info = paste0(
        "cadenas guardadas sin marca y con bytes no ASCII:\n  ",
        paste(expuestas, collapse = "\n  ")
      )
    )
  }
})

test_that("un perfil guardado bajo C se lee bajo UTF-8 sin traducirse", {
  locale_utf8 <- .n86_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 en esta maquina")
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  datos <- .n86_datos()
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"),
              "no se pudo fijar LC_CTYPE=C")
  guardado <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    columnas_no_negativas = names(datos)[2L]
  ))
  saveRDS(guardado, archivo)

  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  avisos <- character()
  leido <- withCallingHandlers(
    readRDS(archivo),
    warning = function(condicion) {
      avisos <<- c(avisos, conditionMessage(condicion))
      invokeRestart("muffleWarning")
    }
  )
  # El aviso de R es la senal de que INTENTO traducir. Donde intenta, glibc
  # falla y salva el dato; `win_iconv` no falla y lo rompe. Que no intente es lo
  # unico que vale en las dos plataformas.
  expect_equal(
    length(avisos), 0L,
    info = paste("R intento traducir al leer:", paste(avisos, collapse = " / "))
  )
  expect_identical(
    charToRaw(leido$columnas$columna[[2L]]),
    charToRaw(.n86_sin_marca("a\u00f1o_medici\u00f3n"))
  )

  fresco <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    columnas_no_negativas = names(datos)[2L]
  ))
  deriva <- suppressWarnings(as.data.frame(comparar_perfiles(leido, fresco)))
  configuracion <- startsWith(as.character(deriva$aspecto), "configuracion_")
  expect_equal(sum(configuracion), 0L)
})

test_that("un historico guardado bajo C lo acepta acumular bajo UTF-8", {
  locale_utf8 <- .n86_locale_utf8()
  if (is.null(locale_utf8)) skip("no hay un locale UTF-8 en esta maquina")
  previo <- Sys.getlocale("LC_CTYPE")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", previo)), add = TRUE)

  acento <- .n86_sin_marca("B\u00e1sico")
  construir <- function() {
    nucleo <- metricas_nucleo()
    instancia <- instanciar(
      especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN86b"),
      "personas", "dato"
    )
    medidas <- medir(
      modelo(instancia), data.frame(dato = c(1, 1, NA, NA)),
      id_medicion = "Zebra_2", fecha = as.POSIXct("2026-09-17", tz = "UTC")
    )
    perfil_ev <- perfil_evaluacion(
      acento, regla_evaluacion("Presente", function(x) x == 1)
    )
    historico_calidad(suppressWarnings(evaluar(medidas, perfil_ev)))
  }

  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"),
              "no se pudo fijar LC_CTYPE=C")
  guardar_historico(construir(), archivo, sobrescribir = TRUE)

  suppressWarnings(Sys.setlocale("LC_CTYPE", locale_utf8))
  guardado <- suppressWarnings(leer_historico(archivo))
  expect_no_error(juntos <- acumular_historico(guardado, construir()))
  expect_identical(nrow(juntos), nrow(guardado))
})

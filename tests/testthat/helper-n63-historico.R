.n63_set_locale <- function(locale) {
  for (categoria in c("LC_CTYPE", "LC_COLLATE")) {
    suppressWarnings(Sys.setlocale(categoria, locale))
    if (!identical(Sys.getlocale(categoria), locale)) {
      stop("no se pudo fijar ", categoria, "=", locale)
    }
  }
}

.n63_sin_marca <- function(x) rawToChar(charToRaw(x))

.n63_crear_historico <- function() {
  nucleo <- metricas_nucleo()
  instancia <- instanciar(
    especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN63"),
    "personas", "dato"
  )
  modelo_n63 <- modelo(instancia)
  perfil_n63 <- perfil_evaluacion(
    .n63_sin_marca("B\u00e1sico"),
    regla_evaluacion("Presente", function(x) x == 1)
  )
  crear_corrida <- function(id, valores) {
    medidas <- medir(
      modelo_n63, data.frame(dato = valores), id_medicion = id,
      fecha = as.POSIXct("2026-09-17 12:00:00", tz = "UTC")
    )
    suppressWarnings(evaluar(medidas, perfil_n63))
  }
  historico_calidad(
    crear_corrida("Zebra_2", c(1, 1, NA, NA)),
    crear_corrida(.n63_sin_marca("a\u00f1o_1"), c(1, 1, 1, NA))
  )
}

.n63_child <- function(args) {
  if (length(args) && identical(args[[1L]], "--args")) args <- args[-1L]
  if (length(args) < 2L) stop("faltan modo y locale")
  modo <- args[[1L]]
  locale <- args[[2L]]
  .n63_set_locale(locale)
  pkgload::load_all("../..", quiet = TRUE)

  if (identical(modo, "guardar")) {
    guardar_historico(.n63_crear_historico(), args[[3L]], sobrescribir = TRUE)
    cat("ok=TRUE\n")
    return(invisible(NULL))
  }

  if (identical(modo, "deriva")) {
    historico <- suppressWarnings(leer_historico(args[[3L]]))
    avisos <- character()
    deriva <- withCallingHandlers(
      detectar_deriva_calidad(historico),
      warning = function(condicion) {
        avisos <<- c(avisos, conditionMessage(condicion))
        invokeRestart("muffleWarning")
      }
    )
    fila <- deriva[deriva$aspecto == "resultado", , drop = FALSE][1L, ,
      drop = FALSE]
    cat(
      "delta=", format(fila$delta[[1L]], digits = 17, scientific = FALSE),
      ";direccion=", as.character(fila$direccion[[1L]]),
      ";severidad=", as.character(fila$severidad[[1L]]),
      ";avisos=", length(avisos), "\n", sep = ""
    )
    return(invisible(NULL))
  }

  if (identical(modo, "acumular")) {
    historico <- suppressWarnings(leer_historico(args[[3L]]))
    nuevo <- .n63_crear_historico()
    guardar_historico(nuevo, args[[4L]], sobrescribir = TRUE)
    nuevo <- suppressWarnings(leer_historico(args[[4L]]))
    avisos <- character()
    acumulado <- withCallingHandlers(
      acumular_historico(historico, nuevo),
      warning = function(condicion) {
        avisos <<- c(avisos, conditionMessage(condicion))
        invokeRestart("muffleWarning")
      }
    )
    cat("ok=TRUE;filas=", nrow(acumulado), ";avisos=", length(avisos), "\n",
        sep = "")
    return(invisible(NULL))
  }

  if (identical(modo, "perfil")) {
    datos <- data.frame(
      texto = c(
        rep(.n63_sin_marca("B\u00e1sico"), 3L), "comun"
      ),
      stringsAsFactors = FALSE
    )
    perfil <- suppressWarnings(perfilar(
      datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
    saveRDS(perfil, args[[3L]])
    cat("ok=TRUE\n")
    return(invisible(NULL))
  }

  if (identical(modo, "comparar_perfiles")) {
    anterior <- suppressWarnings(readRDS(args[[3L]]))
    actual <- suppressWarnings(readRDS(args[[4L]]))
    avisos <- character()
    cambios <- withCallingHandlers(
      comparar_perfiles(anterior, actual),
      warning = function(condicion) {
        avisos <<- c(avisos, conditionMessage(condicion))
        invokeRestart("muffleWarning")
      }
    )
    cat("filas=", nrow(cambios), ";avisos=", length(avisos), "\n", sep = "")
    return(invisible(NULL))
  }

  stop("modo desconocido: ", modo)
}

# O07. Los diagnosticos de texto respetan el universo aplicable.
#
# `perfilar(..., aplicabilidad = list(col = ~cond))` declara que una columna
# solo corresponde a ciertas filas. El analisis y la identidad ya enmascaraban
# las demas como ausentes, pero los diagnosticos de texto recibian la columna
# CRUDA -mientras su vocabulario venia armado sobre la enmascarada-. Con eso
# `espacios_sobrantes` contaba la columna entera y trazaba solo lo aplicable, y
# el paquete disparaba su propia alarma: "cuenta 6 y traza 4".

.o07_datos <- function() {
  data.frame(
    activo = c("si", "si", "si", "si", "no", "no"),
    txt = c(" a ", " b ", " c ", " d ", " e ", " f "),
    stringsAsFactors = FALSE
  )
}

test_that("el paquete no dispara su alarma de trazabilidad con aplicabilidad declarada", {
  alarmas <- character()
  perfil <- withCallingHandlers(
    perfilar(.o07_datos(), aplicabilidad = list(txt = ~activo == "si")),
    warning = function(w) {
      if (grepl("trazabilidad inconsistente", conditionMessage(w), fixed = TRUE)) {
        alarmas <<- c(alarmas, conditionMessage(w))
      }
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(alarmas, character())
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "espacios_sobrantes", ]
  expect_equal(nrow(hallazgo), 1L)
  # Cuenta solo el universo aplicable, y lo que cuenta es lo que traza.
  expect_equal(hallazgo$n_afectados, 4L)
  expect_setequal(hallazgo$trazabilidad[[1L]]$indices_fila, 1:4)
})

test_that("sin aplicabilidad declarada, el diagnostico sigue mirando la columna entera", {
  perfil <- perfilar(.o07_datos())
  hallazgo <- perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "espacios_sobrantes", ]
  expect_equal(hallazgo$n_afectados, 6L)
})

# --- Fase 2: las acciones de limpieza tambien respetan el universo -----------
#
# Decidido por Sebastian el 2026-09-22: respetar la aplicabilidad en todo. Las
# acciones por celda solo tocan filas del universo; las conversiones de tipo,
# que no se pueden aplicar a medias, declaran en el plan su alcance real.

.o07_aplicar <- function(datos, regla) {
  perfil <- perfilar(datos, aplicabilidad = regla)
  plan <- planificar_limpieza(perfil, datos)
  list(plan = plan, resultado = aplicar(plan, datos))
}

test_that("una accion por celda no toca las filas fuera del universo", {
  datos <- data.frame(
    activo = c("si", "si", "si", "si", "no", "no"),
    txt = c("S/D", "S/D", "bueno", "malo", "S/D", "S/D"),
    stringsAsFactors = FALSE
  )
  salida <- .o07_aplicar(datos, list(txt = ~activo == "si"))
  fila <- salida$plan$estrategia == "convertir_ausencias_textuales"
  registro <- salida$resultado$registro
  hecho <- registro$n_cambiadas[registro$estrategia == "convertir_ausencias_textuales"]
  # Lo que el plan prometio es lo que paso.
  expect_equal(hecho, salida$plan$n_afectadas[fila])
  expect_equal(hecho, 2L)
  # Y las filas declaradas fuera quedan como estaban.
  expect_identical(salida$resultado$datos$txt[datos$activo == "no"], c("S/D", "S/D"))
})

test_that("el recorte de espacios tampoco toca las filas fuera del universo", {
  salida <- .o07_aplicar(.o07_datos(), list(txt = ~activo == "si"))
  expect_identical(salida$resultado$datos$txt[5:6], c(" e ", " f "))
  expect_identical(salida$resultado$datos$txt[1:4], c("a", "b", "c", "d"))
})

test_that("una conversion de tipo declara su alcance real", {
  datos <- data.frame(
    activo = c("si", "si", "si", "si", "no", "no"),
    monto = c("1.234,56", "2.345,67", "3.456,78", "4.567,89", "5.678,90", "6.789,01"),
    stringsAsFactors = FALSE
  )
  salida <- .o07_aplicar(datos, list(monto = ~activo == "si"))
  fila <- salida$plan$estrategia == "convertir_numero_regional"
  registro <- salida$resultado$registro
  hecho <- registro$n_cambiadas[registro$estrategia == "convertir_numero_regional"]
  # No se puede convertir media columna: el plan lo dice en vez de prometer 4.
  expect_equal(salida$plan$n_afectadas[fila], 6L)
  expect_equal(hecho, 6L)
  expect_true(grepl("columna entera", as.character(salida$plan$justificacion[fila]),
                    fixed = TRUE))
})

test_that("los bordes: factor, regla indeterminada y otra entrega", {
  # Una columna factor: la celda de fuera vuelve como texto, no como codigo.
  factor_datos <- data.frame(activo = c("si", "si", "no", "no"),
                             txt = factor(c("S/D", "ok", "S/D", "x")))
  salida <- .o07_aplicar(factor_datos, list(txt = ~activo == "si"))
  expect_identical(as.character(salida$resultado$datos$txt[3:4]), c("S/D", "x"))

  # Una fila con regla indeterminada no se toca: no se sabe si corresponde.
  indeterminada <- data.frame(activo = c("si", "si", NA, "no"),
                              txt = c("S/D", "ok", "S/D", "S/D"),
                              stringsAsFactors = FALSE)
  salida <- .o07_aplicar(indeterminada, list(txt = ~activo == "si"))
  expect_identical(salida$resultado$datos$txt[3:4], c("S/D", "S/D"))

  # Aplicado a otra entrega donde la regla no se puede evaluar: la accion
  # falla diciendo por que, en vez de tocar la columna entera.
  base <- data.frame(activo = c("si", "si", "no"), txt = c("S/D", "ok", "S/D"),
                     stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(base, aplicabilidad = list(txt = ~activo == "si")), base)
  otra <- data.frame(txt = c("S/D", "ok", "S/D"), stringsAsFactors = FALSE)
  resultado <- aplicar(plan, otra)
  fila <- resultado$registro$estrategia == "convertir_ausencias_textuales"
  expect_identical(as.character(resultado$registro$estado[fila]), "fallida")
  expect_identical(resultado$datos$txt, otra$txt)
})

test_that("el perfil guarda la regla y sobrevive a guardarlo", {
  datos <- .o07_datos()
  perfil <- perfilar(datos, aplicabilidad = list(txt = ~activo == "si"))
  archivo <- tempfile(fileext = ".rds")
  on.exit(unlink(archivo), add = TRUE)
  saveRDS(perfil, archivo, version = 2L)
  releido <- readRDS(archivo)
  regla <- releido$meta$reglas_aplicabilidad$txt
  expect_s3_class(regla, "formula")
  expect_identical(
    lupa:::.evaluar_predicado_aplicabilidad(datos, "txt", regla),
    datos$activo == "si"
  )
  expect_identical(perfilar(datos)$meta$reglas_aplicabilidad, list())
})

test_that("la regla se evalua sobre los datos originales, no sobre los ya modificados", {
  # Una accion previa del mismo plan cambiaba la columna de la que depende la
  # regla: quitarle el invisible a `cat` la volvia "x", la regla `cat == "x"`
  # metia la fila 3 en el universo, y el recorte tocaba justo la celda que el
  # perfil habia declarado fuera. El registro lo daba por ejecutado.
  invisible_cero <- intToUtf8(0x200B)
  datos <- data.frame(
    cat = c("x", "x", paste0("x", invisible_cero)),
    val = c(" A ", " B ", " C "),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, aplicabilidad = list(val = ~cat == "x"))
  plan <- planificar_limpieza(perfil)
  plan$estado <- "lista"
  plan$aplicar <- TRUE
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  expect_identical(resultado$datos$val[[3L]], " C ")
  recorte <- resultado$registro$estrategia == "recortar_espacios"
  expect_equal(resultado$registro$n_cambiadas[recorte], 2L)
})

test_that("despues de eliminar filas, una accion por celda con regla falla y lo dice", {
  # La mascara de los originales ya no se alinea con las filas que quedan.
  # Tocar la columna entera seria el defecto; se falla con un motivo que dice
  # que hacer.
  datos <- data.frame(activo = c("si", "si", "si", "no"),
                      val = c(" A ", " A ", " B ", " C "),
                      stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, aplicabilidad = list(val = ~activo == "si")), datos
  )
  plan$aplicar <- plan$estrategia %in% c("conservar_primera_duplicada", "recortar_espacios")
  plan$estado[plan$aplicar] <- "lista"
  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  recorte <- resultado$registro$estrategia == "recortar_espacios"
  expect_identical(as.character(resultado$registro$estado[recorte]), "fallida")
  expect_true(grepl("eliminar filas", as.character(resultado$registro$error[recorte]),
                    fixed = TRUE))
})

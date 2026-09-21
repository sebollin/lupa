# N99. Una diferencia que el canal no muestra es una diferencia que el lector
# no puede ver.
#
# Recortar espacios cambia el patron de `a+\a+9\a+9a ` a `a+\a+9\a+9a`. La
# deriva informaba SEIS cambios con severidad `error` cuyos `valor_anterior` y
# `valor_actual` se imprimian identicos: el espacio final no se ve en una
# columna de texto. El usuario leia "error: este patron desaparecio" junto a
# "error: este patron aparecio", con las dos cadenas iguales en pantalla.
#
# Es la misma familia que el NBSP de la evidencia: el paquete manda a mirar una
# superficie donde lo que cambio es invisible.

test_that("la deriva hace visible un cambio que solo esta en los espacios del borde", {
  con_espacio <- c(rep("ab ", 6L), rep("cd ", 3L), "EF", "gh ")
  datos <- data.frame(t = con_espacio, id = seq_along(con_espacio),
                      stringsAsFactors = FALSE)
  antes <- perfilar(datos, proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(antes, datos)
  elegida <- as.character(plan$estrategia) == "recortar_espacios"
  expect_true(any(elegida))
  plan$aplicar <- elegida
  despues <- perfilar(aplicar(plan, datos)$datos, proteger_datos_personales = FALSE)

  deriva <- as.data.frame(comparar_perfiles(antes, despues))
  patron <- deriva[as.character(deriva$aspecto) == "patron", , drop = FALSE]
  expect_true(nrow(patron) > 0L)

  anteriores <- as.character(patron$valor_anterior)
  anteriores <- anteriores[!is.na(anteriores)]
  actuales <- as.character(patron$valor_actual)
  actuales <- actuales[!is.na(actuales)]
  expect_true(length(anteriores) > 0L)

  # Lo que desaparecio tenia un espacio al borde, y eso tiene que VERSE.
  for (v in anteriores) expect_true(grepl('^".*"$', v))
  # Lo que aparecio no lo tiene, y no se cita: la cita es una senal, no un
  # adorno.
  for (v in actuales) expect_false(grepl('^".*"$', v))

  # Y las dos columnas tienen que seguir hablando el mismo idioma: quitando la
  # cita y el espacio, cada desaparecido es un aparecido.
  sin_cita <- function(z) trimws(gsub('^"|"$', "", z))
  expect_setequal(sin_cita(anteriores), sin_cita(actuales))
})

test_that("una deriva corriente no se llena de comillas", {
  antes <- perfilar(data.frame(x = c(1, 2, 3, 4, 5)),
                    fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  despues <- perfilar(data.frame(x = c(1, 2, 3, 4, 99)),
                      fecha = as.POSIXct("2026-01-02", tz = "UTC"))
  deriva <- as.data.frame(comparar_perfiles(antes, despues))
  valores <- c(as.character(deriva$valor_anterior), as.character(deriva$valor_actual))
  valores <- valores[!is.na(valores)]
  expect_true(length(valores) > 0L)
  expect_false(any(grepl('^".*"$', valores)))
})

test_that("el texto de la deriva cita por espacio o por escape, y no mezcla convenciones", {
  citar <- lupa:::.texto_deriva
  # Sin nada que senalar, sale tal cual.
  expect_identical(citar("abc"), "abc")
  # Espacio al borde y cadena vacia: se citan SIN volver a escapar, para que la
  # fila citada siga casando con la que no lo esta.
  expect_identical(citar("abc "), "\"abc \"")
  expect_identical(citar(""), "\"\"")
  expect_identical(citar("a\\b "), "\"a\\b \"")
  # Con algo que escapar, se rinde con el renderizador del paquete.
  invisible_duro <- paste0("x", intToUtf8(0x00A0), "y")
  expect_true(grepl("U+00A0", citar(invisible_duro), fixed = TRUE))
  # Los numeros no pasan por ninguna de las dos.
  expect_identical(citar(3.5), "3.5")
  expect_identical(citar(NA), NA_character_)
})

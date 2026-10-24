# Recoge lo que una expresion imprime, venga por donde venga.
#
# Por que existe: `cli` emite su salida como CONDICIONES de mensaje, y dentro de
# `testthat` esas condiciones quedan atrapadas antes de llegar a ningun flujo
# -es lo que le permite sostener `expect_message()`-. Una prueba que usa
# `capture.output(..., type = "message")` no recibe nada y falla afirmando que
# el texto no esta, cuando lo que no esta es el flujo. Medido: 17 de 192
# archivos de prueba fallaban corridos solos, con 55 fallos, mientras la suite
# entera daba FAIL 0; los 17 capturaban el flujo de mensajes.
#
# Se recogen las condiciones, se suma la salida estandar, se quitan los codigos
# de color y se normaliza el espacio: `cli` ajusta al ancho e inserta saltos
# DENTRO de una frase -medido: "~0,13 GB por\nmillon de filas"-, con lo que una
# busqueda literal falla por donde se corto la linea y no por lo que dice.
salida_cli <- function(expr) {
  mensajes <- character()
  salida <- withCallingHandlers(
    capture.output(force(expr)),
    message = function(m) {
      mensajes <<- c(mensajes, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  texto <- paste(c(mensajes, salida), collapse = " ")
  gsub("[[:space:]]+", " ", cli::ansi_strip(texto))
}

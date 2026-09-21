# La capa de accion, segunda pasada: lo que el plan propone se tiene que poder
# ejecutar, y lo que la deriva publica tiene que ser lo que el perfil publica.

.n94_bytes <- function(codigos) {
  x <- rawToChar(as.raw(codigos))
  Encoding(x) <- "bytes"
  x
}

.N94_SEC <- c(0x61, 0xc3, 0xb1, 0x6f)

test_that("las capitalizaciones se ejecutan sobre una columna declarada `bytes`", {
  # Estaban las tres en `fallida` con `n_cambiadas = 0`: `tolower()` y
  # `toupper()` sobre la columna cruda no pueden con esa marca. El perfil
  # analiza la forma saneada y proponia acciones que no se podian ejecutar.
  datos <- data.frame(
    v = c(.n94_bytes(.N94_SEC), "Montevideo", "MONTEVIDEO", "montevideo",
          "Salto", "SALTO"),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  plan <- suppressWarnings(planificar_limpieza(perfil))
  capitalizaciones <- c("convertir_minusculas", "convertir_titulo",
                        "convertir_mayusculas")
  presentes <- intersect(capitalizaciones, plan$estrategia)
  skip_if(!length(presentes), "el plan no propuso capitalizaciones")

  corrio <- FALSE
  for (estrategia in presentes) {
    i <- which(plan$estrategia == estrategia)[[1L]]
    # Solo las que el plan dejo `lista`: una `bloqueada` se rechaza a
    # proposito, y activarla igual seria probar otra cosa.
    # `as.character()`: `plan$estado` es un FACTOR, asi que comparar con
    # `identical()` contra una cadena da FALSE siempre y el `next` salteaba las
    # tres. La prueba pasaba en verde con CERO comprobaciones.
    if (!identical(as.character(plan$estado[[i]]), "lista")) next
    corrio <- TRUE
    activo <- plan
    activo$aplicar <- seq_len(nrow(plan)) == i
    resultado <- suppressWarnings(aplicar(activo, datos))
    registro <- resultado$registro
    fila <- registro[registro$estrategia == estrategia, , drop = FALSE]
    expect_identical(fila$estado[[1L]], "ejecutada", info = estrategia)
    expect_gt(as.numeric(fila$n_cambiadas[[1L]]), 0)
  }
  # Que el bucle haya corrido alguna vez: una prueba que no comprueba nada pasa
  # en verde y no se distingue de una que mide.
  expect_true(corrio)
})

test_that("la deriva publica el patron, no su clave", {
  # `comparar_perfiles()` sacaba el patron con la barra invertida duplicada
  # -el escape que hace inyectiva a la clave- y no coincidia con el mismo
  # patron en `perfil$patrones`. La clave APAREA; lo que se publica es el
  # patron.
  hacer <- function(valores) suppressWarnings(perfilar(
    data.frame(v = valores, stringsAsFactors = FALSE),
    fecha = as.POSIXct("2026-09-17", tz = "UTC"),
    analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  crudo <- .n94_bytes(.N94_SEC)
  texto <- rawToChar(as.raw(.N94_SEC))
  anterior <- hacer(c(crudo, "Hola ", " Hola", "hola"))
  actual <- hacer(c(texto, "Hola ", " Hola", "hola"))
  deriva <- suppressWarnings(as.data.frame(comparar_perfiles(anterior, actual)))
  filas <- deriva[deriva$aspecto == "patron", , drop = FALSE]
  skip_if(!nrow(filas), "no hubo deriva de patron")

  publicados <- unlist(anterior$patrones$v$patron)
  en_deriva <- unlist(filas$valor_anterior)
  en_deriva <- en_deriva[!is.na(en_deriva)]
  for (valor in en_deriva) {
    expect_true(
      any(vapply(publicados, function(p) identical(charToRaw(p),
                                                   charToRaw(valor)),
                 logical(1L))),
      info = paste("la deriva publico algo que el perfil no publica:", valor)
    )
  }
})

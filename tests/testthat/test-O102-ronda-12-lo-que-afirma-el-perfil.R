# Ronda 12: lo que `perfilar()` afirma, y el informe que lo comparte.

test_that("una columna vacia con dos formas de ausencia cuenta lo mismo en todos lados", {
  avisos <- character()
  perfil <- withCallingHandlers(
    perfilar(data.frame(col = c("", "", "", " "), stringsAsFactors = FALSE)),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # Antes: "cuenta 3 y traza 4", y el paquete se acusaba a si mismo.
  expect_false(any(grepl("problema de `lupa`", avisos, fixed = TRUE)))
  h <- as.data.frame(hallazgos(perfil))
  constante <- h[h$tipo_hallazgo == "constante", , drop = FALSE]
  expect_equal(nrow(constante), 1L)
  expect_equal(constante$n_afectados, 4)
  expect_equal(length(constante$trazabilidad[[1L]]$indices_fila), 4L)
  expect_match(constante$evidencia, "4 de 4", fixed = TRUE)
  # Control: con una sola forma ya coincidian y siguen coincidiendo.
  una <- suppressWarnings(perfilar(data.frame(col = rep("", 4))))
  h1 <- as.data.frame(hallazgos(una))
  expect_equal(h1$n_afectados[h1$tipo_hallazgo == "constante"], 4)
})

test_that("el informe dice que una dependencia se midio sobre la muestra", {
  n <- 2000L
  x <- rep(paste0("g", 1:20), each = 100)
  y <- ifelse(seq_len(n) %% 2 == 1, paste0("v", x), "ROTO")
  datos <- data.frame(x = x, y = y, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(perfilar(datos, muestra = 1000))
  expect_true(isTRUE(attr(perfil$dependencias, "muestreado")))
  archivo <- tempfile(fileext = ".html")
  on.exit(unlink(archivo), add = TRUE)
  reportar(perfil, archivo = archivo)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_match(html, "sobre una muestra de 1.000 de 2.000 filas", fixed = TRUE)
  # Control: sin muestreo, la nota no aparece.
  completo <- suppressWarnings(perfilar(datos))
  reportar(completo, archivo = archivo, sobrescribir = TRUE)
  html <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"), collapse = "")
  expect_false(grepl("se buscaron sobre una muestra", html, fixed = TRUE))
})

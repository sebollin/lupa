# Una cifra publicada sin reproductor envejece en silencio. La vineta
# `perfilar-una-base.Rmd` decia en un pasaje que el perfil cubre 111 campos
# analiticos y en otros dos que cubre 112; el paquete produce 112. La misma
# vineta se contradecia a si misma y nadie lo veia, porque ninguna medicion
# ataba esa cifra a lo que el paquete hace.
#
# Esta guarda la ata: cuenta los campos y exige que TODAS las mencionas de la
# vineta digan ese numero. Si manana el perfil suma un campo, falla aca y no en
# la lectura de alguien.

.campos_analiticos_n70 <- function() {
  datos <- data.frame(
    a = c("x", "y", "x", NA), b = c(1, 2, 3, 4),
    c = as.Date("2026-01-01") + 0:3, stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  # Todas menos la del nombre de la columna, que es como la vineta los cuenta.
  ncol(perfil$columnas) - 1L
}

.vineta_n70 <- function() {
  # Bajo `R CMD check` la vineta viaja instalada en `doc/`; en desarrollo esta
  # en el arbol de fuentes. Se buscan las dos, y si no hay ninguna se salta:
  # lo que no puede pasar es leer una y creer que se leyeron las dos.
  instalada <- system.file("doc", "perfilar-una-base.Rmd", package = "lupa")
  if (nzchar(instalada) && file.exists(instalada)) return(instalada)
  fuente <- testthat::test_path("..", "..", "vignettes", "perfilar-una-base.Rmd")
  if (file.exists(fuente)) return(fuente)
  NULL
}

test_that("la vineta declara la cantidad de campos que el perfil produce", {
  ruta <- .vineta_n70()
  skip_if(is.null(ruta), "la vineta no esta disponible en esta corrida")

  campos <- .campos_analiticos_n70()
  expect_gt(campos, 50L)

  lineas <- readLines(ruta, warn = FALSE)
  # El patron tiene que cubrir las DOS formas en que la vineta los cuenta. La
  # primera version buscaba solo "campos anal" y la mencion equivocada decia
  # "el perfil cubre sus 111": la guarda pasaba sobre el mismisimo caso que la
  # motivo. Un filtro que no atrapa el defecto que lo origino no mide nada.
  menciones <- grep("campos anal|cubre sus", lineas, value = TRUE)
  expect_gt(length(menciones), 2L)

  # Cada numero de dos o tres cifras que acompane a "campos analiticos" -o que
  # los cuente sin nombrarlos, como "cubre sus N"- tiene que ser el medido.
  numeros <- unlist(regmatches(
    menciones, gregexpr("\\b[0-9]{2,4}\\b", menciones)
  ))
  numeros <- as.integer(numeros)
  # `quince` va escrito con letra en la vineta; los numericos son los que
  # cuentan el total del perfil.
  expect_gt(length(numeros), 0L)
  expect_identical(unique(numeros), campos)
})

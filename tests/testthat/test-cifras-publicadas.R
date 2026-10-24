# Las cifras concretas de la documentacion envejecen solas, y nadie las mira.
#
# El 2026-09-05 las vinetas y los dos README decian "109 campos analiticos"
# cuando eran 111. Nadie lo habia notado porque una cifra en prosa no falla: se
# lee y se cree. El proyecto ya tenia una guarda asi para los nombres de
# `tipo_hallazgo` -que cruza los dos README con el codigo-, y esta es la misma
# idea para el ancho del perfil.
#
# La cifra sale de CORRER el perfil, no de una constante escrita al lado.

test_that("la cifra de campos analiticos que publica la documentacion es la real", {
  raiz <- testthat::test_path("..", "..")
  # Bajo `R CMD check` ni los README ni las fuentes de las vinetas estan a la
  # vista; en el arbol de desarrollo estan todos, y ahi se recorren todos: una
  # cifra escrita a mano puede aparecer en cualquier pagina.
  skip_if_not(file.exists(file.path(raiz, "README.es.md")),
              "Sin README a la vista: bajo R CMD check no viajan.")
  archivos <- c(
    file.path(raiz, c("README.md", "README.es.md")),
    list.files(file.path(raiz, "vignettes"), "\\.Rmd$", full.names = TRUE,
               recursive = TRUE)
  )
  archivos <- archivos[file.exists(archivos)]
  expect_gt(length(archivos), 10L)

  perfil <- perfilar(
    data.frame(a = 1:50, b = letters[rep(1:5, 10L)], stringsAsFactors = FALSE),
    analizar_dependencias = FALSE
  )
  # "ademas del nombre de la columna": por eso se descuenta uno.
  campos <- ncol(perfil$columnas) - 1L

  # Primera mitad: el perfil tiene un ancho razonable. Si `perfilar()` devolviera
  # una tabla vacia, todo lo de abajo pasaria sin medir nada.
  expect_gt(campos, 50L)

  # Las dos formas en que la documentacion cuenta los campos: nombrandolos
  # ("117 campos analiticos") y sin nombrarlos ("el perfil cubre sus 117"). Una
  # guarda que buscaba solo la primera paso sobre el caso que la motivo. Lo
  # calculado en linea no aparece como numero en la fuente, y cero menciones
  # escritas a mano es lo esperado.
  patron <- "([0-9]{2,4})[ -](campos anal|analytic-field|analytic field|cam)|cubre sus ([0-9]{2,4})"
  for (archivo in archivos) {
    texto <- paste(readLines(archivo, warn = FALSE, encoding = "UTF-8"),
                   collapse = " ")
    encontradas <- regmatches(texto, gregexpr(patron, texto, perl = TRUE))[[1L]]
    if (!length(encontradas)) next
    cifras <- unique(as.integer(sub("^\\D*([0-9]+).*$", "\\1", encontradas)))
    for (cifra in cifras) {
      expect_equal(cifra, campos, info = basename(archivo))
    }
  }
})

test_that("la tabla de evidencia del README sale de la corrida que dice", {
  raiz <- testthat::test_path("..", "..")
  archivos <- c(
    ingles = file.path(raiz, "README.md"),
    espanol = file.path(raiz, "README.es.md")
  )
  skip_if_not(all(file.exists(archivos)), "Sin README a la vista.")

  # La corrida que el propio README muestra, con sus valores por omision.
  perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
  tipos <- unique(as.character(perfil$hallazgos$tipo_hallazgo))

  # Primera mitad: la corrida produce hallazgos. Si no produjera ninguno, la
  # comprobacion de abajo no probaria nada.
  expect_gt(length(tipos), 5L)

  encabezados <- c(ingles = "Among the outputs of that run are",
                   espanol = "Entre las salidas de esa corrida aparecen")
  for (idioma in names(archivos)) {
    lineas <- readLines(archivos[[idioma]], warn = FALSE, encoding = "UTF-8")
    inicio <- grep(encabezados[[idioma]], lineas, fixed = TRUE)
    # Sin el encabezado la guarda no tiene que mirar: eso es una falla, no un
    # salto. Saltearla dejaba de medir la tabla en silencio el dia en que alguien
    # reescribiera la frase.
    expect_gt(length(inicio), 0L, label = paste("encabezado de la tabla en", idioma))
    if (!length(inicio)) next
    # La tabla termina en la primera linea en blanco despues de empezar.
    resto <- lineas[seq(inicio[[1L]] + 1L, length(lineas))]
    fin <- which(!nzchar(trimws(resto)) & seq_along(resto) > 2L)[[1L]]
    tabla <- resto[seq_len(fin)]
    # Se descartan el encabezado y su separador: la celda de encabezado dice
    # `tipo_hallazgo`, que es el nombre de la columna y no un hallazgo. Sin este
    # recorte la guarda se denunciaba a si misma.
    tabla <- tabla[!grepl("^\\|\\s*(-+|:?-+:?)\\s*\\|", tabla)]
    tabla <- tabla[!grepl("`tipo_hallazgo`", tabla, fixed = TRUE)]

    citados <- unique(unlist(regmatches(
      tabla, gregexpr("(?<=\\| `)[a-z_]+(?=` *\\|)", tabla, perl = TRUE)
    )))
    expect_gt(length(citados), 5L)
    # Cada `tipo_hallazgo` que la tabla cita tiene que salir de esa corrida: la
    # frase que la encabeza lo afirma. El 2026-09-05 la tabla citaba
    # `valor_concentrado`, que esa corrida no produce, y daba como ejemplo de
    # `faltantes_disfrazados` una evidencia que solo aparece con la proteccion
    # de datos personales desactivada.
    expect_equal(setdiff(citados, tipos), character(), info = idioma)
  }
})

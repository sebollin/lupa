# O31: `poder_discriminante = "verificado"` sale de comparar una proporcion
# medida contra un umbral declarado, y esa proporcion no se publicaba: una
# columna con el 90 % de las cedulas validas -el umbral- publicaba exactamente
# lo mismo que una con el 100 %.

.cedulas_de_prueba <- function(n_malas, n = 200L) {
  pesos <- c(2L, 9L, 8L, 7L, 6L, 3L, 4L)
  bases <- seq(1000000L, by = 37L, length.out = n)
  digito <- function(base) {
    d <- as.integer(strsplit(sprintf("%07d", base), "")[[1L]])
    (10L - sum(d * pesos) %% 10L) %% 10L
  }
  cedulas <- vapply(bases, function(b) sprintf("%07d%d", b, digito(b)),
                    character(1L))
  if (n_malas > 0L) {
    malas <- seq_len(n_malas)
    cedulas[malas] <- vapply(bases[malas], function(b) {
      sprintf("%07d%d", b, (digito(b) + 1L) %% 10L)
    }, character(1L))
  }
  cedulas
}

test_that("la proporcion que decide verificado se publica", {
  casos <- list(
    todas = list(malas = 0L, esperado = 1, poder = "verificado"),
    en_el_umbral = list(malas = 20L, esperado = 0.9, poder = "verificado"),
    bajo_el_umbral = list(malas = 40L, esperado = 0.8, poder = "alto")
  )
  for (nombre in names(casos)) {
    caso <- casos[[nombre]]
    cedulas <- .cedulas_de_prueba(caso$malas)
    # La medicion independiente: el validador del paquete cuenta lo mismo que
    # la fila publicada.
    expect_equal(mean(validar_ci_uy(cedulas)), caso$esperado, info = nombre)

    datos <- data.frame(cedula = cedulas, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, analizar_dependencias = FALSE,
                       proteger_datos_personales = FALSE)
    fila <- as.data.frame(perfil$datos_personales)

    expect_equal(nrow(fila), 1L, info = nombre)
    expect_equal(as.character(fila$poder_discriminante[[1L]]), caso$poder,
                 info = nombre)
    expect_equal(fila$proporcion_verificada[[1L]], caso$esperado,
                 info = nombre)
  }
})

test_that("una columna declarada a mano no inventa una proporcion", {
  # Control: la columna nueva no obliga a todas las filas a traer un numero.
  datos <- data.frame(
    cedula = .cedulas_de_prueba(0L),
    otra = paste0("x", seq_len(200L)),
    stringsAsFactors = FALSE
  )

  perfil <- perfilar(datos, columnas_personales = "otra",
                     analizar_dependencias = FALSE,
                     proteger_datos_personales = FALSE)
  fila <- as.data.frame(perfil$datos_personales)
  declarada <- fila[fila$columna == "otra", , drop = FALSE]

  expect_equal(nrow(declarada), 1L)
  expect_true(is.na(declarada$proporcion_verificada[[1L]]))
  expect_equal(as.character(declarada$poder_discriminante[[1L]]), "declarado")
})

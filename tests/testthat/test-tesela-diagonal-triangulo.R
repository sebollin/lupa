# La tesela diagonal de `.comparar_bloques_duplicados()` solo entrega su
# triangulo superior, y calculaba la matriz entera: el doble de distancias. Ahora
# calcula el triangulo par a par, salvo con `jaccard`, donde `stringdist()` par a
# par no coincide con la matriz entre dos textos vacios. Estas pruebas fijan que
# lo que llega a `on_pairs` y lo que se devuelve es lo de la matriz entera, con
# cada metodo que el paquete acepta, y que la diagonal cuesta la mitad.

# Fuera de CRAN: diez metodos por tres anchos de tesela tardan mas de los 0,2 s
# que se admiten en modo CRAN (medido el 2026-11-06). Corre en la suite completa
# y en el CI, con NOT_CRAN=true.
testthat::skip_on_cran()

.metodos_duplicados <- c(
  "osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
  "soundex"
)

.umbral_de <- function(metodo) {
  switch(metodo, osa = 1, lv = 2, dl = 1, hamming = 1, lcs = 2, qgram = 2,
         cosine = 0.2, jaccard = 0.3, jw = 0.1, soundex = 0)
}

# La definicion: cada tesela con la matriz entera de `stringdistmatrix()`, la
# diagonal recortada a su triangulo superior, en el mismo orden de teselas.
.pares_por_matriz <- function(valores, filas, metodo, umbral, bloque) {
  n <- length(valores)
  inicios <- seq.int(1L, n, by = bloque)
  salida <- list()
  for (i in seq_along(inicios)) {
    filas_i <- inicios[[i]]:min(n, inicios[[i]] + bloque - 1L)
    for (j in i:length(inicios)) {
      filas_j <- inicios[[j]]:min(n, inicios[[j]] + bloque - 1L)
      m <- as.matrix(stringdist::stringdistmatrix(
        valores[filas_i], valores[filas_j], method = metodo, p = 0.1,
        nthread = 1L
      ))
      cand <- if (i == j) {
        which(upper.tri(m) & m >= 0 & m <= umbral, arr.ind = TRUE)
      } else {
        which(m >= 0 & m <= umbral, arr.ind = TRUE)
      }
      if (!nrow(cand)) next
      salida[[length(salida) + 1L]] <- list(
        filas[filas_i[cand[, 1L]]], filas[filas_j[cand[, 2L]]],
        as.numeric(m[cand])
      )
    }
  }
  salida
}

.valores_de_prueba <- function() {
  # ASCII y una letra acentuada armada con escape; repetidos, cortos, largos y
  # tres textos vacios, que es donde `jaccard` par a par se aparta.
  letras <- c(letters[1:6], "\u00e1", " ")
  formas <- vapply(seq_len(45L), function(k) {
    paste(letras[(k * c(1L, 3L, 5L, 7L)[seq_len(1L + k %% 4L)]) %% 8L + 1L],
          collapse = "")
  }, character(1L))
  c(formas, formas[c(2L, 5L, 9L, 2L)], "", "", "abcabcabcabcabcabc", "")
}

test_that("on_pairs recibe lo mismo que con la matriz entera, con cada metodo", {
  skip_if_not_installed("stringdist")
  todos <- .valores_de_prueba()
  ascii <- vapply(todos, function(s) all(charToRaw(s) < as.raw(0x80)), logical(1L),
                  USE.NAMES = FALSE)
  for (metodo in .metodos_duplicados) {
    # `soundex` avisa sobre letras fuera de ASCII; el aviso no es lo que se prueba.
    valores <- if (metodo == "soundex") todos[ascii] else todos
    filas <- seq_along(valores) * 2L
    for (bloque in c(17L, 60L, 1000L)) {
      umbral <- .umbral_de(metodo)
      llamadas <- list()
      resultado <- lupa:::.comparar_bloques_duplicados(
        valores, filas, metodo, umbral, bloque, Inf,
        on_pairs = function(f1, f2, d) {
          llamadas[[length(llamadas) + 1L]] <<- list(f1, f2, d)
        },
        nucleos = 1L
      )
      esperado <- .pares_por_matriz(valores, filas, metodo, umbral, bloque)
      expect_identical(llamadas, esperado, label = paste(metodo, bloque))
      # El camino de solo pares, el del vocabulario, entrega lo mismo.
      solo <- list()
      lupa:::.comparar_bloques_duplicados(
        valores, filas, metodo, umbral, bloque, 1L,
        on_pairs = function(f1, f2, d) solo[[length(solo) + 1L]] <<- list(f1, f2, d),
        nucleos = 1L, solo_pares = TRUE
      )
      expect_identical(solo, esperado, label = paste(metodo, bloque, "solo pares"))
      expect_equal(resultado$n_hallados, sum(vapply(esperado, function(x) {
        sum(is.finite(x[[3L]]))
      }, numeric(1L))))
    }
  }
})

test_that("jaccard conserva los pares entre textos vacios", {
  skip_if_not_installed("stringdist")
  # La matriz da 0 entre dos textos vacios; `stringdist()` par a par puede dar
  # NaN, y el par se perderia. Control de que se sigue usando la matriz.
  valores <- c("", "", "a", "ab", "", "b")
  llamadas <- list()
  lupa:::.comparar_bloques_duplicados(
    valores, seq_along(valores), "jaccard", 0.3, 1000L, Inf,
    on_pairs = function(f1, f2, d) llamadas[[length(llamadas) + 1L]] <<- list(f1, f2, d),
    nucleos = 2L
  )
  pares <- paste(llamadas[[1L]][[1L]], llamadas[[1L]][[2L]])
  expect_true(all(c("1 2", "1 5", "2 5") %in% pares))
})

test_that("la tesela diagonal calcula solo su triangulo", {
  skip_if_not_installed("stringdist")
  valores <- .valores_de_prueba()
  n <- length(valores)
  celdas <- 0
  matriz <- lupa:::.matriz_distancias_duplicados
  par_a_par <- lupa:::.distancias_pares_duplicados
  testthat::local_mocked_bindings(
    .matriz_distancias_duplicados = function(a, b, ...) {
      celdas <<- celdas + length(a) * length(b)
      matriz(a, b, ...)
    },
    .distancias_pares_duplicados = function(a, b, ...) {
      celdas <<- celdas + length(a)
      par_a_par(a, b, ...)
    },
    .package = "lupa"
  )
  lupa:::.comparar_bloques_duplicados(
    valores, seq_len(n), "jw", 0.1, 1000L, Inf, nucleos = 1L
  )
  expect_equal(celdas, n * (n - 1) / 2)
  # Con dos teselas: dos triangulos y el rectangulo entre ellas.
  celdas <- 0
  mitad <- ceiling(n / 2)
  lupa:::.comparar_bloques_duplicados(
    valores, seq_len(n), "lv", 1, mitad, Inf, nucleos = 1L
  )
  resto <- n - mitad
  expect_equal(celdas, mitad * (mitad - 1) / 2 + resto * (resto - 1) / 2 +
                 mitad * resto)
  # `jaccard` sigue pagando la matriz entera.
  celdas <- 0
  lupa:::.comparar_bloques_duplicados(
    valores, seq_len(n), "jaccard", 0.3, 1000L, Inf, nucleos = 1L
  )
  expect_equal(celdas, n * n)
})

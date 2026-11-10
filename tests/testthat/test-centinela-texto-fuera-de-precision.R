# Un entero escrito como texto por encima de 2^53 se resume por su texto
# (`omitidos_precision`, extremos exactos, mediana por las centrales). Esa rama
# no sacaba los centinelas declarados: con `-999` declarado publicaba
# `minimo_exacto = "-999"` y `n_negativos = 1`, y la mediana no se publicaba,
# mientras `integer64` y la rama numerica con los mismos valores los dejaban
# afuera y los contaban. Ahora aplica la misma regla. Y un "-0" es el cero.

.ctf_fila <- function(v, sentinelas = NULL) {
  argumentos <- list(datos = data.frame(v = v, stringsAsFactors = FALSE))
  if (length(sentinelas)) argumentos$sentinelas_numericos <- sentinelas
  perfil <- suppressWarnings(suppressMessages(do.call(perfilar, argumentos)))
  perfil$columnas[perfil$columnas$columna == "v", , drop = FALSE]
}

.ctf_campos <- c(
  "estado_resumen_cuantitativo", "minimo_exacto", "maximo_exacto", "mediana",
  "n_ceros", "n_negativos", "n_valores_excluidos_resumen"
)

test_that("la rama de texto saca los centinelas declarados", {
  casos <- list(
    list(v = c("-999", "5", "7", "1152921504606846977", "3", "0"), s = -999,
         esperado = list("omitidos_precision", "0", "1152921504606846977", 5,
                         1L, 0L, 1L)),
    list(v = c("-1", "9", "4", "-1", "9007199254740995", "9", "2"),
         s = c(-1, 9),
         esperado = list("omitidos_precision", "2", "9007199254740995", 4,
                         0L, 0L, 4L)),
    # "-0" es el cero: no es negativo ni el extremo "-0".
    list(v = c("-0", "5", "9007199254740993"), s = NULL,
         esperado = list("omitidos_precision", "0", "9007199254740993", 5,
                         1L, 0L, 0L))
  )
  for (i in seq_along(casos)) {
    fila <- .ctf_fila(casos[[i]]$v, casos[[i]]$s)
    for (j in seq_along(.ctf_campos)) {
      campo <- .ctf_campos[[j]]
      esperado <- casos[[i]]$esperado[[j]]
      observado <- fila[[campo]][[1L]]
      if (is.numeric(esperado)) observado <- as.numeric(observado)
      if (is.integer(esperado)) observado <- as.integer(observado)
      expect_identical(observado, esperado, info = paste(i, campo))
    }
  }
})

test_that("si solo el centinela pasa de 2^53, la columna se resume entera", {
  # 2^60 es un doble exacto: sin el, todo cabe y la columna es la de siempre.
  fila <- .ctf_fila(c("5", "7", "3", "1152921504606846976"), 2^60)
  expect_identical(fila$estado_resumen_cuantitativo, "calculados_sobre_valores")
  expect_identical(as.numeric(fila$media), 5)
  expect_identical(as.numeric(fila$mediana), 5)
  expect_identical(as.numeric(fila$desvio), 2)
  expect_identical(as.integer(fila$n_valores_excluidos_resumen), 1L)
  todos <- .ctf_fila(c("1152921504606846976", "1152921504606846976"), 2^60)
  expect_identical(todos$estado_resumen_cuantitativo, "sin_valores_utilizables")
  expect_identical(as.integer(todos$n_valores_excluidos_resumen), 2L)
})

test_that("el texto publica lo mismo que integer64 con los mismos centinelas", {
  skip_if_not_installed("bit64")
  casos <- list(
    list(v = c("-999", "5", "7", "1152921504606846977", "3", "0"), s = -999),
    list(v = c("5", "7", "3", "1152921504606846976"), s = 2^60),
    list(v = c("1152921504606846976", "1152921504606846976"), s = 2^60),
    list(v = c("-0", "5", "9007199254740993"), s = NULL),
    list(v = c("-1", "9", "4", "-1", "9007199254740995", "9", "2"),
         s = c(-1, 9))
  )
  for (i in seq_along(casos)) {
    texto <- .ctf_fila(casos[[i]]$v, casos[[i]]$s)
    entero <- .ctf_fila(bit64::as.integer64(casos[[i]]$v), casos[[i]]$s)
    for (campo in c("estado_resumen_cuantitativo", "mediana", "media",
                    "desvio", "n_ceros", "n_negativos",
                    "n_valores_excluidos_resumen")) {
      expect_identical(as.character(texto[[campo]]),
                       as.character(entero[[campo]]), info = paste(i, campo))
    }
    if (identical(texto$estado_resumen_cuantitativo, "omitidos_precision")) {
      expect_identical(texto$minimo_exacto, entero$minimo_exacto,
                       info = paste(i))
      expect_identical(texto$maximo_exacto, entero$maximo_exacto,
                       info = paste(i))
    }
  }
})

test_that("el centinela escrito como texto se reconoce aunque la lectura no sea exacta", {
  # En macOS sobre arm64 `as.numeric("1152921504606846976")` da 2^60 - 128: R
  # acumula las cifras en un `long double` que alli es un doble. La lectura se
  # imita corriendo un ulp hacia abajo los valores de mas de 2^53, y el
  # centinela tiene que salir igual que donde la lectura es exacta.
  original <- lupa:::.leer_numeros_texto
  local_mocked_bindings(.leer_numeros_texto = function(texto, convencion = NULL) {
    valores <- original(texto, convencion)
    grandes <- !is.na(valores) & abs(valores) > 2^53
    valores[grandes] <- valores[grandes] * (1 - 2^-53)
    valores
  })
  fila <- .ctf_fila(c("5", "7", "3", "1152921504606846976"), 2^60)
  expect_identical(fila$estado_resumen_cuantitativo, "calculados_sobre_valores")
  expect_identical(as.numeric(fila$media), 5)
  expect_identical(as.numeric(fila$desvio), 2)
  expect_identical(as.integer(fila$n_valores_excluidos_resumen), 1L)
  todos <- .ctf_fila(c("1152921504606846976", "+01152921504606846976"), 2^60)
  expect_identical(todos$estado_resumen_cuantitativo, "sin_valores_utilizables")
  expect_identical(as.integer(todos$n_valores_excluidos_resumen), 2L)
})

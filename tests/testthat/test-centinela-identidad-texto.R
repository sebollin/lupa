# Un centinela declarado se reconoce en una columna de texto por el numero que
# escribe la celda, no por el doble que quedo al leerla. R lee el texto con
# `R_strtod()`, que en macOS sobre arm64 acumula en un doble: `"1152921504606846976"`
# (2^60) llega a 2^60 - 128. El resumen reconocia solo la forma entera sin nada
# mas; `.0`, la cientifica, los miles y la coma decimal dejaban el centinela
# adentro de la media, y el detector de faltantes disfrazados y el plan, que
# comparaban por valor, no lo veian ni en la forma entera. Las pruebas imitan
# esa lectura corriendo un ulp lo que leen los lectores de texto del paquete;
# la comparacion exacta no lee el texto como doble.
#
# La regla: el decimal escrito es exactamente el doble declarado, o tiene a lo
# sumo 17 cifras significativas y R lo lee como el centinela. Un texto con mas
# cifras de las que distingue un doble y que no es exactamente el centinela no
# lo es: `"1152921504606846976.4"` caia en el doble de 2^60 y se sacaba.
testthat::skip_on_cran()

.cit_fila <- function(valores, sentinelas) {
  datos <- data.frame(v = valores, stringsAsFactors = FALSE)
  perfil <- suppressWarnings(suppressMessages(perfilar(
    datos, sentinelas_numericos = sentinelas,
    analizar_dependencias = FALSE
  )))
  perfil$columnas[perfil$columnas$columna == "v", , drop = FALSE]
}

.cit_imitar_lectura_arm64 <- function() {
  original <- lupa:::.leer_numeros_texto
  testthat::local_mocked_bindings(
    .leer_numeros_texto = function(texto, convencion = NULL) {
      valores <- original(texto, convencion)
      grandes <- !is.na(valores) & abs(valores) > 2^53
      valores[grandes] <- valores[grandes] * (1 - 2^-53)
      valores
    },
    .leer_numeros_texto_centinela = function(texto, convencion = NULL) {
      valores <- if (length(convencion) == 1L && !is.na(convencion) &&
                     nzchar(as.character(convencion))) {
        original(texto, as.character(convencion))
      } else {
        suppressWarnings(as.numeric(texto))
      }
      grandes <- !is.na(valores) & abs(valores) > 2^53
      valores[grandes] <- valores[grandes] * (1 - 2^-53)
      valores
    },
    .env = parent.frame()
  )
}

testthat::test_that("las formas regionales del centinela son una sola ausencia", {
  .cit_imitar_lectura_arm64()
  casos <- list(
    list(v = c("5", "7", "3", "1152921504606846976.0"), s = 2^60),
    list(v = c("5", "7", "3", "1.152921504606846976e18"), s = 2^60),
    list(v = c("5", "7", "3", "1152921504606846976,0"), s = 2^60),
    list(v = c("5", "7", "3", "1.152.921.504.606.846.976,0"), s = 2^60)
  )
  for (caso in casos) {
    fila <- .cit_fila(caso$v, caso$s)
    testthat::expect_identical(
      fila$estado_resumen_cuantitativo[[1L]], "calculados_sobre_valores"
    )
    testthat::expect_identical(as.numeric(fila$media[[1L]]), 5)
    testthat::expect_identical(as.integer(
      fila$n_valores_excluidos_resumen[[1L]]
    ), 1L)
    testthat::expect_identical(as.integer(
      fila$n_faltantes_disfrazados_numericos[[1L]]
    ), 1L)
  }
})

testthat::test_that("el detector y el plan comparten la identidad textual", {
  datos <- data.frame(
    v = c("5", "7", "3", "1152921504606846976"),
    stringsAsFactors = FALSE
  )
  perfil_exacto <- suppressWarnings(suppressMessages(perfilar(
    datos, sentinelas_numericos = 2^60,
    analizar_dependencias = FALSE
  )))
  plan <- planificar_limpieza(perfil_exacto, datos)
  .cit_imitar_lectura_arm64()
  perfil <- suppressWarnings(suppressMessages(perfilar(
    datos, sentinelas_numericos = 2^60,
    analizar_dependencias = FALSE
  )))
  fila <- perfil$columnas[perfil$columnas$columna == "v", , drop = FALSE]
  testthat::expect_identical(as.integer(
    fila$n_faltantes_disfrazados_numericos[[1L]]
  ), 1L)
  indice <- which(plan$estrategia == "convertir_sentinelas_numericos")
  testthat::expect_length(indice, 1L)
  testthat::expect_identical(as.numeric(plan$n_afectadas[[indice]]), 1)
  plan$aplicar[[indice]] <- TRUE
  resultado <- aplicar(plan, datos)
  testthat::expect_true(is.na(resultado$datos$v[[4L]]))
  testthat::expect_identical(as.numeric(
    resultado$registro$n_cambiadas[
      resultado$registro$estrategia == "convertir_sentinelas_numericos"
    ]
  ), 1)
})

testthat::test_that("la regla exacta evita confundir un decimal cercano", {
  exacto <- "0.1000000000000000055511151231257827021181583404541015625"
  vecino <- paste0(exacto, "1")
  fila <- .cit_fila(c("5", "7", "3", exacto, vecino), 0.1)
  testthat::expect_identical(as.integer(
    fila$n_valores_excluidos_resumen[[1L]]
  ), 1L)
  testthat::expect_identical(as.integer(
    fila$n_faltantes_disfrazados_numericos[[1L]]
  ), 1L)
  testthat::expect_identical(as.numeric(fila$media[[1L]]), 3.775)
  plan <- planificar_limpieza(
    suppressWarnings(suppressMessages(perfilar(
      data.frame(v = c("5", "7", "3", vecino), stringsAsFactors = FALSE),
      sentinelas_numericos = 0.1, analizar_dependencias = FALSE
    ))),
    data.frame(v = c("5", "7", "3", vecino), stringsAsFactors = FALSE)
  )
  testthat::expect_false(any(
    plan$estrategia == "convertir_sentinelas_numericos"
  ))
})

testthat::test_that("un vecino entero no es el centinela", {
  valores <- c("5", "7", "3", "1152921504606847040")
  fila <- .cit_fila(valores, 2^60)
  testthat::expect_identical(
    fila$estado_resumen_cuantitativo[[1L]], "omitidos_precision"
  )
  testthat::expect_identical(as.integer(
    fila$n_valores_excluidos_resumen[[1L]]
  ), 0L)
  testthat::expect_identical(as.integer(
    fila$n_faltantes_disfrazados_numericos[[1L]]
  ), 0L)
})

testthat::test_that("los literales decimales cortos conservan su regla", {
  .cit_imitar_lectura_arm64()
  fila <- .cit_fila(c("5", "7", "3", "-99.9"), -99.9)
  testthat::expect_identical(as.numeric(fila$media[[1L]]), 5)
  testthat::expect_identical(as.integer(
    fila$n_valores_excluidos_resumen[[1L]]
  ), 1L)
  testthat::expect_identical(as.integer(
    fila$n_faltantes_disfrazados_numericos[[1L]]
  ), 1L)
  sentinela_exponente <- 1e200 * (1 - 2^-53)
  fila_exponente <- .cit_fila(
    c("5", "7", "3", "1e200"), sentinela_exponente
  )
  testthat::expect_identical(as.numeric(fila_exponente$media[[1L]]), 5)
  testthat::expect_identical(as.integer(
    fila_exponente$n_valores_excluidos_resumen[[1L]]
  ), 1L)
  testthat::expect_identical(as.integer(
    fila_exponente$n_faltantes_disfrazados_numericos[[1L]]
  ), 1L)
})

# `n_niveles_observados` es un hecho de la columna: la misma tabla no puede
# publicar dos cifras segun se pase o no el perfil.

test_that("los niveles observados se cuentan en la columna entera", {
  d <- data.frame(
    categoria = sprintf("c%04d", rep(seq_len(1000L), 2L)),
    estado = rep(c("alta", "baja"), 1000L),
    stringsAsFactors = FALSE
  )
  sin_perfil <- clasificar_variables(d, muestra = 100)
  con_perfil <- clasificar_variables(d, perfil = perfilar(d), muestra = 100)
  expect_equal(sin_perfil$n_niveles_observados[[1L]], 1000L)
  expect_equal(sin_perfil$n_niveles_observados,
               con_perfil$n_niveles_observados)
  # La lista sale de la muestra y lo declara.
  expect_true(sin_perfil$niveles_muestreados[[1L]])
  expect_lte(length(sin_perfil$niveles_observados[[1L]]), 100L)
})

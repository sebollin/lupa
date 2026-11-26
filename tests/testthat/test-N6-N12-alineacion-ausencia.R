# N6: una `aplicabilidad` sin nombre se rechaza igual en la medicion en memoria
# y en la de bases de datos, antes de leer. N12: en dependencias, un centinela
# redondo declarado sobre `integer64` se enmascara como sobre un doble.

test_that("medir_dbi() rechaza una aplicabilidad sin nombre como medir()", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  mod <- modelo(instanciar(
    especializar(metricas_nucleo()$NoNulo, "NN", valores_nulos = "n/a"), "d", "x"
  ))
  d <- data.frame(g = c(1, 1, 1, 2), x = c("n/a", "a", "b", "n/a"),
                  stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", d)
  msg <- "lista con nombre por columna"
  expect_error(medir(mod, list(d = d), aplicabilidad = list(~ g == 1)), msg)
  expect_error(medir_dbi(mod, con, aplicabilidad = list(~ g == 1)), msg)
  expect_error(plan_medicion_dbi(mod, con, aplicabilidad = list(~ g == 1)), msg)
  # Con nombre sigue midiendo el universo aplicable: 2 de 3.
  r <- as.data.frame(medir_dbi(mod, con, aplicabilidad = list(x = ~ g == 1)))
  expect_equal(c(r$numerador, r$denominador), c(2, 3))
})

test_that("dependencias enmascara un centinela redondo sobre integer64", {
  skip_on_cran()
  skip_if_not_installed("bit64")
  base_x <- rep(1:10, each = 4)
  x <- c(base_x, rep(100000, 6))
  mk <- function(xx) data.frame(
    x = xx, y = c(paste0("v", base_x), c("p", "q", "r", "s", "p", "q")),
    z = c(base_x * 7, rep(3, 6)), stringsAsFactors = FALSE
  )
  dep <- function(d) {
    r <- as.data.frame(perfilar(
      d, fecha = as.Date("2026-01-01"), analizar_dependencias = TRUE,
      sentinelas_numericos = 100000
    )$dependencias)
    r <- r[order(r$determinante, r$dependiente), ]
    rownames(r) <- NULL
    r[, c("determinante", "dependiente", "cumplimiento", "n_evaluados")]
  }
  expect_equal(dep(mk(bit64::as.integer64(x))), dep(mk(as.numeric(x))))
})

# El plan ofrece el recorte de Tukey tambien en una columna con `escala_mixta`
# (como ofrece en cualquier columna con outliers), sin recomendarlo ni
# aplicarlo, y su justificacion dice por que hay que pensarlo antes.

.datos_escala_plan <- function(n = 400L) {
  set.seed(14)
  altura <- round(rnorm(n, 170, 8))
  altura[1:25] <- round(altura[1:25] / 100, 2)
  data.frame(altura = altura)
}

test_that("el plan ofrece winsorizar_outliers en una columna con escala_mixta", {
  skip_on_cran()
  datos <- .datos_escala_plan()
  perfil <- perfilar(datos, nombre = "x")
  expect_true("escala_mixta" %in% as.character(perfil$hallazgos$tipo_hallazgo))
  plan <- planificar_limpieza(perfil)
  winsor <- plan[
    which(plan$columna == "altura" & plan$estrategia == "winsorizar_outliers"),
    , drop = FALSE
  ]
  expect_equal(nrow(winsor), 1L)
  expect_false(winsor$recomendada[[1L]])
  expect_false(winsor$aplicar[[1L]])
  expect_match(winsor$justificacion[[1L]], "mezclar escalas")
  expect_match(winsor$justificacion[[1L]], "antes de reescalar")
})

test_that("sin escala_mixta la justificacion del recorte no cambia", {
  skip_on_cran()
  set.seed(3)
  datos <- data.frame(x = c(rnorm(300, 50, 5), 120, 130, -40))
  plan <- planificar_limpieza(perfilar(datos, nombre = "x"))
  winsor <- plan[which(plan$estrategia == "winsorizar_outliers"), , drop = FALSE]
  expect_equal(nrow(winsor), 1L)
  expect_false(grepl("mezclar escalas", winsor$justificacion[[1L]]))
})

# La muestra sistematica se alinea con un panel cuando su paso cae justo en el
# periodo: toma siempre los mismos meses y una relacion que la tabla contradice
# sale exacta. Lo que la muestra encuentra se recalcula sobre la tabla.

panel_alineado <- function() {
  set.seed(2582)
  entidades <- 500L
  meses <- 12L
  entidad <- rep(sprintf("E%04d", seq_len(entidades)), each = meses)
  mes <- rep(seq_len(meses), entidades)
  categoria <- rep(sample(c("A", "B", "C", "D"), entidades, TRUE), each = meses)
  # Una correccion en los meses 3 y 4 de 25 entidades.
  corregidas <- rep(seq_len(entidades) %in% sample(entidades, 25L), each = meses)
  categoria[corregidas & mes %in% 3:4] <- "Z"
  d <- data.frame(entidad = entidad, categoria = categoria,
                  stringsAsFactors = FALSE)
  # Una fila mas: 6001 filas y una muestra de 1001 dan un paso de 6 exacto.
  rbind(d, data.frame(entidad = "E0501", categoria = "A"))
}

test_that("una dependencia que la tabla contradice no sale exacta por la muestra", {
  d <- panel_alineado()
  # La premisa: la muestra toma solo los meses 1 y 7.
  indices <- unique(as.integer(round(seq.int(1, nrow(d), length.out = 1001))))
  expect_setequal(unique((indices - 1L) %% 12L + 1L), c(1L, 7L))
  dep <- detectar_dependencias(d, muestra = 1001)
  expect_true(attr(dep, "muestreado"))
  # A mano: 50 filas contradicen la categoria de su entidad, 0,9917 < 0,995.
  completa <- detectar_dependencias(d, muestra = Inf)
  expect_false(any(completa$determinante == "entidad"))
  expect_false(any(dep$determinante == "entidad" & dep$dependiente == "categoria"))
  expect_equal(attr(dep, "n_descartadas_tabla_completa"), 1L)
})

test_that("lo publicado con muestra lleva las cifras de la tabla completa", {
  d <- panel_alineado()
  d$region <- ifelse(d$categoria %in% c("A", "B"), "norte", "sur")
  d$region[d$categoria == "Z"] <- "sur"
  d$region[c(5L, 6L)] <- "oeste"
  dep <- detectar_dependencias(d, muestra = 1001)
  fila <- dep[dep$determinante == "categoria" & dep$dependiente == "region", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_evaluados, nrow(d))
  expect_equal(fila$n_violaciones, 2L)
  expect_false(fila$exacta)
  expect_true(all(attr(dep, "verificadas_tabla_completa")))
  # Sin muestra el objeto no lleva los atributos de la verificacion.
  expect_null(attr(detectar_dependencias(d, muestra = Inf),
                   "verificadas_tabla_completa"))
})

test_that("lo que no entra en el presupuesto conserva la muestra y se declara", {
  d <- panel_alineado()
  d$region <- ifelse(d$categoria %in% c("A", "B"), "norte", "sur")
  dep <- detectar_dependencias(d, muestra = 1001, max_trabajo = nrow(d) - 1)
  expect_true(nrow(dep) > 0L)
  expect_false(any(attr(dep, "verificadas_tabla_completa")))
  cobertura <- lupa:::.cobertura_dependencias(dep)
  expect_match(cobertura$motivo, "conserva", fixed = TRUE)
  expect_match(cobertura$motivo, "verificadas_tabla_completa", fixed = TRUE)
})

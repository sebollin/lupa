# Una muestra solo ve las filas que toma: si las que contradicen una relacion
# caen todas fuera de ella, la muestra la ve exacta. Lo que la muestra
# encuentra se recalcula sobre la tabla. (Una muestra a paso fijo lo hacia con
# datos naturales: sobre un panel mensual cuyo tamano hacia caer el paso en el
# periodo tomaba siempre los mismos meses.)

panel_alineado <- function() {
  set.seed(2582)
  entidades <- 50L
  meses <- 120L
  entidad <- rep(sprintf("E%04d", seq_len(entidades)), each = meses)
  mes <- rep(seq_len(meses), entidades)
  categoria <- rep(sample(c("A", "B", "C", "D"), entidades, TRUE), each = meses)
  d <- data.frame(entidad = entidad, categoria = categoria,
                  stringsAsFactors = FALSE)
  d <- rbind(d, data.frame(entidad = "E0051", categoria = "A"))
  # Cincuenta filas de 25 entidades, elegidas para quedar fuera de las 100
  # prioridades menores de la tabla nueva. La relacion completa queda por
  # debajo de 0,995 aunque la muestra la vea exacta.
  corregidas <- c(
    3420, 3405, 5912, 5989, 1931, 1930, 4091, 4151, 1548, 1471,
    1183, 1107, 2876, 2822, 1208, 1246, 291, 273, 4066, 3997,
    1920, 1813, 4211, 4249, 5748, 5658, 2522, 2535, 3664, 3684,
    4817, 4833, 187, 172, 1015, 1011, 467, 437, 3204, 3240,
    2699, 2653, 4385, 4345, 2474, 2426, 3766, 3826, 4464, 4467
  )
  d$categoria[corregidas] <- "Z"
  d
}

.muestra_panel <- 100L

test_that("una dependencia que la tabla contradice no sale exacta por la muestra", {
  d <- panel_alineado()
  # La premisa: la muestra por huella no toma ninguna de las filas que la
  # contradicen.
  indices <- lupa:::.indices_huella_tabla(d, .muestra_panel)$indices
  expect_false(any(d$categoria[indices] == "Z"))
  expect_equal(sum(d$categoria == "Z"), 50L)
  dep <- detectar_dependencias(d, muestra = .muestra_panel)
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
  dep <- detectar_dependencias(d, muestra = .muestra_panel)
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
  dep <- detectar_dependencias(
    d, muestra = .muestra_panel, max_trabajo = nrow(d) - 1
  )
  expect_true(nrow(dep) > 0L)
  expect_false(any(attr(dep, "verificadas_tabla_completa")))
  cobertura <- lupa:::.cobertura_dependencias(dep)
  expect_match(cobertura$motivo, "conserva", fixed = TRUE)
  expect_match(cobertura$motivo, "verificadas_tabla_completa", fixed = TRUE)
})

# El recorte de `max_resultados` ordena por distancia y desempata por posicion de
# fila. Cuando el corte cae dentro de una banda de distancias empatadas, cuales
# pares sobreviven depende del orden en que llegaron las filas y no de los datos.
# Medido: sobre 60 pares empatados y un corte en 30, dos ordenes distintos no
# comparten ni un solo grupo. El alcance tiene que decirlo, porque `truncado`
# solo se lee como "conserve los mas cercanos".

.tabla_con_empates <- function(g = 60L) {
  set.seed(7)
  pref <- unique(vapply(
    seq_len(g), function(i) paste(sample(LETTERS, 8L, TRUE), collapse = ""),
    character(1L)
  ))
  data.frame(
    nombre = as.vector(rbind(
      paste0(pref, " SOCIEDAD ANONIMA"), paste0(pref, " SOCIEDAD ANONMA")
    )),
    grupo = as.vector(rbind(seq_along(pref), seq_along(pref))),
    stringsAsFactors = FALSE
  )
}

test_that("el recorte dentro de un empate depende del orden, y queda declarado", {
  d0 <- .tabla_con_empates()
  todos <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = Inf
  )
  # La banda de empate existe: si el generador cambiara y dejara de haberla,
  # esta prueba no estaria midiendo lo que dice medir.
  banda <- max(table(round(todos$pares$distancia, 10L)))
  expect_gte(banda, 40L)

  grupos_de <- function(orden, limite) {
    d <- d0[orden, , drop = FALSE]
    rownames(d) <- NULL
    r <- detectar_duplicados_aproximados(
      d["nombre"], umbral = 0.10, max_resultados = limite
    )
    sort(unique(d$grupo[c(r$pares$fila_1, r$pares$fila_2)]))
  }
  natural <- seq_len(nrow(d0))
  g_nat <- grupos_de(natural, 30L)
  g_inv <- grupos_de(rev(natural), 30L)

  # El defecto, escrito: mismo dato, mismo limite, conjuntos disjuntos.
  expect_equal(length(g_nat), length(g_inv))
  expect_false(identical(g_nat, g_inv))
  expect_length(intersect(g_nat, g_inv), 0L)

  # Y el alcance lo declara en vez de callarlo.
  a <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = 30L
  )$alcance
  expect_true(a$truncado)
  expect_true(a$recorte_depende_del_orden)
  expect_equal(a$n_en_distancia_corte, 30L)
  expect_equal(a$distancia_corte, max(todos$pares$distancia[
    order(todos$pares$distancia)][seq_len(30L)]))
})

test_that("sin recorte no se declara dependencia del orden", {
  d0 <- .tabla_con_empates()
  a <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = 1000L
  )$alcance
  expect_false(a$truncado)
  expect_true(is.na(a$distancia_corte))
  expect_true(is.na(a$n_en_distancia_corte))
  expect_false(a$recorte_depende_del_orden)
})

test_that("truncado con un borde unico NO declara dependencia del orden", {
  # El control que hace valer la senal: si diera TRUE siempre que hay recorte,
  # seria `truncado` con otro nombre y no informaria nada nuevo.
  set.seed(1)
  d <- data.frame(
    nombre = replicate(7L, paste(
      sample(c(LETTERS, letters), sample(6:14, 1L), TRUE), collapse = ""
    )),
    stringsAsFactors = FALSE
  )
  todos <- detectar_duplicados_aproximados(d, umbral = 0.95, max_resultados = Inf)
  distancias <- sort(todos$pares$distancia)
  expect_equal(sum(distancias == distancias[[2L]]), 1L)

  a <- detectar_duplicados_aproximados(d, umbral = 0.95, max_resultados = 2L)$alcance
  expect_true(a$truncado)
  expect_equal(a$n_en_distancia_corte, 1L)
  expect_false(a$recorte_depende_del_orden)
})

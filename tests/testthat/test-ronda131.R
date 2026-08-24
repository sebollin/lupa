# El recorte de `max_resultados` ordena por distancia. Entre pares empatados
# desempataba por POSICION DE FILA, y eso hacia que cuales sobrevivieran
# dependiera del orden de llegada y no de los datos: medido sobre 60 pares
# empatados con el corte en 30, cinco ordenes distintos devolvian 30 grupos cada
# uno y no compartian NINGUNO.
#
# Ahora desempata por el rango canonico del valor -la misma decision que ya
# gobierna el recorte del vocabulario, donde tomar las formas en orden de llegada
# daba 26 grupos y en orden alfabetico 148-. La clave es simetrica, `min` y `max`
# del rango, para que tampoco dependa de cual fila quedo primera dentro del par.
#
# Lo que el recorte sigue sin poder evitar es dejar afuera pares igual de
# cercanos cuando el corte cae dentro de un empate. Eso no se arregla: se
# declara, en `corte_en_empate`.

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

.ordenes_de_prueba <- function(n) {
  ordenes <- list(natural = seq_len(n), inverso = rev(seq_len(n)))
  for (s in 1:3) {
    set.seed(100L + s)
    ordenes[[paste0("barajado", s)]] <- sample(n)
  }
  ordenes
}

test_that("el recorte dentro de un empate no depende del orden de las filas", {
  d0 <- .tabla_con_empates()
  todos <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = Inf
  )
  # La banda de empate tiene que existir: sin ella esta prueba no mide lo que
  # dice medir, porque el desempate nunca entraria en juego.
  expect_gte(max(table(round(todos$pares$distancia, 10L))), 40L)

  grupos_de <- function(orden, limite) {
    d <- d0[orden, , drop = FALSE]
    rownames(d) <- NULL
    r <- detectar_duplicados_aproximados(
      d["nombre"], umbral = 0.10, max_resultados = limite
    )
    sort(unique(d$grupo[c(r$pares$fila_1, r$pares$fila_2)]))
  }
  ordenes <- .ordenes_de_prueba(nrow(d0))
  for (limite in c(30L, 40L)) {
    grupos <- lapply(ordenes, grupos_de, limite = limite)
    expect_equal(length(Reduce(union, grupos)), limite)
    # Union e interseccion iguales: los cinco ordenes ven exactamente lo mismo.
    expect_equal(Reduce(union, grupos), Reduce(intersect, grupos))
  }
})

test_that("dejar pares empatados afuera se declara aunque el corte sea estable", {
  d0 <- .tabla_con_empates()
  a <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = 30L
  )$alcance
  expect_true(a$truncado)
  expect_true(a$corte_en_empate)
  expect_equal(a$n_en_distancia_corte, 30L)
})

test_that("sin recorte no hay corte ni empate que declarar", {
  d0 <- .tabla_con_empates()
  a <- detectar_duplicados_aproximados(
    d0["nombre"], umbral = 0.10, max_resultados = 1000L
  )$alcance
  expect_false(a$truncado)
  expect_true(is.na(a$distancia_corte))
  expect_true(is.na(a$n_en_distancia_corte))
  expect_false(a$corte_en_empate)
})

test_that("truncado con un borde unico NO declara empate en el corte", {
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
  expect_false(a$corte_en_empate)
})

test_that("el rango canonico sale del universo entero y es simetrico", {
  # Un rango recalculado por lote seria local y cambiaria la comparacion entre
  # lotes; y una clave no simetrica dependeria de cual fila quedo primera.
  valores <- c("BETA", "ALFA", "GAMA")
  filas <- c(3L, 7L, 5L)
  rango <- lupa:::.rango_canonico_duplicados(valores, filas)
  expect_length(rango, 7L)
  expect_equal(rango[[7L]], 1L)   # ALFA es la primera del orden canonico
  expect_equal(rango[[3L]], 2L)   # BETA
  expect_equal(rango[[5L]], 3L)   # GAMA

  pares <- data.frame(fila_1 = c(3L, 5L), fila_2 = c(5L, 3L),
                      distancia = c(0.1, 0.1), stringsAsFactors = FALSE)
  salida <- lupa:::.ordenar_pares_con_igualdad(pares, c(FALSE, FALSE), rango)
  # Los dos pares son el mismo par escrito al reves: la clave simetrica los deja
  # empatados entre si y el orden no lo decide cual fila vino primera.
  expect_equal(nrow(salida$pares), 2L)
  expect_equal(salida$pares$distancia, c(0.1, 0.1))
})

test_that("los cuatro caminos de acumulacion resisten la permutacion", {
  # El rango se calcula en CUATRO constructores distintos -bloques, lotes, LSH y
  # la rama con `bloquear_por`-. Probar solo el de por omision dejaria sin cubrir
  # tres, que es donde un rango calculado por lote en vez de por universo
  # completo se notaria.
  skip_if_not_installed("stringdist")
  set.seed(7)
  pref <- unique(vapply(
    seq_len(60L), function(i) paste(sample(LETTERS, 8L, TRUE), collapse = ""),
    character(1L)
  ))
  g <- length(pref)
  anio <- rep(c(2023L, 2024L), length.out = g)   # por GRUPO, no por fila: si
  # alterna por fila, las dos variantes caen en bloques distintos, no se
  # emparejan y el recorte no llega a morder -la prueba pasaria sin probar nada-.
  d0 <- data.frame(
    nombre = as.vector(rbind(
      paste0(pref, " SOCIEDAD ANONIMA"), paste0(pref, " SOCIEDAD ANONMA")
    )),
    grupo = as.vector(rbind(seq_len(g), seq_len(g))),
    anio = as.vector(rbind(anio, anio)),
    stringsAsFactors = FALSE
  )
  ordenes <- .ordenes_de_prueba(nrow(d0))

  caminos <- list(
    bloques = function(d) detectar_duplicados_aproximados(
      d["nombre"], umbral = 0.10, max_resultados = 30L
    ),
    lotes = function(d) detectar_duplicados_aproximados(
      d["nombre"], umbral = 0.10, max_resultados = 30L,
      lotes = TRUE, tamano_lote = 25L
    ),
    lsh = function(d) detectar_duplicados_aproximados(
      d["nombre"], umbral = 0.10, max_resultados = 30L, estrategia = "lsh"
    ),
    bloqueado = function(d) detectar_duplicados_aproximados(
      d[c("nombre", "anio")], columnas = "nombre", umbral = 0.10,
      max_resultados = 30L, bloquear_por = "anio"
    )
  )

  for (nombre_camino in names(caminos)) {
    hacer <- caminos[[nombre_camino]]
    # El recorte tiene que morder de verdad en este camino, si no la prueba
    # pasaria sin ejercitar el desempate.
    sin_tope <- detectar_duplicados_aproximados(
      if (identical(nombre_camino, "bloqueado")) d0[c("nombre", "anio")] else d0["nombre"],
      columnas = if (identical(nombre_camino, "bloqueado")) "nombre" else NULL,
      umbral = 0.10, max_resultados = Inf,
      bloquear_por = if (identical(nombre_camino, "bloqueado")) "anio" else NULL
    )
    expect_gt(nrow(sin_tope$pares), 30L)

    grupos <- lapply(ordenes, function(orden) {
      d <- d0[orden, , drop = FALSE]
      rownames(d) <- NULL
      r <- hacer(d)
      sort(unique(d$grupo[c(r$pares$fila_1, r$pares$fila_2)]))
    })
    expect_equal(
      Reduce(union, grupos), Reduce(intersect, grupos),
      info = paste("camino", nombre_camino)
    )
  }
})

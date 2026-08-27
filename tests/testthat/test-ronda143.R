test_that("la inferencia ponderada por vocabulario conserva cada resultado", {
  acento <- paste0("Jos", "\u00e9")
  chico <- rep(
    c(NA_character_, "", acento, "12", "1,25", "2020-01-01", "12:30"),
    length.out = 240L
  )
  formas_mediano <- c(
    NA_character_, "", acento, "12", "1,25", "2020-01-01", "12:30",
    sprintf("texto-%02d", seq_len(58L))
  )
  mediano <- rep(formas_mediano, length.out = 240L)
  completo <- c(
    NA_character_, "", acento, "12", "1,25", "2020-01-01", "12:30",
    sprintf("COD-%03d", seq_len(233L))
  )
  datos <- data.frame(
    vocabulario_chico = chico,
    vocabulario_mediano = mediano,
    vocabulario_completo = completo,
    stringsAsFactors = FALSE
  )

  esperado_inferencias <- lapply(datos, inferir_tipo)
  esperado_perfil <- perfilar(
    datos, analizar_dependencias = FALSE, duplicados_aproximados = FALSE
  )$columnas

  # Esta guarda desactiva la deduplicacion en todas las llamadas al ayudante;
  # las rondas anteriores ya compararon sus otros atajos por esta misma via.
  local_mocked_bindings(
    .vocabulario_texto = function(textos, umbral, valores = NULL) {
      list(
        valores = textos,
        indices = seq_along(textos),
        usar = FALSE,
        n_distintos = length(unique(textos[!is.na(textos)]))
      )
    },
    .package = "lupa"
  )

  referencia_inferencias <- lapply(datos, inferir_tipo)
  referencia_perfil <- perfilar(
    datos, analizar_dependencias = FALSE, duplicados_aproximados = FALSE
  )$columnas

  for (i in seq_along(datos)) {
    expect_identical(esperado_inferencias[[i]], referencia_inferencias[[i]])
  }
  expect_identical(esperado_perfil, referencia_perfil)
})

# La comprobacion de arriba usa tres columnas de 240 filas. Esta amplia el
# abanico a las formas que de verdad aparecen en una tabla administrativa, y
# recorre tres umbrales, porque el umbral decide que tipo gana y podria
# interactuar con el redondeo de las proporciones ponderadas.

test_that("la ponderacion por vocabulario aguanta el abanico de columnas", {
  set.seed(143L)
  n <- 1200L
  casos <- list(
    repetido = sample(paste0("v", seq_len(30L)), n, TRUE),
    identificadores = sprintf("ID%05d", seq_len(n)),
    enteros = as.character(sample(seq_len(99999L), n, TRUE)),
    dobles = format(stats::rnorm(n), digits = 8L),
    coma_decimal = gsub("\\.", ",", format(stats::rnorm(n), digits = 6L)),
    fechas = as.character(as.Date("2020-01-01") + sample(0:900, n, TRUE)),
    horas = sprintf("%02d:%02d", sample(0:23, n, TRUE), sample(0:59, n, TRUE)),
    logicos = sample(c("si", "no", "SI", "No"), n, TRUE),
    punto_de_miles = sprintf("%d.%03d", sample(1:9, n, TRUE), sample(0:999, n, TRUE)),
    ceros_a_la_izquierda = sprintf("%05d", sample(seq_len(999L), n, TRUE)),
    mezcla = sample(c("1", "2", "x", "2020-05-05", "12:30", NA, ""), n, TRUE),
    un_solo_valor = rep("igual", n),
    casi_todo_ausente = c(rep(NA_character_, n - 3L), "a", "b", "c"),
    vacia = character(0)
  )
  umbrales <- c(0.5, 0.8, 0.95)

  esperado <- lapply(casos, function(v) lapply(umbrales, function(u) inferir_tipo(v, umbral = u)))

  # El ayudante forzado a no deduplicar: es el camino que habia antes.
  local_mocked_bindings(
    .vocabulario_texto = function(textos, umbral, valores = NULL) {
      list(
        valores = textos, indices = seq_along(textos), usar = FALSE,
        n_distintos = length(unique(textos[!is.na(textos)]))
      )
    },
    .package = "lupa"
  )
  referencia <- lapply(casos, function(v) lapply(umbrales, function(u) inferir_tipo(v, umbral = u)))

  for (k in seq_along(casos)) {
    for (j in seq_along(umbrales)) {
      expect_identical(
        esperado[[k]][[j]], referencia[[k]][[j]],
        info = paste(names(casos)[k], "umbral", umbrales[j])
      )
    }
  }
})

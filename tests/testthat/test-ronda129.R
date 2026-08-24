# Dos canales que no tenian donde poner la cobertura de diagnosticos, asi que
# cualquier supresion era invisible desde esas puertas.

test_that("perfilar_por declara por grupo lo que no se evaluo", {
  # Cada grupo se perfila por separado y declina los suyos: una columna puede
  # tener bastantes filas en un grupo y muy pocas en otro. Sin esta tabla, un
  # grupo sin hallazgos se lee como un grupo sano, cuando puede ser un grupo
  # sobre el que no se miro.
  set.seed(7)
  edades <- c(round(stats::rnorm(3000L, 40, 12)), 0:100, rep(110L, 12L))
  edades <- edades[edades >= 0 & edades <= 110]
  datos <- data.frame(
    depto = rep(c("A", "B"), length.out = length(edades)),
    edad = edades, stringsAsFactors = FALSE
  )
  hallazgos <- perfilar_por(
    datos, "depto", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  cobertura <- attr(hallazgos, "cobertura_diagnosticos", exact = TRUE)
  expect_true(inherits(cobertura, "data.frame"))
  expect_gt(nrow(cobertura), 0L)
  # La columna `grupo` es lo que hace util la tabla: sin ella no se sabe sobre
  # cual de los grupos no se miro.
  expect_true("grupo" %in% names(cobertura))
  expect_setequal(unique(as.character(cobertura$grupo)), c("A", "B"))
  expect_true("outliers" %in% as.character(cobertura$diagnostico))
})

test_that("sin nada que declarar la tabla existe y esta vacia", {
  # Que exista siempre importa: quien la lee no tiene que distinguir entre "no
  # hay atributo" y "no hubo nada que declarar".
  datos <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 3, 4),
                      stringsAsFactors = FALSE)
  hallazgos <- perfilar_por(
    datos, "g", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  cobertura <- attr(hallazgos, "cobertura_diagnosticos", exact = TRUE)
  expect_true(inherits(cobertura, "data.frame"))
  expect_true("grupo" %in% names(cobertura))
})

test_that("el plan de limpieza lleva la cobertura del perfil que lo origino", {
  set.seed(7)
  edades <- c(round(stats::rnorm(5000L, 40, 12)), 0:100, rep(110L, 12L))
  edades <- edades[edades >= 0 & edades <= 110]
  datos <- data.frame(edad = edades)
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, casi_duplicados_vocabulario = FALSE
  )
  plan <- planificar_limpieza(perfil, datos)
  cobertura <- attr(plan, "cobertura_diagnosticos", exact = TRUE)
  expect_true(inherits(cobertura, "data.frame"))
  expect_equal(nrow(cobertura), nrow(perfil$cobertura_diagnosticos))
  expect_true("outliers" %in% as.character(cobertura$diagnostico))
})

test_that("la gramatica de entidades HTML es una sola para detectar y reparar", {
  # Estaban escritas por separado en dos archivos, una para detectar y otra para
  # reparar. Si divergian se detectaba lo que no se reparaba, o al reves, y
  # ninguna de las dos avisaba.
  valores <- c("Jos&eacute;", "caf&#233;", "sano", "&#x41;lgo", "a &amp; b")
  reparado <- .decodificar_entidades_html(valores)
  expect_equal(reparado$n, 4L)
  expect_equal(reparado$valor[[3L]], "sano")
  expect_false(grepl("&", reparado$valor[[1L]], fixed = TRUE))
})

# `planificar_limpieza()` declara los hallazgos cuya columna no se puede
# identificar porque su nombre se repite. La pregunta -cuantas columnas del
# perfil se llaman asi- se hacia por hallazgo, normalizando en cada uno todos los
# nombres del perfil: con un perfil guardado por una version anterior, que
# publicaba un hallazgo por par de columnas iguales, 1.000 columnas iguales son
# 499.500 hallazgos, y la pregunta se llevaba 81 de los 87 s del plan. La
# respuesta tiene que ser la misma; el trabajo, lineal en hallazgos y columnas.

.cal_perfil <- function(n_hallazgos) {
  datos <- data.frame(
    a = c(" x ", "Y", "z"), b = c(" p ", "q", "r"), c = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  # Dos columnas con el mismo nombre, una con un nombre no ASCII y otra comun.
  names(datos) <- c("a", "a", "pa\u00eds")
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgos <- perfil$hallazgos
  base <- hallazgos[rep(1L, n_hallazgos), , drop = FALSE]
  # Columnas repetidas, unica, no ASCII, ausente del perfil y faltante.
  base$columna <- rep_len(
    c("a", "pa\u00eds", "no_esta", NA_character_, "a", "pa\u00eds"),
    n_hallazgos
  )
  rownames(base) <- NULL
  perfil$hallazgos <- base
  perfil
}

test_that("la columna ambigua se decide igual que por hallazgo", {
  perfil <- .cal_perfil(60L)
  esperado <- which(!is.na(perfil$hallazgos$columna) & vapply(
    perfil$hallazgos$columna,
    function(columna) lupa:::.accion_columna_ambigua(perfil, columna),
    logical(1L)
  ))
  obtenido <- lupa:::.hallazgos_sin_accion_por_columna_ambigua(perfil)
  expect_identical(
    obtenido$columna, as.character(perfil$hallazgos$columna[esperado])
  )
  expect_identical(
    obtenido$hallazgo, as.character(perfil$hallazgos$tipo_hallazgo[esperado])
  )
  expect_setequal(unique(obtenido$columna), c("a", "no_esta"))
})

test_that("los nombres del perfil se normalizan una vez y no por hallazgo", {
  perfil <- .cal_perfil(400L)
  columnas <- length(perfil$columnas$columna)
  normalizados <- 0
  original <- lupa:::.nombres_para_operar
  testthat::local_mocked_bindings(
    .nombres_para_operar = function(nombres) {
      normalizados <<- normalizados + length(nombres)
      original(nombres)
    },
    .package = "lupa"
  )
  salida <- lupa:::.hallazgos_sin_accion_por_columna_ambigua(perfil)
  expect_gt(nrow(salida), 0L)
  # Por hallazgo eran 400 * (3 + 1) = 1.600 nombres; contados una vez, a lo
  # sumo las columnas mas los hallazgos.
  expect_lte(normalizados, columnas + nrow(perfil$hallazgos))
})

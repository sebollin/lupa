tabla_condicionada <- function(n = 200L, ruido = 0L) {
  tipo <- rep(c("A", "B"), each = n / 2L)
  valor <- ifelse(tipo == "A", seq_len(n) / n, NA_real_)
  if (ruido > 0L) {
    valor[which(tipo == "B")[seq_len(ruido)]] <- 0.5
  }
  data.frame(tipo = tipo, valor = valor, stringsAsFactors = FALSE)
}

test_that("cuando otra columna decide la presencia, se sugiere declararla", {
  p <- perfilar(tabla_condicionada(), analizar_dependencias = FALSE)
  senal <- p$hallazgos[p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural", ]
  expect_equal(nrow(senal), 1L)
  expect_equal(as.character(senal$severidad), "ok")
  expect_equal(senal$columna, "valor")
  expect_match(senal$evidencia, "`tipo` predice la presencia de `valor`")
  expect_match(senal$sugerencia, 'aplicabilidad = list(valor = ~ tipo == "A")',
               fixed = TRUE)
})

test_that("la formula que sugiere es la que hace falta escribir", {
  datos <- tabla_condicionada()
  sin <- perfilar(datos, analizar_dependencias = FALSE)
  senal <- sin$hallazgos[
    sin$hallazgos$tipo_hallazgo == "posible_ausencia_estructural",
  ]
  # El texto sugerido se ejecuta tal cual: si la sugerencia no sirve pegada,
  # no sirve.
  formula <- eval(parse(text = sub(
    ".*aplicabilidad = list\\(valor = (~ [^)]+)\\)\\).*", "\\1", senal$sugerencia
  )))
  con <- perfilar(
    datos, aplicabilidad = list(valor = formula), analizar_dependencias = FALSE
  )
  expect_true(any(sin$hallazgos$tipo_hallazgo == "faltantes"))
  expect_false(any(con$hallazgos$tipo_hallazgo == "faltantes"))
  expect_false(any(con$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
})

test_that("no afirma la regla cuando la regla no se cumple", {
  # Con 10 % de valores fuera del patron la relacion existe y no es una regla.
  p <- perfilar(tabla_condicionada(ruido = 10L), analizar_dependencias = FALSE)
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
})

test_that("dos columnas que se reparten las filas se informan sin determinante", {
  n <- 200L
  datos <- data.frame(
    a = c(seq_len(n / 2L), rep(NA_integer_, n / 2L)),
    b = c(rep(NA_integer_, n / 2L), seq_len(n / 2L))
  )
  p <- perfilar(datos, analizar_dependencias = FALSE)
  senal <- p$hallazgos[p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural", ]
  expect_equal(nrow(senal), 2L)
  expect_true(all(as.character(senal$severidad) == "ok"))
  expect_match(senal$evidencia[[1L]], "se pisan en 0 filas")
  expect_match(senal$sugerencia[[1L]], "columnas_opcionales", fixed = TRUE)
})

test_that("lo ya declarado no se vuelve a sugerir", {
  p <- perfilar(
    tabla_condicionada(), aplicabilidad = list(valor = ~ tipo == "A"),
    analizar_dependencias = FALSE
  )
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
})

test_that("una tabla sana no recibe la senal", {
  set.seed(11)
  for (conjunto in list(datasets::airquality, datasets::iris,
                        datasets::ChickWeight, datasets::esoph)) {
    p <- perfilar(as.data.frame(conjunto), analizar_dependencias = FALSE)
    expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
  }
})

test_that("con pocas filas no se busca, y se dice", {
  # Veinte filas: hay ausencia de sobra para que la columna sea candidata, y
  # aun asi no alcanzan las treinta que el patron necesita para significar algo.
  datos <- data.frame(
    tipo = rep(c("A", "B"), 10L),
    a = ifelse(rep(c(TRUE, FALSE), 10L), seq_len(20L), NA_integer_)
  )
  p <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
  cobertura <- p$cobertura_diagnosticos
  fila <- cobertura[cobertura$diagnostico == "posible_ausencia_estructural", ]
  expect_equal(nrow(fila), 1L)
  expect_match(fila$motivo, "se necesitan al menos")
})

test_that("la senal se puede apagar", {
  p <- perfilar(tabla_condicionada(), ausencia_estructural = FALSE,
                analizar_dependencias = FALSE)
  expect_false(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
  expect_false(any(
    p$cobertura_diagnosticos$diagnostico == "posible_ausencia_estructural"
  ))
  expect_error(
    perfilar(tabla_condicionada(), ausencia_estructural = NA),
    "`ausencia_estructural` debe ser TRUE o FALSE"
  )
})

test_that("el vacio disfrazado cuenta igual que el vacio", {
  n <- 200L
  tipo <- rep(c("A", "B"), each = n / 2L)
  datos <- data.frame(
    tipo = tipo,
    valor = ifelse(tipo == "A", "dato", "s/d"),
    stringsAsFactors = FALSE
  )
  p <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(p$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
})

test_that("avisa cuando una columna declarada opcional queda casi vacia", {
  datos <- data.frame(
    x = c(rep(NA_character_, 160L), rep(letters[1:20], 2L)),
    stringsAsFactors = FALSE
  )
  p <- perfilar(datos, columnas_opcionales = "x", analizar_dependencias = FALSE)
  aviso <- p$hallazgos[p$hallazgos$tipo_hallazgo == "regla_silencia_ausencia", ]
  expect_equal(nrow(aviso), 1L)
  expect_equal(as.character(aviso$severidad), "ok")
  expect_equal(aviso$n_afectados, 160)
  expect_equal(aviso$n_evaluados, 200)
  expect_match(aviso$evidencia, "columnas_opcionales", fixed = TRUE)
  # Sin declarar no hay aviso: lo que se informa es el defecto.
  q <- perfilar(datos, analizar_dependencias = FALSE)
  expect_false(any(q$hallazgos$tipo_hallazgo == "regla_silencia_ausencia"))
  expect_true(any(q$hallazgos$tipo_hallazgo == "faltantes"))
})

test_that("avisa cuando la regla declarada deja la columna casi vacia adentro", {
  n <- 1000L
  tipo <- rep(c("A", "B"), each = n / 2L)
  valor <- ifelse(tipo == "A", seq_len(n) / n, NA_real_)
  valor[which(tipo == "A")[seq_len(200L)]] <- NA_real_
  datos <- data.frame(tipo = tipo, valor = valor, stringsAsFactors = FALSE)
  p <- perfilar(datos, aplicabilidad = list(valor = ~ tipo == "A"),
                analizar_dependencias = FALSE)
  aviso <- p$hallazgos[p$hallazgos$tipo_hallazgo == "regla_silencia_ausencia", ]
  expect_equal(nrow(aviso), 1L)
  expect_equal(aviso$n_afectados, 200)
  expect_equal(aviso$n_evaluados, 500)
  expect_match(aviso$evidencia, "universo declarado")
})

test_that("el universo declarado llega al plan de limpieza", {
  n <- 1000L
  tipo <- rep(c("A", "B"), each = n / 2L)
  valor <- ifelse(tipo == "A", seq_len(n) / n, NA_real_)
  valor[which(tipo == "A")[seq_len(200L)]] <- NA_real_
  datos <- data.frame(tipo = tipo, valor = valor, stringsAsFactors = FALSE)
  sin <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE))
  con <- planificar_limpieza(perfilar(
    datos, aplicabilidad = list(valor = ~ tipo == "A"),
    analizar_dependencias = FALSE
  ))
  afectadas <- function(plan) {
    unique(plan$n_afectadas[plan$columna == "valor" &
                              plan$hallazgo == "faltantes"])
  }
  expect_equal(afectadas(sin), 700)
  expect_equal(afectadas(con), 200)
})

# comparar_perfiles(): el vocabulario se gradua sobre todas las filas, sin
# contar lo que puede faltar por azar ni lo que solo cambio de forma; las
# marcas del plan se reconocen por lo que deja aplicar() y no por el nombre; y
# la proteccion no deja salir por otra columna un valor que tapa.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.cva_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cva_febrero <- as.POSIXct("2026-02-01", tz = "UTC")

.cva_perfil <- function(datos, fecha = .cva_enero, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE, ...
  )))
}

.cva_comparar <- function(...) {
  as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(...))))
}

.minusculas_ascii_cva <- function(x) {
  chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
}

.cva_vocabulario <- function(anterior, actual) {
  deriva <- .cva_comparar(.cva_perfil(anterior), .cva_perfil(actual, .cva_febrero))
  deriva[deriva$aspecto == "vocabulario", , drop = FALSE]
}

test_that("el delta del vocabulario es sobre todas las filas, con sus faltantes", {
  una <- .cva_vocabulario(
    data.frame(x = c("A", rep(NA_character_, 999)), stringsAsFactors = FALSE),
    data.frame(x = c("B", rep(NA_character_, 999)), stringsAsFactors = FALSE)
  )
  expect_equal(nrow(una), 1L)
  expect_equal(una$delta, 1 / 1000)
  expect_identical(as.character(una$severidad), "ok")
  diez <- .cva_vocabulario(
    data.frame(x = c(rep("A", 50), rep(NA_character_, 950)), stringsAsFactors = FALSE),
    data.frame(x = c(rep("A", 50), rep("B", 10), rep(NA_character_, 940)),
               stringsAsFactors = FALSE)
  )
  expect_equal(diez$delta, 10 / 1000)
  expect_identical(as.character(diez$severidad), "ok")
  # Sin faltantes, la cifra es la de siempre.
  control <- .cva_vocabulario(
    data.frame(x = rep("A", 1000), stringsAsFactors = FALSE),
    data.frame(x = c(rep("A", 900), rep("B", 100)), stringsAsFactors = FALSE)
  )
  expect_equal(control$delta, 0.1)
  expect_identical(as.character(control$severidad), "sospechoso")
})

test_that("un valor raro que falta en una entrega chica no da alarma", {
  peso <- c(A = 0.40, B = 0.30, C = 0.25, R = 0.05)
  alarmas <- 0L
  for (r in 1:40) {
    set.seed(5000L + r)
    chica <- data.frame(x = sample(names(peso), 30L, TRUE, peso), stringsAsFactors = FALSE)
    grande <- data.frame(x = sample(names(peso), 300L, TRUE, peso), stringsAsFactors = FALSE)
    fila <- .cva_vocabulario(chica, grande)
    alarmas <- alarmas + sum(as.character(fila$severidad) %in% c("sospechoso", "error"))
  }
  expect_identical(alarmas, 0L)
  # Con dos entregas chicas del mismo tamano, un valor que salio 5 veces de 40
  # en una y ninguna en la otra puede ser azar: las 5 caen del mismo lado con
  # probabilidad 1/32.
  rara <- .cva_vocabulario(
    data.frame(x = rep(c("A", "B", "C"), c(15, 13, 12)), stringsAsFactors = FALSE),
    data.frame(x = rep(c("A", "B", "C", "E"), c(13, 12, 10, 5)),
               stringsAsFactors = FALSE)
  )
  expect_identical(as.character(rara$severidad), "ok")
  expect_true(grepl("por azar", rara$evidencia, fixed = TRUE, useBytes = TRUE))
  # Con 100 filas por lado, 10 de un valor nuevo ya no se explican por azar.
  nuevo <- .cva_vocabulario(
    data.frame(x = rep(c("A", "B"), c(50, 50)), stringsAsFactors = FALSE),
    data.frame(x = rep(c("A", "B", "N"), c(45, 45, 10)), stringsAsFactors = FALSE)
  )
  expect_identical(as.character(nuevo$severidad), "sospechoso")
  # Los cambios reales siguen saliendo.
  set.seed(5100L)
  ab <- data.frame(x = sample(c("A", "B"), 200, TRUE), stringsAsFactors = FALSE)
  xy <- data.frame(x = sample(c("X", "Y"), 200, TRUE), stringsAsFactors = FALSE)
  expect_identical(as.character(.cva_vocabulario(ab, xy)$severidad), "error")
  base <- data.frame(x = sample(c("norte", "sur", "este"), 500, TRUE),
                     stringsAsFactors = FALSE)
  nueva <- base
  nueva$x[sample(500, 50)] <- "centro"
  expect_identical(as.character(.cva_vocabulario(base, nueva)$severidad), "sospechoso")
})

test_that("un vocabulario que cambio solo de forma sale ok y lo dice", {
  caja <- .cva_vocabulario(
    data.frame(x = c(rep("Norte", 60), rep("Sur", 40)), stringsAsFactors = FALSE),
    data.frame(x = c(rep("NORTE", 60), rep("Sur", 40)), stringsAsFactors = FALSE)
  )
  expect_identical(as.character(caja$severidad), "ok")
  expect_true(grepl("solo de forma", caja$evidencia, fixed = TRUE, useBytes = TRUE))
  # Con texto latin-1 sin marca, la forma se compara por bytes y no aborta.
  nunez <- rawToChar(as.raw(c(0x4e, 0xfa, 0xf1, 0x65, 0x7a)))
  borde <- rawToChar(as.raw(c(0x4e, 0xfa, 0xf1, 0x65, 0x7a, 0x20)))
  latin <- .cva_vocabulario(
    data.frame(x = rep(c(nunez, "Sur"), c(60, 40)), stringsAsFactors = FALSE),
    data.frame(x = rep(c(borde, "Sur"), c(60, 40)), stringsAsFactors = FALSE)
  )
  expect_true(all(as.character(latin$severidad) == "ok"))
  # Y despues de la limpieza que el plan recomienda.
  datos <- data.frame(x = c(rep("Norte ", 60), rep("Sur ", 40)), stringsAsFactors = FALSE)
  antes <- .cva_perfil(datos)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(antes, datos)))
  expect_true(any(plan$estrategia == "recortar_espacios" & plan$aplicar))
  limpio <- suppressWarnings(suppressMessages(aplicar(plan, datos)))
  deriva <- .cva_comparar(antes, .cva_perfil(limpio$datos, .cva_febrero))
  fila <- deriva[deriva$aspecto == "vocabulario", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "ok")
})

test_that("aplicar() deja el registro de sus marcas y la deriva lo lee", {
  antes <- .cva_perfil(datasets::airquality)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(antes, datasets::airquality)))
  plan$aplicar[plan$recomendada] <- TRUE
  limpio <- suppressWarnings(suppressMessages(aplicar(plan, datasets::airquality)))$datos
  marcas <- attr(limpio, "marcas_limpieza")
  expect_true(is.data.frame(marcas))
  expect_identical(marcas$columna[marcas$marca == ".ausente_Ozone"], "Ozone")
  # La marca sigue siendo marca aunque su columna ya no este.
  sin_base <- limpio
  sin_base$Ozone <- NULL
  deriva <- .cva_comparar(antes, .cva_perfil(sin_base, .cva_febrero))
  expect_identical(
    as.character(deriva$severidad[deriva$columna %in% ".ausente_Ozone"]), "ok"
  )
  expect_identical(
    as.character(deriva$severidad[deriva$columna %in% "Ozone"]), "error"
  )
  # Sobrevive a quedarse con algunas filas; elegir columnas la pierde, y
  # entonces la columna es del usuario: ruido, no silencio.
  algunas <- .cva_comparar(antes, .cva_perfil(limpio[1:100, ], .cva_febrero))
  expect_true(all(algunas$severidad[algunas$aspecto == "columna"] == "ok"))
  columnas <- .cva_comparar(antes, .cva_perfil(limpio[, names(limpio)], .cva_febrero))
  expect_true(all(columnas$severidad[columnas$aspecto == "columna"] == "error"))
})

test_that("una columna del usuario con nombre de marca es una columna del usuario", {
  n <- 100L
  datos <- data.frame(id = seq_len(n), z = seq_len(n) * 2, stringsAsFactors = FALSE)
  datos$x <- ifelse(seq_len(n) %% 10L == 0L, NA, seq_len(n))
  datos$.ausente_x <- is.na(datos$x)
  perfil <- .cva_perfil(datos)
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                    perfil$hallazgos$columna %in% "x"))
  # Control: la marca que deja aplicar() si la silencia.
  empresas <- data.frame(
    tipo = rep(c("persona", "empresa"), each = 60L),
    razon_social = c(rep(NA_character_, 60L), sprintf("Empresa %03d", 1:60)),
    stringsAsFactors = FALSE
  )
  plan <- suppressWarnings(suppressMessages(
    planificar_limpieza(.cva_perfil(empresas), empresas)
  ))
  plan$aplicar <- plan$estrategia == "marcar_filas_ausentes" &
    plan$columna %in% "razon_social"
  marcado <- suppressWarnings(suppressMessages(aplicar(plan, empresas)))$datos
  expect_true(".ausente_razon_social" %in% names(marcado))
  marcado$tipo <- NULL
  expect_false(any(.cva_perfil(marcado)$hallazgos$tipo_hallazgo ==
                     "posible_ausencia_estructural"))
  conmarca <- datos
  conmarca$.fila_duplicada <- rep(c("si", "no"), n / 2L)
  deriva <- .cva_comparar(.cva_perfil(conmarca), .cva_perfil(datos, .cva_febrero))
  expect_identical(
    as.character(deriva$severidad[deriva$columna %in% ".fila_duplicada"]), "error"
  )
})

test_that("con la proteccion, un valor protegido no sale por otra columna", {
  # Valores identificantes -seis caracteres o mas, el piso de la proteccion-:
  # uno corto que se comparte, como "S/D", se sigue publicando en la otra
  # columna (test-proteccion-por-columna.R).
  datos <- data.frame(
    diagnostico = rep(c("diabetes_tipo_dos", "hipertension_cronica"), c(60, 40)),
    codigo = rep(c("diabetes_tipo_dos", "hipertension_cronica"), c(60, 40)),
    otro = rep(c("DIABETES_TIPO_DOS ", "Hipertension_Cronica"), c(60, 40)),
    region = rep(c("norte", "sur"), 50), stringsAsFactors = FALSE
  )
  perfil <- .cva_perfil(datos, proteger_datos_personales = TRUE,
                        columnas_personales = "diagnostico")
  .cva_aparece <- function(objeto) {
    serializado <- .minusculas_ascii_cva(paste(capture.output(dput(objeto)), collapse = ""))
    vapply(c("diabetes_tipo_dos", "hipertension_cronica"), function(valor) {
      grepl(valor, serializado, fixed = TRUE, useBytes = TRUE)
    }, logical(1))
  }
  expect_false(any(.cva_aparece(perfil)))
  expect_identical(unique(perfil$general$vocabulario$columna), "region")
  otra <- datos
  otra$codigo <- rep(c("diabetes_tipo_dos", "hipertension_cronica"), c(40, 60))
  deriva <- suppressWarnings(suppressMessages(comparar_perfiles(
    perfil, .cva_perfil(otra, .cva_febrero, proteger_datos_personales = TRUE,
                        columnas_personales = "diagnostico")
  )))
  expect_false(any(.cva_aparece(deriva)))
  protegidos <- attr(deriva, "campos_protegidos")
  expect_true(any(protegidos$columna == "codigo" & protegidos$campo == "vocabulario"))
})

test_that("una columna de valores largos no guarda vocabulario", {
  largos <- c(strrep("a", 100), strrep("b", 100))
  datos <- data.frame(texto = rep(largos, 50), corto = rep(c("x", "y"), 50),
                      stringsAsFactors = FALSE)
  vocabulario <- .cva_perfil(datos)$general$vocabulario
  expect_identical(unique(vocabulario$columna), "corto")
})

test_that("un faltante disfrazado no es un valor del vocabulario", {
  antes <- data.frame(x = rep(c("A", "B"), c(80, 20)), stringsAsFactors = FALSE)
  despues <- data.frame(x = rep(c("A", "B", "NA"), c(80, 10, 10)),
                        stringsAsFactors = FALSE)
  perfil <- .cva_perfil(despues, .cva_febrero)
  expect_equal(perfil$columnas$n_faltantes_disfrazados[perfil$columnas$columna == "x"], 10)
  expect_false("NA" %in% perfil$general$vocabulario$valor)
  expect_setequal(perfil$general$vocabulario$valor, c("A", "B"))
  # La deriva lo informa por los faltantes, no como valor nuevo.
  deriva <- .cva_comparar(.cva_perfil(antes), perfil)
  expect_false(any(deriva$aspecto == "vocabulario"))
  expect_true(any(grepl("faltantes", deriva$aspecto, fixed = TRUE, useBytes = TRUE) |
                    grepl("faltantes", as.character(deriva$valor_actual),
                          fixed = TRUE, useBytes = TRUE)))
})

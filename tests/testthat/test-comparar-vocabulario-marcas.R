# comparar_perfiles(): el vocabulario de las columnas categoricas, las marcas
# que agrega el plan de limpieza y los objetos de analizar().
#
# Una entrega que cambiaba TODO el vocabulario de una columna con la misma
# forma -`A`/`B` por `X`/`Y`, mismas proporciones- daba cero filas de deriva:
# el perfil no guardaba los valores. Y comparar antes y despues de `aplicar()`
# publicaba cada marca como una columna aparecida con severidad `error`, y la
# marca `.ausente_<x>` "explicaba" la ausencia de `x` como hallazgo.

# Prueba de regresion: corre en la suite completa, en el CI y en R-hub, que usan
# NOT_CRAN=true; en CRAN se saltea para que el check entre en sus tiempos.
testthat::skip_on_cran()

.cvm_enero <- as.POSIXct("2026-01-01", tz = "UTC")
.cvm_febrero <- as.POSIXct("2026-02-01", tz = "UTC")

.cvm_perfil <- function(datos, fecha = .cvm_enero, ...) {
  suppressWarnings(suppressMessages(perfilar(
    datos, fecha = fecha, analizar_dependencias = FALSE, ...
  )))
}

.cvm_comparar <- function(...) {
  as.data.frame(suppressWarnings(suppressMessages(comparar_perfiles(...))))
}

.cvm_categorias <- function(valores, n, prob = NULL, semilla) {
  set.seed(semilla)
  data.frame(estado = sample(valores, n, TRUE, prob = prob),
             stringsAsFactors = FALSE)
}

test_that("un vocabulario cambiado entero con la misma forma es un error", {
  anterior <- .cvm_categorias(c("A", "B"), 200L, semilla = 101L)
  actual <- .cvm_categorias(c("X", "Y"), 200L, semilla = 102L)
  deriva <- .cvm_comparar(.cvm_perfil(anterior), .cvm_perfil(actual, .cvm_febrero))
  fila <- deriva[deriva$aspecto == "vocabulario", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$columna, "estado")
  expect_identical(fila$cambio, "modificado")
  expect_identical(as.character(fila$severidad), "error")
  expect_equal(fila$delta, 1)
  expect_identical(fila$universo_actual, "tabla_completa")
  expect_true(grepl("X (", fila$evidencia, fixed = TRUE, useBytes = TRUE))
  # Con otra forma de los valores tambien, ademas del patron.
  largos <- .cvm_comparar(
    .cvm_perfil(.cvm_categorias(c("INICIADO", "EN_PROCESO"), 200L, semilla = 103L)),
    .cvm_perfil(.cvm_categorias(c("FINALIZADO", "CANCELADO_"), 200L, semilla = 104L),
                .cvm_febrero)
  )
  expect_identical(as.character(largos$severidad[largos$aspecto == "vocabulario"]),
                   "error")
})

test_that("la misma distribucion remuestreada no da deriva de vocabulario", {
  categorias <- c("norte", "sur", "este", "oeste")
  peso <- c(0.4, 0.3, 0.2, 0.1)
  for (r in 1:20) {
    deriva <- .cvm_comparar(
      .cvm_perfil(.cvm_categorias(categorias, 200L, peso, semilla = 2000L + r)),
      .cvm_perfil(.cvm_categorias(categorias, 200L, peso, semilla = 3000L + r),
                  .cvm_febrero)
    )
    expect_false(any(deriva$aspecto == "vocabulario"), label = paste("repeticion", r))
  }
  # Y el mismo par con un valor nuevo de peso si se ve: el control no es mudo.
  base <- .cvm_categorias(categorias, 200L, peso, semilla = 2001L)
  nuevo <- base
  nuevo$estado[1:40] <- "centro"
  deriva <- .cvm_comparar(.cvm_perfil(base), .cvm_perfil(nuevo, .cvm_febrero))
  expect_identical(as.character(deriva$severidad[deriva$aspecto == "vocabulario"]),
                   "error")
})

test_that("una categoria nueva con poco peso se informa sin alarma", {
  set.seed(105)
  base <- data.frame(cat = sample(c("norte", "sur", "este"), 500, TRUE),
                     stringsAsFactors = FALSE)
  poco <- base
  poco$cat[sample(500, 10)] <- "centro"
  fila <- .cvm_comparar(.cvm_perfil(base), .cvm_perfil(poco, .cvm_febrero))
  fila <- fila[fila$aspecto == "vocabulario", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$severidad), "ok")
  expect_false(fila$significativo)
  expect_equal(fila$delta, 0.02)
  mas <- base
  mas$cat[sample(500, 50)] <- "centro"
  fila <- .cvm_comparar(.cvm_perfil(base), .cvm_perfil(mas, .cvm_febrero))
  expect_identical(as.character(fila$severidad[fila$aspecto == "vocabulario"]),
                   "sospechoso")
})

test_that("un identificador no tiene vocabulario y un tope cruzado se declara", {
  set.seed(106)
  codigos <- function(prefijo) {
    data.frame(codigo = sprintf("%s%04d", prefijo, sample(1:9999, 300)),
               stringsAsFactors = FALSE)
  }
  alta <- .cvm_comparar(.cvm_perfil(codigos("C")), .cvm_perfil(codigos("D"), .cvm_febrero))
  expect_false(any(alta$aspecto == "vocabulario"))
  # Pocos valores pero todos distintos: tampoco.
  tres <- .cvm_comparar(
    .cvm_perfil(data.frame(codigo = c("C1", "C2", "C3"))),
    .cvm_perfil(data.frame(codigo = c("D1", "D2", "D3")), .cvm_febrero)
  )
  expect_false(any(tres$aspecto == "vocabulario"))
  # Y el perfil no los guarda.
  expect_false("codigo" %in% .cvm_perfil(codigos("C"))$general$vocabulario$columna)
  cruza <- .cvm_comparar(
    .cvm_perfil(data.frame(cat = sample(sprintf("v%02d", 1:15), 300, TRUE))),
    .cvm_perfil(data.frame(cat = sample(sprintf("v%02d", 1:30), 300, TRUE)), .cvm_febrero)
  )
  fila <- cruza[cruza$aspecto == "vocabulario", , drop = FALSE]
  expect_identical(fila$cambio, "no_comparable")
  expect_identical(as.character(fila$severidad), "ok")
})

test_that("el perfil guarda el vocabulario de las columnas categoricas", {
  datos <- data.frame(
    estado = factor(c("b", "a", "b", NA, "b", "c")),
    monto = c(1, 2, 3, 4, 5, 6), stringsAsFactors = FALSE
  )
  vocabulario <- .cvm_perfil(datos)$general$vocabulario
  expect_identical(names(vocabulario), c("columna", "valor", "frecuencia", "proporcion"))
  expect_identical(vocabulario$columna, rep("estado", 3L))
  expect_identical(vocabulario$valor, c("b", "a", "c"))
  expect_identical(vocabulario$frecuencia, c(3L, 1L, 1L))
  expect_equal(vocabulario$proporcion, c(3, 1, 1) / 5)
})

test_that("un perfil sin vocabulario se declara no comparable", {
  viejo <- .cvm_perfil(.cvm_categorias(c("A", "B"), 200L, semilla = 107L))
  viejo$general$vocabulario <- NULL
  deriva <- .cvm_comparar(viejo, .cvm_perfil(.cvm_categorias(c("X", "Y"), 200L,
                                                             semilla = 108L), .cvm_febrero))
  fila <- deriva[deriva$aspecto == "vocabulario", , drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(fila$cambio, "no_comparable")
  expect_true(is.na(fila$columna))
})

test_that("el vocabulario de una columna protegida no se guarda ni se compara", {
  set.seed(109)
  datos <- function(estados, regiones) {
    data.frame(id = 1:300, estado = sample(estados, 300, TRUE),
               region = sample(regiones, 300, TRUE), stringsAsFactors = FALSE)
  }
  protegido <- function(d, fecha) {
    .cvm_perfil(d, fecha, proteger_datos_personales = TRUE,
                columnas_personales = "estado")
  }
  a <- protegido(datos(c("ALFA", "BETA", "GAMA"), c("norte", "sur")), .cvm_enero)
  b <- protegido(datos(c("DELTA", "OMEGA"), c("este", "oeste")), .cvm_febrero)
  expect_false("estado" %in% a$general$vocabulario$columna)
  expect_false(grepl("ALFA", paste(capture.output(dput(a)), collapse = ""), fixed = TRUE,
                     useBytes = TRUE))
  deriva <- suppressWarnings(suppressMessages(comparar_perfiles(a, b)))
  expect_identical(deriva$columna[deriva$aspecto == "vocabulario"], "region")
  protegidos <- attr(deriva, "campos_protegidos")
  expect_setequal(protegidos$lado[protegidos$columna == "estado" &
                                    protegidos$campo == "vocabulario"],
                  c("anterior", "actual"))
})

test_that("el vocabulario de una base se compara con muestra que cubre y se declara con muestra parcial", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  enero <- .cvm_categorias(c("A", "B"), 200L, semilla = 110L)
  enero$id <- 1:200
  febrero <- .cvm_categorias(c("X", "Y"), 200L, semilla = 111L)
  febrero$id <- 201:400
  DBI::dbWriteTable(con, "ene", enero)
  DBI::dbWriteTable(con, "feb", febrero)
  dbi <- function(tabla, fecha, ...) {
    suppressWarnings(suppressMessages(perfilar_dbi(
      con, tabla, fecha = fecha, proteger_datos_personales = FALSE,
      analizar_dependencias = FALSE, ...
    )))
  }
  completa <- .cvm_comparar(dbi("ene", .cvm_enero), dbi("feb", .cvm_febrero))
  expect_identical(as.character(completa$severidad[completa$aspecto == "vocabulario"]),
                   "error")
  parcial <- .cvm_comparar(dbi("ene", .cvm_enero, muestra = 50L, orden_muestra = "id"),
                           dbi("feb", .cvm_febrero, muestra = 50L, orden_muestra = "id"))
  fila <- parcial[parcial$aspecto == "vocabulario", , drop = FALSE]
  expect_identical(fila$cambio, "no_comparable")
  expect_true(grepl("estado: sin_cifra_sql", fila$evidencia, fixed = TRUE, useBytes = TRUE))
  # Sin valores publicados no hay vocabulario, y se declara.
  sin_valores <- dbi("ene", .cvm_enero, incluir_valores = FALSE)
  expect_equal(nrow(sin_valores$perfil_muestra$general$vocabulario), 0L)
  declarada <- suppressWarnings(suppressMessages(comparar_perfiles(
    sin_valores, dbi("feb", .cvm_febrero, incluir_valores = FALSE)
  )))
  expect_false(any(declarada$aspecto == "vocabulario"))
  expect_true(any(attr(declarada, "campos_protegidos")$campo == "vocabulario"))
})

test_that("las marcas de aplicar() no son columnas nuevas con alarma ni explican su ausencia", {
  antes <- .cvm_perfil(datasets::airquality)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(antes, datasets::airquality)))
  plan$aplicar[plan$recomendada] <- TRUE
  limpio <- suppressWarnings(suppressMessages(aplicar(plan, datasets::airquality)))
  expect_true(".ausente_Ozone" %in% names(limpio$datos))
  despues <- .cvm_perfil(limpio$datos, .cvm_febrero)
  # La marca no explica la ausencia de su propia columna.
  expect_false(any(despues$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                     despues$hallazgos$columna %in% "Ozone"))
  deriva <- .cvm_comparar(antes, despues)
  marcas <- deriva[deriva$aspecto == "columna", , drop = FALSE]
  expect_setequal(marcas$columna, c(".ausente_Ozone", ".outlier_Ozone", ".outlier_Wind"))
  expect_true(all(marcas$severidad == "ok"))
  expect_true(all(marcas$cambio == "aparecida"))
  expect_false(any(deriva$severidad == "error"))
  # Al reves -de lo limpio a una entrega cruda- la marca desaparece sin alarma.
  regreso <- .cvm_comparar(despues, .cvm_perfil(datasets::airquality,
                                               as.POSIXct("2026-03-01", tz = "UTC")))
  expect_true(all(regreso$severidad[regreso$aspecto == "columna"] == "ok"))
  # El control: una columna del usuario con punto inicial sigue siendo una columna.
  propia <- datasets::airquality
  propia$.nota <- "revisado"
  propia$.ausente_inexistente <- FALSE
  usuario <- .cvm_comparar(antes, .cvm_perfil(propia, .cvm_febrero))
  expect_identical(
    as.character(usuario$severidad[usuario$aspecto == "columna"]), c("error", "error")
  )
})

test_that("una columna del usuario sigue explicando una ausencia", {
  # El control de la regla anterior: la determinacion por OTRA columna sigue.
  n <- 120L
  datos <- data.frame(
    tipo = rep(c("persona", "empresa"), each = n / 2L),
    razon_social = c(rep(NA_character_, n / 2L), sprintf("Empresa %03d", 1:(n / 2L))),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(suppressMessages(perfilar(datos, fecha = .cvm_enero)))
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                    perfil$hallazgos$columna %in% "razon_social"))
  # Y una columna con el nombre de la marca de OTRA columna no la silencia.
  datos$.ausente_tipo <- datos$tipo == "persona"
  perfil <- suppressWarnings(suppressMessages(perfilar(datos, fecha = .cvm_enero)))
  expect_true(any(perfil$hallazgos$tipo_hallazgo == "posible_ausencia_estructural" &
                    perfil$hallazgos$columna %in% "razon_social"))
  # La que si es su marca -la que deja aplicar()-, la silencia.
  datos$.ausente_tipo <- NULL
  perfil <- suppressWarnings(suppressMessages(perfilar(datos, fecha = .cvm_enero)))
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(perfil, datos)))
  plan$aplicar <- plan$estrategia == "marcar_filas_ausentes" &
    plan$columna %in% "razon_social"
  datos <- suppressWarnings(suppressMessages(aplicar(plan, datos)))$datos
  expect_true(".ausente_razon_social" %in% names(datos))
  datos$tipo <- NULL
  perfil <- suppressWarnings(suppressMessages(perfilar(datos, fecha = .cvm_enero)))
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "posible_ausencia_estructural"))
})

test_that("un hallazgo sobre una marca no es una alarma de los datos", {
  set.seed(112)
  n <- 300L
  datos <- data.frame(id_caso = sample(1:250, n, TRUE), region = sample(c("a", "b"), n, TRUE),
                      stringsAsFactors = FALSE)
  datos <- rbind(datos, datos[1:20, ])
  antes <- .cvm_perfil(datos)
  plan <- suppressWarnings(suppressMessages(planificar_limpieza(antes, datos)))
  plan$aplicar <- plan$estado == "lista" & plan$estrategia == "marcar_filas_duplicadas"
  limpio <- suppressWarnings(suppressMessages(aplicar(plan, datos)))
  deriva <- .cvm_comparar(antes, .cvm_perfil(limpio$datos, .cvm_febrero))
  grupo <- deriva[deriva$aspecto == "hallazgo" & deriva$columna %in% ".grupo_duplicado", ,
                  drop = FALSE]
  expect_equal(nrow(grupo), 1L)
  expect_identical(as.character(grupo$severidad), "ok")
  expect_true(grepl("marca", grupo$descripcion, fixed = TRUE, useBytes = TRUE))
})

test_that("comparar_perfiles acepta objetos de analizar() y compara sus perfiles", {
  a <- .cvm_categorias(c("A", "B"), 200L, semilla = 113L)
  b <- .cvm_categorias(c("X", "Y"), 200L, semilla = 114L)
  an_a <- suppressWarnings(suppressMessages(analizar(a, fecha = .cvm_enero)))
  an_b <- suppressWarnings(suppressMessages(analizar(b, fecha = .cvm_febrero)))
  directa <- suppressWarnings(suppressMessages(comparar_perfiles(an_a, an_b)))
  por_perfil <- suppressWarnings(suppressMessages(comparar_perfiles(an_a$perfil, an_b$perfil)))
  expect_identical(directa, por_perfil)
  expect_true(any(directa$aspecto == "vocabulario"))
  mixta <- suppressWarnings(suppressMessages(comparar_perfiles(an_a, an_b$perfil)))
  expect_identical(mixta, por_perfil)
})

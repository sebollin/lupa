testthat::skip_on_cran()

.cev_plan <- function(datos, ..., con_datos = TRUE) {
  perfil <- perfilar(
    datos, fecha = "2026-01-15", analizar_dependencias = FALSE, ...
  )
  plan <- if (con_datos) {
    planificar_limpieza(perfil, datos)
  } else {
    planificar_limpieza(perfil)
  }
  list(perfil = perfil, plan = plan)
}

.cev_accion <- function(plan) {
  plan[plan$estrategia == "convertir_sentinelas_numericos", , drop = FALSE]
}

.cev_aplicar <- function(plan, datos) {
  plan$aplicar <- plan$estrategia == "convertir_sentinelas_numericos"
  suppressWarnings(aplicar(plan, datos))
}

.cev_contados <- function(perfil, columna = "x") {
  fila <- perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
  as.numeric(fila$n_faltantes_disfrazados_numericos)
}

test_that("la accion lleva los centinelas que el perfil conto en la columna", {
  casos <- list(
    list(x = c(1:2000, rep(-9, 10)), valores = -9, quedan = 999),
    list(x = c(1:2000, rep(-999, 10)), valores = -999, quedan = 999),
    list(x = c(rep(18:80, 20), rep(-9, 15)), valores = -9, quedan = 80),
    list(
      x = c(seq(100, 5000, by = 7), rep(-99, 12), 999, 9999),
      valores = -99, quedan = c(999, 9999)
    )
  )
  for (caso in casos) {
    for (con_datos in c(TRUE, FALSE)) {
      datos <- data.frame(x = caso$x)
      r <- .cev_plan(datos, con_datos = con_datos)
      accion <- .cev_accion(r$plan)
      contados <- .cev_contados(r$perfil)
      expect_identical(nrow(accion), 1L)
      expect_identical(
        as.numeric(accion$parametros[[1L]]$valores), as.numeric(caso$valores)
      )
      expect_identical(as.numeric(accion$n_afectadas), contados)
      aplicado <- .cev_aplicar(r$plan, datos)
      expect_identical(as.character(aplicado$registro$estado), "ejecutada")
      expect_identical(as.numeric(aplicado$registro$n_cambiadas), contados)
      expect_true(all(caso$quedan %in% aplicado$datos$x))
      expect_false(any(caso$valores %in% aplicado$datos$x))
      expect_identical(sum(is.na(aplicado$datos$x)), as.integer(contados))
    }
  }
})

test_that("la lista se lee con la convencion decimal de la columna", {
  base <- seq(1000.5, 4000.5, by = 3)
  texto <- c(sub(".", ",", sprintf("%.2f", base), fixed = TRUE), rep("-999,00", 5))
  datos <- data.frame(x = texto, stringsAsFactors = FALSE)
  for (con_datos in c(TRUE, FALSE)) {
    r <- .cev_plan(datos, con_datos = con_datos)
    accion <- .cev_accion(r$plan)
    expect_identical(nrow(accion), 1L)
    expect_identical(as.numeric(accion$parametros[[1L]]$valores), -999)
    expect_identical(.cev_contados(r$perfil), 5)
    aplicado <- .cev_aplicar(r$plan, datos)
    expect_identical(as.numeric(aplicado$registro$n_cambiadas), 5)
    expect_identical(sum(is.na(aplicado$datos$x)), 5L)
  }
})

test_that("lo que el usuario declara se suma a la lista", {
  datos <- data.frame(x = c(501:1000, rep(-1, 4)))
  r <- .cev_plan(datos, sentinelas_numericos = c(-1, 999))
  accion <- .cev_accion(r$plan)
  contados <- .cev_contados(r$perfil)
  expect_identical(contados, 5)
  expect_setequal(as.numeric(accion$parametros[[1L]]$valores), c(-1, 999))
  aplicado <- .cev_aplicar(r$plan, datos)
  expect_identical(as.numeric(aplicado$registro$n_cambiadas), contados)
  expect_false(any(c(-1, 999) %in% aplicado$datos$x))
})

test_that("si la evidencia no nombra todo lo contado la lista queda entera", {
  marcas <- c("S/D", "N/A", "NULL", "-", "sin dato", "NA")
  texto <- c(
    as.character(seq(50, 5000, by = 3)), rep(marcas, each = 12),
    rep(c("-9", "-99", "-999"), c(9, 8, 7))
  )
  datos <- data.frame(x = texto, stringsAsFactors = FALSE)
  for (con_datos in c(TRUE, FALSE)) {
    r <- .cev_plan(datos, con_datos = con_datos)
    accion <- .cev_accion(r$plan)
    contados <- .cev_contados(r$perfil)
    expect_identical(contados, 24)
    expect_setequal(
      as.numeric(accion$parametros[[1L]]$valores),
      c(-9999, -999, -99, -9, 999)
    )
    aplicado <- .cev_aplicar(r$plan, datos)
    expect_identical(as.character(aplicado$registro$estado), "ejecutada")
    expect_identical(as.numeric(aplicado$registro$n_cambiadas), contados)
  }
})

test_that("la evidencia solo vale si da cuenta de todo lo contado", {
  expect_identical(.sentinelas_contados_en_evidencia("-9 (10)", 10), -9)
  expect_null(.sentinelas_contados_en_evidencia("-9 (10)", 11))
  expect_setequal(
    .sentinelas_contados_en_evidencia("S/D (3); -9 (2); -99 (1)", 3),
    c(-9, -99)
  )
  expect_null(.sentinelas_contados_en_evidencia("S/D (3); N/A (2)", 4))
  expect_identical(
    .sentinelas_contados_en_evidencia(
      "99 (5); -9 (2)", 2, textuales = "99"
    ),
    -9
  )
  expect_identical(
    .sentinelas_contados_en_evidencia("-999,00 (5)", 5, "decimal_coma"), -999
  )
  expect_identical(
    .sentinelas_contados_en_evidencia("-999 (3); -999,00 (2)", 5, "decimal_coma"),
    -999
  )
  # Una marca declarada con el separador adentro parte la evidencia: no se lee.
  expect_null(.sentinelas_contados_en_evidencia(
    "9; x (7); -9 (2)", 9, textuales = "9; x"
  ))
  expect_null(.sentinelas_contados_en_evidencia(
    "x; 999 (9); -9 (10)", 19, textuales = "x; 999"
  ))
  expect_null(.sentinelas_contados_en_evidencia("9; -9 (9)", 9))
  expect_null(.sentinelas_contados_en_evidencia(NA_character_, 3))
  expect_null(.sentinelas_contados_en_evidencia("", 3))
  expect_null(.sentinelas_contados_en_evidencia("-9 (10)", 0))
})

test_that("una marca declarada con el separador de la evidencia no fabrica centinelas", {
  edades <- as.character(rep(0:105, 3))
  marcas <- c("S/D", "N/A", "NULL", "-", "sin dato")
  texto <- c(
    edades, rep(marcas, each = 12), rep("-9", 10), rep("9; x", 7),
    rep("-99", 5), rep("-999", 4)
  )
  datos <- data.frame(x = texto, stringsAsFactors = FALSE)
  for (con_datos in c(TRUE, FALSE)) {
    r <- .cev_plan(datos, cadenas_ausencia = "9; x", con_datos = con_datos)
    accion <- .cev_accion(r$plan)
    contados <- .cev_contados(r$perfil)
    expect_identical(contados, 19)
    expect_setequal(
      as.numeric(accion$parametros[[1L]]$valores),
      c(-9999, -999, -99, -9, 999)
    )
    aplicado <- .cev_aplicar(r$plan, datos)
    expect_identical(as.numeric(aplicado$registro$n_cambiadas), contados)
    expect_identical(sum(aplicado$datos$x %in% "9"), 3L)
  }
})

test_that("una columna clasificada como personal no apaga la lista acotada", {
  # Sin proteccion, que la tabla tenga una cedula o un correo no cambia nada.
  perfil <- perfilar(datos_administrativos, fecha = "2026-01-15")
  expect_false(isTRUE(perfil$meta$proteger_datos_personales))
  expect_true(length(.columnas_personales_protegidas(perfil)) > 0L)
  plan <- planificar_limpieza(perfil, datos_administrativos)
  accion <- .cev_accion(plan)
  expect_identical(nrow(accion), 1L)
  contados <- .cev_contados(perfil, as.character(accion$columna))
  expect_true(length(accion$parametros[[1L]]$valores) < 5L)
  aplicado <- .cev_aplicar(plan, datos_administrativos)
  expect_identical(as.numeric(aplicado$registro$n_cambiadas), contados)
})

test_that("con un perfil protegido la lista de centinelas no se achica", {
  documento <- c(
    sprintf("%d.%03d.%03d-%d", 1:5, 100:104, 200:204, 1:5),
    rep("9.999.999-9", 3), sprintf("%d.%03d.%03d-%d", 2:4, 300:302, 400:402, 6:8)
  )
  datos <- data.frame(
    documento = rep(documento, 20),
    monto = rep(c(seq(150, 3000, by = 300), -999), 20),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(
    datos, fecha = "2026-01-15", analizar_dependencias = FALSE,
    proteger_datos_personales = TRUE, sentinelas_numericos = c(-999, 99999999)
  )
  skip_if(
    !length(.columnas_personales_protegidas(perfil)),
    "la columna de documentos no quedo protegida en este armado"
  )
  plan <- planificar_limpieza(perfil, datos)
  accion <- plan[
    plan$estrategia == "convertir_sentinelas_numericos" &
      plan$columna %in% "monto", , drop = FALSE
  ]
  expect_identical(nrow(accion), 1L)
  # La lista es la del perfil, con lo tapado en su lugar: nada se cae.
  expect_identical(
    length(accion$parametros[[1L]]$valores),
    length(perfil$meta$sentinelas_numericos)
  )
})

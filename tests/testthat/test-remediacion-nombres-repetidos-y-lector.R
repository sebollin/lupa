# Dos casos donde el plan de limpieza decia otra cosa que el perfil.
#
# Columnas iguales con un nombre repetido en la tabla: el plan proponia dos
# acciones `bloqueada` con parametros degenerados (`columna_2` vacia), cuando la
# regla del planificador para una columna ambigua es no proponer accion y
# declararla con el remedio.
#
# Un telefono entre importes con coma decimal: el perfil no lo lee como numero
# -lo deja fuera del resumen y dice que convertir lo dejaria en `NA`-, pero el
# convertidor del plan le quitaba los puntos y `aplicar()` escribia 99123456.

# Pesada para CRAN (mas de 0,2 s en la medicion sin red del 2026-11-10): corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.rnl_fecha <- as.POSIXct("2026-01-01", tz = "UTC")

.rnl_plan <- function(d) {
  perfil <- suppressWarnings(suppressMessages(perfilar(d, fecha = .rnl_fecha)))
  list(perfil = perfil,
       plan = suppressWarnings(planificar_limpieza(perfil, datos = d)))
}

# De cada grupo de alternativas, la destructiva que este `lista`; fuera de
# grupos, toda accion `lista`.
.rnl_destructivo <- function(plan) {
  estado <- as.character(plan$estado)
  grupo <- as.character(plan$grupo)
  activar <- plan$aplicar
  sin_grupo <- is.na(grupo) | !nzchar(grupo)
  activar[sin_grupo] <- estado[sin_grupo] == "lista"
  for (g in unique(grupo[!sin_grupo])) {
    en <- which(grupo == g)
    destructivas <- en[estado[en] == "lista" & plan$destructiva[en] %in% TRUE]
    if (length(destructivas)) {
      activar[en] <- FALSE
      activar[destructivas[[1L]]] <- TRUE
    }
  }
  plan$aplicar <- activar
  plan
}

.rnl_aplicar_las_dos <- function(plan, d) {
  recomendado <- suppressWarnings(suppressMessages(aplicar(plan, d)))
  destructivo <- suppressWarnings(suppressMessages(aplicar(
    .rnl_destructivo(plan), d, permitir_eliminacion = TRUE
  )))
  expect_false(any(recomendado$registro$estado == "fallida"))
  expect_false(any(destructivo$registro$estado == "fallida"))
  list(recomendado = recomendado, destructivo = destructivo)
}

test_that("un grupo de columnas iguales con un nombre repetido no propone accion", {
  casos <- list(
    c("X", "X", "Y"), c("X", "X", "X"), c("X", "Y", "X"), c("Y", "X", "X")
  )
  for (nombres in casos) {
    d <- data.frame(A = 1:3, B = 1:3,
                    C = if (identical(nombres, c("X", "X", "Y"))) 4:6 else 1:3)
    names(d) <- nombres
    armado <- .rnl_plan(d)
    plan <- as.data.frame(armado$plan)
    info <- paste(nombres, collapse = ", ")
    expect_false(any(plan$hallazgo == "columnas_duplicadas"), info = info)
    # El hallazgo se declara con el remedio, tambien cuando la columna que lo
    # nombra no se repite (`Y, X, X`).
    ambiguos <- attr(armado$plan, "hallazgos_sin_accion_por_columna_ambigua")
    expect_true(any(ambiguos$hallazgo == "columnas_duplicadas"), info = info)
    expect_match(
      ambiguos$como_resolverlo[ambiguos$hallazgo == "columnas_duplicadas"][[1L]],
      "normalizar_nombres", fixed = TRUE, info = info
    )
    sin_accion <- attr(armado$plan, "hallazgos_sin_accion")
    expect_false(any(sin_accion$hallazgo == "columnas_duplicadas"), info = info)
    .rnl_aplicar_las_dos(armado$plan, d)
  }
})

test_that("con nombres distintos el grupo sigue teniendo sus dos acciones", {
  d <- data.frame(id = 1:3, copia = 1:3, otro = 4:6)
  armado <- .rnl_plan(d)
  plan <- as.data.frame(armado$plan)
  duplicadas <- plan[plan$hallazgo == "columnas_duplicadas", , drop = FALSE]
  expect_setequal(duplicadas$estrategia,
                  c("marcar_columnas_duplicadas", "eliminar_columna_duplicada"))
  expect_true(all(as.character(duplicadas$estado) == "lista"))
  aplicados <- .rnl_aplicar_las_dos(armado$plan, d)
  expect_identical(names(aplicados$destructivo$datos), c("id", "otro"))
})

test_that("dos factores con los mismos niveles en otro orden se eliminan al aplicar", {
  d <- data.frame(
    A = factor(c("a", "b", "a"), levels = c("a", "b")),
    B = factor(c("a", "b", "a"), levels = c("b", "a")), C = 1:3
  )
  armado <- .rnl_plan(d)
  aplicados <- .rnl_aplicar_las_dos(armado$plan, d)
  expect_identical(names(aplicados$destructivo$datos), c("A", "C"))
})

.rnl_importes <- function(n) {
  enteros <- 100 + (seq_len(n) * 7919) %% 99000
  formatC(enteros + (seq_len(n) * 37 %% 100) / 100, format = "f", digits = 2,
          big.mark = ".", decimal.mark = ",")
}

# Lo que `aplicar()` escribe en la columna, por valor -la via destructiva puede
# quitar filas-, contra lo que el perfil lee.
.rnl_escribe_lo_que_lee <- function(x, perfil, datos_aplicados) {
  fila <- perfil$columnas[perfil$columnas$columna == "v", , drop = FALSE]
  lee <- lupa:::.leer_numeros_texto(x, fila$numero_texto_convencion)
  escrito <- datos_aplicados$v
  if (!is.numeric(escrito)) return(TRUE)
  all(is.na(escrito) | escrito %in% lee[!is.na(lee)])
}

test_that("el plan no convierte lo que el perfil no lee como numero", {
  casos <- list(
    "un telefono entre importes" = c(rep("1.234,56", 9), "099.123.456"),
    "dos telefonos entre importes" = c(.rnl_importes(36), "099.123.456",
                                       "098.765.432", .rnl_importes(2)),
    "un telefono con punto decimal" = c(rep("1,234.56", 9), "099,123,456")
  )
  for (nombre in names(casos)) {
    x <- casos[[nombre]]
    d <- data.frame(v = x, stringsAsFactors = FALSE)
    armado <- .rnl_plan(d)
    plan <- as.data.frame(armado$plan)
    conversion <- plan[plan$estrategia == "convertir_numero_regional", ,
                       drop = FALSE]
    expect_identical(nrow(conversion), 1L, info = nombre)
    expect_identical(as.character(conversion$estado), "bloqueada", info = nombre)
    aplicados <- .rnl_aplicar_las_dos(armado$plan, d)
    for (via in names(aplicados)) {
      expect_true(
        .rnl_escribe_lo_que_lee(x, armado$perfil, aplicados[[via]]$datos),
        info = paste(nombre, via)
      )
    }
  }
})

test_that("una columna de importes se sigue convirtiendo como la lee el perfil", {
  x <- .rnl_importes(40)
  d <- data.frame(v = x, stringsAsFactors = FALSE)
  armado <- .rnl_plan(d)
  plan <- as.data.frame(armado$plan)
  conversion <- plan[plan$estrategia == "convertir_numero_regional", ,
                     drop = FALSE]
  expect_identical(as.character(conversion$estado), "lista")
  expect_true(conversion$aplicar)
  aplicados <- .rnl_aplicar_las_dos(armado$plan, d)
  fila <- armado$perfil$columnas[armado$perfil$columnas$columna == "v", ]
  expect_identical(
    aplicados$recomendado$datos$v,
    lupa:::.leer_numeros_texto(x, fila$numero_texto_convencion)
  )
})

test_that("con la convencion ambigua, los miles agrupan de a tres sin cero adelante", {
  x <- c("1.500", "2.750", "099.500")
  miles <- list(convencion = "ambigua", punto_sin_coma = "miles",
                coma_sin_punto = NA_character_)
  expect_error(lupa:::.convertir_numero_regional(x, miles),
               "no responden al formato")
  # Leido como decimal no agrupa nada: el valor se lee, y el cero inicial lo
  # declara `.evaluar_conversion()`.
  decimal <- miles
  decimal$punto_sin_coma <- "decimal"
  expect_identical(lupa:::.convertir_numero_regional(x, decimal)$valor,
                   c(1.5, 2.75, 99.5))
  expect_identical(
    lupa:::.convertir_numero_regional(c("1.500", "2.750"), miles)$valor,
    c(1500, 2750)
  )
})

test_that("la perdida de cifras del plan es la regla del perfil", {
  casos <- list(
    enteros = c("9007199254740993", "12", "9007199254740992"),
    decimales = c("0,123456789012345678", "1,5", "0,25"),
    porcentaje = c("33,333333333333333333 %", "12,5 %")
  )
  for (nombre in names(casos)) {
    x <- casos[[nombre]]
    parametros <- list(convencion = "decimal_coma", moneda = NA_character_,
                       unidad = NA_character_, punto_sin_coma = NA_character_,
                       coma_sin_punto = NA_character_)
    convertido <- lupa:::.convertir_numero_regional(x, parametros)$valor
    evaluacion <- lupa:::.evaluar_conversion(
      x, convertido, "convertir_numero_regional", parametros
    )
    texto <- lupa:::.texto_regional_normalizado(x, parametros)$texto
    expect_identical(
      evaluacion$n_redondeados,
      as.integer(sum(lupa:::.cifras_que_redondea_un_doble(
        texto, suppressWarnings(as.numeric(texto))
      ))),
      info = nombre
    )
    expect_gt(evaluacion$n_redondeados, 0L)
  }
})

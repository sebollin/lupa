testthat::skip_on_cran()

.n2585_u <- function(...) intToUtf8(c(...))

test_that("los invisibles ambiguos se conservan y se informan", {
  nbsp <- .n2585_u(0xA0L)
  shy <- .n2585_u(0xADL)
  datos <- data.frame(
    x = c(
      paste0(.n2585_u(0xC5L), nbsp, "koda"),
      paste0("A", .n2585_u(0xC5L), shy, "RA"),
      paste0("A", .n2585_u(0xC4L), nbsp, "RA"),
      paste0(.n2585_u(0xC5L), nbsp, "i Lofoten")
    ),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicado <- aplicar(plan, datos)$datos$x

  expect_identical(aplicado, datos$x)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "controles_invisibles", , drop = FALSE
  ]
  expect_true(nrow(hallazgo) >= 1L)
  expect_true(any(grepl("no puede decidir", hallazgo$descripcion,
                        fixed = TRUE)))
})

test_that("un NBSP que no forma un par ambiguo se normaliza", {
  nbsp <- .n2585_u(0xA0L)
  jose <- paste0(
    "JOS", .n2585_u(0xC9L), nbsp, "P", .n2585_u(0xC9L), "REZ"
  )
  datos <- data.frame(x = c(jose, "otro"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicado <- aplicar(plan, datos)$datos$x

  expect_identical(
    aplicado[[1L]],
    paste0("JOS", .n2585_u(0xC9L), " ", "P", .n2585_u(0xC9L), "REZ")
  )
})

test_that("un par ambiguo conserva el texto y se avisa", {
  patur <- paste0(
    "P", .n2585_u(0xC5L), .n2585_u(0xA0L), "TUR"
  )
  datos <- data.frame(x = c(patur, "otro"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicado <- aplicar(plan, datos)$datos$x
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "controles_invisibles", , drop = FALSE
  ]

  expect_identical(aplicado[[1L]], patur)
  expect_true(any(grepl("no puede decidir", hallazgo$descripcion,
                        fixed = TRUE)))
})

test_that("la capitalizacion comparte la proteccion del texto roto", {
  datos <- data.frame(
    x = c(
      paste0("M", .n2585_u(0xC3L), .n2585_u(0x153L), "NCHEN"),
      paste0("m", .n2585_u(0xC3L), .n2585_u(0x153L), "nchen")
    ),
    stringsAsFactors = FALSE
  )
  esperado <- list(
    c(0xC3L, 0x153L), c(0xC3L, 0x153L)
  )

  for (estrategia in c(
    "convertir_minusculas", "convertir_mayusculas", "convertir_titulo"
  )) {
    perfil <- perfilar(datos)
    plan <- planificar_limpieza(perfil, datos)
    plan$aplicar <- plan$estrategia == estrategia
    aplicado <- aplicar(plan, datos)$datos$x
    expect_equal(lapply(aplicado, function(x) utf8ToInt(x)[2:3]), esperado)
  }
})

testthat::skip_on_cran()

.n2586_u <- function(...) intToUtf8(c(...))

.n2586_aplicar <- function(datos, estrategia) {
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  plan$aplicar <- plan$estrategia == estrategia
  aplicar(plan, datos)$datos$x
}

test_that("los signos y rayas legitimos no se reparan como mojibake", {
  textos <- c(
    paste0("Kategorie ", .n2586_u(0xC4L), .n2586_u(0x2014L)),
    paste0("Teil ", .n2586_u(0xC4L), .n2586_u(0x2014L)),
    paste0(.n2586_u(0xC4L), .n2586_u(0x2014L), " Ende der Liste"),
    paste0(.n2586_u(0x201EL), .n2586_u(0xC5L), .n2586_u(0x201CL)),
    paste0("A", .n2586_u(0x2014L), ", B", .n2586_u(0x2014L), ", ",
           .n2586_u(0xC4L), .n2586_u(0x2014L))
  )
  datos <- data.frame(x = textos, stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicado <- aplicar(plan, datos)$datos$x

  expect_identical(aplicado, textos)
  expect_false(any(perfil$columnas$n_codificacion_rota > 0L))
})

test_that("las palabras mayusculas de varias lenguas cambian de caja completas", {
  mayusculas <- c(
    paste0("V", .n2586_u(0xDDL), .n2586_u(0x160L), "KA"),
    paste0("P", .n2586_u(0xCDL), .n2586_u(0x160L), "EK"),
    paste0("T", .n2586_u(0xC9L), .n2586_u(0x17DL)),
    paste0("L", .n2586_u(0xDDL), .n2586_u(0x17DL), "E"),
    paste0("D", .n2586_u(0xC9L), .n2586_u(0x160L), .n2586_u(0x164L)),
    paste0(.n2586_u(0x17DL), "LU", .n2586_u(0x164L), "OU",
           .n2586_u(0x10CL), "K", .n2586_u(0xDDL)),
    paste0(.n2586_u(0x160L), "KODA"),
    paste0(.n2586_u(0x15EL), .n2586_u(0x130L), .n2586_u(0x15EL), "L",
           .n2586_u(0x130L)),
    paste0(.n2586_u(0x17BL), .n2586_u(0xD3L), .n2586_u(0x141L), "W"),
    paste0(.n2586_u(0x218L), "TEFAN")
  )
  minusculas <- c(
    paste0("v", .n2586_u(0xFDL), .n2586_u(0x161L), "ka"),
    paste0("p", .n2586_u(0xEDL), .n2586_u(0x161L), "ek"),
    paste0("t", .n2586_u(0xE9L), .n2586_u(0x17EL)),
    paste0("l", .n2586_u(0xFDL), .n2586_u(0x17EL), "e"),
    paste0("d", .n2586_u(0xE9L), .n2586_u(0x161L), .n2586_u(0x165L)),
    paste0(.n2586_u(0x17EL), "lu", .n2586_u(0x165L), "ou",
           .n2586_u(0x10DL), "k", .n2586_u(0xFDL)),
    paste0(.n2586_u(0x161L), "koda"),
    paste0(.n2586_u(0x15FL), "i", .n2586_u(0x15FL), "li"),
    paste0(.n2586_u(0x17CL), .n2586_u(0xF3L), .n2586_u(0x142L), "w"),
    paste0(.n2586_u(0x219L), "tefan")
  )
  titulo <- c(
    paste0("V", .n2586_u(0xFDL), .n2586_u(0x161L), "ka"),
    paste0("P", .n2586_u(0xEDL), .n2586_u(0x161L), "ek"),
    paste0("T", .n2586_u(0xE9L), .n2586_u(0x17EL)),
    paste0("L", .n2586_u(0xFDL), .n2586_u(0x17EL), "e"),
    paste0("D", .n2586_u(0xE9L), .n2586_u(0x161L), .n2586_u(0x165L)),
    paste0(.n2586_u(0x17DL), "lu", .n2586_u(0x165L), "ou",
           .n2586_u(0x10DL), "k", .n2586_u(0xFDL)),
    paste0(.n2586_u(0x160L), "koda"),
    paste0(.n2586_u(0x15EL), "i", .n2586_u(0x15FL), "li"),
    paste0(.n2586_u(0x17BL), .n2586_u(0xF3L), .n2586_u(0x142L), "w"),
    paste0(.n2586_u(0x218L), "tefan")
  )
  mayusculas_resultado <- mayusculas
  mayusculas_resultado[[8L]] <- paste0(
    .n2586_u(0x15EL), "I", .n2586_u(0x15EL), "LI"
  )
  datos <- data.frame(x = c(mayusculas, minusculas), stringsAsFactors = FALSE)
  n <- length(mayusculas)

  expect_identical(
    .n2586_aplicar(datos, "convertir_minusculas")[seq_len(n)],
    minusculas
  )
  expect_identical(
    .n2586_aplicar(datos, "convertir_titulo")[seq_len(n)],
    titulo
  )
  expect_identical(
    .n2586_aplicar(datos, "convertir_mayusculas")[n + seq_len(n)],
    mayusculas_resultado
  )
})

test_that("los seis pares ortograficos no se diagnostican ni reparan", {
  textos <- c(
    paste0(.n2586_u(0xC4L), .n2586_u(0x2014L)),
    paste0(.n2586_u(0x201EL), .n2586_u(0xC5L), .n2586_u(0x201CL)),
    paste0(.n2586_u(0x2039L), .n2586_u(0xC8L), .n2586_u(0x203AL)),
    paste0(.n2586_u(0xC4L), .n2586_u(0x178L)),
    paste0(.n2586_u(0xC5L), .n2586_u(0x178L)),
    paste0(.n2586_u(0xC4L), .n2586_u(0x152L))
  )
  datos <- data.frame(x = textos, stringsAsFactors = FALSE)
  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil, datos)
  aplicado <- aplicar(plan, datos)$datos$x

  expect_false(any(perfil$columnas$n_codificacion_rota > 0L))
  expect_false(any(perfil$hallazgos$tipo_hallazgo == "codificacion_rota"))
  expect_identical(aplicado, textos)
})

test_that("las tablas limpias conservan perfil plan y datos", {
  tablas <- list(
    data.frame(opcion = c(
      paste0("A", .n2586_u(0x2014L)), paste0("B", .n2586_u(0x2014L)),
      paste0(.n2586_u(0xC4L), .n2586_u(0x2014L))
    ),
               stringsAsFactors = FALSE),
    data.frame(cita = paste0(.n2586_u(0x201EL), .n2586_u(0xC5L),
                             .n2586_u(0x201CL)), stringsAsFactors = FALSE),
    data.frame(frase = paste0(.n2586_u(0x2039L), .n2586_u(0xC8L),
                              .n2586_u(0x203AL)), stringsAsFactors = FALSE)
  )

  for (datos in tablas) {
    perfil <- perfilar(datos)
    plan <- planificar_limpieza(perfil, datos)
    aplicado <- aplicar(plan, datos)$datos
    expect_false(any(perfil$hallazgos$tipo_hallazgo == "codificacion_rota"))
    expect_identical(lapply(aplicado, identity), lapply(datos, identity))
  }
})

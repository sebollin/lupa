capturar_cli_plan <- function(expr) {
  archivo <- tempfile()
  conexion <- file(archivo, open = "wt")
  sink(conexion)
  sink(conexion, type = "message")
  tryCatch(
    force(expr),
    finally = {
      sink(type = "message")
      sink()
      close(conexion)
    }
  )
  paste(readLines(archivo, warn = FALSE), collapse = "\n")
}

activar_una_accion_honesta <- function(plan, estrategia) {
  indice <- which(plan$estrategia == estrategia)
  stopifnot(length(indice) == 1L)
  grupo <- plan$grupo[[indice]]
  if (!is.na(grupo)) plan$aplicar[plan$grupo == grupo] <- FALSE
  plan$aplicar[[indice]] <- TRUE
  plan$decision_grupo[plan$grupo == grupo] <- "elegida"
  plan
}

test_that("la unidad del hallazgo viaja al plan y al modo guiado", {
  datos <- data.frame(
    zona = c(rep("Activo", 60L), rep("ACTIVO", 30L), rep("activo", 10L)),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  plan <- plan[plan$hallazgo == "mayusculas_inconsistentes", , drop = FALSE]
  indice <- which(plan$estrategia == "convertir_minusculas")

  expect_equal(plan$n_afectadas[[indice]], 3)
  expect_equal(plan$unidad_conteo[[indice]], "valor_distinto")
  expect_match(capturar_cli_plan(print(plan)), "unidad_conteo")

  guiado <- capturar_cli_plan(guiar_limpieza(
    plan, datos, selector = function(decision) "convertir_minusculas"
  ))
  expect_match(guiado, "Cantidad estimada: 3.*valor_distinto")

  plan <- activar_una_accion_honesta(plan, "convertir_minusculas")
  resultado <- aplicar(plan, datos)
  expect_equal(resultado$registro$n_cambiadas, 90)
  expect_equal(resultado$registro$estado, "ejecutada")
})

test_that("un efecto nulo no se registra como ejecutado", {
  datos <- data.frame(x = c(" NA ", NA, "x"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(perfilar(datos), datos)
  resultado <- aplicar(plan, datos)
  recorte <- resultado$registro$estrategia == "recortar_espacios"

  expect_true(any(recorte))
  expect_equal(resultado$registro$n_cambiadas[recorte], 0)
  expect_equal(resultado$registro$estado[recorte], "fallida")
  expect_match(resultado$registro$error[recorte], "sin efecto")
  expect_true(all(is.na(resultado$datos$x[1:2])))
})

test_that("la composicion de acciones de texto tiene un orden efectivo", {
  datos <- data.frame(x = c("\u200B A ", "B"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, muestra = Inf, analizar_dependencias = FALSE), datos
  )
  expect_true(all(plan$aplicar[plan$columna == "x"]))
  expect_lt(
    plan$orden[plan$estrategia == "eliminar_controles_invisibles"],
    plan$orden[plan$estrategia == "recortar_espacios"]
  )

  resultado <- aplicar(plan, datos)
  expect_equal(resultado$datos$x, c("A", "B"))
  expect_true(all(resultado$registro$estado == "ejecutada"))
})

test_that("la imputacion usa los datos cuando el mapa fue protegido", {
  datos <- data.frame(
    documento = rep(c("12345678", "23456789"), each = 6L),
    provincia = rep(c("Norte", "Sur"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$provincia[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__documento")
  expect_length(indice, 1L)
  mapa <- plan$parametros[[indice]]$mapa
  expect_false(any(mapa$determinante %in% datos$documento))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$registro$n_cambiadas, 1)
  expect_equal(resultado$datos$provincia[[12L]], "Sur")
})

test_that("la imputacion con determinante no protegido conserva el mapa", {
  datos <- data.frame(
    codigo = rep(c("AA-001", "BB-002"), each = 6L),
    provincia = rep(c("Norte", "Sur"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$provincia[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__codigo")
  expect_length(indice, 1L)
  expect_true(any(plan$parametros[[indice]]$mapa$determinante %in% datos$codigo))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$registro$n_cambiadas, 1)
  expect_equal(resultado$datos$provincia[[12L]], "Sur")
})

test_that("la imputacion conserva el dependiente protegido sin publicar el mapa", {
  datos <- data.frame(
    codigo = rep(c("AA-001", "BB-002"), each = 6L),
    nombre = rep(c("Ana", "Beto"), each = 6L),
    stringsAsFactors = FALSE
  )
  datos$nombre[[12L]] <- NA_character_
  perfil <- perfilar(datos, muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  indice <- which(plan$estrategia ==
    "imputar_dependencia_funcional__codigo")
  expect_length(indice, 1L)
  expect_false(any(plan$parametros[[indice]]$mapa$dependiente %in% datos$nombre))

  elegido <- activar_una_accion_honesta(plan[indice, , drop = FALSE],
                                        plan$estrategia[[indice]])
  resultado <- aplicar(elegido, datos)
  expect_equal(resultado$registro$estado, "ejecutada")
  expect_equal(resultado$datos$nombre[[12L]], "Beto")
})

test_that("winsorizar deja sin outliers el perfil de una columna chica", {
  datos <- data.frame(x = c(1, 2, 3, 100))
  perfil <- perfilar(datos, muestra = Inf, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  elegido <- activar_una_accion_honesta(plan, "winsorizar_outliers")
  resultado <- aplicar(elegido, datos)
  indice <- resultado$registro$estrategia == "winsorizar_outliers"

  expect_equal(resultado$registro$estado[indice], "ejecutada")
  expect_equal(resultado$registro$n_cambiadas[indice], 1)
  reprofilado <- perfilar(
    resultado$datos, muestra = Inf, analizar_dependencias = FALSE
  )
  expect_false(any(reprofilado$hallazgos$tipo_hallazgo == "outliers"))
})

test_that("al transformar una columna clave se descarta la clave data.table", {
  skip_if_not_installed("data.table")
  datos <- data.table::data.table(x = c("ZETA", "Zeta", "alfa"))
  data.table::setkey(datos, x)
  plan <- planificar_limpieza(perfilar(datos, analizar_dependencias = FALSE))
  plan <- activar_una_accion_honesta(plan, "convertir_minusculas")
  resultado <- aplicar(plan, datos)

  expect_null(data.table::key(resultado$datos))
  expect_equal(resultado$datos$x, c("zeta", "zeta", "alfa"))
})

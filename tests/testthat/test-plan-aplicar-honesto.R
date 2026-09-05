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

# Tres cosas que el plan decia y `aplicar()` desmentia, encontradas el 2026-09-05
# con el encargo de construir el caso donde el plan promete algo que no cumple.

test_that("n_cambiadas cuenta cambios y no valores presentes", {
  # Planificar sobre texto y aplicar sobre la MISMA tabla ya convertida a
  # entero: la conversion es una identidad y no cambia nada. Catorce acciones de
  # `R/remediacion.R` cuentan `sum(cambio)`; las dos conversiones contaban
  # `sum(!is.na(x))`, o sea los presentes, y por eso declaraban `ejecutada` con
  # `n_cambiadas = 4` sobre una columna que quedaba bit a bit igual.
  datos <- data.frame(x = c("1", "2", "3", "4"), stringsAsFactors = FALSE)
  plan <- planificar_limpieza(
    perfilar(datos, analizar_dependencias = FALSE), datos
  )
  skip_if_not(any(plan$estrategia == "convertir_tipo"),
              "el plan no propuso `convertir_tipo` en este entorno")

  # Primera mitad: sobre los datos originales la accion SI convierte. Sin esto,
  # un `n_cambiadas = 0` podria venir de que la accion no corrio.
  real <- aplicar(plan, datos)
  fila_real <- real$registro[real$registro$estrategia == "convertir_tipo", ]
  expect_equal(fila_real$estado, "ejecutada")
  expect_equal(fila_real$n_cambiadas, 4L)
  expect_false(identical(class(real$datos$x), class(datos$x)))

  ya_convertidos <- datos
  ya_convertidos$x <- as.integer(ya_convertidos$x)
  identidad <- aplicar(plan, ya_convertidos)
  fila <- identidad$registro[identidad$registro$estrategia == "convertir_tipo", ]

  # La columna no cambio, asi que el conteo es cero y el estado es `fallida`
  # con su motivo: es lo que `man/planificar_limpieza.Rd` promete para una
  # accion sin efecto cuando el plan estimaba alguno.
  expect_true(identical(identidad$datos$x, ya_convertidos$x))
  expect_equal(fila$n_cambiadas, 0L)
  expect_equal(fila$estado, "fallida")
  expect_match(paste(fila$error, collapse = " "), "sin efecto", fixed = TRUE)
})

test_that("los duplicados se agrupan igual en integer64 que en double", {
  skip_if_not_installed("bit64")
  # `duplicated(x, fromLast = TRUE)` sobre `integer64` devuelve lo mismo que sin
  # `fromLast`: el metodo de bit64 ignora el argumento. Con eso, de un grupo de
  # dos filas identicas se marcaba una sola.
  valores <- c(1, 2, -999, 4, -999)
  con_64 <- data.frame(v = bit64::as.integer64(valores))
  con_dbl <- data.frame(v = valores)

  aplicar_marca <- function(datos) {
    plan <- planificar_limpieza(
      perfilar(datos, analizar_dependencias = FALSE), datos
    )
    aplicar(plan, datos)
  }
  r64 <- aplicar_marca(con_64)
  rdbl <- aplicar_marca(con_dbl)

  # Primera mitad: la accion corrio en los dos casos.
  fila64 <- r64$registro[r64$registro$estrategia == "marcar_filas_duplicadas", ]
  expect_equal(nrow(fila64), 1L)
  expect_equal(fila64$estado, "ejecutada")

  # Las dos filas del grupo quedan identificadas, como declara la documentacion,
  # y el tipo no cambia el resultado.
  expect_equal(r64$datos$.grupo_duplicado, rdbl$datos$.grupo_duplicado)
  expect_equal(sum(!is.na(r64$datos$.grupo_duplicado)), 2L)
  expect_equal(fila64$n_cambiadas, 2L)
})

test_that("el numero del plan y su unidad de conteo hablan de lo mismo", {
  # `unidad_conteo` se heredaba del hallazgo pisando lo que la accion declaraba:
  # `convertir_tipo` cuenta valores presentes y quedaba con `columna`, asi que
  # el plan decia "5 columna" sobre una tabla de UNA columna.
  datos <- data.frame(x = c("1", "1", "2", "2", "3"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  fila <- plan[plan$estrategia == "convertir_tipo", ]
  skip_if_not(nrow(fila) == 1L, "el plan no propuso `convertir_tipo`")

  expect_equal(fila$unidad_conteo, "valor")
  expect_equal(fila$n_afectadas, sum(!is.na(datos$x)))

  # Control, y es el que hace que esto pruebe algo: una accion que SI cuenta en
  # la unidad del hallazgo tiene que seguir heredandola. Sin esta mitad, poner
  # "valor" en todas pasaria el test.
  con_duplicados <- data.frame(a = c(1, 1, 2, 2, 3), b = c(1, 1, 2, 2, 3))
  plan2 <- planificar_limpieza(
    perfilar(con_duplicados, analizar_dependencias = FALSE), con_duplicados
  )
  columnas <- plan2[plan2$estrategia == "marcar_columnas_duplicadas", ]
  filas <- plan2[plan2$estrategia == "marcar_filas_duplicadas", ]
  if (nrow(columnas)) expect_equal(columnas$unidad_conteo, "columna")
  if (nrow(filas)) expect_equal(filas$unidad_conteo, "fila")
})

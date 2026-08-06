test_that("cada hallazgo declara sus conteos y su unidad", {
  perfil <- perfilar(
    data.frame(
      importe = c(999, 1, NA),
      constante = c("A", "A", "A"),
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE
  )

  expect_true(all(c("n_evaluados", "n_afectados", "unidad_conteo") %in%
                    names(perfil$hallazgos)))
  expect_true(all(!is.na(perfil$hallazgos$unidad_conteo)))
  faltantes <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados", ,
    drop = FALSE
  ]
  expect_equal(faltantes$n_evaluados, 3)
  expect_equal(faltantes$n_afectados, 1)
  expect_equal(faltantes$unidad_conteo, "fila")
  constante <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "constante", ,
    drop = FALSE
  ]
  expect_equal(constante$n_evaluados, 3)
  expect_equal(constante$n_afectados, 3)
  expect_equal(constante$unidad_conteo, "fila")
})

test_that("los hallazgos estructurales declaran unidades distintas", {
  datos <- data.frame(a = c(1, 1), b = c(1, 1), check.names = FALSE)
  names(datos) <- c("a", "a")
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  duplicadas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "columnas_duplicadas", ,
    drop = FALSE
  ]
  expect_true(nrow(duplicadas) >= 1)
  expect_equal(duplicadas$unidad_conteo, "columna")
  expect_equal(duplicadas$n_evaluados, 2)
  expect_equal(duplicadas$n_afectados, 2)
})

test_that("los hallazgos de pares cuentan pares comparados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres"),
    domicilio = c("Calle 1", "Calle 1")
  )
  perfil <- perfilar(
    datos,
    analizar_dependencias = FALSE,
    duplicados_aproximados = list(
      columnas = c("nombre", "domicilio"),
      estrategia = "teselas",
      max_resultados = Inf
    )
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "duplicados_aproximados", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$unidad_conteo, "par")
  expect_equal(hallazgo$n_evaluados, 1)
  expect_equal(hallazgo$n_afectados, 1)
})

test_that("las propiedades de columna cuentan columnas, no filas", {
  datos <- datos_operativos
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  tipos <- c("alta_cardinalidad", "posible_identificador",
             "tipo_declarado_distinto")
  hallazgos <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% tipos, , drop = FALSE
  ]
  expect_true(nrow(hallazgos) > 0)
  expect_true(all(hallazgos$unidad_conteo == "columna"))
  expect_true(all(hallazgos$n_evaluados == 1))
  expect_true(all(hallazgos$n_afectados == 1))
})

test_that("las fechas partidas cuentan las columnas involucradas", {
  datos <- data.frame(
    fecha_anio = c(2020, 2021, 2022),
    fecha_mes = c(1, 2, 3), fecha_dia = c(3, 28, 15)
  )
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "fecha_partida_columnas", , drop = FALSE
  ]
  expect_equal(hallazgo$unidad_conteo, "columna")
  expect_equal(hallazgo$n_evaluados, 3)
  expect_equal(hallazgo$n_afectados, 3)
})

test_that("patron_raro conserva el total de valores afectados y sus filas", {
  datos <- data.frame(cod = c(rep("USR-001", 40L), "S/D", "MAL-5"))
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$n_afectados, 2)
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(traza$total, 2)
  expect_equal(traza$indices_fila, c(41L, 42L))
  expect_equal(traza$estado, "disponible")
})

test_that("la trazabilidad de patrones muestreados declara su alcance", {
  valores <- rep("AA-111", 2000L)
  valores[c(1980L, 2000L)] <- c("S/D", "MAL-5")
  perfil <- perfilar(
    data.frame(cod = valores), muestra = 100L,
    analizar_dependencias = FALSE
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "patron_raro", , drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1)
  expect_equal(hallazgo$n_evaluados, 100)
  expect_equal(hallazgo$n_afectados, 2)
  expect_equal(hallazgo$trazabilidad[[1L]]$alcance, "muestra_patrones")
  expect_equal(hallazgo$trazabilidad[[1L]]$indices_fila, c(1980L, 2000L))
})

test_that("la trazabilidad es uniforme, acotada y no se imprime como indices", {
  datos <- data.frame(x = rep(1, 2005L))
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, max_filas_hallazgo = 10L
  )
  constante <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "constante", , drop = FALSE
  ]
  expect_equal(nrow(constante), 1)
  traza <- constante$trazabilidad[[1L]]
  expect_equal(traza$estado, "truncada")
  expect_equal(traza$total, 2005)
  expect_equal(traza$mostrados, 10)
  expect_true(traza$truncado)
  expect_equal(length(traza$indices_fila), 10)

  ruta <- reportar(perfil)
  html <- paste(readLines(ruta, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  unlink(ruta)
  expect_false(grepl("indices_fila", html, fixed = TRUE))
  expect_false(grepl("1, 2, 3, 4", html, fixed = TRUE))
  expect_match(html, "filas mostradas")
})

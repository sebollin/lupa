# Cobertura del trabajo por columnas y de las fronteras de agregacion de bases.

.con_coleccion_ancha <- function() {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

.tabla_ancha <- function(prefijo, clave, desplazamiento = 0L) {
  columnas <- list(clave = clave)
  for (i in seq_len(29L)) {
    columnas[[paste0(prefijo, sprintf("%02d", i))]] <-
      seq_along(clave) + desplazamiento + i
  }
  as.data.frame(columnas, stringsAsFactors = FALSE, check.names = FALSE)
}

test_that("las candidatas reducen el costo y las podas se declaran", {
  con <- .con_coleccion_ancha()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  izquierda <- .tabla_ancha("a_relleno_", 1:20)
  derecha <- .tabla_ancha("b_relleno_", 1:20, desplazamiento = 1000L)
  izquierda$a_num <- 1001:1020
  izquierda$a_texto <- paste0("a", seq_len(20L))
  derecha$b_num <- 2001:2020
  derecha$b_texto <- paste0("b", seq_len(20L))
  izquierda$clave <- 1:20
  derecha$clave <- 1:20
  DBI::dbWriteTable(con, "izquierda", izquierda)
  DBI::dbWriteTable(con, "derecha", derecha)
  col <- coleccion(con, c("izquierda", "derecha"))
  candidatas <- list(
    izquierda = c("clave", "a_num", "a_texto"),
    derecha = c("clave", "b_num", "b_texto")
  )

  completo <- estimar_costo_coleccion(
    col, pares = data.frame(tabla_1 = "izquierda", tabla_2 = "derecha"),
    columnas_candidatas = NULL
  )
  acotado <- estimar_costo_coleccion(
    col, pares = data.frame(tabla_1 = "izquierda", tabla_2 = "derecha"),
    columnas_candidatas = candidatas
  )
  expect_equal(completo$n_comparaciones_columnas, ncol(izquierda) * ncol(derecha))
  expect_equal(acotado$n_comparaciones_columnas, 9)

  resultado <- relaciones_coleccion(
    col,
    pares = data.frame(tabla_1 = "izquierda", tabla_2 = "derecha"),
    columnas_candidatas = candidatas,
    # `podar = TRUE` pide tambien las podas que cambian lo informado: sin eso
    # solo se aplica la cierta, que es la de rangos disjuntos.
    podar = TRUE,
    orden = list(izquierda = "clave", derecha = "clave")
  )
  expect_equal(nrow(resultado$relaciones), 1L)
  expect_equal(resultado$relaciones$columna_tabla1, "clave")
  expect_equal(resultado$relaciones$columna_tabla2, "clave")
  expect_true(resultado$meta$combinaciones_podadas > 0L)
  expect_true(all(c("tipos_incompatibles", "rangos_disjuntos") %in%
                    resultado$cobertura_podas$motivo))
  expect_true(all(!grepl("a_relleno_01|b_relleno_01",
                         resultado$meta$lecturas$sql)))
})

test_that("el presupuesto agotado deja los pares pendientes en cobertura", {
  con <- .con_coleccion_ancha()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "origen", data.frame(id = 1:20))
  DBI::dbWriteTable(con, "destino", data.frame(fk = 1:20))
  DBI::dbWriteTable(con, "tercero", data.frame(fk = 1:20))
  col <- coleccion(con, c("origen", "destino", "tercero"))
  pares <- data.frame(
    tabla_1 = c("origen", "origen"),
    tabla_2 = c("destino", "tercero"),
    stringsAsFactors = FALSE
  )

  resultado <- relaciones_coleccion(
    col, pares = pares, tope_memoria_mb = 1e-9
  )
  expect_equal(resultado$meta$pares_declarados, 2L)
  expect_equal(resultado$meta$pares_comparados, 1L)
  expect_equal(resultado$meta$pares_sin_comparar_por_presupuesto, 1L)
  expect_equal(nrow(resultado$cobertura_pares), 1L)
  expect_true(grepl("tope_memoria_mb", resultado$cobertura_pares$motivo,
                    fixed = TRUE))
  expect_equal(nrow(resultado$relaciones), 1L)
})

.medidas_entidad_ancha <- function() {
  salida <- data.frame(
    id_medicion = rep("m-ancha", 2L),
    fecha = as.POSIXct(rep("2026-08-20", 2L), tz = "UTC"),
    metrica = rep("completitud", 2L),
    metrica_especifica = rep("NoNulo", 2L),
    metrica_instanciada = rep("NoNulo@tabla", 2L),
    dimension = rep("Completitud", 2L),
    factor = rep("Presencia", 2L),
    granularidad = rep("entidad", 2L),
    tipo_resultado = rep("real", 2L),
    entidad = c("tabla_a", "tabla_b"),
    atributo = NA_character_, fila = NA_integer_,
    objeto_medible = c("tabla_a", "tabla_b"),
    resultado = c(0.9, 0.5), agregacion = NA_character_,
    stringsAsFactors = FALSE
  )
  class(salida) <- c("medicion", "data.frame")
  salida
}

test_that("la agregacion conserva alcances distintos y requiere pesos", {
  con <- .con_coleccion_ancha()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  frontera <- coleccion(
    con, data.frame(
      esquema = NA_character_,
      tabla = c("tabla_a", "tabla_b", "tabla_no_medida"),
      stringsAsFactors = FALSE
    ),
    nombre = "base_ancha"
  )
  medidas <- .medidas_entidad_ancha()

  expect_error(
    agregar(medidas, "coleccion", "promedio", coleccion = frontera),
    "solo se admite 'promedio_ponderado'"
  )
  agregado <- agregar(
    medidas, "coleccion", "promedio_ponderado",
    pesos = c(0.6, 0.4), coleccion = frontera
  )
  cobertura <- attr(agregado, "cobertura_coleccion")
  expect_equal(cobertura$tablas_declaradas, 3L)
  expect_equal(cobertura$tablas_en_el_numero, 2L)
  expect_equal(cobertura$cobertura, 2 / 3)
  expect_equal(agregado$resultado, 0.74)
})

test_that("un conjunto de colecciones exige una frontera y pesos", {
  con <- .con_coleccion_ancha()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  base_a <- coleccion(con, "tabla_a", nombre = "base_a")
  base_b <- coleccion(con, "tabla_b", nombre = "base_b")
  medidas <- .medidas_entidad_ancha()
  a <- agregar(
    medidas[1, , drop = FALSE], "coleccion", "promedio_ponderado",
    pesos = 1, coleccion = base_a
  )
  b <- agregar(
    medidas[2, , drop = FALSE], "coleccion", "promedio_ponderado",
    pesos = 1, coleccion = base_b
  )
  medidas_conjunto <- rbind(a, b)
  class(medidas_conjunto) <- c("medicion", "data.frame")

  expect_error(
    agregar(
      medidas_conjunto, "conjuntoColecciones", "promedio",
      colecciones = list(base_a = base_a, base_b = base_b)
    ),
    "solo se admite 'promedio_ponderado'"
  )
  agregado <- agregar(
    medidas_conjunto, "conjuntoColecciones", "promedio_ponderado",
    pesos = c(0.25, 0.75),
    colecciones = list(base_a = base_a, base_b = base_b)
  )
  expect_equal(agregado$granularidad, "conjuntoColecciones")
  expect_equal(agregado$resultado, 0.6)
  cobertura <- attr(agregado, "cobertura_conjunto_colecciones")
  expect_equal(cobertura$colecciones_declaradas, 2L)
  expect_equal(cobertura$colecciones_en_el_numero, 2L)
  expect_equal(cobertura$cobertura, 1)
})

test_that("la entrada data.frame valida los mismos faltantes y tipos", {
  con <- .con_coleccion_ancha()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_error(
    coleccion(con, data.frame(tabla = c("a", NA_character_))),
    "no admite `NA`"
  )
  expect_error(
    coleccion(con, data.frame(tabla = c("a", ""))),
    "cadenas vacias"
  )
  expect_error(
    coleccion(con, data.frame(tabla = 1:2)),
    "debe ser de texto"
  )
  expect_error(
    coleccion(con, data.frame(tabla = c("a", "b"), tipo = c("tabla", NA))),
    "no admite `NA`"
  )
})

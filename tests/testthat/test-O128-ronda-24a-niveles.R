# Ronda 24-A: colecciones, organizaciones y lo que sube de nivel.

.fecha_O128 <- as.POSIXct("2026-03-01", tz = "UTC")

.medicion_O128 <- function(datos, id = "M", fecha = .fecha_O128) {
  especie <- especializar(metricas_nucleo()$NoNulo)
  instancias <- lapply(names(datos), function(t) instanciar(especie, t, "x"))
  suppressWarnings(medir(modelo(instancias), datos, id_medicion = id, fecha = fecha))
}

.entidades_O128 <- function() {
  datos <- list(
    a1 = data.frame(x = c(1, NA, 3, 4)), a2 = data.frame(x = c(1, 2, 3, NA)),
    b1 = data.frame(x = c(NA, NA, NA, 1)), b2 = data.frame(x = c(1, 2, 3, 4))
  )
  medicion <- .medicion_O128(datos)
  list(
    datos = datos,
    entidades = agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")
  )
}

.colecciones_O128 <- function(con, datos) {
  for (n in names(datos)) DBI::dbWriteTable(con, n, datos[[n]])
  list(
    A = coleccion(con, c("a1", "a2", "a3"), nombre = "A"),
    B = coleccion(con, c("b1", "b2"), nombre = "B")
  )
}

test_that("la cobertura de cada coleccion sobrevive a rbind en cualquier orden", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .entidades_O128()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  colecciones <- .colecciones_O128(con, base$datos)
  ent <- base$entidades
  cA <- agregar(ent[ent$entidad %in% c("a1", "a2"), ], "coleccion",
                "promedio_ponderado", pesos = c(a1 = 0.5, a2 = 0.5),
                coleccion = colecciones$A)
  cB <- agregar(ent[ent$entidad %in% c("b1", "b2"), ], "coleccion",
                "promedio_ponderado", pesos = c(b1 = 0.5, b2 = 0.5),
                coleccion = colecciones$B)
  org <- organizacion("Org", colecciones)
  for (union in list(rbind(cA, cB), rbind(cB, cA))) {
    o <- agregar(union, "organizacion", "promedio_ponderado",
                 pesos = c(A = 0.5, B = 0.5), organizacion = org)
    # A mano: .5 * (.5 * .75 + .5 * .75) + .5 * (.5 * .25 + .5 * 1) = .6875.
    expect_equal(o$resultado, 0.6875)
    # Antes: con `rbind(cB, cA)` la organizacion salia completa y `a3` no
    # aparecia en ningun lado.
    cobertura <- attr(o, "cobertura_organizacion")
    expect_false(cobertura$completo)
    expect_true(any(grepl("A", cobertura$partes_incompletas)))
    impreso <- paste(capture.output(print(tablero_calidad(o))), collapse = "\n")
    expect_match(impreso, "a3", fixed = TRUE)
  }
})

test_that("la regla de un agregado no da por completa una parte sin medir", {
  datos <- list(a1 = data.frame(x = c(1, NA)), a2 = data.frame(x = numeric()))
  agregada <- agregar(.medicion_O128(datos), "atributo", "ratio")
  regla <- regla_evaluacion("Completo", function(x) x > 0.4,
                            metricas = unique(agregada$metrica_instanciada))
  evaluacion <- evaluar(agregada, perfil_evaluacion("P", regla))
  # Antes: 1 -la regla cumplia sobre a1 y a2 no existia-; la misma regla sobre
  # las celdas daba NA, y la deriva leia la ausencia como mejora.
  expect_true(is.na(evaluacion$reglas$resultado))
})

test_that("una tabla y una columna con punto no se funden en un atributo", {
  especie <- especializar(metricas_nucleo()$NoNulo)
  medicion <- medir(
    modelo(
      instanciar(especie, "ventas", "total.mes", nombre_instancia = "NoNulo@v1"),
      instanciar(especie, "ventas.total", "mes", nombre_instancia = "NoNulo@v2")
    ),
    list(ventas = data.frame(total.mes = c(1, 2, 3, 4)),
         ventas.total = data.frame(mes = c(1, NA, NA, NA))),
    id_medicion = "M", fecha = .fecha_O128
  )
  atributos <- agregar(medicion, "atributo", "ratio")
  # Antes: una fila con 0.625 (5 de 8).
  expect_equal(nrow(atributos), 2L)
  expect_setequal(atributos$resultado, c(1, 0.25))
  expect_false(anyDuplicated(atributos$id_medida) > 0L)
})

test_that("una parte completa agregada por separado suma sus celdas", {
  # `Formato` no mide la celda vacia: b1 mide 2 de 4. `NoNulo` mediria las 4.
  formato <- especializar(metricas_nucleo()$Formato, nombre_especifico = "FmtO128",
                          expresion_regular = "^[0-9]{2}$")
  datos <- list(
    a1 = data.frame(cod = c("12", "ab", "34", "56"), stringsAsFactors = FALSE),
    b1 = data.frame(cod = c(NA, NA, "12", "xx"), stringsAsFactors = FALSE)
  )
  medicion <- medir(modelo(lapply(names(datos), function(t) instanciar(formato, t, "cod"))),
                    datos, id_medicion = "M", fecha = .fecha_O128)
  atributos <- agregar(medicion, "atributo", "ratio")
  parte <- function(t) agregar(atributos[atributos$entidad == t, ], "entidad", "promedio")
  conjunto <- agregar(rbind(parte("a1"), parte("b1")), "conjuntoEntidades", "promedio")
  alcance <- attr(conjunto, "alcance_medidas")
  # Antes: 3 de 5 -la entidad completa entraba como una celda-. A mano: 4 de 4
  # mas 2 de 4.
  expect_equal(alcance$medidas, 6)
  expect_equal(alcance$en_el_universo, 8)
})

test_that("la cobertura de metricas de otra coleccion no viaja con esta", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- list(a1 = data.frame(x = c(1, NA)), a3 = data.frame(x = numeric()),
                b1 = data.frame(x = c(1, NA)))
  medicion <- .medicion_O128(datos)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in names(datos)) DBI::dbWriteTable(con, n, datos[[n]])
  entidades <- agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")
  cB <- agregar(entidades[entidades$entidad == "b1", ], "coleccion",
                "promedio_ponderado", pesos = c(b1 = 1),
                coleccion = coleccion(con, "b1", nombre = "B"))
  # Antes: B traia "la entidad `a3` tiene cero filas", y una regla sin
  # `metricas` daba NA sobre una coleccion completa.
  expect_null(attr(cB, "cobertura_metricas"))
  evaluacion <- evaluar(cB, perfil_evaluacion("P", regla_evaluacion(
    "Media", function(x) x > 0.4
  )))
  expect_equal(evaluacion$reglas$resultado, 1)
})

test_that("la evaluacion y el historico nombran la tabla que no entro", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .entidades_O128()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  colecciones <- .colecciones_O128(con, base$datos)
  ent <- base$entidades
  cA <- agregar(ent[ent$entidad %in% c("a1", "a2"), ], "coleccion",
                "promedio_ponderado", pesos = c(a1 = 0.5, a2 = 0.5),
                coleccion = colecciones$A)
  evaluacion <- evaluar(cA, perfil_evaluacion("P", regla_evaluacion(
    "Alta", function(x) x > 0.5
  )))
  expect_identical(attr(evaluacion, "cobertura_coleccion")$tablas_sin_medir, "a3")
  impreso <- paste(capture.output(print(evaluacion)), collapse = "\n")
  expect_match(impreso, "a3", fixed = TRUE)
  historico <- historico_calidad(evaluacion)
  no_medidas <- historico[historico$nivel == "parte_no_medida", ]
  expect_identical(no_medidas$entidad, "a3")
  expect_true(is.na(no_medidas$resultado))
})

test_that("el nombre de lista manda en la organizacion y en el conjunto", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  base <- .entidades_O128()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in names(base$datos)) DBI::dbWriteTable(con, n, base$datos[[n]])
  colA <- coleccion(con, c("a1", "a2"), nombre = "base_a")
  colB <- coleccion(con, c("b1", "b2"), nombre = "base_b")
  ent <- base$entidades
  cA <- agregar(ent[ent$entidad %in% c("a1", "a2"), ], "coleccion",
                "promedio_ponderado", pesos = c(a1 = 0.5, a2 = 0.5), coleccion = colA)
  cB <- agregar(ent[ent$entidad %in% c("b1", "b2"), ], "coleccion",
                "promedio_ponderado", pesos = c(b1 = 0.5, b2 = 0.5), coleccion = colB)
  union <- rbind(cA, cB)
  renombradas <- list(padron = colA, tramites = colB)
  # Antes: abortaba -"no pertenecen a la organizacion"- con cualquier pesos.
  o <- agregar(union, "organizacion", "promedio_ponderado",
               pesos = c(padron = 0.5, tramites = 0.5),
               organizacion = organizacion("Org", renombradas))
  expect_equal(o$resultado, 0.5 * 0.75 + 0.5 * 0.625)
  por_objeto <- agregar(union, "organizacion", "promedio_ponderado",
                        pesos = c(base_a = 0.5, base_b = 0.5),
                        organizacion = organizacion("Org", renombradas))
  expect_equal(por_objeto$resultado, o$resultado)
  conjunto <- agregar(union, "conjuntoColecciones", "promedio_ponderado",
                      pesos = c(padron = 0.5, tramites = 0.5),
                      colecciones = renombradas)
  expect_equal(conjunto$resultado, o$resultado)
})

test_that("dos corridas no se agregan en silencio y el mensaje dice por que", {
  a <- .medicion_O128(list(t1 = data.frame(x = c(1, NA))), fecha = .fecha_O128)
  b <- .medicion_O128(list(t2 = data.frame(x = c(1, 2))),
                      fecha = as.POSIXct("2026-04-01", tz = "UTC"))
  union <- rbind(a, b)
  # Antes: "Las medidas deben compartir: fecha.", sin decir que hacer.
  expect_error(agregar(union, "atributo", "ratio"), "historico_calidad")
})

test_that("dos objetos distintos no comparten identificador ni celda", {
  medicion <- .medicion_O128(list(
    "a, b" = data.frame(x = c(1, 1, 1, NA)), c = data.frame(x = c(1, 1, 1, 1)),
    a = data.frame(x = c(NA, NA, NA, 1)), "b, c" = data.frame(x = c(NA, NA, NA, 1))
  ))
  entidades <- agregar(agregar(medicion, "atributo", "ratio"), "entidad", "promedio")
  conjunto <- function(partes) {
    agregar(entidades[entidades$entidad %in% partes, ], "conjuntoEntidades", "promedio")
  }
  uno <- conjunto(c("a, b", "c"))
  otro <- conjunto(c("a", "b, c"))
  # Antes: los dos se llamaban "a, b, c", con el mismo `id_medida`.
  expect_false(identical(uno$id_medida, otro$id_medida))
  expect_false(identical(uno$entidad, otro$entidad))
  expect_equal(nrow(tablero_calidad(rbind(uno, otro))), 2L)
})

test_that("los pesos que casan con dos vocabularios se rechazan", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- list(a = data.frame(x = c(1, 2, 3, 4)), b = data.frame(x = c(1, NA, NA, NA)))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (n in names(datos)) DBI::dbWriteTable(con, n, datos[[n]])
  ent <- agregar(agregar(.medicion_O128(datos), "atributo", "ratio"), "entidad", "promedio")
  # La coleccion `a` tiene la tabla `b` y la coleccion `b` la tabla `a`.
  ca <- agregar(ent[ent$entidad == "b", ], "coleccion", "promedio_ponderado",
                pesos = c(b = 1), coleccion = coleccion(con, "b", nombre = "a"))
  cb <- agregar(ent[ent$entidad == "a", ], "coleccion", "promedio_ponderado",
                pesos = c(a = 1), coleccion = coleccion(con, "a", nombre = "b"))
  # Antes: ganaba la tabla en silencio y publicaba 0.925 en vez de 0.325.
  expect_error(
    agregar(rbind(ca, cb), "organizacion", "promedio_ponderado",
            pesos = c(a = 0.9, b = 0.1), organizacion = organizacion("Org", c("a", "b"))),
    "reparten distinto"
  )
})

test_that("el mensaje de pesos de agregar dice cuando un nombre difiere en su forma", {
  nfc <- "educaci\u00f3n"
  nfd <- "educacio\u0301n"
  datos <- list(data.frame(x = c(1, NA)), data.frame(x = c(1, 2)))
  names(datos) <- c(nfc, "salud")
  entidades <- agregar(agregar(.medicion_O128(datos), "atributo", "ratio"),
                       "entidad", "promedio")
  pesos <- c(0.5, 0.5)
  names(pesos) <- c(nfd, "salud")
  # Antes: "Faltan pesos para: `educacion`. Sobran pesos para: `educacion`."
  expect_error(
    agregar(entidades, "conjuntoEntidades", "promedio_ponderado", pesos = pesos),
    "se ve igual"
  )
})

test_that("las claves del tablero y de las dependencias no pegan nombres con punto", {
  medidas <- data.frame(
    granularidad = "conjuntoAtributos", entidad = c("a.b", "a"),
    atributo = NA_character_, objeto_medible = c("c", "b.c"),
    stringsAsFactors = FALSE
  )
  # Antes: una sola clave para dos objetos distintos.
  expect_equal(length(unique(lupa:::.claves_objeto_tablero(medidas))), 2L)
  # La via de cardinalidad alta de las dependencias, forzada.
  testthat::local_mocked_bindings(
    .clave_dependencia_segura = function(k_x, k_y) FALSE, .package = "lupa"
  )
  pares <- lupa:::.codificar_parejas_dependencia(
    factor(c("a.b", "a")), c("c", "b.c")
  )
  expect_equal(length(unique(pares)), 2L)
})

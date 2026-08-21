.medida_coleccion <- function(entidad, resultado, granularidad = "coleccion") {
  salida <- data.frame(
    id_medicion = "m", fecha = as.POSIXct("2026-08-20", tz = "UTC"),
    metrica = "completitud", metrica_especifica = "NoNulo",
    metrica_instanciada = "NoNulo@t", dimension = "Completitud",
    factor = "Presencia", orientacion = "conformidad",
    granularidad = granularidad, tipo_resultado = "real", entidad = entidad,
    atributo = NA_character_, fila = NA_integer_, objeto_medible = entidad,
    resultado = resultado, agregacion = NA_character_, stringsAsFactors = FALSE
  )
  class(salida) <- c("medicion", "data.frame")
  salida
}

.unir_medidas <- function(...) {
  salida <- do.call(rbind, list(...))
  class(salida) <- c("medicion", "data.frame")
  salida
}

test_that("las diez granularidades del marco figuran implementadas", {
  catalogo <- granularidades()
  expect_equal(nrow(catalogo), 10L)
  expect_true(all(catalogo$implementada))
  expect_equal(
    catalogo$granularidad[9:10], c("organizacion", "conjuntoOrganizaciones")
  )
})

test_that("una organizacion es una declaracion y no necesita conexion", {
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  expect_s3_class(organismo, "organizacion_lupa")
  expect_equal(organismo$nombre, "MIDES")
  expect_equal(organismo$declaradas, c("padron", "tramites"))
  expect_equal(organismo$n_declaradas, 2L)
  # `cli` escribe por la via de mensajes, igual que los demas `print()` del
  # paquete; capturar stdout devolveria vacio.
  impreso <- capture.output(print(organismo), type = "message")
  expect_true(any(grepl("MIDES", impreso)))
  expect_true(any(grepl("no infiere", impreso)))
})

test_that("el nombre de la lista manda sobre el del objeto", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(a = 1:3))
  base <- coleccion(con, "t", nombre = "base_tecnica")
  expect_equal(organizacion("MIDES", list(base))$declaradas, "base_tecnica")
  expect_equal(
    organizacion("MIDES", list(padron_social = base))$declaradas,
    "padron_social"
  )
})

test_that("agregar a organizacion respeta la frontera declarada", {
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  medidas <- .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5)
  )
  agregado <- agregar(
    medidas, "organizacion", "promedio_ponderado", pesos = c(0.25, 0.75),
    organizacion = organismo
  )
  expect_equal(agregado$resultado, 0.6)
  expect_equal(agregado$entidad, "MIDES")
  expect_equal(agregado$granularidad, "organizacion")
  expect_equal(attr(agregado, "cobertura_organizacion")$cobertura, 1)
})

test_that("la cobertura de la organizacion declara lo que no entro al numero", {
  organismo <- organizacion("MIDES", c("padron", "tramites", "encuestas"))
  medidas <- .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5)
  )
  agregado <- agregar(
    medidas, "organizacion", "promedio_ponderado", pesos = c(0.5, 0.5),
    organizacion = organismo
  )
  cobertura <- attr(agregado, "cobertura_organizacion")
  expect_equal(cobertura$declaradas, 3L)
  expect_equal(cobertura$en_el_numero, 2L)
  expect_equal(cobertura$sin_medir, "encuestas")
  expect_equal(cobertura$cobertura, 2 / 3)
  expect_match(cobertura$advertencia, "no todas las declaradas")
})

test_that("un conjunto de organizaciones exige objetos de organizacion()", {
  mides <- organizacion("MIDES", c("padron", "tramites"))
  mtss <- organizacion("MTSS", "planillas")
  parcial <- function(org, medidas, pesos) {
    agregar(medidas, "organizacion", "promedio_ponderado", pesos = pesos,
            organizacion = org)
  }
  a <- parcial(mides, .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5)
  ), c(0.5, 0.5))
  b <- parcial(mtss, .medida_coleccion("planillas", 0.7), 1)
  conjunto <- .unir_medidas(a, b)

  agregado <- agregar(
    conjunto, "conjuntoOrganizaciones", "promedio_ponderado",
    pesos = c(0.6, 0.4), organizaciones = list(mides, mtss)
  )
  expect_equal(agregado$resultado, 0.7)
  expect_equal(agregado$granularidad, "conjuntoOrganizaciones")
  expect_equal(
    attr(agregado, "cobertura_conjunto_organizaciones")$declaradas, 2L
  )

  expect_error(
    agregar(conjunto, "conjuntoOrganizaciones", "promedio_ponderado",
            pesos = c(0.5, 0.5), organizaciones = list("MIDES", "MTSS")),
    "debe provenir de organizacion"
  )
})

test_that("sin frontera declarada se niega y dice como declararla", {
  medidas <- .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5)
  )
  # El nivel existe y es opcional: no se inventa una organizacion que nadie
  # nombro, y el mensaje tiene que decir por donde se declara.
  expect_error(
    agregar(medidas, "organizacion", "promedio_ponderado", pesos = c(0.5, 0.5)),
    "organizacion\\(nombre, colecciones\\)"
  )
  # El conjunto se alimenta de medidas de nivel organizacion: con medidas de
  # coleccion lo que falla primero es la transicion, que es otro error.
  de_organismos <- .unir_medidas(
    .medida_coleccion("MIDES", 0.7, granularidad = "organizacion"),
    .medida_coleccion("MTSS", 0.7, granularidad = "organizacion")
  )
  expect_error(
    agregar(de_organismos, "conjuntoOrganizaciones", "promedio_ponderado",
            pesos = c(0.5, 0.5)),
    "`organizaciones` de `agregar\\(\\)`"
  )
})

test_that("los dos niveles institucionales exigen pesos declarados", {
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  medidas <- .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5)
  )
  # Promediar organismos de tamano distinto sin declararlo es el mismo juicio
  # inventado que el paquete se niega a hacer un piso mas abajo.
  expect_error(
    agregar(medidas, "organizacion", "promedio", organizacion = organismo),
    "solo se admite 'promedio_ponderado'"
  )
})

test_that("una medida de fuera de la frontera no entra al numero", {
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  medidas <- .unir_medidas(
    .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.5),
    .medida_coleccion("base_ajena", 1)
  )
  expect_error(
    agregar(medidas, "organizacion", "promedio_ponderado",
            pesos = rep(1 / 3, 3), organizacion = organismo),
    "no pertenecen a la organizacion"
  )
})

test_that("una declaracion que no identifica sus partes se rechaza", {
  expect_error(organizacion("", "padron"), "texto no vacio")
  expect_error(organizacion("MIDES", character()), "lista no vacia")
  expect_error(organizacion("MIDES", c("a", "a")), "repite nombres")
  expect_error(organizacion("MIDES", list(1L)), "quedar identificado")
})

test_that("la cobertura de una parte incompleta no se pierde al subir", {
  # Un conjunto armado con una organizacion a la que le falto una coleccion no
  # esta completo, y decir cobertura 1 seria informar como completo lo que es
  # parcial: el mismo defecto que el paquete persigue, un piso mas arriba.
  a <- organizacion("A", c("c1", "c2", "c3"))
  b <- organizacion("B", "c4")
  parcial <- agregar(
    .unir_medidas(.medida_coleccion("c1", 0.9), .medida_coleccion("c2", 0.8)),
    "organizacion", "promedio_ponderado", pesos = c(0.5, 0.5), organizacion = a
  )
  expect_equal(attr(parcial, "cobertura_organizacion")$cobertura, 2 / 3)
  completa <- agregar(
    .medida_coleccion("c4", 1), "organizacion", "promedio_ponderado",
    pesos = 1, organizacion = b
  )

  conjunto <- agregar(
    .unir_medidas(parcial, completa), "conjuntoOrganizaciones",
    "promedio_ponderado", pesos = c(0.5, 0.5), organizaciones = list(a, b)
  )
  cobertura <- attr(conjunto, "cobertura_conjunto_organizaciones")
  expect_false(isTRUE(cobertura$completo))
  expect_true(length(cobertura$partes_incompletas) > 0L)
  expect_match(cobertura$advertencia, "venian")
  expect_true(!is.null(attr(conjunto, "cobertura_de_partes")))
})

test_that("el nombre de lista y el del objeto identifican a la misma parte", {
  # `agregar()` escribe el nombre del objeto y el nivel de arriba compara contra
  # el declarado: sin alias, la composicion documentada no corria.
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  medida <- agregar(
    .unir_medidas(
      .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.7)
    ),
    "organizacion", "promedio_ponderado", pesos = c(0.5, 0.5),
    organizacion = organismo
  )
  expect_equal(medida$entidad, "MIDES")
  agregado <- expect_no_error(agregar(
    medida, "conjuntoOrganizaciones", "promedio_ponderado", pesos = 1,
    organizaciones = list(Ministerio = organismo)
  ))
  cobertura <- attr(agregado, "cobertura_conjunto_organizaciones")
  expect_equal(cobertura$declaradas, 1L)
  expect_equal(cobertura$en_el_numero, 1L)
})

test_that("una parte con peso cero entra a la cobertura y se declara", {
  organismo <- organizacion("MIDES", c("padron", "tramites"))
  agregado <- agregar(
    .unir_medidas(
      .medida_coleccion("padron", 0.9), .medida_coleccion("tramites", 0.1)
    ),
    "organizacion", "promedio_ponderado", pesos = c(1, 0),
    organizacion = organismo
  )
  cobertura <- attr(agregado, "cobertura_organizacion")
  expect_equal(cobertura$partes_con_peso_cero, "tramites")
  expect_match(cobertura$advertencia, "peso cero")
})

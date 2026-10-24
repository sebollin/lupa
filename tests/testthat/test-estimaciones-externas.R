# `lupa` no estima: eso necesita diseño muestral, estimación de varianza y otra
# disciplina. Lo que sabe hacer es evaluar contra un marco declarado.
#
# El adaptador recibe estimaciones ya calculadas y las lleva al contrato de
# `medir()`. Cada estadístico se convierte en su propia medida canónica, porque
# los siete tienen unidades, dominios y orientaciones distintas: un coeficiente
# de variación de 0,30 y un tamaño de muestra de 0,30 no se leen igual.

.estimaciones_de_prueba <- function() {
  data.frame(
    celda = c("Montevideo", "Interior", "Rural"),
    stat = c(0.42, 0.38, 0.51),
    cv = c(0.08, 0.22, 0.34),
    n = c(1200L, 300L, 90L),
    stringsAsFactors = FALSE
  )
}

test_that("cada estadístico se convierte en su propia medida, con orientación", {
  medicion <- medicion_desde_estimaciones(
    .estimaciones_de_prueba(), entidad = "ech2024", atributo = "celda",
    fuente = "survey 4.4"
  )

  expect_s3_class(medicion, "medicion_calidad")
  expect_equal(nrow(medicion), 9L)
  expect_setequal(
    unique(medicion$metrica),
    c("Estimacion", "CoeficienteVariacion", "TamanoMuestra")
  )

  # La orientación es lo que permite evaluar: sin ella el número no se puede
  # leer. Un cv alto es defecto; un tamaño de muestra alto es conformidad.
  orientaciones <- unique(medicion[, c("metrica", "orientacion")])
  expect_equal(
    orientaciones$orientacion[orientaciones$metrica == "CoeficienteVariacion"],
    "defecto"
  )
  expect_equal(
    orientaciones$orientacion[orientaciones$metrica == "TamanoMuestra"],
    "conformidad"
  )
  # Una estimación puntual no es ni buena ni mala por sí misma.
  expect_equal(
    orientaciones$orientacion[orientaciones$metrica == "Estimacion"],
    "no_aplica"
  )
})

test_that("lo que la tabla no trae no se rellena: se declara ausente", {
  medicion <- medicion_desde_estimaciones(
    .estimaciones_de_prueba(), entidad = "ech2024", atributo = "celda",
    fuente = "survey 4.4"
  )
  expect_setequal(
    attr(medicion, "estadisticos_ausentes"), c("se", "df", "deff", "ess")
  )
  expect_false(any(medicion$metrica %in% c("ErrorEstandar", "GradosLibertad")))
})

test_that("la procedencia viaja en cada medida y es obligatoria", {
  medicion <- medicion_desde_estimaciones(
    .estimaciones_de_prueba(), entidad = "ech2024", atributo = "celda",
    fuente = "survey 4.4"
  )
  expect_equal(unique(medicion$fuente), "survey 4.4")
  expect_equal(attr(medicion, "fuente"), "survey 4.4")

  expect_error(
    medicion_desde_estimaciones(
      .estimaciones_de_prueba(), entidad = "ech2024", fuente = ""
    ),
    "de donde viene"
  )
})

test_that("las estimaciones se evalúan contra umbrales declarados", {
  # Reproduce la forma del estándar: el mismo conjunto evaluado con dos
  # umbrales distintos da veredictos distintos, sin que `lupa` estime nada.
  medicion <- medicion_desde_estimaciones(
    .estimaciones_de_prueba(), entidad = "ech2024", atributo = "celda",
    fuente = "survey 4.4"
  )
  evaluar_con <- function(maximo) {
    regla <- regla_evaluacion(
      "cv fiable", function(x, maximo) x <= maximo,
      metricas = "CoeficienteVariacion@ech2024",
      umbrales = list(maximo = maximo)
    )
    evaluar(medicion, perfil = perfil_evaluacion("cepal", regla))$reglas
  }

  estricto <- evaluar_con(0.15)
  intermedio <- evaluar_con(0.30)
  expect_equal(estricto$n_medidas, 3)
  expect_equal(estricto$resultado, 1 / 3)
  expect_equal(intermedio$resultado, 2 / 3)
})

test_that("se pueden renombrar las columnas de origen", {
  estimaciones <- data.frame(
    celda = c("a", "b"), coef_var = c(0.1, 0.4), casos = c(100L, 20L)
  )
  medicion <- medicion_desde_estimaciones(
    estimaciones, entidad = "t", atributo = "celda", fuente = "x",
    columnas = c(cv = "coef_var", n = "casos")
  )
  expect_setequal(
    unique(medicion$metrica), c("CoeficienteVariacion", "TamanoMuestra")
  )
  expect_equal(
    medicion$resultado[medicion$metrica == "CoeficienteVariacion"], c(0.1, 0.4)
  )
})

test_that("una tabla sin estadísticos reconocidos se rechaza", {
  expect_error(
    medicion_desde_estimaciones(
      data.frame(a = 1, b = 2), entidad = "t", fuente = "x"
    ),
    "ningun estadistico reconocido"
  )
  expect_error(
    medicion_desde_estimaciones(
      .estimaciones_de_prueba(), entidad = "t", fuente = "x",
      columnas = c(inventado = "cv")
    ),
    "no se reconocen"
  )
  expect_error(
    medicion_desde_estimaciones(
      .estimaciones_de_prueba(), entidad = "t", fuente = "x",
      atributo = "no_existe"
    ),
    "Disponibles"
  )
})

test_that("el catálogo declara los siete con su orientación y unidad", {
  catalogo <- estadisticos_estimacion()
  expect_equal(nrow(catalogo), 7L)
  expect_setequal(
    catalogo$estadistico, c("stat", "se", "cv", "n", "df", "deff", "ess")
  )
  expect_true(all(
    catalogo$orientacion %in% c("conformidad", "defecto", "no_aplica")
  ))
  expect_true(all(nzchar(catalogo$unidad)))
})

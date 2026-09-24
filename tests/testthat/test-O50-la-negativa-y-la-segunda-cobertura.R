# Dos silencios en las puertas de salida.
#
# 1. `aplicar()` se niega cuando el plan trae una accion eliminatoria sin
#    consentimiento, y esa negativa alcanza a TODO el plan: las acciones
#    benignas seleccionadas tampoco corren. El mensaje solo decia que hacia
#    falta el consentimiento, asi que "no se aplico nada" habia que deducirlo, y
#    el objeto que lo diria -el registro- no existe, porque la negativa ocurre
#    antes de ejecutar.
# 2. Un `perfil_dbi` tiene DOS coberturas y su impresion nombraba una. La que
#    contesta "y de lo demas, se miro todo?" -los diagnosticos no evaluados
#    sobre la muestra- quedaba invisible, porque `cobertura()` sobre ese objeto
#    devuelve la de metricas SQL.

test_that("la negativa por accion eliminatoria dice que no se aplico nada", {
  datos <- data.frame(col = c("a", NA, " b "), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  seleccion <- as.character(plan$estrategia) %in%
    c("eliminar_filas_ausentes", "recortar_espacios")
  expect_true(sum(seleccion) >= 2L)
  plan$aplicar <- seleccion

  mensaje <- tryCatch(
    {
      aplicar(plan, datos)
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_false(is.na(mensaje))
  expect_true(grepl("eliminar_filas_ausentes", mensaje, fixed = TRUE))
  expect_true(grepl("No se aplic", mensaje))
  expect_true(grepl("permitir_eliminacion", mensaje, fixed = TRUE))
  expect_true(grepl("aplicar\\[", mensaje))
  # Y lo que el mensaje afirma es cierto: los datos quedaron intactos.
  expect_identical(datos$col, c("a", NA, " b "))
})

test_that("con el consentimiento, las dos acciones corren y se registran", {
  # El control del caso anterior: si el plan fuera inaplicable por otra razon,
  # la comprobacion de arriba no distinguiria nada.
  datos <- data.frame(col = c("a", NA, " b "), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  plan <- planificar_limpieza(perfil, datos = datos)
  plan$aplicar <- as.character(plan$estrategia) %in%
    c("eliminar_filas_ausentes", "recortar_espacios")

  resultado <- aplicar(plan, datos, permitir_eliminacion = TRUE)
  registro <- as.data.frame(resultado$registro)

  expect_equal(nrow(registro), sum(plan$aplicar))
  expect_true(all(as.character(registro$estado) == "ejecutada"))
})

if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {

  test_that("el perfil DBI declara sus dos coberturas al imprimirse", {
    datos <- data.frame(
      id = 1:10,
      bandera = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
                  FALSE),
      monto = c(-9999, 10, 20, 30, -9999, 50, 60, 70, 80, 90)
    )
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "t", datos)
    perfil <- perfilar_dbi(con, "t", analizar_dependencias = FALSE)

    diagnosticos <- perfil$perfil_muestra$cobertura_diagnosticos
    expect_gt(nrow(diagnosticos), 0L)
    # `cobertura()` de un perfil DBI es la de las metricas SQL: otro esquema,
    # otra pregunta. Los diagnosticos viven en el bloque de la muestra.
    expect_true("bloque" %in% names(cobertura(perfil)))
    expect_true("diagnostico" %in% names(diagnosticos))

    salida <- NULL
    invisible(capture.output(salida <- cli::cli_fmt(print(perfil))))
    texto <- paste(salida, collapse = " ")
    expect_true(grepl("no evaluados sobre la muestra", texto))
    expect_true(grepl("cobertura_diagnosticos", texto, fixed = TRUE))
  })
}

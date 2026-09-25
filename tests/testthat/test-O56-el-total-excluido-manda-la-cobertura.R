# La promesa esta escrita sobre el NUMERO: "`n_valores_excluidos_resumen` cuenta
# todo valor presente que no sostiene el resumen, sea un periodo de mes o un
# texto que ningun formato pudo leer. Siempre que ese total sea mayor que cero,
# el perfil agrega una fila `resumen_cuantitativo` en `cobertura_diagnosticos`".
# La fila se decidia por una LISTA de dos estados -los del resumen parcial-, asi
# que el resumen que no se pudo calcular quedaba sin declarar: tres fechas
# ambiguas publicaban 3 excluidos y cobertura vacia, y una columna de periodos
# mensuales publicaba `NA` excluidos mientras contaba tres por granularidad.

perfil_o56 <- function(valores) {
  perfilar(data.frame(fecha = valores, stringsAsFactors = FALSE),
           analizar_dependencias = FALSE)
}

fila_resumen_o56 <- function(perfil) {
  cobertura <- cobertura(perfil)
  cobertura[as.character(cobertura$diagnostico) == "resumen_cuantitativo", ,
            drop = FALSE]
}

test_that("un total excluido mayor que cero siempre trae su fila", {
  casos <- list(
    ambiguas = c("03/04/2023", "05/06/2023", "07/08/2023"),
    ambiguas_con_mes = c("03/04/2023", "05/06/2023", "07/08/2023", "01/2023"),
    solo_meses = c("01/2023", "02/2023", "03/2023"),
    parcial = c("13/04/2023", "25/06/2023", "17/08/2023", "01/2023")
  )

  for (nombre in names(casos)) {
    perfil <- perfil_o56(casos[[nombre]])
    fila <- perfil$columnas
    excluidos <- fila$n_valores_excluidos_resumen[[1L]]
    expect_false(is.na(excluidos), info = nombre)
    expect_gt(excluidos, 0, label = nombre)
    expect_equal(nrow(fila_resumen_o56(perfil)), 1L, info = nombre)
  }
})

test_that("una columna sin excluidos no trae la fila", {
  # El control: si la fila se agregara siempre, esta comprobacion no
  # distinguiria nada.
  perfil <- perfil_o56(c("13/04/2023", "25/06/2023", "17/08/2023"))
  expect_equal(perfil$columnas$n_valores_excluidos_resumen[[1L]], 0)
  expect_equal(nrow(fila_resumen_o56(perfil)), 0L)
})

test_that("el estado no culpa a la granularidad cuando la causa es la ambiguedad", {
  # `granularidad_incompleta` esta definido para una columna de periodos
  # expresados SOLO como mes y anio. Con tres fechas de dia -aunque su formato
  # quede candidato- la columna no es eso.
  ambiguas <- perfil_o56(c("03/04/2023", "05/06/2023", "07/08/2023", "01/2023"))
  expect_identical(
    as.character(ambiguas$columnas$estado_resumen_cuantitativo), "sin_valores"
  )
  expect_identical(
    as.character(ambiguas$columnas$estado_tipo_inferido), "candidato"
  )
  # Y el periodo de mes sigue contado donde corresponde.
  expect_equal(ambiguas$columnas$n_fechas_excluidas_granularidad[[1L]], 1)

  solo_meses <- perfil_o56(c("01/2023", "02/2023", "03/2023"))
  expect_identical(
    as.character(solo_meses$columnas$estado_resumen_cuantitativo),
    "granularidad_incompleta"
  )
})

test_that("la fila dice que no hubo resumen cuando no lo hubo", {
  perfil <- perfil_o56(c("03/04/2023", "05/06/2023", "07/08/2023"))
  motivo <- fila_resumen_o56(perfil)$motivo[[1L]]

  expect_true(grepl("no se pudo calcular", motivo, fixed = TRUE))
  expect_false(grepl("se calculo sobre 0", motivo, fixed = TRUE))

  # Y cuando si hubo resumen parcial, lo dice con su cuenta.
  parcial <- perfil_o56(c("13/04/2023", "25/06/2023", "17/08/2023", "01/2023"))
  motivo_parcial <- fila_resumen_o56(parcial)$motivo[[1L]]
  expect_true(grepl("se calculo sobre 3", motivo_parcial, fixed = TRUE))
})

.comparar_formato_fecha <- function(obtenido, esperado) {
  expect_identical(names(obtenido), names(esperado))
  for (campo in names(obtenido)) {
    expect_identical(obtenido[[campo]], esperado[[campo]], info = campo)
  }
  nombres_atributos <- names(attributes(obtenido))
  expect_identical(nombres_atributos, names(attributes(esperado)))
  for (nombre in nombres_atributos) {
    expect_identical(
      attr(obtenido, nombre), attr(esperado, nombre), info = nombre
    )
  }
}

test_that("el detector de fechas pondera el vocabulario sin cambiar resultados", {
  set.seed(20260829L)
  formatos <- list(
    iso = format(as.Date("2020-01-01") + 0:20, "%Y-%m-%d"),
    iso_muchos = format(as.Date("2020-01-01") + 0:83, "%Y-%m-%d"),
    dmy = c("13/02/2020", "02/03/2020", "31/12/2021"),
    mdy = c("02/13/2020", "03/02/2020", "12/31/2021"),
    ambiguas = c("01/02/2020", "02/03/2020", "11/12/2021"),
    dos_digitos = c("13/02/20", "02/03/20", "31/12/21"),
    meses = c(
      "15 de marzo de 2024", "enero 2023", "March 5, 2024",
      "february 2023", "16-set-2022"
    ),
    horas = c(
      "2020-01-01 10:20", "2020-01-01 10:20:30",
      "2020-01-01T10:20:30Z", "2020-01-01T10:20:30-03:00"
    ),
    invalidas = c("2020-13-01", "31/02/2020", "25:00", "texto", ""),
    compactas = c("19850518", "20201231", "45031155"),
    libres = c("texto libre", "otra cosa", "sin fecha")
  )
  casos <- list(
    list(valores = as.Date(c("2020-01-01", NA)), muestra = 1e5),
    list(
      valores = as.POSIXct(c("2020-01-01", NA), tz = "UTC"), muestra = 1e5
    ),
    list(
      valores = as.POSIXct(
        c("2020-01-01 00:30", "2020-06-01 00:30"),
        tz = "America/Montevideo"
      ),
      muestra = 1e5
    ),
    list(valores = "2020-01-01", muestra = 1e5),
    list(valores = character(), muestra = 1e5)
  )
  for (i in seq_len(220L)) {
    nombres <- sample(names(formatos), sample(1:4, 1L))
    valores <- sample(unlist(formatos[nombres], use.names = FALSE),
                      sample(c(1L, 2L, 5L, 12L, 40L, 90L), 1L), TRUE)
    if (i %% 3L == 0L) {
      valores <- c(valores, sample(c(NA_character_, "", " texto "),
                                   sample(1:3, 1L), TRUE))
    }
    if (i %% 5L == 0L) {
      valores <- c(valores, sample(c("2020-02-30", "no es fecha"), 1L))
    }
    casos[[length(casos) + 1L]] <- list(
      valores = valores,
      muestra = if (i %% 7L == 0L) 30L else 1e5
    )
  }

  esperado <- lapply(casos, function(caso) {
    detectar_formatos_fecha(caso$valores, muestra = caso$muestra)
  })
  esperado_parseo <- lapply(seq_along(casos), function(i) {
    .parsear_fechas(casos[[i]]$valores, esperado[[i]])
  })
  casos_inferencia <- casos[c(1:4, seq(5L, length(casos), by = 11L))]
  esperado_inferencias <- lapply(casos_inferencia, function(caso) {
    inferir_tipo(caso$valores, muestra = caso$muestra)
  })

  # Este mock conserva el recorrido anterior, sin deduplicar ninguna forma.
  local_mocked_bindings(
    .vocabulario_texto = function(textos, umbral, valores = NULL) {
      list(
        valores = textos,
        indices = seq_along(textos),
        usar = FALSE,
        n_distintos = length(unique(textos[!is.na(textos)]))
      )
    },
    .package = "lupa"
  )
  referencia <- lapply(casos, function(caso) {
    detectar_formatos_fecha(caso$valores, muestra = caso$muestra)
  })
  referencia_parseo <- lapply(seq_along(casos), function(i) {
    .parsear_fechas(casos[[i]]$valores, referencia[[i]])
  })
  referencia_inferencias <- lapply(casos_inferencia, function(caso) {
    inferir_tipo(caso$valores, muestra = caso$muestra)
  })

  for (i in seq_along(casos)) {
    .comparar_formato_fecha(esperado[[i]], referencia[[i]])
    expect_identical(esperado_parseo[[i]], referencia_parseo[[i]],
                     info = paste("parseo caso", i))
  }
  for (i in seq_along(casos_inferencia)) {
    expect_identical(
      esperado_inferencias[[i]], referencia_inferencias[[i]],
      info = paste("inferencia caso", i)
    )
  }
})

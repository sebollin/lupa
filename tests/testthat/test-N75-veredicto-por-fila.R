# El veredicto de una fila lo decidian las OTRAS filas de la tanda. La regla
# unica que arreglo las cuatro columnas del veredicto calculaba su tolerancia
# con `max(abs(delta), abs(corte))`, y `delta` es un VECTOR: `max()` colapsa el
# grupo entero. Consecuencias medidas, las dos publicadas:
#   - un solo `delta` NA -una medicion sin evaluar- dejaba `significativo`,
#     `direccion`, `severidad` y `descripcion` en NA para TODOS los pares del
#     grupo, incluido un deterioro de 0,9 a 0,7 que es cuatro veces el umbral;
#   - la tolerancia de una fila salia del delta mas grande del grupo, asi que la
#     misma comparacion cambiaba de veredicto segun con quien viajara.
#
# La prueba no mira la aritmetica: compara la fila publicada con la fila que la
# misma comparacion publica cuando viaja sola.

.n75_armar_corridas <- function() {
  nucleo <- metricas_nucleo()
  par_a <- especializar(nucleo$NoNulo, nombre_especifico = "NNA")
  par_b <- especializar(nucleo$NoNulo, nombre_especifico = "NNB")
  modelo_dos <- function() {
    modelo(list(
      instanciar(par_a, "t", "dato"), instanciar(par_b, "t", "extra")
    ))
  }
  perfil <- perfil_evaluacion(
    "Operativo", regla_evaluacion("C", function(x) x > 0.6)
  )
  buenos <- data.frame(
    dato = c(rep("x", 9L), NA), extra = c(rep("y", 9L), NA),
    stringsAsFactors = FALSE
  )
  malos <- data.frame(
    dato = c(rep("x", 5L), rep(NA, 5L)), extra = c(rep("y", 9L), NA),
    stringsAsFactors = FALSE
  )
  corrida <- function(id, fecha, datos, inaplicable = FALSE) {
    argumentos <- list(
      modelo_dos(), datos, id_medicion = id,
      fecha = as.POSIXct(fecha, tz = "UTC")
    )
    if (inaplicable) {
      # Una sola metrica sin universo aplicable deja el resultado del perfil
      # sin evaluar. Con las dos, `medir()` rechaza la medicion entera y no
      # hay `NA` que contamine: el fixture comodo no muestra el defecto.
      argumentos$aplicabilidad <- list(extra = ~ extra == "no_existe")
    }
    suppressWarnings(evaluar(do.call(medir, argumentos), perfil))
  }
  list(
    buena = corrida("m1", "2026-01-31", buenos),
    mala = corrida("m2", "2026-02-28", malos),
    sin_evaluar = corrida("m3", "2026-03-31", malos, inaplicable = TRUE),
    mala_final = corrida("m4", "2026-04-30", malos)
  )
}

.n75_fila <- function(historico, anterior, actual) {
  deriva <- suppressWarnings(detectar_deriva_calidad(historico, umbral = 0.05))
  fila <- deriva[
    deriva$aspecto == "resultado" &
      deriva$id_medicion_anterior == anterior &
      deriva$id_medicion_actual == actual, , drop = FALSE
  ]
  fila
}

test_that("un par sin evaluar no contamina el veredicto de los otros pares", {
  c3 <- .n75_armar_corridas()

  solo <- .n75_fila(historico_calidad(c3$buena, c3$mala), "m1", "m2")
  skip_if(!nrow(solo), "la deriva no produjo el par m1->m2")

  # El mismo par, ahora acompanado por dos pares cuyo delta es NA.
  acompanado <- .n75_fila(
    historico_calidad(c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final),
    "m1", "m2"
  )
  expect_equal(nrow(acompanado), 1L)

  # El par cambio de verdad y se publica como tal, viaje solo o acompanado.
  expect_true(isTRUE(solo$significativo[[1L]]))
  expect_true(isTRUE(acompanado$significativo[[1L]]))
  expect_false(is.na(acompanado$direccion[[1L]]))
  expect_false(is.na(acompanado$severidad[[1L]]))
  expect_false(is.na(acompanado$descripcion[[1L]]))

  # Y las cuatro columnas del veredicto dicen exactamente lo mismo en los dos.
  for (columna in c("delta", "cambio_absoluto", "significativo", "direccion",
                    "severidad", "descripcion")) {
    expect_identical(
      acompanado[[columna]][[1L]], solo[[columna]][[1L]],
      info = paste("columna", columna)
    )
  }
})

test_that("la fila del par sin evaluar dice que no se puede comparar", {
  # El otro lado de la misma tanda: el par que SI tiene el NA no puede publicar
  # un veredicto, y tiene que decir de que lado falta el resultado.
  c3 <- .n75_armar_corridas()
  historico <- historico_calidad(
    c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final
  )
  hacia <- .n75_fila(historico, "m2", "m3")
  desde <- .n75_fila(historico, "m3", "m4")
  skip_if(!nrow(hacia) || !nrow(desde), "la deriva no produjo los pares con NA")

  expect_false(isTRUE(hacia$significativo[[1L]]))
  expect_false(isTRUE(desde$significativo[[1L]]))
  expect_match(as.character(hacia$descripcion[[1L]]), "actual no se evalu")
  expect_match(as.character(desde$descripcion[[1L]]), "anterior no se evalu")
})

test_that("un par que no se puede comparar no publica veredicto", {
  # La otra mitad del mismo defecto, y la mas cara: el par donde un resultado no
  # se evaluo publicaba `significativo = FALSE`, `direccion = "estable"` y
  # `severidad = "ok"` -tres afirmaciones de salud- al lado de su propia
  # descripcion, que decia "No se puede comparar". El usuario que filtra
  # `severidad != "ok"` para encontrar problemas no veia esas filas: lo no
  # medido quedaba contado entre lo sano, en una serie de monitoreo.
  # La convencion del paquete para lo desconocido ya estaba escrita -"los
  # conteos desconocidos son NA, nunca cero"- y es la que se aplica.
  c3 <- .n75_armar_corridas()
  historico <- historico_calidad(
    c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final
  )
  hacia <- .n75_fila(historico, "m2", "m3")
  desde <- .n75_fila(historico, "m3", "m4")
  skip_if(!nrow(hacia) || !nrow(desde), "la deriva no produjo los pares con NA")

  for (fila in list(hacia, desde)) {
    expect_true(is.na(fila$delta[[1L]]))
    expect_true(is.na(fila$significativo[[1L]]))
    expect_true(is.na(fila$direccion[[1L]]))
    expect_true(is.na(fila$severidad[[1L]]))
    # Y la descripcion, que ya era correcta, lo sigue siendo.
    expect_match(as.character(fila$descripcion[[1L]]), "No se puede comparar")
  }

  # La mitad de control: el par comparable del mismo historico conserva su
  # veredicto completo. Una guarda que apague el veredicto de todos no sirve.
  comparable <- .n75_fila(historico, "m1", "m2")
  expect_true(isTRUE(comparable$significativo[[1L]]))
  expect_identical(as.character(comparable$direccion[[1L]]), "deterioro")
  expect_identical(as.character(comparable$severidad[[1L]]), "error")
})

test_that("las tarjetas del informe suman las filas de la tabla", {
  # El informe resume la deriva en tres tarjetas contadas con `na.rm = TRUE`.
  # Cuando la deriva empezo a publicar `NA` en la severidad de un par que no se
  # puede comparar -que es lo correcto-, esas filas dejaron de contarse en
  # ningun lado: un informe de cinco filas mostraba "Errores 3, Sospechosos 0,
  # Correctos 0" y nada decia donde estaban las otras dos. Antes se contaban
  # entre las correctas, que era peor. La tarjeta "No evaluados" ya existia.
  c3 <- .n75_armar_corridas()
  deriva <- suppressWarnings(detectar_deriva_calidad(
    historico_calidad(c3$buena, c3$mala, c3$sin_evaluar, c3$mala_final),
    umbral = 0.05
  ))
  sin_veredicto <- sum(is.na(deriva$severidad))
  skip_if(sin_veredicto == 0L, "la deriva no produjo filas sin veredicto")

  tarjetas <- function(objeto) {
    ruta <- tempfile(fileext = ".html")
    on.exit(unlink(ruta), add = TRUE)
    invisible(reportar(objeto, archivo = ruta))
    html <- paste(readLines(ruta, warn = FALSE), collapse = "")
    etiquetas <- c("Errores", "Sospechosos", "Correctos", "No evaluados")
    conteos <- vapply(etiquetas, function(etiqueta) {
      patron <- paste0("<span>", etiqueta, "</span><strong>")
      posicion <- regexpr(patron, html, fixed = TRUE)
      if (posicion < 0L) return(NA_real_)
      resto <- substr(html, posicion + attr(posicion, "match.length"),
                      posicion + attr(posicion, "match.length") + 20L)
      as.numeric(sub("</strong>.*$", "", resto))
    }, numeric(1L))
    conteos
  }

  conteos <- tarjetas(deriva)
  expect_false(is.na(conteos[["No evaluados"]]))
  expect_equal(conteos[["No evaluados"]], as.numeric(sin_veredicto))
  expect_equal(sum(conteos, na.rm = TRUE), as.numeric(nrow(deriva)))

  # Control: una deriva sin filas incomparables no inventa la tarjeta.
  sin_na <- suppressWarnings(detectar_deriva_calidad(
    historico_calidad(c3$buena, c3$mala), umbral = 0.05
  ))
  skip_if(any(is.na(sin_na$severidad)), "el control trajo filas sin veredicto")
  conteos_control <- tarjetas(sin_na)
  expect_true(is.na(conteos_control[["No evaluados"]]))
  expect_equal(sum(conteos_control, na.rm = TRUE), as.numeric(nrow(sin_na)))
})

test_that("la clave declarada viaja al perfilado de cada grupo", {
  # `perfilar()` excluye de Benford la columna que se DECLARA clave, y publica
  # como motivo el hecho -"la clave fue declarada"- en vez de la inferencia.
  # `perfilar_por()` recibia `clave` como formal y nunca lo reenviaba, asi que en
  # el camino agrupado solo quedaba la guarda de la inferencia, que reconoce
  # claves densas. Una clave repartida en un rango ancho -lo normal en un
  # padron- perdia la forma de correlativo y el paquete publicaba
  # `desviacion_benford` sobre la columna que el usuario declaro que identifica
  # filas.
  set.seed(42)
  datos <- data.frame(
    entidad = sample(1:6000, 2000),
    atributo = rep(c("pais", "edad", "importe"), length.out = 2000),
    valor = sample(c("UY", "AR"), 2000, TRUE),
    stringsAsFactors = FALSE
  )
  benford_sobre <- function(hallazgos, columna) {
    h <- as.data.frame(hallazgos)
    if (!nrow(h)) return(0L)
    sum(!is.na(h$columna) & h$columna == columna &
          as.character(h$tipo_hallazgo) == "desviacion_benford")
  }

  con_clave <- suppressWarnings(perfilar_por(
    datos, "atributo", clave = "entidad", min_filas = 30L,
    analizar_dependencias = FALSE
  ))
  expect_equal(benford_sobre(con_clave, "entidad"), 0L)

  # Y el motivo publicado es el HECHO, no la deduccion: publicar "parece un
  # identificador" cuando la clave se declaro le atribuye al paquete una
  # inferencia que no hizo.
  cobertura <- as.data.frame(
    attr(con_clave, "cobertura_diagnosticos", exact = TRUE)
  )
  if (nrow(cobertura) && "columna" %in% names(cobertura)) {
    filas <- cobertura[!is.na(cobertura$columna) &
                         cobertura$columna == "entidad", , drop = FALSE]
    if (nrow(filas)) {
      expect_true(any(grepl("clave fue declarada", filas$motivo)))
    }
  }

  # La mitad de control, y sin esto el arreglo no vale: SIN declarar la clave, el
  # hallazgo tiene que seguir apareciendo. Una guarda que apaga Benford en todos
  # los casos arregla el falso positivo y silencia lo real.
  sin_clave <- suppressWarnings(perfilar_por(
    datos, "atributo", min_filas = 30L, analizar_dependencias = FALSE
  ))
  expect_gt(benford_sobre(sin_clave, "entidad"), 0L)
})

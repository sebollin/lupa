# La unicidad de la clave declarada se informaba con un aviso de consola, que no
# viaja: no tenia severidad, no llegaba al informe ni al plan, y no decia que
# filas repiten.

test_that("una clave declarada que repite es un hallazgo, no un aviso", {
  datos <- data.frame(
    IDpersona = c(1L, 2L, 3L, 3L, 5L, 5L), nombre = letters[1:6],
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, clave = "IDpersona", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  ))
  hallazgo <- perfil$hallazgos[
    as.character(perfil$hallazgos$tipo_hallazgo) == "clave_no_unica", ,
    drop = FALSE
  ]
  expect_equal(nrow(hallazgo), 1L)
  # Es un defecto, no una sospecha: la clave la declaro quien conoce la tabla.
  expect_equal(as.character(hallazgo$severidad[[1L]]), "error")
  expect_equal(hallazgo$n_afectados[[1L]], 4L)
  expect_match(as.character(hallazgo$evidencia[[1L]]), "2 valores repetidos")
  # Y dice cuales son las filas, que es lo que evita buscarlas a mano.
  traza <- hallazgo$trazabilidad[[1L]]
  expect_equal(as.character(traza$estado), "disponible")
  expect_equal(traza$indices_fila, c(3L, 4L, 5L, 6L))
})

test_that("una clave declarada que identifica no genera hallazgo", {
  datos <- data.frame(IDpersona = 1:6, nombre = letters[1:6],
                      stringsAsFactors = FALSE)
  perfil <- perfilar(
    datos, clave = "IDpersona", analizar_dependencias = FALSE,
    casi_duplicados_vocabulario = FALSE
  )
  expect_false(
    "clave_no_unica" %in% as.character(perfil$hallazgos$tipo_hallazgo)
  )
})

test_that("las sugerencias ponen primero la que identifica y se llama como clave", {
  set.seed(2)
  datos <- data.frame(
    edad = sample(18:80, 200L, replace = TRUE),
    id_persona = 1:200,
    id_hogar = rep(1:50, each = 4L),
    stringsAsFactors = FALSE
  )
  sug <- sugerir_clave(datos)
  expect_equal(sug$columna[[1L]], "id_persona")
  expect_true(sug$identifica[[1L]])
  # `id_hogar` repite cuatro veces cada valor: ni siquiera se acerca.
  expect_false("id_hogar" %in% sug$columna)
})

test_that("no repetir y no estar en todas las filas se informan distinto", {
  # Son dos problemas con arreglos distintos: uno son duplicados de carga y el
  # otro es una columna incompleta. Decir "el resto repite" sobre una columna
  # que no repite nada manda a buscar lo que no esta.
  # `casi` tiene que quedar por encima del umbral de 0,95 para que se informe
  # como candidata con duplicados; con cuatro filas y un repetido da 0,75 y se
  # descarta antes de llegar al motivo.
  datos <- data.frame(
    documento = c(seq_len(199L), NA),
    casi = c(seq_len(199L), 199L),
    stringsAsFactors = FALSE
  )
  sug <- sugerir_clave(datos)
  motivo_doc <- sug$motivo[sug$columna == "documento"]
  motivo_casi <- sug$motivo[sug$columna == "casi"]
  expect_match(motivo_doc, "no estan en todas las filas")
  expect_match(motivo_casi, "el resto repite")
  expect_false(grepl("el resto repite", motivo_doc, fixed = TRUE))
})

test_that("elegir_clave no pregunta ni elige sola fuera de una sesion interactiva", {
  # Un guion que corre solo no puede quedarse esperando, y elegir por su cuenta
  # seria justo lo que esta funcion existe para no hacer.
  datos <- data.frame(id_persona = 1:3, edad = c(30, 41, 25))
  expect_message(resultado <- elegir_clave(datos), "no interactiva")
  expect_null(resultado)
})

test_that("una clave escrita a mano se valida contra la tabla", {
  datos <- data.frame(id_persona = 1:3, edad = c(30, 41, 25))
  expect_equal(.clave_escrita("id_persona, edad", datos),
               c("id_persona", "edad"))
  expect_null(.clave_escrita("", datos))
  expect_message(resultado <- .clave_escrita("no_existe", datos), "No existe")
  expect_null(resultado)
})

# `.tipo_declarado()` enumeraba clases a mano y `is.double(x)` atrapaba a
# `difftime` antes de llegar al final de la funcion -que ya publicaba
# `class(x)[[1]]` para lo que el paquete no tiene palabra-. Una columna de
# duraciones en minutos publicaba `tipo_declarado = "doble"`, que no es lo que la
# columna declara de si misma; y el paquete lo sabia, porque dos predicados la
# excluyen de Benford y de las relaciones aritmeticas por
# `inherits(x, "difftime")`. El propio paquete tenia la leccion escrita en
# `hallazgos.R`: enumerar las clases a mano deja afuera la proxima.

test_that("una columna con clase propia publica su clase, no su almacenamiento", {
  tipo_declarado <- getFromNamespace(".tipo_declarado", "lupa")

  # El caso que fallaba, y sus hermanos: cualquier clase sobre `double`.
  expect_identical(tipo_declarado(as.difftime(1:5, units = "mins")), "difftime")

  # Y la mitad de control: las palabras del paquete no se mueven. Publicar la
  # clase cruda donde el paquete tiene un nombre propio seria el defecto espejo.
  esperados <- list(
    "doble" = 1.5, "entero" = 1L, "texto" = "a", "logico" = TRUE,
    "factor" = factor("a"), "factor-ordenado" = factor("a", ordered = TRUE),
    "fecha" = Sys.Date(), "fecha-hora" = Sys.time(),
    "lista" = list(1, 2), "matriz" = matrix(1:4, nrow = 2L)
  )
  for (palabra in names(esperados)) {
    expect_identical(tipo_declarado(esperados[[palabra]]), palabra, info = palabra)
  }

  # `AsIs` es como el dato viajo dentro del data.frame, no una declaracion sobre
  # el dato: `I(1:3)` sigue siendo entero y no pasa a "AsIs".
  expect_identical(tipo_declarado(I(1:3)), "entero")
  expect_identical(tipo_declarado(I(c(1.5, 2.5))), "doble")
})

test_that("una clase ajena al vocabulario no dispara tipo_declarado_distinto", {
  # El acoplamiento que el arreglo tenia que respetar: `tipo_declarado_distinto`
  # compara el tipo declarado con el inferido, y publicar la clase real haria
  # disparar ese hallazgo en TODA columna de duracion si la comparacion no
  # supiera que son dos afirmaciones sobre cosas distintas.
  datos <- data.frame(id = 1:40)
  datos$duracion <- as.difftime(seq_len(40L), units = "mins")
  perfil <- suppressWarnings(perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))
  columnas <- as.data.frame(perfil$columnas)
  fila <- columnas[columnas$columna == "duracion", , drop = FALSE]
  expect_identical(as.character(fila$tipo_declarado[[1L]]), "difftime")

  hallazgos <- as.data.frame(perfil$hallazgos)
  distintos <- hallazgos[
    !is.na(hallazgos$columna) & hallazgos$columna == "duracion" &
      as.character(hallazgos$tipo_hallazgo) == "tipo_declarado_distinto", ,
    drop = FALSE
  ]
  expect_equal(nrow(distintos), 0L)

  # Control: una discrepancia REAL sigue saliendo. Texto que en verdad son
  # fechas: declarado texto, inferido fecha.
  reales <- data.frame(
    f = format(as.Date("2024-01-01") + seq_len(40L)), stringsAsFactors = FALSE
  )
  hallazgos_reales <- as.data.frame(suppressWarnings(perfilar(
    reales, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  ))$hallazgos)
  expect_true(any(
    as.character(hallazgos_reales$tipo_hallazgo) == "tipo_declarado_distinto"
  ))
})

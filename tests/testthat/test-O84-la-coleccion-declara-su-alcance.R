# La coleccion tiene que declarar el alcance con el que calculo cada numero, y un
# objeto de otra generacion del esquema no puede matar la corrida entera.
#
# Tres defectos medidos, y uno de ellos con seis puertas:
#
#   1. `relaciones_coleccion()` no reenviaba `muestra` a `detectar_relaciones()`,
#      asi que con `muestra > 1e5` publicaba como evidencia las filas leidas y
#      calculaba la cobertura sobre un submuestreo que no declaraba. La fila se
#      contradecia a si misma.
#   2. El alcance de cada par salia como atributos del PRIMER par publicado, con
#      etiquetas que no nombran a ninguno, y el total de la tabla no estaba en
#      ninguna ruta del objeto.
#   3. Una coleccion sin una columna posterior a la primera version del objeto
#      mataba `perfilar_coleccion()` con un error de `data.frame()`.

skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

.o84_conexion <- function() {
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

test_that("la cobertura se calcula sobre las filas que el objeto dice haber leido", {
  skip_on_cran()
  # El caso se construye para que las dos respuestas difieran: las claves validas
  # caen justo en las posiciones que el submuestreo de 2e5 a 1e5 no toma.
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n <- 2e5
  i <- seq_len(n)
  claves <- ifelse(
    (i %% 2 == 0 & i <= 1e5) | (i %% 2 == 1 & i >= 100001),
    paste0("k", (i + 1) %/% 2), paste0("basura", i)
  )
  DBI::dbWriteTable(con, "personas", data.frame(
    clave = claves, stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "visitas", data.frame(
    ref = paste0("k", 1:1e5), stringsAsFactors = FALSE
  ))
  resultado <- relaciones_coleccion(
    coleccion(con, c("personas", "visitas")),
    data.frame(
      tabla_1 = "personas", tabla_2 = "visitas", stringsAsFactors = FALSE
    ),
    muestra = n
  )
  fila <- resultado$relaciones
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$filas_leidas_1, n)

  # La cobertura rehecha a mano sobre las MISMAS filas que la fila declara.
  d1 <- DBI::dbGetQuery(
    con, paste0("SELECT \"clave\" FROM \"personas\" LIMIT ", format(n, scientific = FALSE))
  )
  d2 <- DBI::dbGetQuery(
    con, paste0("SELECT \"ref\" FROM \"visitas\" LIMIT ", format(n, scientific = FALSE))
  )
  a_mano <- mean(d1$clave %in% unique(d2$ref))
  expect_equal(fila$cobertura_tabla1_en_tabla2, a_mano)
  # Y la cifra del defecto -0- ya no se publica: era la de la mitad de las filas.
  expect_gt(fila$cobertura_tabla1_en_tabla2, 0)
  # La fila tampoco se contradice: los comunes se contaron sobre la misma lectura.
  expect_equal(fila$n_valores_comunes, length(unique(d2$ref)) - 1)
})

test_that("cada par declara su propio alcance y el total de la tabla", {
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "a", data.frame(k = 1:5))
  DBI::dbWriteTable(con, "b", data.frame(k = 1:5))
  DBI::dbWriteTable(con, "g", data.frame(k = 1:2000))
  DBI::dbWriteTable(con, "grande", data.frame(k = 1:2000))
  resultado <- relaciones_coleccion(
    coleccion(con, c("a", "b", "g", "grande")),
    data.frame(
      tabla_1 = c("a", "g"), tabla_2 = c("b", "grande"),
      stringsAsFactors = FALSE
    ),
    muestra = 1000
  )
  relaciones <- resultado$relaciones
  expect_equal(nrow(relaciones), 2L)
  primero <- which(relaciones$tabla_1 == "a")
  segundo <- which(relaciones$tabla_1 == "g")

  # El par chico se leyo completo: el total es exacto y no hubo muestreo.
  expect_false(relaciones$muestreado_1[[primero]])
  expect_equal(relaciones$filas_totales_1[[primero]], 5)
  # El par grande se trunco: el total NO se sabe, y eso se declara en vez de
  # publicar el tope como si fuera el total.
  expect_true(relaciones$muestreado_1[[segundo]])
  expect_true(is.na(relaciones$filas_totales_1[[segundo]]))
  expect_equal(relaciones$filas_leidas_1[[segundo]], 1000)

  # Y los atributos que describian UN par se retiraron: eran los del primero
  # publicado y se leian como si describieran el objeto entero.
  for (sobrante in c(
    "filas_totales", "filas_analizadas", "muestreado", "n_pares_totales",
    "n_pares_comparados"
  )) {
    expect_null(
      attr(relaciones, sobrante, exact = TRUE),
      info = paste("sobrevive el atributo heredado", sobrante)
    )
  }
  # El conteo de pares vive donde cuenta todos.
  expect_equal(resultado$meta$pares_declarados, 2)
})

test_that("la bitacora de lecturas dice si la lectura quedo truncada", {
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "chica", data.frame(k = 1:10))
  DBI::dbWriteTable(con, "mediana", data.frame(k = 1:500))
  resultado <- relaciones_coleccion(
    coleccion(con, c("chica", "mediana")),
    data.frame(
      tabla_1 = "chica", tabla_2 = "mediana", stringsAsFactors = FALSE
    ),
    muestra = 100
  )
  lecturas <- resultado$meta$lecturas
  chica <- lecturas[grepl("chica", lecturas$tabla), , drop = FALSE]
  mediana <- lecturas[grepl("mediana", lecturas$tabla), , drop = FALSE]
  expect_equal(chica$filas_leidas, 10)
  expect_equal(chica$filas_totales, 10)
  expect_false(chica$muestreado)
  # La lectura truncada devuelve EXACTAMENTE el tope, no el tope mas uno: la fila
  # de sobra sirve para saber que hay mas y no viaja al resultado.
  expect_equal(mediana$filas_leidas, 100)
  expect_true(is.na(mediana$filas_totales))
  expect_true(mediana$muestreado)

  # El borde: una tabla con exactamente `muestra` filas NO esta muestreada, y su
  # total es exacto. Es el caso donde el tope y el total coinciden, que con el
  # conteo viejo era indistinguible de una lectura truncada.
  DBI::dbWriteTable(con, "justa", data.frame(k = 1:100))
  DBI::dbWriteTable(con, "otra", data.frame(k = 1:100))
  borde <- relaciones_coleccion(
    coleccion(con, c("justa", "otra")),
    data.frame(tabla_1 = "justa", tabla_2 = "otra", stringsAsFactors = FALSE),
    muestra = 100
  )
  fila_justa <- borde$meta$lecturas[
    grepl("justa", borde$meta$lecturas$tabla), , drop = FALSE
  ]
  expect_equal(fila_justa$filas_leidas, 100)
  expect_equal(fila_justa$filas_totales, 100)
  expect_false(fila_justa$muestreado)
})

test_that("una coleccion de otra generacion del esquema se perfila igual", {
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:5))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1, 1, 2)))
  declarada <- coleccion(con, c("personas", "visitas"), nombre = "vieja")

  # El recorrido: se quita CADA columna, de a una, y se exige que la coleccion
  # siga perfilandose. El informe traia una sola -`tipo`- y el recorrido encontro
  # cinco mas, incluida `referencia`, cuya ausencia perfilaba CERO tablas sin
  # abortar, que es peor que abortar.
  completables <- setdiff(
    names(declarada$tablas), lupa:::.COLUMNAS_EXIGIDAS_COLECCION
  )
  expect_gte(length(completables), 6L)
  ejercidas <- 0L
  for (columna in completables) {
    recortada <- declarada
    recortada$tablas[[columna]] <- NULL
    perfilada <- suppressWarnings(perfilar_coleccion(recortada))
    ejercidas <- ejercidas + 1L
    expect_equal(
      nrow(perfilada$resumen_coleccion), 2L,
      info = paste("sin la columna", columna, "no se perfilaron las dos tablas")
    )
  }
  # Se cuenta lo ejercido: un recorrido que no recorrio nada pasaria igual.
  expect_equal(ejercidas, length(completables))
})

test_that("lo que no se puede completar se rechaza nombrando la columna", {
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:5))
  declarada <- coleccion(con, "personas", nombre = "sin_nombre")
  declarada$tablas$tabla <- NULL
  # El motivo nombra la columna. Antes el error venia de `data.frame()` -"los
  # argumentos implican un numero diferente de filas"- y no nombraba ni la
  # columna ni la tabla.
  expect_error(perfilar_coleccion(declarada), "`tabla`")
  expect_error(perfilar_coleccion(declarada), "no se puede completar")
})

test_that("lo derivado se deriva siempre, aunque el objeto traiga una copia vieja", {
  # `identificador` y `referencia` son consecuencia del nombre -`catalogo`,
  # `esquema`, `tabla`-, y una consecuencia guardada no puede ganarle a la
  # declaracion de la que sale. Antes se recalculaban solo cuando FALTABAN, asi
  # que una copia desactualizada se usaba tal cual: la fila publicaba
  # `tabla = t1` con las filas de t2, o sea identificaba una tabla y traia los
  # datos de otra, en silencio.
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t1", data.frame(x = 1:3))
  DBI::dbWriteTable(con, "t2", data.frame(x = 1:7))
  declarada <- coleccion(con, c("t1", "t2"))

  # Control primero: sobre un objeto sano, derivar no cambia nada.
  sana <- suppressWarnings(perfilar_coleccion(declarada))
  filas_sanas <- as.data.frame(sana$resumen_coleccion)
  expect_equal(
    filas_sanas$n_filas[filas_sanas$tabla == "t1"], 3
  )
  expect_equal(
    filas_sanas$n_filas[filas_sanas$tabla == "t2"], 7
  )

  una_tabla <- function() {
    recortada <- declarada
    recortada$tablas <- recortada$tablas[1, , drop = FALSE]
    recortada
  }
  # `referencia` apuntando a otra tabla.
  con_referencia_vieja <- una_tabla()
  con_referencia_vieja$tablas$referencia <- I(list(
    declarada$tablas$referencia[[2L]]
  ))
  perfilada <- suppressWarnings(perfilar_coleccion(con_referencia_vieja))
  publicada <- as.data.frame(perfilada$resumen_coleccion)
  expect_equal(publicada$tabla, "t1")
  # La cifra que importa: 3 son las filas de t1; 7 serian las de t2.
  expect_equal(publicada$n_filas, 3)

  # `identificador` inconsistente con el nombre.
  con_identificador_viejo <- una_tabla()
  con_identificador_viejo$tablas$identificador <- "viejo.t1"
  perfilada2 <- suppressWarnings(
    perfilar_coleccion(con_identificador_viejo)
  )
  publicada2 <- as.data.frame(perfilada2$resumen_coleccion)
  expect_equal(publicada2$tabla, "t1")
  expect_equal(publicada2$n_filas, 3)
})

test_that("el constructor y el adaptador derivan con la misma funcion", {
  # Tenian cada uno su copia y ya habian divergido -una armaba `referencia` sin
  # el `I()` de la otra-. La prueba fija que derivar sobre lo que el constructor
  # produjo es identico a lo que el constructor produjo: si alguien vuelve a
  # escribir la regla dos veces, esto falla.
  con <- .o84_conexion()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mi tabla", data.frame(x = 1:2))
  DBI::dbWriteTable(con, "otra", data.frame(x = 1:2))
  declarada <- coleccion(con, c("mi tabla", "otra"))
  vuelta <- lupa:::.derivar_columnas_coleccion(declarada$tablas)
  expect_identical(
    as.character(vuelta$identificador),
    as.character(declarada$tablas$identificador)
  )
  expect_identical(
    lapply(vuelta$referencia, identity),
    lapply(declarada$tablas$referencia, identity)
  )
})

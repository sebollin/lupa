# Dos silencios de la capa institucional.
#
# 1. La medicion agregada a `coleccion` LLEVA la cobertura de esa coleccion como
#    atributo y su impresion la tiraba: sobre tres tablas declaradas con una que
#    no se pudo leer, la fila publica `entidad = tres_tablas` y el promedio de
#    DOS, con `advertencia_agregacion = NA`. El indice ya imprimia esa cobertura y
#    el tablero se arreglo en su vuelta; esta era la tercera capa con la misma
#    pregunta, y la primera que se mira despues de agregar.
# 2. Los pesos se emparejaban solo contra `objeto_medible`, que en los niveles
#    altos es la lista de partes unida con coma -una construccion interna-. Los
#    pesos escritos con los nombres que `organizacion(colecciones = )` exige se
#    rechazaban, y el error los enumeraba separados por coma: dos partes llamadas
#    `t1, t3` y `u1, u2` se leian como cuatro.

.o58_medidas_por_tabla <- function(tablas, id = "o58") {
  nucleo <- metricas_nucleo()
  instancias <- lapply(tablas, function(t) {
    instanciar(especializar(nucleo$NoNulo), t, "x")
  })
  datos <- stats::setNames(
    lapply(tablas, function(t) data.frame(x = c(1, NA, 3, 4))), tablas
  )
  medidas <- medir(modelo(instancias), datos, id_medicion = id)
  agregar(agregar(medidas, "atributo", "ratio"), "entidad", "promedio")
}

.o58_subconjunto <- function(x, tablas) {
  y <- x[x$entidad %in% tablas, , drop = FALSE]
  class(y) <- class(x)
  for (a in setdiff(names(attributes(x)), c("names", "row.names", "class"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}

if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {

  test_that("la medicion agregada declara la cobertura de su coleccion", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "t1", data.frame(x = c(1, NA, 3, 4)))
    DBI::dbWriteTable(con, "t3", data.frame(x = c(NA, 2)))
    coleccion_declarada <- coleccion(
      con, c("t1", "t2_ausente", "t3"), nombre = "tres_tablas"
    )
    perfil <- suppressWarnings(perfilar_coleccion(coleccion_declarada))

    entidades <- .o58_medidas_por_tabla(c("t1", "t3"))
    agregada <- agregar(
      entidades, "coleccion", "promedio_ponderado",
      pesos = c(t1 = 0.5, t3 = 0.5), coleccion = perfil
    )
    cobertura <- attr(agregada, "cobertura_coleccion", exact = TRUE)
    expect_equal(cobertura$tablas_declaradas, 3)
    expect_equal(cobertura$tablas_en_el_numero, 2)

    salida <- NULL
    invisible(capture.output(salida <- cli::cli_fmt(print(agregada))))
    texto <- paste(salida, collapse = " ")
    expect_true(grepl("Cobertura de la colecci", texto))
    expect_true(grepl("t2_ausente", texto, fixed = TRUE))
    expect_true(grepl("no la coleccion declarada", texto, fixed = TRUE))
  })

  test_that("una coleccion sin tablas caidas no anuncia cobertura parcial", {
    # El control: si la impresion anunciara siempre, no distinguiria nada.
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWriteTable(con, "t1", data.frame(x = c(1, NA, 3, 4)))
    DBI::dbWriteTable(con, "t3", data.frame(x = c(NA, 2)))
    perfil <- suppressWarnings(perfilar_coleccion(
      coleccion(con, c("t1", "t3"), nombre = "dos_tablas")
    ))
    agregada <- agregar(
      .o58_medidas_por_tabla(c("t1", "t3")), "coleccion",
      "promedio_ponderado", pesos = c(t1 = 0.5, t3 = 0.5), coleccion = perfil
    )
    cobertura <- attr(agregada, "cobertura_coleccion", exact = TRUE)
    expect_equal(cobertura$tablas_declaradas, cobertura$tablas_en_el_numero)
    expect_equal(length(cobertura$tablas_sin_medir), 0L)
  })

  test_that("los pesos aceptan el nombre que la frontera declara", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    for (tabla in c("t1", "t3", "u1", "u2")) {
      DBI::dbWriteTable(con, tabla, data.frame(x = c(1, NA, 3, 4)))
    }
    col_a <- coleccion(con, c("t1", "t3"), nombre = "padron_a")
    col_b <- coleccion(con, c("u1", "u2"), nombre = "padron_b")
    perfil_a <- suppressWarnings(perfilar_coleccion(col_a))
    perfil_b <- suppressWarnings(perfilar_coleccion(col_b))

    entidades <- .o58_medidas_por_tabla(c("t1", "t3", "u1", "u2"))
    por_coleccion <- rbind(
      as.data.frame(agregar(
        .o58_subconjunto(entidades, c("t1", "t3")), "coleccion",
        "promedio_ponderado", pesos = c(t1 = 0.5, t3 = 0.5),
        coleccion = perfil_a
      )),
      as.data.frame(agregar(
        .o58_subconjunto(entidades, c("u1", "u2")), "coleccion",
        "promedio_ponderado", pesos = c(u1 = 0.5, u2 = 0.5),
        coleccion = perfil_b
      ))
    )
    class(por_coleccion) <- c("medicion", "data.frame")
    # Las dos identidades de la misma medida: la declarada y la interna.
    expect_setequal(por_coleccion$entidad, c("padron_a", "padron_b"))
    expect_setequal(por_coleccion$objeto_medible, c("t1, t3", "u1, u2"))

    org <- organizacion("Organismo X", colecciones = list(
      padron_a = col_a, padron_b = col_b
    ))
    declarados <- agregar(
      por_coleccion, "organizacion", "promedio_ponderado",
      pesos = c(padron_a = 0.5, padron_b = 0.5), organizacion = org
    )
    internos <- agregar(
      por_coleccion, "organizacion", "promedio_ponderado",
      pesos = stats::setNames(c(0.5, 0.5), por_coleccion$objeto_medible),
      organizacion = org
    )
    expect_equal(
      as.data.frame(declarados)$resultado, as.data.frame(internos)$resultado
    )

    # El control: un nombre que no es ninguna de las dos identidades se rechaza,
    # y el mensaje entrecomilla cada parte para que `t1, t3` no se lea como dos.
    expect_error(
      agregar(por_coleccion, "organizacion", "promedio_ponderado",
              pesos = c(padron_a = 0.5, inventado = 0.5), organizacion = org),
      "`inventado`"
    )
    mensaje <- tryCatch(
      {
        agregar(por_coleccion, "organizacion", "promedio_ponderado",
                pesos = c(padron_a = 0.5, inventado = 0.5),
                organizacion = org)
        NA_character_
      },
      error = function(e) conditionMessage(e)
    )
    expect_true(grepl("`t1, t3`", mensaje, fixed = TRUE))
    expect_true(grepl("Se aceptan los nombres", mensaje, fixed = TRUE))
  })
}

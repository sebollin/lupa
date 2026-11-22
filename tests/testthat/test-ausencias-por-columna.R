# `sentinelas_numericos` y `cadenas_ausencia` aceptan una lista con nombre
# columna -> valores. Cada entrada vale para su columna lo que el vector valdria
# para ella, y las columnas que la lista no nombra reciben el valor por omision.
# Un `99` puede ser "no sabe" en una columna y un valor legitimo en la de al lado:
# aplanar la lista aplicaba los valores de una columna a todas.

testthat::skip_on_cran()

datos_por_columna <- function(semilla = 11, veces = 1L) {
  set.seed(semilla)
  n <- 30L * veces
  data.frame(
    x = c(rep(99, 6L * veces), sample(1:50, n - 6L * veces, TRUE)),
    y = c(rep(99, 4L * veces), sample(c(1:98, 100:120), n - 4L * veces, TRUE)),
    t = c(rep("NC", 5L * veces), sample(c("a", "b", "c"), n - 5L * veces, TRUE)),
    stringsAsFactors = FALSE
  )
}

fecha_fija <- as.POSIXct("2026-01-01", tz = "UTC")

fila_columna <- function(perfil, columna) {
  fila <- perfil$columnas[perfil$columnas$columna == columna, , drop = FALSE]
  rownames(fila) <- NULL
  fila
}

test_that("la fila de una columna declarada es la de esa columna con el vector", {
  d <- datos_por_columna()
  lista <- perfilar(
    d, sentinelas_numericos = list(x = 99), cadenas_ausencia = list(t = "NC"),
    fecha = fecha_fija
  )
  con_vector_x <- perfilar(d, sentinelas_numericos = 99, fecha = fecha_fija)
  con_vector_t <- perfilar(d, cadenas_ausencia = "NC", fecha = fecha_fija)
  por_omision <- perfilar(d, fecha = fecha_fija)
  expect_identical(fila_columna(lista, "x"), fila_columna(con_vector_x, "x"))
  expect_identical(fila_columna(lista, "t"), fila_columna(con_vector_t, "t"))
  # La columna que la lista no nombra recibe el valor por omision, no el 99.
  expect_identical(fila_columna(lista, "y"), fila_columna(por_omision, "y"))
  expect_identical(fila_columna(lista, "x")$n_faltantes_disfrazados, 6L)
  expect_identical(fila_columna(lista, "y")$n_faltantes_disfrazados, 0L)
})

test_that("el perfil guarda la declaracion por columna y nada nuevo con vectores", {
  d <- datos_por_columna()
  lista <- perfilar(d, sentinelas_numericos = list(x = c(99, 98)), fecha = fecha_fija)
  expect_identical(
    lista$meta$ausencias_por_columna$sentinelas_numericos, list(x = c(98, 99))
  )
  expect_identical(lista$meta$sentinelas_numericos, c(-9999, -999, -99, -9, 999))
  vector <- perfilar(d, sentinelas_numericos = 99, fecha = fecha_fija)
  expect_false("ausencias_por_columna" %in% names(vector$meta))
  expect_identical(vector$meta$sentinelas_numericos, 99)
})

test_that("un nombre que la tabla no tiene es un error", {
  d <- datos_por_columna()
  expect_error(
    perfilar(d, sentinelas_numericos = list(z = 99)), "nombra columnas inexistentes"
  )
  expect_error(
    perfilar(d, cadenas_ausencia = list(z = "NC")), "nombra columnas inexistentes"
  )
  expect_error(
    perfilar(d, sentinelas_numericos = list(99)), "debe nombrar columnas"
  )
  expect_error(
    perfilar(d, sentinelas_numericos = list(x = "99")), "vector num"
  )
})

test_that("el plan convierte el 99 de la columna declarada y no el de la otra", {
  d <- datos_por_columna()
  perfil <- perfilar(d, sentinelas_numericos = list(x = 99), fecha = fecha_fija)
  plan <- planificar_limpieza(perfil, d)
  centinelas <- plan[plan$estrategia == "convertir_sentinelas_numericos", ]
  expect_identical(as.character(centinelas$columna), "x")
  expect_identical(centinelas$parametros[[1L]]$valores, 99)
  plan$aplicar <- plan$estado == "lista" &
    plan$estrategia %in% "convertir_sentinelas_numericos"
  aplicado <- aplicar(plan, d)
  expect_identical(sum(is.na(aplicado$datos$x)), 6L)
  expect_identical(sum(aplicado$datos$y == 99), sum(d$y == 99))
})

test_that("las relaciones entre columnas usan la entrada de cada una", {
  d <- data.frame(
    x = c(rep(99, 10), rep(1:20, 2)), y = c(1:10, rep(1:20, 2)),
    stringsAsFactors = FALSE
  )
  # Declarar 99 en `x` saca esas filas de la relacion y `x` determina `y`;
  # declararlo en `y`, que no lo tiene, deja el 99 de `x` como un valor que
  # rompe la dependencia.
  solo_x <- detectar_dependencias(d, sentinelas_numericos = list(x = 99))
  solo_y <- detectar_dependencias(d, sentinelas_numericos = list(y = 99))
  vector <- detectar_dependencias(d, sentinelas_numericos = 99)
  expect_false(identical(solo_x, solo_y))
  expect_identical(solo_x, vector)
  expect_identical(
    detectar_dependencias(d, sentinelas_numericos = list()),
    detectar_dependencias(d)
  )
  expect_error(
    detectar_dependencias(d, sentinelas_numericos = list(z = 99)),
    "nombra columnas inexistentes"
  )
})

disfrazados_por_columna <- function(hallazgos) {
  h <- hallazgos[hallazgos$tipo_hallazgo == "faltantes_disfrazados", , drop = FALSE]
  as.character(h$columna)
}

test_that("perfilar_por() recorta la entrada de una columna que la rebanada no tiene", {
  # Grupos de 60 filas: `perfilar_por()` no perfila grupos de menos de 30.
  d <- cbind(g = rep(c("A", "B"), 60), datos_por_columna(veces = 4L),
             stringsAsFactors = FALSE)
  pp <- perfilar_por(
    d, "g", sentinelas_numericos = list(x = 99, g = 1),
    analizar_dependencias = FALSE
  )
  expect_s3_class(pp, "hallazgos_por_grupo")
  recortes <- attr(pp, "cobertura_grupos")
  expect_true(any(grepl("sentinelas_numericos", unlist(recortes), fixed = TRUE)))
  vector <- perfilar_por(
    d, "g", sentinelas_numericos = 99, analizar_dependencias = FALSE
  )
  expect_true("x" %in% disfrazados_por_columna(pp))
  expect_false("y" %in% disfrazados_por_columna(pp))
  expect_true("y" %in% disfrazados_por_columna(vector))
})

test_that("perfilar_dbi() resuelve la lista contra la tabla en el resumen y en la muestra", {
  skip_if_not_installed("RSQLite")
  d <- datos_por_columna()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", d)
  lista <- suppressWarnings(perfilar_dbi(
    con, "d", sentinelas_numericos = list(x = 99), cadenas_ausencia = list(t = "NC")
  ))
  vector <- suppressWarnings(perfilar_dbi(
    con, "d", sentinelas_numericos = 99, cadenas_ausencia = "NC"
  ))
  muestra <- lista$perfil_muestra$columnas
  expect_identical(muestra$n_faltantes_disfrazados[muestra$columna == "x"], 6L)
  expect_identical(muestra$n_faltantes_disfrazados[muestra$columna == "y"], 0L)
  expect_identical(muestra$n_faltantes_disfrazados[muestra$columna == "t"], 5L)
  # Con el vector, el 99 de `y` tambien cuenta.
  m_vector <- vector$perfil_muestra$columnas
  expect_gt(m_vector$n_faltantes_disfrazados[m_vector$columna == "y"], 0L)
  expect_error(
    perfilar_dbi(con, "d", sentinelas_numericos = list(z = 99)),
    class = "lupa_error_argumento_dbi"
  )
  plan <- plan_perfilado_dbi(con, "d", sentinelas_numericos = list(x = 99))
  expect_s3_class(plan, "data.frame")
})

test_that("perfilar_coleccion() acepta la lista por tabla y rechaza una por columna", {
  skip_if_not_installed("RSQLite")
  d <- datos_por_columna()
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "uno", d)
  DBI::dbWriteTable(con, "dos", d)
  col <- coleccion(con, c("uno", "dos"))
  pc <- suppressWarnings(perfilar_coleccion(
    col, conservar_perfiles = TRUE,
    sentinelas_numericos = list(uno = list(x = 99))
  ))
  disfrazados <- function(tabla, columna) {
    m <- pc$perfiles[[tabla]]$perfil_muestra$columnas
    m$n_faltantes_disfrazados[m$columna == columna]
  }
  expect_identical(disfrazados("uno", "x"), 6L)
  expect_identical(disfrazados("uno", "y"), 0L)
  # La tabla que la lista no nombra recibe el valor por omision.
  expect_identical(disfrazados("dos", "x"), 0L)
  expect_error(
    perfilar_coleccion(col, sentinelas_numericos = list(x = 99)),
    "anidar", class = "lupa_error_argumento_dbi"
  )
})

test_that("comparar_perfiles() declara el cambio de politica de una columna", {
  d <- datos_por_columna()
  antes <- perfilar(d, fecha = fecha_fija)
  despues <- perfilar(d, sentinelas_numericos = list(x = 99), fecha = fecha_fija)
  cmp <- comparar_perfiles(antes, despues)
  fila <- cmp[cmp$aspecto == "configuracion_sentinelas_numericos", , drop = FALSE]
  expect_identical(as.character(fila$columna), "x")
  # Con vectores iguales no aparece ninguna fila de politica.
  igual <- comparar_perfiles(antes, perfilar(d, fecha = fecha_fija))
  expect_false(any(grepl("^configuracion_(sentinelas|cadenas)", igual$aspecto)))
})

test_that("analizar() lleva la declaracion por columna a las asociaciones", {
  d <- datos_por_columna()
  a <- analizar(d, argumentos_perfil = list(sentinelas_numericos = list(x = 99)))
  expect_identical(
    a$perfil$meta$ausencias_por_columna$sentinelas_numericos, list(x = 99)
  )
})

# La guarda que cierra el defecto: un lector que recibe la lista sin
# seleccionar la entrada de su columna aborta, en vez de aplanarla.
test_that("los normalizadores de bajo nivel rechazan una lista sin resolver", {
  expect_error(
    lupa:::.normalizar_sentinelas_numericos(list(x = 99, y = 77)),
    class = "lupa_error_interno"
  )
  expect_error(
    lupa:::.sentinelas_numericos_declarados(list(x = 99)),
    class = "lupa_error_interno"
  )
  expect_error(
    lupa:::.cadenas_ausencia_columna("a", list(t = "NC")),
    class = "lupa_error_interno"
  )
})

# Y el recorrido: cada puerta que recibe la declaracion, con la entrada en una
# columna, deja a la otra como con el valor por omision. Si algun lector la
# aplanara, el 99 de `y` contaria.
test_that("ninguna puerta aplica la entrada de una columna a otra", {
  skip_if_not_installed("RSQLite")
  d <- datos_por_columna(semilla = 23)
  lista <- list(x = 99)
  y_de <- function(columnas) columnas$n_faltantes_disfrazados[columnas$columna == "y"]
  p <- perfilar(d, sentinelas_numericos = lista, fecha = fecha_fija)
  expect_identical(y_de(p$columnas), 0L)
  pp <- perfilar_por(
    cbind(g = rep(c("A", "B"), 60), datos_por_columna(semilla = 23, veces = 4L)),
    "g", sentinelas_numericos = lista, analizar_dependencias = FALSE
  )
  expect_true("x" %in% disfrazados_por_columna(pp))
  expect_false("y" %in% disfrazados_por_columna(pp))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", d)
  pd <- suppressWarnings(perfilar_dbi(con, "d", sentinelas_numericos = lista))
  expect_identical(y_de(pd$perfil_muestra$columnas), 0L)
  pb <- suppressWarnings(perfilar_dbi(
    con, "d", sentinelas_numericos = lista, bloque_filas = 10
  ))
  expect_identical(y_de(pb$perfil_muestra$columnas), 0L)
  plan <- planificar_limpieza(p, d)
  expect_false("y" %in% plan$columna[plan$estrategia == "convertir_sentinelas_numericos"])
  a <- analizar(d, argumentos_perfil = list(sentinelas_numericos = lista))
  expect_identical(y_de(a$perfil$columnas), 0L)
})

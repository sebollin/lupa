# Lo que publica un perfil de base es dato: ni funciones, ni entornos, ni
# punteros externos. Una funcion publicada arrastra el entorno donde nacio, y
# en DuckDB eso hacia que un perfil de 200 filas pesara 15 MB serializado en
# vez de 75 KB (el candidato de mediana consolidada traia las funciones que
# arman su SQL). La prueba recorre el objeto entero, atributos incluidos, en
# vez de mirar el campo donde aparecio: el defecto es la propiedad, no el sitio.

.rutas_con_codigo <- function(x, ruta = "x") {
  if (is.function(x) || is.environment(x) ||
      typeof(x) %in% c("externalptr", "builtin", "special")) {
    return(ruta)
  }
  salida <- character()
  if (is.list(x)) {
    nombres <- names(x)
    for (i in seq_along(x)) {
      etiqueta <- if (is.null(nombres) || !nzchar(nombres[[i]])) i else nombres[[i]]
      salida <- c(salida, .rutas_con_codigo(x[[i]], paste0(ruta, "$", etiqueta)))
    }
  }
  propios <- setdiff(
    names(attributes(x)),
    c("names", "class", "row.names", "dim", "dimnames", "levels")
  )
  for (a in propios) {
    salida <- c(salida, .rutas_con_codigo(attr(x, a, exact = TRUE),
                                          paste0(ruta, "@", a)))
  }
  salida
}

.datos_sin_codigo <- function() {
  data.frame(
    id = 1:300,
    x = c(seq(-3, 3, length.out = 299), NA),
    y = rep(letters, length.out = 300),
    stringsAsFactors = FALSE
  )
}

test_that("el perfil de DuckDB no publica funciones ni entornos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(.duckdb_prueba())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "datos_sin_codigo", .datos_sin_codigo())

  for (argumentos in list(list(), list(muestra = 50), list(muestra = 50, clave = "id"))) {
    perfil <- suppressWarnings(suppressMessages(
      do.call(perfilar_dbi, c(list(con, "datos_sin_codigo"), argumentos))
    ))
    expect_identical(.rutas_con_codigo(unclass(perfil)), character())
    # La decision publicada sigue diciendo que se eligio y por que.
    consolidada <- perfil$resumen_tabla$meta$mediana_consolidada
    if (!is.null(consolidada)) {
      expect_true(is.logical(consolidada$disponible))
      expect_true(is.character(consolidada$motivo))
      if (isTRUE(consolidada$disponible)) {
        expect_true(is.character(consolidada$candidato$nombre))
      }
    }
    expect_lt(length(serialize(perfil, NULL)), 2e6)
  }

  plan <- suppressWarnings(suppressMessages(
    plan_perfilado_dbi(con, "datos_sin_codigo")
  ))
  expect_identical(.rutas_con_codigo(plan), character())
})

test_that("el perfil de SQLite tampoco publica funciones ni entornos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "datos_sin_codigo", .datos_sin_codigo())
  perfil <- suppressWarnings(suppressMessages(
    perfilar_dbi(con, "datos_sin_codigo", muestra = 50)
  ))
  expect_identical(.rutas_con_codigo(unclass(perfil)), character())
  plan <- suppressWarnings(suppressMessages(
    plan_perfilado_dbi(con, "datos_sin_codigo")
  ))
  expect_identical(.rutas_con_codigo(plan), character())
})

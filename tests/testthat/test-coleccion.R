# Nivel 7 del marco: la colección, una base entera. Estaba declarada y no se
# medía, y el mensaje decía que faltaba implementarla. Lo que faltaba era el
# objeto: nadie le decía a `lupa` qué tablas la componen.
#
# Las pruebas siguen los criterios que salieron de refutar el diseño contra
# bases reales de más de mil tablas: frontera declarada, esquema como parte de
# la identidad, permisos parciales como caso normal, muestreo por tabla, y
# ninguna lectura instantánea.

.con_de_prueba <- function() {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(
    id = 1:50, nombre = rep(letters[1:5], 10L), stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "hogares", data.frame(
    id = 1:20, depto = c(rep("MVD", 15L), rep(NA_character_, 5L)),
    stringsAsFactors = FALSE
  ))
  con
}

test_that("la colección declara la frontera y no consulta nada", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Se puede declarar una tabla que no existe: declarar no es medir.
  col <- coleccion(con, c("personas", "hogares", "no_existe"), nombre = "padron")
  expect_s3_class(col, "coleccion_lupa")
  expect_equal(col$n_declaradas, 3L)
  expect_equal(col$nombre, "padron")
  expect_setequal(col$tablas$tabla, c("personas", "hogares", "no_existe"))
})

test_that("el esquema es parte de la identidad de la tabla", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  por_texto <- coleccion(con, c("dbo.personas", "ods.personas"))
  expect_equal(por_texto$tablas$esquema, c("dbo", "ods"))
  expect_equal(por_texto$tablas$tabla, c("personas", "personas"))
  # El mismo nombre en dos esquemas son dos tablas, no una repetida.
  expect_equal(por_texto$n_declaradas, 2L)

  por_tabla <- coleccion(con, data.frame(
    esquema = c("dbo", "ods"), tabla = c("personas", "hogares"),
    stringsAsFactors = FALSE
  ))
  expect_equal(por_tabla$tablas$identificador, c("dbo.personas", "ods.hogares"))
})

test_that("lo que no se pudo leer se declara, y nunca queda en cero", {
  # Es el caso normal en bases institucionales: una credencial lee unas pocas
  # tablas de un esquema con cientos de objetos.
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares", "sin_permiso"))
  )
  expect_s3_class(perfil, "perfil_coleccion")
  expect_equal(perfil$meta$n_declaradas, 3L)
  expect_equal(perfil$meta$n_perfiladas, 2L)
  expect_equal(perfil$meta$n_sin_perfilar, 1L)

  falta <- perfil$cobertura_coleccion
  expect_equal(nrow(falta), 1L)
  expect_equal(falta$tabla, "sin_permiso")
  expect_true(nzchar(falta$motivo))
  expect_true(nzchar(falta$como_resolverlo))
  # Y no aparece en el resumen con ceros.
  expect_false("sin_permiso" %in% perfil$resumen_coleccion$tabla)
})

test_that("un objeto que no es tabla base se declara en vez de perfilarse", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  col <- coleccion(con, data.frame(
    esquema = c(NA, NA), tabla = c("personas", "hogares"),
    tipo = c("tabla", "vista"), stringsAsFactors = FALSE
  ))
  perfil <- perfilar_coleccion(col)

  expect_equal(perfil$meta$n_perfiladas, 1L)
  declarado <- perfil$cobertura_coleccion
  expect_equal(declarado$tabla, "hogares")
  expect_equal(declarado$tipo, "vista")
  expect_true(grepl("vista", declarado$motivo, fixed = TRUE))
})

test_that("cada tabla declara su propio muestreo, sin promediar alcances", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares")), muestra = 30L
  )
  resumen <- perfil$resumen_coleccion
  expect_equal(nrow(resumen), 2L)
  expect_true(all(resumen$muestra_solicitada == 30))
  # Las tablas tienen 50 y 20 filas: los alcances analizados difieren y se
  # declaran por separado, no se promedian.
  expect_equal(sort(resumen$n_filas), c(20, 50))
  expect_equal(length(unique(resumen$muestra_analizada)), 2L)
})

test_that("los agregados exactos vienen del resumen en SQL", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  resumen <- perfilar_coleccion(
    coleccion(con, c("personas", "hogares"))
  )$resumen_coleccion
  hogares <- resumen[resumen$tabla == "hogares", ]
  # 5 de 20 ausentes en una columna, sobre la tabla entera y no sobre la muestra.
  expect_equal(hogares$prop_faltantes_maxima, 0.25)
  expect_equal(hogares$n_filas, 20)
})

test_that("no hay lectura instantánea, y el objeto lo declara", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  perfil <- perfilar_coleccion(coleccion(con, c("personas", "hogares")))
  expect_false(perfil$meta$snapshot)
  expect_true(grepl("instantanea", perfil$meta$nota_snapshot, fixed = TRUE))
  # Cada tabla trae el momento en que se midió.
  expect_true(inherits(perfil$resumen_coleccion$momento, "POSIXct"))
  expect_false(anyNA(perfil$resumen_coleccion$momento))
})

test_that("los perfiles completos no se retienen salvo que se pidan", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  col <- coleccion(con, c("personas", "hogares"))

  liviano <- perfilar_coleccion(col)
  expect_null(liviano$perfiles)

  pesado <- perfilar_coleccion(col, conservar_perfiles = TRUE)
  expect_equal(length(pesado$perfiles), 2L)
  expect_s3_class(pesado$perfiles[["personas"]], "perfil_dbi")
})

test_that("una colección mal declarada se rechaza", {
  con <- .con_de_prueba()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_error(coleccion(con, character()), "vector de nombres")
  expect_error(coleccion(con, c("a", "a")), "repite")
  expect_error(coleccion(con, data.frame(x = 1)), "columna `tabla`")
  expect_error(perfilar_coleccion(list()), "coleccion\\(\\)")
})

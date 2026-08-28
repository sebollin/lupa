# La ausencia de filas en un catalogo puede ser falta de visibilidad, no falta
# de una clave. SQLite permite ademas una PRIMARY KEY de texto con NULL.

.ronda159_clave_vacia <- function(conexion) {
  testthat::with_mocked_bindings(
    lupa:::.clave_primaria_dbi(conexion, "tabla"),
    .consultar_dbi = function(...) {
      list(ok = TRUE, datos = data.frame(), motivo = NA_character_)
    },
    .package = "lupa"
  )
}

test_that("un catalogo vacio no se presenta como clave no declarada", {
  conexion <- structure(list(), class = "MotorInventado")
  resultado <- .ronda159_clave_vacia(conexion)

  expect_identical(resultado$columnas, character())
  expect_identical(resultado$garantia, "desconocida")
  expect_false(resultado$estado$visible)
  expect_match(resultado$motivo, "no devolvio filas")
  expect_match(resultado$motivo, "no ser visible")
})

test_that("la visibilidad vacia conserva las diferencias entre motores", {
  expect_true(.catalogo_clave_visible("pg_catalog", "postgresql"))
  expect_true(.catalogo_clave_visible("pragma", "sqlite"))
  expect_true(.catalogo_clave_visible("duckdb_constraints", "duckdb"))
  expect_true(.catalogo_clave_visible("all_constraints", "oracle"))
  # Medido contra contenedores el 2026-08-27, no deducido: MySQL 8 muestra la
  # restriccion a un rol con solo `SELECT` y MariaDB 11 no. Agruparlos por
  # parecido hacia que `lupa` afirmara "no hay clave declarada" sobre una tabla
  # de MariaDB que si la tiene.
  expect_true(.catalogo_clave_visible("information_schema", "mysql"))
  expect_false(.catalogo_clave_visible("information_schema", "mariadb"))
  # SQL Server no se pudo medir -el contenedor no levanto- asi que queda
  # ambiguo, que es la respuesta segura. Cuando se mida, se mueve con su numero.
  expect_false(.catalogo_clave_visible("information_schema", "sqlserver"))
  expect_false(.catalogo_clave_visible("information_schema", "desconocido"))
})

test_that("PostgreSQL consulta sus catalogos del sistema directamente", {
  consultas <- lupa:::.consultas_clave_primaria()
  pg <- consultas[[5L]]$sql("esquema", "tabla")

  expect_identical(
    .via_clave_primaria(structure(list(), class = "PqConnection")),
    "pg_catalog"
  )
  expect_match(pg, "pg_catalog.pg_constraint", fixed = TRUE)
  expect_match(pg, "pg_catalog.pg_class", fixed = TRUE)
  expect_match(pg, "pg_catalog.pg_namespace", fixed = TRUE)
  expect_match(pg, "convalidated", ignore.case = TRUE)
  expect_match(pg, "n.nspname = 'esquema'", fixed = TRUE)
  expect_false(grepl("information_schema", pg, fixed = TRUE))
})

test_that("una respuesta vacia de PostgreSQL tiene la garantia de su catalogo", {
  postgres <- structure(list(), class = "PqConnection")
  resultado <- testthat::with_mocked_bindings(
    lupa:::.clave_primaria_dbi(postgres, "tabla"),
    .consultar_dbi = function(...) {
      list(ok = TRUE, datos = data.frame(), motivo = NA_character_)
    },
    .package = "lupa"
  )

  expect_identical(resultado$garantia, "no_declarada")
  expect_true(resultado$estado$visible)
  expect_true(is.na(resultado$motivo))
})

test_that("SQLite separa unicidad y ausencia de nulos con evidencia real", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, paste(
    "CREATE TABLE text_notnull (",
    "id TEXT NOT NULL PRIMARY KEY, label TEXT)"
  ))
  DBI::dbExecute(con, "INSERT INTO text_notnull VALUES ('a', 'ok')")
  expect_error(
    DBI::dbExecute(con, "INSERT INTO text_notnull VALUES (NULL, 'reject')")
  )

  DBI::dbExecute(con, paste(
    "CREATE TABLE text_nullable (",
    "id TEXT PRIMARY KEY, label TEXT)"
  ))
  DBI::dbExecute(con, "INSERT INTO text_nullable VALUES (NULL, 'one')")
  DBI::dbExecute(con, "INSERT INTO text_nullable VALUES (NULL, 'two')")
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM text_nullable")$n,
    2
  )

  notnull <- .clave_primaria_dbi(con, "text_notnull")
  expect_identical(notnull$columnas, "id")
  expect_identical(notnull$garantia, "garantizada")
  expect_identical(notnull$estado$unicidad, "garantizada")
  expect_identical(notnull$estado$unicidad_aplica_a, "valores no nulos")
  expect_identical(notnull$estado$ausencia_de_nulos, "garantizada")

  nullable <- .clave_primaria_dbi(con, "text_nullable")
  expect_identical(nullable$columnas, "id")
  expect_identical(nullable$garantia, "desconocida")
  expect_identical(nullable$estado$unicidad, "garantizada")
  expect_identical(nullable$estado$unicidad_aplica_a, "valores no nulos")
  expect_identical(nullable$estado$ausencia_de_nulos, "no_verificada")
})

test_that("SQLite confirma la ausencia de clave cuando el catalogo si es visible", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE no_key (value TEXT)")

  resultado <- .clave_primaria_dbi(con, "no_key")
  expect_identical(resultado$columnas, character())
  expect_identical(resultado$garantia, "no_declarada")
  expect_true(resultado$estado$visible)
  expect_true(is.na(resultado$motivo))
})

# Que motores filtran `information_schema` por permisos NO se deduce del
# parecido entre ellos: se midio contra contenedores reales el 2026-08-27,
# creando un rol con solo `SELECT` sobre una tabla con clave primaria y
# contando lo que devuelve `information_schema.table_constraints`.
#
#   MySQL 8        el rol restringido ve 1  -> la vista es visible
#   MariaDB 11     el rol restringido ve 0  -> AMBIGUO
#   PostgreSQL 16  el rol restringido ve 0  -> por eso su via es `pg_catalog`
#
# MariaDB y MySQL parecen el mismo motor y aca no lo son. La primera version de
# este cambio los agrupo, y con eso `lupa` seguia afirmando "no hay clave
# declarada" sobre una tabla de MariaDB que si la tiene, leida con la credencial
# tipica de perfilado. Estas comprobaciones fijan la distincion medida.

test_that("la visibilidad del catalogo no agrupa motores por parecido", {
  # Vias cuyo catalogo es visible para cualquier credencial que ya lee la tabla.
  for (via in c("pg_catalog", "pragma", "duckdb_constraints")) {
    expect_true(lupa:::.catalogo_clave_visible(via, "postgresql"), info = via)
  }
  expect_true(lupa:::.catalogo_clave_visible("all_constraints", "oracle"))

  # Medido: MySQL si, MariaDB no.
  expect_true(lupa:::.catalogo_clave_visible("information_schema", "mysql"))
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "mariadb"))

  # Sin medir todavia: SQL Server y cualquier motor no reconocido quedan
  # ambiguos. Ambiguo es la respuesta segura; suponer visibilidad convierte una
  # falta de permiso en una afirmacion sobre los datos.
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "sqlserver"))
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "desconocido"))
})

test_that("un catalogo ambiguo no afirma que no hay clave", {
  # La funcion que decide el estado a partir de una respuesta vacia: con la via
  # ambigua tiene que quedar `desconocida` y `visible = FALSE`, nunca
  # `no_declarada`, que seria afirmar algo sobre los datos.
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "mariadb"))
  expect_true(lupa:::.catalogo_clave_visible("information_schema", "mysql"))
})

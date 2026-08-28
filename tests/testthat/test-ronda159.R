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

.ronda159_clave_simulada <- function(conexion, datos, ok = TRUE,
                                     motivo = NA_character_) {
  llamadas <- new.env(parent = emptyenv())
  llamadas$n <- 0L
  llamadas$sql <- NA_character_
  resultado <- testthat::with_mocked_bindings(
    lupa:::.clave_primaria_dbi(conexion, "tabla", esquema = "esquema"),
    .consultar_dbi = function(conexion, sql, presupuesto) {
      llamadas$n <- llamadas$n + 1L
      llamadas$sql <- sql
      list(ok = ok, datos = datos, motivo = motivo)
    },
    .package = "lupa"
  )
  list(resultado = resultado, consultas = llamadas$n, sql = llamadas$sql)
}

test_that("un catalogo vacio no se presenta como clave no declarada", {
  conexion <- structure(list(), class = "MotorInventado")
  resultado <- .ronda159_clave_vacia(conexion)

  expect_identical(resultado$columnas, character())
  expect_identical(resultado$garantia, "desconocida")
  expect_true(is.na(resultado$estado$visible))
  expect_match(resultado$motivo, "no devolvio filas")
  expect_match(resultado$motivo, "no ser visible")
})

test_that("la visibilidad vacia conserva las diferencias entre motores", {
  expect_true(.catalogo_clave_visible("pg_catalog", "postgresql"))
  expect_true(.catalogo_clave_visible("pragma", "sqlite"))
  expect_true(.catalogo_clave_visible("duckdb_constraints", "duckdb"))
  expect_true(.catalogo_clave_visible("all_constraints", "oracle"))
  expect_true(.catalogo_clave_visible("show_index", "mariadb"))
  # Medido contra contenedores el 2026-08-27, no deducido: MySQL 8 muestra la
  # restriccion a un rol con solo `SELECT` y MariaDB 11 no. Agruparlos por
  # parecido hacia que `lupa` afirmara "no hay clave declarada" sobre una tabla
  # de MariaDB que si la tiene.
  expect_true(.catalogo_clave_visible("information_schema", "mysql"))
  expect_false(.catalogo_clave_visible("information_schema", "mariadb"))
  # SQL Server: medido el 2026-08-28, con un rol de solo `SELECT` sobre tablas
  # con clave simple, compuesta y sin clave. La vista devuelve 1, 1 y 0. Lo
  # sostienen dos mediciones independientes -un contenedor 2022 y un servidor
  # 2016 con la credencial real de un perfilado-, no la documentacion: MariaDB
  # documenta lo mismo que MySQL y midiendo dio lo contrario.
  expect_true(.catalogo_clave_visible("information_schema", "sqlserver"))
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

test_that("la clave estructural decide sin publicar ni medir distintos", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, paste(
    "CREATE TABLE tabla_clave (id TEXT NOT NULL PRIMARY KEY, valor INTEGER)"
  ))
  DBI::dbExecute(con, "INSERT INTO tabla_clave VALUES ('a', 1), ('b', 1)")

  plan <- plan_perfilado_dbi(
    con, "tabla_clave", metricas = "moda",
    politica_costo = "por_cardinalidad", bloque_muestra = "solo_agregados"
  )
  fuente <- attr(plan, "fuente_cardinalidad_costo", exact = TRUE)$id
  expect_identical(fuente$nombre, "clave_primaria_garantizada")
  expect_false(attr(plan, "estrategia_distintos", exact = TRUE)$publica)
  distintos <- plan[grepl("distintos por lotes", plan$clase_consulta), , drop = FALSE]
  # `id` no entra en el lote: solo se mide `valor`, cuya fuente sigue siendo
  # desconocida.
  expect_equal(distintos$n_consultas, 1)

  resultado <- perfilar_dbi(
    con, "tabla_clave", metricas = "moda",
    politica_costo = "por_cardinalidad", bloque_muestra = "solo_agregados",
    proteger_datos_personales = FALSE
  )
  expect_true(all(is.na(resultado$resumen_tabla$columnas$n_distintos)))
  n_distintos <- resultado$resumen_tabla$sql[
    resultado$resumen_tabla$sql$metrica == "n_distintos", , drop = FALSE
  ]
  expect_true(all(n_distintos$estado == "no_solicitado"))
  expect_false(resultado$resumen_tabla$meta$estrategia_distintos$publica)
})

# Que motores filtran `information_schema` por permisos NO se deduce del
# parecido entre ellos: se midio contra contenedores reales el 2026-08-27,
# creando un rol con solo `SELECT` sobre una tabla con clave primaria y
# contando lo que devuelve `information_schema.table_constraints`.
#
#   MySQL 8        el rol restringido ve 1  -> la vista es visible
#   MariaDB 11     el rol restringido ve 0  -> usa `SHOW INDEX`, que devuelve 1
#                   para la misma clave
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

  # Medido: MySQL si; MariaDB no en `information_schema`, porque usa
  # `show_index`.
  expect_true(lupa:::.catalogo_clave_visible("information_schema", "mysql"))
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "mariadb"))

  # SQL Server ya esta medido y pasa a visible. Un motor no reconocido sigue
  # ambiguo: ambiguo es la respuesta segura, porque suponer visibilidad convierte
  # una falta de permiso en una afirmacion sobre los datos.
  expect_true(lupa:::.catalogo_clave_visible("information_schema", "sqlserver"))
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "desconocido"))
})

test_that("un catalogo ambiguo no afirma que no hay clave", {
  # La funcion que decide el estado a partir de una respuesta vacia: con la via
  # ambigua tiene que quedar `desconocida` y `visible = NA`, nunca
  # `no_declarada`, que seria afirmar algo sobre los datos.
  expect_false(lupa:::.catalogo_clave_visible("information_schema", "mariadb"))
  expect_true(lupa:::.catalogo_clave_visible("information_schema", "mysql"))
})

test_that("SHOW INDEX identifica y ordena la clave de MariaDB", {
  maria <- structure(list(), class = "MariaDBConnection")
  datos <- data.frame(
    Table = rep("tabla", 3L),
    Key_name = c("indice_auxiliar", "PRIMARY", "PRIMARY"),
    Seq_in_index = c(1L, 2L, 1L),
    Column_name = c("valor", "parte_a", "parte_b"),
    stringsAsFactors = FALSE
  )
  capturado <- .ronda159_clave_simulada(maria, datos)

  expect_identical(.via_clave_primaria(maria), "show_index")
  expect_identical(capturado$consultas, 1L)
  expect_match(capturado$sql, "SHOW INDEX FROM `esquema`.`tabla`", fixed = TRUE)
  expect_identical(capturado$resultado$fuente, "show_index")
  expect_identical(capturado$resultado$columnas, c("parte_b", "parte_a"))
  expect_true(capturado$resultado$estado$visible)
  expect_identical(capturado$resultado$garantia, "desconocida")
})

test_that("la decision de visibilidad distingue ceros y errores", {
  maria <- structure(list(), class = "MariaDBConnection")
  sin_pk <- .ronda159_clave_simulada(maria, data.frame())$resultado
  expect_identical(sin_pk$garantia, "no_declarada")
  expect_true(sin_pk$estado$visible)

  sin_primaria <- .ronda159_clave_simulada(
    maria,
    data.frame(Key_name = "indice_auxiliar", Seq_in_index = 1L,
               Column_name = "valor")
  )$resultado
  expect_identical(sin_primaria$garantia, "no_declarada")
  expect_true(sin_primaria$estado$visible)

  permiso <- .ronda159_clave_simulada(
    maria, NULL, ok = FALSE, motivo = "SHOW command denied"
  )$resultado
  expect_identical(permiso$garantia, "desconocida")
  expect_false(permiso$estado$visible)

  otro_error <- .ronda159_clave_simulada(
    maria, NULL, ok = FALSE, motivo = "table does not exist"
  )$resultado
  expect_identical(otro_error$garantia, "desconocida")
  expect_true(is.na(otro_error$estado$visible))
})

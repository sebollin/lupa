# En PostgreSQL, una consulta sin `ONLY` incluye a las tablas que heredan, y la
# clave primaria del padre NO gobierna las filas de los hijos. El catalogo sigue
# informando `contype = 'p'` y `convalidated = t`, asi que la garantia se leia
# como si valiera sobre el universo perfilado.
#
# Medido contra PostgreSQL 16 con un hijo que repite un valor del padre:
#   FROM padre       -> 6001 validos, 6000 distintos   (la unicidad NO se cumple)
#   FROM ONLY padre  -> 5000 validos, 5000 distintos
# El catalogo decia `garantizada` en los dos casos.

.ronda162_catalogo <- function(descendientes = 0L, validated = TRUE,
                               diferible = FALSE, relkind = "r",
                               indice_unico = TRUE) {
  datos <- data.frame(
    column_name = "id",
    ordinal_position = 1L,
    constraint_enforced = TRUE,
    constraint_validated = validated,
    stringsAsFactors = FALSE
  )
  if (!is.null(descendientes)) {
    datos$constraint_descendientes <- descendientes
    datos$constraint_diferible <- diferible
    datos$constraint_relkind <- relkind
    datos$constraint_indice_unico <- indice_unico
  }
  datos
}

test_that("sin descendientes la clave de PostgreSQL queda garantizada", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 0L), "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_false(resultado$estado$universo_incluye_descendientes)
})

test_that("con descendientes la garantia baja, porque el universo es otro", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 1L), "pg_catalog", "postgresql"
  )

  # La restriccion existe y es valida; lo que no vale es sobre las filas que se
  # van a perfilar. Decir "garantizada" seria afirmar sobre otro universo.
  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_true(resultado$estado$universo_incluye_descendientes)
})

test_that("varios descendientes cuentan igual que uno", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 4L), "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
})

test_that("una restriccion no validada sigue sin garantia, haya o no herencia", {
  for (descendientes in c(0L, 2L)) {
    resultado <- lupa:::.garantia_clave_primaria(
      .ronda162_catalogo(descendientes = descendientes, validated = FALSE),
      "pg_catalog", "postgresql"
    )
    expect_identical(
      resultado$garantia, "declarada_no_garantizada",
      info = paste("descendientes:", descendientes)
    )
  }
})

test_that("un catalogo sin la columna de descendientes no rompe", {
  # Los otros motores no la traen: la ausencia no puede alterar su lectura.
  datos <- .ronda162_catalogo(descendientes = NULL)
  expect_false("constraint_descendientes" %in% names(datos))

  resultado <- lupa:::.garantia_clave_primaria(
    datos, "information_schema", "mysql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_false(resultado$estado$universo_incluye_descendientes)
})

test_that("una restriccion diferible no se declara garantizada", {
  # Una PK `DEFERRABLE INITIALLY DEFERRED` puede estar violada mientras una
  # transaccion sigue abierta, y el catalogo la informa validada igual. Medido
  # contra PostgreSQL 16, dentro de una transaccion que inserta un duplicado:
  #   contype = p, convalidated = t, condeferrable = t
  #   count(*) = 3, count(id) = 3, count(DISTINCT id) = 2
  # Una derivacion de la unicidad habria publicado 3 cuando el valor es 2.
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 0L, diferible = TRUE),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_true(resultado$estado$restriccion_diferible)
})

test_that("una restriccion no diferible y sin descendientes si esta garantizada", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 0L, diferible = FALSE),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_false(resultado$estado$restriccion_diferible)
})

test_that("las dos condiciones se acumulan sin taparse", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 2L, diferible = TRUE),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_true(resultado$estado$universo_incluye_descendientes)
  expect_true(resultado$estado$restriccion_diferible)
})

test_that("una tabla particionada conserva la garantia pese a tener hijos", {
  # `pg_inherits` registra la herencia tradicional Y el particionado
  # declarativo, y solo la primera deja filas fuera del alcance de la clave: el
  # motor exige que la clave de una tabla particionada incluya las columnas de
  # particion. Medido contra PostgreSQL 16:
  #   relkind = 'r' con un hijo que repite -> 6001 validos, 6000 distintos
  #   relkind = 'p' con dos particiones    -> 19999 validos, 19999 distintos
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 2L, relkind = "p"),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_true(resultado$estado$relacion_particionada)
  expect_false(resultado$estado$universo_incluye_descendientes)
})

test_that("una tabla regular con hijos si pierde la garantia", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 2L, relkind = "r"),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_false(resultado$estado$relacion_particionada)
  expect_true(resultado$estado$universo_incluye_descendientes)
})

test_that("sin la columna relkind se trata como herencia, que es lo seguro", {
  # PostgreSQL anterior a la version 10 no tiene particionado declarativo: toda
  # descendencia es herencia, y ahi degradar es lo correcto.
  datos <- .ronda162_catalogo(descendientes = 1L, relkind = "r")
  datos$constraint_relkind <- NULL

  resultado <- lupa:::.garantia_clave_primaria(
    datos, "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
})

test_that("un indice de respaldo no unico retira la garantia", {
  # La unicidad la impone el indice que respalda la restriccion, no la fila de
  # `pg_constraint`. Por DDL normal no se llega a un indice de PK no unico -al
  # adjuntar una particion el motor crea el indice unico solo-, asi que esto es
  # defensa ante un catalogo alterado o un estado anormal. Verificado contra
  # PostgreSQL 16 forzando el estado: la tabla acepta un duplicado y queda con
  # 4 validos y 3 distintos mientras la restriccion sigue figurando validada.
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(indice_unico = FALSE), "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_true(resultado$estado$indice_no_unico)
})

test_that("el indice no unico tambien pesa sobre una tabla particionada", {
  # La excepcion por `relkind = 'p'` no puede saltearse esta comprobacion: una
  # particion cuyo indice dejo de ser unico admite duplicados igual.
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(descendientes = 2L, relkind = "p", indice_unico = FALSE),
    "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "declarada_no_garantizada")
  expect_true(resultado$estado$indice_no_unico)
})

test_that("un indice unico no altera el resto de las condiciones", {
  resultado <- lupa:::.garantia_clave_primaria(
    .ronda162_catalogo(indice_unico = TRUE), "pg_catalog", "postgresql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_false(resultado$estado$indice_no_unico)
})

test_that("sin la columna del indice no cambia nada, para los otros motores", {
  datos <- .ronda162_catalogo()
  datos$constraint_indice_unico <- NULL

  resultado <- lupa:::.garantia_clave_primaria(
    datos, "information_schema", "mysql"
  )

  expect_identical(resultado$garantia, "garantizada")
  expect_false(resultado$estado$indice_no_unico)
})

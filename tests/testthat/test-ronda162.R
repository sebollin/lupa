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
                               diferible = FALSE) {
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

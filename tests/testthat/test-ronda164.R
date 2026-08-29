# Una garantia "desconocida" tiene que decir por que no se pudo saber.
#
# Medido el 2026-08-29 contra los contenedores: MySQL 8.4 publica `ENFORCED` en
# `information_schema.TABLE_CONSTRAINTS` y MariaDB 11.8 no -sus columnas son
# CONSTRAINT_CATALOG, CONSTRAINT_SCHEMA, CONSTRAINT_NAME, TABLE_SCHEMA,
# TABLE_NAME y CONSTRAINT_TYPE-. La misma tabla, con la misma DDL, daba
# `garantizada` en uno y `desconocida` en el otro, y el segundo no explicaba
# nada: `motivo` venia `NA`.
#
# El paquete acertaba al no afirmar. Lo que fallaba es que quien perfila sobre
# MariaDB no podia distinguir un privilegio que le falta, un motor no cubierto o
# un limite del catalogo. Donde no hay senal, el paquete habla.

test_that("una garantia desconocida explica que campo falto y en que motor", {
  datos <- data.frame(
    column_name = "id", ordinal_position = 1L,
    stringsAsFactors = FALSE
  )
  resultado <- lupa:::.garantia_clave_primaria(datos, "show_index", "mariadb")

  expect_identical(resultado$garantia, "desconocida")
  expect_false(is.na(resultado$motivo))
  # El motivo nombra la via, el motor y los campos que no se pudieron consultar.
  expect_match(resultado$motivo, "show_index", fixed = TRUE)
  expect_match(resultado$motivo, "mariadb", fixed = TRUE)
  expect_match(resultado$motivo, "enforced", fixed = TRUE)
  # Y descarta explicitamente las dos lecturas equivocadas, que son las que
  # llevarian a quien perfila a buscar el problema donde no esta.
  expect_match(resultado$motivo, "privilegio", fixed = TRUE)
})

test_that("una garantia que si se pudo establecer no arrastra un motivo", {
  # MySQL responde `ENFORCED = YES` y ahi no hay nada que explicar: un motivo
  # sobre una garantia establecida seria ruido, no informacion.
  datos <- data.frame(
    column_name = "id", ordinal_position = 1L,
    constraint_enforced = "YES", stringsAsFactors = FALSE
  )
  resultado <- lupa:::.garantia_clave_primaria(
    datos, "information_schema", "mysql"
  )
  expect_identical(resultado$garantia, "garantizada")
  expect_null(resultado$motivo)
})

test_that("el motivo viaja hasta quien llama y no se pierde en el camino", {
  # La ayudante que lo publica existe porque `%||%` llego a base en R 4.4.0 y
  # el paquete declara `R (>= 4.1.0)`: usarlo habria andado en esta maquina y
  # fallado en el piso que DESCRIPTION promete.
  expect_identical(
    lupa:::.motivo_garantia(list(garantia = "garantizada")),
    NA_character_
  )
  expect_identical(
    lupa:::.motivo_garantia(list(garantia = "desconocida", motivo = "porque si")),
    "porque si"
  )
  expect_identical(
    lupa:::.motivo_garantia(list(motivo = character())),
    NA_character_
  )
})
# Un nombre de tabla sin calificar traia la clave de TODAS las tablas homonimas,
# de todos los esquemas, y las fusionaba en una sola respuesta publicada como
# `garantizada`. Reproducido el 2026-08-29 contra PostgreSQL 16 y MySQL 8.4:
# con `public.dup` (clave `id`), `s1.dup` (`a,b`) y `s2.dup` (`x`), el paquete
# publicaba `columnas = id, a, x, b`. Esa clave no existe en ninguna tabla.
#
# Es la violacion mas grave del invariante que se encontro: una declaracion del
# catalogo, sobre un universo que NO es el que se midio, presentada como una
# verificacion sobre el que si.
#
# Se arregla en cada via -`pg_table_is_visible()` resuelve el `search_path` igual
# que el motor; `DATABASE()` y `SCHEMA_NAME()` hacen lo propio-, y ademas hay una
# red por encima para el motor que no se conoce.

test_that("no se fusiona la clave de dos relaciones distintas", {
  datos <- data.frame(
    column_name = c("id", "p", "q"),
    ordinal_position = c(1L, 1L, 2L),
    constraint_esquema = c("lupa", "otra", "otra"),
    constraint_nombre = c("PRIMARY", "PRIMARY", "PRIMARY"),
    stringsAsFactors = FALSE
  )
  motivo <- lupa:::.clave_ambigua(datos)
  expect_false(is.null(motivo))
  expect_match(motivo, "2 relaciones", fixed = TRUE)
  expect_match(motivo, "lupa", fixed = TRUE)
  expect_match(motivo, "otra", fixed = TRUE)
  # Y dice como resolverlo, que es lo unico accionable para quien perfila.
  expect_match(motivo, "calificar el nombre", fixed = TRUE)
})

test_that("una sola relacion no dispara la guarda", {
  datos <- data.frame(
    column_name = c("a", "b"), ordinal_position = c(1L, 2L),
    constraint_esquema = c("lupa", "lupa"),
    constraint_nombre = c("PRIMARY", "PRIMARY"),
    stringsAsFactors = FALSE
  )
  expect_null(lupa:::.clave_ambigua(datos))
})

test_that("las vias que no traen discriminador no se rompen ni inventan", {
  # `pragma` de SQLite y `SHOW INDEX` de MariaDB preguntan por UNA tabla, no por
  # un nombre suelto: no hay ambiguedad posible y tampoco columnas que comparar.
  # La guarda tiene que dejarlas pasar, no fallar por campos ausentes.
  datos <- data.frame(
    column_name = c("id"), ordinal_position = 1L, stringsAsFactors = FALSE
  )
  expect_null(lupa:::.clave_ambigua(datos))
  expect_null(lupa:::.clave_ambigua(datos[0L, , drop = FALSE]))
})

test_that("la consulta de PostgreSQL restringe al esquema visible", {
  sql <- lupa:::.consultas_clave_primaria()
  pg <- Filter(function(x) identical(x$nombre, "pg_catalog"), sql)[[1L]]
  sin_calificar <- pg$sql(NA_character_, "dup")
  calificada <- pg$sql("s1", "dup")
  expect_match(sin_calificar, "pg_table_is_visible", fixed = TRUE)
  # Con esquema explicito manda el esquema, no la visibilidad.
  expect_false(grepl("pg_table_is_visible", calificada, fixed = TRUE))
  expect_match(calificada, "n.nspname", fixed = TRUE)
})

test_that("la via estandar restringe al esquema propio de cada motor", {
  sql <- lupa:::.consultas_clave_primaria()
  est <- Filter(function(x) identical(x$nombre, "information_schema"), sql)[[1L]]
  expect_match(est$sql(NA_character_, "t", "mysql"), "DATABASE()", fixed = TRUE)
  expect_match(
    est$sql(NA_character_, "t", "sqlserver"), "SCHEMA_NAME()", fixed = TRUE
  )
  # Y siempre pide el discriminador, que es lo que alimenta la red de seguridad.
  expect_match(
    est$sql(NA_character_, "t", "desconocido"), "constraint_esquema",
    fixed = TRUE
  )
})

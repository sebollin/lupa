# Un hallazgo que sólo se puede leer sirve la mitad que uno que se puede
# verificar. `clave` nombra las columnas que identifican una fila, y la
# trazabilidad trae su valor para las filas señaladas, de modo que el caso se
# pueda buscar en el sistema de origen sin abrir la tabla.
#
# `estado` y `localizador` son dos ejes distintos y no se pisan: una
# trazabilidad puede ser al mismo tiempo por clave y truncada.
#
# Y hay una tensión que este rasgo no puede ignorar: **la clave que permite ir a
# verificar es exactamente lo que identifica a una persona.**

.tabla_con_clave <- function(n = 120L) {
  data.frame(
    id = sprintf("K%03d", seq_len(n)),
    constante = rep("a", n),
    stringsAsFactors = FALSE
  )
}

test_that("con clave declarada la trazabilidad trae sus valores", {
  datos <- .tabla_con_clave()
  perfil <- perfilar(datos, clave = "id")

  con_claves <- Filter(
    function(t) !is.null(t$claves), perfil$hallazgos$trazabilidad
  )
  expect_gt(length(con_claves), 0L)

  traza <- con_claves[[1L]]
  expect_equal(traza$localizador, "clave_declarada")
  # Las claves son un data frame: preservan columnas y tipos en vez de
  # concatenarse, que haría ambigua una clave compuesta.
  expect_true(is.data.frame(traza$claves))
  expect_equal(names(traza$claves), "id")
  expect_equal(nrow(traza$claves), length(traza$indices_fila))
  # Y corresponden a las filas señaladas.
  expect_equal(traza$claves$id, datos$id[traza$indices_fila])
})

test_that("sin clave el índice de fila sigue siendo el localizador", {
  datos <- .tabla_con_clave()
  perfil <- perfilar(datos)

  localizadores <- vapply(
    perfil$hallazgos$trazabilidad, function(t) t$localizador, character(1L)
  )
  expect_true("indice_fila" %in% localizadores)
  expect_false("clave_declarada" %in% localizadores)
  expect_true(all(vapply(
    perfil$hallazgos$trazabilidad, function(t) is.null(t$claves), logical(1L)
  )))
})

test_that("estado y localizador son ejes independientes", {
  datos <- .tabla_con_clave(300L)
  # Un tope chico fuerza el truncamiento sin quitar la clave.
  perfil <- perfilar(datos, clave = "id", max_filas_hallazgo = 5L)

  truncadas <- Filter(
    function(t) isTRUE(t$truncado), perfil$hallazgos$trazabilidad
  )
  expect_gt(length(truncadas), 0L)
  traza <- truncadas[[1L]]
  # Las dos cosas a la vez: truncada Y por clave.
  expect_equal(traza$estado, "truncada")
  expect_equal(traza$localizador, "clave_declarada")
  expect_equal(nrow(traza$claves), 5L)
  expect_gt(traza$total, traza$mostrados)
})

test_that("una clave compuesta preserva sus columnas y tipos", {
  datos <- data.frame(
    anio = rep(2023L, 120L),
    numero = seq_len(120L),
    constante = rep("a", 120L),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, clave = c("anio", "numero"))
  traza <- Filter(
    function(t) !is.null(t$claves), perfil$hallazgos$trazabilidad
  )[[1L]]

  expect_equal(names(traza$claves), c("anio", "numero"))
  expect_type(traza$claves$anio, "integer")
  expect_type(traza$claves$numero, "integer")
})

test_that("la clave declarada se protege como la evidencia", {
  # Es la prueba que decide si el rasgo está bien construido: la clave que
  # permite verificar es la que identifica a una persona.
  datos <- data.frame(
    cedula = sprintf("%d", 40000000L + seq_len(120L)),
    constante = rep("a", 120L),
    stringsAsFactors = FALSE
  )

  protegido <- perfilar(datos, clave = "cedula")
  con_claves <- Filter(
    function(t) !is.null(t$claves), protegido$hallazgos$trazabilidad
  )
  expect_gt(length(con_claves), 0L)
  expect_true(all(vapply(con_claves, function(t) {
    all(t$claves$cedula == "[clave protegida]")
  }, logical(1L))))
  # Y declara qué protegió, en vez de hacerlo en silencio.
  expect_equal(con_claves[[1L]]$claves_protegidas, "cedula")

  # Con la protección desactivada, el valor está.
  crudo <- perfilar(datos, clave = "cedula", proteger_datos_personales = FALSE)
  sin_proteger <- Filter(
    function(t) !is.null(t$claves), crudo$hallazgos$trazabilidad
  )[[1L]]
  expect_true(all(grepl("^4000", sin_proteger$claves$cedula)))
})

test_that("una clave mal declarada se rechaza diciendo qué hay", {
  datos <- .tabla_con_clave()
  expect_error(perfilar(datos, clave = "no_existe"), "no estan en los datos")
  expect_error(perfilar(datos, clave = "no_existe"), "Disponibles")
  expect_error(perfilar(datos, clave = c("id", "id")), "repite")
  expect_error(perfilar(datos, clave = 1), "nombres de columna")
  expect_error(perfilar(datos, clave = NA_character_), "sin NA")
})

test_that("una clave que no es única avisa y no rompe", {
  datos <- data.frame(
    k = c("a", "a", "b"), x = 1:3, stringsAsFactors = FALSE
  )
  expect_warning(perfilar(datos, clave = "k"), "no es unica")
})

test_that("analizar() traslada la clave por las dos vías", {
  datos <- .tabla_con_clave()
  por_puntos <- analizar(datos, clave = "id")
  por_lista <- analizar(datos, argumentos_perfil = list(clave = "id"))
  expect_identical(por_puntos$perfil$hallazgos, por_lista$perfil$hallazgos)

  localizadores <- vapply(
    por_puntos$perfil$hallazgos$trazabilidad,
    function(t) t$localizador, character(1L)
  )
  expect_true("clave_declarada" %in% localizadores)
})

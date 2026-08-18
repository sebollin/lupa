# El perfilado evalúa; no corrige. Ninguna función de análisis puede alterar la
# tabla que recibe: ni su contenido, ni sus tipos, ni sus nombres, ni sus
# atributos. La única capa autorizada a producir datos distintos es la de
# remediación, y aun ahí devuelve una copia y nunca modifica la original.
#
# En R esto no es automático. Un `data.table` se modifica **por referencia**, así
# que `set()`, `:=`, `setnames()` o `setkey()` aplicados a la entrada le
# cambiarían la tabla al usuario sin que se entere. Estas pruebas existen para
# que eso no pueda pasar inadvertido.

.tabla_de_prueba <- function(n = 120L) {
  set.seed(20260817)
  data.frame(
    id = sprintf("K%04d", seq_len(n)),
    nombre = rep(c("Jose Perez", "JOSÉ PÉREZ", "Ana  Lopez", "Ana Lopez"),
                 length.out = n),
    monto = c(rep(NA_real_, 5L), round(runif(n - 5L, 10, 9000), 2)),
    fecha = rep(c("30/11/2023", "01/12/2023", "3/1/2023"), length.out = n),
    categoria = factor(rep(c("a", "b", "c"), length.out = n)),
    entero = seq_len(n),
    stringsAsFactors = FALSE
  )
}

.huella_tabla <- function(x) {
  list(
    contenido = x,
    nombres = names(x),
    tipos = vapply(x, function(col) paste(class(col), collapse = "/"),
                   character(1L)),
    dimension = dim(x),
    atributos = attributes(x),
    clase = class(x)
  )
}

test_that("perfilar y analizar no alteran la tabla recibida", {
  datos <- .tabla_de_prueba()
  antes <- .huella_tabla(datos)

  perfilar(datos)
  expect_identical(.huella_tabla(datos), antes)

  analizar(datos)
  expect_identical(.huella_tabla(datos), antes)
})

test_that("los detectores no alteran la tabla recibida", {
  datos <- .tabla_de_prueba()
  antes <- .huella_tabla(datos)

  detectar_claves(datos)
  detectar_dependencias(datos)
  distribucion_valores(datos)
  detectar_asociaciones(datos)
  detectar_formatos_fecha(datos$fecha)
  if (requireNamespace("stringdist", quietly = TRUE)) {
    detectar_duplicados_aproximados(datos, columnas = "nombre")
  }

  expect_identical(.huella_tabla(datos), antes)
})

test_that("planificar y guiar la limpieza no alteran la tabla recibida", {
  datos <- .tabla_de_prueba()
  antes <- .huella_tabla(datos)

  perfil <- perfilar(datos)
  plan <- planificar_limpieza(perfil)
  guiar_limpieza(plan)

  expect_identical(.huella_tabla(datos), antes)
})

test_that("un data.table no se modifica por referencia", {
  skip_if_not_installed("data.table")
  # Es el caso que importa: `data.table` permite la modificación por referencia,
  # así que una asignación descuidada dentro del paquete le cambiaría la tabla
  # al usuario. Con `data.frame` la copia al modificar lo enmascararía.
  datos <- data.table::as.data.table(.tabla_de_prueba())
  antes <- .huella_tabla(datos)
  direccion <- data.table::address(datos)

  perfilar(datos)
  analizar(datos)
  detectar_claves(datos)
  detectar_dependencias(datos)

  expect_identical(.huella_tabla(datos), antes)
  expect_identical(data.table::address(datos), direccion)
})

test_that("un tibble no se modifica ni pierde su clase", {
  skip_if_not_installed("tibble")
  datos <- tibble::as_tibble(.tabla_de_prueba())
  antes <- .huella_tabla(datos)

  perfilar(datos)
  analizar(datos)

  expect_identical(.huella_tabla(datos), antes)
})

test_that("aplicar() devuelve una copia y deja intacta la original", {
  # Se elige una tabla que sí produce una acción seleccionada, para que la
  # prueba no pueda pasar sin haber aplicado nada.
  datos <- data.frame(
    texto = c(rep("  hola  ", 40L), rep("chau", 40L), rep(" ay ", 40L)),
    stringsAsFactors = FALSE
  )
  antes <- .huella_tabla(datos)

  plan <- planificar_limpieza(perfilar(datos))
  seleccionadas <- plan[plan$aplicar %in% TRUE & plan$estado == "lista", ]
  expect_gt(nrow(seleccionadas), 0L)
  expect_true("recortar_espacios" %in% seleccionadas$estrategia)

  limpio <- aplicar(plan, datos)

  # La salida cambió, y se comprueba el contenido: una aserción de
  # `identical()` contra `NULL` pasaría aunque `aplicar()` no devolviera nada.
  expect_s3_class(limpio, "resultado_limpieza")
  expect_true(is.data.frame(limpio$datos))
  expect_false(any(grepl("^\\s|\\s$", limpio$datos$texto)))
  expect_true(any(grepl("^\\s|\\s$", datos$texto)))

  # Y la entrada no.
  expect_identical(.huella_tabla(datos), antes)
})

test_that("aplicar() tampoco modifica por referencia un data.table", {
  skip_if_not_installed("data.table")
  datos <- data.table::as.data.table(data.frame(
    texto = c(rep("  hola  ", 40L), rep("chau", 40L), rep(" ay ", 40L)),
    stringsAsFactors = FALSE
  ))
  antes <- .huella_tabla(datos)
  direccion <- data.table::address(datos)

  plan <- planificar_limpieza(perfilar(datos))
  expect_gt(sum(plan$aplicar %in% TRUE & plan$estado == "lista"), 0L)
  aplicar(plan, datos)

  expect_identical(.huella_tabla(datos), antes)
  expect_identical(data.table::address(datos), direccion)
})

# La cuarta evaluacion real: un nombre de columna que contiene un valor
# protegido -"Segundo" es un nombre de pila corriente- no se tapa en ninguna
# salida. Se tapaba en los atributos del plan, y el hallazgo declarado sin accion
# dejaba de corresponder a su columna.

.datos_O123 <- function() {
  set.seed(123)
  n <- 600L
  nombres <- c("SEGUNDO", "JUAN", "MARIA", "PEDRO", "ANA", "LUCIA")
  apellidos <- c("PEREZ", "RODRIGUEZ", "GONZALEZ", "SOSA", "SILVA", "GOMEZ")
  ruido <- function(v) {
    i <- sample(length(v), 20L)
    v[i] <- tolower(v[i])
    j <- sample(length(v), 6L)
    v[j] <- paste0(v[j], "-", sample(1:9, 6L, TRUE))
    v
  }
  data.frame(
    cedula = sprintf("%d%07d", sample(1:6, n, TRUE), sample(0:9999999, n, TRUE)),
    primer_nombre = ruido(sample(nombres, n, TRUE)),
    segundo_nombre = ruido(sample(nombres, n, TRUE)),
    segundo_apellido = ruido(sample(apellidos, n, TRUE)),
    stringsAsFactors = FALSE
  )
}

.nombres_rotos_O123 <- function(x) {
  hojas <- character()
  recorrer <- function(y) {
    for (a in setdiff(names(attributes(y)), c("names", "class", "row.names"))) {
      recorrer(attr(y, a, exact = TRUE))
    }
    if (is.factor(y)) hojas <<- c(hojas, levels(y))
    else if (is.character(y)) hojas <<- c(hojas, y)
    else if (is.list(y)) for (e in y) recorrer(e)
  }
  recorrer(x)
  unique(hojas[grepl("[valor protegido]_", hojas, fixed = TRUE)])
}

test_that("todo hallazgo del plan queda con accion o declarado en su columna", {
  datos <- .datos_O123()
  perfil <- suppressWarnings(perfilar(datos))
  expect_true("segundo_nombre" %in% .columnas_personales_protegidas(perfil))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  hallazgos <- hallazgos(perfil)
  medidos <- hallazgos[as.character(hallazgos$severidad) != "ok", , drop = FALSE]
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  cubiertos <- c(paste(plan$columna, plan$hallazgo),
                 paste(sin_accion$columna, sin_accion$hallazgo))
  claves <- paste(medidos$columna, medidos$tipo_hallazgo)
  # Antes: `[valor protegido]_nombre` en el atributo, y los hallazgos de
  # `segundo_nombre` sin accion ni declaracion.
  expect_true(any(startsWith(claves, "segundo_nombre ")))
  expect_identical(claves[!claves %in% cubiertos], character())
  expect_identical(.nombres_rotos_O123(plan), character())
})

test_that("ningun nombre de columna se rompe en el perfil, el analisis o las distribuciones", {
  datos <- .datos_O123()
  perfil <- suppressWarnings(perfilar(datos))
  analisis <- suppressWarnings(analizar(datos))
  # Antes: `columnas_analizadas` de las dependencias y las asociaciones, y
  # `columnas_datos_personales_protegidas` del plan del analisis.
  expect_identical(.nombres_rotos_O123(perfil), character())
  expect_identical(.nombres_rotos_O123(analisis), character())
  expect_identical(
    .nombres_rotos_O123(suppressWarnings(distribucion_valores(datos))), character()
  )
})

test_that("el resumen del motor no rompe un nombre de columna", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", .datos_O123())
  resultado <- suppressMessages(suppressWarnings(perfilar_dbi(conexion, "t")))
  expect_identical(.nombres_rotos_O123(resultado), character())
})

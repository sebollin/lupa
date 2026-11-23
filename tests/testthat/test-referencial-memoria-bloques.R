# `medir_dbi()` con `estrategia_no_sql = "bloques"` compara en R una metrica
# referencial con un referencial en R (la via `distintos_r`). Esa lectura del
# referencial se registra como en `medir()`: con su nombre, sus filas, la huella
# de sus datos y la misma identidad. Antes no se registraba y la fila no tenia
# tabla referida. Dos referenciales distintos con el mismo nombre son dos, como
# en `medir()`.

.bloq_padron <- function(n) {
  referencial(data.frame(k = seq_len(n), v = letters[seq_len(n)]), clave = "k",
              valor = "v", completo = TRUE, alcance = "e", nombre = "padron")
}

.bloq_modelo <- function(nombre, n) {
  modelo(instanciar(especializar(metricas_referencial()$CorrectitudSemFuerte, nombre),
                    "d", "k", referencial = .bloq_padron(n)))
}

.bloq_datos <- function() data.frame(k = c(1, 2, 3, NA))

.bloq_con <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "d", .bloq_datos())
  con
}

.bloq_medir <- function(con, nombre, n) {
  medir_dbi(.bloq_modelo(nombre, n), con, id_medicion = "j",
            fecha = as.POSIXct("2026-06-30", tz = "UTC"),
            estrategia_no_sql = "bloques")
}

test_that("bloques registra la lectura del referencial en R con su huella", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .bloq_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  m <- .bloq_medir(con, "A", 3)
  expect_identical(unname(attr(m, "ejecucion_dbi")$vias), "distintos_r")
  expect_identical(m$entidad_referida, "padron")
  expect_identical(m$n_filas_leidas_referida, 3)
  l <- attr(m, "lecturas", exact = TRUE)
  propia <- l[l$entidad == "padron", , drop = FALSE]
  expect_identical(nrow(propia), 1L)
  expect_identical(propia$control, "huella_de_datos")
  # La misma huella y la misma identidad que registra `medir()` en memoria.
  p <- medir(.bloq_modelo("A", 3), list(d = .bloq_datos()), id_medicion = "j",
             fecha = as.POSIXct("2026-06-30", tz = "UTC"), detalle = "resumen")
  lp <- attr(p, "lecturas", exact = TRUE)
  expect_identical(propia$instantanea, lp$instantanea[lp$entidad == "padron"])
  expect_false(is.na(propia$tabla))
  expect_identical(propia$tabla, lp$tabla[lp$entidad == "padron"])
})

test_that("dos referenciales en R con el mismo nombre en dos partes con bloques son dos", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .bloq_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_warning(
    u <- rbind(uno = .bloq_medir(con, "A", 3), dos = .bloq_medir(con, "B", 4)), NA
  )
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  expect_identical(u$n_filas_leidas_referida, c(3, 4))
  l <- attr(u, "lecturas", exact = TRUE)
  expect_identical(length(unique(l$tabla[l$entidad == "padron"])), 2L)
  # Unida con la parte de `medir()` de uno de los dos, tambien: es el mismo
  # referencial que el de la primera parte, y otro que el de la segunda.
  en_memoria <- medir(.bloq_modelo("C", 3), list(d = .bloq_datos()),
                      id_medicion = "j",
                      fecha = as.POSIXct("2026-06-30", tz = "UTC"),
                      detalle = "resumen")
  l <- attr(suppressWarnings(rbind(u, en_memoria)), "lecturas", exact = TRUE)
  expect_identical(length(unique(l$tabla[l$entidad == "padron"])), 2L)
})

test_that("el control: el mismo referencial en las dos partes cierra", {
  skip_on_cran()
  skip_if_not_installed("RSQLite")
  con <- .bloq_con()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  u <- rbind(uno = .bloq_medir(con, "A", 3), dos = .bloq_medir(con, "B", 3))
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  l <- attr(u, "lecturas", exact = TRUE)
  expect_identical(length(unique(l$tabla[l$entidad == "padron"])), 1L)
})

# Un referencial en R es la tabla referida de su metrica: su lectura se
# registra con la huella de sus datos, y lo identifican su nombre y esa huella.
# Dos referenciales distintos con el mismo nombre -el nombre por omision de
# `referencial(<expresion>)` es siempre `referencial`- son dos referenciales y
# no uno que cambio: cada cifra cierra con la lectura del suyo, vayan en una
# llamada o en dos partes.

.mem_ref_medir <- function(padron, nombre_metrica = "A", entidad = "d") {
  ref <- metricas_referencial()
  datos <- stats::setNames(list(data.frame(k = c(1, 2, 3, NA))), entidad)
  suppressWarnings(medir(
    modelo(instanciar(especializar(ref$CorrectitudSemFuerte, nombre_metrica),
                      entidad, "k", referencial = padron)),
    datos, id_medicion = "j", fecha = as.POSIXct("2026-06-30", tz = "UTC"),
    detalle = "resumen"
  ))
}

.mem_ref_padron <- function(n, nombre = NULL) {
  referencial(data.frame(k = seq_len(n), v = letters[seq_len(n)]), clave = "k",
              valor = "v", completo = TRUE, alcance = "e", nombre = nombre)
}

test_that("el referencial en R es la tabla referida y registra su lectura", {
  skip_on_cran()
  p <- .mem_ref_medir(.mem_ref_padron(3, "padron"))
  expect_identical(p$entidad_referida, "padron")
  expect_identical(p$n_filas_leidas_referida, 3)
  l <- attr(p, "lecturas", exact = TRUE)
  expect_identical(l$n_filas[l$entidad == "padron"], 3)
  expect_true(startsWith(l$instantanea[l$entidad == "padron"], "huella:"))
})

test_that("dos referenciales con el mismo nombre en dos partes son dos, no uno que cambio", {
  skip_on_cran()
  for (nombre in list("padron", NULL)) {
    etiqueta <- if (is.null(nombre)) "nombre por omision" else nombre
    p1 <- .mem_ref_medir(.mem_ref_padron(3, nombre), "A")
    p2 <- .mem_ref_medir(.mem_ref_padron(4, nombre), "B")
    # Sin aviso: ninguna cifra deja de cerrar.
    expect_warning(u <- rbind(uno = p1, dos = p2), NA)
    expect_identical(u$estado_cuentas, c("cierran", "cierran"), label = etiqueta)
    expect_identical(u$n_filas_leidas_referida, c(3, 4), label = etiqueta)
    # Cada uno con su lectura, y cada lectura con su identidad.
    l <- attr(u, "lecturas", exact = TRUE)
    propias <- l[l$entidad == u$entidad_referida[[1L]], , drop = FALSE]
    expect_identical(propias$n_filas, c(3, 4), label = etiqueta)
    expect_identical(length(unique(propias$tabla)), 2L, label = etiqueta)
    # El tablero no aparta ninguna.
    expect_null(attr(tablero_calidad(u), "cuentas", exact = TRUE), label = etiqueta)
    # Con `prioridad` tambien se juntan: no es una tabla con otra instantanea.
    k <- rbind(uno = p1, dos = p2, prioridad = c("uno", "dos"))
    expect_identical(k$estado_cuentas, c("cierran", "cierran"), label = etiqueta)
    expect_identical(k$fuente, c("uno", "dos"), label = etiqueta)
  }
})

test_that("la misma celda medida con otro referencial no se junta con prioridad", {
  skip_on_cran()
  p1 <- .mem_ref_medir(.mem_ref_padron(3, "padron"), "A")
  p2 <- .mem_ref_medir(.mem_ref_padron(4, "padron"), "A")
  expect_error(rbind(uno = p1, dos = p2, prioridad = c("uno", "dos")),
               class = "lupa_consolidacion_no_admitida")
})

test_that("el control: el mismo referencial en las dos partes cierra", {
  skip_on_cran()
  padron <- .mem_ref_padron(3, "padron")
  u <- rbind(uno = .mem_ref_medir(padron, "A"), dos = .mem_ref_medir(padron, "B"))
  expect_identical(u$estado_cuentas, c("cierran", "cierran"))
  s <- semaforo_calidad(u, cortes = list(Exactitud = c(0.5, 0.9)))
  expect_identical(s$control_lecturas, c("completo", "completo"))
})

test_that("el control: dos referenciales distintos con un nombre en una corrida no se cruzan", {
  skip_on_cran()
  ref <- metricas_referencial()
  p <- suppressWarnings(medir(
    modelo(
      instanciar(especializar(ref$CorrectitudSemFuerte, "A"), "d", "k",
                 referencial = .mem_ref_padron(3)),
      instanciar(especializar(ref$CorrectitudSemFuerte, "B"), "d", "k",
                 referencial = .mem_ref_padron(4))
    ),
    list(d = data.frame(k = c(1, 2, 3, NA))), id_medicion = "j",
    fecha = as.POSIXct("2026-06-30", tz = "UTC"), detalle = "resumen"
  ))
  expect_identical(p$estado_cuentas, c("cierran", "cierran"))
  # Se registran los dos, cada uno con su huella, y cada cifra sabe cual leyo
  # (`test-referencial-memoria-ambiguo.R`).
  expect_identical(p$entidad_referida, c("referencial", "referencial"))
  expect_identical(p$n_filas_leidas_referida, c(3, 4))
})

test_that("un referencial con el nombre de una entidad no se confunde con ella", {
  skip_on_cran()
  p <- .mem_ref_medir(.mem_ref_padron(3, "d"))
  expect_identical(p$entidad_referida, "referencial d")
  expect_identical(p$estado_cuentas, "cierran")
  l <- attr(p, "lecturas", exact = TRUE)
  expect_setequal(l$entidad, c("d", "referencial d"))
})

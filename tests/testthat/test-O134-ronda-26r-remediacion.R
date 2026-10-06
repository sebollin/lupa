# Ronda 26-R: lo que la refutacion del plan de limpieza encontro. Una prueba por
# hallazgo, con la cifra rehecha a mano. Las tablas son chicas a proposito: el
# check tiene un presupuesto de tiempo.

.plan_O134 <- function(datos, ...) {
  perfil <- suppressWarnings(perfilar(datos, analizar_dependencias = FALSE, ...))
  suppressWarnings(planificar_limpieza(perfil, datos))
}

.solo_O134 <- function(plan, estrategia) {
  plan$aplicar <- as.character(plan$estrategia) == estrategia
  plan
}

test_that("A: conservar_primera_duplicada con integer64 quita solo la fila repetida", {
  skip_if_not_installed("bit64")
  datos <- data.frame(
    id = bit64::as.integer64(c(-1, -2, 3, -1)), b = c(1, 1, 2, 1)
  )
  # A mano: la fila 4 repite la 1; la 2 (id = -2) no se repite.
  plan <- .solo_O134(.plan_O134(datos), "conservar_primera_duplicada")
  r <- suppressWarnings(aplicar(plan, datos, permitir_eliminacion = TRUE))
  # Antes: quedaban 2 filas -tambien se iba la del id -2- y n_cambiadas = 2.
  expect_identical(nrow(r$datos), 3L)
  expect_identical(as.character(r$datos$id), c("-1", "-2", "3"))
  expect_equal(r$registro$n_cambiadas, 1)
})

test_that("H: los duplicados se marcan con matriz, data.frame anidado y POSIXlt", {
  base <- data.frame(v = c(1, 2, 1))
  con_matriz <- base
  con_matriz$m <- matrix(c(1, 2, 1, 5, 6, 5), ncol = 2L)
  con_tabla <- base
  con_tabla$d <- data.frame(a = c("x", "y", "x"), b = c(1, 2, 1))
  con_lt <- base
  con_lt$t <- strptime(
    c("2020-01-01 10:00", "2020-01-02 10:00", "2020-01-01 10:00"),
    "%Y-%m-%d %H:%M", tz = "UTC"
  )
  for (datos in list(con_matriz, con_tabla, con_lt)) {
    plan <- .plan_O134(datos)
    fila <- as.character(plan$estrategia) == "marcar_filas_duplicadas"
    expect_true(plan$aplicar[fila])
    r <- suppressWarnings(aplicar(.solo_O134(plan, "marcar_filas_duplicadas"), datos))
    # Antes: `fallida` ("replacement has 6 rows" / "columnas de lista").
    expect_identical(r$registro$estado, "ejecutada")
    # A mano: la fila 3 repite la 1.
    expect_identical(r$datos$.fila_duplicada, c(FALSE, FALSE, TRUE))
    expect_identical(r$datos$.grupo_duplicado, c(1L, NA, 1L))
  }
  # La guarda del plan hace la pregunta del ejecutor: una lista dentro de una
  # tabla anidada no se puede comparar, y la accion queda bloqueada en vez de
  # ofrecerse lista y fallar.
  con_lista <- base
  con_lista$d <- data.frame(a = c("x", "y", "x"))
  con_lista$d$l <- list(1, 2, 1)
  plan <- .plan_O134(con_lista)
  fila <- as.character(plan$estrategia) == "marcar_filas_duplicadas"
  expect_identical(sum(fila), 1L)
  expect_identical(as.character(plan$estado[fila]), "bloqueada")
  expect_false(plan$aplicar[fila])
})

test_that("N: un factor con codigo NA y nivel NA se agrupa como en el perfil", {
  datos <- data.frame(b = c(1, 1, 1, 1))
  datos$f <- structure(c(1L, 2L, NA, NA), levels = c("a", NA), class = "factor")
  # A mano, por codigo: las filas 3 y 4 (codigo NA) son iguales; la 2 (nivel
  # NA, codigo 2) es otra fila.
  perfil <- suppressWarnings(perfilar(datos, analizar_dependencias = FALSE))
  expect_equal(perfil$general$filas_duplicadas, 1)
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  marcado <- suppressWarnings(aplicar(.solo_O134(plan, "marcar_filas_duplicadas"), datos))
  # Antes: la fila 2 entraba al grupo de las filas 3 y 4.
  expect_identical(marcado$datos$.fila_duplicada, c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(marcado$datos$.grupo_duplicado, c(NA, NA, 1L, 1L))
  eliminado <- suppressWarnings(aplicar(
    .solo_O134(plan, "conservar_primera_duplicada"), datos,
    permitir_eliminacion = TRUE
  ))
  expect_identical(nrow(eliminado$datos), 3L)
})

test_that("B: winsorizar recorta a los limites de Tukey y no toca lo demas", {
  x <- c(41, 42, 42, 47, 49, 49, 50, 52, 52, 53, 53, 53, 55, 56, 56, 56, 59,
         59, 100, 100, 150, 1000, 1000, 5000)
  datos <- data.frame(v = x)
  # A mano (cuantiles tipo 7 sobre 24 valores): Q1 = 49 + 0.75 * 1 = 49.75,
  # Q3 = 59 + 0.25 * 41 = 69.25, IQR = 19.5, limites [20.5, 98.5]: seis
  # extremos, todos arriba.
  esperado <- pmin(pmax(x, 20.5), 98.5)
  plan <- .plan_O134(datos)
  expect_equal(plan$n_afectadas[plan$estrategia == "winsorizar_outliers"], 6)
  r <- suppressWarnings(aplicar(.solo_O134(plan, "winsorizar_outliers"), datos))
  # Antes: las 24 celdas en 52.5 y n_cambiadas = 24.
  expect_equal(r$datos$v, esperado)
  expect_equal(r$registro$n_cambiadas, 6)
  expect_equal(r$registro$n_no_reversibles, 6)
})

test_that("C: reemplazar_separadores cambia solo los codigos 9 a 13 y no parte una letra", {
  x <- c("l1\tl2", "p1\u2028p2", "q1\u2029q2", "n1\u0085n2", "a", "b", "c", "d")
  datos <- data.frame(t = x, stringsAsFactors = FALSE)
  plan <- .plan_O134(datos)
  r <- suppressWarnings(aplicar(.solo_O134(plan, "reemplazar_separadores"), datos))
  # A mano: solo la tabulacion de la fila 1. Antes: 4 celdas.
  expect_equal(r$registro$n_cambiadas, 1)
  expect_identical(r$datos$t, c("l1 l2", x[-1L]))

  # Celdas `bytes` con UTF-8 valido: A con anillo es c3 85, a con ogonek c4 85.
  b <- c("\u00c5sa\tB", "Wis\u0142a \u0105\tC", "a", "b")
  Encoding(b) <- "bytes"
  bytes <- data.frame(t = b, stringsAsFactors = FALSE)
  bytes$t <- b
  plan <- .plan_O134(bytes)
  skip_if_not(any(plan$estrategia == "reemplazar_separadores"))
  r <- suppressWarnings(aplicar(.solo_O134(plan, "reemplazar_separadores"), bytes))
  # A mano: solo el 09 pasa a 20. Antes: c3 85 -> c3 20, UTF-8 invalido.
  expect_identical(charToRaw(r$datos$t[[1L]]),
                   as.raw(c(0xc3, 0x85, 0x73, 0x61, 0x20, 0x42)))
  expect_true(all(validUTF8(r$datos$t)))
})

test_that("D: la imputacion por dependencia respeta la aplicabilidad", {
  datos <- data.frame(codigo = rep(1:3, each = 6L))
  datos$descripcion <- rep(c("A", "B", "C"), each = 6L)
  datos$flag <- "si"
  datos$flag[c(2L, 9L)] <- "no"
  datos$descripcion[c(2L, 9L, 14L)] <- NA
  aplic <- list(descripcion = ~ flag == "si")
  perfil <- suppressWarnings(perfilar(datos, muestra = Inf, aplicabilidad = aplic))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  i <- which(startsWith(as.character(plan$estrategia), "imputar_dependencia") &
               plan$columna == "descripcion")[1L]
  skip_if(is.na(i))
  # A mano: las filas 2 y 9 estan fuera del universo (vacio por diseno); solo
  # la 14 se imputa, con "C". Antes: n_afectadas 3 e imputaba las tres.
  expect_equal(plan$n_afectadas[[i]], 1)
  plan$aplicar <- seq_len(nrow(plan)) == i
  r <- suppressWarnings(aplicar(plan, datos))
  esperado <- datos$descripcion
  esperado[14L] <- "C"
  expect_identical(r$datos$descripcion, esperado)
  expect_equal(r$registro$n_cambiadas, 1)
})

test_that("E: -2147483648 no se convierte a entero ni se recomienda", {
  datos <- data.frame(x = c("-2147483648", "5", "6", "7"), stringsAsFactors = FALSE)
  # A mano: el rango de integer es +-2147483647 y as.integer(-2147483648) es NA.
  expect_true(is.na(suppressWarnings(as.integer(-2147483648))))
  plan <- .plan_O134(datos)
  fila <- as.character(plan$estrategia) == "convertir_tipo"
  expect_true(any(fila))
  # Antes: recomendada, activa e inyectiva; aplicar dejaba NA con
  # n_no_reversibles = 0.
  expect_false(any(plan$aplicar[fila]))
  r <- suppressWarnings(aplicar(plan, datos))
  expect_false(anyNA(r$datos$x))
  # El borde de adentro sigue convirtiendo.
  borde <- data.frame(x = c("-2147483647", "5", "6", "7"), stringsAsFactors = FALSE)
  r <- suppressWarnings(aplicar(.plan_O134(borde), borde))
  expect_identical(r$datos$x, c(-2147483647L, 5L, 6L, 7L))
})

test_that("F: los centinelas de una columna protegida se convierten sin publicarse", {
  cedulas <- 10000003 + 1234567 * seq_len(21L)
  datos <- data.frame(cedula = c(cedulas, 99999999, 99999999, -999))
  perfil <- suppressWarnings(perfilar(datos, muestra = Inf, analizar_dependencias = FALSE))
  skip_if_not("cedula" %in% lupa:::.columnas_personales_protegidas(perfil))
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  i <- which(plan$estrategia == "convertir_sentinelas_numericos")
  expect_length(i, 1L)
  # El catalogo del paquete no es un dato: se publica entero. Antes salia con
  # un hueco en la posicion de -999 y la accion quedaba sin efecto.
  expect_identical(plan$parametros[[i]]$valores, c(-9999, -999, -99, -9, 999))
  r <- suppressWarnings(aplicar(.solo_O134(plan, "convertir_sentinelas_numericos"), datos))
  # A mano: solo el -999 pasa a NA; 99999999 no es un centinela declarado.
  expect_identical(r$registro$estado, "ejecutada")
  expect_equal(r$registro$n_cambiadas, 1)
  expect_identical(which(is.na(r$datos$cedula)), 24L)

  # Declarado por el usuario: el perfil protegido ya no lo conserva, el plan
  # no lo publica y `aplicar()` lo resuelve sobre los datos.
  declarado <- data.frame(cedula = c(cedulas[1:20], rep(99999999, 6L)))
  perfil <- suppressWarnings(perfilar(
    declarado, muestra = Inf, analizar_dependencias = FALSE,
    sentinelas_numericos = 99999999
  ))
  skip_if_not("cedula" %in% lupa:::.columnas_personales_protegidas(perfil))
  plan <- suppressWarnings(planificar_limpieza(perfil, declarado))
  i <- which(plan$estrategia == "convertir_sentinelas_numericos")
  expect_length(i, 1L)
  expect_identical(as.character(plan$estado[[i]]), "lista")
  publicado <- c(
    capture.output(print(as.data.frame(plan))),
    vapply(plan$parametros, function(p) paste(deparse(p), collapse = " "), "")
  )
  expect_false(any(grepl("99999999", publicado, fixed = TRUE)))
  r <- suppressWarnings(aplicar(.solo_O134(plan, "convertir_sentinelas_numericos"), declarado))
  # A mano: las seis celdas declaradas a NA. Antes: `fallida` y las seis seguian.
  expect_equal(r$registro$n_cambiadas, 6)
  expect_identical(which(is.na(r$datos$cedula)), 21:26)
  registro <- paste(deparse(r$registro$parametros), collapse = " ")
  expect_false(grepl("99999999", registro, fixed = TRUE))

  # Si lo tapado no se puede reconstruir sobre los datos, la accion no se
  # ofrece como lista: queda bloqueada y dice por que.
  otro <- data.frame(cedula = c(cedulas[1:20], rep(12345678, 3L)))
  perfil <- suppressWarnings(perfilar(
    otro, muestra = Inf, analizar_dependencias = FALSE,
    sentinelas_numericos = 12345678
  ))
  skip_if_not("cedula" %in% lupa:::.columnas_personales_protegidas(perfil))
  plan <- suppressWarnings(planificar_limpieza(perfil, otro))
  i <- which(plan$estrategia == "convertir_sentinelas_numericos")
  expect_length(i, 1L)
  expect_true(anyNA(plan$parametros[[i]]$valores))
  expect_identical(as.character(plan$estado[[i]]), "bloqueada")
  expect_false(plan$aplicar[[i]])
  expect_match(plan$justificacion[[i]], "valor protegido", fixed = TRUE)
})

test_that("G: el plan no nombra marcadores que el detector descarto", {
  estados <- c("ca", "tx", "ny", "fl", "wa", "or", "nv", "az", "ut", "sd", "nd", "co")
  datos <- data.frame(estado = c(estados, "S/D"), stringsAsFactors = FALSE)
  # A mano: con 13 valores distintos el detector saca sd y nd del catalogo;
  # solo S/D es un marcador.
  plan <- .plan_O134(datos)
  i <- which(plan$estrategia == "convertir_ausencias_textuales")
  expect_length(i, 1L)
  # Antes: valores = c("sd", "nd", "s/d"), no recomendada por "sd, nd".
  expect_identical(plan$parametros[[i]]$valores, "s/d")
  expect_true(plan$recomendada[[i]])
  expect_false(grepl("sd, nd", plan$justificacion[[i]], fixed = TRUE))
})

test_that("I: planificar de nuevo sobre lo aplicado no actua sobre las marcas", {
  datos <- data.frame(id = 1:6, x = c("a", NA, "b", "c", "a", "d"),
                      stringsAsFactors = FALSE)
  datos <- rbind(datos, datos[1L, ])
  ronda <- function(d) {
    plan <- .plan_O134(d)
    plan$aplicar <- plan$recomendada & as.character(plan$estado) == "lista"
    list(plan = plan, resultado = suppressWarnings(aplicar(plan, d)))
  }
  primera <- ronda(datos)
  expect_true(all(c(".fila_duplicada", ".grupo_duplicado", ".ausente_x") %in%
                    names(primera$resultado$datos)))
  segunda <- ronda(primera$resultado$datos)
  marcas <- c(".fila_duplicada", ".grupo_duplicado", ".ausente_x")
  # Antes: marcaba ausentes en `.grupo_duplicado` -cinco filas que solo no
  # participan de un grupo- y volvia a recomendar marcar `x`, que fallaba.
  expect_false(any(segunda$plan$columna %in% marcas))
  expect_false(any(segunda$resultado$registro$estado == "fallida"))
  expect_false(".ausente_.grupo_duplicado" %in% names(segunda$resultado$datos))
  i <- which(segunda$plan$estrategia == "marcar_filas_ausentes" &
               segunda$plan$columna == "x")
  expect_identical(as.character(segunda$plan$estado[i]), "bloqueada")
  sin_accion <- attr(segunda$plan, "hallazgos_sin_accion")
  expect_true(".grupo_duplicado" %in% sin_accion$columna)
})

test_that("J: ampliar valores de convertir_ausencias_textuales convierte lo agregado", {
  datos <- data.frame(cat = c(" S/D ", "B", "A ", "N/A", "B", "C"),
                      stringsAsFactors = FALSE)
  plan <- .solo_O134(.plan_O134(datos), "convertir_ausencias_textuales")
  i <- which(plan$aplicar)
  expect_identical(plan$parametros[[i]]$valores, c("s/d", "n/a"))
  plan$parametros[[i]]$valores <- c(plan$parametros[[i]]$valores, "b")
  r <- suppressWarnings(aplicar(plan, datos))
  # A mano: el plan editado nombra s/d, n/a y b -> filas 1, 2, 4 y 5 a NA.
  # Antes: 2 celdas, las "B" seguian y el registro publicaba la lista ampliada.
  expect_identical(r$datos$cat, c(NA, NA, "A ", NA, NA, "C"))
  expect_equal(r$registro$n_cambiadas, 4)
})

test_that("K: guiar_limpieza imprime el numero que resuelve", {
  datos <- data.frame(zona = c("Norte", "NORTE", "sur", "Sur", "Este"),
                      stringsAsFactors = FALSE)
  plan <- .plan_O134(datos)
  # Reordenar el plan es una edicion inocua; la bloqueada queda tercera.
  plan <- plan[order(as.character(plan$estrategia)), , drop = FALSE]
  elegir <- "convertir_titulo"
  numero <- NA_integer_
  mensajes <- testthat::capture_messages(guiado <- guiar_limpieza(
    plan, datos, selector = function(decision) 0L
  ))
  lineas <- grep("^[0-9]+\\. ", mensajes, value = TRUE)
  numeros <- as.integer(sub("^([0-9]+)\\..*", "\\1", lineas))
  estrategias <- sub("^[0-9]+\\. ([a-z_]+).*", "\\1", lineas)
  # Antes: "1.", "2.", "4." -la bloqueada se comia el 3-.
  expect_identical(numeros, seq_along(lineas))
  numero <- numeros[estrategias == elegir]
  expect_length(numero, 1L)
  guiado <- suppressMessages(guiar_limpieza(
    plan, datos, selector = function(decision) numero
  ))
  # Elegir el numero que se ve activa la estrategia que se ve.
  expect_identical(as.character(guiado$estrategia[guiado$aplicar]), elegir)
})

test_that("L: quitar filas o convertir el tipo conserva la etiqueta de variable", {
  datos <- data.frame(
    id = c(1, 2, 3, 3, 4, 1),
    grupo = c("a", "b", "c", "c", NA, "a"),
    num = c("1", "2", "3", "3", "5", "1"),
    stringsAsFactors = FALSE
  )
  for (nombre in names(datos)) {
    attr(datos[[nombre]], "label") <- paste("Etiqueta de", nombre)
  }
  plan <- .plan_O134(datos)
  etiquetas <- function(tabla) {
    vapply(tabla[names(datos)], function(x) {
      e <- attr(x, "label", exact = TRUE)
      if (is.null(e)) NA_character_ else e
    }, character(1L))
  }
  for (estrategia in c("eliminar_filas_ausentes", "conservar_primera_duplicada",
                       "convertir_tipo")) {
    if (!any(plan$estrategia == estrategia)) next
    r <- suppressWarnings(aplicar(
      .solo_O134(plan, estrategia), datos, permitir_eliminacion = TRUE
    ))
    expect_identical(r$registro$estado, "ejecutada", info = estrategia)
    # Antes: las tres columnas sin etiqueta al quitar filas, y `num` al
    # convertirla.
    expect_identical(unname(etiquetas(r$datos)),
                     paste("Etiqueta de", names(datos)), info = estrategia)
  }
})

test_that("M: quitar filas de un data.table con clave conserva la clave", {
  skip_if_not_installed("data.table")
  base <- data.frame(id = c(1, 2, 2, 3, 4), v = c("a", "b", "b", NA, "d"),
                     stringsAsFactors = FALSE)
  dt <- data.table::as.data.table(base)
  data.table::setkey(dt, id)
  plan <- .plan_O134(dt)
  for (estrategia in c("conservar_primera_duplicada", "eliminar_filas_ausentes")) {
    r <- suppressWarnings(aplicar(
      .solo_O134(plan, estrategia), dt, permitir_eliminacion = TRUE
    ))
    expect_identical(r$registro$estado, "ejecutada", info = estrategia)
    # A mano: quitar filas de una tabla ordenada por id la deja ordenada.
    expect_identical(data.table::key(r$datos), "id", info = estrategia)
  }
  expect_identical(data.table::key(dt), "id")
})

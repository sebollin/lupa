# `columnas_duplicadas` publica un hallazgo por GRUPO de columnas iguales, como
# `filas_duplicadas` con sus filas. Con uno por par, k columnas iguales eran
# k (k - 1) / 2 hallazgos: con `A = B = C` el plan proponia eliminar B, C y otra
# vez C, y la tercera eliminacion quedaba `fallida` al aplicarse; la deriva
# publicaba dos pares resueltos cuando una sola columna dejaba de ser igual; y
# con 1.000 columnas iguales eran 499.500 hallazgos y planificar no terminaba.

# Fuera de CRAN: el archivo arma unos treinta perfiles y tarda mas de los 0,2 s
# que se admiten en modo CRAN. Corre en la suite completa y en el CI, con
# NOT_CRAN=true.
testthat::skip_on_cran()

.dup_perfil <- function(datos, mes = 1L, ...) {
  suppressWarnings(perfilar(
    datos, fecha = as.POSIXct(sprintf("2026-%02d-01", mes), tz = "UTC"),
    analizar_dependencias = FALSE, ...
  ))
}

.dup_hallazgos <- function(perfil) {
  h <- hallazgos(perfil)
  h[h$tipo_hallazgo == "columnas_duplicadas", , drop = FALSE]
}

.dup_iguales <- function() {
  data.frame(A = 1:5, B = 1:5, C = 1:5, D = c(2L, 1L, 3L, 5L, 4L))
}

test_that("tres columnas iguales son un hallazgo que las nombra a todas", {
  perfil <- .dup_perfil(.dup_iguales())
  h <- .dup_hallazgos(perfil)
  expect_identical(nrow(h), 1L)
  expect_identical(h$columna, "A")
  expect_identical(h$evidencia, "A = B = C; comparadas sobre 5 filas")
  expect_identical(h$descripcion, "3 columnas tienen el mismo contenido.")
  expect_identical(
    h$sugerencia,
    "Confirmar si todas las columnas son necesarias o si existe redundancia."
  )
  expect_equal(h$n_evaluados, 4)
  expect_equal(h$n_afectados, 3)
  expect_identical(h$unidad_conteo, "columna")
  # El perfil guarda cada repetida junto a la primera: k - 1 filas.
  expect_identical(perfil$general$columnas_duplicadas$columna_1, c("A", "A"))
  expect_identical(perfil$general$columnas_duplicadas$columna_2, c("B", "C"))

  una_fila <- .dup_hallazgos(.dup_perfil(data.frame(p = 1, q = 1, r = 1)))
  expect_identical(una_fila$evidencia, "p = q = r; comparadas sobre 1 fila")
})

test_that("con dos columnas el hallazgo es el de siempre, y los grupos van en el orden de los datos", {
  par <- c(2L, 1L, 3L, 5L, 4L)
  datos <- data.frame(D = par, A = 1:5, E = par, B = 1:5, C = 1:5)
  h <- .dup_hallazgos(.dup_perfil(datos))
  expect_identical(h$columna, c("D", "A"))
  expect_identical(h$evidencia, c(
    "D = E; comparadas sobre 5 filas", "A = B = C; comparadas sobre 5 filas"
  ))
  expect_identical(h$descripcion[[1L]], "Dos columnas tienen el mismo contenido.")
  expect_identical(
    h$sugerencia[[1L]],
    "Confirmar si ambas columnas son necesarias o si existe redundancia."
  )
  expect_equal(h$n_afectados, c(2, 3))
})

test_that("el plan tiene una decision por grupo y aplicarla no deja acciones fallidas", {
  par <- c(2L, 1L, 3L, 5L, 4L)
  casos <- list(
    "A = B = C" = list(.dup_iguales(), c("A", "D")),
    "A = B = C y D = E" = list(transform(.dup_iguales(), E = par), c("A", "D")),
    "60 iguales" = list(
      as.data.frame(stats::setNames(rep(list(c("x", "y", "z")), 60L),
                                    paste0("V", 1:60)),
                    stringsAsFactors = FALSE),
      "V1"
    )
  )
  for (nombre in names(casos)) {
    datos <- casos[[nombre]][[1L]]
    perfil <- .dup_perfil(datos)
    plan <- planificar_limpieza(perfil, datos)
    n_grupos <- nrow(.dup_hallazgos(perfil))
    duplicadas <- plan[plan$hallazgo == "columnas_duplicadas", , drop = FALSE]
    expect_identical(length(unique(duplicadas$grupo)), n_grupos, label = nombre)
    expect_identical(nrow(duplicadas), 2L * n_grupos, label = nombre)
    expect_true(all(duplicadas$estado == "lista"), label = nombre)

    recomendado <- aplicar(plan, datos)
    expect_true(all(recomendado$registro$estado == "ejecutada"), label = nombre)

    destructivo <- plan
    destructivo$aplicar <- destructivo$estrategia == "eliminar_columna_duplicada"
    resultado <- aplicar(destructivo, datos, permitir_eliminacion = TRUE)
    expect_true(all(resultado$registro$estado == "ejecutada"), label = nombre)
    expect_identical(names(resultado$datos), casos[[nombre]][[2L]], label = nombre)
    expect_identical(
      sum(resultado$registro$n_columnas_eliminadas),
      as.numeric(ncol(datos) - length(casos[[nombre]][[2L]])), label = nombre
    )
  }

  datos <- .dup_iguales()
  plan <- planificar_limpieza(.dup_perfil(datos), datos)
  duplicadas <- plan[plan$hallazgo == "columnas_duplicadas", , drop = FALSE]
  for (parametros in duplicadas$parametros) {
    expect_identical(parametros$columna_1, "A")
    expect_identical(parametros$columna_2, c("B", "C"))
    expect_identical(parametros$eliminar, c("B", "C"))
  }
  expect_equal(duplicadas$n_afectadas, c(2, 2))
  marcado <- aplicar(plan, datos)
  marcas <- attr(marcado$datos, "columnas_duplicadas_marcadas")
  expect_identical(paste(marcas$columna_1, marcas$columna_2), c("A B", "A C"))
  expect_equal(
    marcado$registro$n_cambiadas[
      marcado$registro$estrategia == "marcar_columnas_duplicadas"
    ], 2
  )

  destructivo <- plan
  destructivo$aplicar <- destructivo$estrategia == "eliminar_columna_duplicada"
  resultado <- aplicar(destructivo, datos, permitir_eliminacion = TRUE)
  expect_identical(resultado$registro$columna, "B,C")
  retirado <- resultado$eliminados$columnas[[1L]]
  expect_identical(retirado$nombre, c("B", "C"))
  expect_identical(retirado$valores, list(B = 1:5, C = 1:5))

  # Si una de las repetidas deja de ser igual, la accion falla entera y no
  # toca los datos.
  cambiados <- datos
  cambiados$C[[1L]] <- 99L
  fallo <- aplicar(destructivo, cambiados, permitir_eliminacion = TRUE)
  expect_identical(fallo$registro$estado, "fallida")
  expect_identical(names(fallo$datos), names(cambiados))
})

# La forma de una version anterior: un hallazgo y una fila de `general` por
# cada par de columnas iguales, en el orden de `combn()`.
.dup_a_pares <- function(perfil) {
  h <- perfil$hallazgos
  filas <- list()
  pares <- list()
  for (i in seq_len(nrow(h))) {
    if (!identical(as.character(h$tipo_hallazgo[[i]]), "columnas_duplicadas")) {
      filas[[length(filas) + 1L]] <- h[i, , drop = FALSE]
      next
    }
    tramos <- strsplit(h$evidencia[[i]], "; ", fixed = TRUE)[[1L]]
    miembros <- strsplit(tramos[[1L]], " = ", fixed = TRUE)[[1L]]
    combinados <- utils::combn(miembros, 2L)
    for (j in seq_len(ncol(combinados))) {
      fila <- h[i, , drop = FALSE]
      fila$columna <- combinados[1L, j]
      fila$descripcion <- "Dos columnas tienen el mismo contenido."
      fila$evidencia <- paste0(combinados[1L, j], " = ", combinados[2L, j],
                               "; ", tramos[[2L]])
      fila$sugerencia <-
        "Confirmar si ambas columnas son necesarias o si existe redundancia."
      fila$n_afectados <- 2
      filas[[length(filas) + 1L]] <- fila
      pares[[length(pares) + 1L]] <- data.frame(
        columna_1 = combinados[1L, j], columna_2 = combinados[2L, j],
        stringsAsFactors = FALSE
      )
    }
  }
  viejo <- perfil
  nuevos <- do.call(rbind, filas)
  rownames(nuevos) <- NULL
  attr(nuevos, "cobertura_diagnosticos") <- attr(
    h, "cobertura_diagnosticos", exact = TRUE
  )
  viejo$hallazgos <- nuevos
  viejo$general$columnas_duplicadas <- do.call(rbind, pares)
  viejo
}

.dup_deriva <- function(anterior, actual) {
  d <- as.data.frame(suppressMessages(comparar_perfiles(anterior, actual)))
  d[d$aspecto == "hallazgo" &
      (d$valor_anterior %in% "columnas_duplicadas" |
         d$valor_actual %in% "columnas_duplicadas"),
    c("columna", "cambio", "evidencia"), drop = FALSE]
}

.dup_base <- function() {
  data.frame(A = c(1, 2, 3, 4, 5, 6), B = c(1, 2, 3, 4, 5, 6),
             C = c(1, 2, 3, 4, 5, 6), D = c(9, 3, 7, 1, 8, 2))
}

test_that("la deriva compara los grupos por las igualdades que afirman", {
  base <- .dup_base()
  otro <- c(6, 5, 4, 3, 2, 1)
  deriva <- function(a, b) .dup_deriva(.dup_perfil(a, 1L), .dup_perfil(b, 2L))

  expect_identical(nrow(deriva(base, base)), 0L)
  # La primera del grupo cambia y el grupo es el mismo.
  expect_identical(nrow(deriva(base, base[c("C", "B", "A", "D")])), 0L)
  # C deja de ser igual: se resuelve el grupo, y `A = B`, que sigue, no
  # aparece.
  d <- deriva(base, transform(base, C = otro))
  expect_identical(d$cambio, "resuelto")
  expect_match(d$evidencia, "^A = B = C;")
  # Aparece F igual a A: aparece el grupo, y el viejo no se resuelve.
  d <- deriva(base, transform(base, F = A))
  expect_identical(d$cambio, "aparecido")
  expect_match(d$evidencia, "^A = B = C = F;")
  # Dos grupos que se unen, y uno que se parte.
  separados <- transform(base, C = otro, D = otro)
  unidos <- transform(base, C = A, D = A)
  d <- deriva(separados, unidos)
  expect_identical(d$cambio, "aparecido")
  expect_match(d$evidencia, "^A = B = C = D;")
  d <- deriva(unidos, separados)
  expect_identical(d$cambio, "resuelto")
  expect_match(d$evidencia, "^A = B = C = D;")
  # Una columna retirada no resuelve a las demas; si ademas otra deja de ser
  # igual, si.
  expect_identical(nrow(deriva(base, base[c("A", "B", "D")])), 0L)
  expect_identical(nrow(deriva(base, base[c("B", "C", "D")])), 0L)
  d <- deriva(base, transform(base[c("A", "B", "D")], B = otro))
  expect_identical(d$cambio, "resuelto")
  # Una columna nueva igual a otra si es una igualdad nueva.
  d <- deriva(base[c("A", "D")], transform(base[c("A", "D")], F = A))
  expect_identical(d$cambio, "aparecido")
})

test_that("un perfil guardado con un hallazgo por par se compara sin deriva falsa", {
  base <- .dup_base()
  viejo <- .dup_a_pares(.dup_perfil(base, 1L))
  expect_identical(
    sum(viejo$hallazgos$tipo_hallazgo == "columnas_duplicadas"), 3L
  )
  expect_identical(nrow(viejo$general$columnas_duplicadas), 3L)
  expect_identical(nrow(.dup_deriva(viejo, .dup_perfil(base, 2L))), 0L)
  expect_identical(
    nrow(.dup_deriva(viejo, .dup_perfil(base[c("C", "B", "A", "D")], 2L))), 0L
  )
  d <- .dup_deriva(viejo, .dup_perfil(transform(base, C = rev(C)), 2L))
  expect_identical(d$cambio, "resuelto")
  # Se nombra el grupo, no el primer par: `A = B` sigue valiendo.
  expect_match(d$evidencia, "^A = B = C;")
  # Y al reves, contra un perfil viejo posterior.
  expect_identical(
    nrow(.dup_deriva(.dup_perfil(base, 1L), .dup_a_pares(.dup_perfil(base, 2L)))),
    0L
  )
  # El plan lee el perfil viejo como un grupo: una decision, que aplica.
  plan <- planificar_limpieza(viejo, base)
  duplicadas <- plan[plan$hallazgo == "columnas_duplicadas", , drop = FALSE]
  expect_identical(nrow(duplicadas), 2L)
  expect_identical(duplicadas$parametros[[1L]]$eliminar, c("B", "C"))
  # Los otros pares del grupo estan cubiertos por esa decision: no se declaran
  # como hallazgos sin accion.
  sin_accion <- attr(plan, "hallazgos_sin_accion", exact = TRUE)
  expect_false(any(sin_accion$hallazgo == "columnas_duplicadas"))
  destructivo <- plan
  destructivo$aplicar <- destructivo$estrategia == "eliminar_columna_duplicada"
  resultado <- aplicar(destructivo, base, permitir_eliminacion = TRUE)
  expect_true(all(resultado$registro$estado == "ejecutada"))
  expect_identical(names(resultado$datos), c("A", "D"))
})

test_that("con la proteccion, el grupo que nombra a una persona conserva sus acciones", {
  set.seed(2579L)
  n <- 60L
  d <- data.frame(
    titular = rep(c("Juan Perez Gonzalez", "Maria Lopez Fernandez",
                    "Carlos Rodriguez Silva"), length.out = n),
    stringsAsFactors = FALSE
  )
  d[["Juan Perez Gonzalez"]] <- round(stats::runif(n, 1, 12))
  d$copia <- d[["Juan Perez Gonzalez"]]
  d$otra <- d$copia
  planes <- lapply(c(FALSE, TRUE), function(proteger) {
    p <- .dup_perfil(d, columnas_personales = c(titular = "nombre_persona"),
                     proteger_datos_personales = proteger)
    plan <- suppressWarnings(planificar_limpieza(p, d))
    k <- which(plan$hallazgo == "columnas_duplicadas")
    list(estado = as.character(plan$estado[k]), parametros = plan$parametros[k])
  })
  expect_identical(planes[[1L]]$estado, c("lista", "lista"))
  expect_identical(planes[[2L]], planes[[1L]])
  expect_identical(planes[[1L]]$parametros[[2L]]$eliminar, c("copia", "otra"))
})

# Propiedades de los invisibles que la reparacion de codificacion consume.
testthat::skip_on_cran()

# Un proceso hijo carga el mismo paquete que se prueba: el arbol fuente si la
# prueba corre desde el (test_local), o el instalado si corre dentro de
# R CMD check, donde la raiz de las pruebas no tiene DESCRIPTION.
.c1_moji_carga_hijo <- function() {
  ruta <- normalizePath(testthat::test_path("../.."), winslash = "/",
                        mustWork = FALSE)
  if (file.exists(file.path(ruta, "DESCRIPTION")) &&
      dir.exists(file.path(ruta, "R"))) {
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE))",
            deparse(ruta))
  } else {
    "suppressMessages(library(lupa))"
  }
}

.c1_moji_latin1 <- function(x) {
  iconv(x, from = "latin1", to = "UTF-8")
}

.c1_moji_windows1252 <- function(x) {
  tabla <- lupa:::.ftfy_tablas_bytes[["sloppy-windows-1252"]]
  vapply(x, function(texto) {
    bytes <- as.integer(charToRaw(texto))
    codigo <- bytes
    indices <- bytes >= 128L
    codigo[indices] <- tabla[bytes[indices] - 127L]
    intToUtf8(codigo)
  }, character(1L), USE.NAMES = FALSE)
}

.c1_moji_plan <- function(datos) {
  perfil <- perfilar(
    datos, muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  list(perfil = perfil, plan = planificar_limpieza(perfil, datos))
}

.c1_moji_aplicar_orden <- function(plan, datos, acciones) {
  acciones <- as.character(acciones)
  subplan <- plan[plan$estrategia %in% acciones, , drop = FALSE]
  subplan$aplicar <- TRUE
  subplan$orden <- match(as.character(subplan$estrategia), acciones)
  aplicar(subplan, datos)$datos
}

.c1_moji_permutaciones <- function(x) {
  if (length(x) <= 1L) return(list(x))
  unlist(lapply(seq_along(x), function(i) {
    resto <- x[-i]
    lapply(.c1_moji_permutaciones(resto), function(y) c(x[[i]], y))
  }), recursive = FALSE)
}

.c1_moji_bateria <- function() {
  originales <- c(
    "\u00c0", "\u00e0", "\u00dd",
    "\u00c1", "\u00e1", "\u00c9", "\u00e9", "\u00cd", "\u00ed",
    "\u00d3", "\u00f3", "\u00da", "\u00fa", "\u00d1", "\u00f1",
    "\u00dc", "\u00fc", "\u2013", "\u20ac", "\u00bf", "\u00a1"
  )
  casos <- c(
    .c1_moji_latin1(originales),
    .c1_moji_latin1(.c1_moji_latin1(originales)),
    .c1_moji_windows1252(originales),
    .c1_moji_windows1252(.c1_moji_windows1252(originales)),
    intToUtf8(c(0x80L, 0x85L, 0x91L:0x94L, 0x96L, 0x97L), multiple = TRUE),
    intToUtf8(c(0x81L, 0x8dL, 0x8fL, 0x90L, 0x9dL), multiple = TRUE),
    paste0("Mar\u00c3\u00ad a"), paste0("Y\u00c3\u009d")
  )
  fixture <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "ftfy-corpus-6.3.1.json"),
    simplifyVector = FALSE
  )
  c(casos, vapply(fixture, `[[`, character(1L), "original"))
}

.c1_moji_generados <- function() {
  set.seed(20261111L)
  ascii <- c("a", "B", "7", " ", "-", "z", "Q")
  acentuadas <- c(
    "\u00e1", "\u00e9", "\u00ed", "\u00f3", "\u00fa", "\u00f1", "\u00fc",
    "\u00c1", "\u00c9", "\u00cd", "\u00d3", "\u00da", "\u00d1", "\u00dc"
  )
  invisibles <- c(
    "", intToUtf8(0x80L), paste0(intToUtf8(0x80L), intToUtf8(0x80L)),
    intToUtf8(0x81L), paste0(intToUtf8(0x81L), intToUtf8(0x81L)),
    intToUtf8(0xADL), paste0(intToUtf8(0xADL), intToUtf8(0xADL)),
    intToUtf8(0xA0L), paste0(intToUtf8(0xA0L), intToUtf8(0xA0L))
  )
  vapply(seq_len(24L), function(i) {
    limpio <- paste0(
      paste(sample(ascii, sample(0:7, 1L), replace = TRUE), collapse = ""),
      sample(acentuadas, 1L),
      paste(sample(ascii, sample(0:7, 1L), replace = TRUE), collapse = "")
    )
    roto <- if (sample(c(FALSE, TRUE), 1L)) {
      .c1_moji_latin1(.c1_moji_latin1(limpio))
    } else {
      .c1_moji_latin1(limpio)
    }
    paste0(sample(invisibles, 1L), roto, sample(invisibles, 1L))
  }, character(1L))
}

.c1_moji_tablas_generadas <- function() {
  set.seed(20261111L)
  n <- 36L
  limpio <- rep(c(
    "nino", "ni\u00f1o", "", NA_character_, " texto limpio ", "Montevideo"
  ), length.out = n)
  factor_texto <- factor(rep(c(
    "A", "ni\u00f1o", "", NA_character_, "B", "C"
  ), length.out = n), exclude = NULL)
  latin1 <- iconv(rep(c(
    "ni\u00f1o", "canci\u00f3n", "", NA_character_, "Jos\u00e9"
  ), length.out = n), from = "UTF-8", to = "latin1")
  Encoding(latin1) <- "latin1"

  controles <- rep(c(
    "ok", paste0("A", intToUtf8(1L), "B"),
    paste0("x", intToUtf8(0x81L), "y"),
    paste0("z", intToUtf8(0xADL), "z")
  ), length.out = n)
  controles[[1L]] <- paste0(
    intToUtf8(1L), intToUtf8(c(0xD0L, 0xA0L, 0x81L)), intToUtf8(0xA0L)
  )
  espacios <- rep(c(
    " a ", "ni\u00f1o", "", NA_character_,
    paste0("x", intToUtf8(0xA0L), "y"), " borde "
  ), length.out = n)
  roto2 <- rep(c(
    "limpio", paste0("n", intToUtf8(c(0xC3L, 0xB1L)), "o"), ""
  ), length.out = n)
  roto3 <- rep(c(
    paste0("euro ", intToUtf8(c(0xE2L, 0x82L, 0xACL))), "ni\u00f1o",
    paste0("a", intToUtf8(c(0xD0L, 0xA0L, 0x81L)), "b")
  ), length.out = n)
  roto4 <- rep(c(
    paste0("cara ", intToUtf8(c(0xF0L, 0x9FL, 0x98L, 0x80L))),
    "texto", NA_character_
  ), length.out = n)
  entidades <- rep(c(
    "&amp;", "&nbsp;", "texto", "", NA_character_,
    paste0(intToUtf8(0xC3L), "&nbsp;")
  ), length.out = n)
  borde <- rep(c(
    " adelante", "atr\u00e1s ", " adentro ", "sin borde", "", NA_character_,
    paste0(intToUtf8(0xC2L), " ")
  ), length.out = n)
  tabla <- data.frame(
    limpio = limpio, factor_texto = factor_texto, latin1 = latin1,
    controles = controles, espacios = espacios, roto2 = roto2,
    roto3 = roto3, roto4 = roto4, entidades = entidades, borde = borde,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    tabla,
    tabla[c(seq(36L, 1L, by = -1L), 1L:12L),
          c("borde", "roto4", "latin1", "controles", "entidades", "factor_texto"),
          drop = FALSE],
    data.frame(
      limpio = limpio[c(1L, 4L, 7L, 10L, 13L, 16L)],
      factor_texto = factor_texto[c(1L, 4L, 7L, 10L, 13L, 16L)],
      latin1 = latin1[c(1L, 4L, 7L, 10L, 13L, 16L)],
      controles = controles[c(1L, 4L, 7L, 10L, 13L, 16L)],
      espacios = espacios[c(1L, 4L, 7L, 10L, 13L, 16L)],
      roto2 = roto2[c(1L, 4L, 7L, 10L, 13L, 16L)],
      roto3 = roto3[c(1L, 4L, 7L, 10L, 13L, 16L)],
      roto4 = roto4[c(1L, 4L, 7L, 10L, 13L, 16L)],
      entidades = entidades[c(1L, 4L, 7L, 10L, 13L, 16L)],
      borde = borde[c(1L, 4L, 7L, 10L, 13L, 16L)],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  )
}

.c1_moji_cambios_protegidos <- function(textos) {
  proteccion <- .proteccion_texto_valores(textos)
  list(
    n_espacios_borde = .recortar_texto(textos, proteccion)$valor,
    n_invisibles_eliminables = .quitar_controles_invisibles(
      textos, proteccion
    )$valor,
    n_espacios_invisibles = .normalizar_espacios_invisibles(
      textos, proteccion
    )$valor,
    n_separadores_en_campo = .reemplazar_separadores(
      textos, proteccion
    )$valor,
    n_entidades_html = .decodificar_entidades_html(
      textos, proteccion
    )$valor,
    n_codificacion_reparable = .reparar_codificacion(
      textos, list(max_iteraciones = 20L)
    )$valor
  )
}

.c1_moji_lcs_referencia <- function(entrada, salida) {
  n <- length(entrada)
  m <- length(salida)
  tabla <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  if (n && m) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        if (identical(entrada[[i]], salida[[j]])) {
          tabla[i + 1L, j + 1L] <- tabla[i, j] + 1L
        } else {
          tabla[i + 1L, j + 1L] <- max(
            tabla[i, j + 1L], tabla[i + 1L, j]
          )
        }
      }
    }
  }
  i <- n
  j <- m
  pares_entrada <- integer()
  pares_salida <- integer()
  while (i > 0L && j > 0L) {
    if (identical(entrada[[i]], salida[[j]])) {
      pares_entrada <- c(pares_entrada, i)
      pares_salida <- c(pares_salida, j)
      i <- i - 1L
      j <- j - 1L
    } else if (tabla[i, j + 1L] >= tabla[i + 1L, j]) {
      i <- i - 1L
    } else {
      j <- j - 1L
    }
  }
  list(
    entrada = rev(pares_entrada), salida = rev(pares_salida),
    longitud = tabla[n + 1L, m + 1L]
  )
}

.c1_moji_subsecuencia <- function(entrada, salida) {
  if (!length(entrada)) return(integer())
  indice <- 1L
  pares <- integer(length(entrada))
  for (i in seq_along(entrada)) {
    if (indice > length(salida)) return(NULL)
    candidatos <- which(salida[indice:length(salida)] == entrada[[i]])
    if (!length(candidatos)) return(NULL)
    pares[[i]] <- indice + candidatos[[1L]] - 1L
    indice <- pares[[i]] + 1L
  }
  pares
}

# Esta es la alineacion de la cuarta parte. Se conserva en la prueba solo para
# mostrar el caso que puede devolver indices equivocados; no es la referencia.
.c1_moji_alineacion_v4 <- function(entrada, salida) {
  n <- length(entrada)
  m <- length(salida)
  if (!n) return(logical())
  if (!m) return(rep(TRUE, n))
  limite <- n + m
  desplazamiento <- limite + 2L
  indice <- function(k) k + desplazamiento
  frente <- rep(0L, 2L * limite + 5L)
  frente[indice(0L)] <- 0L
  traza <- vector("list", limite + 1L)
  distancia <- NA_integer_
  for (d in 0:limite) {
    traza[[d + 1L]] <- frente
    for (k in seq.int(-d, d, by = 2L)) {
      x <- if (k == -d || (k != d &&
                           frente[indice(k - 1L)] < frente[indice(k + 1L)])) {
        frente[indice(k + 1L)]
      } else {
        frente[indice(k - 1L)] + 1L
      }
      y <- x - k
      while (x < n && y < m && identical(entrada[[x + 1L]], salida[[y + 1L]])) {
        x <- x + 1L
        y <- y + 1L
      }
      frente[indice(k)] <- x
      if (x >= n && y >= m) {
        distancia <- d
        break
      }
    }
    if (!is.na(distancia)) break
  }
  alineadas <- rep(FALSE, n)
  x <- n
  y <- m
  if (is.na(distancia)) return(rep(TRUE, n))
  if (distancia > 0L) {
    for (d in seq.int(distancia, 1L, by = -1L)) {
      anterior <- traza[[d + 1L]]
      k <- x - y
      k_anterior <- if (k == -d || (k != d &&
                                    anterior[indice(k - 1L)] <=
                                      anterior[indice(k + 1L)])) {
        k + 1L
      } else {
        k - 1L
      }
      x_anterior <- anterior[indice(k_anterior)]
      y_anterior <- x_anterior - k_anterior
      while (x > x_anterior && y > y_anterior) {
        alineadas[[x]] <- TRUE
        x <- x - 1L
        y <- y - 1L
      }
      x <- x_anterior
      y <- y_anterior
    }
  }
  while (x > 0L && y > 0L) {
    if (identical(entrada[[x]], salida[[y]])) alineadas[[x]] <- TRUE
    x <- x - 1L
    y <- y - 1L
  }
  !alineadas
}

test_that("los puntos reparados no generan un segundo hallazgo", {
  originales <- c(
    "\u00c0", "\u00e0", "\u00dd",
    "\u00c1", "\u00e1", "\u00c9", "\u00e9", "\u00cd", "\u00ed",
    "\u00d3", "\u00f3", "\u00da", "\u00fa", "\u00d1", "\u00f1",
    "\u00dc", "\u00fc", "\u2013", "\u20ac", "\u00bf", "\u00a1"
  )
  casos <- c(
    .c1_moji_latin1(originales),
    .c1_moji_latin1(.c1_moji_latin1(originales)),
    .c1_moji_windows1252(originales)
  )
  for (texto in casos) {
    perfil <- perfilar(
      data.frame(texto = texto, stringsAsFactors = FALSE),
      muestra = Inf, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_identical(perfil$hallazgos$tipo_hallazgo, "codificacion_rota")
  }
})

test_that("los C1 sueltos respetan lo que la reparacion puede convertir", {
  equivalentes <- c(0x80L, 0x85L, 0x91L:0x94L, 0x96L, 0x97L)
  sin_equivalente <- c(0x81L, 0x8dL, 0x8fL, 0x90L, 0x9dL)
  for (codigo in equivalentes) {
    perfil <- perfilar(
      data.frame(texto = intToUtf8(codigo), stringsAsFactors = FALSE),
      muestra = Inf, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_identical(perfil$hallazgos$tipo_hallazgo, "codificacion_rota")
  }
  for (codigo in sin_equivalente) {
    perfil <- perfilar(
      data.frame(texto = intToUtf8(codigo), stringsAsFactors = FALSE),
      muestra = Inf, analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    )
    expect_true("controles_invisibles" %in% perfil$hallazgos$tipo_hallazgo)
    expect_true("codificacion_rota" %in% perfil$hallazgos$tipo_hallazgo)
  }
})

test_that("los controles genuinos siguen siendo visibles y eliminables", {
  mojibake <- .c1_moji_latin1(c("A\u00c1rea", "N\u00d1"))
  texto <- paste0(
    mojibake[[1L]], intToUtf8(1L), "Q",
    mojibake[[2L]]
  )
  datos <- data.frame(texto = texto, stringsAsFactors = FALSE)
  piezas <- .c1_moji_plan(datos)
  fila <- piezas$perfil$columnas[piezas$perfil$columnas$columna == "texto", , drop = FALSE]
  expect_equal(fila$n_codificacion_rota, 1L)
  expect_equal(fila$n_controles_invisibles, 1L)
  expect_equal(fila$n_invisibles_eliminables, 1L)
  control <- piezas$perfil$hallazgos[
    piezas$perfil$hallazgos$tipo_hallazgo == "controles_invisibles", , drop = FALSE
  ]
  expect_true(grepl(paste0("A", "\u00c3"), control$evidencia, fixed = TRUE))
  expect_true(grepl("<U+0001>", control$evidencia, fixed = TRUE))
  expect_false(grepl("A\u00c1rea", control$evidencia, fixed = TRUE))

  solo_control <- piezas$plan
  solo_control$aplicar <- solo_control$estrategia ==
    "eliminar_controles_invisibles"
  salida_control <- aplicar(solo_control, datos)$datos$texto[[1L]]
  expect_identical(
    salida_control,
    paste0(mojibake[[1L]], "Q", mojibake[[2L]])
  )

  solo_reparacion <- piezas$plan
  solo_reparacion$aplicar <- solo_reparacion$estrategia ==
    "reparar_codificacion"
  salida_reparacion <- aplicar(solo_reparacion, datos)$datos$texto[[1L]]
  expect_identical(
    salida_reparacion,
    paste0("A\u00c1rea", intToUtf8(1L), "Q", "N\u00d1")
  )

  ambas <- aplicar(piezas$plan, datos)
  expect_identical(ambas$datos$texto[[1L]], paste0("A\u00c1reaQN\u00d1"))
  expect_true(all(ambas$registro$estado == "ejecutada"))
  expect_false("omitida" %in% ambas$registro$estado)
})

.c1_moji_acciones_directas <- list(
  recortar_espacios = function(x) .recortar_texto(x)$valor,
  eliminar_controles_invisibles = function(x) {
    .quitar_controles_invisibles(x)$valor
  },
  normalizar_espacios_invisibles = function(x) {
    .normalizar_espacios_invisibles(x)$valor
  },
  decodificar_entidades_html = function(x) {
    .decodificar_entidades_html(x)$valor
  },
  reemplazar_separadores = function(x) .reemplazar_separadores(x)$valor
)

.c1_moji_aplicar_directo <- function(x, orden) {
  for (nombre in orden) x <- .c1_moji_acciones_directas[[nombre]](x)
  x
}

.c1_moji_reparar_directo <- function(x) {
  vapply(
    x, function(texto) .ftfy_reparar_uno(texto)$texto,
    character(1L), USE.NAMES = FALSE
  )
}

.c1_moji_ordenes_p1 <- function(acciones) {
  pares <- unlist(lapply(seq_along(acciones), function(i) {
    lapply(setdiff(seq_along(acciones), i), function(j) {
      acciones[c(i, j)]
    })
  }), recursive = FALSE)
  c(
    list(character()),
    lapply(acciones, function(x) x),
    pares,
    list(acciones, rev(acciones))
  )
}

.c1_moji_casos_propiedad <- function() {
  c(
    .c1_moji_bateria(),
    .c1_moji_generados(),
    paste0("Mar", intToUtf8(0xAD), intToUtf8(0xC3L), intToUtf8(0xAD), "a"),
    paste0("Mar", intToUtf8(0x81), intToUtf8(0xC3L), intToUtf8(0xAD), "a"),
    paste0("Mar", intToUtf8(0xA0), intToUtf8(0xC3L), intToUtf8(0xAD), "a"),
    paste0(strrep(paste0(intToUtf8(0xC3L), intToUtf8(0xAD)), 200), intToUtf8(0x81)),
    paste0(strrep(paste0(intToUtf8(0xC3L), intToUtf8(0xAD)), 200), intToUtf8(0xAD))
  )
}

test_that("P1 separa las acciones de la reparacion", {
  casos <- .c1_moji_casos_propiedad()
  datos <- data.frame(texto = casos, stringsAsFactors = FALSE)
  piezas <- .c1_moji_plan(datos)
  acciones <- intersect(
    names(.c1_moji_acciones_directas), as.character(piezas$plan$estrategia)
  )
  perfil_inicial <- .analizar_codificacion(casos)
  # P1 habla de letras rotas: celdas con alguna posicion que la reparacion
  # consume o protege. Un C1 suelto que la reparacion deja como esta es un
  # control genuino; quitarlo resuelve el defecto, y el hallazgo puede irse.
  letra_rota <- vapply(casos, function(texto) {
    any(.ftfy_posiciones_consumidas(texto))
  }, logical(1L), USE.NAMES = FALSE)

  ordenes <- .c1_moji_ordenes_p1(acciones)
  for (orden in ordenes) {
    plan_a <- piezas$plan[piezas$plan$estrategia %in% orden, , drop = FALSE]
    if (nrow(plan_a)) {
      plan_a$aplicar <- TRUE
      plan_a$orden <- match(as.character(plan_a$estrategia), orden)
      despues_de_a <- aplicar(plan_a, datos)$datos
    } else {
      despues_de_a <- datos
    }
    perfil_despues <- .analizar_codificacion(despues_de_a$texto)
    expect_true(
      all(!(perfil_inicial$afectados & letra_rota) | perfil_despues$afectados),
      info = paste("orden:", paste(orden, collapse = ","))
    )
    plan_r <- piezas$plan[
      piezas$plan$estrategia == "reparar_codificacion", , drop = FALSE
    ]
    plan_r$aplicar <- TRUE
    izquierdo <- aplicar(plan_r, despues_de_a)$datos
    reparado <- aplicar(plan_r, datos)$datos
    derecho <- if (nrow(plan_a)) aplicar(plan_a, reparado)$datos else reparado
    # Las tres primeras filas son las construcciones cortas con un control
    # genuino junto a la letra rota; el tramo largo se comprueba abajo en P3, donde se exige
    # conservar el control genuino fuera de la mascara.
    expect_identical(
      izquierdo$texto[seq_len(3L)], derecho$texto[seq_len(3L)],
      info = paste("orden:", paste(orden, collapse = ","))
    )
  }

  # El espacio completo de ordenes pasa tambien por los casos que bloquean la
  # alineacion; la tabla, el lote y los casos dirigidos ya recorrieron todos los
  # subconjuntos disponibles en la planificacion anterior.
  extremos <- tail(casos, 5L)[seq_len(3L)]
  datos_extremos <- data.frame(texto = extremos, stringsAsFactors = FALSE)
  piezas_extremos <- .c1_moji_plan(datos_extremos)
  acciones_extremos <- intersect(
    names(.c1_moji_acciones_directas),
    as.character(piezas_extremos$plan$estrategia)
  )
  for (orden in .c1_moji_permutaciones(acciones_extremos)) {
    plan_a <- piezas_extremos$plan[
      piezas_extremos$plan$estrategia %in% orden, , drop = FALSE
    ]
    if (nrow(plan_a)) {
      plan_a$aplicar <- TRUE
      plan_a$orden <- match(as.character(plan_a$estrategia), orden)
    }
    plan_r <- piezas_extremos$plan[
      piezas_extremos$plan$estrategia == "reparar_codificacion", , drop = FALSE
    ]
    if (!nrow(plan_r)) next
    plan_r$aplicar <- TRUE
    datos_a <- if (nrow(plan_a)) aplicar(plan_a, datos_extremos)$datos else
      datos_extremos
    izquierda <- aplicar(plan_r, datos_a)$datos
    reparado_extremos <- aplicar(plan_r, datos_extremos)$datos
    derecha <- if (nrow(plan_a)) {
      aplicar(plan_a, reparado_extremos)$datos
    } else {
      reparado_extremos
    }
    expect_identical(
      izquierda$texto, derecha$texto,
      info = paste("orden:", paste(orden, collapse = ","))
    )
  }
})

test_that("P2 respeta A(R(x)) aunque el orden editado ponga A primero", {
  datos <- data.frame(
    texto = c(
      paste0("Mar", intToUtf8(0xAD), intToUtf8(0xC3L), intToUtf8(0xAD), "a"),
      paste0(" x\n100", intToUtf8(0x80L), intToUtf8(0xA0L), intToUtf8(1L)),
      " &amp;Y\n\u00c3\u009d"
    ), stringsAsFactors = FALSE
  )
  piezas <- .c1_moji_plan(datos)
  acciones <- names(.c1_moji_acciones_directas)
  plan <- piezas$plan[piezas$plan$estrategia %in%
    c(acciones, "reparar_codificacion"), , drop = FALSE]
  expect_true(all(acciones %in% plan$estrategia))
  expect_true("reparar_codificacion" %in% plan$estrategia)

  for (orden in .c1_moji_permutaciones(acciones)) {
    subplan <- plan
    subplan$aplicar <- TRUE
    subplan$orden[subplan$estrategia == "reparar_codificacion"] <- 1L
    subplan$orden[subplan$estrategia != "reparar_codificacion"] <-
      match(subplan$estrategia[subplan$estrategia != "reparar_codificacion"],
            orden) + 1L
    resultado <- aplicar(subplan, datos)
    esperado <- .c1_moji_aplicar_directo(
      .c1_moji_reparar_directo(datos$texto), orden
    )
    expect_identical(resultado$datos$texto, esperado,
                     info = paste("orden:", paste(orden, collapse = ",")))
  }
})

test_that("P3 conserva los controles genuinos fuera del mojibake", {
  casos <- c(
    paste0("Mar", intToUtf8(0xAD), intToUtf8(0xC3L), intToUtf8(0xAD), "a"),
    paste0(strrep(paste0(intToUtf8(0xC3L), intToUtf8(0xAD)), 200), intToUtf8(0x81)),
    paste0(strrep(paste0(intToUtf8(0xC3L), intToUtf8(0xAD)), 200), intToUtf8(0xAD))
  )
  datos <- data.frame(texto = casos, stringsAsFactors = FALSE)
  piezas <- .c1_moji_plan(datos)
  fila <- piezas$perfil$columnas[
    piezas$perfil$columnas$columna == "texto", , drop = FALSE
  ]
  expect_equal(fila$n_codificacion_rota, 3L)
  expect_equal(fila$n_controles_invisibles, 3L)
  expect_true("eliminar_controles_invisibles" %in% piezas$plan$estrategia)

  resultado <- aplicar(piezas$plan, datos)
  perfil_final <- perfilar(
    resultado$datos, muestra = Inf, analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE
  )
  fila_final <- perfil_final$columnas[
    perfil_final$columnas$columna == "texto", , drop = FALSE
  ]
  expect_equal(fila_final$n_codificacion_rota, 3L)
  expect_equal(fila_final$n_controles_invisibles, 0L)
  expect_true(grepl(
    paste0("Mar", intToUtf8(0xC3L), intToUtf8(0xADL), "a"),
    resultado$datos$texto[[1L]], fixed = TRUE
  ))
  expect_true(any(resultado$registro$estrategia ==
                   "eliminar_controles_invisibles"))
})

test_that("la reparacion precede a mayusculas, diccionario y ausencias", {
  mojibake <- .c1_moji_latin1("Mar\u00eda")
  d <- data.frame(a = c(mojibake, .c1_moji_latin1("mar\u00eda"), "MAR\u00cdA"),
                  stringsAsFactors = FALSE)
  piezas <- .c1_moji_plan(d)

  mayusculas <- piezas$plan[
    piezas$plan$estrategia %in% c("convertir_mayusculas", "reparar_codificacion"),
    , drop = FALSE
  ]
  mayusculas$aplicar <- TRUE
  mayusculas$orden[mayusculas$estrategia == "convertir_mayusculas"] <- 100L
  mayusculas$orden[mayusculas$estrategia == "reparar_codificacion"] <- 200L
  r_mayusculas <- aplicar(mayusculas, d)
  expect_identical(r_mayusculas$datos$a, rep("MAR\u00cdA", 3L))
  expect_identical(
    as.character(r_mayusculas$registro$estrategia),
    c("reparar_codificacion", "convertir_mayusculas")
  )
  expect_true(all(r_mayusculas$registro$estado == "ejecutada"))
  expect_equal(r_mayusculas$registro$n_cambiadas, c(2, 2))

  diccionario <- piezas$plan[
    piezas$plan$estrategia %in% c("convertir_segun_diccionario", "reparar_codificacion"),
    , drop = FALSE
  ]
  diccionario$aplicar <- TRUE
  diccionario$orden[diccionario$estrategia == "convertir_segun_diccionario"] <- 100L
  diccionario$orden[diccionario$estrategia == "reparar_codificacion"] <- 200L
  fila_diccionario <- which(diccionario$estrategia == "convertir_segun_diccionario")
  diccionario$parametros[[fila_diccionario]] <- list(
    diccionario = c("Mar\u00eda" = "MARIA")
  )
  diccionario$estado[fila_diccionario] <- "lista"
  r_diccionario <- aplicar(diccionario, d)
  expect_identical(r_diccionario$datos$a[[1L]], "MARIA")
  expect_identical(r_diccionario$datos$a[[2L]], "mar\u00eda")
  expect_identical(
    as.character(r_diccionario$registro$estrategia),
    c("reparar_codificacion", "convertir_segun_diccionario")
  )
  expect_true(all(r_diccionario$registro$estado == "ejecutada"))

  ausencias <- data.frame(a = c("N/A", mojibake), stringsAsFactors = FALSE)
  ausencia <- .c1_moji_plan(ausencias)$plan
  ausencia <- ausencia[
    ausencia$estrategia %in% c("convertir_ausencias_textuales", "reparar_codificacion"),
    , drop = FALSE
  ]
  ausencia$aplicar <- TRUE
  ausencia$orden[ausencia$estrategia == "convertir_ausencias_textuales"] <- 100L
  ausencia$orden[ausencia$estrategia == "reparar_codificacion"] <- 200L
  r_ausencia <- aplicar(ausencia, ausencias)
  expect_true(is.na(r_ausencia$datos$a[[1L]]))
  expect_identical(r_ausencia$datos$a[[2L]], "Mar\u00eda")
  expect_identical(
    as.character(r_ausencia$registro$estrategia),
    c("reparar_codificacion", "convertir_ausencias_textuales")
  )
  expect_true(all(r_ausencia$registro$estado == "ejecutada"))
})

test_that("las acciones de espacios no consumen puntos de la reparacion", {
  casos <- list(
    trim_aria = list(texto = paste0("\u00c3", " "), accion = "recortar_espacios"),
    tab_aria = list(texto = paste0("\u00c3", "\t"), accion = "reemplazar_separadores"),
    tab_circunflejo = list(texto = paste0("\u00c2", "\t"), accion = "reemplazar_separadores")
  )
  for (caso in casos) {
    antes <- caso$texto
    despues <- switch(caso$accion,
      recortar_espacios = .recortar_texto(antes)$valor,
      reemplazar_separadores = .reemplazar_separadores(antes)$valor
    )
    reparado <- .ftfy_reparar_uno(antes)$texto
    expect_identical(despues, antes)
    expect_identical(.ftfy_reparar_uno(despues)$texto, reparado)
  }
})

test_that("el detector vectorizado cubre toda reparacion que cambia el texto", {
  testthat::skip_if_not_installed("jsonlite")
  entradas <- .c1_moji_bateria()
  expect_length(entradas, 260L)
  reparados <- vapply(
    entradas, function(texto) lupa:::.ftfy_reparar_uno(texto)$texto,
    character(1L), USE.NAMES = FALSE
  )
  cambia <- !is.na(entradas) & reparados != entradas
  filtro <- lupa:::.ftfy_mascara_candidatos(entradas)
  expect_true(all(filtro[cambia]))
  analisis <- lupa:::.analizar_codificacion(entradas)
  expect_identical(analisis$candidatos, filtro)
  expect_true(all(analisis$afectados[cambia]))
})

test_that("la preseleccion rapida conserva toda celda reparable", {
  testthat::skip_if_not_installed("jsonlite")
  entradas <- c(.c1_moji_bateria(), "\u00c2 ", "x\u00c2 ")
  reparados <- vapply(
    entradas, function(texto) lupa:::.ftfy_reparar_uno(texto)$texto,
    character(1L), USE.NAMES = FALSE
  )
  cambia <- !is.na(entradas) & reparados != entradas
  preseleccion <- grepl(
    lupa:::.ftfy_patron_candidatos_rapido, entradas, perl = TRUE
  ) | grepl(intToUtf8(0xfffdL), entradas, fixed = TRUE)
  expect_true(all(preseleccion[cambia]))
  expect_true(all(lupa:::.ftfy_mascara_candidatos(entradas)[cambia]))
})

test_that("la mascara es una alineacion valida y maxima por definicion", {
  entradas <- c(.c1_moji_bateria(), .c1_moji_generados())
  empates <- c(
    paste0("\u00c3", intToUtf8(0x81L), intToUtf8(0x81L), "REA"),
    paste0(intToUtf8(0x81L), "\u00c3", intToUtf8(0x81L), "REA"),
    paste0("\u00c3\u00ad\u00ad", "REA"),
    paste0("\u00ad\u00c3\u00ad", "REA"),
    paste0("\u00c3\u00a0\u00a0", "REA"),
    paste0("\u00a0\u00c3\u00a0", "REA")
  )
  entradas <- c(entradas, empates)
  reparados <- vapply(
    entradas, function(texto) lupa:::.ftfy_reparar_uno(texto)$texto,
    character(1L), USE.NAMES = FALSE
  )
  cambia <- which(!is.na(entradas) & reparados != entradas)
  for (i in cambia) {
    entrada <- lupa:::.codigos_decodificables(entradas[[i]])
    salida <- lupa:::.codigos_decodificables(reparados[[i]])
    mascara <- lupa:::.ftfy_mascara_no_alineada(entrada, salida)
    referencia <- .c1_moji_lcs_referencia(entrada, salida)
    # Un tramo largo usa el detector, no una LCS que crece con la celda.
    if (max(length(entrada), length(salida)) > lupa:::.ftfy_umbral_mascara_lcs) {
      detector <- lupa:::.ftfy_mascara_mojibake_consumida(entrada) |
        lupa:::.ftfy_mascara_continuaciones_utf8(entrada)
      expect_true(
        all(!mascara | detector),
        info = "el retorno largo solo protege posiciones marcadas por el detector"
      )
      next
    }
    alineadas <- which(!mascara)
    expect_equal(length(alineadas), referencia$longitud)
    pares <- .c1_moji_subsecuencia(entrada[alineadas], salida)
    expect_true(!is.null(pares))
    expect_identical(entrada[alineadas], salida[pares])
  }
  expect_true(length(cambia) > 200L)
})

test_that("la regresion de la alineacion anterior falla en otro proceso", {
  # Bajo covr el proceso hijo carga la copia instrumentada del paquete, que
  # no es la que esta prueba imita: la comparacion no mide nada ahi.
  skip_on_covr()
  entrada <- c(0xC4L, 0xB1L, 0x7aL, 0xC4L, 0xB1L)
  salida <- c(0x131L, 0x7aL, 0x131L)
  referencia <- .c1_moji_lcs_referencia(entrada, salida)
  mascara_correcta <- rep(TRUE, length(entrada))
  mascara_correcta[referencia$entrada] <- FALSE
  guion <- tempfile(fileext = ".R")
  on.exit(unlink(guion), add = TRUE)
  funcion <- paste(deparse(.c1_moji_alineacion_v4), collapse = "\n")
  dput_entrada <- paste(capture.output(dput(entrada)), collapse = "\n")
  dput_salida <- paste(capture.output(dput(salida)), collapse = "\n")
  dput_esperada <- paste(capture.output(dput(mascara_correcta)), collapse = "\n")
  carga <- .c1_moji_carga_hijo()
  writeLines(c(
    carga,
    paste0("alineacion_anterior <- ", funcion),
    "assignInNamespace('.ftfy_mascara_no_alineada', alineacion_anterior, ns = 'lupa')",
    paste0("entrada <- ", dput_entrada),
    paste0("salida <- ", dput_salida),
    paste0("esperada <- ", dput_esperada),
    "obtenida <- lupa:::.ftfy_mascara_no_alineada(entrada, salida)",
    "cat(if (identical(obtenida, esperada)) 'NO_FALLA' else 'FALLA', '\\n')"
  ), guion)
  salida_hijo <- system2(
    file.path(R.home("bin"), "Rscript"), shQuote(guion),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(
    any(grepl("FALLA", salida_hijo, fixed = TRUE)),
    info = paste(salida_hijo, collapse = "\n")
  )
})

test_that("el tope protege un tramo largo sin anclas", {
  entrada <- rep(c(0xC4L, 0xB1L), lupa:::.ftfy_umbral_mascara_lcs + 1L)
  salida <- rep(0x131L, lupa:::.ftfy_umbral_mascara_lcs + 1L)
  expect_true(all(lupa:::.ftfy_mascara_no_alineada(entrada, salida)))
})

test_that("un plan nuevo no deja una accion de invisibles sin trabajo", {
  casos <- list(
    c("A\u00c3\u0081rea", "N\u00c3\u0091"),
    c(paste0("A\u00c3\u0081rea", intToUtf8(1L)), "N\u00c3\u0091"),
    c("Mar\u00c3\u00ad a", "Y\u00c3\u009d")
  )
  for (valores in casos) {
    datos <- data.frame(texto = valores, stringsAsFactors = FALSE)
    piezas <- .c1_moji_plan(datos)
    estrategias <- piezas$plan$estrategia
    if ("eliminar_controles_invisibles" %in% estrategias &&
        "reparar_codificacion" %in% estrategias) {
      resultado <- aplicar(piezas$plan, datos)
      expect_true(all(resultado$registro$estado == "ejecutada"))
    }
    expect_false(any(piezas$plan$estado == "omitida"))
  }
})

test_that("la salida publica conserva la reparacion de C1 y su evidencia", {
  textos <- c(
    paste0(intToUtf8(0x93L), "hola", intToUtf8(0x94L)),
    paste0("100", intToUtf8(0x80L)),
    paste0("a", intToUtf8(0x96L), "b"),
    paste0("x", intToUtf8(0x81L), "y"),
    "Mar\u00c3\u00ad a", "Y\u00c3\u009d"
  )
  esperados <- c("\u201chola\u201d", "100\u20ac", "a\u2013b", "x\u0081y",
                 "Mar\u00ed a", "Y\u00dd")
  for (i in seq_along(textos)) {
    datos <- data.frame(texto = textos[[i]], stringsAsFactors = FALSE)
    piezas <- .c1_moji_plan(datos)
    reparar <- piezas$plan
    reparar$aplicar <- reparar$estrategia == "reparar_codificacion"
    resultado <- aplicar(reparar, datos)
    expect_identical(resultado$datos$texto[[1L]], esperados[[i]])
  }
})

test_that("el detector y cada accion de invisibles comparten la mascara", {
  casos <- c(
    .c1_moji_bateria(), .c1_moji_generados(),
    paste0("Mar", intToUtf8(0xADL), intToUtf8(0xC3L), intToUtf8(0xADL), "a"),
    paste0("Mar", intToUtf8(0x81L), intToUtf8(0xC3L), intToUtf8(0xADL), "a"),
    paste0("Mar", intToUtf8(0xA0L), intToUtf8(0xC3L), intToUtf8(0xADL), "a"),
    paste0("\u0153 x", intToUtf8(1L))
  )
  acciones <- list(
    eliminar_controles_invisibles = list(
      campo = "n_invisibles_eliminables",
      aplicar = function(x, p) .quitar_controles_invisibles(x, p)$valor
    ),
    normalizar_espacios_invisibles = list(
      campo = "n_espacios_invisibles",
      aplicar = function(x, p) .normalizar_espacios_invisibles(x, p)$valor
    ),
    reemplazar_separadores = list(
      campo = "n_separadores_en_campo",
      aplicar = function(x, p) .reemplazar_separadores(x, p)$valor
    )
  )
  for (texto in casos) {
    diagnostico <- .diagnosticar_texto(texto)
    proteccion <- .proteccion_texto_valores(texto)
    for (nombre in names(acciones)) {
      accion <- acciones[[nombre]]
      cuenta <- isTRUE(diagnostico[[accion$campo]] > 0L)
      despues <- accion$aplicar(texto, proteccion)
      cambia <- isTRUE(.celdas_cambiadas(texto, despues)[[1L]])
      expect_identical(
        cuenta, cambia,
        info = paste(nombre, paste(sprintf("%04X", utf8ToInt(texto)), collapse = " "))
      )
    }
  }

  datos <- data.frame(texto = casos, stringsAsFactors = FALSE)
  piezas <- .c1_moji_plan(datos)
  resultado <- aplicar(piezas$plan, datos)
  invisibles <- c(
    "eliminar_controles_invisibles", "normalizar_espacios_invisibles",
    "reemplazar_separadores"
  )
  expect_false(any(
    resultado$registro$estrategia %in% invisibles &
      resultado$registro$estado == "fallida"
  ))
})

test_that("el respaldo estructural protege continuaciones y deja controles ajenos", {
  entrada <- c(0xC3L, 0xADL, 0xC3L, 0x80L, 0x81L, 0xE2L, 0x80L, 0xADL, 0x81L)
  mascara <- .ftfy_mascara_continuaciones_utf8(entrada)
  expect_identical(
    mascara,
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  salida <- .ftfy_mascara_respaldo(entrada)
  expect_true(all(mascara <= salida))
  expect_false(salida[[5L]])
  expect_false(salida[[9L]])
})

test_that("las tablas generadas conservan la cuenta de cada accion de texto", {
  # Bajo covr el proceso hijo carga la copia instrumentada del paquete, que
  # no es la que esta prueba imita: la comparacion no mide nada ahi.
  skip_on_covr()
  tablas <- .c1_moji_tablas_generadas()
  campos <- c(
    n_espacios_borde = "recortar_espacios",
    n_invisibles_eliminables = "eliminar_controles_invisibles",
    n_espacios_invisibles = "normalizar_espacios_invisibles",
    n_separadores_en_campo = "reemplazar_separadores",
    n_entidades_html = "decodificar_entidades_html",
    n_codificacion_reparable = "reparar_codificacion"
  )
  for (datos in tablas) {
    expect_no_error(piezas <- .c1_moji_plan(datos))
    expect_no_error(resultado <- aplicar(piezas$plan, datos))
    acciones_texto <- c(
      "recortar_espacios", "eliminar_controles_invisibles",
      "normalizar_espacios_invisibles", "reemplazar_separadores",
      "decodificar_entidades_html", "reparar_codificacion"
    )
    expect_false(any(
      resultado$registro$estrategia %in% acciones_texto &
        resultado$registro$estado == "fallida"
    ))
    for (columna in names(datos)) {
      texto <- as.character(datos[[columna]])
      fila <- piezas$perfil$columnas[
        piezas$perfil$columnas$columna == columna, , drop = FALSE
      ]
      expect_equal(nrow(fila), 1L, info = columna)
      cambios <- .c1_moji_cambios_protegidos(texto)
      for (campo in names(campos)) {
        esperado <- sum(.celdas_cambiadas(texto, cambios[[campo]]))
        expect_equal(
          as.integer(fila[[campo]][[1L]]), as.integer(esperado),
          info = paste(columna, campo, sep = ":")
        )
        estrategia <- unname(campos[[campo]])
        indice_plan <- which(
          piezas$plan$columna == columna &
            piezas$plan$estrategia == estrategia
        )
        if (length(indice_plan) == 1L) {
          esperado_plan <- if (identical(campo, "n_codificacion_reparable")) {
            esperado + as.integer(
              fila$n_codificacion_reparable_parcialmente[[1L]]
            )
          } else {
            esperado
          }
          expect_equal(
            as.integer(piezas$plan$n_afectadas[[indice_plan]]),
            as.integer(esperado_plan),
            info = paste(columna, estrategia, "plan", sep = ":")
          )
        }
      }
    }
  }

  # El defecto de la novena parte se conserva como una prueba negativa en otro
  # proceso: sin recalcular la mascara despues de quitar un control, la primera
  # tabla deja la normalizacion sin efecto y la accion termina fallida.
  rds <- tempfile(fileext = ".rds")
  guion <- tempfile(fileext = ".R")
  on.exit(unlink(c(rds, guion)), add = TRUE)
  saveRDS(tablas, rds)
  writeLines(c(
    .c1_moji_carga_hijo(),
    sprintf("tablas <- readRDS(%s)", deparse(rds)),
    "acciones_texto <- c('recortar_espacios', 'eliminar_controles_invisibles', 'normalizar_espacios_invisibles', 'reemplazar_separadores', 'decodificar_entidades_html', 'reparar_codificacion')",
    "assignInNamespace('.actualizar_proteccion_texto', function(protecciones, datos, columna) protecciones, ns = 'lupa')",
    paste0(
      "falla <- vapply(tablas, function(datos) {",
      "p <- perfilar(datos, muestra = Inf, analizar_dependencias = FALSE, proteger_datos_personales = FALSE); ",
      "z <- aplicar(planificar_limpieza(p, datos), datos); ",
      "any(z$registro$estrategia %in% acciones_texto & z$registro$estado == 'fallida')",
      "}, logical(1L))"
    ),
    "cat(if (any(falla)) 'FALLA_V9\\n' else 'NO_FALLA_V9\\n')"
  ), guion)
  salida <- system2(
    file.path(R.home("bin"), "Rscript"), shQuote(guion),
    stdout = TRUE, stderr = TRUE
  )
  expect_true(
    any(grepl("FALLA_V9", salida, fixed = TRUE)),
    info = paste(salida, collapse = "\n")
  )
})

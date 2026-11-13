testthat::skip_on_cran()

test_that("la mascara de respaldo cubre representaciones de bytes de continuacion", {
  casos <- list(
    comilla = intToUtf8(c(0xE2L, 0x20ACL, 0x9DL)),
    guion = intToUtf8(c(0xE2L, 0x20ACL, 0x90L)),
    emoji = intToUtf8(c(0xF0L, 0x178L, 0x2DCL, 0x20ACL))
  )
  for (texto in casos) {
    datos <- data.frame(
      texto = paste0(strrep("a", 260L), texto),
      stringsAsFactors = FALSE
    )
    perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
    fila <- perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
    testthat::expect_equal(fila$n_codificacion_rota[[1L]], 1L)
    plan <- planificar_limpieza(perfil, datos)
    plan$aplicar[as.character(plan$estrategia) == "reparar_codificacion"] <- FALSE
    salida <- aplicar(plan, datos)
    testthat::expect_identical(salida$datos$texto, datos$texto)
    final <- perfilar(salida$datos, fecha = "2026-01-15", muestra = Inf)
    testthat::expect_equal(
      final$columnas$n_codificacion_rota[final$columnas$columna == "texto"],
      1L
    )
  }
})

test_that("codificacion afectada cuenta filas repetidas y conserva el hallazgo", {
  irreparable <- paste0(intToUtf8(0x152L), " ")
  reparable <- paste0("x ", intToUtf8(c(0xC3L, 0xA9L)))
  base <- data.frame(
    texto = c(irreparable, reparable, "limpio"),
    stringsAsFactors = FALSE
  )
  repetida <- base[rep(seq_len(nrow(base)), 4L), , drop = FALSE]
  perfiles <- lapply(list(base, repetida), function(datos) {
    perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  })
  filas <- lapply(perfiles, function(perfil) {
    perfil$columnas[perfil$columnas$columna == "texto", , drop = FALSE]
  })
  testthat::expect_equal(filas[[2L]]$n_codificacion_rota[[1L]],
                         4L * filas[[1L]]$n_codificacion_rota[[1L]])
  testthat::expect_equal(filas[[2L]]$n_codificacion_reparable[[1L]],
                         4L * filas[[1L]]$n_codificacion_reparable[[1L]])
  testthat::expect_equal(filas[[2L]]$n_codificacion_no_se_pudo[[1L]],
                         4L * filas[[1L]]$n_codificacion_no_se_pudo[[1L]])
  testthat::expect_identical(
    filas[[1L]]$estado_codificacion_reparacion[[1L]],
    filas[[2L]]$estado_codificacion_reparacion[[1L]]
  )
  hallazgos <- lapply(perfiles, function(perfil) {
    perfil$hallazgos[perfil$hallazgos$tipo_hallazgo == "codificacion_rota", ,
                     drop = FALSE]
  })
  testthat::expect_identical(
    as.character(hallazgos[[1L]]$tipo_hallazgo),
    as.character(hallazgos[[2L]]$tipo_hallazgo)
  )
  testthat::expect_equal(
    hallazgos[[2L]]$n_afectados,
    4L * hallazgos[[1L]]$n_afectados
  )
})

test_that("las acciones de caja protegen una celda reparable aunque no se repare", {
  roto <- paste0("M", intToUtf8(c(0xC3L, 0x153L)), "NCHEN")
  datos <- data.frame(t = c(roto, "foo", "FOO"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  fila <- perfil$columnas[perfil$columnas$columna == "t", , drop = FALSE]
  testthat::expect_equal(fila$n_codificacion_reparable[[1L]], 1L)
  plan <- planificar_limpieza(perfil, datos)
  for (estrategia in c(
    "convertir_minusculas", "convertir_mayusculas", "convertir_titulo"
  )) {
    elegido <- plan
    elegido$aplicar <- as.character(elegido$estrategia) == estrategia
    salida <- aplicar(elegido, datos)
    reparado <- lupa:::.ftfy_reparar_uno(salida$datos$t[[1L]])
    esperado <- lupa:::.ftfy_reparar_uno(roto)$texto
    testthat::expect_identical(salida$datos$t[[1L]], roto)
    testthat::expect_identical(reparado$texto, esperado)
    final <- perfilar(salida$datos, fecha = "2026-01-15", muestra = Inf)
    testthat::expect_equal(
      final$columnas$n_codificacion_rota[final$columnas$columna == "t"],
      1L
    )
  }
})

test_that("una entidad junto a mojibake conserva ambos hallazgos y la celda", {
  txt <- paste0("&#233; c", intToUtf8(0x00C5L), intToUtf8(0x201CL), "ur")
  datos <- data.frame(x = txt, stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  testthat::expect_true(all(c("entidades_html", "codificacion_rota") %in% tipos))
  plan <- planificar_limpieza(perfil, datos)
  testthat::expect_true(any(plan$estrategia == "decodificar_entidades_html"))
  plan$aplicar[plan$estrategia == "reparar_codificacion"] <- FALSE
  plan$aplicar[plan$estrategia == "decodificar_entidades_html"] <- TRUE
  resultado <- aplicar(plan, datos)
  testthat::expect_identical(resultado$datos$x[[1L]], txt)
  registro <- resultado$registro[
    resultado$registro$estrategia == "decodificar_entidades_html", , drop = FALSE
  ]
  testthat::expect_equal(registro$n_cambiadas[[1L]], 0)
})

test_that("la evidencia de codificacion usa ejemplos distintos y un motivo cierto", {
  c1 <- intToUtf8(0x81L)
  # Los ejemplos van separados por "; " y el motivo tambien lleva un punto y
  # coma: no se cuentan partiendo el texto sino buscando cada valor citado, que
  # sale entre comillas y con el control escrito como escape.
  cita <- function(antes, despues) paste0('"', antes, "\\u0081", despues, '"')
  veces <- function(evidencia, patron) {
    hallados <- gregexpr(patron, evidencia, fixed = TRUE, useBytes = TRUE)[[1L]]
    if (identical(hallados[[1L]], -1L)) 0L else length(hallados)
  }
  motivo <- "(mal convertido; el motor no pudo repararlo)"

  repetido <- paste0("x", c1, "y")
  valores <- c(rep(repetido, 75L), paste0("z", c1, "q"), "limpio")
  perfil <- perfilar(
    data.frame(ctrl = valores, stringsAsFactors = FALSE),
    fecha = "2026-01-15", muestra = Inf
  )
  hallazgo <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "codificacion_rota", , drop = FALSE
  ]
  testthat::expect_equal(hallazgo$n_afectados[[1L]], 76L)
  evidencia <- as.character(hallazgo$evidencia[[1L]])
  testthat::expect_false(grepl("reemplazo", evidencia,
                               fixed = TRUE, useBytes = TRUE))
  # El valor que se repite 75 veces se cita una vez, y el otro tambien.
  testthat::expect_equal(veces(evidencia, cita("x", "y")), 1L)
  testthat::expect_equal(veces(evidencia, cita("z", "q")), 1L)
  testthat::expect_equal(veces(evidencia, motivo), 2L)

  letras <- c("a", "b", "c", "d", "e", "f")
  distintos <- paste0(letras, c1, "q")
  perfil_distinto <- perfilar(
    data.frame(ctrl = distintos, stringsAsFactors = FALSE),
    fecha = "2026-01-15", muestra = Inf
  )
  hallazgo_distinto <- perfil_distinto$hallazgos[
    perfil_distinto$hallazgos$tipo_hallazgo == "codificacion_rota", , drop = FALSE
  ]
  evidencia_distinta <- as.character(hallazgo_distinto$evidencia[[1L]])
  testthat::expect_false(grepl("reemplazo", evidencia_distinta,
                               fixed = TRUE, useBytes = TRUE))
  citados <- vapply(letras, function(letra) {
    veces(evidencia_distinta, cita(letra, "q"))
  }, integer(1L))
  # Hasta cinco ejemplos, ninguno repetido, cada uno con el mismo motivo.
  testthat::expect_true(all(citados <= 1L))
  testthat::expect_equal(sum(citados), 5L)
  testthat::expect_equal(veces(evidencia_distinta, motivo), 5L)
})

test_that("la reparacion parcial queda lista y se puede seleccionar", {
  parcial <- paste0(
    "a", intToUtf8(0xA0L), intToUtf8(c(0xC3L, 0xA9L)),
    "l", intToUtf8(c(0xC3L, 0xA8L)), "ve"
  )
  datos <- data.frame(txt = parcial, stringsAsFactors = FALSE)
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  fila <- plan[plan$hallazgo == "codificacion_rota", , drop = FALSE]
  testthat::expect_identical(fila$estrategia[[1L]], "reparar_codificacion")
  testthat::expect_identical(as.character(fila$estado[[1L]]), "lista")
  testthat::expect_false(fila$aplicar[[1L]])
  testthat::expect_true(isTRUE(fila$parametros[[1L]]$aplicar_parciales))
  plan$aplicar[plan$hallazgo == "codificacion_rota"] <- TRUE
  resultado <- aplicar(plan, datos)
  testthat::expect_identical(resultado$registro$estado[[1L]], "ejecutada")
  testthat::expect_equal(resultado$registro$n_cambiadas[[1L]], 1)

  sin_reparacion <- data.frame(
    txt = paste0("x", intToUtf8(0x81L), "y"), stringsAsFactors = FALSE
  )
  perfil_sin <- perfilar(sin_reparacion, fecha = "2026-01-15", muestra = Inf)
  plan_sin <- planificar_limpieza(perfil_sin, sin_reparacion)
  fila_sin <- plan_sin[plan_sin$hallazgo == "codificacion_rota", , drop = FALSE]
  testthat::expect_identical(
    fila_sin$estrategia[[1L]], "recuperar_codificacion_en_origen"
  )
  testthat::expect_true(grepl("motor reconoce", fila_sin$justificacion[[1L]],
                              fixed = TRUE, useBytes = TRUE))
  testthat::expect_false(grepl("reemplazo", fila_sin$justificacion[[1L]],
                               fixed = TRUE, useBytes = TRUE))

  perdido <- data.frame(
    txt = paste0("x", intToUtf8(0xFFFdL), "y"), stringsAsFactors = FALSE
  )
  perfil_perdido <- perfilar(perdido, fecha = "2026-01-15", muestra = Inf)
  plan_perdido <- planificar_limpieza(perfil_perdido, perdido)
  fila_perdido <- plan_perdido[
    plan_perdido$hallazgo == "codificacion_rota", , drop = FALSE
  ]
  testthat::expect_true(grepl("reemplazo", fila_perdido$justificacion[[1L]],
                              fixed = TRUE, useBytes = TRUE))
  testthat::expect_true(grepl("bytes", fila_perdido$justificacion[[1L]],
                              fixed = TRUE, useBytes = TRUE))
})


# El plan promete, para cada accion de texto, las filas que va a cambiar. Junto
# a una letra rota la accion puede no tener permitido tocar la celda: quitar un
# invisible, o el espacio del borde, cambia lo que la reparacion reconoce. Esas
# celdas no se cuentan, asi que ninguna accion marcada por omision queda sin
# efecto.
.n2582_plan_sin_filas <- function(plan) {
  plan[!plan$estrategia %in% c(
    "marcar_filas_duplicadas", "conservar_primera_duplicada",
    "conservar_mas_completa"
  ), , drop = FALSE]
}

.n2582_acciones_contadas <- c(
  "recortar_espacios", "eliminar_controles_invisibles",
  "normalizar_espacios_invisibles", "reemplazar_separadores"
)

test_that("el plan cuenta lo que cada accion de texto puede cambiar junto a una letra rota", {
  u <- function(...) intToUtf8(c(...))
  celdas <- list(
    # El guion blando mal convertido: su segundo punto es un guion blando, y
    # quitarlo deja la primera mitad sin arreglo. Solo se recorta el borde.
    guion_blando_roto = list(
      entra = paste0(" ", u(0xC2L, 0xADL, 0xD8L), "l|"),
      sale = paste0(u(0xC2L, 0xADL, 0xD8L), "l|")
    ),
    # Un guion blando genuino delante de una comilla rota: quitarlo vuelve
    # reparable la comilla, y la accion cruda le quitaba ademas su ultimo punto.
    control_y_comilla = list(
      entra = paste0(u(0xADL, 0xE2L, 0x20ACL, 0x9DL), "&amp; ", u(0xC3L, 0xBCL)),
      sale = paste0(u(0xADL, 0xE2L, 0x20ACL, 0x9DL), "&amp; ", u(0xC3L, 0xBCL))
    ),
    # Un espacio duro delante de dos letras rotas: normalizarlo cambia cuantas
    # se reparan.
    espacio_duro = list(
      entra = paste0("a", u(0xA0L, 0xC5L, 0xBEL), "i", u(0xC5L, 0xBEL), "ek"),
      sale = paste0("a", u(0xA0L, 0xC5L, 0xBEL), "i", u(0xC5L, 0xBEL), "ek")
    ),
    # El espacio del borde es lo que deja reconocer la letra rota: recortado,
    # la celda deja de estar senalada.
    borde_final = list(
      entra = paste0("dN|", u(0xC3L, 0x152L), " "),
      sale = paste0("dN|", u(0xC3L, 0x152L), " ")
    ),
    borde_inicial = list(
      entra = paste0(" ", u(0xC2L, 0xA5L), "\nXT"),
      sale = paste0(" ", u(0xC2L, 0xA5L), "\nXT")
    ),
    # Un espacio duro delante de unas comillas rotas, la de cierre con un C1.
    comillas_y_espacio_duro = list(
      entra = paste0("a", u(0xA0L, 0xE2L, 0x20ACL, 0x153L), "hola",
                     u(0xE2L, 0x20ACL, 0x9DL)),
      sale = paste0("a", u(0xA0L, 0xE2L, 0x20ACL, 0x153L), "hola",
                    u(0xE2L, 0x20ACL, 0x9DL))
    )
  )
  for (nombre in names(celdas)) {
    entra <- celdas[[nombre]]$entra
    datos <- data.frame(
      t = c(rep(entra, 3L), "abc", "otra cosa"), stringsAsFactors = FALSE
    )
    perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
    testthat::expect_equal(
      perfil$columnas$n_codificacion_rota[perfil$columnas$columna == "t"], 3L,
      info = nombre
    )
    plan <- .n2582_plan_sin_filas(planificar_limpieza(perfil, datos))
    # Cada accion sola cambia exactamente las filas que el plan cuenta.
    for (k in which(plan$estrategia %in% .n2582_acciones_contadas &
                      plan$estado == "lista")) {
      sola <- plan
      sola$aplicar <- seq_len(nrow(plan)) == k
      registro <- aplicar(sola, datos)$registro
      info <- paste(nombre, plan$estrategia[[k]])
      testthat::expect_identical(registro$estado[[1L]], "ejecutada", info = info)
      testthat::expect_equal(
        registro$n_cambiadas[[1L]], plan$n_afectadas[[k]], info = info
      )
    }
    # El plan por omision, tal como sale y con la reparacion desmarcada, no
    # tiene acciones sin efecto.
    sin_reparar <- plan
    sin_reparar$aplicar[sin_reparar$estrategia == "reparar_codificacion"] <- FALSE
    for (elegido in list(plan, sin_reparar)) {
      if (!any(elegido$aplicar)) next
      testthat::expect_false(
        "fallida" %in% aplicar(elegido, datos)$registro$estado, info = nombre
      )
    }
    # Sin la reparacion, la letra rota queda como estaba y sigue senalada.
    salida <- if (any(sin_reparar$aplicar)) {
      aplicar(sin_reparar, datos)$datos
    } else datos
    testthat::expect_identical(
      salida$t, c(rep(celdas[[nombre]]$sale, 3L), "abc", "otra cosa"),
      info = nombre
    )
    final <- perfilar(salida, fecha = "2026-01-15", muestra = Inf)
    testthat::expect_equal(
      final$columnas$n_codificacion_rota[final$columnas$columna == "t"], 3L,
      info = nombre
    )
  }
})

test_that("el conteo del plan y la accion coinciden sobre un banco de letras rotas", {
  u <- function(...) intToUtf8(c(...))
  # Letras de 2 y 3 bytes leidas como Latin-1 y como Windows-1252, entre los
  # caracteres que cada accion de texto busca.
  rotas <- list(
    e_aguda = u(0xC3L, 0xA9L), i_aguda = u(0xC3L, 0xADL),
    a_grave = u(0xC3L, 0xA0L), eszett = u(0xC3L, 0x178L),
    u_dieresis_mayuscula = u(0xC3L, 0x153L), euro = u(0xE2L, 0x201AL, 0xACL),
    comilla = u(0xE2L, 0x20ACL, 0x9DL), comilla_c1 = u(0xE2L, 0x80L, 0x9DL)
  )
  contextos <- c(
    " ", u(0xA0L), u(0xADL), "\t", u(0x200BL), u(0x81L), "&amp;",
    u(0x152L), u(0xB0L), "x"
  )
  for (nombre in names(rotas)) {
    rota <- rotas[[nombre]]
    valores <- c(
      paste0(contextos, rota, "k"), paste0("a", rota, contextos),
      paste0(contextos, rota, contextos), "abc", "otra cosa"
    )
    datos <- data.frame(t = valores, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
    plan <- .n2582_plan_sin_filas(planificar_limpieza(perfil, datos))
    plan$aplicar[plan$estrategia == "reparar_codificacion"] <- FALSE
    for (k in which(plan$estrategia %in% .n2582_acciones_contadas &
                      plan$estado == "lista")) {
      sola <- plan
      sola$aplicar <- seq_len(nrow(plan)) == k
      resultado <- aplicar(sola, datos)
      info <- paste(nombre, plan$estrategia[[k]])
      testthat::expect_equal(
        resultado$registro$n_cambiadas[[1L]], plan$n_afectadas[[k]], info = info
      )
      # La accion no toca la letra rota de ninguna celda.
      testthat::expect_true(all(grepl(
        enc2utf8(rota), enc2utf8(resultado$datos$t[seq_len(30L)]),
        fixed = TRUE, useBytes = TRUE
      )), info = info)
    }
    if (any(plan$aplicar)) {
      resultado <- aplicar(plan, datos)
      testthat::expect_false(
        "fallida" %in% resultado$registro$estado, info = nombre
      )
      testthat::expect_true(all(grepl(
        enc2utf8(rota), enc2utf8(resultado$datos$t[seq_len(30L)]),
        fixed = TRUE, useBytes = TRUE
      )), info = nombre)
    }
  }
})

test_that("aplicar no calcula la proteccion de una columna que ninguna accion de texto usa", {
  # Calcular al empezar la mascara de toda columna con una accion marcada hacia
  # que un plan que solo repara la codificacion tardara casi cuatro veces mas
  # sobre 200.000 filas. Se cuenta el calculo, no el tiempo.
  llamadas <- 0L
  original <- lupa:::.proteccion_texto_valores
  testthat::local_mocked_bindings(
    .proteccion_texto_valores = function(...) {
      llamadas <<- llamadas + 1L
      original(...)
    },
    .package = "lupa"
  )
  roto <- paste0("valor-", intToUtf8(c(0xC3L, 0xA9L)))
  datos <- data.frame(
    a = rep(roto, 40L), b = rep("limpio", 40L), stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, fecha = "2026-01-15", muestra = Inf)
  plan <- planificar_limpieza(perfil, datos)
  marcadas <- as.character(plan$estrategia[plan$aplicar])
  testthat::expect_true("reparar_codificacion" %in% marcadas)
  testthat::expect_false(any(marcadas %in% .n2582_acciones_contadas))
  llamadas <- 0L
  resultado <- aplicar(plan, datos)
  testthat::expect_identical(
    resultado$datos$a, rep(paste0("valor-", intToUtf8(0xE9L)), 40L)
  )
  testthat::expect_equal(llamadas, 0L)

  # El contador ve el calculo cuando una accion de texto si lo usa: una vez por
  # accion que cambia la columna, no una por accion marcada mas otra al final.
  con_borde <- data.frame(
    a = c(rep(paste0(" ", roto), 20L), rep("limpio", 20L)),
    stringsAsFactors = FALSE
  )
  perfil_borde <- perfilar(con_borde, fecha = "2026-01-15", muestra = Inf)
  plan_borde <- planificar_limpieza(perfil_borde, con_borde)
  solo_recorte <- plan_borde
  solo_recorte$aplicar <- solo_recorte$estrategia == "recortar_espacios"
  testthat::expect_equal(sum(solo_recorte$aplicar), 1L)
  llamadas <- 0L
  recortado <- aplicar(solo_recorte, con_borde)
  testthat::expect_identical(recortado$datos$a[[1L]], roto)
  testthat::expect_equal(llamadas, 1L)
})

# Una celda limpia que termina en `A` con circunflejo seguida de un espacio
# tipografico no se normaliza: con el espacio comun pasaria a leerse como una
# letra rota. Esa proteccion se perdia cuando la columna traia ademas una celda
# rota, porque la respuesta de la comprobacion se guardaba en la posicion de
# otra celda: la accion cambiaba una fila mas de las que el plan contaba y el
# perfil siguiente encontraba una celda rota que antes no habia.
test_that("normalizar los espacios invisibles no vuelve rota una celda limpia que convive con una rota", {
  skip_on_cran()
  u <- function(...) intToUtf8(c(...))
  datos <- data.frame(
    x = c(u(0xC3, 0xA9, 0x2000), u(0x78, 0x2000), u(0xC2, 0x2000),
          u(0x79, 0x2001)),
    stringsAsFactors = FALSE
  )
  perfil <- perfilar(datos, proteger_datos_personales = FALSE)
  plan <- planificar_limpieza(perfil, datos)
  es_la_accion <- plan$estrategia == "normalizar_espacios_invisibles"
  expect_identical(sum(es_la_accion), 1L)
  # La accion sola, con la reparacion desmarcada.
  plan$aplicar <- es_la_accion
  resultado <- suppressMessages(aplicar(plan, datos))
  expect_identical(resultado$datos$x[[3L]], datos$x[[3L]])
  registro <- resultado$registro[
    resultado$registro$estrategia == "normalizar_espacios_invisibles", ,
    drop = FALSE
  ]
  expect_identical(as.character(registro$estado), "ejecutada")
  expect_equal(registro$n_cambiadas, plan$n_afectadas[es_la_accion])
  despues <- perfilar(resultado$datos, proteger_datos_personales = FALSE)
  expect_equal(despues$columnas$n_codificacion_rota,
               perfil$columnas$n_codificacion_rota)
})

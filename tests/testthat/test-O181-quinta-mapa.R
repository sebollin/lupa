# Q5: el mapa de una dependencia tapa por construccion el lado protegido.

.q5_clase <- function(fechas, clase) {
  switch(
    clase,
    entero = as.integer(format(fechas, "%Y%m%d")),
    texto = format(fechas, "%Y-%m-%d"),
    Date = fechas,
    POSIXct = as.POSIXct(fechas, tz = "UTC") + 12 * 3600
  )
}

.q5_matriz <- function(lado) {
  set.seed(5181L)
  clases <- c("entero", "texto", "Date", "POSIXct")
  n_grupos <- 4L
  repeticiones <- 4L
  n <- n_grupos * repeticiones
  datos <- data.frame(row.names = seq_len(n))
  protegidas <- character()
  objetivos <- character()
  for (j in seq_along(clases)) {
    clase <- clases[[j]]
    if (lado == "determinante") {
      grupo <- sample(rep(seq_len(n_grupos), each = repeticiones))
      fechas <- as.Date("1980-01-01") + j * 2000L + grupo * 180L
      determinante <- paste0("FechaDet_", clase)
      objetivo <- paste0("Edad_", clase)
      datos[[determinante]] <- .q5_clase(fechas, clase)
      datos[[objetivo]] <- as.integer(20L + j * 10L + grupo)
      protegidas <- c(protegidas, determinante)
    } else {
      grupo <- sample(rep(seq_len(n_grupos), each = repeticiones))
      fechas <- as.Date("2010-01-01") + j * 2000L + grupo * 180L
      objetivo <- paste0("FechaDep_", clase)
      datos[[paste0("Hogar_", clase)]] <- paste0("H", j, "_", grupo)
      datos[[objetivo]] <- .q5_clase(fechas, clase)
      protegidas <- c(protegidas, objetivo)
    }
    for (g in seq_len(n_grupos)) {
      datos[[objetivo]][which(grupo == g)[1L]] <- NA
    }
    objetivos <- c(objetivos, objetivo)
  }
  list(datos = datos, protegidas = protegidas, objetivos = objetivos,
       clases = clases)
}

.q5_accion <- function(plan, columna) {
  which(startsWith(as.character(plan$estrategia),
                   "imputar_dependencia_funcional__") &
          as.character(plan$columna) == columna)
}

.q5_hojas <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character())
  if (inherits(x, "POSIXt")) {
    return(c(as.character(x), format(x, "%Y-%m-%d %H:%M:%S"),
             format(as.numeric(x), scientific = FALSE, trim = TRUE)))
  }
  if (inherits(x, "Date")) {
    return(c(as.character(x), format(x, "%Y-%m-%d"),
             format(as.numeric(x), scientific = FALSE, trim = TRUE)))
  }
  if (is.data.frame(x) || is.list(x)) {
    return(unlist(lapply(x, .q5_hojas), use.names = FALSE))
  }
  if (is.factor(x)) return(c(as.character(x), levels(x)))
  if (is.numeric(x)) {
    return(c(as.character(x), format(x, scientific = FALSE, trim = TRUE)))
  }
  if (is.character(x)) return(x)
  character()
}

.q5_activar <- function(plan, indices) {
  plan$aplicar[] <- FALSE
  plan$aplicar[indices] <- TRUE
  plan
}

.q5_dependencias <- function(caso, lado) {
  determinantes <- if (lado == "determinante") {
    paste0("FechaDet_", caso$clases)
  } else {
    paste0("Hogar_", caso$clases)
  }
  dependientes <- caso$objetivos
  data.frame(
    determinante = determinantes, dependiente = dependientes,
    cumplimiento = rep(1, length(determinantes)),
    n_evaluados = rep(nrow(caso$datos), length(determinantes)),
    n_grupos = rep(4L, length(determinantes)), n_violaciones = rep(0L, length(determinantes)),
    exacta = rep(TRUE, length(determinantes)), evidencia = rep("", length(determinantes)),
    stringsAsFactors = FALSE
  )
}

test_that("Q5: el mapa tapa ambos lados en las cuatro clases", {
  for (lado in c("determinante", "dependiente")) {
    caso <- .q5_matriz(lado)
    datos <- caso$datos
    perfil <- suppressWarnings(perfilar(
      datos, columnas_personales = caso$protegidas, muestra = nrow(datos),
      max_patrones = 3L, normalizar = FALSE,
      casi_duplicados_vocabulario = FALSE, max_filas_hallazgo = 20L,
      analizar_dependencias = FALSE
    ))
    perfil$dependencias <- .q5_dependencias(caso, lado)
    plan <- suppressWarnings(planificar_limpieza(perfil, datos))
    perfil_claro <- perfil
    perfil_claro$meta$proteger_datos_personales <- FALSE
    plan_claro <- suppressWarnings(planificar_limpieza(perfil_claro, datos))
    indices <- integer()
    indices_claros <- integer()
    valores <- unlist(lapply(caso$protegidas, function(columna) {
      x <- datos[[columna]][!is.na(datos[[columna]])]
      .q5_hojas(x)
    }), use.names = FALSE)
    for (j in seq_along(caso$clases)) {
      clase <- caso$clases[[j]]
      columna <- caso$objetivos[[j]]
      i <- .q5_accion(plan, columna)
      i_claro <- .q5_accion(plan_claro, columna)
      expect_true(length(i) == 1L, info = columna)
      expect_true(length(i_claro) == 1L, info = columna)
      indices <- c(indices, i)
      indices_claros <- c(indices_claros, i_claro)
      mapa <- plan$parametros[[i]]$mapa
      mapa_claro <- plan_claro$parametros[[i_claro]]$mapa
      protegido <- if (lado == "determinante") {
        mapa$determinante
      } else {
        mapa$dependiente
      }
      if (is.character(protegido)) {
        expect_true(all(protegido == "[valor protegido]"), info = clase)
      } else {
        expect_true(all(is.na(protegido)), info = clase)
      }
      publico <- if (lado == "determinante") {
        mapa$dependiente
      } else {
        mapa$determinante
      }
      publico_claro <- if (lado == "determinante") {
        mapa_claro$dependiente
      } else {
        mapa_claro$determinante
      }
      expect_identical(publico, publico_claro, info = columna)
      expect_true(isTRUE(plan$parametros[[i]]$mapa_enmascarado), info = columna)
    }
    expect_false(any(vapply(unique(valores), function(valor) {
      any(grepl(valor, .q5_hojas(plan), fixed = TRUE))
    }, logical(1L))))

    protegido <- aplicar(.q5_activar(plan, indices), datos)
    claro <- aplicar(.q5_activar(plan_claro, indices_claros), datos)
    for (columna in names(datos)) {
      expect_identical(protegido$datos[[columna]], claro$datos[[columna]],
                       info = paste(lado, columna))
    }
    columnas_registro <- c("estrategia", "estado", "n_cambiadas",
                           "n_no_reversibles", "error")
    expect_identical(
      as.data.frame(protegido$registro)[, columnas_registro, drop = FALSE],
      as.data.frame(claro$registro)[, columnas_registro, drop = FALSE]
    )
    expect_true(all(as.character(protegido$registro$estado) == "ejecutada"))

    if (lado == "determinante") {
      guiado <- suppressWarnings(guiar_limpieza(plan, datos))
      i_guiado <- .q5_accion(guiado, "Edad_entero")
      expect_true(all(is.na(guiado$parametros[[i_guiado]]$mapa$determinante)))
    } else if (clase == "Date") {
      i_fecha <- .q5_accion(plan, columna)
      plan_extra <- plan
      plan_extra$parametros[[i_fecha]]$fecha_extra <- as.Date("1980-02-02")
      guiado <- suppressWarnings(guiar_limpieza(
        plan_extra, datos, selector = function(decision) 0
      ))
      expect_true(is.na(guiado$parametros[[i_fecha]]$fecha_extra))
    }
  }
})

test_that("Q5: analizar tambien publica el lado protegido tapado", {
  caso <- .q5_matriz("determinante")
  analisis <- suppressWarnings(suppressMessages(analizar(
    caso$datos, columnas_personales = caso$protegidas,
    muestra = nrow(caso$datos), conservar_datos = FALSE,
    medir_propuesta = FALSE
  )))
  i <- .q5_accion(analisis$plan_limpieza, "Edad_entero")
  expect_true(length(i) == 1L)
  expect_true(all(is.na(
    analisis$plan_limpieza$parametros[[i]]$mapa$determinante
  )))
  expect_false(any(grepl("1980-", .q5_hojas(analisis$plan_limpieza),
                         fixed = TRUE)))
})

test_that("Q5: el piso no vuelve a publicar titulares en el lado publico", {
  datos <- data.frame(
    titular = c(rep("TitularUno", 4L), rep("TitularDos", 4L)),
    ci_titular = c(rep(101L, 4L), rep(202L, 4L)),
    sucursal = c(NA_character_, rep("Farmacia TitularUno", 3L),
                 NA_character_, rep("Farmacia TitularDos", 3L)),
    stringsAsFactors = FALSE
  )
  perfil <- suppressWarnings(perfilar(
    datos, columnas_personales = c("titular", "ci_titular"),
    muestra = nrow(datos), normalizar = FALSE,
    casi_duplicados_vocabulario = FALSE, max_filas_hallazgo = 20L,
    analizar_dependencias = FALSE
  ))
  perfil$dependencias <- data.frame(
    determinante = "titular", dependiente = "sucursal",
    cumplimiento = 1, n_evaluados = 8L, n_grupos = 2L,
    n_violaciones = 0L, exacta = TRUE, evidencia = "",
    stringsAsFactors = FALSE
  )
  plan <- suppressWarnings(planificar_limpieza(perfil, datos))
  i <- .q5_accion(plan, "sucursal")
  expect_true(length(i) == 1L)
  mapa <- plan$parametros[[i]]$mapa
  titulares <- unique(datos$titular)
  expect_false(any(vapply(titulares, function(valor) {
    any(grepl(valor, .q5_hojas(mapa), fixed = TRUE))
  }, logical(1L))))
  expect_true(all(mapa$determinante == "[valor protegido]"))
})

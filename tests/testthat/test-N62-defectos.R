# Regresiones N62. Las cadenas acentuadas se construyen desde sus bytes para
# reproducir la marca `unknown` que deja `read.csv()`.

.sin_marca_n62 <- function(x) rawToChar(charToRaw(x))

.fixture_texto_n62 <- function(n = 30L) {
  nombre <- .sin_marca_n62(paste0("categor", intToUtf8(0xED), "a"))
  datos <- data.frame(id = seq_len(n), stringsAsFactors = FALSE)
  datos[[nombre]] <- c(
    rep(.sin_marca_n62(intToUtf8(0xF1)), n %/% 3L),
    rep(.sin_marca_n62(intToUtf8(0xE1)), n %/% 3L),
    rep("Maria", n - 2L * (n %/% 3L))
  )
  datos
}

.fixture_dominio_n62 <- function() {
  nombre <- .sin_marca_n62(paste0("categor", intToUtf8(0xED), "a"))
  datos <- data.frame(
    entero = seq_len(90L),
    valor = c(rep(1, 70L), rep(NA_real_, 20L)),
    stringsAsFactors = FALSE
  )
  datos[[nombre]] <- c(
    rep(.sin_marca_n62(intToUtf8(0xF1)), 30L),
    rep(.sin_marca_n62(intToUtf8(0xE1)), 25L),
    rep(.sin_marca_n62(intToUtf8(0xFC)), 20L),
    rep(.sin_marca_n62(intToUtf8(0x1F600)), 10L),
    rep("comun", 5L)
  )
  datos
}

.localizar_n62 <- function() {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  anteriores <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  locale_utf8 <- .primer_locale_utf8_n61()
  if (is.null(locale_utf8)) return(NULL)
  list(locale_utf8 = locale_utf8, categorias = categorias,
       anteriores = anteriores)
}

test_that("la evidencia de duplicados conserva los bytes del nombre", {
  estado <- .localizar_n62()
  if (is.null(estado)) skip("no hay ningun locale UTF-8 disponible en esta maquina")
  on.exit(for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, estado$anteriores[[categoria]]))
  }, add = TRUE)
  datos <- .fixture_texto_n62()
  nombre <- names(datos)[[2L]]
  resultados <- lapply(c(estado$locale_utf8, "C"), function(locale) {
    for (categoria in estado$categorias) {
      suppressWarnings(Sys.setlocale(categoria, locale))
      expect_identical(Sys.getlocale(categoria), locale)
    }
    salida <- suppressMessages(detectar_duplicados_aproximados(
      datos, columnas = nombre, metodo = "jw", umbral = 0.2,
      proteger_datos_personales = FALSE
    ))
    salida$pares$evidencia_1[[1L]]
  })
  expect_identical(
    charToRaw(resultados[[1L]]), charToRaw(resultados[[2L]])
  )
  expect_false(grepl("<c3><ad>", resultados[[2L]], fixed = TRUE))
  expect_true(grepl(
    .marcar_utf8_textos(nombre), resultados[[2L]], fixed = TRUE
  ))
})

test_that("las metricas textuales de medir son invariantes y persisten", {
  estado <- .localizar_n62()
  if (is.null(estado)) skip("no hay ningun locale UTF-8 disponible en esta maquina")
  on.exit(for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, estado$anteriores[[categoria]]))
  }, add = TRUE)
  datos <- .fixture_dominio_n62()
  resultados <- lapply(c(estado$locale_utf8, "C"), function(locale) {
    for (categoria in estado$categorias) {
      suppressWarnings(Sys.setlocale(categoria, locale))
      expect_identical(Sys.getlocale(categoria), locale)
    }
    analisis <- suppressMessages(analizar(
      datos, nombre = "tabla",
      fecha = as.POSIXct("2026-09-17", tz = "UTC"),
      analizar_dependencias = FALSE,
      proteger_datos_personales = FALSE
    ))
    medido <- analisis$medicion$resultado[
      analisis$medicion$metrica == "ValoresPosiblesPorExtension"
    ]
    archivo <- tempfile(fileext = ".rds")
    on.exit(unlink(archivo), add = TRUE)
    guardar_analisis(
      analisis, archivo, proteger_datos_personales = FALSE,
      sobrescribir = TRUE
    )
    persistido <- leer_analisis(archivo)$medicion$resultado[
      leer_analisis(archivo)$medicion$metrica == "ValoresPosiblesPorExtension"
    ]
    c(medido[[1L]], persistido[[1L]])
  })
  expect_identical(resultados[[1L]], c(1, 1))
  expect_identical(resultados[[2L]], c(1, 1))
})

test_that("las comparaciones textuales de los contratos usan bytes", {
  estado <- .localizar_n62()
  if (is.null(estado)) skip("no hay ningun locale UTF-8 disponible en esta maquina")
  on.exit(for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, estado$anteriores[[categoria]]))
  }, add = TRUE)
  nombre <- .sin_marca_n62(paste0("categor", intToUtf8(0xED), "a"))
  ene <- .sin_marca_n62(intToUtf8(0xF1))
  a <- .sin_marca_n62(intToUtf8(0xE1))
  datos <- data.frame(id = seq_len(4L), stringsAsFactors = FALSE)
  datos[[nombre]] <- c(ene, a, "comun", NA_character_)
  nucleo <- metricas_nucleo()
  instancias <- list(
    instanciar(
      especializar(nucleo$NoNulo, nombre_especifico = "NoNuloN62",
                   valores_nulos = ene),
      "tabla", nombre
    ),
    instanciar(
      especializar(nucleo$Formato, nombre_especifico = "FormatoN62",
                   diccionario = c(ene, a)),
      "tabla", nombre
    ),
    instanciar(
      especializar(nucleo$ValoresPosiblesPorExtension,
                   nombre_especifico = "DominioN62", valores = c(ene, a)),
      "tabla", nombre
    )
  )
  resultados <- lapply(c(estado$locale_utf8, "C"), function(locale) {
    for (categoria in estado$categorias) {
      suppressWarnings(Sys.setlocale(categoria, locale))
      expect_identical(Sys.getlocale(categoria), locale)
    }
    medir(modelo(instancias), datos)$resultado
  })
  expect_identical(resultados[[1L]], resultados[[2L]])
  expect_identical(resultados[[2L]], c(0, 1, 1, 0, 1, 1, 0, 1, 1, 0))
})

test_that("la frontera DBI marca texto UTF-8 valido sin cambiar sus bytes", {
  estado <- .localizar_n62()
  if (is.null(estado)) skip("no hay ningun locale UTF-8 disponible en esta maquina")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  on.exit(for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, estado$anteriores[[categoria]]))
  }, add = TRUE)
  texto <- .sin_marca_n62(intToUtf8(0xF1))
  tabla <- data.frame(nombre = texto, stringsAsFactors = FALSE)
  for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, "C"))
    expect_identical(Sys.getlocale(categoria), "C")
  }
  marcado <- .marcar_utf8_tabla(tabla)
  expect_identical(charToRaw(marcado$nombre[[1L]]), charToRaw(texto))
  expect_identical(Encoding(marcado$nombre[[1L]]), "UTF-8")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # La escritura se hace bajo UTF-8 para que la prueba aísle la salida DBI.
  for (categoria in estado$categorias) {
    suppressWarnings(Sys.setlocale(categoria, estado$locale_utf8))
  }
  DBI::dbWriteTable(con, "texto_n62", data.frame(
    nombre = c(texto, .sin_marca_n62(intToUtf8(0xE1)), "Maria"),
    stringsAsFactors = FALSE
  ))
  longitudes <- lapply(c(estado$locale_utf8, "C"), function(locale) {
    for (categoria in estado$categorias) {
      suppressWarnings(Sys.setlocale(categoria, locale))
      expect_identical(Sys.getlocale(categoria), locale)
    }
    perfil <- suppressMessages(perfilar_dbi(
      con, "texto_n62", metricas = "validos", muestra = Inf,
      analizar_dependencias = FALSE, proteger_datos_personales = FALSE
    ))
    fila <- perfil$perfil_muestra$columnas[
      perfil$perfil_muestra$columnas$columna == "nombre", , drop = FALSE
    ]
    c(fila$longitud_minima[[1L]], fila$longitud_maxima[[1L]])
  })
  expect_identical(longitudes[[1L]], longitudes[[2L]])
  expect_identical(longitudes[[2L]], c(1, 5))
})

.verificar_producto_n62 <- function(conexion) {
  for (universo in c("tabla_completa", "muestra_motor")) {
    for (bloque in c("con_muestra", "solo_agregados")) {
      argumentos <- list(
        conexion = conexion, tabla = "t", universo = universo,
        bloque_muestra = bloque, muestra = 4L, metricas = "validos",
        instrumentar = FALSE, analizar_dependencias = FALSE,
        proteger_datos_personales = FALSE
      )
      if (identical(universo, "muestra_motor")) {
        argumentos$muestra_motor <- 4L
      }
      argumentos_plan <- argumentos[setdiff(
        names(argumentos), c("analizar_dependencias", "proteger_datos_personales")
      )]
      plan <- do.call(plan_perfilado_dbi, argumentos_plan)
      corrida <- do.call(perfilar_dbi, argumentos)
      perfil_publicado <- !is.null(corrida$perfil_muestra)
      expect_identical(
        perfil_publicado, identical(bloque, "con_muestra"),
        info = paste(universo, bloque)
      )
      cobertura <- corrida$resumen_tabla$cobertura
      no_solicitada <- cobertura[
        cobertura$bloque == "perfil_muestra" &
          cobertura$estado == "no_solicitado", , drop = FALSE
      ]
      expect_identical(
        nrow(no_solicitada), if (identical(bloque, "solo_agregados")) 1L else 0L,
        info = paste(universo, bloque)
      )
      expect_identical(
        is.finite(attr(plan, "muestra")), identical(bloque, "con_muestra"),
        info = paste("plan", universo, bloque)
      )
      expect_identical(
        is.finite(attr(corrida$resumen_tabla$meta$plan, "muestra")),
        identical(bloque, "con_muestra"),
        info = paste("corrida", universo, bloque)
      )
      materializacion <- corrida$resumen_tabla$meta$materializacion
      bloques <- corrida$resumen_tabla$meta$bloques
      if (identical(universo, "muestra_motor") &&
          identical(bloque, "con_muestra")) {
        expect_identical(materializacion$estado, "cerrada_verificada")
        expect_identical(bloques$filas_vistas, 4)
      } else if (identical(universo, "muestra_motor")) {
        expect_identical(materializacion$estado, "no_solicitado")
        expect_identical(bloques$filas_vistas, 0)
      } else {
        expect_null(materializacion)
        expect_null(bloques)
      }
    }
  }
}

test_that("solo_agregados respeta el producto completo en RSQLite", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conexion), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    g = rep(c("a", "b", "c"), 4), x = 1:12
  ))
  .verificar_producto_n62(conexion)
})

test_that("solo_agregados respeta el producto completo en DuckDB", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  conexion <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conexion, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(conexion, "t", data.frame(
    g = rep(c("a", "b", "c"), 4), x = 1:12
  ))
  .verificar_producto_n62(conexion)
})

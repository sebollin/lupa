testthat::skip_on_cran()

.n2581_declarar_latin1 <- function(textos) {
  salida <- iconv(textos, from = "UTF-8", to = "latin1")
  Encoding(salida) <- "latin1"
  salida
}

.n2581_fixture <- function() {
  bytes <- seq.int(0x80L, 0xffL)
  roto <- vapply(bytes, function(byte) {
    paste0(
      if (byte %% 2L) " Borde " else " borde ",
      intToUtf8(c(0xc3L, byte), multiple = TRUE),
      intToUtf8(0xadL),
      " &amp; ",
      collapse = ""
    )
  }, character(1L))
  roto <- c(roto, intToUtf8(0x0aL))
  invisible <- c(
    vapply(bytes, intToUtf8, character(1L)),
    intToUtf8(0x0aL)
  )
  latin1 <- list(
    roto = .n2581_declarar_latin1(roto),
    invisible = .n2581_declarar_latin1(invisible)
  )
  utf8 <- lapply(latin1, lupa:::.latin1_a_utf8)
  datos <- data.frame(
    roto_latin1 = latin1$roto,
    roto_utf8 = utf8$roto,
    invisible_latin1 = latin1$invisible,
    invisible_utf8 = utf8$invisible,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(datos = datos, bytes = bytes)
}

.n2581_profile_signature <- function(perfil, columnas) {
  nombres <- c(
    "n_espacios_borde", "n_controles_invisibles",
    "n_espacios_invisibles", "n_separadores_en_campo",
    "n_entidades_html", "n_codificacion_rota",
    "n_codificacion_reparable", "n_codificacion_reparable_parcialmente",
    "n_codificacion_irreparable", "n_codificacion_no_se_pudo",
    "n_codificacion_invalida"
  )
  salida <- perfil$columnas[perfil$columnas$columna %in% columnas, nombres,
                            drop = FALSE]
  salida[] <- lapply(salida, as.numeric)
  salida
}

.n2581_plan_signature <- function(plan) {
  data.frame(
    columna = sub("_(latin1|utf8)$", "", as.character(plan$columna)),
    estrategia = as.character(plan$estrategia),
    n_afectadas = as.numeric(plan$n_afectadas),
    estado = as.character(plan$estado),
    stringsAsFactors = FALSE
  )
}

.n2581_correr <- function(datos, perfil, plan, estrategias, reparar, editar_orden) {
  plan$aplicar <- as.character(plan$estrategia) %in% estrategias &
    as.character(plan$estado) == "lista"
  reparaciones <- as.character(plan$estrategia) == "reparar_codificacion"
  plan$aplicar[reparaciones] <- reparar
  if (editar_orden) plan$orden <- rev(seq_len(nrow(plan)))
  list(plan = plan, resultado = aplicar(plan, datos))
}

.n2581_celdas_de_bytes <- function(celdas, utf8 = FALSE) {
  latin1 <- vapply(celdas, function(bytes) {
    texto <- rawToChar(as.raw(bytes))
    Encoding(texto) <- "latin1"
    texto
  }, character(1L))
  if (utf8) lupa:::.latin1_a_utf8(latin1) else latin1
}

.n2581_firma_plan_completa <- function(plan) {
  data.frame(
    columna = as.character(plan$columna),
    hallazgo = as.character(plan$hallazgo),
    estrategia = as.character(plan$estrategia),
    recomendada = as.logical(plan$recomendada),
    aplicar = as.logical(plan$aplicar),
    n_afectadas = as.numeric(plan$n_afectadas),
    estado = as.character(plan$estado),
    stringsAsFactors = FALSE
  )
}

.n2581_firma_registro <- function(registro) {
  registro[, c("estrategia", "estado", "n_cambiadas", "n_no_reversibles"),
           drop = FALSE]
}

.n2581_correr_caso <- function(celdas, estrategias) {
  latin1 <- data.frame(
    t = .n2581_celdas_de_bytes(celdas),
    stringsAsFactors = FALSE
  )
  utf8 <- data.frame(
    t = .n2581_celdas_de_bytes(celdas, utf8 = TRUE),
    stringsAsFactors = FALSE
  )
  pares <- list(latin1 = latin1, utf8 = utf8)
  perfiles <- lapply(pares, function(datos) suppressWarnings(perfilar(
    datos, fecha = "2026-01-15", proteger_datos_personales = FALSE
  )))
  planes <- Map(function(perfil, datos) {
    planificar_limpieza(perfil, datos)
  }, perfiles, pares)
  expect_identical(
    .n2581_firma_plan_completa(planes[[1L]]),
    .n2581_firma_plan_completa(planes[[2L]])
  )
  ejecutar <- function(indice, solo = FALSE) {
    plan <- planes[[indice]]
    if (solo) {
      plan$aplicar <- as.character(plan$estrategia) %in% estrategias &
        as.character(plan$estado) == "lista"
    }
    aplicar(plan, pares[[indice]])
  }
  comparar <- function(resultados) {
    convertir <- function(x) enc2utf8(lupa:::.latin1_a_utf8(x))
    expect_identical(
      convertir(resultados[[1L]]$datos$t),
      convertir(resultados[[2L]]$datos$t)
    )
    expect_identical(
      .n2581_firma_registro(resultados[[1L]]$registro),
      .n2581_firma_registro(resultados[[2L]]$registro)
    )
  }
  for (solo in c(FALSE, TRUE)) {
    resultados <- list(ejecutar(1L, solo), ejecutar(2L, solo))
    comparar(resultados)
  }
}

.n2581_comparar_pares <- function(resultado) {
  datos <- resultado$datos
  a_utf8 <- function(x) enc2utf8(lupa:::.latin1_a_utf8(x))
  expect_identical(
    a_utf8(datos$roto_latin1),
    a_utf8(datos$roto_utf8)
  )
  expect_identical(
    a_utf8(datos$invisible_latin1),
    a_utf8(datos$invisible_utf8)
  )
}

test_that("latin1 no cambia el perfil, el plan ni la proteccion textual", {
  fixture <- .n2581_fixture()
  datos <- fixture$datos
  perfiles <- lapply(
    list(datos[c("roto_latin1", "invisible_latin1")],
         datos[c("roto_utf8", "invisible_utf8")]),
    function(x) suppressWarnings(perfilar(
      x, fecha = "2026-01-15", proteger_datos_personales = FALSE
    ))
  )
  planes <- lapply(seq_along(perfiles), function(i) {
    nombres <- if (i == 1L) {
      c("roto_latin1", "invisible_latin1")
    } else c("roto_utf8", "invisible_utf8")
    planificar_limpieza(
      perfiles[[i]],
      datos[nombres]
    )
  })
  expect_identical(
    .n2581_profile_signature(
      perfiles[[1L]], c("roto_latin1", "invisible_latin1")
    ),
    .n2581_profile_signature(
      perfiles[[2L]], c("roto_utf8", "invisible_utf8")
    )
  )
  expect_identical(
    .n2581_plan_signature(planes[[1L]]),
    .n2581_plan_signature(planes[[2L]])
  )

  independientes <- c(
    "recortar_espacios", "eliminar_controles_invisibles",
    "normalizar_espacios_invisibles", "decodificar_entidades_html",
    "reemplazar_separadores"
  )
  for (capitalizacion in c(
    "convertir_minusculas", "convertir_mayusculas", "convertir_titulo"
  )) {
    estrategias <- c(independientes, capitalizacion)
    for (reparar in c(FALSE, TRUE)) {
      for (editar_orden in c(FALSE, TRUE)) {
        corridas <- lapply(seq_along(planes), function(i) {
          nombres <- if (i == 1L) {
            c("roto_latin1", "invisible_latin1")
          } else c("roto_utf8", "invisible_utf8")
          .n2581_correr(
            datos[nombres], perfiles[[i]], planes[[i]], estrategias,
            reparar, editar_orden
          )
        })
        .n2581_comparar_pares(
          structure(
            list(datos = cbind(
              corridas[[1L]]$resultado$datos,
              corridas[[2L]]$resultado$datos
            )),
            class = "resultado_limpieza"
          )
        )
      }
    }
  }
  for (estrategia in independientes) {
    for (reparar in FALSE) {
      corridas <- lapply(seq_along(planes), function(i) {
        nombres <- if (i == 1L) {
          c("roto_latin1", "invisible_latin1")
        } else c("roto_utf8", "invisible_utf8")
        .n2581_correr(
          datos[nombres], perfiles[[i]], planes[[i]], estrategia,
          reparar, FALSE
        )
      })
      .n2581_comparar_pares(
        structure(
          list(datos = cbind(
            corridas[[1L]]$resultado$datos,
            corridas[[2L]]$resultado$datos
          )),
          class = "resultado_limpieza"
        )
      )
      for (i in seq_along(corridas)) {
        plan <- corridas[[i]]$plan
        registro <- corridas[[i]]$resultado$registro
        seleccionadas <- which(plan$aplicar &
                                 as.character(plan$estrategia) == estrategia)
        for (j in seleccionadas) {
          fila <- registro[registro$id_accion == plan$id_accion[[j]], ,
                           drop = FALSE]
          expect_identical(nrow(fila), 1L)
          expect_identical(
            as.numeric(plan$n_afectadas[[j]]),
            as.numeric(fila$n_cambiadas)
          )
        }
      }
    }
  }
})

test_that("la proteccion textual conserva cuatro secuencias con latin1", {
  casos <- list(
    celda45 = list(
      acciones = c(
        "eliminar_controles_invisibles",
        "normalizar_espacios_invisibles"
      ),
      celdas = list(
        c(0x20L, 0x50L, 0x77L, 0x8fL, 0xc2L, 0xa7L, 0xa0L,
          0xe2L, 0x80L, 0x93L),
        c(0x6fL, 0x6bL), c(0x6fL, 0x74L, 0x72L, 0x6fL)
      )
    ),
    c1_mojibake = list(
      acciones = "eliminar_controles_invisibles",
      celdas = list(
        c(0x09L, 0x9dL, 0xe2L, 0x80L, 0xa6L),
        c(0x20L, 0x50L, 0x77L, 0x8fL, 0xc2L, 0xa7L, 0xa0L,
          0xe2L, 0x80L, 0x93L),
        c(0x6fL, 0x6bL)
      )
    ),
    espacio_guion_c1 = list(
      acciones = "eliminar_controles_invisibles",
      celdas = list(
        c(0xa0L, 0xadL, 0xc2L, 0x81L, 0x59L, 0x6dL, 0x4aL,
          0x4dL, 0x7aL, 0x47L),
        c(0x20L, 0x50L, 0x77L, 0x8fL, 0xc2L, 0xa7L, 0xa0L,
          0xe2L, 0x80L, 0x93L),
        c(0x6fL, 0x6bL)
      )
    ),
    espacio_duro_c1 = list(
      acciones = "normalizar_espacios_invisibles",
      celdas = list(
        c(0xa0L, 0xccL, 0x81L),
        c(0x20L, 0x50L, 0x77L, 0x8fL, 0xc2L, 0xa7L, 0xa0L,
          0xe2L, 0x80L, 0x93L),
        c(0x6fL, 0x6bL)
      )
    )
  )
  for (caso in casos) {
    .n2581_correr_caso(caso$celdas, caso$acciones)
  }
})

# Un espacio Unicode al borde -el duro U+00A0, el fino U+202F y los demas
# separadores de Unicode- es un espacio al borde: el perfil lo cuenta en
# `n_espacios_borde`, `recortar_espacios` lo quita, y
# `normalizar_espacios_invisibles` corre antes que el recorte. Antes el perfil y
# el recorte solo veian `[ \t\r\n]`, y la normalizacion corria despues: el duro
# del borde quedaba convertido en un espacio comun que nadie recortaba, y el
# perfil de lo aplicado volvia a publicar `espacios_sobrantes`. El duro que es
# la segunda mitad de una letra mal convertida no es un espacio al borde.
testthat::skip_on_cran()

.eub_u <- function(...) intToUtf8(c(...))

.eub_perfil <- function(datos) {
  perfilar(datos, fecha = "2026-01-15", analizar_dependencias = FALSE)
}

# Celdas que empiezan o terminan con un blanco ASCII o un separador de Unicode.
.eub_n_borde <- function(x) {
  x <- enc2utf8(as.character(x[!is.na(x)]))
  sum(grepl("^[\\s\\p{Z}]|[\\s\\p{Z}]$", x, perl = TRUE))
}

.eub_tipos <- function(perfil, columna) {
  h <- perfil$hallazgos
  as.character(h$tipo_hallazgo[h$columna == columna])
}

test_that("aplicar el plan por omision no deja espacios al borde", {
  nb <- .eub_u(0xA0)
  esperado <- c("uno", "dos", "tres", "cuatro", "cinco", "seis")
  casos <- list(
    # el recorte estaba en el plan, pero corria antes que la normalizacion
    con_espacio_ascii = c(paste0(nb, "uno"), "dos ", paste0("tres", nb),
                          "cuatro", "cinco", "seis"),
    # el perfil no contaba el duro y el plan no traia el recorte
    solo_duro = c(paste0(nb, "uno"), "dos", paste0("tres", nb),
                  "cuatro", "cinco", "seis")
  )
  for (nombre in names(casos)) {
    for (forma in c("utf8", "latin1", "factor")) {
      x <- casos[[nombre]]
      if (identical(forma, "latin1")) {
        x <- iconv(x, "UTF-8", "latin1")
        Encoding(x) <- "latin1"
      }
      if (identical(forma, "factor")) x <- factor(x)
      datos <- data.frame(t = x, stringsAsFactors = FALSE)
      perfil <- .eub_perfil(datos)
      expect_equal(perfil$columnas$n_espacios_borde, 2 + (nombre == "con_espacio_ascii"),
                   info = paste(nombre, forma))
      resultado <- aplicar(planificar_limpieza(perfil, datos), datos)
      final <- as.character(resultado$datos$t)
      expect_identical(enc2utf8(final), esperado, info = paste(nombre, forma))
      expect_equal(.eub_n_borde(final), 0L, info = paste(nombre, forma))
      tipos <- .eub_tipos(.eub_perfil(resultado$datos), "t")
      expect_false("espacios_sobrantes" %in% tipos, info = paste(nombre, forma))
      expect_false("controles_invisibles" %in% tipos, info = paste(nombre, forma))
    }
  }
})

test_that("la normalizacion corre entre la eliminacion de invisibles y el recorte", {
  nb <- .eub_u(0xA0)
  datos <- data.frame(
    t = c(paste0("uno", .eub_u(0x200B)), paste0(nb, "dos"), "tres ",
          "cuatro", "cinco"),
    stringsAsFactors = FALSE
  )
  plan <- planificar_limpieza(.eub_perfil(datos), datos)
  orden <- stats::setNames(plan$orden, plan$estrategia)
  expect_lt(orden[["eliminar_controles_invisibles"]],
            orden[["normalizar_espacios_invisibles"]])
  expect_lt(orden[["normalizar_espacios_invisibles"]],
            orden[["recortar_espacios"]])
  resultado <- aplicar(plan, datos)
  registro <- resultado$registro
  expect_true(all(registro$estado == "ejecutada"))
  expect_lt(match("normalizar_espacios_invisibles", registro$estrategia),
            match("recortar_espacios", registro$estrategia))
  expect_identical(resultado$datos$t, c("uno", "dos", "tres", "cuatro", "cinco"))
})

test_that("cada espacio Unicode al borde se cuenta y el recorte solo lo quita", {
  # Los mismos que convierte `normalizar_espacios_invisibles`.
  codigos <- c(0xA0, 0x1680, 0x2000:0x200A, 0x2028, 0x2029, 0x202F, 0x205F,
               0x3000)
  base <- paste0("v", seq_along(codigos))
  celdas <- ifelse(seq_along(codigos) %% 2L == 0L,
                   paste0(vapply(codigos, .eub_u, ""), base),
                   paste0(base, vapply(codigos, .eub_u, "")))
  datos <- data.frame(t = c(celdas, "ok", "ok"), stringsAsFactors = FALSE)
  perfil <- .eub_perfil(datos)
  expect_equal(perfil$columnas$n_espacios_borde, length(codigos))
  expect_true("espacios_sobrantes" %in% .eub_tipos(perfil, "t"))
  plan <- planificar_limpieza(perfil, datos)
  recorte <- plan$estrategia == "recortar_espacios"
  expect_equal(plan$n_afectadas[recorte], length(codigos))
  plan$aplicar <- recorte
  resultado <- aplicar(plan, datos)
  expect_identical(resultado$datos$t, c(base, "ok", "ok"))
  expect_equal(resultado$registro$n_cambiadas, length(codigos))
  expect_identical(resultado$registro$estado, "ejecutada")
})

test_that("el espacio al borde es el mismo conjunto en validadores, recorte y normalizacion", {
  # Todo el plano basico menos las mitades de los pares sustitutos, que no son
  # caracteres. Lo que el recorte deja vacio es un espacio al borde.
  codigos <- setdiff(1L:0xFFFFL, 0xD800L:0xDFFFL)
  caracteres <- intToUtf8(codigos, multiple = TRUE)
  espacios <- codigos[!nzchar(.recortar_espacios_borde(caracteres))]
  esperados <- c(9L:13L, 32L, as.integer(.codigos_espacios_invisibles))
  expect_setequal(espacios, esperados)
  expect_equal(length(espacios), length(esperados))
  # y la accion y la validacion usan la misma regla
  muestra <- paste0(caracteres[espacios], "a", caracteres[espacios])
  expect_identical(.recortar_texto(muestra)$valor, rep("a", length(espacios)))
})

test_that("el duro de una letra mal convertida no es un espacio al borde", {
  nb <- .eub_u(0xA0)
  # `A` con tilde seguida de un duro es una `a` con grave leida como latin-1.
  rota <- paste0("all", .eub_u(0xC3), nb)
  datos <- data.frame(
    t = c(rota, paste0(rota, nb), paste0(nb, "dos"), "tres", "cuatro"),
    stringsAsFactors = FALSE
  )
  perfil <- .eub_perfil(datos)
  # la letra rota sola no se cuenta; con un duro de mas, si
  expect_equal(perfil$columnas$n_espacios_borde, 2)
  plan <- planificar_limpieza(perfil, datos)
  sin_reparar <- plan
  sin_reparar$aplicar <- sin_reparar$aplicar &
    sin_reparar$estrategia != "reparar_codificacion"
  resultado <- aplicar(sin_reparar, datos)
  expect_identical(resultado$datos$t,
                   c(rota, rota, "dos", "tres", "cuatro"))
  # con la reparacion, que el plan trae marcada, las dos se reparan
  reparado <- aplicar(plan, datos)
  expect_identical(reparado$datos$t,
                   c(rep(paste0("all", .eub_u(0xE0)), 2L), "dos", "tres", "cuatro"))
  # el duro del medio de una letra rota tampoco se toca
  medio <- paste0("all", .eub_u(0xC3), nb, "y")
  expect_identical(.recortar_texto(c(medio, rota))$valor, c(medio, rota))
})

test_that("el duro del medio se normaliza y no se recorta", {
  nb <- .eub_u(0xA0)
  datos <- data.frame(
    t = c(paste0("uno", nb, "y"), "dos ", "tres", "cuatro", "cinco"),
    stringsAsFactors = FALSE
  )
  resultado <- aplicar(planificar_limpieza(.eub_perfil(datos), datos), datos)
  expect_identical(resultado$datos$t, c("uno y", "dos", "tres", "cuatro", "cinco"))
})

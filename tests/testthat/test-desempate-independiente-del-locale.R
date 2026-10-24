# Un desempate que consulta la configuracion regional publica cosas distintas en
# maquinas distintas sobre el mismo dato.
#
# El paquete ya arreglo esta familia una vez -el recorte de pares de duplicados,
# que paso a `.ordenar_por_bytes()`- y `R/columnas.R:30` ya usaba
# `method = "radix"` para el desempate de la moda. El arreglo no habia llegado a
# `R/patrones.R` ni a `R/faltantes.R`, y ahi `table()` ordena sus niveles con la
# intercalacion del locale mientras `sort()` conserva ese orden en los empates.
#
# Medido el 2026-09-05, con `max_patrones = 2` y un empate por el segundo
# puesto: `A+ a+` con la colacion por omision y `A+ Aa` con `LC_COLLATE = C`. No
# es orden de filas, son patrones distintos, y con ellos cambian los ejemplos.
#
# Estas pruebas cambian una variable GLOBAL de la sesion, asi que la restauran
# con `on.exit()`: una prueba que deja el estado movido hace fallar a las que
# vienen despues, y el fallo aparece lejos de su causa.
#
# Y una trampa que costo la primera version de este archivo: **dentro de la
# suite `LC_COLLATE` ya vale `C`**, porque testthat lo fija. La primera version
# comparaba "el locale de la sesion" contra `C`, que ahi son el mismo, y pasaba
# con el defecto puesto sin medir nada. Hay que nombrar los DOS locales.

# Un locale cuya intercalacion NO coincide con la de `C`: en `C` ordena por
# bytes -mayusculas antes que minusculas- y en estos, no. Se prueban varios
# porque no toda maquina tiene los mismos instalados.
.locale_no_c <- function() {
  anterior <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", anterior)), add = TRUE)
  for (candidato in c("es_UY.UTF-8", "es_ES.UTF-8", "en_US.UTF-8", "C.UTF-8")) {
    puesto <- suppressWarnings(Sys.setlocale("LC_COLLATE", candidato))
    if (nzchar(puesto) && !identical(sort(c("a", "A")), c("A", "a"))) {
      return(candidato)
    }
  }
  NULL
}

.con_colacion <- function(locale, expr) {
  anterior <- Sys.getlocale("LC_COLLATE")
  cambiado <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", anterior)), add = TRUE)
  if (!nzchar(cambiado)) {
    return(NULL)  # esta maquina no tiene ese locale
  }
  force(expr)
}

test_that("los patrones publicados no dependen de la colacion de la sesion", {
  # Tres patrones y `max_patrones = 2`: el segundo puesto queda empatado entre
  # `a+` y `Aa`, ambos con 50 filas.
  datos <- data.frame(
    texto = c(rep("SI", 400L), rep("no", 50L), rep("No", 50L)),
    stringsAsFactors = FALSE
  )
  medir <- function() {
    perfil <- perfilar(datos, max_patrones = 2L, analizar_dependencias = FALSE)
    perfil$patrones$texto[, c("patron", "n")]
  }

  otro <- .locale_no_c()
  skip_if(is.null(otro), "esta maquina no tiene un locale que difiera de C")

  con_c <- .con_colacion("C", medir())
  con_otro <- .con_colacion(otro, medir())
  skip_if(is.null(con_c) || is.null(con_otro), "no se pudo fijar la colacion")

  # Primera mitad: el recorte se activo. Si `max_patrones` no recortara, las dos
  # corridas coincidirian sin probar nada sobre el desempate.
  expect_equal(nrow(con_c), 2L)
  expect_equal(con_c$n, c(400L, 50L))

  expect_equal(con_otro$patron, con_c$patron)
})

test_that("las etiquetas de faltante disfrazado no dependen de la colacion", {
  # Ocho etiquetas con la MISMA frecuencia y seis lugares en la evidencia: el
  # desempate decide cuales dos quedan afuera.
  etiquetas <- c("N/A", "n/a", "NA", "na", "S/D", "s/d", "SIN DATO", "sin dato")
  datos <- data.frame(
    t = c(rep(etiquetas, each = 10L), sprintf("valor%03d", 1:200)),
    stringsAsFactors = FALSE
  )
  medir <- function() {
    perfil <- perfilar(datos, analizar_dependencias = FALSE)
    fila <- perfil$hallazgos[
      perfil$hallazgos$tipo_hallazgo == "faltantes_disfrazados", , drop = FALSE
    ]
    if (!nrow(fila)) NA_character_ else fila$evidencia[1L]
  }

  otro <- .locale_no_c()
  skip_if(is.null(otro), "esta maquina no tiene un locale que difiera de C")

  con_c <- .con_colacion("C", medir())
  con_otro <- .con_colacion(otro, medir())
  skip_if(is.null(con_c) || is.null(con_otro), "no se pudo fijar la colacion")

  # Primera mitad: el hallazgo se emitio y la evidencia recorto a seis.
  expect_false(is.na(con_c))
  expect_equal(lengths(regmatches(con_c, gregexpr("(", con_c, fixed = TRUE))), 6L)

  expect_equal(con_otro, con_c)
})

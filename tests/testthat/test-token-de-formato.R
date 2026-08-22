skip_if_not_installed("stringdist")

# Un token que aparece en buena parte de la columna es parte del FORMATO, no una
# errata. Salio de mirar que reportaba el detector sobre PED/Flight, una tabla
# real de vuelos:
#
#   [12:00 a.m. (5) / 12:00 p.m. (42)]    <- doce horas de diferencia, no una variante
#   [1:48 p.m. (27) / 1:48 p.m.  Delayed (1)]  <- el estado del vuelo pegado en la hora
#
# La primera es un falso positivo y la segunda un hallazgo real. Las dos difieren
# poco como cadena; lo que las distingue es que `a.m.` y `p.m.` estan en casi
# todos los valores y `Delayed` en uno.

.horas_con_formato <- function() {
  # 24 horas distintas en los dos meridianos: `a.m.` y `p.m.` aparecen en la
  # mitad de las formas cada uno, muy por encima del umbral.
  horas <- sprintf("%d:%02d", rep(1:12, each = 2L), rep(c(5L, 35L), 12L))
  c(
    rep(paste(horas, "a.m."), 3L),
    rep(paste(horas, "p.m."), 3L),
    # Y el caso real: el estado pegado en una sola fila.
    rep("7:05 p.m.", 20L), "7:05 p.m. Delayed"
  )
}

.grupos_de <- function(x) {
  .grupos_casi_duplicados_vocabulario(x, NULL, "hora")$grupos
}

.variantes_de <- function(grupos) {
  lapply(grupos, function(g) sort(g$variantes))
}

test_that("un token que es marca de formato no genera un falso duplicado", {
  grupos <- .grupos_de(.horas_con_formato())
  variantes <- .variantes_de(grupos)

  # Lo que NO puede aparecer: dos horas iguales en meridianos distintos.
  meridiano <- vapply(variantes, function(v) {
    length(v) == 2L && identical(sub(" [ap]\\.m\\.$", "", v[[1L]]),
                                 sub(" [ap]\\.m\\.$", "", v[[2L]]))
  }, logical(1L))
  expect_false(any(meridiano))

  # Lo que SI tiene que aparecer: el estado pegado dentro de la hora. Sin esta
  # mitad, la prueba pasaria con un detector que no encuentra nada.
  pegado <- vapply(variantes, function(v) {
    any(grepl("Delayed", v, fixed = TRUE))
  }, logical(1L))
  expect_true(any(pegado))
})

test_that("el descarte se declara en el alcance", {
  alcance <- .grupos_casi_duplicados_vocabulario(
    .horas_con_formato(), NULL, "hora"
  )$alcance
  # Descartar sin decirlo seria el mismo problema que el detector arregla.
  expect_gt(alcance$n_pares_descartados_formato, 0L)
})

test_that("un valor de un solo token nunca se descarta por formato", {
  # Esta es la condicion que faltaba en la primera version y tiro 44 pruebas:
  # si el valor es un solo token, "el token que difiere" es el valor entero, y
  # la regla borraba el caso central del detector.
  # El par sale de datos reales: `CURUPU` y `CURURU` son dos calles de
  # Montevideo que el detector agrupa, y difieren en una sola letra. (Un par a
  # dos letras como `Marano`/`Marebo` el paquete lo separa a proposito, asi que
  # no sirve de control: ver `test-casi-duplicados-vocabulario.R`.)
  variantes <- c(rep("CURUPU", 30L), "CURURU")
  relleno <- paste0("RELLENO", LETTERS[1:24])
  grupos <- .grupos_de(c(variantes, relleno))
  encontrado <- vapply(.variantes_de(grupos), function(v) {
    all(c("CURUPU", "CURURU") %in% v)
  }, logical(1L))
  expect_true(any(encontrado))
})

test_that("con un vocabulario chico no se descarta nada por formato", {
  # "Aparece en toda la columna" no significa nada sobre cinco formas: ahi
  # cualquier token pasa el umbral por aritmetica.
  chico <- c(rep("7:05 a.m.", 20L), "7:05 p.m.", "8:10 a.m.", "9:15 a.m.")
  alcance <- .grupos_casi_duplicados_vocabulario(chico, NULL, "hora")$alcance
  expect_equal(alcance$n_pares_descartados_formato, 0L)
})

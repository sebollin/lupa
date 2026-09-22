# O02. Una accion cuyo acierto depende del dominio no se autoaplica.
#
# `convertir_ausencias_textuales` convertia `NA` en ausencia sobre una columna
# de codigos de pais ISO, donde `NA` **es** Namibia: tres valores reales
# destruidos en silencio por el plan recomendado.
#
# La vuelta anterior corrigio la JUSTIFICACION para que pidiera confirmar el
# dominio, y el plan siguio marcandose `recomendada`, `aplicar = TRUE` y
# `destructiva = FALSE` sobre un cambio que la misma fila declaraba
# `reversible = FALSE`. Cambiar la frase no cambia lo que el paquete hace: por
# eso esta prueba mide la CONDUCTA y no el texto.
#
# La distincion no es "irreversible": `recortar_espacios` tambien lo es y se
# recomienda con razon, porque no exige ningun juicio sobre el dominio. Es
# "podria ser un valor legitimo": `NA` es Namibia y `NULL` puede ser un
# apellido, pero nadie escribe `S/D` ni `sin dato` como dato.

.o02_plan <- function(valores) {
  datos <- data.frame(v = valores, id = seq_along(valores), stringsAsFactors = FALSE)
  list(datos = datos,
       plan = planificar_limpieza(
         perfilar(datos, analizar_dependencias = FALSE), datos))
}

.o02_fila <- function(plan) grep("ausencias", as.character(plan$estrategia))

test_that("un marcador que podria ser un valor legitimo queda para activar a mano", {
  for (caso in list(
    c("NA", "NA", "NA", "AF", "AU", "BR", "CA", "CN", "DE", "ES"),
    c("NULL", "NULL", "Perez", "Gomez", "Diaz", "Lopez")
  )) {
    armado <- .o02_plan(caso)
    fila <- .o02_fila(armado$plan)
    expect_true(length(fila) > 0L, info = caso[[1L]])
    expect_false(any(as.logical(armado$plan$recomendada[fila])), info = caso[[1L]])
    expect_false(any(as.logical(armado$plan$aplicar[fila])), info = caso[[1L]])
    # La documentacion del paquete dice que lo destructivo requiere activacion
    # explicita: esto es exactamente eso.
    expect_true(any(as.logical(armado$plan$destructiva[fila])), info = caso[[1L]])

    # Y la conducta, que es lo que importa: los valores sobreviven.
    resultado <- aplicar(armado$plan, armado$datos)$datos$v
    expect_equal(sum(is.na(resultado)), 0L, info = caso[[1L]])
    expect_identical(as.character(resultado), caso, info = caso[[1L]])
  }
})

test_that("un marcador que nadie escribiria como dato se sigue aplicando solo", {
  for (caso in list(
    c("S/D", "S/D", " A", " A", "B", "C"),
    c("sin dato", "sin dato", "a", "b", "c", "d")
  )) {
    armado <- .o02_plan(caso)
    fila <- .o02_fila(armado$plan)
    expect_true(length(fila) > 0L, info = caso[[1L]])
    expect_true(all(as.logical(armado$plan$recomendada[fila])), info = caso[[1L]])
    expect_true(all(as.logical(armado$plan$aplicar[fila])), info = caso[[1L]])
    resultado <- aplicar(armado$plan, armado$datos)$datos$v
    expect_true(sum(is.na(resultado)) >= 2L, info = caso[[1L]])
  }
})

test_that("la justificacion nombra el marcador que obligo a confirmar", {
  armado <- .o02_plan(c("NA", "NA", "NA", "AF", "AU", "BR", "CA", "CN", "DE", "ES"))
  fila <- .o02_fila(armado$plan)
  justificacion <- as.character(armado$plan$justificacion[fila])[[1L]]
  expect_true(grepl("NA", justificacion, fixed = TRUE))
  expect_true(grepl("Namibia", justificacion, fixed = TRUE))
})

test_that("los marcadores se leen de la evidencia que el propio paquete publica", {
  leer <- lupa:::.marcadores_disfrazados
  expect_identical(leer("NA (3)"), "NA")
  expect_identical(leer("NC (59); SD (7)"), c("NC", "SD"))
  expect_identical(leer(""), character())
  expect_identical(leer(NA_character_), character())
  expect_identical(leer(NULL), character())
  # Un marcador con espacio adentro no se parte.
  expect_identical(leer("sin dato (4)"), "sin dato")

  decide <- lupa:::.marcador_puede_ser_valor
  expect_true(decide("NA"))
  expect_true(decide(c("S/D", "NULL")))
  expect_false(decide("S/D"))
  expect_false(decide("sin dato"))
  expect_false(decide("-"))
  expect_false(decide(character()))
})

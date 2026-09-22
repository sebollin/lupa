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

test_that("un marcador ambiguo escondido por la evidencia recortada se ve igual", {
  # La evidencia se recorta a los seis marcadores mas frecuentes. La primera
  # version de la regla la leia de ahi, y un `NA` poco frecuente -Namibia-
  # quedaba fuera de la vista: la conversion se autoaplicaba y lo destruia.
  columna <- c(rep("S/D", 3L), rep("-", 2L), rep("?", 2L), rep(".", 2L),
               rep("..", 2L), rep("N/D", 2L), "NA", rep("B", 4L))
  datos <- data.frame(x = columna, stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE)

  # Con los datos, que es lo habitual, se ve el marcador escondido.
  plan <- planificar_limpieza(perfil, datos)
  fila <- .o02_fila(plan)
  expect_true(length(fila) > 0L)
  expect_false(any(as.logical(plan$aplicar[fila])))
  resultado <- aplicar(plan, datos)$datos$x
  expect_identical(resultado[[which(columna == "NA")]], "NA")

  # Sin los datos no se puede ver, pero SI se puede saber que la evidencia vino
  # recortada: los conteos no suman el total. Y entonces se pide confirmar.
  solo_perfil <- planificar_limpieza(perfil)
  fila <- .o02_fila(solo_perfil)
  expect_true(length(fila) > 0L)
  expect_false(any(as.logical(solo_perfil$aplicar[fila])))
})

test_that("lo que el usuario declara como ausencia no se le discute", {
  # La deteccion ya deja pasar lo declarado porque el paquete no tiene con que
  # contradecir a quien conoce el dato. El plan no puede pedirle despues que
  # confirme lo que acaba de afirmar.
  for (caso in list(
    list(valores = c("SD", "SD", "SD", "NC", "NC", rep("B", 5L)), declarados = c("SD", "NC")),
    list(valores = c("NA1", "NA1", "NA1", rep("B", 4L)), declarados = "NA1")
  )) {
    datos <- data.frame(x = caso$valores, stringsAsFactors = FALSE)
    perfil <- perfilar(datos, cadenas_ausencia = caso$declarados,
                       analizar_dependencias = FALSE)
    plan <- planificar_limpieza(perfil, datos)
    fila <- .o02_fila(plan)
    expect_true(length(fila) > 0L, info = caso$declarados[[1L]])
    expect_true(all(as.logical(plan$aplicar[fila])), info = caso$declarados[[1L]])
  }
})

test_that("la lectura de marcadores y la decision de ambiguedad", {
  leer <- lupa:::.marcadores_de_ausencia
  # Desde los datos: completa, y en minusculas como los compara el catalogo.
  desde_datos <- leer(c("NA", "B", "S/D", NA), "ignorada", 2L)
  expect_true(desde_datos$completa)
  expect_setequal(desde_datos$marcadores, c("na", "s/d"))
  # Desde la evidencia: completa solo si los conteos suman el total.
  expect_true(leer(NULL, "S/D (3); N/D (2)", 5L)$completa)
  expect_false(leer(NULL, "S/D (3); N/D (2)", 6L)$completa)
  expect_false(leer(NULL, "", 3L)$completa)
  expect_identical(leer(NULL, "sin dato (4)", 4L)$marcadores, "sin dato")

  ambiguos <- lupa:::.marcadores_ambiguos
  expect_identical(ambiguos(c("na", "s/d")), "na")
  expect_identical(ambiguos(c("na1", "-")), "na1")
  expect_identical(ambiguos(c("sin dato", "s/d", "?")), character())
  # Lo declarado no cuenta como ambiguo.
  expect_identical(ambiguos(c("sd", "nc"), declarados = c("SD", "NC")), character())
})

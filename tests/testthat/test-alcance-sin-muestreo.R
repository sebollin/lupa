# Un resumen calculado sobre parte de la columna lo declara SIEMPRE, haya
# muestreo o no.
#
# La conversion de texto a numero descarta lo que no puede leer, y eso no tiene
# nada que ver con el muestreo. Pero el campo que lo declaraba nacio de un
# pendiente redactado sobre `muestra`, asi que la rama traia un `else 0L`
# literal: la MISMA columna informaba `n_valores_excluidos_resumen = 0` con
# estado `calculados` sin muestrear, y 100 con `calculados_sobre_valores` con
# `muestra = 50`. Los cien valores quedaban afuera en los dos casos; lo unico
# que cambiaba era si se decia.
#
# Lo que fija este archivo es justamente eso: que las dos corridas coincidan.
# Un test que solo mirara el caso muestreado habria pasado con el defecto vivo.

test_that("los valores que no convierten se cuentan con y sin muestreo", {
  # 900 se leen como numero; 60 tienen separadores que no convierten y 40 no son
  # numero. Ninguno es NA de origen: los 1000 estan presentes.
  valores <- c(rep("100", 900L), rep("1.234,5", 60L), rep("no-numero", 40L))
  datos <- data.frame(x = valores, stringsAsFactors = FALSE)

  completo <- perfilar(datos, muestra = Inf, analizar_dependencias = FALSE)
  muestreado <- perfilar(datos, muestra = 50L, analizar_dependencias = FALSE)

  for (perfil in list(completo, muestreado)) {
    columna <- perfil$columnas
    expect_equal(columna$n, 1000L)
    expect_equal(columna$n_faltantes, 0L)
    expect_equal(columna$n_valores_excluidos_resumen, 100L)
    expect_equal(columna$estado_resumen_cuantitativo, "calculados_sobre_valores")
    # Y la no-medicion parcial deja su fila, no solo el campo.
    expect_true("resumen_cuantitativo" %in% perfil$cobertura_diagnosticos$diagnostico)
  }

  # Las dos corridas coinciden en el alcance declarado: es la comprobacion que
  # el defecto rompia.
  expect_equal(completo$columnas$n_valores_excluidos_resumen,
               muestreado$columnas$n_valores_excluidos_resumen)
  expect_equal(completo$columnas$estado_resumen_cuantitativo,
               muestreado$columnas$estado_resumen_cuantitativo)
})

test_that("sin valores descartados el estado sigue diciendo `calculados`", {
  # El control: si no hay nada que excluir, no se degrada el estado ni se emite
  # una cobertura que no corresponde. Sin este caso, contar siempre podria
  # ensuciar toda columna sana y el test anterior no lo notaria.
  limpio <- data.frame(x = as.character(seq_len(500)), stringsAsFactors = FALSE)

  for (m in list(Inf, 50L)) {
    perfil <- perfilar(limpio, muestra = m, analizar_dependencias = FALSE)
    columna <- perfil$columnas
    expect_equal(columna$n_valores_excluidos_resumen, 0L)
    expect_equal(columna$estado_resumen_cuantitativo, "calculados")
    expect_false("resumen_cuantitativo" %in% perfil$cobertura_diagnosticos$diagnostico)
  }
})

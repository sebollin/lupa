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

# La misma regla, en la OTRA salida del paquete. El arreglo de arriba llego a
# `perfilar()` y no a `analizar_tiempo()`, que traia su propio `else 0L` en
# `.fecha_columna_avanzada()`. Encontrado el 2026-09-04 pidiendo explicitamente
# el caso que rompe, no revisando: dos salidas del mismo paquete describian la
# misma columna de dos maneras incompatibles, y la que mentia era la que NO
# muestreaba, que es la corrida por omision.
test_that("analizar_tiempo declara el descarte de parseo con y sin muestreo", {
  # 950 fechas legibles y 50 cadenas con mes 13: presentes, con forma de fecha,
  # y que ningun formato del catalogo puede validar. No son NA.
  datos <- data.frame(
    id = 1:1000L,
    f = c(rep("2024-01-01", 950L), rep("2022-13-99", 50L)),
    stringsAsFactors = FALSE
  )

  sin_muestreo <- analizar_tiempo(datos, columnas = "f", frecuencia_dias = 1)
  perfil_muestreado <- perfilar(datos, muestra = 50L, analizar_dependencias = FALSE)
  con_muestreo <- analizar_tiempo(datos, columnas = "f", frecuencia_dias = 1,
                                  perfil = perfil_muestreado)

  for (salida in list(sin_muestreo, con_muestreo)) {
    fila <- salida$resumen[salida$resumen$columna == "f", ]
    expect_equal(nrow(fila), 1L)
    expect_equal(fila$n_fechas_excluidas_parseo, 50L)
    expect_equal(fila$estado_resumen, "calculados_sobre_fechas_parseadas")
  }

  # Y las dos salidas del paquete tienen que coincidir sobre la misma columna:
  # `perfilar()` ya declaraba los 50 mientras `analizar_tiempo()` decia 0.
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "f", ]
  expect_equal(columna$n_valores_excluidos_resumen, 50L)

  # El control que hace falta para que esto pruebe algo: una columna donde NO
  # hay descarte tiene que seguir diciendo `calculados` y cero. Sin esta mitad,
  # el test pasaria tambien con un campo que informara siempre lo que descarto.
  limpia <- data.frame(
    id = 1:200L,
    f = format(as.Date("2024-01-01") + 0:199),
    stringsAsFactors = FALSE
  )
  fila_limpia <- analizar_tiempo(limpia, columnas = "f",
                                 frecuencia_dias = 1)$resumen
  expect_equal(fila_limpia$n_fechas_excluidas_parseo, 0L)
  expect_equal(fila_limpia$estado_resumen, "calculados")

  # Y un NA no es un descarte de parseo: es una ausencia declarada, y se cuenta
  # como faltante, no como valor que el resumen no pudo leer.
  con_na <- data.frame(
    id = 1:200L,
    f = c(format(as.Date("2024-01-01") + 0:189), rep(NA_character_, 10L)),
    stringsAsFactors = FALSE
  )
  fila_na <- analizar_tiempo(con_na, columnas = "f", frecuencia_dias = 1)$resumen
  expect_equal(fila_na$n_fechas_excluidas_parseo, 0L)
  expect_equal(fila_na$estado_resumen, "calculados")
})

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

# Y la forma mas cara del mismo error: no declarar de menos, sino **desaparecer**.
# Bastaba un formato de mes confirmado para que `analizar_tiempo()` abandonara la
# columna entera: no salia fila, no salia en `columnas_omitidas`, y
# `columnas_analizadas` seguia nombrandola. `perfilar()` sobre esa misma columna
# la resume bien y `man/perfilar.Rd` declara ese trato para las columnas mixtas.
test_that("analizar_tiempo resume la columna mixta de dias y meses", {
  # 900 presentes: 700 con dia, 100 periodos de mes, 100 que ningun formato lee.
  datos <- data.frame(
    f = c(rep("2024-01-15", 700L), rep("2024-02", 100L), rep("2022-13-99", 100L)),
    stringsAsFactors = FALSE
  )

  temporal <- analizar_tiempo(datos, columnas = "f", frecuencia_dias = 1)
  fila <- temporal$resumen[temporal$resumen$columna == "f", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_presentes, 700L)
  expect_equal(fila$n_fechas_excluidas_parseo, 200L)
  expect_equal(fila$n_fechas_excluidas_granularidad, 100L)
  expect_equal(fila$estado_resumen, "calculados_sobre_dias")
  expect_equal(attr(temporal, "columnas_analizadas"), "f")

  # Las dos salidas del paquete, sobre la misma columna, tienen que coincidir.
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  columna <- perfil$columnas[perfil$columnas$columna == "f", ]
  expect_equal(columna$n_fechas_resumidas, fila$n_presentes)
  expect_equal(columna$n_fechas_excluidas_granularidad,
               fila$n_fechas_excluidas_granularidad)
  expect_equal(columna$n_valores_excluidos_resumen,
               fila$n_fechas_excluidas_parseo)
  expect_equal(columna$estado_resumen_cuantitativo, fila$estado_resumen)

  # Control 1: sin ninguna fecha con dia no hay serie diaria que construir, y
  # entonces la columna se DECLARA omitida en vez de desaparecer.
  solo_meses <- data.frame(
    f = rep(c("2024-01", "2024-02", "2024-03"), 100L), stringsAsFactors = FALSE
  )
  sin_serie <- analizar_tiempo(solo_meses, columnas = "f", frecuencia_dias = 1)
  expect_equal(nrow(sin_serie$resumen), 0L)
  expect_equal(attr(sin_serie, "columnas_analizadas"), character())
  expect_true("f" %in% attr(sin_serie, "columnas_omitidas"))
  expect_true("f" %in% attr(sin_serie, "columnas_sin_serie_diaria"))

  # Control 2: una columna sin nada que excluir sigue diciendo `calculados` y
  # cero en los dos campos. Sin esta mitad, el test pasaria con un campo que
  # informara siempre algo.
  limpia <- data.frame(
    f = format(as.Date("2024-01-01") + 0:199), stringsAsFactors = FALSE
  )
  fila_limpia <- analizar_tiempo(limpia, columnas = "f",
                                 frecuencia_dias = 1)$resumen
  expect_equal(fila_limpia$n_fechas_excluidas_parseo, 0L)
  expect_equal(fila_limpia$n_fechas_excluidas_granularidad, 0L)
  expect_equal(fila_limpia$estado_resumen, "calculados")
})

# El quinto caso de la regla, encontrado repitiendo el MISMO encargo sobre el
# paquete ya arreglado: los blancos.
#
# Un blanco -`" "`, `""`, un tabulador- es un valor PRESENTE que no llega a
# numero ni a fecha. El paquete no lo trata como ausencia declarada: lo cuenta
# en `n_blancos`, lo sospecha en `n_faltantes_disfrazados` y deja `n_faltantes`
# en cero. Pero `trimws()` lo dejaba en `""` y el `nzchar()` lo descartaba de la
# cuenta de excluidos, asi que la media se publicaba sobre 900 de 1000 filas con
# `estado = "calculados"` y `n_valores_excluidos_resumen = 0`.
test_that("un blanco es un valor presente que no entro, y se declara", {
  formas <- list(espacio = " ", vacia = "", tabulador = "\t",
                 ilegible = "no-numero")
  for (nombre in names(formas)) {
    datos <- data.frame(
      x = c(rep("10", 900L), rep(formas[[nombre]], 100L)),
      stringsAsFactors = FALSE
    )
    fila <- perfilar(datos, analizar_dependencias = FALSE)$columnas
    # Primera mitad: el paquete NO los cuenta como ausencia declarada, que es
    # lo que hace que su exclusion tenga que declararse.
    expect_equal(fila$n_faltantes, 0L, info = nombre)
    expect_equal(fila$n_valores_excluidos_resumen, 100L, info = nombre)
    expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_valores",
                 info = nombre)
  }

  # Control 1: `NA` SI es ausencia declarada. Se informa como faltante y no
  # como excluido del resumen; son dos cosas distintas y no se confunden.
  con_na <- data.frame(
    x = c(rep("10", 900L), rep(NA_character_, 100L)), stringsAsFactors = FALSE
  )
  fila_na <- perfilar(con_na, analizar_dependencias = FALSE)$columnas
  expect_equal(fila_na$n_faltantes, 100L)
  expect_equal(fila_na$n_valores_excluidos_resumen, 0L)
  expect_equal(fila_na$estado_resumen_cuantitativo, "calculados")

  # Control 2: sin nada que excluir, nada se declara.
  limpia <- data.frame(x = rep(c("10", "20"), 500L), stringsAsFactors = FALSE)
  fila_limpia <- perfilar(limpia, analizar_dependencias = FALSE)$columnas
  expect_equal(fila_limpia$n_valores_excluidos_resumen, 0L)
  expect_equal(fila_limpia$estado_resumen_cuantitativo, "calculados")
})

test_that("en fechas, el estado nombra la razon por la que quedo algo afuera", {
  # `calculados_sobre_dias` nombra una razon concreta -quedaron afuera periodos
  # de mes-. Cuando lo que quedo afuera son valores ilegibles o en blanco, la
  # razon es otra, y antes las dos decian lo mismo: quien lo leyera buscaba una
  # granularidad que no existia.
  medir <- function(relleno) {
    datos <- data.frame(
      f = c(rep("2024-01-15", 900L), rep(relleno, 100L)),
      stringsAsFactors = FALSE
    )
    perfilar(datos, analizar_dependencias = FALSE)$columnas
  }

  por_mes <- medir("2024-02")
  expect_equal(por_mes$n_fechas_excluidas_granularidad, 100L)
  expect_equal(por_mes$estado_resumen_cuantitativo, "calculados_sobre_dias")

  for (relleno in c(" ", "no-fecha")) {
    fila <- medir(relleno)
    expect_equal(fila$n_fechas_excluidas_granularidad, 0L, info = relleno)
    expect_equal(fila$n_valores_excluidos_resumen, 100L, info = relleno)
    expect_equal(fila$estado_resumen_cuantitativo, "calculados_sobre_valores",
                 info = relleno)
  }

  # Control: una columna de fechas sin nada afuera no declara nada.
  limpia <- medir("2024-06-01")
  expect_equal(limpia$n_valores_excluidos_resumen, 0L)
  expect_equal(limpia$estado_resumen_cuantitativo, "calculados")
})

# Y el escalon siguiente: que el propio hallazgo respete el alcance.
#
# Declarar el alcance en `n_valores_excluidos_resumen` y en
# `cobertura_diagnosticos` no alcanza si el hallazgo publica un denominador que
# lo ignora: nadie que lee una fila de `hallazgos` esta obligado a cruzarla con
# otra tabla. Los diagnosticos que se calculan sobre el resumen cuantitativo
# publicaban `n_evaluados` = la columna entera.
test_that("los hallazgos del resumen cuantitativo cuentan sobre lo que entro", {
  utiles <- c(rep("0", 50L), rep("-5", 50L), rep("100", 800L))
  con_exclusiones <- data.frame(
    x = c(utiles, rep("no-numero", 100L)), stringsAsFactors = FALSE
  )
  perfil <- perfilar(con_exclusiones, columnas_sin_ceros = "x",
                     columnas_no_negativas = "x", analizar_dependencias = FALSE)

  # Primera mitad: hay exclusiones de verdad y estan declaradas.
  expect_equal(perfil$columnas$n, 1000L)
  expect_equal(perfil$columnas$n_valores_excluidos_resumen, 100L)

  sobre_resumen <- c("outliers", "ceros_no_permitidos",
                     "negativos_no_permitidos")
  filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo %in% sobre_resumen, , drop = FALSE
  ]
  expect_true(nrow(filas) >= 1L)
  # 1000 filas menos 100 que no convierten: 900 valores evaluados.
  expect_true(all(filas$n_evaluados == 900))

  # Control 1: un diagnostico que cuenta FILAS y no valores convertidos conserva
  # la columna entera como denominador. Sin esta mitad, bajar el denominador de
  # todo pasaria el test.
  por_filas <- perfil$hallazgos[
    perfil$hallazgos$tipo_hallazgo == "filas_duplicadas", , drop = FALSE
  ]
  if (nrow(por_filas)) expect_true(all(por_filas$n_evaluados == 1000))

  # Control 2: la misma columna sin nada que excluir no cambia de denominador.
  sin_exclusiones <- data.frame(
    x = c(utiles, rep("100", 100L)), stringsAsFactors = FALSE
  )
  perfil_limpio <- perfilar(sin_exclusiones, columnas_sin_ceros = "x",
                            columnas_no_negativas = "x",
                            analizar_dependencias = FALSE)
  expect_equal(perfil_limpio$columnas$n_valores_excluidos_resumen, 0L)
  filas_limpias <- perfil_limpio$hallazgos[
    perfil_limpio$hallazgos$tipo_hallazgo %in% sobre_resumen, , drop = FALSE
  ]
  expect_true(all(filas_limpias$n_evaluados == 1000))
})

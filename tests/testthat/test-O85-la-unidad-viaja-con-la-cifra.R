# La unidad tiene que viajar al lado de la cifra que califica, y compararse.
#
# Sin el campo, cuatro columnas medidas en unidades DISTINTAS publicaban la misma
# fila -`media = 2`, `desvio = 1`- y un `Period` de dos dias publicaba `172800`
# sin decir que eran segundos. Y el campo solo no alcanzaba: el enunciado del
# defecto era "una entrega donde cambio la unidad no muestra cambio", asi que la
# comparacion de perfiles tambien tiene que verlo.
#
# Buscar el precedente propio destapo dos defectos en el precedente:
# `clasificar_variables()` publicaba `km` para una columna en `km/h` -una unidad
# equivocada, no ausente- y ABORTABA con `m^2`, porque el numerador de un
# `symbolic_units` es un vector.

test_that("la unidad declarada sale canonica y de una sola pieza", {
  skip_if_not_installed("units")
  esperado <- c(
    kg = "kg", "km/h" = "km/h", "m^2" = "m^2", "kg*m/s^2" = "kg*m/s^2",
    "1/s" = "1/s", "m/s^2" = "m/s^2"
  )
  for (declarada in names(esperado)) {
    columna <- units::set_units(c(1, 2, 3), declarada, mode = "standard")
    obtenida <- lupa:::.unidad_declarada(columna)
    expect_length(obtenida, 1L)
    expect_equal(
      obtenida, unname(esperado[[declarada]]),
      info = paste("la unidad", declarada, "no se publica canonica")
    )
  }
  # El defecto exacto: `[[1L]]` sobre el atributo devolvia el numerador, que en
  # `km/h` es `km` y en `m^2` son DOS valores.
  atributo <- attr(
    units::set_units(1, "m^2", mode = "standard"), "units", exact = TRUE
  )
  expect_length(atributo[[1L]], 2L)
  expect_length(lupa:::.unidad_declarada(
    units::set_units(1, "m^2", mode = "standard")
  ), 1L)
})

test_that("clasificar_variables publica la unidad entera y no aborta", {
  skip_if_not_installed("units")
  datos <- data.frame(id = 1:3)
  datos$velocidad <- units::set_units(c(10, 20, 30), "km/h", mode = "standard")
  datos$area <- units::set_units(c(1, 2, 3), "m^2", mode = "standard")
  datos$fuerza <- units::set_units(c(1, 2, 3), "kg*m/s^2", mode = "standard")
  # Con `m^2` esto moria con "replacement has 2 rows, data has 3", asi que la
  # funcion entera no devolvia nada: ni las columnas que si podia clasificar.
  clasificadas <- clasificar_variables(datos, proteger_datos_personales = FALSE)
  expect_equal(nrow(clasificadas), 4L)
  unidades <- clasificadas$unidad[match(
    c("velocidad", "area", "fuerza", "id"), clasificadas$columna
  )]
  expect_equal(unidades[[1L]], "km/h")
  expect_equal(unidades[[2L]], "m^2")
  expect_equal(unidades[[3L]], "kg*m/s^2")
  expect_true(is.na(unidades[[4L]]))
  # Y no publica `km` para una columna en `km/h`: esa era la unidad equivocada.
  expect_false(any(clasificadas$unidad %in% "km", na.rm = TRUE))
})

test_that("la tabla de columnas dice en que unidad estan sus cifras", {
  skip_if_not_installed("units")
  datos <- data.frame(id = 1:3)
  datos$kg <- units::set_units(c(1, 2, 3), "kg", mode = "standard")
  datos$kmh <- units::set_units(c(1, 2, 3), "km/h", mode = "standard")
  datos$m2 <- units::set_units(c(1, 2, 3), "m^2", mode = "standard")
  datos$dias <- as.difftime(c(1, 2, 3), units = "days")
  datos$fecha <- as.Date("2026-01-01") + c(0, 1, 2)
  datos$hora <- as.POSIXct("2026-01-01", tz = "UTC") + c(0, 3600, 7200)
  datos$texto <- c("a", "b", "c")
  perfil <- perfilar(datos)
  columnas <- as.data.frame(perfil$columnas)
  unidad_de <- function(nombre) {
    columnas$unidad[[which(columnas$columna == nombre)]]
  }

  # Las tres `units` publican `media = 2` y `desvio = 1`: sin este campo eran
  # filas identicas para tres magnitudes distintas.
  expect_equal(
    unique(columnas$media[columnas$columna %in% c("kg", "kmh", "m2")]), 2
  )
  expect_equal(unidad_de("kg"), "kg")
  expect_equal(unidad_de("kmh"), "km/h")
  expect_equal(unidad_de("m2"), "m^2")
  # `difftime` PUBLICA sus estadisticos en su unidad declarada. Se abstenia
  # mientras no habia donde publicar la unidad, y era el unico que lo hacia:
  # `Date` y `POSIXt` publican `desvio` en segundos desde antes. Esta afirmacion
  # decia lo contrario cuando se escribio, y cambio por decision del 2026-09-29.
  expect_equal(columnas$media[[which(columnas$columna == "dias")]], 2)
  expect_equal(columnas$minimo[[which(columnas$columna == "dias")]], 1)
  expect_equal(columnas$maximo[[which(columnas$columna == "dias")]], 3)
  expect_equal(unidad_de("dias"), "days")
  expect_equal(
    as.character(
      columnas$estado_resumen_cuantitativo[[which(columnas$columna == "dias")]]
    ),
    "calculados"
  )
  # Fecha y fecha-hora publican `desvio` en segundos, que hasta ahora solo estaba
  # dicho en prosa en la documentacion.
  expect_equal(unidad_de("fecha"), "segundos")
  expect_equal(unidad_de("hora"), "segundos")
  # Y donde no hay unidad, el campo no inventa una.
  expect_true(is.na(unidad_de("texto")))
  expect_true(is.na(unidad_de("id")))
})

test_that("difftime publica en la unidad que declara, no en segundos", {
  # Convertir a segundos publicaria un numero que no esta en la columna. Las dos
  # columnas guardan los mismos instantes y declaran unidades distintas: las
  # cifras tienen que salir distintas y cada una con su unidad.
  datos <- data.frame(
    dias = as.difftime(c(1, 2, 3, 10), units = "days"),
    minutos = as.difftime(c(30, 60, 90, 120), units = "mins")
  )
  columnas <- as.data.frame(perfilar(datos)$columnas)
  fila <- function(nombre) columnas[columnas$columna == nombre, , drop = FALSE]
  expect_equal(fila("dias")$media, mean(c(1, 2, 3, 10)))
  expect_equal(fila("dias")$desvio, stats::sd(c(1, 2, 3, 10)))
  expect_equal(fila("dias")$unidad, "days")
  expect_equal(fila("minutos")$media, mean(c(30, 60, 90, 120)))
  expect_equal(fila("minutos")$unidad, "mins")
  # Y el tipo declarado sigue diciendo que es una duracion: el resumen se publica
  # sin disfrazar la columna de numero pelado.
  expect_equal(unique(as.character(columnas$tipo_declarado[
    columnas$columna %in% c("dias", "minutos")
  ])), "difftime")
})

test_that("la regla de la unidad del resumen cubre las clases que publican segundos", {
  # `Period` y `Duration` se ejercitan con un objeto de esa clase y no con
  # `lubridate`, que no esta en `Suggests`: usar un paquete sin declararlo pasa la
  # suite y rompe el check. Lo que la regla mira es la clase, que es justo lo que
  # se le pasa. El camino de punta a punta con las clases base -`Date` y
  # `POSIXt`- lo cubre la prueba de arriba.
  expect_equal(
    lupa:::.unidad_del_resumen(structure(86400, class = "Period")), "segundos"
  )
  expect_equal(
    lupa:::.unidad_del_resumen(structure(86400, class = "Duration")), "segundos"
  )
  expect_equal(
    lupa:::.unidad_del_resumen(as.difftime(1, units = "hours")), "hours"
  )
  expect_true(is.na(lupa:::.unidad_del_resumen(c(1, 2, 3))))
  expect_true(is.na(lupa:::.unidad_del_resumen(letters)))
})

test_that("mas de una unidad declarada se publica entera, no la primera", {
  # Un atributo `units` de largo mayor que uno no es una unidad. Quedarse con la
  # primera es el mismo defecto que publicaba `km` por `km/h`: elegir un valor y
  # callar el resto.
  columna <- structure(c(1, 2, 3), units = c("kg", "g"))
  publicada <- lupa:::.unidad_declarada(columna)
  expect_length(publicada, 1L)
  expect_match(publicada, "kg")
  expect_match(publicada, "g")
})

test_that("la comparacion de perfiles ve el cambio de unidad", {
  skip_if_not_installed("units")
  en_metros <- data.frame(id = 1:3)
  en_metros$v <- units::set_units(c(1, 2, 3), "m", mode = "standard")
  en_kilometros <- data.frame(id = 1:3)
  en_kilometros$v <- units::set_units(c(1, 2, 3), "km", mode = "standard")
  anterior <- perfilar(en_metros, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  actual <- perfilar(
    en_kilometros, fecha = as.POSIXct("2026-02-01", tz = "UTC")
  )
  # Los valores son los MISMOS: lo unico que cambio es la unidad. Sin el aspecto,
  # esta comparacion devolvia cero filas, que es el enunciado del defecto.
  deriva <- comparar_perfiles(anterior, actual)
  fila <- as.data.frame(deriva)[
    as.character(deriva$aspecto) == "unidad", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$columna), "v")
  expect_equal(as.character(fila$valor_anterior), "m")
  expect_equal(as.character(fila$valor_actual), "km")
  # La severidad es la misma que un cambio de tipo, por el mismo motivo: la cifra
  # deja de significar lo que significaba.
  expect_equal(as.character(fila$severidad), "error")

  # La otra mitad: sin cambio de unidad no aparece ninguna fila. Un aspecto que
  # informara siempre no se distinguiria de uno que no mide.
  igual <- perfilar(en_metros, fecha = as.POSIXct("2026-03-01", tz = "UTC"))
  sin_cambio <- comparar_perfiles(anterior, igual)
  expect_equal(
    sum(as.character(sin_cambio$aspecto) == "unidad"), 0L
  )
})

test_that("una entrega sin el campo declara que no se puede comparar", {
  skip_if_not_installed("units")
  en_metros <- data.frame(id = 1:3)
  en_metros$v <- units::set_units(c(1, 2, 3), "m", mode = "standard")
  anterior <- perfilar(en_metros, fecha = as.POSIXct("2026-01-01", tz = "UTC"))
  actual <- perfilar(en_metros, fecha = as.POSIXct("2026-02-01", tz = "UTC"))
  # Un perfil guardado antes de que el campo existiera no puede hacer que la
  # comparacion mienta ni que aborte: se declara la no comparabilidad.
  viejo <- anterior
  viejo$columnas$unidad <- NULL
  deriva <- comparar_perfiles(viejo, actual)
  fila <- as.data.frame(deriva)[
    as.character(deriva$aspecto) == "unidad", , drop = FALSE
  ]
  expect_equal(nrow(fila), 1L)
  expect_match(as.character(fila$descripcion), "no se compara")
})

test_that("una columna compuesta no publica unidad, aunque la declare", {
  # El campo se calla cuando la fila no publica cifras, y la rama de columnas
  # compuestas escribe su propio estado -`tipo_compuesto_no_analizado`- asi que
  # esquivaba el discriminador, que miraba solo `no_aplica`. Medido: una matriz
  # con `attr(m, "units") <- "kg"` publicaba `unidad = "kg"` junto a `media = NA`,
  # que es la forma exacta que el arreglo del `factor` habia venido a cerrar.
  matriz <- matrix(1:12, nrow = 4)
  attr(matriz, "units") <- "kg"
  lista <- I(list(1, 2, 3, 4))
  attr(lista, "units") <- "kg"
  datos <- data.frame(id = 1:4)
  datos$mat <- I(matriz)
  datos$lst <- lista
  columnas <- as.data.frame(
    perfilar(datos, analizar_dependencias = FALSE)$columnas
  )
  fila <- function(nombre) columnas[columnas$columna == nombre, , drop = FALSE]

  # Los dos estados que significan "esta clase no produce resumen", los dos
  # callados. La primera version del arreglo cubria uno solo.
  expect_equal(
    as.character(fila("mat")$estado_resumen_cuantitativo),
    "tipo_compuesto_no_analizado"
  )
  expect_true(is.na(fila("mat")$unidad))
  expect_equal(
    as.character(fila("lst")$estado_resumen_cuantitativo), "no_aplica"
  )
  expect_true(is.na(fila("lst")$unidad))
  # Y los dos estados estan nombrados en un solo lugar.
  expect_setequal(
    lupa:::.ESTADOS_SIN_RESUMEN_CUANTITATIVO,
    c("no_aplica", "tipo_compuesto_no_analizado")
  )
})

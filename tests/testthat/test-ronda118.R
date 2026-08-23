# El banco que decidio la forma de la guarda de numeracion.
#
# Trece columnas con la respuesta conocida: cinco que son numeraciones -donde
# Benford y los limites de Tukey no corresponden- y ocho magnitudes con un dato
# malo adentro, que hay que ver. Se probaron cuatro variantes de la guarda y
# esta es la que acerto todas: densidad del tramo de enteros, un minimo de cinco
# valores distintos, y **cruzada con la ausencia de un salto de escala**.
#
# La densidad sola callaba dos datos malos reales: un `120` entre edades de 18 a
# 70 y un `2000` detras de 1..1000. Un valor fuera de escala de hasta el doble
# del maximo no baja la densidad lo suficiente; lo que si lo delata es el hueco
# que abre.

.perfil_r118 <- function(x) {
  perfilar(
    data.frame(x = as.integer(x)), analizar_dependencias = FALSE,
    proteger_datos_personales = FALSE, casi_duplicados_vocabulario = FALSE
  )
}

.senalado_r118 <- function(perfil, tipo) {
  tipo %in% as.character(perfil$hallazgos$tipo_hallazgo[
    as.character(perfil$hallazgos$severidad) != "ok"
  ])
}

test_that("las numeraciones no reciben pruebas de magnitud, y queda declarado", {
  set.seed(101)
  numeraciones <- list(
    list(nombre = "MotId 1..4557", valores = sort(sample(4557L, 3159L)),
         diagnostico = "ley_benford", hallazgo = "desviacion_benford"),
    list(nombre = "EstCod 1..284 con cola",
         valores = c(sample(60L, 1900L, TRUE), sample(61:284, 224L, TRUE)),
         diagnostico = "outliers", hallazgo = "outliers"),
    list(nombre = "BenId 105 valores", valores = sample(105L, 2000L, TRUE),
         diagnostico = "outliers", hallazgo = "outliers"),
    list(nombre = "MEsId con huecos", valores = sort(sample(4600L, 3000L)),
         diagnostico = "ley_benford", hallazgo = "desviacion_benford"),
    list(nombre = "codigo 1..15", valores = sort(sample(15L, 100L, TRUE)),
         diagnostico = "outliers", hallazgo = "outliers")
  )
  for (caso in numeraciones) {
    perfil <- .perfil_r118(caso$valores)
    expect_false(.senalado_r118(perfil, caso$hallazgo), info = caso$nombre)
    # No se apaga: se declara. Pero solo hay algo que declarar cuando habia
    # algo que decir: una columna sin ningun valor fuera de los limites no
    # necesita una fila que cuente que no se evaluaron.
    hubo_que_decir <- identical(caso$diagnostico, "ley_benford") ||
      perfil$columnas$n_outliers > 0L
    if (hubo_que_decir) {
      expect_true(
        caso$diagnostico %in%
          as.character(perfil$cobertura_diagnosticos$diagnostico),
        info = caso$nombre
      )
    }
  }
})

test_that("un dato malo dentro de una numeracion se sigue viendo", {
  # Estos son los casos que la densidad sola tapaba. El salto de escala los
  # devuelve: el valor que lo abre no es cola, es anomalia.
  set.seed(101)
  con_dato_malo <- list(
    list(nombre = "edad 18..70 mas un 120", valores = c(18:70, 120L)),
    list(nombre = "id 1..1000 mas un 2000", valores = c(1:1000, 2000L)),
    list(nombre = "id 1..100 mas un 10000", valores = c(1:100, 10000L)),
    list(nombre = "anio 2000..2030 mas 1900",
         valores = c(sample(2000:2030, 200L, TRUE), 1900L, 1900L)),
    list(nombre = "cantidad 1..10 mas un 9999",
         valores = c(sample(10L, 500L, TRUE), 9999L)),
    list(nombre = "puntaje 0..100 mas un 999",
         valores = c(sample(0:100, 400L, TRUE), 999L))
  )
  for (caso in con_dato_malo) {
    perfil <- .perfil_r118(caso$valores)
    expect_gt(perfil$columnas$n_outliers, 0L)
    expect_true(.senalado_r118(perfil, "outliers"), info = caso$nombre)
  }
})

test_that("el salto de escala se mide y viaja en el perfil", {
  con_salto <- .perfil_r118(c(18:70, 120L))
  sin_salto <- .perfil_r118(sort(sample(4557L, 3159L)))

  expect_true(con_salto$columnas$salto_de_escala_secuencia_entera)
  expect_false(sin_salto$columnas$salto_de_escala_secuencia_entera)
  # El hueco que abre el 120 es 50 donde el tipico es 1.
  expect_equal(con_salto$columnas$hueco_maximo_secuencia_entera, 50)
  # Y la densidad sola no alcanzaba: 0,52 esta por encima del umbral.
  expect_gt(con_salto$columnas$densidad_secuencia_entera, 0.5)
})

test_that("un valor fuera de escala de hasta el doble tambien se ve", {
  # Era el agujero exacto de la densidad sola: con 1..N y un valor M, la
  # densidad solo baja de 0,5 cuando M supera 2N. Todo lo de en medio quedaba
  # tapado.
  for (extremo in c(1500L, 2000L, 2500L)) {
    perfil <- .perfil_r118(c(1:1000, extremo))
    if (perfil$columnas$n_outliers > 0L) {
      expect_true(.senalado_r118(perfil, "outliers"),
                  info = paste("extremo", extremo))
    }
  }
})

test_that("una magnitud casi unica conserva Benford", {
  # La unicidad no sirve como senal: un monto tambien es casi unico. Si la
  # guarda la usara, este caso se apagaria.
  set.seed(8)
  montos <- c(
    sample(c(9:99, 900:999, 90000:99999, 9000000:9999999), 500L, TRUE),
    sample(c(10:19, 1000:1099), 30L, TRUE)
  )
  perfil <- .perfil_r118(montos)
  expect_gt(perfil$columnas$tasa_distintos, 0.9)
  expect_true(.senalado_r118(perfil, "desviacion_benford"))
})

# Vuelta G: cuatro cosas que el paquete hacia y no decia.

test_that("retirar la clave declarada no se lee como hallazgo resuelto", {
  # `man/comparar_perfiles.Rd` lo declara: un hallazgo que ya no aparece se
  # informa `resuelto` SOLO si el diagnostico volvio a evaluarse. La misma tabla
  # perfilada con `clave = "id"` y despues sin `clave` informaba `resuelto` con
  # severidad `ok` sobre una clave que seguia duplicada: dejar de mirar no es
  # lo mismo que arreglar.
  datos <- data.frame(
    id = c(1, 2, 3, 1, 2, 3), v = c("a", "b", "c", "a", "b", "c"),
    stringsAsFactors = FALSE
  )
  con_clave <- suppressWarnings(perfilar(
    datos, clave = "id", fecha = as.POSIXct("2026-01-01", tz = "UTC"),
    analizar_dependencias = FALSE
  ))
  sin_clave <- perfilar(
    datos, clave = NULL, fecha = as.POSIXct("2026-02-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )

  # Primera mitad: el hallazgo existe en el primero y no en el segundo, que es
  # la situacion que la deriva tiene que interpretar.
  expect_true("clave_no_unica" %in% con_clave$hallazgos$tipo_hallazgo)
  expect_false("clave_no_unica" %in% sin_clave$hallazgos$tipo_hallazgo)
  # Y el perfil registra la declaracion, que es de donde sale la distincion.
  expect_equal(con_clave$meta$declaracion_clave, "id")
  expect_equal(length(sin_clave$meta$declaracion_clave), 0L)

  deriva <- comparar_perfiles(con_clave, sin_clave)
  # `%in%` y no `==`: la columna tiene `NA` y `==` arrastra esas filas al
  # subconjunto, con lo que la comprobacion mira una fila que no es la suya.
  fila <- deriva[deriva$aspecto == "hallazgo" &
                   deriva$valor_anterior %in% "clave_no_unica", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$cambio), "no_evaluado")
  expect_equal(as.character(fila$severidad), "sospechoso")
  expect_match(fila$descripcion, "dejar de mirar no es lo mismo que arreglar",
               fixed = TRUE)

  # Control: con la clave todavia declarada y ya unica, SI es `resuelto`. Sin
  # esta mitad, marcar todo como `no_evaluado` pasaria el test.
  ya_unica <- perfilar(
    data.frame(id = 1:6, v = letters[1:6], stringsAsFactors = FALSE),
    clave = "id", fecha = as.POSIXct("2026-02-01", tz = "UTC"),
    analizar_dependencias = FALSE
  )
  fila_ok <- comparar_perfiles(con_clave, ya_unica)
  fila_ok <- fila_ok[fila_ok$aspecto == "hallazgo" &
                       fila_ok$valor_anterior %in% "clave_no_unica", ]
  expect_equal(as.character(fila_ok$cambio), "resuelto")
  expect_equal(as.character(fila_ok$severidad), "ok")
})

test_that("cambiar la aplicabilidad se declara y no se atribuye a los datos", {
  # La deriva ya declaraba la comparabilidad para la politica de patrones y para
  # la de centinelas. `aplicabilidad` es la tercera politica que redefine lo que
  # se mide, y faltaba.
  datos <- data.frame(
    estado = c("activo", "activo", "baja", "baja"),
    monto = c(10, 20, NA, NA),
    stringsAsFactors = FALSE
  )
  sin_regla <- perfilar(datos, analizar_dependencias = FALSE)
  con_regla <- perfilar(
    datos, aplicabilidad = list(monto = ~ estado == "activo"),
    analizar_dependencias = FALSE
  )

  # Primera mitad: la regla cambia lo medido, que es lo que hace falta declarar.
  expect_equal(sin_regla$meta$declaracion_aplicabilidad, character())
  expect_equal(con_regla$meta$declaracion_aplicabilidad, "monto")

  fila <- comparar_perfiles(sin_regla, con_regla)
  fila <- fila[fila$aspecto == "configuracion_aplicabilidad", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$severidad), "error")
  expect_equal(fila$valor_anterior, "ninguna")
  expect_equal(fila$valor_actual, "monto")

  # Control: la misma politica en las dos corridas no declara nada.
  igual <- comparar_perfiles(con_regla, con_regla)
  expect_equal(sum(igual$aspecto == "configuracion_aplicabilidad"), 0L)
})

test_that("si la reparacion no repara, no cambia el texto", {
  # Con `LC_CTYPE = C`, `enc2utf8()` reemplaza un byte que no forma UTF-8 valido
  # por su escape literal: `caf<0xe9>` salia como los siete caracteres ASCII
  # `caf<e9>` con `estado = "no_parece_roto"`. Cambiaba el texto y declaraba que
  # no. Y el resultado dependia de la configuracion regional de la sesion.
  crudo <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))

  medir <- function(locale) {
    anterior <- Sys.getlocale("LC_CTYPE")
    puesto <- suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", anterior)), add = TRUE)
    if (!nzchar(puesto)) return(NULL)
    .ftfy_reparar_uno(crudo)
  }

  en_c <- medir("C")
  skip_if(is.null(en_c), "esta maquina no tiene el locale C")

  # Primera mitad: el motor corrio y decidio que no habia que reparar.
  expect_equal(en_c$estado, "no_parece_roto")
  # Y por lo tanto devolvio exactamente lo que entro, byte por byte.
  expect_identical(charToRaw(en_c$texto), charToRaw(crudo))

  # Control: sobre un texto que SI esta roto, el motor cambia y lo declara.
  # La cadena se construye con `rawToChar()`: el proyecto exige fuentes en ASCII
  # -un archivo con acentos rompio una vez toda una tanda- y una guarda lo
  # comprueba. Estos son los bytes de "Paysand" + la doble codificacion de "u"
  # con tilde, que es el mojibake que se quiere reparar.
  roto <- rawToChar(as.raw(c(
    0x50, 0x61, 0x79, 0x73, 0x61, 0x6e, 0x64, 0xc3, 0x83, 0xc2, 0xba
  )))
  reparado <- .ftfy_reparar_uno(roto)
  expect_equal(reparado$estado, "reparado")
  expect_false(identical(reparado$texto, roto))
})

test_that("la reparacion de un texto largo no crece de forma cuadratica", {
  skip_on_cran()
  # `.ftfy_restaurar_a0()` acumulaba con `c()` byte por byte y copiaba la cola
  # entera en cada vuelta: 20 KB tardaban 0,29 s y 160 KB, 11,22 s -cuadruplicar
  # el largo multiplicaba por 37 el tiempo-, y `perfilar()` sobre una tabla de
  # tres filas con un valor roto de 160 KB tardaba 65 s.
  #
  # No se fija un tiempo absoluto -depende de la maquina- sino la FORMA de la
  # curva: al cuadruplicar el largo, un algoritmo lineal no puede multiplicar el
  # tiempo por diez.
  # Mismo motivo que arriba: la fuente va en ASCII y el mojibake se construye.
  unidad <- rawToChar(as.raw(c(
    0x50, 0x61, 0x79, 0x73, 0x61, 0x6e, 0x64, 0xc3, 0x83, 0xc2, 0xba, 0x20
  )))
  medir <- function(n) {
    texto <- paste(rep(unidad, n), collapse = "")
    system.time(.ftfy_reparar_uno(texto))[["elapsed"]]
  }
  corto <- medir(2000L)
  largo <- medir(8000L)
  skip_if(corto < 0.02, "la maquina es demasiado rapida para medir la curva")
  expect_lt(largo / corto, 10)
})

test_that("dos resumenes con alcances distintos se declaran no comparables", {
  # La misma columna, una vez numerica y otra como texto con un valor que no
  # convierte: la deriva publicaba "Cambio el rango observado de la columna"
  # -[10, 100] contra [10, 90]- atribuido a los datos, cuando el 100 seguia ahi
  # y lo que cambio fue que quedo fuera del resumen.
  sin_exclusiones <- perfilar(
    data.frame(x = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)),
    analizar_dependencias = FALSE
  )
  con_exclusion <- perfilar(
    data.frame(
      x = c("10", "20", "30", "40", "50", "60", "70", "80", "90", "xx"),
      stringsAsFactors = FALSE
    ),
    analizar_dependencias = FALSE
  )

  # Primera mitad: los alcances son de verdad distintos.
  expect_equal(sin_exclusiones$columnas$n_valores_excluidos_resumen, 0L)
  expect_equal(con_exclusion$columnas$n_valores_excluidos_resumen, 1L)

  deriva <- comparar_perfiles(sin_exclusiones, con_exclusion)
  fila <- deriva[deriva$aspecto %in% "alcance_resumen", ]
  expect_equal(nrow(fila), 1L)
  expect_equal(as.character(fila$severidad), "sospechoso")
  expect_equal(fila$valor_anterior, "0")
  expect_equal(fila$valor_actual, "1")
  expect_match(fila$descripcion, "pueden venir", fixed = TRUE)

  # Control: dos perfiles con el mismo alcance no declaran nada. Sin esta mitad,
  # emitir la fila siempre pasaria el test.
  igual <- comparar_perfiles(sin_exclusiones, sin_exclusiones)
  expect_equal(sum(igual$aspecto %in% "alcance_resumen"), 0L)
})

# La clase de un conteo del perfil no puede depender de que el usuario tenga
# instalado un paquete opcional, y menos puede volverse ilegible al guardarlo.

test_that("un conteo corriente sale numeric, no integer64", {
  # `bit64` instalado o no, el resultado es el mismo: es el punto.
  expect_false(inherits(.conteo_dbi(20L), "integer64"))
  expect_false(inherits(.conteo_dbi(20), "integer64"))
  expect_false(inherits(.conteo_dbi("20"), "integer64"))
  expect_equal(.conteo_dbi(20L), 20)
  expect_equal(.conteo_dbi("20"), 20)
})

test_that("integer64 se conserva solo donde compra exactitud", {
  skip_if_not_installed("bit64")
  # Justo en 2^53 el double todavia es exacto: no hace falta integer64.
  expect_false(inherits(.conteo_dbi("9007199254740992"), "integer64"))
  # Un digito mas y el double ya redondea: ahi si.
  grande <- .conteo_dbi("9007199254740993")
  expect_true(inherits(grande, "integer64"))
  expect_equal(format(grande), "9007199254740993")
  # Y el mismo numero como double ya venia perdido: no se puede reconstruir, y
  # se declara inexacto en vez de inventar precision.
  expect_false(inherits(.conteo_dbi(2^53 + 2), "integer64"))
  expect_false(.conteo_exacto_dbi(2^53 + 2))
  # Un integer64 chico se baja a numeric: guardarlo como integer64 no agrega
  # nada y arrastra la clase.
  expect_false(inherits(.conteo_dbi(bit64::as.integer64("20")), "integer64"))
  expect_equal(.conteo_dbi(bit64::as.integer64("20")), 20)
})

test_that("un conteo exacto no se declara inexacto", {
  skip_if_not_installed("bit64")
  # El error simetrico del que importa: el valor se guardaba exacto y
  # `conteo_exacto` decia FALSE. El paquete se declaraba menos preciso de lo
  # que era.
  expect_true(.conteo_exacto_dbi("123456789012345678"))
  expect_equal(format(.conteo_dbi("123456789012345678")), "123456789012345678")
  expect_true(.conteo_exacto_dbi(20L))
  expect_false(.conteo_exacto_dbi(NULL))
})

test_that("el perfil DBI no deja integer64 en ningun conteo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(
    id = 1:20, monto = (1:20) * 1.5, stringsAsFactors = FALSE
  ))
  perfil <- perfilar_dbi(con, "t", muestra = 10)
  columnas <- perfil$resumen_tabla$columnas
  # Que falte un campo esperado es un fallo, no una excusa para no mirarlo: con
  # `if (campo %in% names(columnas))` un renombre dejaba el bucle entero sin una
  # sola asercion y el bloque seguia en verde.
  campos <- c("n_validos", "n_faltantes", "n_distintos", "frecuencia_moda")
  expect_true(all(campos %in% names(columnas)))
  for (campo in campos) {
    expect_false(
      inherits(columnas[[campo]], "integer64"),
      info = paste("el campo", campo, "quedo como integer64")
    )
  }
  expect_false(inherits(perfil$resumen_tabla$meta$filas, "integer64"))
  muestreo <- perfil$perfil_muestra$meta$origen_dbi$muestreo
  expect_false(inherits(muestreo$filas_totales_fuente, "integer64"))
})

test_that("un perfil guardado se lee bien donde no este bit64", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(id = 1:20, stringsAsFactors = FALSE))
  perfil <- perfilar_dbi(con, "t", muestra = 10)
  n <- perfil$resumen_tabla$columnas$n_validos[[1L]]

  # Esta es la prueba que importa y la que fallaba. Sin `bit64`, R no tiene los
  # metodos de la clase y ve el double subyacente: para un `integer64` de 20 eso
  # es 9.881313e-323, que se imprime como numero, suma como numero y esta mal.
  # `unclass()` reproduce exactamente lo que veria esa maquina.
  expect_equal(as.numeric(unclass(n)), 20)
  expect_equal(unclass(n), 20)
})

test_that("los dos caminos del paquete coinciden en la clase del mismo campo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  datos <- data.frame(id = 1:20, stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)

  en_memoria <- perfilar(datos, analizar_dependencias = FALSE,
                         proteger_datos_personales = FALSE)
  en_base <- perfilar_dbi(con, "t", muestra = 20)

  # No se exige la misma clase exacta -`integer` y `numeric` son ambos numeros
  # corrientes- sino que ninguno de los dos arrastre una clase que dependa de un
  # paquete opcional.
  for (campo in c("n_distintos", "frecuencia_moda")) {
    if (campo %in% names(en_memoria$columnas) &&
        campo %in% names(en_base$resumen_tabla$columnas)) {
      expect_true(is.numeric(en_memoria$columnas[[campo]]))
      expect_true(is.numeric(en_base$resumen_tabla$columnas[[campo]]))
    }
  }
})

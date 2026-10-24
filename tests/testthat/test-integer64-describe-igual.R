# Guardar la misma columna en `integer64` no puede cambiar lo que el paquete
# publica sobre ella.
#
# El 2026-09-05 se encontraron tres desvios de esta clase, todos con el mismo
# metodo: describir la MISMA columna por dos caminos y comparar. `integer64` es
# la tercera forma de guardar un entero -despues de `integer` y `double`- y
# habia quedado afuera de guardas que ya se habian ampliado una vez.
#
# La causa de fondo del desempate merece quedar escrita: **`bit64` enmascara
# `order()` cuando esta adjunto**, y dentro del espacio de nombres de un paquete
# no lo esta. Ahi `order()` es el de base, que sobre un `integer64` ordena por
# los bits del `double` subyacente y manda los negativos al final.

test_that("la moda no depende de si la columna se guarda como integer64", {
  skip_if_not_installed("bit64")
  # Cinco valores con la MISMA frecuencia: el resultado lo decide el desempate.
  crudos <- rep(c(1, 2, -999, 4, 5), 60L)
  en_doble <- perfilar(data.frame(v = as.numeric(crudos)),
                       analizar_dependencias = FALSE)
  en_64 <- perfilar(data.frame(v = bit64::as.integer64(crudos)),
                    analizar_dependencias = FALSE)

  # Primera mitad: hay empate de verdad. Sin el, el desempate no decide nada y
  # la prueba pasaria con el defecto vivo.
  expect_equal(en_doble$columnas$frecuencia_moda, 60L)
  expect_equal(en_64$columnas$frecuencia_moda, 60L)

  expect_equal(as.character(en_64$columnas$moda),
               as.character(en_doble$columnas$moda))
  expect_equal(as.character(en_64$columnas$moda), "-999")
})

test_that("el orden seguro no depende de que bit64 este adjunto", {
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64(c(1, 2, -999, 4, 5))
  # El `order()` de base sobre `integer64` da el orden equivocado; esta prueba
  # fija que el paquete NO lo usa. Si algun dia base aprendiera a ordenarlos,
  # el `expect_false` de abajo avisaria y habria que revisar esta nota.
  expect_equal(.orden_seguro(x), c(3L, 1L, 2L, 4L, 5L))

  # Y por encima de 2^53, donde convertir a `double` perderia precision.
  grandes <- bit64::as.integer64(2)^60 + c(3, 1, 2)
  expect_equal(.orden_seguro(grandes), c(2L, 3L, 1L))
})

test_that("una numeracion densa se reconoce igual en integer64", {
  skip_if_not_installed("bit64")
  # `1:1000` contiene el 999, que esta en la lista por omision de centinelas.
  # En las tres representaciones la columna se reconoce como numeracion y el
  # 999 queda protegido por eso; la conversion de `integer64` debe conservar
  # exactamente los mismos hallazgos.
  perfiles <- lapply(
    list(entero = 1:1000, doble = as.numeric(1:1000),
         i64 = bit64::as.integer64(1:1000)),
    function(x) perfilar(data.frame(v = x), analizar_dependencias = FALSE)
  )

  for (nombre in names(perfiles)) {
    perfil <- perfiles[[nombre]]
    expect_true(isTRUE(perfil$columnas$secuencia_entera_densa), info = nombre)
    expect_equal(perfil$columnas$n_faltantes_disfrazados, 0L, info = nombre)
    expect_true("posible_identificador" %in% perfil$hallazgos$tipo_hallazgo,
                info = nombre)
    # Y la no evaluacion de Benford se declara en los tres: un diagnostico que
    # no corre se declara, tambien cuando no corre por el tipo.
    expect_true("ley_benford" %in% perfil$cobertura_diagnosticos$diagnostico,
                info = nombre)
  }

  # Control: por encima de la precision de `double` NO se mide la secuencia y se
  # dice. Sin esta mitad, medir siempre pasaria el test.
  enormes <- perfilar(
    data.frame(v = bit64::as.integer64(2)^54 + 1:1000),
    analizar_dependencias = FALSE
  )
  expect_false(isTRUE(enormes$columnas$secuencia_entera_densa))
  expect_true("integer64_fuera_precision_double" %in%
                enormes$hallazgos$tipo_hallazgo)
})

test_that("memoria y motor dan la misma moda sobre una columna integer64", {
  skip_if_not_installed("bit64")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  datos <- data.frame(v = bit64::as.integer64(rep(c(1, 2, -999, 4, 5), 60L)))
  en_memoria <- perfilar(datos, analizar_dependencias = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", datos)
  por_motor <- perfilar_dbi(con, "t")

  # Primera mitad: el motor produjo un resumen y una muestra, no tablas vacias.
  expect_true(nrow(por_motor$resumen_tabla$columnas) >= 1L)
  expect_true(nrow(por_motor$perfil_muestra$columnas) >= 1L)

  expect_equal(as.character(por_motor$resumen_tabla$columnas$moda),
               as.character(en_memoria$columnas$moda))
  expect_equal(as.character(por_motor$perfil_muestra$columnas$moda),
               as.character(en_memoria$columnas$moda))
})

test_that("la moda de una columna integer64 no depende de bit64 adjunto", {
  skip_if_not_installed("bit64")
  # `bit64` NO registra sus metodos S3 cuando solo esta cargado, asi que dentro
  # del espacio de nombres del paquete cualquier operacion de base sobre un
  # `integer64` -incluso `x[1]`- lo degrada a `double` REINTERPRETANDO SUS BITS.
  # La moda publicada era `4.4501477170144e-308` cuando el usuario no tenia
  # `bit64` adjunto, y el valor correcto cuando si: el numero publicado dependia
  # de lo que el usuario tuviera cargado en su sesion.
  #
  # Los valores se construyen desde CADENA: `as.integer64(9007199254740993)`
  # recibe un literal `double` que ya redondeo a 2^53, asi que los dos valores
  # serian el mismo y la prueba no probaria nada.
  columna <- bit64::as.integer64(c("9007199254740992", "9007199254740993"))
  expect_equal(length(unique(bit64::as.character.integer64(columna))), 2L)

  # NO se afirma aqui si `bit64` tiene registrados sus metodos S3: eso depende de
  # lo que este adjunto en la sesion -dentro de la suite puede estarlo por otra
  # prueba- y es un detalle de otro paquete. Lo que esta prueba fija es la
  # CONDUCTA: el valor publicado es el correcto pase lo que pase.

  moda <- .moda_columna(columna)
  expect_equal(moda$valor, "9007199254740992")
  expect_equal(moda$frecuencia, 1L)

  # Y el desempate sigue el orden numerico, igual que en una columna `double`
  # equivalente: no se cambia el criterio segun el tipo.
  con_empate <- rep(c(1, 2, -999, 4, 5), 60L)
  expect_equal(
    .moda_columna(bit64::as.integer64(con_empate))$valor,
    .moda_columna(as.numeric(con_empate))$valor
  )
  # Y el caso donde el orden por bytes daria otra cosa: "10" antes que "9".
  nueve_diez <- c(rep(9, 3L), rep(10, 3L))
  expect_equal(
    .moda_columna(bit64::as.integer64(nueve_diez))$valor,
    .moda_columna(as.numeric(nueve_diez))$valor
  )
})

test_that("el perfil de normalización compara sin tocar los datos", {
  perfil <- normalizacion()
  expect_s3_class(perfil, "normalizacion_lupa")
  expect_identical(
    lupa:::.normalizacion_aplicar(c("JOSÉ PÉREZ", "jose perez"), perfil),
    c("jose perez", "jose perez")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(c("Pena", "Peña"), perfil),
    c("pena", "peña")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(c("‘Ana’ Silva", "Ana Silva", "D’Angelo"), perfil),
    c("ana silva", "ana silva", "d'angelo")
  )
})

test_that("la descomposición canónica iguala NFC y NFD", {
  skip_if_not_installed("stringi")
  nfd <- intToUtf8(c(74L, 111L, 115L, 101L, 769L))
  salida <- lupa:::.normalizacion_aplicar(
    c("José", nfd, "g̃"),
    normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                  comillas = FALSE)
  )
  expect_identical(salida[[1L]], salida[[2L]])
  expect_identical(salida[[3L]], "g̃")
  marcas_desordenadas <- intToUtf8(c(769L, 803L, 97L))
  marcas_ordenadas <- intToUtf8(c(803L, 769L, 97L))
  expect_identical(
    lupa:::.normalizacion_aplicar(
      c(marcas_desordenadas, marcas_ordenadas),
      normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                    comillas = FALSE)
    )[[1L]],
    lupa:::.normalizacion_aplicar(
      marcas_ordenadas,
      normalizacion(acentos = FALSE, minusculas = FALSE, espacios = FALSE,
                    comillas = FALSE)
    )[[1L]]
  )
  expect_identical(salida, stringi::stri_trans_nfd(c("José", nfd, "g̃")))
})

test_that("los grafemas guaranies se protegen al quitar acentos", {
  guarani <- "mba'e g̃uahe"
  expect_identical(
    lupa:::.normalizacion_aplicar(guarani, normalizacion()),
    guarani
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(
      guarani, normalizacion(proteger = character())
    ),
    "mba'e guahe"
  )
  expect_s3_class(normalizacion(proteger = "g̃"), "normalizacion_lupa")
})

test_that("la tabla latina de descomposición coincide con stringi", {
  skip_if_not_installed("stringi")
  codigos <- c(0x00A0:0x024F, 0x1E00:0x1EFF)
  caracteres <- intToUtf8(codigos, multiple = TRUE)
  perfil <- normalizacion(
    minusculas = FALSE, espacios = FALSE, acentos = FALSE,
    comillas = FALSE
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(caracteres, perfil),
    stringi::stri_trans_nfd(caracteres)
  )
})

test_that("los pasos optativos son explícitos", {
  entrada <- c("oﬁcina", "ＡＢＣ", "Ana, Silva")
  expect_identical(
    lupa:::.normalizacion_aplicar(entrada, normalizacion()),
    c("oﬁcina", "ａｂｃ", "ana, silva")
  )
  expect_identical(
    lupa:::.normalizacion_aplicar(
      entrada, normalizacion(ligaduras = TRUE, ancho = TRUE, puntuacion = TRUE)
    ),
    c("oficina", "abc", "ana silva")
  )
  expect_true(is.list(lupa:::.resolver_normalizacion("amplio")))
})

test_that("la herencia pasa por el perfil y las fusiones quedan observables", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = c("JOSÉ", "jose"), stringsAsFactors = FALSE)
  perfil <- perfilar(datos, analizar_dependencias = FALSE,
                     normalizar = normalizacion(acentos = FALSE))
  expect_s3_class(perfil$meta$normalizacion, "normalizacion_resuelta_lupa")
  duplicados <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", normalizar = NULL, perfil = perfil,
    max_resultados = Inf
  )
  expect_true(nrow(duplicados$pares) > 0L)
  expect_false(any(duplicados$pares$tipo_par == "exacto"))
  expect_true(is.list(duplicados$normalizacion$fusiones))
  expect_true(is.list(perfil$meta$normalizacion_fusiones$nombre))
  claves <- detectar_claves(datos, normalizar = NULL, perfil = perfil)
  expect_true(all(c("unicidad_exacta", "unicidad_normalizada") %in% names(claves)))
})

test_that("normalizar FALSE no calcula un informe de fusiones", {
  datos <- data.frame(
    nombre = c("JOSÉ", "jose", "Ana"),
    stringsAsFactors = FALSE
  )
  salida <- detectar_duplicados_aproximados(
    datos, columnas = "nombre", normalizar = FALSE, max_resultados = Inf
  )
  expect_false("fusiones" %in% names(salida$normalizacion))
  perfil <- perfilar(
    datos, normalizar = FALSE, duplicados_aproximados = FALSE,
    analizar_dependencias = FALSE
  )
  expect_null(perfil$meta$normalizacion_fusiones)
})

test_that("perfilar reutiliza el informe al ejecutar duplicados", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(
    nombre = c("JOSÉ", "jose", "Ana"),
    stringsAsFactors = FALSE
  )
  original <- lupa:::.normalizacion_fusiones_tabla
  llamadas <- 0L
  testthat::local_mocked_bindings(
    .normalizacion_fusiones_tabla = function(...) {
      llamadas <<- llamadas + 1L
      original(...)
    },
    .package = "lupa"
  )
  perfil <- perfilar(
    datos, duplicados_aproximados = TRUE, analizar_dependencias = FALSE
  )
  expect_equal(llamadas, 1L)
  expect_identical(
    perfil$duplicados_aproximados$normalizacion$fusiones,
    perfil$meta$normalizacion_fusiones
  )
})

test_that("los perfiles opcionales no delegan un vector innecesariamente", {
  skip_on_cran()
  valores <- paste0("nombre", seq_len(20000L))
  perfiles <- list(
    normalizacion(),
    normalizacion(puntuacion = TRUE),
    normalizacion(ligaduras = TRUE),
    normalizacion(ancho = TRUE),
    normalizacion(comillas = TRUE),
    .resolver_normalizacion("amplio")$general
  )
  medir <- function(perfil) {
    median(replicate(3L, system.time(
      lupa:::.normalizacion_aplicar(valores, perfil)
    )["elapsed"]))
  }
  tiempos <- vapply(perfiles, medir, numeric(1L))
  base <- max(tiempos[[1L]], 0.01)
  expect_lt(max(tiempos[-1L]), base * 8)
})

test_that("las fusiones declaran el vocabulario completo y sus contribuciones", {
  perfil <- normalizacion()
  exactas <- lupa:::.normalizacion_fusiones_vocabulario(
    c("José", "jose", "Ana"), perfil
  )
  expect_identical(exactas$estado, "exacto")
  expect_identical(exactas$n_distintos, 3L)
  expect_identical(exactas$n_usados, 3L)
  expect_identical(exactas$n_distintos_normalizados, 2L)
  contraste <- lupa:::.normalizacion_fusiones_vocabulario(
    c("JOSÉ", "jose", "José", "ANA", "ana", "ÁNA"), perfil
  )
  expect_identical(contraste$pasos$acentos, 2L)
  expect_identical(contraste$pasos$minusculas, 3L)
  expect_gt(sum(unlist(contraste$pasos)),
            contraste$n_distintos - contraste$n_distintos_normalizados)
  expect_false("descomposicion_canonica" %in% names(contraste$pasos))
  completas <- lupa:::.normalizacion_fusiones_vocabulario(
    paste0("valor-", seq_len(700L)), perfil
  )
  expect_identical(completas$estado, "exacto")
  expect_identical(completas$n_distintos, 700L)
  expect_identical(completas$n_usados, 700L)
  expect_identical(completas$n_distintos_normalizados, 700L)
  expect_true(is.list(completas$pasos))

  # Las fusiones son una propiedad de pares: tomar una muestra de valores
  # puede dejar fuera ambos miembros de todos los pares. El informe usa el
  # vocabulario completo y no puede convertir una tasa conocida en cero.
  gemelos <- c(
    paste0("nómbre", seq_len(1000L)),
    paste0("nombre", seq_len(1000L))
  )
  resultado <- lupa:::.normalizacion_fusiones_vocabulario(gemelos, perfil)
  expect_identical(resultado$estado, "exacto")
  expect_identical(resultado$n_usados, 2000L)
  expect_identical(resultado$n_distintos_normalizados, 1000L)
  expect_identical(resultado$pasos$acentos, 1000L)
})

test_that("los perfiles por columna se resuelven sin mezclar criterios", {
  skip_if_not_installed("stringdist")
  datos <- data.frame(nombre = c("JOSÉ", "jose"), codigo = c("A-1", "A1"),
                      stringsAsFactors = FALSE)
  perfil <- list(
    nombre = normalizacion(acentos = TRUE),
    codigo = normalizacion(acentos = FALSE, puntuacion = TRUE)
  )
  salida <- detectar_duplicados_aproximados(
    datos, columnas = names(datos), normalizar = perfil, max_resultados = Inf
  )
  expect_equal(salida$pares$tipo_par, "exacto_normalizado")
  expect_true(salida$pares$igualo_normalizar)
})

test_that("las metricas de vocabulario aceptan perfiles de normalizacion", {
  skip_if_not_installed("stringdist")
  metrica <- especializar(
    metricas_nucleo()$EntidadContradictoria,
    normalizar = normalizacion(acentos = FALSE)
  )
  medicion <- medir(
    modelo(instanciar(metrica, "personas", "nombre")),
    data.frame(nombre = c("JOSÉ", "jose"), stringsAsFactors = FALSE)
  )
  alcance <- attr(medicion, "alcance_metricas")[[1L]]
  expect_identical(alcance$normalizacion$general$acentos, FALSE)
})

# La sustitucion de la tabla extraia y reensamblaba con `regmatches()`, que
# indexa por CARACTER: en una cadena UTF-8 eso obliga a recorrerla desde el byte
# cero en cada coincidencia, y el costo crece con el cuadrado. Medido sobre
# 160.000 bytes: 41,1 s por caracter contra 0,079 s por bytes, con las mismas
# coincidencias. Ahora se usa un patron equivalente en bytes.
#
# Es equivalente porque cada clave de la tabla es un unico codepoint: en UTF-8
# ninguna secuencia valida es prefijo de otra ni empieza en medio de otra.
test_that("los dos patrones de la tabla encuentran lo mismo", {
  tabla <- lupa:::.normalizacion_tabla_vectorizada()
  expect_true(nzchar(tabla$patron_bytes))

  # Todas las claves de la tabla, y no una muestra comoda: si alguna difiere,
  # la sustitucion produce otro texto.
  claves <- names(tabla$reemplazos)
  texto <- paste0("a", paste(claves, collapse = "b"), "c")

  por_caracter <- regmatches(
    texto, gregexpr(tabla$patron, texto, perl = TRUE)
  )[[1L]]
  por_bytes <- regmatches(
    texto, gregexpr(tabla$patron_bytes, texto, perl = TRUE, useBytes = TRUE)
  )[[1L]]
  Encoding(por_bytes) <- "UTF-8"

  expect_equal(length(por_caracter), length(claves))
  expect_identical(por_bytes, por_caracter)
})

test_that("la sustitucion conserva el texto y su codificacion", {
  u <- function(...) rawToChar(as.raw(c(...)))
  entradas <- c(
    vacio = "", ascii = "hola mundo",
    acentos = u(0xC3, 0xA9, 0xC3, 0xB1, 0xC3, 0x9C),
    ligadura = u(0xEF, 0xAC, 0x81),
    cjk = u(0xE6, 0x97, 0xA5, 0xE6, 0x9C, 0xAC),
    emoji = u(0xF0, 0x9F, 0x98, 0x80),
    mezcla = paste0("Jose ", u(0xC3, 0xA9), " ", u(0xE6, 0x97, 0xA5)),
    ausente = NA_character_
  )
  salida <- lupa:::.normalizacion_reemplazar_tabla(unname(entradas))

  # El mecanismo se activo: al menos una entrada cambio.
  expect_true(any(salida != entradas, na.rm = TRUE))
  # Lo que no tiene nada que sustituir vuelve intacto, y NA sigue siendo NA.
  expect_identical(salida[[1L]], "")
  expect_identical(salida[[2L]], "hola mundo")
  expect_true(is.na(salida[[length(salida)]]))
  # Y todo lo no ausente queda utilizable como UTF-8: sin esto el reensamblado
  # por bytes deja la cadena sin marca y el resto del paquete cuenta mal.
  presentes <- salida[!is.na(salida)]
  expect_true(all(validUTF8(presentes)))
  expect_false(any(Encoding(presentes) == "bytes"))
})

test_that("el costo de la sustitucion es lineal en el largo", {
  skip_on_cran()
  u <- function(...) rawToChar(as.raw(c(...)))
  medir_largo <- function(bytes) {
    texto <- paste(rep(u(0xC3, 0xA9), bytes / 2), collapse = "")
    inicio <- Sys.time()
    invisible(lupa:::.normalizacion_reemplazar_tabla(texto))
    as.numeric(difftime(Sys.time(), inicio, units = "secs"))
  }
  chico <- medir_largo(20000)
  grande <- medir_largo(160000)
  # Ocho veces los datos. Lineal daria ocho veces el tiempo; el cuadratico que
  # habia daba sesenta y cuatro, y medido daba doscientas cuarenta.
  expect_lt(grande, max(chico * 24, 3))
})

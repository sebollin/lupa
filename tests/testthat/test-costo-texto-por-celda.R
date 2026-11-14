# El perfil de una columna de texto paga por celda solo lo que la celda puede
# necesitar: lo que solo cambia o busca caracteres no ASCII no recorre las celdas
# ASCII, lo que casi nunca encuentra nada se pregunta solo donde puede estar, y
# lo que ya se calculo no se vuelve a calcular. Cada prueba cuenta el trabajo y
# comprueba que la respuesta es la de siempre.

# Cuantas celdas recibe una funcion de base mientras corre `codigo`: la suma del
# largo de su argumento `argumento` en las llamadas en que `condicion` (una
# expresion sobre sus argumentos) es verdadera.
.celdas_en_base <- function(funcion, argumento, condicion, codigo) {
  registro <- new.env()
  registro$celdas <- 0
  suppressMessages(trace(
    funcion, where = asNamespace("base"), print = FALSE,
    tracer = bquote(if (isTRUE(.(condicion))) {
      assign("celdas", get("celdas", envir = .(registro)) +
               length(.(as.name(argumento))), envir = .(registro))
    })
  ))
  on.exit(suppressMessages(untrace(funcion, where = asNamespace("base"))),
          add = TRUE)
  valor <- codigo
  list(valor = valor, celdas = registro$celdas)
}

test_that("declarar la codificacion no recorre las celdas ASCII", {
  skip_on_cran()
  acento <- paste0("Jos", intToUtf8(0xE9L))
  Encoding(acento) <- "unknown"
  x <- c(rep(c("ABC", "12,5", NA), 2000L), acento)
  r <- .celdas_en_base("Encoding<-", "x", TRUE, .declarar_utf8_valido(x))
  # La celda no ASCII sin marca queda declarada; las demas, como estaban.
  expect_identical(Encoding(r$valor[[6001L]]), "UTF-8")
  expect_identical(r$valor[-6001L], x[-6001L])
  # Y es la unica que recibe la marca: antes se remarcaban las 4.001 presentes.
  expect_identical(r$celdas, 1)
})

test_that("bajar la caja no lee en UTF-8 las celdas ASCII", {
  skip_on_cran()
  x <- c(rep(c("ABC", "Montevideo"), 1000L),
         paste0(intToUtf8(0xC9L), "XITO"), paste0("TREI", intToUtf8(0x3C2L)))
  r <- .celdas_en_base(
    "chartr", "x", quote(any(grepl("[^\\001-\\177]", old, useBytes = TRUE))),
    .normalizacion_minusculas(x)
  )
  expect_identical(r$valor[1:2], c("abc", "montevideo"))
  expect_identical(r$valor[[2001L]], paste0(intToUtf8(0xE9L), "xito"))
  expect_identical(r$valor[[2002L]], paste0("trei", intToUtf8(0x3C3L)))
  # Los dos mapas no ASCII -minusculas y sigma final- ven las dos celdas que
  # pueden cambiar, no las 2.002.
  expect_identical(r$celdas, 4)
})

test_that("las entidades HTML se buscan solo donde hay un ampersand", {
  skip_on_cran()
  x <- c(rep(c("sin entidad", "12,5", NA), 1000L),
         "Caf&eacute;", "A & B", "&noexiste;")
  r <- .celdas_en_base("gregexpr", "text", TRUE, .entidades_html_en_texto(x))
  expect_identical(r$valor, c(rep(FALSE, 3000L), TRUE, FALSE, FALSE))
  expect_identical(r$celdas, 3)
})

test_that("los invisibles se preguntan solo en las celdas que traen alguno", {
  skip_on_cran()
  x <- c(rep(paste0("Jos", intToUtf8(0xE9L)), 1000L),
         paste0("a", intToUtf8(0xA0L), "b"), paste0("c", intToUtf8(0x200BL), "d"),
         NA, "a\tb")
  r <- .celdas_en_base(
    "grepl", "x", quote(isTRUE(perl) && length(x) == 1L),
    .predicados_invisibles(x)
  )
  expect_identical(lapply(r$valor, which), list(
    control = c(1001L, 1002L), eliminable = 1002L, espacio = 1001L,
    significativo = integer(), separador = 1004L
  ))
  # Una columna con un acento ya no pasa cada celda por el bucle: antes eran
  # mas de mil llamadas de un valor, una por celda.
  expect_lte(r$celdas, 10)
})

test_that("normalizar un vocabulario ASCII no lo recorre en los pasos no ASCII", {
  skip_on_cran()
  x <- sprintf("USD %d,%02d", 1:2000, 1:2000 %% 100)
  perfil <- .normalizacion_para_columna(.resolver_normalizacion(TRUE), "x")
  registro <- new.env()
  registro$celdas <- 0
  funciones <- c(gsub = "x", grepl = "x", sub = "x", chartr = "x",
                 regexpr = "text", gregexpr = "text")
  for (f in names(funciones)) {
    suppressMessages(trace(f, where = asNamespace("base"), print = FALSE,
      tracer = bquote(assign("celdas", get("celdas", envir = .(registro)) +
        length(.(as.name(funciones[[f]]))), envir = .(registro)))))
  }
  on.exit(for (f in names(funciones)) {
    suppressMessages(untrace(f, where = asNamespace("base")))
  }, add = TRUE)
  salida <- .normalizacion_aplicar(x, perfil)
  expect_identical(salida[1:2], c("usd 1,01", "usd 2,02"))
  # Cada celda pasa por las expresiones y los `chartr()` de los pasos que
  # pueden cambiar ASCII (espacios, comillas, minusculas): eran 28 pasadas por
  # celda con los de acentos, descomposicion y grafemas protegidos, que sobre
  # ASCII no cambian nada.
  expect_lte(registro$celdas / length(x), 20)
})

test_that("las fusiones de un vocabulario ASCII no lo normalizan sin acentos", {
  skip_on_cran()
  perfil <- .normalizacion_para_columna(.resolver_normalizacion(TRUE), "x")
  llamadas <- 0L
  original <- .normalizacion_aplicar
  local_mocked_bindings(.normalizacion_aplicar = function(textos, perfil) {
    llamadas <<- llamadas + 1L
    original(textos, perfil)
  })
  barrios <- sprintf("Barrio %02d", 1:70)
  ascii <- .normalizacion_fusiones(c(barrios, "BARRIO 01"), perfil)
  expect_identical(ascii$pasos, list(espacios = 0L, comillas = 0L,
                                     acentos = 0L, minusculas = 1L))
  # La completa y una por cada paso que puede cambiar ASCII: sin la de acentos.
  expect_identical(llamadas, 4L)
  llamadas <- 0L
  acentuado <- .normalizacion_fusiones(
    c(barrios, paste0("Jos", intToUtf8(0xE9L)), "Jose"), perfil
  )
  expect_identical(acentuado$pasos$acentos, 1L)
  expect_identical(llamadas, 5L)
})

test_that("el vocabulario casi duplicado se normaliza una vez por forma", {
  skip_on_cran()
  x <- rep(c("Montevideo", "Montevido", "Salto"), c(1000L, 10L, 500L))
  celdas <- 0
  original <- .normalizacion_aplicar
  local_mocked_bindings(.normalizacion_aplicar = function(textos, perfil) {
    celdas <<- celdas + length(textos)
    original(textos, perfil)
  })
  g <- .grupos_casi_duplicados_vocabulario(x, .resolver_normalizacion(TRUE), "x")
  expect_identical(g$alcance$largo_maximo, 10L)
  expect_identical(g$alcance$n_valores_distintos, 3L)
  # Tres formas, tres normalizaciones: antes eran las 1.510 filas y despues
  # otra vez las tres formas.
  expect_identical(celdas, 3)
  skip_if_not_installed("stringdist")
  expect_identical(g$grupos[[1L]]$variantes, c("Montevideo", "Montevido"))
  expect_identical(g$grupos[[1L]]$frecuencias, c(1000L, 10L))
})

test_that("sin centinelas declarados los faltantes disfrazados se detectan una vez", {
  skip_on_cran()
  datos <- data.frame(
    v = c(sprintf("%d,5", 1:60), "-999", "sin dato", "-999"),
    stringsAsFactors = FALSE
  )
  llamadas <- 0L
  original <- .detectar_faltantes_disfrazados
  local_mocked_bindings(.detectar_faltantes_disfrazados = function(...) {
    llamadas <<- llamadas + 1L
    original(...)
  })
  sin_declarar <- perfilar(datos, analizar_dependencias = FALSE)
  n_sin_declarar <- llamadas
  llamadas <- 0L
  declarados <- perfilar(datos, analizar_dependencias = FALSE,
                         sentinelas_numericos = -999)
  n_declarados <- llamadas
  # Con la lista declarada hace falta la segunda lectura -cuales coinciden con
  # lo declarado-; sin ella, no.
  expect_identical(n_declarados - n_sin_declarar, 1L)
  expect_identical(sin_declarar$columnas$n_faltantes_disfrazados[[1L]], 3L)
  expect_identical(declarados$columnas$n_faltantes_disfrazados[[1L]], 3L)
})

test_that("el mapa de distintos de una columna numerica se arma una vez", {
  skip_on_cran()
  set.seed(2)
  datos <- data.frame(
    x = round(stats::rnorm(300), 2),
    y = as.Date("2020-01-01") + c(1:299, 1L)
  )
  mapas <- 0L
  original <- .mapa_distintos_bloques
  local_mocked_bindings(.mapa_distintos_bloques = function(...) {
    mapas <<- mapas + 1L
    original(...)
  })
  p <- perfilar(datos, analizar_dependencias = FALSE)
  expect_identical(p$columnas$n_distintos, c(
    length(unique(datos$x)), 299L
  ))
  expect_identical(p$columnas$moda[[2L]], "2020-01-02")
  # El conteo de distintos y la moda lo armaban cada uno: dos por columna.
  expect_identical(mapas, 2L)
})

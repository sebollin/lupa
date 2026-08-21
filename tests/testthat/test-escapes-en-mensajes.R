# En codigo R los no-ASCII van con escapes \uXXXX. Escribir dos barras invertidas
# no produce una vocal acentuada sino la secuencia literal, y el usuario termina
# leyendo "pérdida" en pantalla. Es un error silencioso: el paquete instala,
# la suite pasa y el check no lo ve, porque la cadena es ASCII perfectamente
# valida. Solo se nota mirando el mensaje.
#
# Este barrido recorre los literales de cadena del espacio de nombres ya
# cargado -no el fuente-, asi que vale igual bajo `R CMD check`, donde `R/` no
# existe.

.literales_de <- function(x, acc = character()) {
  if (is.character(x)) return(c(acc, x))
  if (is.function(x)) {
    # Mirar solo el cuerpo dejaba fuera dos sitios donde de verdad viven
    # mensajes: los valores por omision de los argumentos -que es donde suelen
    # ir los textos configurables- y los atributos. Un barrido que mira menos
    # de lo que dice es peor que ninguno, porque da confianza.
    acc <- .literales_de(body(x), acc)
    acc <- .literales_de(formals(x), acc)
    return(.literales_de(.atributos_sin_fuente(x), acc))
  }
  if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x)) {
    for (i in seq_along(x)) {
      # Un argumento formal sin valor por omision es el simbolo vacio, y
      # cualquier intento de forzarlo tira "argumento ausente". No se puede
      # comprobar antes sin forzarlo, asi que se atrapa.
      acc <- tryCatch(.literales_de(x[[i]], acc), error = function(e) acc)
    }
  }
  acc
}

.atributos_sin_fuente <- function(x) {
  atributos <- attributes(x)
  # `srcref` trae el texto fuente entero, incluido el de este mismo archivo:
  # meterlo al barrido lo llenaria de coincidencias que no son mensajes.
  atributos[!names(atributos) %in% c("srcref", "srcfile", "wholeSrcref")]
}

.literales_del_paquete <- function() {
  ns <- asNamespace("lupa")
  unlist(lapply(ls(ns, all.names = TRUE), function(nombre) {
    objeto <- tryCatch(get(nombre, envir = ns), error = function(e) NULL)
    if (is.function(objeto)) {
      .literales_de(objeto)
    } else if (is.character(objeto)) {
      objeto
    } else {
      character()
    }
  }))
}

test_that("el barrido ve de verdad los literales del paquete", {
  # Sin esto, un barrido que devuelve cero cadenas pasaria la prueba de abajo
  # sin haber mirado nada.
  literales <- .literales_del_paquete()
  expect_gt(length(literales), 1000L)
  expect_true(any(grepl("lupa", literales, fixed = TRUE)))
})

test_that("el barrido marca un escape mal escrito, en los tres sitios", {
  en_cuerpo <- function() {
    cli::cli_alert_danger("revise la p\\u00e9rdida declarada")
  }
  # Estos dos no los veia: el valor por omision de un argumento es donde suele
  # ir un texto configurable, y un atributo es donde se guarda un mensaje
  # asociado a la funcion.
  en_omision <- function(aviso = "revise la p\\u00e9rdida declarada") NULL
  en_atributo <- function() NULL
  attr(en_atributo, "aviso") <- "revise la p\\u00e9rdida declarada"

  for (funcion in list(en_cuerpo, en_omision, en_atributo)) {
    expect_length(
      grep("\\\\u[0-9a-fA-F]{4}", .literales_de(funcion)),
      1L
    )
  }
})

test_that("lo que el barrido no puede ver queda dicho, no supuesto", {
  # Una constante capturada por closure desde un ambito local no se ve: haria
  # falta recorrer entornos, y el de una funcion del paquete es el espacio de
  # nombres entero. En `lupa` no es un agujero real, porque las constantes del
  # paquete son enlaces del propio espacio de nombres y el barrido las recorre
  # una por una ahi. Queda escrito para que nadie lea el barrido como una
  # garantia mas fuerte de la que da.
  fabrica <- function() {
    guardado <- "revise la p\\u00e9rdida declarada"
    function() guardado
  }
  expect_length(grep("\\\\u[0-9a-fA-F]{4}", .literales_de(fabrica())), 0L)
  # Pero una constante del espacio de nombres si se ve, y es donde viven las
  # del paquete.
  constantes <- Filter(
    is.character,
    mget(ls(asNamespace("lupa"), all.names = TRUE),
         envir = asNamespace("lupa"), ifnotfound = list(NULL))
  )
  expect_gt(length(constantes), 0L)
})

test_that("ningun mensaje del paquete lleva un escape sin resolver", {
  literales <- .literales_del_paquete()
  malos <- unique(literales[grepl("\\\\u[0-9a-fA-F]{4}", literales)])
  expect_equal(malos, character())
})

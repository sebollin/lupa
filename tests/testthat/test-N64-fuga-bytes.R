# Bajo un locale que no puede representar un caracter, el paquete tiene dos
# maneras de publicarlo: `<U+00F1>`, que es lo que hace R con una cadena
# DECLARADA UTF-8, y `<c3><b1>`, que son los bytes crudos de una cadena sin
# declarar. La segunda es ilegible y el proyecto ya la descarto una vez, en el
# consejo publicado. Esta guarda existe porque volvio a aparecer en otro canal:
# un aviso componia prosa del paquete con el nombre que puso el usuario, y la
# frase entera salia a medias -el texto fijo legible, el nombre en bytes-.
#
# Alcance, y esta acotacion es deliberada: se mide el canal de PROSA -lo que el
# paquete escribe por `stderr` con cli-, no la impresion de un data.frame
# devuelto. Para los datos rige la politica contraria y ya decidida: los valores
# se marcan y los NOMBRES no, porque son dato publicado y marcarlos cambiaria lo
# que el usuario recibe. Ahi `print.data.frame()` de base escapa en octal bajo
# `C`, y eso es R haciendo lo suyo, no el paquete.
#
# La guarda se probo contra el defecto: antes del arreglo,
# `.avisar_costo_tabla_ancha()` publicaba `f_a<c3><b1>o` y este archivo fallaba.

.sin_marca_n64 <- function(x) rawToChar(charToRaw(x))

.patron_bytes_crudos_n64 <- "<[0-9a-f][0-9a-f]>"

# La version anterior de esta guarda capturaba SOLO `stderr`, porque los canales
# que conocia publicaban por `cli`. Cuatro metodos `print.*` publican prosa por
# `stdout` con `cat()`, y para la guarda no existian: uno de ellos emitia bytes
# crudos y pasaba. Una guarda que mira una sola corriente no dice "no hay
# fugas", dice "no hay fugas donde miro".
.prosa_bajo_c_n64 <- function(f) {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  originales <- stats::setNames(
    vapply(categorias, Sys.getlocale, character(1L)), categorias
  )
  on.exit(
    for (categoria in categorias) {
      suppressWarnings(Sys.setlocale(categoria, originales[[categoria]]))
    },
    add = TRUE
  )
  for (categoria in categorias) suppressWarnings(Sys.setlocale(categoria, "C"))
  if (!identical(Sys.getlocale("LC_CTYPE"), "C")) {
    skip("no se pudo fijar LC_CTYPE = C en esta maquina")
  }
  # Recibe la FUNCION y la llama, en vez de una expresion con `substitute()`:
  # evaluar en `parent.frame(n)` depende de cuantos marcos hay en el medio y se
  # rompe en cuanto la captura se anida. Dos errores de marco en instrumentos
  # propios el mismo dia; este no tiene marcos que contar.
  captura <- function(tipo) {
    paste(
      capture.output(suppressWarnings(f()), type = tipo),
      collapse = " "
    )
  }
  list(stderr = captura("message"), stdout = captura("output"))
}

test_that("la prosa del paquete no publica bytes crudos bajo C", {
  columna <- .sin_marca_n64("a\u00f1o_medici\u00f3n")
  datos <- data.frame(x = c("a", "b", "a", "c"), stringsAsFactors = FALSE)
  names(datos) <- columna
  datos[[.sin_marca_n64("categor\u00eda")]] <- c(
    .sin_marca_n64("com\u00fan"), "comun", .sin_marca_n64("raz\u00f3n"), "x"
  )
  perfil <- perfilar(
    datos, analizar_dependencias = FALSE, proteger_datos_personales = FALSE
  )

  avisar <- getFromNamespace(".avisar_costo_tabla_ancha", "lupa")
  proyeccion <- list(
    celdas = 1e6, umbral_celdas = 1,
    duracion_estimada_segundos = 1.5,
    fuente = .sin_marca_n64("fuente_a\u00f1o")
  )

  canales <- list(
    perfil = function() print(perfil),
    propuesta = function() print(proponer_modelo(perfil)),
    limpieza = function() print(planificar_limpieza(perfil)),
    senal = function() print(senal_redundante(
      c(columna, "otra"), nombre = .sin_marca_n64("se\u00f1al_a\u00f1o")
    )),
    organizacion = function() print(organizacion(
      .sin_marca_n64("org_a\u00f1o"), .sin_marca_n64("tabla_a\u00f1o")
    )),
    aviso_costo = function() avisar(proyeccion, TRUE, TRUE)
  )

  # Los cuatro metodos que publican prosa por `stdout` salen del mismo auxiliar
  # -`.cat_publicado()`-, asi que los cuatro entran aca. Se enumeraron desde el
  # codigo, buscando quien publica, no desde los que se me ocurrieron.
  canales$normalizacion <- function() print(normalizacion())
  canales$inferencia <- function() print(inferir_tipo(c("1", "2", "3")))
  canales$referencial <- function() {
    tabla <- data.frame(
      clave = c("a", "b"),
      valor = c(.sin_marca_n64("raz\u00f3n"), .sin_marca_n64("a\u00f1o")),
      stringsAsFactors = FALSE
    )
    print(referencial(
      tabla, clave = "clave", valor = "valor",
      nombre = .sin_marca_n64("referencial_a\u00f1o")
    ))
  }
  canales$validadores <- function() print(pack_validadores(
    .sin_marca_n64("pack_a\u00f1o"),
    validadores = list(siempre = function(x) rep(TRUE, length(x))),
    descripcion = .sin_marca_n64("descripci\u00f3n con acento")
  ))

  # El alcance de la guarda se declara y se cuenta. Si manana alguien agrega un
  # metodo que publica prosa y no lo agrega aca, este numero lo delata: una
  # guarda que no dice cuantos canales recorre no distingue "los recorri todos"
  # de "recorri los que conocia cuando la escribi".
  expect_identical(length(canales), 10L)

  for (nombre in names(canales)) {
    prosa <- .prosa_bajo_c_n64(canales[[nombre]])
    # Por `stderr` los bytes crudos llegan como TEXTO -`<c3>`-, porque cli los
    # describe. Por `stdout` llegan como bytes de verdad. Son dos formas del
    # mismo defecto y hacen falta las dos comprobaciones.
    expect_false(
      grepl(.patron_bytes_crudos_n64, prosa$stderr, perl = TRUE),
      info = paste0(nombre, " (stderr): ", substr(prosa$stderr, 1, 200))
    )
    expect_false(
      grepl("[\x80-\xff]", prosa$stdout, useBytes = TRUE),
      info = paste0(nombre, " (stdout): ", substr(prosa$stdout, 1, 200))
    )
  }
})

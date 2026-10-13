# Ronda 31: lo que la refutacion de la proteccion encontro. Una prueba por
# hallazgo, barata: tablas de decenas de filas. Lo que solo se ve con un motor
# que cita la celda sin comillas -SQL Server con la sesion en sueco- va a
# nivel de unidad, con su mensaje.

.r31_cedulas <- function(numeros) {
  vapply(numeros, function(num) {
    s <- sprintf("%07d", num)
    d <- as.integer(strsplit(s, "")[[1L]])
    dv <- (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
    sprintf("%s.%s.%s-%d", substr(s, 1L, 1L), substr(s, 2L, 4L),
            substr(s, 5L, 7L), dv)
  }, character(1L), USE.NAMES = FALSE)
}

# Todo el texto de un objeto: sus hojas de texto y factores, y lo que imprime.
.r31_textos <- function(x) {
  hojas <- unlist(rapply(unclass(x), function(z) as.character(z),
                         classes = c("character", "factor"), how = "unlist"),
                  use.names = FALSE)
  impreso <- tryCatch(utils::capture.output(print(x)),
                      error = function(e) character())
  c(hojas[!is.na(hojas)], impreso)
}

# SQL Server con la sesion en sueco cita la celda sin comillas.
.r31_sueco <- function(x) {
  paste0("[FreeTDS][SQL Server]Konvertering av varchar vardet av typen ", x,
         " till datatypen int misslyckades.")
}

test_that("H1: la coleccion guarda los valores que el mensaje del motor lleva", {
  previos_ids <- .fijar_identificadores_motor(NULL)
  previos <- .activar_valores_motor()
  on.exit({
    .fijar_valores_motor(previos)
    .fijar_identificadores_motor(previos_ids)
  }, add = TRUE)
  # Lo que el mensaje deja de una celda que la coleccion leyo en `clientes` y
  # que cita la vista leida despues, cuyas columnas se suman al leerla.
  mensaje <- function(valor, columnas_vista) {
    .fijar_identificadores_motor(NULL)
    .activar_valores_motor()
    .sumar_identificadores_motor(c("clientes", "id", "nombre", "celular"))
    .sumar_valores_motor(valor)
    .sumar_identificadores_motor(c("vista", "id", columnas_vista))
    .sin_literales_motor(.r31_sueco(valor))
  }
  tapado <- paste0("[FreeTDS][SQL Server][literal omitido] varchar ",
                   "[literal omitido] [valor protegido] [literal omitido] int ",
                   "[literal omitido].")
  # Un celular con un signo que no une: la segunda pasada lee tres numeros de
  # tres cifras y los publica. Los veinte que midio el refutador.
  signos <- c(";", "|", "   ", " / ", "#", "*", " ; ", "!", "=", "~", "&", "?",
              "^", "\"", "<", " , ", " + ", " _ ", " ' ")
  for (s in signos) {
    valor <- paste0("099", s, "123", s, "456")
    salida <- mensaje(valor, "monto")
    expect_false(grepl("123", salida, fixed = TRUE), info = valor)
    expect_false(grepl("456", salida, fixed = TRUE), info = valor)
  }
  for (valor in c("099;123;456", "4;685;467-9", "4111|1111|1111|1111",
                  "4   685   467   9")) {
    expect_identical(mensaje(valor, "monto"), tapado, info = valor)
  }
  # Dos palabras que en la vista son columnas: el nombre de R partido por
  # puntos, o dos columnas. Y con una tercera palabra que la vista no tiene.
  expect_identical(mensaje("Florencia Martinez", "Florencia.Martinez"), tapado)
  expect_identical(mensaje("Florencia Martinez", c("Florencia", "Martinez")),
                   tapado)
  expect_identical(
    mensaje("Ana Florencia Martinez", c("Florencia", "Martinez")), tapado
  )
  # Lo que la segunda pasada omite entero no se guarda: el mensaje ya no lo
  # lleva.
  salida <- mensaje("099 123 456", "monto")
  expect_identical(salida, paste0(
    "[FreeTDS][SQL Server][literal omitido] varchar [literal omitido] int ",
    "[literal omitido]."
  ))
  expect_false("099 123 456" %in% .valores_motor()$valor)
  # La seleccion pregunta por la forma del valor, para no correr la segunda
  # pasada valor por valor; nunca descarta uno del que la segunda pasada, con
  # toda palabra permitida, deja pasar algo. Sobre textos al azar con cifras,
  # letras de otros alfabetos, signos que unen y que no, `^` y saltos de linea.
  set.seed(3101L)
  alfabeto <- c(rep(as.character(0:9), 3L), letters[1:6], "\u00e9", "\u03b1",
                "\u02bc", "\u0661", "\u00b2", " ", "  ", ".", ",", "-", "/",
                ";", "|", "'", "\"", "_", "+", "(", ")", "^", "\n", "\t",
                "\u00a0", "\u2013", "\u00b7", ":", "#", "*",
                "[literal omitido]", "LINE 1:")
  azar <- vapply(seq_len(300L), function(i) {
    paste(sample(alfabeto, sample(3:14, 1L), TRUE), collapse = "")
  }, character(1L))
  publicado <- .publicar_lo_permitido_motor(azar, toda_palabra = TRUE)
  lleva <- grepl("[\\p{L}\\p{N}]",
                 gsub("[literal omitido]", "", publicado, fixed = TRUE),
                 perl = TRUE)
  expect_identical(sum(lleva & !azar %in% .valores_que_lleva_el_motor(azar)), 0L)
})

test_that("H2 y H5: el celular escrito como numero de planilla da la misma cifra", {
  # La misma cifra entera para todas las escrituras del mismo numero.
  escrituras <- c(
    "59898617353.00", "59.898.617.353", "59,898,617,353",
    "59 898 617 353", "59'898'617'353", "59.898.617.353,00",
    "59,898,617,353.00", "59 898 617 353,00", "59'898'617'353.00",
    "+59.898.617.353", "+59,898,617,353.00", "5.9898617353e+10",
    "5.9898617353 e+10", "5.9898617353 e+010"
  )
  expect_identical(.cifra_entera_escrita(escrituras),
                   rep("59898617353", length(escrituras)))
  # Un importe con centavos y la parte decimal con el mismo signo que los
  # miles no son una cifra entera.
  expect_identical(.cifra_entera_escrita(c("1.234.567,45", "59.898.617.353.00",
                                           "12.345")),
                   rep(NA_character_, 3L))
  # De punta a punta: el celular protegido nacional y, en otra columna, el
  # mismo celular con su prefijo escrito asi. Una escritura por fila.
  formas <- list(
    function(e) gsub("#", ".", formatC(e, format = "f", digits = 0, big.mark = "#")),
    function(e) formatC(e, format = "f", digits = 0, big.mark = ","),
    function(e) formatC(e, format = "f", digits = 0, big.mark = " "),
    function(e) formatC(e, format = "f", digits = 0, big.mark = "'"),
    function(e) paste0(gsub("#", ".", formatC(e, format = "f", digits = 0,
                                               big.mark = "#")), ",00"),
    function(e) paste0(formatC(e, format = "f", digits = 0, big.mark = ","), ".00"),
    function(e) paste0("+", formatC(e, format = "f", digits = 0, big.mark = "'")),
    function(e) sprintf("%.10f e+10", e / 1e10),
    function(e) sprintf("%.10f e+010", e / 1e10)
  )
  n <- 3L * length(formas)
  nucleos <- sprintf("9%d%06d", rep(1:9, length.out = n), 100000L + 7919L * seq_len(n))
  nacional <- paste0("0", substr(nucleos, 1L, 2L), " ", substr(nucleos, 3L, 5L),
                     " ", substr(nucleos, 6L, 8L))
  e164 <- as.numeric(paste0("598", nucleos))
  nota <- vapply(seq_len(n), function(i) {
    paste("whatsapp", formas[[(i - 1L) %% length(formas) + 1L]](e164[[i]]))
  }, character(1L))
  d <- data.frame(id = seq_len(n), nombre = sprintf("Cliente Apellidoso %03d", seq_len(n)),
                  celular = nacional, nota = nota, stringsAsFactors = FALSE)
  cp <- c(nombre = "nombre_persona", celular = "telefono")
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = cp, conservar_datos = TRUE
  )))
  # Cuantas notas quedan en claro en la copia: a mano, ninguna de las 27.
  expect_identical(sum(an$datos$nota %in% d$nota), 0L)
  # Y ninguna parte nacional en el perfil, en ninguna escritura.
  cifras <- gsub("[^0-9]", "", .r31_textos(an$perfil))
  expect_identical(sum(vapply(nucleos, function(k) any(grepl(k, cifras, fixed = TRUE)),
                              logical(1L))), 0L)
})

test_that("H3: la copia lee una funcion, una llamada y una formula como las imprime", {
  set.seed(3103L)
  n <- 30L
  ced <- .r31_cedulas(sample(1000000:6999999, n))
  nom <- sprintf("Cliente Apellidoso %03d", seq_len(n))
  d <- data.frame(id = seq_len(n), nombre = nom, cedula = ced,
                  stringsAsFactors = FALSE)
  cp <- c(nombre = "nombre_persona", cedula = "documento_identidad")
  con_fuente <- eval(parse(
    text = paste0("function() {\n  # titular ", ced[[4L]], "\n  NULL\n}"),
    keep.source = TRUE
  ))
  nombre_atributo <- 1:3
  attr(nombre_atributo, ced[[7L]]) <- TRUE
  entorno <- new.env()
  assign("f", eval(parse(text = paste0("function() 1 # ", nom[[8L]]),
                         keep.source = TRUE)), envir = entorno)
  casos <- list(
    list(structure(function() NULL, nota = ced[[1L]]), ced[[1L]]),
    list(structure(quote(f(x)), nota = ced[[2L]]), ced[[2L]]),
    list(structure(y ~ x, nota = nom[[3L]]), nom[[3L]]),
    list(con_fuente, ced[[4L]]),
    list(parse(text = paste0("x + 1 # ", nom[[5L]]), keep.source = TRUE), nom[[5L]]),
    list(nombre_atributo, ced[[7L]]),
    list(entorno, nom[[8L]])
  )
  d$extra <- I(c(lapply(casos, `[[`, 1L), rep(list(NULL), n - length(casos))))
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = cp, conservar_datos = TRUE
  )))
  for (k in seq_along(casos)) {
    valor <- casos[[k]][[2L]]
    impreso <- paste(utils::capture.output(print(an$datos$extra[[k]])),
                     collapse = "\n")
    expect_false(grepl(valor, impreso, fixed = TRUE), info = valor)
    bytes <- serialize(an$datos$extra[[k]], NULL)
    expect_length(grepRaw(charToRaw(valor), bytes, fixed = TRUE), 0L)
  }
  # El nombre del atributo se tapa y el vector queda.
  expect_identical(as.vector(an$datos$extra[[6L]]), 1:3)
  expect_identical(names(attributes(an$datos$extra[[6L]])), "[valor protegido]")
  # Lo que no lleva un valor protegido queda como esta, con su fuente. En un
  # entorno propio: el de la prueba guarda la tabla, y una funcion o una
  # formula que lo lleva se tapa.
  limpio <- new.env(parent = globalenv())
  limpia <- eval(parse(text = "function(x) {\n  # suma uno\n  x + 1\n}",
                       keep.source = TRUE), envir = limpio)
  objetos <- list(limpia, local(y ~ x, envir = limpio), quote(sum(x)), mean,
                  structure(eval(quote(function() NULL), envir = limpio),
                            nota = "sin datos"),
                  parse(text = "x + 1 # nada", keep.source = TRUE))
  # Y una columna lista de puros `NULL`, que abortaba la copia.
  d$extra <- I(rep(list(NULL), n))
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = cp, conservar_datos = TRUE
  )))
  expect_identical(unclass(an$datos$extra), rep(list(NULL), n))
  protegido <- .piso_valor_conservado(objetos, .valores_identificantes(c(ced, nom)))
  for (k in seq_along(objetos)) {
    expect_true(identical(protegido[[k]], objetos[[k]], ignore.srcref = FALSE),
                info = k)
  }
})

test_that("H4: el aviso de consola de una coleccion pasa por el piso final", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  n <- 30L
  clientes <- data.frame(
    id = seq_len(n), nombre = sprintf("Cliente Apellidoso %03d", seq_len(n)),
    celular = sprintf("09%d %03d %03d", rep(1:9, length.out = n),
                      100L + seq_len(n), 300L + seq_len(n)),
    stringsAsFactors = FALSE
  )
  clientes$celular[[25L]] <- "Campos Cadena"
  DBI::dbWriteTable(con, "r31_clientes", clientes)
  DBI::dbExecute(con, "CREATE VIEW r31_vista AS SELECT id FROM r31_clientes")
  # Un motor que cita sin comillas la celda de la tabla base al leer la vista,
  # que va PRIMERO: el aviso sale antes de leer la base.
  real <- perfilar_dbi
  llamadas <- 0L
  local_mocked_bindings(perfilar_dbi = function(conexion, tabla, ...) {
    llamadas <<- llamadas + 1L
    if (llamadas == 1L) {
      warning("El motor cito Campos Cadena al leer la vista.", call. = FALSE)
    }
    real(conexion, tabla, ...)
  })
  for (conservar in c(TRUE, FALSE)) {
    llamadas <- 0L
    avisos <- character()
    pc <- withCallingHandlers(
      perfilar_coleccion(coleccion(con, c("r31_vista", "r31_clientes")),
                         conservar_perfiles = conservar),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    )
    expect_s3_class(pc, "perfil_coleccion")
    expect_true(any(grepl("El motor cito [valor protegido] al leer la vista.",
                          avisos, fixed = TRUE)), info = paste("conservar_perfiles =", conservar))
    expect_false(any(grepl("Campos Cadena", avisos, fixed = TRUE)),
                 info = paste("conservar_perfiles =", conservar))
  }
})

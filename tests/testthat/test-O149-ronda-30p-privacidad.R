# Ronda 30: lo que la refutacion de la proteccion encontro. Una prueba por
# hallazgo, barata: tablas de decenas de filas.

# Todas las hojas de texto de un objeto, tambien los atributos y los nombres de
# fila; las funciones, las llamadas y los entornos, por su texto y su contenido.
.r30_hojas <- function(x, profundidad = 0L) {
  if (is.null(x) || profundidad > 8L) return(character())
  if (is.function(x) || is.language(x)) {
    return(paste(deparse(x), collapse = "\n"))
  }
  if (is.environment(x)) {
    if (identical(x, globalenv()) || isNamespace(x)) return(character())
    return(.r30_hojas(as.list.environment(x, all.names = TRUE), profundidad + 1L))
  }
  salida <- character()
  atributos <- attributes(x)
  for (nombre in setdiff(names(atributos), "class")) {
    valor <- atributos[[nombre]]
    if (identical(nombre, "row.names") && !is.character(valor)) next
    salida <- c(salida, .r30_hojas(valor, profundidad + 1L))
  }
  if (is.list(x)) {
    salida <- c(salida, unlist(lapply(unclass(x), .r30_hojas,
                                      profundidad = profundidad + 1L),
                               use.names = FALSE))
  } else if (is.factor(x)) {
    salida <- c(salida, levels(x))
  } else if (is.atomic(x) && !is.raw(x)) {
    salida <- c(salida, as.character(unclass(x)))
  }
  salida[!is.na(salida)]
}

.r30_cedulas <- function(numeros) {
  vapply(numeros, function(num) {
    s <- sprintf("%07d", num)
    d <- as.integer(strsplit(s, "")[[1L]])
    dv <- (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
    sprintf("%s.%s.%s-%d", substr(s, 1L, 1L), substr(s, 2L, 4L),
            substr(s, 5L, 7L), dv)
  }, character(1L), USE.NAMES = FALSE)
}

# SQL Server con la sesion en sueco cita la celda sin comillas.
.r30_sueco <- function(x) {
  paste0("Konvertering av varchar v\u00e4rdet av typen ", x,
         " till datatypen int misslyckades.")
}

test_that("H1: el mensaje del motor lee el numero como lo lee el piso", {
  # Los separadores entre los grupos de una cedula y de un celular que el piso
  # lee como el mismo numero y la segunda pasada del mensaje del motor no: el
  # refutador midio 39 de 67 -los de Unicode cuya forma de compatibilidad une,
  # signos ASCII, dobles y saltos de linea-, mas el punto medio y la raya, que
  # SQL Server cito de punta a punta.
  unicode <- intToUtf8(c(0xA0, 0x2000:0x200A, 0x202F, 0x205F, 0x3000, 0x2024,
                         0xFE10, 0xFE50, 0xFE52, 0xFE63, 0xFF0C:0xFF0F, 0xB7,
                         0x2013), multiple = TRUE)
  separadores <- c(unicode, "(", ")", "+", "[", "]", "\t", "'", " - ", ". ",
                   " .", "  ", "- ", " -", "\n", "\r\n")
  protegidos <- .valores_identificantes(c("4.685.467-9", "099 123 456"))
  escritos <- c(paste0("4", separadores, "685", separadores, "467-9"),
                paste0("099", separadores, "123", separadores, "456"))
  piso <- .reemplazar_valores_protegidos(escritos, protegidos) != escritos
  salida <- .sin_literales_motor(.r30_sueco(escritos),
                                 identificadores = c("clientes", "celular"))
  publica <- vapply(seq_along(escritos), function(i) {
    grepl(escritos[[i]], salida[[i]], fixed = TRUE) ||
      grepl("685467|099123456", gsub("[^0-9]", "", salida[[i]]))
  }, logical(1L))
  # El piso los reconoce a todos: es la lectura que el mensaje tiene que
  # compartir. Y el mensaje no publica ninguno (el refutador: 39 de 45).
  expect_identical(sum(piso), length(escritos))
  expect_identical(sum(piso & publica), 0L)
  # Los numeros cortos y las palabras del mensaje quedan.
  expect_identical(
    .sin_literales_motor("LINE 1: SELECT 12 - 345 FROM clientes",
                         identificadores = "clientes"),
    "LINE 1: SELECT 12 - 345 FROM clientes"
  )
})

test_that("H1: la coleccion tapa el valor escrito con palabras permitidas que leyo", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # duckdb cita la celda entre comillas; aca se la cita como SQL Server en
  # sueco, sin comillas, que es donde la segunda pasada la publicaba.
  real <- .sin_literales_motor
  local_mocked_bindings(.mensaje_motor = function(condicion) {
    texto <- conditionMessage(condicion)
    citado <- regmatches(texto, regexec("string '([^']*)' to", texto))[[1L]]
    if (length(citado) == 2L) texto <- .r30_sueco(citado[[2L]])
    real(texto, solo_comillas = inherits(condicion, c("lupa_error_dbi",
                                                      "lupa_aviso_dbi")))
  })
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(3001L)
  celular <- sprintf("09%d%06d", sample(1:9, 20L, TRUE),
                     sample(100000:999999, 20L))
  # Dos apellidos que son palabras del vocabulario castellano de los mensajes.
  celular[[7L]] <- "Cadena Campos"
  DBI::dbWriteTable(con, "clientes", data.frame(
    nombre = sprintf("Cliente Apellidoso %03d", 1:20), celular_txt = celular,
    stringsAsFactors = FALSE
  ))
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_cel AS SELECT nombre, CAST(celular_txt AS INTEGER) AS celular",
    "FROM clientes"
  ))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    coleccion(con, c("clientes", "v_cel"))
  )))
  hojas <- .r30_hojas(list(pc$cobertura_coleccion, pc$cobertura_metricas,
                           pc$meta$lecturas))
  expect_identical(sum(grepl("Cadena Campos", hojas, fixed = TRUE)), 0L)
  # El mensaje llego y sigue diciendo que fallo la conversion.
  expect_true(any(grepl("varchar [literal omitido] [valor protegido]", hojas,
                        fixed = TRUE)))
})

test_that("H2 y H3: la cifra entera escrita como planilla es el documento entero, no el parcial", {
  set.seed(3002L)
  numeros <- sample(1000000:6999999, 200L)
  cedulas <- .r30_cedulas(numeros)
  protegidos <- .valores_identificantes(c(cedulas, "099 123 456"))
  tapa <- function(x) {
    .reemplazar_valores_protegidos(x, protegidos, exigir_limites = FALSE) != x
  }
  # H3: el importe redondo cuyo entero es, en todos, la cedula protegida sin
  # su verificador -el peor caso-. La ronda 29 tapaba los 200, y la ronda 28
  # ninguno: un decimal no es el documento parcial. (Con un solo cero,
  # "4685467.0" se lee tambien como la cedula con su verificador tras un punto,
  # como en la ronda 28: es la regla del numero de un solo signo.)
  for (forma in c("%d.00", "%d,00", "%d.0000",
                  "transferencia por %d.00 pesos")) {
    expect_identical(mean(tapa(sprintf(forma, numeros))), 0, info = forma)
  }
  # El entero de siete cifras si es el parcial: la decision declarada.
  expect_identical(mean(tapa(sprintf("%d", numeros))), 1)
  # H2: la misma cifra con cualquier cantidad de ceros o cualquier exponente
  # es el documento entero, tambien el celular con su prefijo de pais.
  digitos <- gsub("[^0-9]", "", cedulas)
  for (forma in c("ci %s.0000", "ci %s,00000000")) {
    expect_identical(mean(tapa(sprintf(forma, digitos))), 1, info = forma)
  }
  expect_true(all(tapa(c(
    "llamar al 59899123456.0000", "llamar al 59899123456,00000000",
    "llamar al 5.9899123456e+010", "x 5.9899123456E010",
    "x 5.9899123456E+0010"
  ))))
  expect_identical(
    .cifra_entera_escrita(c("5.9899123456e+010", "5.9899123456E+0010", "1E+999")),
    c("59899123456", "59899123456", NA)
  )
  # De punta a punta: la nota no protegida en los ejemplos del perfil.
  nac <- sprintf("9%d%06d", sample(1:9, 30L, TRUE), sample(0:999999, 30L))
  d <- data.frame(
    nombre = sprintf("Cliente Apellidoso %03d", 1:30),
    celular = sprintf("0%s %s %s", substr(nac, 1L, 2L), substr(nac, 3L, 5L),
                      substr(nac, 6L, 8L)),
    nota = paste0("llamar al 598", nac, ".0000"),
    stringsAsFactors = FALSE
  )
  d$nota[16:30] <- sprintf("llamar al 5.98%se+010", nac[16:30])
  p <- suppressWarnings(perfilar(
    d, columnas_personales = c(nombre = "nombre_persona", celular = "telefono")
  ))
  hojas <- .r30_hojas(p)
  expect_identical(sum(grepl("llamar al 5", hojas, fixed = TRUE)), 0L)
})

test_that("H4: todo atributo y todo objeto de la copia pasa por el piso", {
  set.seed(3004L)
  n <- 12L
  cedulas <- .r30_cedulas(sample(1000000:6999999, n))
  titulares <- sprintf("Cliente Apellidoso %03d", seq_len(n))
  d <- data.frame(id = seq_len(n), nombre = titulares, cedula = cedulas,
                  stringsAsFactors = FALSE)
  cp <- c(nombre = "nombre_persona", cedula = "documento")
  # Una POSIXlt con `comment` y `label`.
  d$alta <- strptime(sprintf("2024-01-%02d 10:00:00", seq_len(n)),
                     "%Y-%m-%d %H:%M:%S", tz = "UTC")
  attr(d$alta, "comment") <- paste("titular", cedulas[[2L]])
  attr(d$alta, "label") <- titulares[[3L]]
  # `units` en una columna protegida.
  d$nacimiento <- structure(as.Date("1980-01-01") + seq_len(n),
                            units = titulares[[4L]])
  cp <- c(cp, nacimiento = "fecha_nacimiento")
  # Una tabla anidada cuyos nombres de columna son cedulas, un entorno con
  # atributo `name`, una llamada y una funcion, en columnas lista.
  anidada <- data.frame(1, 2, 3)
  names(anidada) <- cedulas[5:7]
  entorno <- new.env()
  entorno$doc <- cedulas[[8L]]
  attr(entorno, "name") <- "registro"
  resto <- rep(list(NULL), n - 1L)
  d$anidada <- I(c(list(anidada), resto))
  d$entorno <- I(c(list(entorno), resto))
  d$llamada <- I(c(list(call("buscar", cedulas[[9L]])), resto))
  d$funcion <- I(c(list(eval(call("function", NULL, cedulas[[10L]]))), resto))
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = cp, conservar_datos = TRUE
  )))
  hojas <- .r30_hojas(an$datos)
  for (valor in c(cedulas[2:10], titulares[3:4])) {
    expect_false(any(grepl(valor, hojas, fixed = TRUE)), info = valor)
  }
  # El entorno del usuario no se toca, y la POSIXlt sigue siendo una.
  expect_identical(get("doc", entorno), cedulas[[8L]])
  expect_s3_class(an$datos$alta, "POSIXlt")
  expect_identical(an$datos$entorno[[1L]], "[valor protegido]")
  expect_identical(an$datos$funcion[[1L]], "[valor protegido]")
  # Lo que no lleva un valor protegido queda como esta.
  expect_identical(.piso_valor_conservado(list(quote(sum(x)), mean, globalenv()),
                                          .valores_identificantes(cedulas)),
                   list(quote(sum(x)), mean, globalenv()))
})

test_that("H5: el nombre de una columna que es un valor protegido conocido no se exceptua", {
  # En el mensaje mismo, con los valores que la llamada conoce.
  previos <- .activar_valores_motor()
  on.exit(.fijar_valores_motor(previos), add = TRUE)
  .sumar_valores_motor("Florencia")
  m <- "ERROR:  invalid input syntax for type integer: \"Florencia\""
  expect_identical(
    .sin_literales_motor(m, identificadores = c("curso", "Florencia")),
    "ERROR:  invalid input syntax for type integer: \"[literal omitido]\""
  )
  # Un nombre que no es un valor queda.
  expect_identical(
    .sin_literales_motor("Referenced column \"curso\" not found",
                         identificadores = c("curso", "Florencia")),
    "Referenced column \"curso\" not found"
  )
  .fijar_valores_motor(previos)
  # De punta a punta, en una coleccion, que junta sus propios valores.
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(3005L)
  # La planilla pivoteada por docente: una columna se llama como la docente,
  # y en un legajo se tipeo su nombre. La vista castea el legajo.
  DBI::dbWriteTable(con, "docentes", data.frame(
    nombre = c("Florencia", sprintf("Cliente Apellidoso %03d", 2:20)),
    celular = sprintf("09%d%06d", sample(1:9, 20L, TRUE),
                      sample(100000:999999, 20L)),
    stringsAsFactors = FALSE
  ))
  planilla <- data.frame(curso = rep(c("1A", "1B"), 10L),
                         legajo_txt = as.character(sample(10000:99999, 20L)),
                         stringsAsFactors = FALSE)
  planilla$legajo_txt[[5L]] <- "Florencia"
  planilla$Florencia <- sample(1:12, 20L, TRUE)
  DBI::dbWriteTable(con, "planilla", planilla)
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_plan AS SELECT curso, CAST(legajo_txt AS INTEGER) AS legajo,",
    "\"Florencia\" FROM planilla"
  ))
  # La vista antes que la tabla de docentes: el piso del final de la coleccion.
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    coleccion(con, c("v_plan", "docentes"))
  )))
  motivos <- c(pc$cobertura_coleccion$motivo, pc$cobertura_metricas$motivo,
               pc$meta$lecturas$motivo)
  expect_identical(sum(grepl("'Florencia'", motivos, fixed = TRUE)), 0L)
  expect_true(any(grepl("Could not convert string", motivos, fixed = TRUE)))
})

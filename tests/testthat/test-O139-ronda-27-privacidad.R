# Ronda 27: lo que la refutacion de la proteccion encontro. Una prueba por
# hallazgo, barata: tablas de decenas o pocos cientos de filas.

# Una tabla donde varias columnas protegidas tienen valores que son palabras de
# los nombres de columna: "Segundo" en `Segundo Nombre` y `Monto Segundo Pago`,
# "Rosario" en `Sucursal Rosario`, "Mercedes" en `Barrio Mercedes`. Nada
# declarado: las protege el clasificador por su nombre.
.r27_tabla_nombres <- function() {
  set.seed(27L)
  n <- 160L
  persona <- rep(seq_len(40L), 4L)
  segundos <- sample(c("Maria", "Jose", "Ines", "Pablo", "Mercedes", "Rosario"),
                     40L, TRUE)
  d <- data.frame(
    id = seq_len(n),
    id_persona = persona,
    pn = sample(c("Segundo", "Rosario", "Mercedes", "Florencia", "Joaquin",
                  "Valentina"), n, TRUE),
    sn = segundos[persona],
    monto = round(stats::rlnorm(n, 5, 1), 2),
    stock = sample(c(1:30, NA), n, TRUE),
    barrio = round(stats::runif(n, 10, 50), 2),
    stringsAsFactors = FALSE
  )
  d$sn[sample(n, 30L)] <- NA
  d$monto[1:3] <- c(99999, 88888, 77777)
  # Y una columna que se llama EXACTAMENTE como un valor protegido -una planilla
  # con una columna por persona-: su nombre es estructura en el plan.
  d$ventas <- round(stats::rlnorm(n, 4, 1), 2)
  d$ventas[4:6] <- c(55555, 66666, 44444)
  names(d) <- c("id", "id_persona", "Primer Nombre", "Segundo Nombre",
                "Monto Segundo Pago", "Sucursal Rosario", "Barrio Mercedes",
                "Mercedes")
  d
}

# Las hojas de texto de un objeto cualquiera, recorriendo listas y tablas.
.r27_hojas <- function(x) {
  if (is.null(x)) return(character())
  if (is.data.frame(x) || is.list(x)) {
    return(unlist(lapply(x, .r27_hojas), use.names = FALSE))
  }
  if (is.factor(x)) return(levels(x))
  if (is.character(x)) return(x)
  character()
}

# Lo que el plan EJECUTA: todos los parametros de todas sus acciones -los que
# hay hoy y los que se agreguen-, mas la columna de cada accion. Se excluye solo
# lo que el plan declara que NO ejecuta tal cual: el mapa con
# `mapa_enmascarado` y los valores con `valores_enmascarados` se resuelven sobre
# los datos que recibe `aplicar()`.
.r27_ejecutables <- function(plan) {
  lapply(seq_len(nrow(plan)), function(i) {
    p <- plan$parametros[[i]]
    if (!is.list(p)) p <- list()
    if (isTRUE(p$mapa_enmascarado)) p$mapa <- NULL
    if (isTRUE(p$valores_enmascarados)) p$valores <- NULL
    p$mapa_enmascarado <- NULL
    p$valores_enmascarados <- NULL
    c(list(columna = plan$columna[[i]]), p)
  })
}

test_that("T1 GUARDA: ningun parametro ejecutable ni nombre de aplicar() se tapa", {
  d <- .r27_tabla_nombres()
  mascara <- "[valor protegido]"
  corridas <- lapply(c(FALSE, TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, proteger_datos_personales = proteger))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    # `analizar()` solo con proteccion: es lo que la guarda mira, y es lo caro.
    an <- if (proteger) {
      suppressWarnings(suppressMessages(analizar(d)))
    }
    activar <- pl$recomendada |
      startsWith(as.character(pl$estrategia), "imputar_dependencia_funcional__")
    pl$aplicar <- activar
    ap <- suppressWarnings(aplicar(pl, d))
    reg <- as.data.frame(ap$registro)
    list(p = p, pl = pl, an = an, ap = ap,
         reg = reg[, c("estrategia", "estado", "n_cambiadas")])
  })
  sin <- corridas[[1L]]
  con <- corridas[[2L]]
  # La tabla ejercita lo que la guarda vigila: varias columnas protegidas, una
  # marca de una de ellas, la imputacion que la nombra y una recomendada.
  protegidas <- .columnas_personales_protegidas(con$p)
  expect_true(all(c("Primer Nombre", "Segundo Nombre") %in% protegidas))
  expect_true(any(con$pl$recomendada))
  expect_true(any(con$pl$estrategia == "marcar_filas_ausentes" &
                    con$pl$columna %in% protegidas))
  expect_true(any(startsWith(as.character(con$pl$estrategia),
                             "imputar_dependencia_funcional__")))
  expect_true(any(con$pl$columna %in% "Mercedes"))
  for (plan in list(con$pl, con$an$plan_limpieza)) {
    hojas <- .r27_hojas(.r27_ejecutables(plan))
    expect_false(any(grepl(mascara, hojas, fixed = TRUE)),
                 info = paste(hojas[grepl(mascara, hojas, fixed = TRUE)],
                              collapse = " | "))
  }
  expect_false(any(grepl(mascara, names(con$ap$datos), fixed = TRUE)))
  expect_identical(names(con$ap$datos), names(sin$ap$datos))
  # Lo que se ejecuta guarda su valor real: los parametros son los mismos que
  # sin proteccion, y `aplicar()` hace lo mismo.
  expect_identical(as.character(con$pl$estrategia),
                   as.character(sin$pl$estrategia))
  ej_con <- .r27_ejecutables(con$pl)
  ej_sin <- .r27_ejecutables(sin$pl)
  for (i in seq_along(ej_con)) {
    if (is.null(ej_con[[i]]$mapa)) ej_sin[[i]]$mapa <- NULL
  }
  expect_identical(ej_con, ej_sin)
  expect_identical(con$reg, sin$reg)
  expect_false(any(con$reg$estado == "fallida"))
})

test_that("T1: la marca de una columna protegida no se tapa ni llega tapada a los datos", {
  n <- 120L
  d <- data.frame(
    id = seq_len(n),
    pn = rep(c("Segundo", "Florencia", "Martina", "Joaquin", "Rosario",
               "Valentina"), length.out = n),
    sn = rep(c("Maria", "Jose", NA, "Ines", "Pablo"), length.out = n),
    stringsAsFactors = FALSE
  )
  names(d) <- c("id", "Primer Nombre", "Segundo Nombre")
  p <- suppressWarnings(perfilar(d))
  expect_true("Segundo Nombre" %in% .columnas_personales_protegidas(p))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  k <- which(pl$estrategia == "marcar_filas_ausentes" &
               pl$columna == "Segundo Nombre")
  expect_length(k, 1L)
  expect_identical(pl$parametros[[k]]$columna_marca, ".ausente_Segundo.Nombre")
  pl$aplicar <- FALSE
  pl$aplicar[k] <- TRUE
  ap <- suppressWarnings(aplicar(pl, d))
  expect_true(".ausente_Segundo.Nombre" %in% names(ap$datos))
  expect_false(any(grepl("[valor protegido]", names(ap$datos), fixed = TRUE)))
  # El valor sigue tapado donde es un valor.
  expect_false(any(grepl("Segundo", unlist(p$columnas$moda), fixed = TRUE)))
})

test_that("T1: el dependiente de la imputacion se conserva y la accion se ejecuta", {
  set.seed(2717L)
  n <- 240L
  personas <- sprintf("P%03d", seq_len(60L))
  pn <- stats::setNames(sample(c("Segundo", "Florencia", "Martina", "Joaquin",
                                 "Rosario"), 60L, TRUE), personas)
  sn <- stats::setNames(sample(c("Maria", "Jose", "Ines", "Pablo", "Lucia",
                                 "Ana"), 60L, TRUE), personas)
  k <- sample(personas, n, TRUE)
  d <- data.frame(id = seq_len(n), id_persona = k, pn = unname(pn[k]),
                  sn = unname(sn[k]), stringsAsFactors = FALSE)
  d$sn[sample(n, 20L)] <- NA
  names(d)[3:4] <- c("Primer Nombre", "Segundo Nombre")
  p <- suppressWarnings(perfilar(d))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  k <- which(startsWith(as.character(pl$estrategia),
                        "imputar_dependencia_funcional__"))
  expect_length(k, 1L)
  expect_identical(pl$parametros[[k]]$dependiente, "Segundo Nombre")
  pl$aplicar <- FALSE
  pl$aplicar[k] <- TRUE
  ap <- suppressWarnings(aplicar(pl, d))
  reg <- as.data.frame(ap$registro)
  expect_identical(reg$estado, "ejecutada")
  expect_gt(reg$n_cambiadas, 0)
})

# 300 filas: `cod_suc` y `sucursal` no son personales, pero la sucursal S01 se
# llama como un titular protegido.
.r27_tabla_sucursales <- function() {
  set.seed(2711L)
  n <- 300L
  suc <- c(S01 = "Farmacia Rodriguez Perez", S02 = "Farmacia Centro Norte",
           S03 = "Farmacia del Puerto", S04 = "Farmacia Las Acacias")
  cod <- sample(names(suc), n, TRUE)
  d <- data.frame(
    id = seq_len(n), cod_suc = cod, sucursal = unname(suc[cod]),
    titular = sample(c("Rodriguez Perez", sprintf("Cliente Apellidoso %03d",
                                                   1:80)),
                     n, TRUE, prob = c(0.1, rep(0.9 / 80, 80))),
    stringsAsFactors = FALSE
  )
  d
}

test_that("T2: el mapa tapado por contenido se reconstruye sobre los datos", {
  base <- .r27_tabla_sucursales()
  for (sentido in c("valor", "clave")) {
    d <- base
    dependiente <- if (sentido == "valor") "sucursal" else "cod_suc"
    d[[dependiente]][c(which(d$cod_suc == "S01")[1:6],
                       which(d$cod_suc == "S02")[1:6])] <- NA
    p <- suppressWarnings(perfilar(d, columnas_personales = "titular"))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    an <- suppressWarnings(suppressMessages(
      analizar(d, columnas_personales = "titular")
    ))
    for (plan in list(pl, an$plan_limpieza)) {
      k <- which(startsWith(as.character(plan$estrategia),
                            "imputar_dependencia_funcional__") &
                   plan$columna == dependiente)
      expect_length(k, 1L)
      parametros <- plan$parametros[[k]]
      # El mapa publicado no lleva el valor, y queda dicho que no se usa tal cual.
      expect_false(any(grepl("Rodriguez", .r27_hojas(parametros$mapa),
                             fixed = TRUE)))
      expect_true(isTRUE(parametros$mapa_enmascarado), info = sentido)
    }
    pl$aplicar <- FALSE
    pl$aplicar[k] <- TRUE
    ap <- suppressWarnings(aplicar(pl, d))
    reg <- as.data.frame(ap$registro)
    expect_identical(reg$estado, "ejecutada", info = sentido)
    expect_identical(as.integer(reg$n_cambiadas), 12L, info = sentido)
    expect_false(anyNA(ap$datos[[dependiente]]), info = sentido)
  }
  # Control: un mapa que la proteccion no toca no se marca, y viaja tal cual.
  d <- base
  d$sucursal[d$sucursal == "Farmacia Rodriguez Perez"] <- "Farmacia Centro Sur"
  d$sucursal[1:12] <- NA
  p <- suppressWarnings(perfilar(d, columnas_personales = "titular"))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  k <- which(startsWith(as.character(pl$estrategia),
                        "imputar_dependencia_funcional__") &
               pl$columna == "sucursal")
  expect_length(k, 1L)
  expect_null(pl$parametros[[k]]$mapa_enmascarado)
})

test_that("F1: una tabla de la coleccion que no se puede leer cierra a las demas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # `clientes` es una vista cuya tabla base ya no esta: leerla falla, como una
  # tabla sin permiso para el rol que perfila.
  DBI::dbExecute(con, "CREATE TABLE base_clientes (cedula TEXT, nombre TEXT)")
  DBI::dbExecute(con, "CREATE VIEW clientes AS SELECT * FROM base_clientes")
  DBI::dbExecute(con, "DROP TABLE base_clientes")
  cedula <- "2.360.786-1"
  obs <- c(rep(paste("pago de Cliente Apellidoso Numero 007 CI", cedula), 60L),
           sprintf("pago de Cliente Apellidoso Numero %03d CI %d.%03d.%03d-%d",
                   1:140, 1:140 %% 6L + 1L, 100L + 1:140, 900L - 1:140,
                   1:140 %% 10L))
  DBI::dbWriteTable(con, "movs", data.frame(id = seq_along(obs), obs = obs,
                                            stringsAsFactors = FALSE))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    coleccion(con, c("clientes", "movs")), conservar_perfiles = TRUE
  )))
  expect_identical(names(pc$perfiles), "movs")
  resumen <- pc$perfiles$movs$resumen_tabla
  otras <- paste(resumen$meta$proteccion_personal$otras_tablas, collapse = " ")
  expect_true(grepl("clientes", otras, fixed = TRUE))
  hojas <- c(.r27_hojas(resumen), .r27_hojas(pc$perfiles$movs$perfil_muestra))
  expect_false(any(grepl(cedula, hojas, fixed = TRUE)))
  expect_false(any(grepl("Numero 007", hojas, fixed = TRUE)))
})

# Cedulas validas de mentira, con su digito verificador.
.r27_cedulas <- function(numeros) {
  vapply(numeros, function(num) {
    s <- sprintf("%07d", num)
    d <- as.integer(strsplit(s, "")[[1L]])
    dv <- (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
    sprintf("%s.%s.%s-%d", substr(s, 1L, 1L), substr(s, 2L, 4L),
            substr(s, 5L, 7L), dv)
  }, character(1L), USE.NAMES = FALSE)
}

test_that("F2: con muestra_motor el relleno no se confirma sobre la muestra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(2709L)
  frecuente <- "2.222.222-2"
  clientes <- .r27_cedulas(sample(1000000:6999999, 2500L))
  cedula <- sample(c(rep(clientes, each = 2L), rep(frecuente, 300L)))
  d <- data.frame(id = seq_along(cedula), cedula = cedula,
                  obs = paste("pago CI", cedula), stringsAsFactors = FALSE)
  # En la tabla entera la cedula frecuente es de alguien: el resto no es casi
  # una clave, y no es un relleno (la regla de `perfilar()` en memoria).
  expect_length(.rellenos_de_columna(d$cedula), 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mov", d)
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "mov", universo = "muestra_motor", muestra_motor = 400L,
    columnas_personales = "cedula"
  )))
  hojas <- c(.r27_hojas(pd$resumen_tabla), .r27_hojas(pd$perfil_muestra))
  expect_false(any(grepl(frecuente, hojas, fixed = TRUE)))
})

test_that("F5: el relleno de una columna no saca del piso el valor de otra", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(27095L)
  relleno <- "1.111.111-1"
  n <- 3000L
  cedula <- sample(c(.r27_cedulas(sample(1000000:6999999, n - 300L)),
                     rep(relleno, 300L)))
  conyuge <- sample(.r27_cedulas(sample(1000000:6999999, 60L)), n, TRUE)
  # La cedula del conyuge que coincide con el relleno, en la ultima fila: fuera
  # de la muestra.
  conyuge[n] <- relleno
  obs <- ifelse(seq_len(n) %% 50L == 0L, paste("ver conyuge CI", relleno),
                paste("mov", seq_len(n) %% 977L))
  d <- data.frame(id = seq_len(n), cedula = cedula, ci_conyuge = conyuge,
                  obs = obs, stringsAsFactors = FALSE)
  # En memoria se decide por columna: es relleno de `cedula` y un valor de
  # `ci_conyuge`, que entra al piso.
  expect_identical(.rellenos_de_columna(d$cedula), relleno)
  expect_length(.rellenos_de_columna(d$ci_conyuge), 0L)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "tc", d)
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(
    con, "tc", muestra = 600L, columnas_personales = c("cedula", "ci_conyuge")
  )))
  hojas <- c(.r27_hojas(pd$resumen_tabla), .r27_hojas(pd$perfil_muestra))
  expect_false(any(grepl(paste("CI", relleno), hojas, fixed = TRUE)))
})

test_that("F3: un valor protegido igual a un nombre de columna no se publica en sus valores", {
  set.seed(2703L)
  docentes <- c("Florencia Rodriguez", "Martin Gomez")
  n <- 200L
  d <- data.frame(
    id = seq_len(n),
    nombre_docente = c(docentes, sprintf("Docente Apellido %03d", seq_len(n - 2L))),
    aprobo = sample(c(docentes, NA), n, TRUE, prob = c(0.45, 0.45, 0.1)),
    a = round(stats::runif(n, 1, 12), 3),
    b = round(stats::runif(n, 1, 12), 3),
    stringsAsFactors = FALSE
  )
  # Planilla pivoteada: una columna de notas por docente.
  names(d)[4:5] <- docentes
  p <- suppressWarnings(perfilar(d))
  expect_true("nombre_docente" %in% .columnas_personales_protegidas(p))
  publicados <- c(.r27_hojas(p$patrones$aprobo),
                  .r27_hojas(attr(p$patrones$aprobo, "resumen_patrones")))
  expect_false(any(grepl("Rodriguez|Gomez", publicados)))
  # Los nombres de columna siguen siendo estructura.
  expect_true(all(docentes %in% p$columnas$columna))
  cv <- suppressWarnings(clasificar_variables(d))
  expect_false(any(grepl("Rodriguez|Gomez", .r27_hojas(cv$niveles_observados))))
  an <- suppressWarnings(suppressMessages(analizar(d)))
  expect_false(any(grepl("Rodriguez|Gomez",
                         .r27_hojas(an$variables$niveles_observados))))
  configuracion <- .r27_hojas(lapply(an$propuesta_modelo$configuracion,
                                     function(x) x$valores))
  expect_false(any(grepl("Rodriguez|Gomez", configuracion)))
  # Y el plan sobre las columnas que se llaman como una docente se ejecuta.
  pl <- suppressWarnings(planificar_limpieza(p, d))
  expect_false(any(grepl("[valor protegido]", .r27_hojas(.r27_ejecutables(pl)),
                         fixed = TRUE)))
})

test_that("F3: el valor igual a un nombre DERIVADO tampoco se publica", {
  set.seed(2701L)
  logins <- c("florencia_rodriguez", "martin_gomez")
  n <- 200L
  d <- data.frame(
    id = seq_len(n),
    usuario = c(logins, sprintf("usuario_%03d", seq_len(n - 2L))),
    cargado_por = sample(logins, n, TRUE),
    a = round(stats::runif(n, 1, 12), 3),
    b = round(stats::runif(n, 1, 12), 3),
    stringsAsFactors = FALSE
  )
  # Las columnas cuyo snake_case es el login protegido.
  names(d)[4:5] <- c("Florencia Rodriguez", "Martin Gomez")
  p <- suppressWarnings(perfilar(d, columnas_personales = "usuario"))
  publicados <- c(.r27_hojas(p$patrones$cargado_por),
                  .r27_hojas(attr(p$patrones$cargado_por, "resumen_patrones")))
  expect_false(any(grepl("rodriguez|gomez", publicados)))
})

test_that("F4: el celular con prefijo de pais pegado (E.164) se tapa, en los dos sentidos", {
  tapa <- function(texto, valor) {
    !identical(.reemplazar_valores_protegidos(texto, valor), texto)
  }
  for (texto in c("whatsapp wa.me/59899123456", "llamar al +59899123456",
                  "llamar al 0059899123456", "+598 99 123 456")) {
    expect_true(tapa(texto, "099 123 456"), info = texto)
  }
  expect_true(tapa("llamar al 099 123 456", "+59899123456"))
  expect_true(tapa("wa.me/59899123456", "+59899123456"))
  # Y en la salida: la moda de una nota que cita el celular.
  d <- data.frame(
    id = seq_len(120L),
    celular = c(rep("099 123 456", 30L),
                sprintf("09%d %03d %03d", 1:90 %% 9 + 1, 100 + 1:90, 200 + 1:90)),
    nota = c(rep("llamar al +59899123456", 60L), rep(c("sin novedad", "ok"), 30L)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(d, columnas_personales = "celular"))
  expect_false(any(grepl("59899123456", .r27_hojas(p$columnas), fixed = TRUE)))
})

test_that("F6: las formas de compatibilidad se leen en el correo y en la fecha con hora", {
  u <- function(...) intToUtf8(c(...))
  tapa <- function(texto, valor) {
    !identical(.reemplazar_valores_protegidos(texto, valor), texto)
  }
  correo <- "juan.perez@empresa.com.uy"
  textos <- c(
    paste0("contacto juan.perez", u(0xFF5F), "at", u(0xFF60), "empresa.com.uy"),
    paste0("contacto juan.perez", u(0x2985), "at", u(0x2986), "empresa.com.uy"),
    paste0("contacto juan.perez", u(0xFE59), "at", u(0xFE5A), "empresa.com.uy"),
    paste0("contacto juan.perez(at)empresa", u(0x3002), "com", u(0x3002), "uy"),
    paste0("contacto juan.perez(at)empresa", u(0xFF61), "com", u(0xFF61), "uy"),
    paste0("contacto juan.perez(at)empresa", u(0xFE52), "com", u(0xFE52), "uy"),
    paste0("contacto juan.perez", u(0x85), "at", u(0x85), "empresa.com.uy")
  )
  for (texto in textos) expect_true(tapa(texto, correo), info = texto)
  momento <- "2024-06-07 09:04:05"
  for (signo in c(0xFE6B, 0xFE54, 0xA6, 0xFFE4)) {
    texto <- paste0("alta 2024-06-07", u(signo), "09:04:05")
    expect_true(tapa(texto, momento), info = sprintf("U+%04X", signo))
  }
  # Las letras de otro alfabeto que se parecen no son el mismo signo: decision.
  expect_false(tapa(paste0("contacto juan.perez (", u(0x430), "t) empresa.com.uy"),
                    correo))
})

test_that("T3: el telefono con prefijo de pais no forma un documento con el prefijo", {
  tapa <- function(texto, valor) {
    !identical(.reemplazar_valores_protegidos(texto, valor), texto)
  }
  # "598" y "4499" formaban "5984499", la cedula 5.984.499-0 sin su verificador.
  for (texto in c("+598 4499 0917", "tel +598 4499 0917", "+598-4499-0917",
                  "(+598) 4499 0917", "00598 4499 0917")) {
    expect_false(tapa(texto, "5.984.499-0"), info = texto)
  }
  expect_false(tapa("+598 2901 1234", "5.982.901-3"))
  # El mismo telefono, si es el protegido, se sigue tapando.
  expect_true(tapa("+598 4499 0917", "4499 0917"))
  expect_true(tapa("+598 2901 1234", "2901-1234"))
  expect_true(tapa("+59829011234", "2901 1234"))
  # Y el numero entero, si es lo protegido.
  expect_true(tapa("ref 59844990917", "59844990917"))
})

test_that("F7: la copia conservada por analizar() no deja el valor en otra columna", {
  set.seed(2719L)
  cedulas <- .r27_cedulas(sample(1000000:6999999, 40L))
  k <- sample(40L, 160L, TRUE)
  d <- data.frame(id = seq_len(160L), cedula = cedulas[k],
                  obs = paste("pago CI", cedulas[k]),
                  monto = round(stats::runif(160L, 10, 999), 2),
                  stringsAsFactors = FALSE)
  a <- suppressWarnings(suppressMessages(
    analizar(d, columnas_personales = "cedula", conservar_datos = TRUE)
  ))
  expect_true(all(a$datos$cedula == "[valor protegido]"))
  expect_false(any(vapply(cedulas, function(v) {
    any(grepl(v, a$datos$obs, fixed = TRUE))
  }, logical(1L))))
  expect_true(all(startsWith(a$datos$obs, "pago CI")))
  # Lo que no es un valor protegido queda como estaba.
  expect_identical(a$datos$monto, d$monto)
  expect_identical(a$datos$id, d$id)
})

test_that("A1: texto latin1 sin marca no hace avisar al barrido de nombres", {
  lat <- function(bytes) {
    x <- rawToChar(as.raw(bytes))
    Encoding(x) <- "unknown"
    x
  }
  set.seed(2722L)
  n <- 120L
  d <- data.frame(
    id = seq_len(n),
    cedula = .r27_cedulas(sample(1000000:6999999, n)),
    stringsAsFactors = FALSE
  )
  # "Jose, cliente" y "Pena, pago" con la tilde y la enie en latin1.
  d$nota <- sample(c(
    lat(c(0x4a, 0x6f, 0x73, 0xe9, 0x2c, 0x20, 0x63, 0x6c, 0x69, 0x65, 0x6e,
          0x74, 0x65)),
    lat(c(0x50, 0x65, 0xf1, 0x61, 0x2c, 0x20, 0x70, 0x61, 0x67, 0x6f)),
    "sin novedad"
  ), n, TRUE)
  avisos <- character()
  p <- withCallingHandlers(
    perfilar(d, columnas_personales = "cedula"),
    warning = function(w) {
      avisos <<- c(avisos, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("invalid UTF-8|invalid in this locale", avisos)))
  expect_s3_class(p, "perfil")
})

test_that("E1: un nombre de columna marcado bytes no hace abortar el plan", {
  set.seed(2723L)
  n <- 120L
  grupo <- sample(sprintf("G%02d", 1:12), n, TRUE)
  categoria <- stats::setNames(sample(c("alta", "media", "baja"), 12L, TRUE),
                               sprintf("G%02d", 1:12))
  d <- data.frame(id = seq_len(n), grupo = grupo,
                  cat = unname(categoria[grupo]), stringsAsFactors = FALSE)
  d$cat[seq(5L, n, by = 10L)] <- NA
  # "notae" con la e con tilde en latin1, declarado `bytes`.
  nombre <- rawToChar(as.raw(c(0x6e, 0x6f, 0x74, 0x61, 0xe9)))
  Encoding(nombre) <- "bytes"
  names(d)[3L] <- nombre
  for (proteger in c(FALSE, TRUE)) {
    p <- suppressWarnings(perfilar(d, proteger_datos_personales = proteger))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    expect_s3_class(pl, "plan_limpieza")
    if (proteger) {
      an <- suppressWarnings(suppressMessages(analizar(d)))
      expect_s3_class(an, "analisis")
    }
    k <- which(startsWith(as.character(pl$estrategia),
                          "imputar_dependencia_funcional__"))
    expect_length(k, 1L)
    pl$aplicar <- FALSE
    pl$aplicar[k] <- TRUE
    ap <- suppressWarnings(aplicar(pl, d))
    reg <- as.data.frame(ap$registro)
    expect_identical(reg$estado, "ejecutada")
    expect_false(anyNA(ap$datos[[3L]]))
  }
})

# La proteccion de datos personales: una prueba por defecto, barata: tablas de
# decenas o pocos cientos de filas.

# Todas las hojas de texto de un objeto, tambien los atributos y los nombres de
# fila: lo que un buscador que solo recorre listas no ve.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r28_hojas <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character())
  salida <- character()
  atributos <- attributes(x)
  for (nombre in setdiff(names(atributos), "class")) {
    valor <- atributos[[nombre]]
    if (identical(nombre, "row.names") && !is.character(valor)) next
    salida <- c(salida, .r28_hojas(valor))
  }
  if (is.list(x)) {
    salida <- c(salida, unlist(lapply(unclass(x), .r28_hojas), use.names = FALSE))
  } else if (is.factor(x)) {
    salida <- c(salida, levels(x))
  } else if (is.atomic(x)) {
    salida <- c(salida, as.character(unclass(x)))
  }
  salida[!is.na(salida)]
}

.r28_cedulas <- function(numeros) {
  vapply(numeros, function(num) {
    s <- sprintf("%07d", num)
    d <- as.integer(strsplit(s, "")[[1L]])
    dv <- (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
    sprintf("%s.%s.%s-%d", substr(s, 1L, 1L), substr(s, 2L, 4L),
            substr(s, 5L, 7L), dv)
  }, character(1L), USE.NAMES = FALSE)
}

test_that("H1: el mensaje del motor se publica sin los literales que cita", {
  # La regla, sobre los mensajes de cuatro motores: el literal se tapa, la frase
  # queda; el apostrofo pegado a letras no corta; la comilla sin cierre tapa
  # hasta el fin de la linea; el contexto JSON de PostgreSQL, sin comillas, se
  # tapa entero.
  mensajes <- c(
    "ERROR:  invalid input syntax for type bigint: \"4.685.467-9\"",
    "Conversion Error: Could not convert string '6.678.340-2' to INT64",
    "Conversion failed when converting the varchar value 'Ana Sosa' to int.",
    "can't convert 'O'Brien' to int",
    "cortado: \"Juan Pe",
    "CONTEXT:  JSON data, line 1: {\"ci\": 5.819.344-",
    "Incorrect syntax near 'LIMIT'.",
    "sin comillas", NA
  )
  esperados <- c(
    "ERROR:  invalid input syntax for type bigint: \"[literal omitido]\"",
    "Conversion Error: Could not convert string '[literal omitido]' to INT64",
    paste0("Conversion failed when converting the varchar value ",
           "'[literal omitido]' to int."),
    "can't convert '[literal omitido]' to int",
    # Fuera de las comillas se publica solo lo permitido -el
    # vocabulario de los mensajes de los motores, SQL, tipos, nombres de la
    # consulta, signos y numeros cortos-; "cortado" y "comillas" no son de un
    # motor y se omiten.
    "[literal omitido]: \"[literal omitido]\"",
    "CONTEXT:  JSON data, line 1: [literal omitido]",
    # Una palabra de SQL entre comillas queda: dice que el dialecto no sirve.
    "Incorrect syntax near 'LIMIT'.",
    "sin [literal omitido]", NA
  )
  expect_identical(.sin_literales_motor(mensajes), esperados)
  tipograficas <- paste0("valor ", intToUtf8(0x201C), "Ana Sosa",
                         intToUtf8(0x201D))
  expect_false(grepl("Ana Sosa", .sin_literales_motor(tipograficas),
                     fixed = TRUE))
})

test_that("H1: perfilar_dbi() no publica la cedula que cita el error del motor", {
  # Una vista castea la cedula y UNA fila la tiene escrita con puntos. El motor
  # la cita en el error de cada consulta que la lee -seis consultas- y `ci`
  # sale protegida.
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(2809L)
  cedulas <- .r28_cedulas(sample(1000000:6999999, 30L))
  clientes <- data.frame(
    id = 1:30, nombre = sprintf("Cliente Apellidoso Numero %03d", 1:30),
    cedula_num = gsub("[^0-9]", "", cedulas), stringsAsFactors = FALSE
  )
  clientes$cedula_num[25L] <- cedulas[25L]
  DBI::dbWriteTable(con, "clientes", clientes)
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_ci AS SELECT id, CAST(cedula_num AS BIGINT) AS ci, nombre",
    "FROM clientes"
  ))
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(con, "v_ci")))
  # La muestra aisla `ci` -falla en la fila 25- sobre sus mismas filas y lee
  # las demas: `ci` no entra en la muestra (antes se perdia
  # la muestra entera y se tapaba todo) y ninguna metrica SQL suya se calcula,
  # porque todas leen la fila 25. Lo que se promete es lo de abajo: la cedula
  # que cita el motor no se publica en ningun lado.
  expect_false("ci" %in% pd$perfil_muestra$columnas$columna)
  fila_muestra <- pd$resumen_tabla$cobertura
  fila_muestra <- fila_muestra[fila_muestra$bloque == "perfil_muestra", , drop = FALSE]
  expect_true(grepl("ci", fila_muestra$motivo, fixed = TRUE))
  sql_ci <- pd$resumen_tabla$sql[pd$resumen_tabla$sql$columna == "ci" &
                                   pd$resumen_tabla$sql$metrica != "n", ]
  expect_true(all(sql_ci$estado %in% c("no_disponible", "no_aplica",
                                       "no_solicitado")))
  hojas <- .r28_hojas(pd)
  expect_identical(sum(grepl(cedulas[25L], hojas, fixed = TRUE)), 0L)
  # El motivo sigue diciendo por que fallo.
  motivos <- as.character(pd$resumen_tabla$sql$motivo)
  expect_true(any(grepl("Conversion Error", motivos, fixed = TRUE) &
                    grepl("[literal omitido]", motivos, fixed = TRUE)))
})

test_that("H2: la copia de analizar() no deja el valor en ninguna parte del data.frame", {
  set.seed(2806L)
  n <- 40L
  cedulas <- .r28_cedulas(sample(1000000:6999999, n))
  docentes <- c("Florencia Rodriguez", "Martin Gomez", "Lucia Pereira",
                "Andres Silveira")
  atencion <- as.POSIXct("2024-06-07 09:04:05", tz = "UTC") +
    sample(1e6, n)
  d <- data.frame(id = seq_len(n), cedula = cedulas,
                  monto = round(stats::runif(n, 10, 999), 2),
                  stringsAsFactors = FALSE)
  # Una columna protegida con etiquetas de valor, como la deja `haven`.
  d$docente <- structure(
    as.double(sample(4L, n, TRUE)),
    labels = stats::setNames(as.double(1:4), docentes),
    label = "Docente a cargo", class = c("haven_labelled", "double")
  )
  d$atencion <- atencion
  d$registro <- atencion
  d$notas <- I(lapply(cedulas, function(v) c("pago", paste("CI", v))))
  rownames(d) <- cedulas
  attr(d, "nota") <- paste("revisar CI", cedulas[[1L]])
  attr(d$monto, "origen") <- paste("titular", cedulas[[2L]])
  an <- suppressWarnings(suppressMessages(analizar(
    d, conservar_datos = TRUE,
    columnas_personales = c(cedula = "documento_identidad",
                            docente = "nombre_persona",
                            atencion = "otro")
  )))
  copia <- an$datos
  hojas <- .r28_hojas(copia)
  # Las cuatro puertas: nombres de fila, etiquetas, la copia POSIXct y la lista;
  # y los atributos de la tabla y de una columna.
  for (v in c(cedulas, docentes)) {
    expect_false(any(grepl(v, hojas, fixed = TRUE)), info = v)
  }
  expect_identical(attr(copia, "row.names"), seq_len(n))
  expect_null(attr(copia$docente, "labels"))
  expect_true(all(is.na(unclass(copia$docente))))
  expect_identical(sum(is.na(copia$registro)), n)
  expect_identical(vapply(copia$notas, `[[`, character(1L), 1L), rep("pago", n))
  # Lo que no lleva un valor protegido queda como estaba.
  expect_equal(as.numeric(copia$monto), as.numeric(d$monto))
  expect_identical(attr(copia$monto, "origen"), "titular [valor protegido]")
  expect_identical(copia$id, d$id)
  expect_s3_class(copia$registro, "POSIXct")
})

# Lo que se ejecuta, con y sin proteccion.
.r28_corridas_plan <- function(d, ..., activar = NULL) {
  lapply(c(FALSE, TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, ..., proteger_datos_personales = proteger))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    if (!is.null(activar)) {
      pl$aplicar <- pl$aplicar | (pl$estrategia %in% activar &
                                    pl$estado == "lista")
    }
    ap <- suppressWarnings(aplicar(pl, d))
    list(pl = pl, reg = as.data.frame(ap$registro), datos = ap$datos)
  })
}

test_that("H3: las acciones de ausencias y de centinelas declarados guardan su valor", {
  set.seed(2803L)
  n <- 200L
  personas <- .r28_cedulas(sample(1000000:6999999, 50L))
  nombres <- paste(sample(c("Ana", "Juan", "Lucia", "Pedro"), 50L, TRUE),
                   sample(c("Fernandez", "Olivera", "Sosa", "Techera"), 50L,
                          TRUE))
  k <- sample(50L, n, TRUE)
  d <- data.frame(
    id = seq_len(n), cedula = personas[k], nombre = nombres[k],
    localidad = sample(c("Salto", "Paysandu", "Rocha", "Florida"), n, TRUE),
    monto = round(stats::runif(n, 100, 9000), 2), stringsAsFactors = FALSE
  )
  # El marcador del catalogo en la protegida; el declarado en la protegida y en
  # la vecina; el centinela declarado de ocho cifras solo en `monto`.
  d$nombre[1:10] <- "sin dato"
  d$nombre[11:20] <- "SIN REGISTRO"
  d$localidad[21:30] <- "SIN REGISTRO"
  d$monto[31:38] <- 99999999
  corridas <- .r28_corridas_plan(
    d, columnas_personales = c(cedula = "documento_identidad",
                               nombre = "nombre_persona"),
    cadenas_ausencia = "SIN REGISTRO", sentinelas_numericos = c(-999, 99999999),
    activar = "convertir_sentinelas_numericos"
  )
  sin <- corridas[[1L]]
  con <- corridas[[2L]]
  for (estrategia in c("convertir_ausencias_textuales",
                       "convertir_sentinelas_numericos")) {
    k_sin <- which(sin$pl$estrategia == estrategia)
    k_con <- which(con$pl$estrategia == estrategia)
    expect_identical(as.character(con$pl$columna[k_con]),
                     as.character(sin$pl$columna[k_sin]))
    expect_identical(lapply(con$pl$parametros[k_con], `[`,
                            c("valores", "cadenas_ausencia")),
                     lapply(sin$pl$parametros[k_sin], `[`,
                            c("valores", "cadenas_ausencia")))
  }
  # A mano: 20 marcadores en `nombre`, 10 en `localidad`, 8 centinelas.
  reg <- con$reg[con$reg$estrategia %in% c("convertir_ausencias_textuales",
                                           "convertir_sentinelas_numericos"), ]
  cambiadas <- stats::setNames(as.integer(reg$n_cambiadas),
                               paste(reg$estrategia, reg$columna))
  expect_identical(
    cambiadas[sort(names(cambiadas))],
    c("convertir_ausencias_textuales localidad" = 10L,
      "convertir_ausencias_textuales nombre" = 20L,
      "convertir_sentinelas_numericos monto" = 8L)
  )
  expect_identical(con$reg[, c("estrategia", "columna", "estado", "n_cambiadas")],
                   sin$reg[, c("estrategia", "columna", "estado", "n_cambiadas")])
  expect_identical(con$datos$localidad, sin$datos$localidad)
  expect_identical(con$datos$monto, sin$datos$monto)
})

test_that("H3: el centinela declarado que es el valor de una protegida se declara", {
  # El 99999999 se repite en una cedula de una tabla de movimientos: puede ser
  # una cedula, y sigue en el piso. La accion de `monto` no lo publica, lo dice
  # (`valores_enmascarados`) y lo resuelve con el relleno de `monto`; la de
  # `cuota`, sin relleno que lo resuelva, queda `bloqueada` y no `fallida`.
  set.seed(2818L)
  n <- 200L
  personas <- .r28_cedulas(sample(1000000:6999999, 40L))
  d <- data.frame(id = seq_len(n), cedula = personas[sample(40L, n, TRUE)],
                  monto = round(stats::runif(n, 100, 9000), 2),
                  cuota = sample(1:12, n, TRUE), stringsAsFactors = FALSE)
  d$cedula[1:12] <- "99999999"
  d$monto[13:22] <- 99999999
  d$cuota[23:30] <- 99999999
  p <- suppressWarnings(perfilar(
    d, columnas_personales = c(cedula = "documento_identidad"),
    sentinelas_numericos = c(-999, 99999999)
  ))
  pl <- suppressWarnings(planificar_limpieza(p, d))
  k <- which(pl$estrategia == "convertir_sentinelas_numericos")
  por_columna <- stats::setNames(k, pl$columna[k])
  for (columna in c("monto", "cuota")) {
    parametros <- pl$parametros[[por_columna[[columna]]]]
    expect_false(99999999 %in% parametros$valores, info = columna)
    expect_true(isTRUE(parametros$valores_enmascarados), info = columna)
  }
  expect_identical(as.character(pl$estado[[por_columna[["monto"]]]]), "lista")
  expect_identical(as.character(pl$estado[[por_columna[["cuota"]]]]),
                   "bloqueada")
  pl$aplicar[por_columna[["monto"]]] <- TRUE
  reg <- as.data.frame(suppressWarnings(aplicar(pl, d))$registro)
  fila <- reg[reg$columna == "monto" &
                reg$estrategia == "convertir_sentinelas_numericos", ]
  expect_identical(fila$estado, "ejecutada")
  expect_identical(as.integer(fila$n_cambiadas), 10L)
})

test_that("H4: el celular E.164 pegado a otro numero se prueba por su parte nacional", {
  protegido <- "099 123 456"
  pegados <- c(
    "+59899123456/+59899765432", "+59899765432/+59899123456",
    "+59899123456-+59899765432", "59899123456/59899765432",
    "+59899123456/099765432", "099765432/+59899123456",
    "29011234/+59899123456", "Llamar al +59891106924/+59899123456 por reclamo"
  )
  tapados <- .reemplazar_valores_protegidos(pegados, protegido)
  expect_identical(sum(tapados != pegados), length(pegados))
  # Controles: la misma forma sin el protegido no se toca.
  controles <- c("+59899765432/+59899111111", "+59899765432",
                 "29011234/+59894444444")
  expect_identical(.reemplazar_valores_protegidos(controles, protegido),
                   controles)

  # Punta a punta: la prosa con dos celulares unidos por una barra.
  set.seed(2810L)
  celulares <- sprintf("09%d %03d %03d", sample(1:9, 60L, TRUE),
                       sample(0:999, 60L), sample(0:999, 60L))
  e164 <- paste0("+598", gsub("^0| ", "", celulares))
  d <- data.frame(
    id = 1:120, celular = rep(celulares, 2L),
    obs = c(paste0("Llamar al ", e164[1:30], "/+59899737569 por reclamo"),
            rep("Reclamo por factura", 90L)),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(d, columnas_personales = c(celular = "telefono")))
  hojas <- .r28_hojas(p)
  for (v in e164[1:30]) {
    expect_false(any(grepl(v, hojas, fixed = TRUE)), info = v)
  }
})

test_that("H5: un miembro declarado 'vista' se registra y las demas cierran", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cedula <- "2.360.786-1"
  DBI::dbWriteTable(con, "clientes", data.frame(
    cedula = c(cedula, .r28_cedulas(1000001:1000039)),
    nombre = sprintf("Cliente Apellidoso Numero %03d", 7:46),
    stringsAsFactors = FALSE
  ))
  obs <- c(rep(paste("pago de Cliente Apellidoso Numero 007 CI", cedula), 60L),
           sprintf("pago de Cliente Apellidoso Numero %03d CI %d.%03d.%03d-%d",
                   1:140, 1:140 %% 6L + 1L, 100L + 1:140, 900L - 1:140,
                   1:140 %% 10L))
  DBI::dbWriteTable(con, "movs", data.frame(id = seq_along(obs), obs = obs,
                                            stringsAsFactors = FALSE))
  col <- coleccion(con, data.frame(tabla = c("clientes", "movs"),
                                   tipo = c("vista", "tabla"),
                                   stringsAsFactors = FALSE))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    col, conservar_perfiles = TRUE
  )))
  expect_identical(names(pc$perfiles), "movs")
  resumen <- pc$perfiles$movs$resumen_tabla
  otras <- paste(resumen$meta$proteccion_personal$otras_tablas, collapse = " ")
  expect_true(grepl("clientes", otras, fixed = TRUE))
  hojas <- c(.r28_hojas(resumen), .r28_hojas(pc$perfiles$movs$perfil_muestra))
  expect_false(any(grepl(cedula, hojas, fixed = TRUE)))
  expect_false(any(grepl("Numero 007", hojas, fixed = TRUE)))
})

# El mapa enmascarado sobre otras entregas, con y sin proteccion.
test_that("H6: con el mapa enmascarado, aplicar() valida como sin proteccion", {
  set.seed(2801L)
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
  na_s01 <- which(d$cod_suc == "S01")[1:6]
  na_s02 <- which(d$cod_suc == "S02")[1:6]
  d$sucursal[c(na_s01, na_s02)] <- NA
  s01 <- d
  s01$sucursal[!is.na(s01$sucursal) & s01$cod_suc == "S01"] <- "Farmacia Plaza Nueva"
  s02 <- d
  s02$sucursal[!is.na(s02$sucursal) & s02$cod_suc == "S02"] <- "Farmacia Centro Sur"
  s05 <- d
  nuevas <- which(d$cod_suc %in% c("S03", "S04"))[1:20]
  s05$cod_suc[nuevas] <- "S05"
  s05$sucursal[nuevas] <- "Farmacia Sucursal Cinco"
  s05$sucursal[nuevas[1:4]] <- NA
  entregas <- list(mismos = d, s01 = s01, s02 = s02, s05 = s05)
  resultados <- lapply(c(FALSE, TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, columnas_personales = "titular",
                                   proteger_datos_personales = proteger))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    k <- which(startsWith(as.character(pl$estrategia),
                          "imputar_dependencia_funcional__") &
                 pl$columna == "sucursal")
    expect_length(k, 1L)
    expect_identical(isTRUE(pl$parametros[[k]]$mapa_enmascarado), proteger)
    pl$aplicar <- FALSE
    pl$aplicar[k] <- TRUE
    lapply(entregas, function(x) {
      reg <- as.data.frame(suppressWarnings(aplicar(pl, x))$registro)
      c(estado = reg$estado, n = as.character(reg$n_cambiadas))
    })
  })
  # A mano: la misma entrega llena las 12; la renombrada en claro (S02) o
  # tapada (S01) contradice el plan; con S05 solo se llenan las 12 de S01 y S02
  # -las 4 de S05 no estan en el plan-.
  esperado <- list(mismos = c(estado = "ejecutada", n = "12"),
                   s01 = c(estado = "fallida", n = "0"),
                   s02 = c(estado = "fallida", n = "0"),
                   s05 = c(estado = "ejecutada", n = "12"))
  expect_identical(resultados[[1L]], esperado)
  expect_identical(resultados[[2L]], esperado)
})

test_that("H7: un campo que refiere columnas publica la columna aunque mezcle (tabla)", {
  set.seed(2813L)
  docentes <- c("Florencia Rodriguez", "Martin Gomez", "Lucia Pereira",
                "Andres Silveira")
  n <- 120L
  d <- data.frame(id = seq_len(n), docente = sample(docentes, n, TRUE),
                  curso = sample(c("A1", "B2", "C3", "D4"), n, TRUE),
                  stringsAsFactors = FALSE)
  for (docente in docentes) d[[docente]] <- round(stats::runif(n, 1, 12))
  d$turno <- c(A1 = "Matutino", B2 = "Vespertino", C3 = "Nocturno",
               D4 = "Matutino")[d$curso]
  d[["Martin Gomez"]] <- d[["Florencia Rodriguez"]] * 2
  d$turno[sample(n, 10L)] <- NA
  d[["Lucia Pereira"]][sample(n, 8L)] <- NA
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = c(docente = "nombre_persona")
  )))
  # La columna `Lucia Pereira` es el objeto de su medida de completitud; las dos
  # columnas proporcionales, el origen de la propuesta.
  expect_true("Lucia Pereira" %in% as.character(an$medicion$objeto_medible))
  expect_true("Lucia Pereira" %in% as.character(an$tablero$objeto))
  expect_true(any(grepl("dependencia_funcional:Florencia Rodriguez->Martin Gomez",
                        an$propuesta_modelo$origen, fixed = TRUE)))
  for (campo in list(an$medicion$objeto_medible, an$tablero$objeto,
                     an$propuesta_modelo$origen)) {
    expect_false(any(grepl("[valor protegido]", campo, fixed = TRUE)))
  }
  # Los valores de la docente siguen tapados.
  expect_identical(
    as.character(an$perfil$columnas$moda[an$perfil$columnas$columna == "docente"]),
    "[valor protegido]"
  )
})

test_that("H8: toda forma de compatibilidad de un signo ASCII se lee como el signo", {
  # Los 28 caracteres que faltaban, con el signo al que los lleva NFKC (Unicode
  # 15.1, recorrido entero): escritos a mano.
  formas <- c(
    "037E" = ";", "1FEF" = "`", "2024" = ".", "207A" = "+", "207C" = "=",
    "207D" = "(", "207E" = ")", "208A" = "+", "208C" = "=", "208D" = "(",
    "208E" = ")", "FB29" = "+", "FE10" = ",", "FE13" = ":", "FE14" = ";",
    "FE15" = "!", "FE16" = "?", "FE33" = "_", "FE34" = "_", "FE35" = "(",
    "FE36" = ")", "FE37" = "{", "FE38" = "}", "FE47" = "[", "FE48" = "]",
    "FE4D" = "_", "FE4E" = "_", "FE4F" = "_"
  )
  caracteres <- vapply(strtoi(names(formas), 16L), intToUtf8, character(1L))
  expect_identical(unname(.ancho_y_espacios_ascii(caracteres)), unname(formas))
  u <- function(...) intToUtf8(c(...))
  correo <- "juan.perez@empresa.com.uy"
  escritos <- c(
    paste0("juan.perez", u(0x207D), "at", u(0x207E), "empresa.com.uy"),
    paste0("juan.perez", u(0x208D), "at", u(0x208E), "empresa.com.uy"),
    paste0("juan.perez", u(0xFE35), "at", u(0xFE36), "empresa.com.uy"),
    paste0("juan.perez", u(0x207D), "arroba", u(0x207E), "empresa.com.uy"),
    paste0("juan.perez", u(0x207D), "a", u(0x207E), "empresa.com.uy"),
    paste0("juan.perez(at)empresa", u(0x2024), "com", u(0x2024), "uy")
  )
  tapados <- .reemplazar_valores_protegidos(escritos, correo)
  expect_identical(sum(tapados != escritos), length(escritos))
  # Control: otro correo escrito igual no se toca.
  otro <- paste0("ana.sosa", u(0x207D), "at", u(0x207E), "empresa.com.uy")
  expect_identical(.reemplazar_valores_protegidos(otro, correo), otro)
})

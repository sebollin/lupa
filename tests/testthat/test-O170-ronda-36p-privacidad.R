# O170: ronda 36-P, la refutacion de la proteccion tras la ronda 35. Dos altos
# de la coleccion -H1, la copia sin verificador escrita como texto de cifras en
# OTRA tabla no era candidata y publicaba moda y ejemplos; H2, la lectura de las
# candidatas no pasaba por la conversion del motor (`money`, `real`)-, las
# formas comunes que el clasificador por valores no reconocia (H3/H4) y el
# tapado de mas que la ronda 35 trajo (H5). Cada bloque rehace la cifra a mano y
# falla contra el commit de la ronda 35 (a042822).

# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r36p_dv <- function(x) {
  d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
  (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
}

.r36p_cedula <- function(x) {
  sprintf("%d.%03d.%03d-%d", x %/% 1000000L, (x %/% 1000L) %% 1000L,
          x %% 1000L, vapply(x, .r36p_dv, integer(1L)))
}

# Los textos de las hojas de un objeto.
.r36p_textos <- function(x) {
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(unclass(x))
  textos[!is.na(textos)]
}

.r36p_numeros <- function(x) {
  numeros <- numeric()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.numeric(y)) {
      numeros <<- c(numeros, as.numeric(y))
    }
    invisible()
  }
  recorrer(unclass(x))
  numeros[!is.na(numeros)]
}

# Las cedulas sin verificador `nums` que el objeto publica, como numero o
# escritas en un texto con puntos, comas, espacios, ceros decimales o exponente.
.r36p_cifras_publicadas <- function(x, nums) {
  textos <- .r36p_textos(x)
  tramos <- unlist(regmatches(
    textos, gregexpr("[0-9][0-9., ]*[0-9](?:e[0-9]+)?", textos, perl = TRUE)
  ), use.names = FALSE)
  exponente <- grepl("e[0-9]+$", tramos)
  leidos <- suppressWarnings(as.numeric(tramos[exponente]))
  tramos <- tramos[!exponente]
  tramos <- tramos[!grepl("[.,]([0-9][1-9]|[1-9][0-9]?)$", tramos)]
  tramos <- gsub("[., ]", "", sub("[.,]0+$", "", tramos))
  numeros <- .r36p_numeros(x)
  unique(c(as.character(numeros[numeros %in% nums]),
           tramos[tramos %in% as.character(nums)],
           as.character(leidos[!is.na(leidos) & leidos %in% nums])))
}

test_that("H1: en una coleccion, la copia sin verificador escrita como texto de cifras en OTRA tabla se protege, como en una tabla sola", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(3611L)
  n <- 40L
  nums <- sample(1000000L:6999999L, n)
  k <- c(1L, 1L, seq_len(n))
  grupos <- function(sep, fin) {
    paste0(sprintf("%d%s%03d%s%03d", nums[k] %/% 1000000L, sep,
                   (nums[k] %/% 1000L) %% 1000L, sep, nums[k] %% 1000L), fin)
  }
  personas <- data.frame(id = seq_len(n), cedula = .r36p_cedula(nums),
                         stringsAsFactors = FALSE)
  # Cuatro escrituras que `perfilar()` reconoce en una tabla sola y que se
  # infieren texto o identificador: no publican un extremo numerico.
  tramites <- data.frame(
    id = seq_along(k),
    ref_coma = grupos(".", ",00"),
    ref_espacio = grupos(" ", ".0"),
    ref_cient = sprintf("%.6fe6", nums[k] / 1e6),
    ref_sd = ifelse(seq_along(k) %% 4L == 0L, "S/D", paste0(nums[k], ".0")),
    stringsAsFactors = FALSE
  )
  copias <- c("ref_coma", "ref_espacio", "ref_cient", "ref_sd")
  # La cifra a mano: el numero que el paquete lee en cada forma es la cedula
  # sin su verificador.
  expect_equal(as.numeric(gsub(" ", "", sub("[.]0$", "", tramites$ref_espacio))),
               as.numeric(nums[k]))
  expect_equal(as.numeric(tramites$ref_cient), as.numeric(nums[k]))
  # En una tabla sola, las cuatro se protegen.
  juntas <- cbind(personas[k, , drop = FALSE], tramites[, copias])
  pj <- suppressWarnings(perfilar(juntas))
  expect_true(all(copias %in% pj$datos_personales$columna[
    pj$datos_personales$proteger]))

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", personas)
  DBI::dbWriteTable(con, "tramites", tramites)
  pc <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("personas", "tramites")), conservar_perfiles = TRUE
  ))
  pt <- pc$perfiles$tramites
  expect_setequal(intersect(copias,
                            pt$resumen_tabla$meta$proteccion_personal$columnas),
                  copias)
  expect_identical(.r36p_cifras_publicadas(pc, nums), character())
  sin <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("personas", "tramites")), conservar_perfiles = TRUE,
    proteger_datos_personales = FALSE
  ))
  expect_gt(length(.r36p_cifras_publicadas(sin, nums)), 0L)
})

test_that("H2: la coleccion lee las candidatas a copia con la expresion con que las leyo perfilar_dbi()", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(ref = c(10L, 20L, 20L, 30L)))
  # Una conversion que se ve en el resultado: la lectura la tiene que usar,
  # como la de las columnas protegidas (`money` de PostgreSQL llega en ceros si
  # se lee crudo; ver `.lectura_convertida_dbi()`).
  convertidas <- data.frame(columna = "ref", lectura = '("ref" * 7)',
                            stringsAsFactors = FALSE)
  leidos <- .distintos_columna_coleccion(con, "t", "ref",
                                         convertidas = convertidas)
  expect_true(isTRUE(leidos$ok))
  expect_setequal(as.numeric(leidos$valores), c(70, 140, 210))
  crudos <- .distintos_columna_coleccion(con, "t", "ref")
  expect_setequal(as.numeric(crudos$valores), c(10, 20, 30))
  # Y la misma expresion para las dos lecturas de valores de la coleccion.
  expect_identical(
    .expresion_lectura_coleccion(con, "ref", convertidas)$expresion,
    '("ref" * 7)'
  )
})

test_that("H3/H4: las formas comunes de nombres, telefonos, domicilios y fechas de nacimiento se reconocen en una columna calificada", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  nbsp <- "\u00a0"
  raya <- "\u2013"
  # Un nombre en latin1 sin marca, como lo deja un CSV de Windows leido bajo C.
  jose <- rawToChar(as.raw(c(0x4a, 0x6f, 0x73, 0xe9, 0x20, 0x50, 0xe9, 0x72,
                             0x65, 0x7a)))
  ines <- rawToChar(as.raw(c(0x49, 0x6e, 0xe9, 0x73, 0x20, 0x53, 0x69, 0x6c,
                             0x76, 0x61)))
  d <- data.frame(
    id = seq_len(n),
    responsable_grupo = r(paste0(c("Juan", "Maria", "Carlos"), nbsp,
                                 c("Perez", "Gomez", "Rodriguez"))),
    funcionario_grupo = r(c("Juan A. Silva", "Ana O. Castro", "Luis U. Pena")),
    contacto_tipo = r(c("Juan O'Connor", "Maria O'Neill", "Pedro O'Brien")),
    usuario_grupo = r(c(jose, ines)),
    responsable_canal = r(paste0(c("099", "098", "091"), nbsp,
                                 c("123", "234", "345"), nbsp,
                                 c("456", "567", "678"))),
    celular_estado = r(paste0(c("099", "098", "094"), raya, c("111", "222", "333"),
                              raya, c("444", "555", "666"))),
    celular_tipo = r(c("099.12.34.56", "098.23.45.67", "091.34.56.78")),
    telefono_grupo = r(c("59899123456.0", "59898234567.0", "59894345678.0")),
    usuario_canal = r(c("0059899123456", "0059898234567", "0059891456789")),
    domicilio_tipo = r(c("Br. Artigas 1234", "Cno. Maldonado 5678",
                         "Rbla. Gandhi 123", "Pje. Asilo 456",
                         "Gral. Flores 2345")),
    direccion_grupo = r(paste0(c("Av.", "Calle", "Bvar."), nbsp,
                               c("Italia", "Rivera", "Artigas"), nbsp,
                               c("2345", "1234", "910"))),
    nacimiento_estado = r(as.Date(c("1985-03-14", "1990-07-22", "1978-11-05"))),
    stringsAsFactors = FALSE
  )
  esperado <- c(responsable_grupo = "nombre", funcionario_grupo = "nombre",
                contacto_tipo = "nombre", usuario_grupo = "nombre",
                responsable_canal = "telefono", celular_estado = "telefono",
                celular_tipo = "telefono", telefono_grupo = "telefono",
                usuario_canal = "telefono", domicilio_tipo = "domicilio",
                direccion_grupo = "domicilio",
                nacimiento_estado = "fecha_nacimiento")
  # La cifra a mano: el nombre latin1 no es UTF-8 valido y no lleva marca.
  expect_false(validUTF8(jose))
  expect_identical(Encoding(jose), "unknown")
  p <- suppressWarnings(perfilar(d))
  dp <- p$datos_personales
  protegidas <- dp[dp$proteger, , drop = FALSE]
  expect_setequal(protegidas$columna, names(esperado))
  expect_identical(stats::setNames(protegidas$tipo, protegidas$columna)[
    names(esperado)], esperado)
  columnas <- p$columnas
  for (columna in names(esperado)) {
    expect_identical(columnas$moda[columnas$columna == columna],
                     "[valor protegido]", info = columna)
  }
  expect_true(columnas$minimo_fecha[columnas$columna == "nacimiento_estado"]
              %in% c(NA, "[valor protegido]"))
  # Y por la otra puerta, `medir()`, igual.
  expect_setequal(.columnas_personales_rapidas(d), names(esperado))
})

test_that("H5: no se tapan importes, codigos, medios, lugares, estados ni categorias con una palabra que tambien es un nombre o una marca de calle", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    id = seq_len(n),
    importe_telefono = r(c(1250000L, 1250000L, 2340000L, 3450000L)),
    importe_cliente = r(c("1.250.000", "1.250.000", "2.340.000", "3.450.000")),
    saldo_socio = r(c("1 250 000", "1 250 000", "2 340 000", "3 450 000")),
    cliente_id = r(c("2024-00001", "2024-00001", "2024-00002", "2024-00003")),
    medio_pago_cliente = r(c("Credito", "Credito", "Debito", "Efectivo")),
    pais_nacimiento = r(c("Uruguay", "Uruguay", "Argentina", "Brasil")),
    pais_destino = r(c("Uruguay", "Uruguay", "Chile", "Paraguay")),
    usuario_baja = r(c("Sin baja", "Sin baja", "Baja definitiva",
                       "Baja temporal", "Activo")),
    nivel_beneficiario = r(c("Primer Pilar", "Primer Pilar", "Segundo Pilar",
                             "Tercer Pilar")),
    tipo_cliente = r(c("Convenio Marco", "Convenio Marco", "Norma General",
                       "Plan Max")),
    grupo_funcionario = r(c("Turno Domingo", "Turno Domingo", "Dia Franco",
                            "Linea Blanca")),
    segmento_cliente = r(c("Radio 5 km", "Radio 5 km", "Radio 10 km",
                           "Radio 20 km")),
    estado_socio = r(c("En ruta", "En ruta", "En camino", "Sucursal 1")),
    calle_metros = r(c(12.5, 12.5, 8.5, 22)),
    stringsAsFactors = FALSE
  )
  moda <- function(x) names(sort(table(x), decreasing = TRUE))[[1L]]
  p <- suppressWarnings(perfilar(d))
  expect_identical(p$datos_personales$columna[p$datos_personales$proteger],
                   character())
  columnas <- p$columnas
  for (columna in setdiff(names(d), c("id", "importe_telefono",
                                      "calle_metros"))) {
    expect_identical(columnas$moda[columnas$columna == columna],
                     moda(d[[columna]]), info = columna)
  }
  expect_equal(columnas$maximo[columnas$columna == "importe_telefono"],
               3450000)
  expect_equal(columnas$maximo[columnas$columna == "calle_metros"], 22)
  expect_identical(.columnas_personales_rapidas(d), character())
})

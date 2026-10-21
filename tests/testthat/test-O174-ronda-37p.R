# O174: ronda 37-P, la refutacion de la proteccion tras la ronda 36. Dos altos
# -P-A1, las abreviaturas comunes de cada tipo personal en el nombre de una
# columna (`tel`, `cel`, `nro_tel`) no se reconocian; P-A2, una cedula de seis
# cifras entre las copias de otra tabla bastaba para que la copia se publicara-,
# el tapado de mas que trajeron las formas nuevas de la ronda 36 y las de antes
# (P-G2), los nombres con dos iniciales y las formas bajas de la misma
# construccion. Cada bloque rehace la cifra a mano y falla contra el commit de la
# ronda 36 (9452b8a).

.r37p_dv <- function(x) {
  d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
  (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
}

.r37p_cedula <- function(x) {
  paste0(formatC(x, big.mark = ".", format = "d", decimal.mark = ","), "-",
         vapply(x, .r37p_dv, integer(1L)))
}

.r37p_numeros <- function(x) {
  numeros <- numeric()
  textos <- character()
  recorrer <- function(y) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      for (k in seq_along(y)) recorrer(y[[k]])
    } else if (is.numeric(y)) {
      numeros <<- c(numeros, as.numeric(y))
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(unclass(x))
  list(numeros = numeros[!is.na(numeros)], textos = textos[!is.na(textos)])
}

test_that("P-A1, el recorrido: las abreviaturas comunes de cada tipo personal en el nombre de una columna, solas, con prefijo, con numero y en camelCase", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  valores <- list(
    telefono = r(c("099123456", "098765432", "091111222", "094555666")),
    nombre = r(c("Florencia Rodriguez", "Martina Olivera", "Juan Perez")),
    domicilio = r(c("Av. Italia 2345", "Bvar. Artigas 1120", "Rivera 1234")),
    correo = r(c("ana@correo.com.uy", "luis@correo.com.uy", "eva@correo.com.uy")),
    documento_identidad = r(.r37p_cedula(c(4685467L, 1234567L, 3719285L)))
  )
  palabras <- list(
    telefono = c("telefono", "telefonos", "tel", "telf", "tlf", "tfno", "telef",
                 "fono", "celular", "celulares", "cel", "movil", "contacto"),
    nombre = c("nombre", "nombres", "nom", "apellido", "apellidos", "ape"),
    domicilio = c("direccion", "direcciones", "direc", "dir", "domicilio",
                  "domicilios", "domic", "calle"),
    correo = c("correo", "correos", "email", "emails", "mail", "mails",
               "e_mail"),
    documento_identidad = c("cedula", "ced", "ci", "documento", "doc", "dni")
  )
  formas <- function(palabra) {
    camel <- paste0(toupper(substr(palabra, 1L, 1L)), substring(palabra, 2L))
    c(palabra, paste0("nro_", palabra), paste0("num_", palabra),
      paste0(palabra, "2"), paste0(palabra, "_particular"),
      paste0(camel, "Cliente"))
  }
  fallos <- character()
  recorridas <- 0L
  for (tipo in names(palabras)) {
    x <- valores[[tipo]]
    inferencia <- list(tipo = .inferir_tipo_interno(x)$tipo)
    for (palabra in palabras[[tipo]]) {
      for (columna in formas(palabra)) {
        recorridas <- recorridas + 1L
        clasificacion <- .clasificar_dato_personal(x, columna, inferencia)
        if (!isTRUE(clasificacion$proteger) ||
            !identical(clasificacion$tipo, tipo)) {
          fallos <- c(fallos, paste(columna, clasificacion$tipo))
        }
      }
    }
  }
  # La cifra a mano: 13 + 6 + 8 + 7 + 6 palabras, seis formas cada una.
  expect_identical(recorridas, 6L * (13L + 6L + 8L + 7L + 6L))
  expect_identical(fallos, character())
  # Y por la funcion publica: las cinco columnas de la refutacion.
  cel <- valores$telefono
  d <- data.frame(tel = cel, tel_particular = cel, nro_tel = cel, telef = cel,
                  cel = cel, stringsAsFactors = FALSE)
  p <- suppressWarnings(perfilar(d))
  expect_setequal(p$datos_personales$columna[p$datos_personales$proteger],
                  names(d))
  expect_false(any(unique(cel) %in% .r37p_numeros(p)$textos))
  # `mov` no es una palabra del telefono: es un movimiento.
  control <- data.frame(tipo_mov = r(c("debito", "credito")),
                        mov = r(c(1L, 2L, 3L)), stringsAsFactors = FALSE)
  pc <- suppressWarnings(perfilar(control))
  expect_identical(pc$datos_personales$columna[pc$datos_personales$proteger],
                   character())
})

test_that("P-A2: en una coleccion, la copia de las cedulas de otra tabla se protege aunque haya cedulas de seis cifras", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  ced <- c(854321L, 742110L, 912345L, 4685467L, 2499716L, 1019433L, 3719285L,
           5128904L)
  personas <- data.frame(id = seq_along(ced), cedula = .r37p_cedula(ced),
                         stringsAsFactors = FALSE)
  casos <- list(
    solo6 = ced[c(1L, 1L, 2L, 3L)],
    mezcla = ced[c(1L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)],
    uno_en_50 = c(ced[[1L]], rep(ced[4:8], 10L))
  )
  # La cifra a mano: la cedula de seis cifras sin su verificador es un numero
  # de seis cifras, y su verificador es valido.
  expect_identical(personas$cedula[[1L]], "854.321-2")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", personas)
  for (caso in names(casos)) {
    DBI::dbWriteTable(con, caso, data.frame(id = seq_along(casos[[caso]]),
                                            ref = casos[[caso]]))
  }
  pc <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("personas", names(casos))), conservar_perfiles = TRUE
  ))
  for (caso in names(casos)) {
    perfil <- pc$perfiles[[caso]]
    expect_true("ref" %in% perfil$resumen_tabla$meta$proteccion_personal$columnas,
                info = caso)
    columnas <- perfil$resumen_tabla$columnas
    expect_true(all(is.na(unlist(columnas[columnas$columna == "ref",
                                          c("minimo", "maximo", "mediana")]))),
                info = caso)
  }
  hojas <- .r37p_numeros(pc)
  expect_false(any(ced %in% hojas$numeros))
  # Y en una tabla sola, por `perfilar()`.
  juntas <- data.frame(cedula = personas$cedula[c(1L, 1L, 2L, 3L, 4L, 5L)],
                       ref = ced[c(1L, 1L, 2L, 3L, 4L, 5L)],
                       stringsAsFactors = FALSE)
  pj <- suppressWarnings(perfilar(juntas))
  expect_true("ref" %in% pj$datos_personales$columna[pj$datos_personales$proteger])
})

test_that("P-G2: no se tapan importes, codigos, lugares, causas, edades, estados, motivos ni categorias en columnas con palabra personal", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    id = seq_len(n),
    ajuste_usuario_monto = r(c("-12345.10", "-12345.10", "-2345.50", "-345.75")),
    importe_cliente_dec4 = r(c("1234.5000", "1234.5000", "2345.7500",
                               "3456.1200")),
    monto_socio_esp = r(c("12 345.50", "12 345.50", "23 456.75", "34 567.10")),
    cliente_id_anio_pto = r(c("2024.00001", "2024.00001", "2024.00002",
                              "2024.00003")),
    saldo_cliente = r(c("-1.250.000", "-1.250.000", "-2.340.000")),
    cliente_id_suc = r(c("10-0012345", "10-0012345", "10-0012346")),
    pais_cliente = r(c("Uruguay", "Uruguay", "Argentina", "Brasil")),
    nacionalidad_socio = r(c("Uruguaya", "Uruguaya", "Argentina")),
    departamento_cliente = r(c("Montevideo", "Montevideo", "Canelones")),
    ciudad_domicilio = r(c("Salto", "Salto", "Rivera", "Paysandu")),
    municipio_nacimiento = r(c("Florida", "Florida", "Durazno")),
    causa_defuncion = r(c("Cardiovascular", "Cardiovascular", "Respiratoria",
                          "Oncologica")),
    edad_fallecimiento = r(c(72, 72, 81, 65, 88)),
    anio_nacimiento = r(c(1952, 1952, 1943, 1959)),
    usuario_baja = r(c("Cancelada", "Cancelada", "Rechazada", "Bloqueada")),
    funcionario_baja = r(c("Renuncia", "Renuncia", "Jubilacion", "Despido")),
    tipo_sintoma_paciente = r(c("Dolores musculares agudos",
                                "Dolores musculares agudos",
                                "Dolores articulares cronicos")),
    resultado_socio = r(c("Victoria local", "Victoria local",
                          "Victoria visitante", "Empate")),
    forma_pago_cliente = r(c("Efectivo", "Efectivo", "Tarjeta de Credito")),
    metodo_pago_cliente = r(c("Debito", "Debito", "Transferencia")),
    telefono_plazo_dias = r(c(30, 30, 60, 90)),
    stringsAsFactors = FALSE
  )
  con <- suppressWarnings(perfilar(d))
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_identical(con$datos_personales$columna[con$datos_personales$proteger],
                   character())
  # Celda por celda contra la corrida sin proteccion.
  for (campo in c("moda", "minimo", "maximo", "mediana")) {
    expect_identical(con$columnas[[campo]], sin$columnas[[campo]], info = campo)
  }
  expect_identical(.columnas_personales_rapidas(d), character())
})

test_that("Nombres con dos iniciales o con O' y las formas bajas de la misma construccion se reconocen en una columna calificada", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    responsable_grupo = r(c("Juan M. A. Perez", "Ana L. M. Silva",
                            "Luis J. Gomez")),
    usuario_alta = r(c("Juan M. A. Perez", "A. Rodriguez", "O. Fernandez",
                       "Gomez, A.", "O'Neill")),
    funcionario_tipo = r(c("Maria E. O'Neill", "Carlos A. O'Connor",
                           "Lucia R. Acosta")),
    telefono_estado = r(c("0059899123456.0000", "0059898765432.0000",
                          "0059891234567.0000")),
    tipo_telefono = r(c("99.123.456", "98.765.432", "91.234.567")),
    domicilio_tipo = r(c("Rivera entre Soca y Ellauri 1234",
                         "Cno. El Dorado 123", "Calle A 1234",
                         "Av. Luis A. de Herrera 2345")),
    usuario_canal = r(c(59899123456, 59898765432, 59891234567)),
    stringsAsFactors = FALSE
  )
  esperado <- c(responsable_grupo = "nombre", usuario_alta = "nombre",
                funcionario_tipo = "nombre", telefono_estado = "telefono",
                tipo_telefono = "telefono", domicilio_tipo = "domicilio",
                usuario_canal = "telefono")
  p <- suppressWarnings(perfilar(d))
  dp <- p$datos_personales
  protegidas <- dp[dp$proteger, , drop = FALSE]
  expect_setequal(protegidas$columna, names(esperado))
  expect_identical(stats::setNames(protegidas$tipo, protegidas$columna)[
    names(esperado)], esperado)
  for (columna in names(esperado)) {
    expect_true(p$columnas$moda[p$columnas$columna == columna] %in%
                  c(NA, "[valor protegido]"), info = columna)
  }
  expect_setequal(.columnas_personales_rapidas(d), names(esperado))
})

test_that("Segunda vuelta: la regla de quien actua no toma por estados los nombres completos con apellidos de terminacion de participio", {
  set.seed(3721L)
  n <- 120L
  # Nombres de pila de la lista y de fuera de ella, y apellidos corrientes que
  # terminan como un estado (`Machado`, `Delgado`, `Garrido`, `Fuentes`).
  listados <- c("Juan", "Maria", "Laura", "Florencia", "Diego", "Lucia")
  fuera <- c("Wenceslao", "Ataulfo", "Eufemia", "Prudencio", "Anacleto")
  apellidos <- c("Rodriguez", "Gonzalez", "Silva", "Perez", "Machado",
                 "Delgado", "Maldonado", "Salgado", "Garrido", "Mercado",
                 "Prado", "Hurtado", "Fuentes", "Ventura", "Segura", "Estrada")
  nombres <- function() {
    pila <- ifelse(runif(n) < 0.5, sample(listados, n, TRUE),
                   sample(fuera, n, TRUE))
    paste(pila, sample(apellidos, n, TRUE), sample(apellidos, n, TRUE))
  }
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    usuario_alta = nombres(), funcionario_baja = nombres(),
    responsable_alta = toupper(nombres()),
    usuario_baja = paste0(sample(apellidos, n, TRUE), " ",
                          sample(apellidos, n, TRUE), ", ",
                          sample(c(listados, fuera), n, TRUE)),
    stringsAsFactors = FALSE
  )
  # La cifra a mano: casi la mitad de los nombres de pila no estan en la lista,
  # y mas de un tercio de los apellidos termina como un estado.
  expect_true(mean(apellidos %in% c("Machado", "Delgado", "Maldonado", "Salgado",
                                    "Garrido", "Mercado", "Prado", "Hurtado",
                                    "Fuentes", "Ventura", "Segura",
                                    "Estrada")) > 0.5)
  p <- suppressWarnings(perfilar(d))
  expect_setequal(p$datos_personales$columna[p$datos_personales$proteger],
                  names(d))
  textos <- .r37p_numeros(p)$textos
  expect_false(any(unique(unlist(d)) %in% textos))
  # Y los estados y los motivos de una baja siguen publicandose.
  estados <- data.frame(
    usuario_baja = r(c("Cancelada", "Cancelada", "Rechazada", "Bloqueada")),
    funcionario_baja = r(c("Renuncia", "Renuncia", "Jubilacion", "Despido")),
    stringsAsFactors = FALSE
  )
  pe <- suppressWarnings(perfilar(estados))
  expect_identical(pe$datos_personales$columna[pe$datos_personales$proteger],
                   character())
  expect_identical(pe$columnas$moda, c("Cancelada", "Renuncia"))
})

test_that("Segunda vuelta: un nombre de lugar, un documento sin cifras y la direccion de una red o de un organismo no se tapan", {
  n <- 24L
  r <- function(v) rep(v, length.out = n)
  d <- data.frame(
    nombre_departamento = r(c("Montevideo", "Montevideo", "Canelones",
                              "Maldonado")),
    nom_depto = r(c("Salto", "Salto", "Paysandu", "Rivera")),
    nom_dpto = r(c("Florida", "Florida", "Durazno")),
    nombre_localidad = r(c("Las Piedras", "Las Piedras", "Pando", "Young")),
    nom_localidad = r(c("Carmelo", "Carmelo", "Young")),
    cedula_valida = r(c("S", "S", "N")),
    ced_valida = r(c("N", "N", "S")),
    cod_documento = r(c("1", "1", "2", "3", "9")),
    cod_doc = r(c(1L, 1L, 2L, 9L)),
    dir_web = r(c("https://www.gub.uy", "https://www.gub.uy",
                  "https://www.bps.gub.uy")),
    dir_ip = r(c("10.1.2.3", "10.1.2.3", "192.168.0.10", "172.16.5.4")),
    dir_general = r(c("Direccion de Salud", "Direccion de Salud",
                      "Direccion de Cultura")),
    stringsAsFactors = FALSE
  )
  con <- suppressWarnings(perfilar(d))
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_identical(con$datos_personales$columna[con$datos_personales$proteger],
                   character())
  for (campo in c("moda", "minimo", "maximo")) {
    expect_identical(con$columnas[[campo]], sin$columnas[[campo]], info = campo)
  }
  # Con nombres de personas bajo esos nombres, los valores confirman y se
  # protegen.
  personas <- data.frame(
    nombre_departamento = r(c("Juan Perez", "Maria Silva", "Luis Gomez")),
    nom_localidad = r(c("Ana Rodriguez", "Diego Sosa", "Lucia Acosta")),
    stringsAsFactors = FALSE
  )
  pp <- suppressWarnings(perfilar(personas))
  expect_setequal(pp$datos_personales$columna[pp$datos_personales$proteger],
                  names(personas))
})

# Ronda 29: lo que la refutacion de la proteccion encontro. Una prueba por
# hallazgo, barata: tablas de decenas o pocos cientos de filas.

# Todas las hojas de texto de un objeto, tambien los atributos y los nombres de
# fila.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r29_hojas <- function(x) {
  if (is.null(x) || is.function(x) || is.environment(x)) return(character())
  salida <- character()
  atributos <- attributes(x)
  for (nombre in setdiff(names(atributos), "class")) {
    valor <- atributos[[nombre]]
    if (identical(nombre, "row.names") && !is.character(valor)) next
    salida <- c(salida, .r29_hojas(valor))
  }
  if (is.list(x)) {
    salida <- c(salida, unlist(lapply(unclass(x), .r29_hojas), use.names = FALSE))
  } else if (is.factor(x)) {
    salida <- c(salida, levels(x))
  } else if (is.atomic(x) && !is.raw(x)) {
    salida <- c(salida, as.character(unclass(x)))
  }
  salida[!is.na(salida)]
}

.r29_cedulas <- function(numeros) {
  vapply(numeros, function(num) {
    s <- sprintf("%07d", num)
    d <- as.integer(strsplit(s, "")[[1L]])
    dv <- (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
    sprintf("%s.%s.%s-%d", substr(s, 1L, 1L), substr(s, 2L, 4L),
            substr(s, 5L, 7L), dv)
  }, character(1L), USE.NAMES = FALSE)
}

# Planilla pivoteada: la docente protegida, determinada por el curso, y una
# columna de notas por docente que se llama como ella. Faltan docentes y
# cursos: el plan imputa en los dos sentidos.
.r29_pivot <- function() {
  docentes <- c("1A" = "Juan Perez Gonzalez", "1B" = "Maria Lopez Fernandez",
                "2A" = "Carlos Rodriguez Silva", "2B" = "Ana Martinez Suarez")
  cursos <- rep(names(docentes), 10L)
  d <- data.frame(curso = cursos, docente = unname(docentes[cursos]),
                  stringsAsFactors = FALSE)
  set.seed(2917L)
  for (k in docentes) d[[k]] <- round(stats::runif(nrow(d), 1, 12))
  d$docente[c(3L, 9L, 15L)] <- NA
  d$curso[c(5L, 11L)] <- NA
  list(d = d, docentes = unname(docentes))
}

test_that("H1: el mapa de la imputacion no se publica aunque sus valores sean nombres de columna", {
  pivot <- .r29_pivot()
  d <- pivot$d
  cp <- c(docente = "nombre_persona")
  planes <- lapply(c(FALSE, TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, columnas_personales = cp,
                                   proteger_datos_personales = proteger))
    suppressWarnings(planificar_limpieza(p, d))
  })
  sin <- planes[[1L]]
  con <- planes[[2L]]
  imputa <- function(plan) {
    which(startsWith(as.character(plan$estrategia),
                     "imputar_dependencia_funcional__"))
  }
  # Los dos sentidos: curso -> docente y docente -> curso.
  expect_setequal(as.character(con$columna[imputa(con)]), c("docente", "curso"))
  expect_identical(as.character(con$estrategia[imputa(con)]),
                   as.character(sin$estrategia[imputa(sin)]))
  nombres_en <- function(mapa) {
    sum(unlist(lapply(mapa, as.character), use.names = FALSE) %in%
          pivot$docentes)
  }
  for (k in seq_along(imputa(con))) {
    i <- imputa(con)[[k]]
    expect_true(isTRUE(con$parametros[[i]]$mapa_enmascarado))
    expect_identical(nombres_en(con$parametros[[i]]$mapa), 0L)
    # Sin la proteccion el mapa lleva los cuatro nombres: la prueba mira algo.
    expect_identical(nombres_en(sin$parametros[[imputa(sin)[[k]]]]$mapa), 4L)
  }
  # Lo que se ejecuta no cambia: `aplicar()` escribe lo mismo, celda por celda.
  correr <- function(plan) {
    plan$aplicar <- seq_len(nrow(plan)) %in% imputa(plan)
    ap <- suppressWarnings(aplicar(plan, d))
    list(datos = ap$datos,
         registro = as.data.frame(ap$registro)[, c("estrategia", "estado",
                                                    "n_cambiadas")])
  }
  a_sin <- correr(sin)
  a_con <- correr(con)
  expect_identical(a_con$registro, a_sin$registro)
  expect_true(all(a_con$registro$estado == "ejecutada"))
  expect_identical(sum(a_con$registro$n_cambiadas), 5)
  expect_identical(a_con$datos$docente, a_sin$datos$docente)
  expect_identical(a_con$datos$curso, a_sin$datos$curso)
})

test_that("H2: el mensaje del motor publica solo lo permitido", {
  # Las formas que se escapaban, con mensajes reales: duckdb sin comillas -el
  # valor, la multiplicacion, la cadena que `strptime` repite en otra linea-,
  # SQL Server en sueco, neerlandes y polaco, y un valor de PostgreSQL con una
  # comilla adentro.
  m <- c(
    "Invalid Error: Conversion Error: Type INT64 with value 59899123456 can't be cast because the value is out of range for the destination type INT32 when casting from source column celular",
    "Conversion Error: Could not cast value 59899123456 to DECIMAL(9,0) when casting from source column celular",
    "Out of Range Error: Overflow in multiplication of INT64 (59899123456 * 1000000000)!",
    "Invalid Input Error: Could not parse string \"Calle 18 de Julio 1234\" according to format specifier \"%d/%m/%Y\"\nCalle 18 de Julio 1234\n^\nError: Expected a number",
    "Konvertering av varchar v\u00e4rdet av typen 3.783.715-1 till datatypen int misslyckades.",
    "Konvertering av varchar v\u00e4rdet av typen Juan Perez Gonzalez till datatypen int misslyckades.",
    "De conversie van de varchar-waarde 4.685.467-9 naar gegevenstype int is mislukt.",
    "Konwersja warto\u015bci \u201e3.783.715-1\u201d obiektu varchar na typ danych int nie powiod\u0142a si\u0119.",
    "Fehler beim Konvertieren des Werts \u00bb4.685.467-9\u00ab in den int-Datentyp.",
    "ERROR:  invalid input syntax for type integer: \"Ana \"Anita\" Suarez Gomez\"",
    "Conversion Error: Could not convert string 'Farmacia D' Angelo Martinez' to INT32"
  )
  fuera <- .sin_literales_motor(m)
  for (valor in c("59899123456", "1000000000", "3.783.715-1", "4.685.467-9",
                  "Calle", "Julio", "1234", "Juan", "Perez", "Gonzalez",
                  "Anita", "Suarez", "Gomez", "Angelo", "Martinez")) {
    expect_false(any(grepl(valor, fuera, fixed = TRUE)), info = valor)
  }
  # Y sigue diciendo que fallo, como y donde: tres mensajes reales, antes y
  # despues. El nombre de la columna que se esta leyendo queda.
  limpio <- .sin_literales_motor(m[c(1L, 10L, 5L)],
                                 identificadores = c("celular", "v_cel"))
  expect_identical(limpio, c(
    "Invalid Error: Conversion Error: Type INT64 with value [literal omitido] can't be cast because the value is out of range for the destination type INT32 when casting from source column celular",
    "ERROR:  invalid input syntax for type integer: \"[literal omitido]\" [literal omitido]\"",
    "[literal omitido] varchar [literal omitido] int [literal omitido]."
  ))
  # Lo que ya quedaba: SQL entre comillas, un signo, un numero de linea; y un
  # codigo SQLSTATE, que dice que error fue.
  expect_identical(
    .sin_literales_motor(c("Incorrect syntax near 'LIMIT'.", "near ','",
                           "LINE 1: SELECT 1",
                           "07009: Invalid descriptor index",
                           "SQLSTATE 22P02: invalid input syntax")),
    c("Incorrect syntax near 'LIMIT'.", "near ','", "LINE 1: SELECT 1",
      "07009: Invalid descriptor index", "SQLSTATE 22P02: invalid input syntax")
  )
})

test_that("H2: perfilar_dbi() no publica el valor que el motor cita sin comillas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(2903L)
  cedulas <- .r29_cedulas(sample(1000000:6999999, 20L))
  celular <- as.numeric(sprintf("9%07d", sample(1000000:9999999, 20L)))
  celular[7L] <- 59899123456
  DBI::dbWriteTable(con, "clientes", data.frame(
    nombre = sprintf("Cliente Apellidoso %03d", 1:20), celular = celular,
    nacimiento = rep(c("01/02/1990", "15/07/1985"), 10L), cedula = cedulas,
    stringsAsFactors = FALSE
  ))
  DBI::dbExecute(con, paste(
    "UPDATE clientes SET nacimiento = cedula WHERE nombre = 'Cliente Apellidoso 011'"
  ))
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_cel AS SELECT nombre, CAST(celular AS INTEGER) AS celular,",
    "strptime(nacimiento, '%d/%m/%Y') AS nacimiento FROM clientes"
  ))
  pd <- suppressWarnings(suppressMessages(perfilar_dbi(con, "v_cel")))
  hojas <- .r29_hojas(pd)
  expect_identical(sum(grepl("59899123456", hojas, fixed = TRUE)), 0L)
  expect_identical(sum(grepl(cedulas[[11L]], hojas, fixed = TRUE)), 0L)
  # El motivo sigue diciendo por que fallo, y en que columna.
  motivos <- as.character(pd$resumen_tabla$sql$motivo)
  expect_true(any(grepl("Conversion Error", motivos, fixed = TRUE) &
                    grepl("celular", motivos, fixed = TRUE)))
})

test_that("H2: el piso entre tablas de la coleccion cubre la cobertura y las lecturas", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # El piso solo, sin la segunda pasada del mensaje del motor: lo que el motor
  # cita sin comillas llega a la coleccion, y el celular protegido de
  # `clientes` tiene que taparse ahi con los valores leidos.
  primera <- .sin_literales_motor
  local_mocked_bindings(.mensaje_motor = function(condicion) {
    if ("solo_comillas" %in% names(formals(primera))) {
      primera(conditionMessage(condicion), solo_comillas = TRUE)
    } else {
      primera(conditionMessage(condicion))
    }
  })
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  set.seed(2913L)
  celular <- as.numeric(sprintf("9%07d", sample(1000000:9999999, 20L)))
  celular[7L] <- 59899123456
  DBI::dbWriteTable(con, "clientes", data.frame(
    nombre = sprintf("Cliente Apellidoso %03d", 1:20), celular = celular,
    stringsAsFactors = FALSE
  ))
  DBI::dbExecute(con, paste(
    "CREATE VIEW v_cel AS SELECT nombre, CAST(celular AS INTEGER) AS celular",
    "FROM clientes"
  ))
  pc <- suppressWarnings(suppressMessages(perfilar_coleccion(
    coleccion(con, c("clientes", "v_cel")), conservar_perfiles = TRUE
  )))
  for (parte in list(pc$cobertura_coleccion, pc$cobertura_metricas,
                     pc$meta$lecturas)) {
    expect_identical(sum(grepl("59899123456", .r29_hojas(parte), fixed = TRUE)),
                     0L)
  }
  # El motivo llego: la prueba mira algo.
  expect_true(any(grepl("literal omitido|valor protegido",
                        .r29_hojas(pc$cobertura_metricas))))
})

test_that("H3: el mapa enmascarado sobre otra entrega no escribe a medias sin decirlo", {
  set.seed(2801L)
  n <- 240L
  sucursales <- c(S01 = "Farmacia Rodriguez Perez", S02 = "Farmacia Centro Norte",
                  S03 = "Farmacia del Puerto", S04 = "Farmacia Las Acacias")
  cod <- rep(names(sucursales), length.out = n)
  d <- data.frame(
    sucursal = unname(sucursales[cod]), cod_suc = cod,
    titular = sample(c("Rodriguez Perez", sprintf("Cliente Apellidoso %03d", 1:80)),
                     n, TRUE, prob = c(0.1, rep(0.9 / 80, 80))),
    monto = round(stats::runif(n, 10, 999), 2), stringsAsFactors = FALSE
  )
  d$sucursal[which(d$cod_suc == "S01")[1:6]] <- NA
  d$sucursal[which(d$cod_suc == "S02")[1:6]] <- NA
  # La entrega con solo las filas a completar de S01 -sin ningun par de esa
  # clave-, y la misma sin el titular que tapo la fila del plan.
  solo_na <- d[!(d$cod_suc == "S01" & !is.na(d$sucursal)), ]
  sin_ambos <- solo_na[solo_na$titular != "Rodriguez Perez", ]
  expect_true(any(solo_na$titular == "Rodriguez Perez"))
  planes <- lapply(c(sin = FALSE, con = TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, columnas_personales = "titular",
                                   proteger_datos_personales = proteger))
    pl <- suppressWarnings(planificar_limpieza(p, d))
    k <- which(startsWith(as.character(pl$estrategia),
                          "imputar_dependencia_funcional__") &
                 pl$columna == "sucursal")
    expect_length(k, 1L)
    pl$aplicar <- seq_len(nrow(pl)) == k
    list(plan = pl, k = k)
  })
  corrida <- function(proteger, x) {
    plan <- planes[[if (proteger) "con" else "sin"]]
    ap <- suppressWarnings(aplicar(plan$plan, x))
    reg <- as.data.frame(ap$registro)
    list(plan = plan$plan$parametros[[plan$k]],
         registro = reg[reg$columna == "sucursal", ], datos = ap$datos)
  }
  con_d <- corrida(TRUE, d)
  expect_true(isTRUE(con_d$plan$mapa_enmascarado))
  # Los mismos datos: lo mismo con y sin proteccion, celda por celda.
  sin_d <- corrida(FALSE, d)
  expect_identical(con_d$datos$sucursal, sin_d$datos$sucursal)
  expect_identical(con_d$registro$estado, "ejecutada")
  for (x in list(solo_na, sin_ambos)) {
    sin_x <- corrida(FALSE, x)
    con_x <- corrida(TRUE, x)
    # Sin la proteccion se escriben todas, tambien las de S01.
    expect_identical(sin_x$registro$estado, "ejecutada")
    expect_identical(sum(is.na(sin_x$datos$sucursal)), 0L)
    # Con ella no se puede saber que va en S01: la accion queda `fallida`, con
    # el motivo, y no escribe la mitad en silencio.
    expect_identical(con_x$registro$estado, "fallida")
    expect_match(con_x$registro$error, "tapada", fixed = TRUE)
    expect_identical(con_x$datos$sucursal, x$sucursal)
  }
})

test_that("H4: la columna duplicada que se llama como una persona protegida conserva sus acciones", {
  set.seed(2908L)
  n <- 60L
  d <- data.frame(
    titular = rep(c("Juan Perez Gonzalez", "Maria Lopez Fernandez",
                    "Carlos Rodriguez Silva", "Ana Martinez Suarez"),
                  length.out = n),
    monto = round(stats::runif(n, 10, 999), 2), stringsAsFactors = FALSE
  )
  d[["Juan Perez Gonzalez"]] <- round(stats::runif(n, 1, 12))
  # La columna se llama como un valor protegido que ESTA en los datos.
  expect_true("Juan Perez Gonzalez" %in% d$titular)
  d$copia <- d[["Juan Perez Gonzalez"]]
  cp <- c(titular = "nombre_persona")
  planes <- lapply(c(FALSE, TRUE), function(proteger) {
    p <- suppressWarnings(perfilar(d, columnas_personales = cp,
                                   proteger_datos_personales = proteger))
    list(perfil = p, plan = suppressWarnings(planificar_limpieza(p, d)))
  })
  duplicadas <- function(plan) {
    k <- which(plan$estrategia %in% c("marcar_columnas_duplicadas",
                                      "eliminar_columna_duplicada"))
    list(estado = as.character(plan$estado[k]),
         recomendada = plan$recomendada[k], parametros = plan$parametros[k])
  }
  sin <- duplicadas(planes[[1L]]$plan)
  con <- duplicadas(planes[[2L]]$plan)
  expect_length(sin$estado, 2L)
  expect_identical(con, sin)
  expect_identical(
    planes[[2L]]$perfil$general$columnas_duplicadas$columna_1,
    planes[[1L]]$perfil$general$columnas_duplicadas$columna_1
  )
  # El criterio cubre TODO campo cuyas hojas son nombres de columna, en el
  # perfil y en el plan: el recorrido de la ronda 29, en chico.
  campos <- character()
  recorrer <- function(x, campo) {
    if (is.null(x) || is.function(x) || is.environment(x)) return(invisible())
    for (a in setdiff(names(attributes(x)),
                      c("names", "class", "row.names", "dim", "dimnames"))) {
      recorrer(attr(x, a, exact = TRUE), a)
    }
    if (is.data.frame(x)) {
      for (j in seq_along(x)) recorrer(x[[j]], names(x)[[j]])
    } else if (is.list(x)) {
      nombres <- names(x)
      for (i in seq_along(x)) {
        propio <- if (!is.null(nombres) && nzchar(nombres[[i]])) nombres[[i]] else campo
        recorrer(x[[i]], propio)
      }
    } else if (is.character(x) || is.factor(x)) {
      v <- as.character(if (is.factor(x)) levels(x) else x)
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) && all(v %in% names(d)) && !is.null(campo)) {
        campos <<- c(campos, campo)
      }
    }
    invisible()
  }
  recorrer(planes[[1L]]$perfil, NULL)
  recorrer(planes[[1L]]$plan, NULL)
  campos <- unique(campos)
  expect_true(all(c("columna_1", "columna_2", "eliminar") %in% campos))
  cubiertos <- vapply(campos, .refiere_columnas, logical(1L))
  expect_true(all(cubiertos), info = paste(campos[!cubiertos], collapse = ", "))
})

test_that("H5: el celular escrito como numero de planilla es la misma cifra", {
  notas <- c("llamar al 093 897 037", "llamar al 59893897037.0",
             "llamar al 5.9893897037E+10")
  for (celular in c("59893897037.0", "59893897037,0", "5.9893897037E+10")) {
    tapadas <- .reemplazar_valores_protegidos(
      notas, .valores_identificantes(celular)
    )
    expect_false(any(grepl("093 897 037|59893897037|9893897037E", tapadas)),
                 info = celular)
  }
  # Al reves: el protegido en su forma nacional, el texto como planilla.
  tapadas <- .reemplazar_valores_protegidos(
    c("cel 59899123456.0", "cel 59899123456,0", "cel 5.9899123456E+10"),
    .valores_identificantes("099 123 456")
  )
  expect_false(any(grepl("5989|9899", tapadas)))
  # Y el piso numerico: el mismo numero guardado como doble.
  expect_true(is.na(.proteger_numeros_parametros(59893897037, "59893897037.0")))
  # La cifra entera de lo escrito asi; lo que tiene decimales no es una.
  expect_identical(
    .cifra_entera_escrita(c("59893897037.00", "12.5", "1.5E+3",
                            "5.9893897037E+10", "1.5E+0")),
    c("59893897037", NA, "1500", "59893897037", NA)
  )
  # En un texto, un millar no es un decimal de ceros: "1.000" no se lee. Desde
  # la ronda 30 la funcion devuelve las cifras enteras -con cualquier cantidad
  # de ceros detras de seis cifras- y no reescribe el texto: la cifra entera
  # entra solo como documento entero.
  expect_identical(
    .cifras_enteras_en_texto("de 1.000 a 123456.000; cel 59899123456.0"),
    c("123456", "59899123456")
  )
  # De punta a punta: la nota no protegida en la copia y en el perfil.
  set.seed(2912L)
  nac <- sprintf("09%d %03d %03d", sample(1:9, 30L, TRUE), sample(0:999, 30L),
                 sample(0:999, 30L))
  d <- data.frame(
    nombre = sprintf("Cliente Apellidoso %03d", 1:30),
    nota = paste("llamar al", nac),
    celular = paste0("598", gsub("^0| ", "", nac), ".0"),
    stringsAsFactors = FALSE
  )
  p <- suppressWarnings(perfilar(
    d, columnas_personales = c(nombre = "nombre_persona", celular = "telefono")
  ))
  expect_false(any(grepl("llamar al 09", .r29_hojas(p$patrones$nota))))
})

test_that("H6: una columna POSIXlt no hace abortar la copia protegida", {
  set.seed(2916L)
  n <- 20L
  cedulas <- .r29_cedulas(sample(1000000:6999999, n))
  d <- data.frame(id = seq_len(n), cedula = cedulas,
                  monto = round(stats::runif(n, 1, 100), 2),
                  stringsAsFactors = FALSE)
  d$registro <- strptime(sprintf("2024-01-%02d 10:%02d:00", seq_len(n), seq_len(n)),
                         "%Y-%m-%d %H:%M:%S", tz = "UTC")
  an <- suppressWarnings(suppressMessages(analizar(
    d, columnas_personales = c(cedula = "documento"), conservar_datos = TRUE
  )))
  expect_s3_class(an$datos$registro, "POSIXlt")
  expect_identical(sum(is.na(an$datos$registro)), 0L)
  expect_identical(sum(.r29_hojas(an$datos) %in% cedulas), 0L)
  # La POSIXlt que ES la fecha con hora protegida se tapa como la POSIXct; y
  # un data.frame anidado, un entorno que se guarda a si mismo y una lista con
  # clase terminan.
  protegida <- "2024-01-03 10:03:00"
  lt <- d$registro[1:4]
  tapada <- .piso_valor_conservado(lt, .valores_identificantes(protegida))
  expect_identical(is.na(tapada), c(FALSE, FALSE, TRUE, FALSE))
  identificantes <- .valores_identificantes(cedulas)
  anidado <- data.frame(a = cedulas[1:3], b = 1:3, stringsAsFactors = FALSE)
  expect_identical(sum(.piso_valor_conservado(anidado, identificantes)$a %in%
                         cedulas), 0L)
  ciclo <- new.env()
  ciclo$v <- cedulas[[1L]]
  ciclo$yo <- ciclo
  expect_identical(.piso_valor_conservado(list(ciclo), identificantes),
                   list("[valor protegido]"))
  expect_identical(get("v", ciclo), cedulas[[1L]])
  # Una lista con clase cualquiera se recorre sin la clase y la conserva.
  registro <- structure(list(x = cedulas[1:3], y = 1:3), class = "r29_registro")
  tapado <- .piso_valor_conservado(registro, identificantes)
  expect_s3_class(tapado, "r29_registro")
  expect_identical(sum(unclass(tapado)$x %in% cedulas), 0L)
})

test_that("H7: un valor con una comilla adentro no deja su cola en el mensaje", {
  m <- c(
    "ERROR:  invalid input syntax for type integer: \"Calle \"18 de Julio\" 1234 Montevideo\"",
    "Conversion Error: Could not convert string 'Smiths' Bakery Gonzalez' to INT32"
  )
  fuera <- .sin_literales_motor(m)
  # El numero de puerta queda: cuatro cifras no identifican -el piso de la
  # proteccion es de seis-.
  for (trozo in c("Montevideo", "Julio", "Bakery", "Gonzalez")) {
    expect_false(any(grepl(trozo, fuera, fixed = TRUE)), info = trozo)
  }
})

test_that("H8: la unidad, los bytes, los entornos y los niveles pasan por el piso", {
  set.seed(2918L)
  n <- 30L
  titulares <- sprintf("Titular Apellidoso %03d", seq_len(n))
  cedulas <- .r29_cedulas(sample(1000000:6999999, n))
  # Con una columna `id`: el nombre de una columna adentro de una palabra
  # -"Apellidoso"- no es la columna.
  d <- data.frame(id = seq_len(n), titular = titulares, cedula = cedulas,
                  stringsAsFactors = FALSE)
  cp <- c(titular = "nombre_persona", cedula = "documento")
  # Cada unidad en su perfil: una que se tapa no deja barrer a la otra.
  unidad <- function(columna, valor) {
    x <- d
    x[[columna]] <- structure(stats::runif(n), units = valor)
    p <- suppressWarnings(perfilar(x, columnas_personales = cp))
    as.character(p$columnas$unidad[p$columnas$columna == columna])
  }
  expect_identical(unidad("ratio", titulares[[7L]]), "[valor protegido]")
  expect_false(grepl(cedulas[[4L]], unidad("tasa", paste("CI", cedulas[[4L]])),
                     fixed = TRUE))
  # La copia de `analizar()`: un BLOB con los nombres en bytes, un entorno con
  # una cedula y un entero con `levels`.
  d2 <- d[, c("id", "titular", "cedula")]
  d2$blob <- I(lapply(paste("titular", titulares), charToRaw))
  entorno <- new.env()
  entorno$v <- cedulas[[3L]]
  d2$entorno <- I(c(list(entorno), rep(list(NULL), n - 1L)))
  d2$codigo <- structure(seq_len(n), levels = cedulas[1:3])
  an <- suppressWarnings(suppressMessages(analizar(
    d2, columnas_personales = cp, conservar_datos = TRUE
  )))
  bytes <- vapply(an$datos$blob, rawToChar, character(1L))
  expect_identical(sum(vapply(titulares, function(t) {
    any(grepl(t, bytes, fixed = TRUE))
  }, logical(1L))), 0L)
  expect_identical(an$datos$entorno[[1L]], "[valor protegido]")
  expect_identical(get("v", entorno), cedulas[[3L]])
  expect_false(any(attr(an$datos$codigo, "levels") %in% cedulas))
})

test_that("cosmetico: el valor tapado no se come el espacio que lo sigue", {
  # `format()` de la columna de texto rellena con espacios hasta el ancho, y la
  # aguja rellena se comia el espacio de la prosa que la cita.
  d <- data.frame(docente = c("Maria Lopez Fernandez", "Carlos Rodriguez Silva"),
                  stringsAsFactors = FALSE)
  agujas <- .valores_publicables_protegidos(d, "docente")
  expect_false(any(grepl("^\\s|\\s$", agujas)))
  prosa <- paste("La relacion Carlos Rodriguez Silva -> Maria Lopez Fernandez",
                 "es exacta")
  expect_identical(
    .reemplazar_valores_protegidos(prosa, .valores_identificantes(agujas),
                                   exigir_limites = TRUE),
    "La relacion [valor protegido] -> [valor protegido] es exacta"
  )
})

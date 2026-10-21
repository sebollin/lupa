# O166: ronda 35-P, la refutacion de la proteccion tras la ronda 34. Tres
# medios -H1, la copia sin verificador de los documentos de OTRA tabla de una
# coleccion; H2, la copia escrita como numero con decimales en un texto; H3, el
# calificador del nombre que sacaba la proteccion a columnas con nombres,
# logins, celulares y domicilios-. Cada bloque rehace a mano la cifra de la
# sonda, mira las dos caras -lo que se publicaba y lo que no hay que tapar- y
# falla contra el commit de la ronda 34 (afd202a).

# Las hojas de un objeto, numeros y textos por separado.
.r35p_hojas <- function(x) {
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

# Los numeros de `nums` que el objeto publica, como numero o escritos en un
# texto con puntos, comas o ceros decimales.
.r35p_cifras_publicadas <- function(x, nums) {
  hojas <- .r35p_hojas(x)
  blancos <- as.character(nums)
  en_numeros <- unique(hojas$numeros[hojas$numeros %in% nums])
  tramos <- unlist(regmatches(
    hojas$textos, gregexpr("[0-9][0-9., ]*[0-9]", hojas$textos, perl = TRUE)
  ), use.names = FALSE)
  # Un tramo con centavos que no son cero es un importe, no un documento.
  tramos <- tramos[!grepl("[.,]([0-9][1-9]|[1-9][0-9]?)$", tramos)]
  tramos <- gsub("[., ]", "", sub("[.,]0+$", "", tramos))
  unique(c(as.character(en_numeros), tramos[tramos %in% blancos]))
}

.r35p_dv <- function(x) {
  d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
  (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
}

.r35p_cedula <- function(x) {
  sprintf("%d.%03d.%03d-%d", x %/% 1000000L, (x %/% 1000L) %% 1000L,
          x %% 1000L, vapply(x, .r35p_dv, integer(1L)))
}

.r35p_moda <- function(x) names(sort(table(x), decreasing = TRUE))[[1L]]

test_that("H3: el calificador del nombre no saca la proteccion cuando los valores la confirman; los controles siguen publicando", {
  set.seed(3531L)
  n <- 60L
  pila <- c("Florencia", "Martina", "Ximena", "Joaquin", "Ramon", "Lucia")
  apellidos <- c("Rodriguez", "Olivera", "Pereyra", "Silveira", "Acosta",
                 "Ferreira")
  nombres <- function() {
    paste(sample(pila, n, TRUE), sample(apellidos, n, TRUE))
  }
  inicial <- function() tolower(substr(sample(pila, n, TRUE), 1L, 1L))
  calles <- c("Rivera", "Colonia", "Mercedes", "Agraciada", "Millan")
  d <- data.frame(
    id = seq_len(n),
    # Nombres, logins, celulares y domicilios bajo un nombre calificado por el
    # resto: fecha (`alta`, `baja`, `desde`), categoria (`grupo`, `canal`,
    # `estado`, `tipo`) o medio.
    usuario_alta = nombres(), funcionario_baja = nombres(),
    responsable_grupo = nombres(), nombre_jefe_grupo = nombres(),
    usuario_baja_login = paste0(inicial(), tolower(sample(apellidos, n, TRUE)),
                                sample(10:99, n, TRUE)),
    usuario_modif_alta = paste0(inicial(), tolower(sample(apellidos, n, TRUE))),
    celular_estado = sprintf("09%d%06d", sample(1:9, n, TRUE),
                             sample(0:999999, n)),
    contacto_medio = sprintf("09%d%06d", sample(1:9, n, TRUE),
                             sample(0:999999, n)),
    direccion_tipo = paste(sample(calles, n, TRUE), sample(100:4999, n),
                           "apto", sample(1:20, n, TRUE)),
    # Los controles de la ronda 34 y categorias de dos palabras o con un
    # numero: no tienen un nombre de pila ni una marca de calle.
    tipo_cliente = rep(c("minorista", "minorista", "mayorista", "empresa"),
                       length.out = n),
    estado_socio = rep(c("activo", "activo", "suspendido", "baja"),
                       length.out = n),
    cliente_desde = as.character(as.Date("2015-01-01") +
                                  c(rep(0L, 10L), sample(1:3000, n - 10L))),
    cliente_id = sample(1:25, n, TRUE),
    responsable_tipo = rep(c("Persona Fisica", "Persona Fisica",
                             "Persona Juridica", "Persona Publica"),
                           length.out = n),
    telefono_tipo = rep(c("celular", "celular", "fijo", "fax"),
                        length.out = n),
    domicilio_tipo = rep(c("Tipo 1", "Tipo 1", "Tipo 2", "Tipo 3"),
                         length.out = n),
    fecha_contacto = as.character(as.Date("2020-01-01") +
                                   c(rep(0L, 10L), sample(1:900, n - 10L))),
    stringsAsFactors = FALSE
  )
  personales <- c("usuario_alta", "funcionario_baja", "responsable_grupo",
                  "nombre_jefe_grupo", "usuario_baja_login",
                  "usuario_modif_alta", "celular_estado", "contacto_medio",
                  "direccion_tipo")
  controles <- c("tipo_cliente", "estado_socio", "cliente_desde",
                 "responsable_tipo", "telefono_tipo",
                 "domicilio_tipo", "fecha_contacto")
  # La cifra a mano: cada columna personal tiene mas de tres valores distintos,
  # y ninguno se repite en un control.
  for (columna in personales) {
    expect_gt(length(unique(d[[columna]])), 3L)
  }
  expect_length(intersect(unlist(d[personales]), unlist(d[controles])), 0L)

  p <- suppressWarnings(perfilar(d))
  dp <- p$datos_personales
  expect_setequal(dp$columna[dp$proteger], personales)
  tipos <- stats::setNames(dp$tipo, dp$columna)
  expect_true(all(tipos[personales[1:6]] == "nombre"))
  expect_true(all(tipos[c("celular_estado", "contacto_medio")] == "telefono"))
  expect_identical(unname(tipos["direccion_tipo"]), "domicilio")
  # Ningun valor de una columna personal sale en el objeto.
  hojas <- .r35p_hojas(p)
  valores <- unique(unlist(d[personales], use.names = FALSE))
  publicados <- valores[vapply(valores, function(v) {
    any(grepl(v, hojas$textos, fixed = TRUE))
  }, logical(1L))]
  expect_identical(publicados, character())
  # Y los controles publican su moda.
  columnas <- p$columnas
  for (columna in controles) {
    expect_identical(columnas$moda[columnas$columna == columna],
                     .r35p_moda(d[[columna]]), info = columna)
  }
  expect_equal(columnas$maximo[columnas$columna == "cliente_id"],
               max(d$cliente_id))
  # Sin proteccion se publican: la sonda no es ciega.
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  hojas_sin <- .r35p_hojas(sin)
  expect_true(any(vapply(valores, function(v) {
    any(grepl(v, hojas_sin$textos, fixed = TRUE))
  }, logical(1L))))

  # Con pocos valores: dos funcionarios en `usuario_alta` no son una fecha; un
  # codigo `u001` en `usuario_id` y `USR001` en `cod_usuario` siguen siendo
  # claves.
  pocos <- data.frame(
    usuario_alta = rep(c("Martina Olivera", "Ramon Acosta"), length.out = n),
    usuario_id = sprintf("u%03d", seq_len(n)),
    cod_usuario = rep(c("USR001", "USR001", "USR002", "USR003"), length.out = n),
    stringsAsFactors = FALSE
  )
  pp <- suppressWarnings(perfilar(pocos))
  expect_identical(pp$datos_personales$columna[pp$datos_personales$proteger],
                   "usuario_alta")
  expect_identical(pp$columnas$moda[pp$columnas$columna == "cod_usuario"],
                   "USR001")
  expect_false(any(grepl("Martina Olivera", .r35p_hojas(pp)$textos,
                         fixed = TRUE)))

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "auditoria", d)
  pd <- suppressWarnings(perfilar_dbi(con, "auditoria"))
  expect_setequal(pd$resumen_tabla$meta$proteccion_personal$columnas,
                  personales)
  resumen <- pd$resumen_tabla$columnas
  expect_identical(resumen$moda[resumen$columna == "tipo_cliente"],
                   .r35p_moda(d$tipo_cliente))
})

test_that("H2: la copia sin verificador escrita como numero con decimales en un texto se protege entera; el importe con decimales no", {
  set.seed(3521L)
  n <- 150L
  nums <- sample(1000000L:6999999L, n)
  miles <- sprintf("%d.%03d.%03d,00", nums %/% 1000000L,
                   (nums %/% 1000L) %% 1000L, nums %% 1000L)
  importes <- sample(1000000L:6999999L, n)
  d <- data.frame(
    id = seq_len(n), cedula = .r35p_cedula(nums),
    ref_punto = paste0(nums, ".0"), ref_coma = paste0(nums, ",00"),
    ref_miles = miles,
    ref_mezcla = ifelse(seq_len(n) %% 2L == 0L, paste0(nums, ".0"),
                        as.character(nums)),
    importe_txt = paste0(importes, ".00"),
    # Segunda vuelta: notacion cientifica, signo y espacio de miles con ceros
    # decimales. Y el importe con espacio de miles y centavos cuyas cifras, sin
    # la coma, serian la cedula: no es un documento.
    ref_exp = paste0(nums, "e0"), ref_mas = paste0("+", nums, ".0"),
    ref_espacio = sprintf("%d %03d %03d.0", nums %/% 1000000L,
                          (nums %/% 1000L) %% 1000L, nums %% 1000L),
    importe_espacio = sprintf("%d %03d.%02d", nums %/% 100000L,
                              (nums %/% 100L) %% 1000L, nums %% 100L),
    stringsAsFactors = FALSE
  )
  copias <- c("ref_punto", "ref_coma", "ref_miles", "ref_mezcla", "ref_exp",
              "ref_mas", "ref_espacio")
  # Los centavos son cero en menos de tres: el importe no pasa por copia ni
  # quitandole una parte decimal de ceros.
  expect_lt(sum(nums %% 100L == 0L), 3L)
  # La cifra a mano: el numero que el paquete lee en cada texto es la cedula
  # sin su verificador; en el control, casi ninguno coincide.
  expect_identical(as.numeric(sub(",", ".", gsub(".", "", d$ref_miles,
                                                 fixed = TRUE))),
                   as.numeric(nums))
  expect_lt(sum(importes %in% nums), 3L)

  p <- suppressWarnings(perfilar(d))
  dp <- p$datos_personales
  fila <- dp[dp$columna %in% copias, , drop = FALSE]
  expect_setequal(fila$columna, copias)
  expect_true(all(fila$proteger))
  expect_true(all(fila$poder_discriminante == "alto"))
  expect_true(all(grepl("`cedula` sin su verificador", fila$fundamento,
                        fixed = TRUE)))
  # El importe con centavos no se clasifica como copia. Sus ejemplos los puede
  # tapar el piso por valor -sus cifras, sin separadores, son las de una copia
  # protegida-: es el piso documentado, no la clasificacion.
  expect_false(any(dp$proteger[dp$columna %in% c("importe_txt",
                                                  "importe_espacio")]))
  expect_identical(.r35p_cifras_publicadas(p, nums), character())
  # El control publica sus extremos.
  columnas <- p$columnas
  expect_equal(columnas$minimo[columnas$columna == "importe_txt"],
               min(importes))
  expect_equal(columnas$maximo[columnas$columna == "importe_txt"],
               max(importes))
  sin <- suppressWarnings(perfilar(d, proteger_datos_personales = FALSE))
  expect_gt(length(.r35p_cifras_publicadas(sin, nums)), 0L)

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "cargas", d)
  for (bloque in list(NULL, 40L)) {
    info <- paste("perfilar_dbi() bloque_filas =", format(bloque))
    pd <- suppressWarnings(perfilar_dbi(con, "cargas", bloque_filas = bloque))
    expect_true(all(copias %in%
                      pd$resumen_tabla$meta$proteccion_personal$columnas),
                info = info)
    expect_identical(.r35p_cifras_publicadas(pd, nums), character(),
                     info = info)
  }
})

test_that("H1: en una coleccion, la copia sin verificador de los documentos de OTRA tabla se protege entera, en cualquier orden", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  set.seed(3511L)
  n <- 120L
  nums <- sample(1000000L:6999999L, n)
  k <- sample(n, 200L, TRUE)
  personas <- data.frame(
    id = seq_len(n), cedula = .r35p_cedula(nums),
    ciudad = sample(c("Montevideo", "Salto", "Rivera"), n, TRUE),
    stringsAsFactors = FALSE
  )
  # `ref` es la cedula de la persona SIN su verificador; `monto`, un importe
  # entero de siete cifras que no copia nada: el control.
  tramites <- data.frame(
    id = seq_len(200L), ref = nums[k],
    monto = sample(1000000L:8999999L, 200L), stringsAsFactors = FALSE
  )
  # La cifra a mano: los extremos y la mediana de `ref` son cedulas sin su
  # verificador; los de `monto`, casi nunca.
  primeras <- substr(gsub("[^0-9]", "", personas$cedula), 1L, 7L)
  expect_true(all(as.character(range(tramites$ref)) %in% primeras))
  expect_lt(sum(as.character(tramites$monto) %in% primeras), 3L)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", personas)
  DBI::dbWriteTable(con, "tramites", tramites)
  for (orden in list(c("personas", "tramites"), c("tramites", "personas"))) {
    info <- paste(orden, collapse = " + ")
    pc <- suppressWarnings(perfilar_coleccion(coleccion(con, orden),
                                              conservar_perfiles = TRUE))
    pt <- pc$perfiles$tramites
    dp <- pt$perfil_muestra$datos_personales
    fila <- dp[dp$columna == "ref", , drop = FALSE]
    expect_true(isTRUE(fila$proteger), info = info)
    expect_identical(fila$poder_discriminante, "alto", info = info)
    expect_true(grepl("la tabla `personas` sin su verificador", fila$fundamento,
                      fixed = TRUE), info = info)
    expect_true("ref" %in% pt$resumen_tabla$meta$proteccion_personal$columnas,
                info = info)
    for (tabla in list(pt$resumen_tabla$columnas, pt$perfil_muestra$columnas)) {
      expect_true(all(is.na(unlist(tabla[tabla$columna == "ref",
                                         c("minimo", "maximo", "mediana")]))),
                  info = info)
      expect_equal(tabla$maximo[tabla$columna == "monto"], max(tramites$monto),
                   info = info)
    }
    expect_identical(.r35p_cifras_publicadas(pc, nums), character(),
                     info = info)
  }
  sin <- suppressWarnings(perfilar_coleccion(
    coleccion(con, c("personas", "tramites")), conservar_perfiles = TRUE,
    proteger_datos_personales = FALSE
  ))
  resumen <- sin$perfiles$tramites$resumen_tabla$columnas
  expect_equal(resumen$minimo[resumen$columna == "ref"], min(tramites$ref))
})

test_that("H3, el recorrido: cada palabra calificadora de cada tipo, contra valores personales de cualquier tipo y contra los de la clase que anuncia", {
  # Recorre el vocabulario entero: cada palabra de `.CALIFICADORES_DE_NOMBRE`
  # que vale para cada tipo (`.CALIFICADORES_POR_TIPO`), detras de una palabra
  # de ese tipo. Con valores personales -nombres, celulares, correos,
  # domicilios, del tipo de la palabra o de otro- la columna se protege con el
  # tipo de los VALORES; con los de la clase que el calificador anuncia
  # -categorias, medios, fechas, claves-, no.
  set.seed(3532L)
  n <- 60L
  pila <- c("Florencia", "Martina", "Ximena", "Joaquin", "Ramon", "Lucia")
  apellidos <- c("Rodriguez", "Olivera", "Pereyra", "Silveira", "Acosta",
                 "Ferreira")
  personales <- list(
    nombre = paste(sample(pila, n, TRUE), sample(apellidos, n, TRUE)),
    telefono = sprintf("09%d%06d", sample(1:9, n, TRUE), sample(0:999999, n)),
    correo = paste0(tolower(sample(pila, n, TRUE)), ".",
                    tolower(sample(apellidos, n, TRUE)), "@correo.com.uy"),
    domicilio = paste(sample(c("Av. Italia", "Bvar. Artigas", "Rivera"),
                             n, TRUE), sample(100:4999, n))
  )
  no_personales <- list(
    categoria = rep(c("alfa", "alfa", "beta", "gamma"), length.out = n),
    medio = rep(c("llamada", "llamada", "visita", "correo"), length.out = n),
    fecha = as.character(as.Date("2020-01-01") + sample(0:900, n, TRUE)),
    clave = sample(1:25, n, TRUE),
    # Las clases de la ronda 37: un lugar y una medida.
    lugar = rep(c("Montevideo", "Montevideo", "Salto", "Rivera"),
                length.out = n),
    medida = rep(c(30L, 30L, 60L, 90L), length.out = n),
    # Y las de la segunda vuelta: la direccion de una red o de un organismo.
    red = rep(c("10.1.2.3", "10.1.2.3", "192.168.0.10", "172.16.5.4"),
              length.out = n),
    institucion = rep(c("Direccion de Salud", "Direccion de Salud",
                        "Direccion de Cultura"), length.out = n)
  )
  palabra_de <- c(persona = "usuario", telefono = "celular",
                  domicilio = "direccion")
  fallos <- character()
  recorridas <- 0L
  inferidos <- list()
  inferir <- function(clave, x) {
    if (is.null(inferidos[[clave]])) {
      inferidos[[clave]] <<- .inferir_tipo_interno(x)$tipo
    }
    list(tipo = inferidos[[clave]])
  }
  for (tipo in names(palabra_de)) {
    for (clase in .CALIFICADORES_POR_TIPO[[tipo]]) {
      for (palabra in strsplit(.CALIFICADORES_DE_NOMBRE[[clase]], "|",
                               fixed = TRUE)[[1L]]) {
        nombre <- paste0(palabra_de[[tipo]], "_", palabra)
        for (valores in names(personales)) {
          x <- personales[[valores]]
          clasificacion <- .clasificar_dato_personal(x, nombre,
                                                     inferir(valores, x))
          recorridas <- recorridas + 1L
          if (!isTRUE(clasificacion$proteger) ||
              !identical(clasificacion$tipo, valores)) {
            fallos <- c(fallos, paste(nombre, valores, clasificacion$tipo))
          }
        }
        x <- no_personales[[clase]]
        clasificacion <- .clasificar_dato_personal(x, nombre, inferir(clase, x))
        recorridas <- recorridas + 1L
        if (isTRUE(clasificacion$proteger)) {
          fallos <- c(fallos, paste(nombre, clase))
        }
      }
    }
  }
  # La cifra a mano, con las palabras de la ronda 37: 21 de categoria, 9 de
  # medio, 6 de fecha, 9 de clave, 11 de lugar, 9 de medida, 12 de red y 6 de
  # institucion. La persona lleva categoria, medio, fecha, clave y lugar (56);
  # el telefono categoria, medio, fecha y medida (45); el domicilio categoria,
  # medio, fecha, lugar, medida, red e institucion (74). Cada una con los cuatro
  # tipos de valores personales y con los de su clase.
  expect_identical(recorridas, 5L * (56L + 45L + 74L))
  expect_identical(fallos, character())
  # La palabra de nombre lo expresa en cualquier lugar del nombre, como dice la
  # ayuda: `nombre_jefe_grupo` con apellidos solos o logins es un nombre, y
  # `nombre_grupo` tambien; una columna de numeros no es el nombre de nadie.
  # Desde la segunda vuelta de la ronda 37, solo un lugar la califica
  # (`nombre_departamento`, que decide con sus valores).
  expect_identical(.CALIFICADORES_POR_TIPO$nombre, "lugar")
  for (columna in c("nombre_jefe_grupo", "nombre_tipo", "nombre_alta")) {
    x <- rep(c("Acosta", "jperez", "Pereyra"), length.out = n)
    expect_true(isTRUE(.clasificar_dato_personal(
      x, columna, inferir("apellidos", x)
    )$proteger), info = columna)
  }
  x <- sample(1:4, n, TRUE)
  expect_false(isTRUE(.clasificar_dato_personal(
    x, "cantidad_nombres", inferir("conteo", x)
  )$proteger))
})

test_that("H3, segunda vuelta: la forma sola no confirma un nombre ni un domicilio; los valores de otro tipo personal si confirman", {
  n <- 25L
  r <- function(v) rep(v, length.out = n)
  # La cara del tapado de mas: categorias de dos palabras, departamentos,
  # estados con particula, oficinas con numero, centinelas sin fecha, codigos,
  # importes con centavos y productos. Ninguna tiene un nombre de pila, una
  # marca de calle, una `@` ni un telefono escrito como telefono: publican.
  categorias <- data.frame(
    id = seq_len(n),
    tipo_cliente = r(c("Gran Empresa", "Gran Empresa", "Sin Asignar",
                       "Mesa Central", "Alta Demanda")),
    grupo_usuario = r(c("Cerro Largo", "Cerro Largo", "Treinta y Tres",
                        "Rio Negro", "San Jose")),
    estado_socio = r(c("Pendiente de Pago", "Pendiente de Pago", "En Revision",
                       "Fuera de Plazo", "Baja Temporal")),
    tipo_domicilio = r(c("Oficina 101", "Oficina 101", "Sucursal 202",
                         "Agencia 303", "Local 404")),
    cliente_desde = r(c("Sin fecha", "Sin fecha", "Pendiente", "No aplica",
                        "Por definir")),
    cliente_id_alfa = r(c("NORTE01", "NORTE01", "OESTE02", "CENTRO03")),
    usuario_cod_guion = r(c("AB-CD01", "AB-CD01", "EF-GH02", "IJ-KL03")),
    usuario_tipo = r(c("Arroz Premium", "Arroz Premium", "Fideos Comun",
                       "Aceite Grande", "Yerba Chico")),
    paciente_alta = r(c("Traslado", "Traslado", "Fuga", "Alta medica")),
    monto_celular = sprintf("%.2f", seq(1234567.10, by = 1111.15,
                                        length.out = n)),
    stringsAsFactors = FALSE
  )
  pc <- suppressWarnings(perfilar(categorias))
  expect_identical(pc$datos_personales$columna[pc$datos_personales$proteger],
                   character())
  columnas <- pc$columnas
  for (columna in setdiff(names(categorias), c("id", "monto_celular"))) {
    expect_identical(columnas$moda[columnas$columna == columna],
                     .r35p_moda(categorias[[columna]]), info = columna)
  }
  expect_equal(columnas$minimo[columnas$columna == "monto_celular"], 1234567.1)

  # La cara de la fuga: nombres con un nombre de pila -dos personas, tres
  # `Maria`, el mismo apellido, en mayusculas o con acentos-, apellidos solos
  # en `nombre_jefe_grupo`, logins sin cifras de quien dio el alta, y valores
  # de OTRO tipo personal que la palabra: celulares en `responsable_canal`,
  # nombres en `contacto_alta`, domicilios en `cliente_tipo`, correos de una
  # intranet en `correo_alta`.
  acento <- "N\u00fa\u00f1ez"
  personas <- data.frame(
    id = seq_len(n),
    usuario_baja = r(c("Florencia Rodriguez", "Martina Olivera")),
    usuario_alta = r(c("jperez", "mrodriguez", "folivera")),
    responsable_grupo = r(c("Maria Rodriguez", "Maria Olivera",
                            "Maria Pereyra", "Ximena Silveira")),
    responsable_estado = r(c("Perez Joaquin", "Perez Bruno", "Perez Leticia")),
    # Desde la ronda 36, sin `Max`, que tambien es una palabra de una
    # categoria (`"Plan Max"`) y salio de la lista de nombres de pila.
    funcionario_tipo = r(paste(acento, c("Bruno", "Lucia", "Ramon"))),
    nombre_jefe_grupo = r(c("Acosta", "Pereyra", "Cabral")),
    responsable_canal = r(c("099 123 456", "+598 98 234 567", "2 710 44 55")),
    telefono_estado = r(c("2901 1234 int 101", "2902 2345 int 202")),
    contacto_alta = r(c("Ramon Acosta", "Lucia Ferreira", "Joaquin Silva")),
    cliente_tipo = r(c("Av. Italia 2345", "Bvar. Artigas 1120", "Colonia 890",
                       "Rivera 3410", "Mercedes 1205")),
    correo_alta = r(c("jgomez@intranet", "asilva@intranet")),
    direccion_tipo = r(c("Av. Italia esq. Comercio", "Rivera esq. Soca")),
    celular_tipo = r(c("099 123 456 / 099 999 888",
                       "098 234 567 / 098 777 666")),
    stringsAsFactors = FALSE
  )
  esperado <- c(usuario_baja = "nombre", usuario_alta = "nombre",
                responsable_grupo = "nombre", responsable_estado = "nombre",
                funcionario_tipo = "nombre", nombre_jefe_grupo = "nombre",
                responsable_canal = "telefono", telefono_estado = "telefono",
                contacto_alta = "nombre", cliente_tipo = "domicilio",
                correo_alta = "correo", direccion_tipo = "domicilio",
                celular_tipo = "telefono")
  pp <- suppressWarnings(perfilar(personas))
  dp <- pp$datos_personales
  protegidas <- dp[dp$proteger, , drop = FALSE]
  expect_setequal(protegidas$columna, names(esperado))
  expect_identical(stats::setNames(protegidas$tipo, protegidas$columna)[
    names(esperado)], esperado)
  hojas <- .r35p_hojas(pp)
  valores <- unique(unlist(personas[names(esperado)], use.names = FALSE))
  valores <- unique(c(valores, unlist(strsplit(valores, " / ", fixed = TRUE))))
  publicados <- valores[vapply(valores, function(v) {
    any(grepl(v, hojas$textos, fixed = TRUE))
  }, logical(1L))]
  expect_identical(publicados, character())
  # Y por la otra puerta, `medir()` decide igual: no infiere tipos.
  rapidas <- .columnas_personales_rapidas(personas)
  expect_setequal(rapidas, names(esperado))
  expect_identical(.columnas_personales_rapidas(categorias), character())
})

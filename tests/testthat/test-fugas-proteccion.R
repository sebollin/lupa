capturar_consola_proteccion <- function(codigo) {
  # Antes desviaba los dos flujos a un archivo con `sink()`. No alcanza: `cli`
  # emite su salida como CONDICIONES de mensaje y dentro de `testthat` quedan
  # atrapadas antes de llegar a ningun flujo, asi que el archivo salia sin las
  # lineas que estas pruebas buscan. `salida_cli()` las recoge como condiciones
  # -ver `helper-salida-cli.R`-, que es lo unico que funciona en los dos
  # contextos.
  salida_cli(codigo)
}

test_that("guiar_limpieza enmascara filas enteras con columnas protegidas", {
  set.seed(16)
  nd <- 30L
  datos <- data.frame(
    documento = sprintf("707771%02d", seq_len(nd)),
    correo = sprintf("persona%03d@correo771.test", seq_len(nd)),
    telefono = sprintf("0997700%02d", seq_len(nd)),
    stringsAsFactors = FALSE
  )
  datos[3L, ] <- datos[2L, ]
  perfil <- perfilar(
    datos, proteger_datos_personales = TRUE,
    analizar_dependencias = FALSE
  )

  tipos <- as.character(perfil$hallazgos$tipo_hallazgo)
  expect_true("filas_duplicadas" %in% tipos)
  expect_true(all(perfil$columnas$dato_personal_protegido))
  expect_true(all(perfil$columnas$moda == "[valor protegido]"))

  plan <- planificar_limpieza(perfil, datos)
  expect_setequal(
    attr(plan, "columnas_datos_personales_protegidas", exact = TRUE),
    names(datos)
  )
  consola <- paste(
    capturar_consola_proteccion(print(perfil)),
    capturar_consola_proteccion(print(perfil$columnas)),
    capturar_consola_proteccion(print(plan)),
    capturar_consola_proteccion(
      guiar_limpieza(plan, datos, selector = function(x) 0L)
    )
  )
  expect_match(consola, "Ejemplos reales:", fixed = TRUE)
  expect_match(
    consola,
    "fila 2 [grupo 1]: documento=\"[valor protegido]\", correo=\"[valor protegido]\", telefono=\"[valor protegido]\"",
    fixed = TRUE
  )
  expect_match(
    consola,
    "fila 3 [grupo 1]: documento=\"[valor protegido]\", correo=\"[valor protegido]\", telefono=\"[valor protegido]\"",
    fixed = TRUE
  )
  crudos <- unique(unlist(lapply(datos, as.character), use.names = FALSE))
  crudos <- crudos[!is.na(crudos) & nzchar(crudos)]
  expect_false(any(vapply(
    crudos, function(valor) grepl(valor, consola, fixed = TRUE), logical(1L)
  )))
})

test_that("la proteccion conserva la senal geometrica pero no publica bbox", {
  skip_if_not_installed("sf")
  set.seed(77)
  wkt <- sprintf(
    "POINT (%.5f %.5f)",
    runif(50L, -56.2, -56.0), runif(50L, -34.9, -34.7)
  )
  perfil <- perfilar(
    data.frame(domicilio = wkt, stringsAsFactors = FALSE),
    proteger_datos_personales = TRUE, analizar_dependencias = FALSE
  )
  fila <- perfil$columnas[perfil$columnas$columna == "domicilio", , drop = FALSE]

  expect_true(all(perfil$columnas$dato_personal_protegido))
  expect_identical(fila$tipo_geometria, "POINT")
  expect_identical(fila$dimension_geometria, "XY")
  expect_true(!is.na(fila$bbox_alcance))

  bbox <- c("bbox_xmin", "bbox_xmax", "bbox_ymin", "bbox_ymax")
  expect_true(all(is.na(fila[bbox])))
  expect_match(fila$bbox_alcance, "proteg", ignore.case = TRUE)
  geometria <- sf::st_sfc(
    lapply(seq_len(3L), function(i) {
      sf::st_point(c(-56 + i / 1000, -34 + i / 1000))
    }),
    crs = 4326
  )
  datos_sf <- data.frame(geometria)
  names(datos_sf) <- "domicilio"
  perfil_sf <- perfilar(
    datos_sf, proteger_datos_personales = TRUE,
    analizar_dependencias = FALSE
  )
  fila_sf <- perfil_sf$columnas[
    perfil_sf$columnas$columna == "domicilio", , drop = FALSE
  ]
  expect_identical(fila_sf$crs_declarado, "4326")
  expect_identical(fila_sf$tipo_geometria, "POINT")
  expect_identical(fila_sf$dimension_geometria, "XY")
  expect_identical(fila_sf$n_geometrias_vacias, 0L)
  expect_identical(fila_sf$n_geometrias_invalidas, 0L)
  expect_identical(fila_sf$n_bbox_evaluados, 3L)
  expect_true(all(is.na(fila_sf[bbox])))
  consola <- paste(
    capturar_consola_proteccion(print(perfil)),
    capturar_consola_proteccion(print(perfil$columnas)),
    capturar_consola_proteccion(print(perfil_sf$columnas))
  )
  coordenadas <- unique(unlist(regmatches(
    wkt, gregexpr("[-+]?[0-9]+\\.[0-9]+", wkt, perl = TRUE)
  )))
  expect_false(any(vapply(
    coordenadas,
    function(valor) grepl(valor, consola, fixed = TRUE),
    logical(1L)
  )))
})

# `perfilar_por()` publica los valores de la columna de agrupacion como etiqueta
# de grupo, y `perfilar()` sobre esa misma columna los enmascara. Encontrado el
# 2026-09-04 pidiendo construir la fuga N+1, no revisando la capa de proteccion.
#
# No se enmascara -la etiqueta es el eje del resultado- pero se declara. Lo que
# este archivo fija es que la declaracion exista y que NO aparezca donde no
# corresponde: sin esa segunda mitad, un atributo que se llenara siempre pasaria
# igual y no probaria nada.
test_that("perfilar_por declara que las etiquetas de grupo son datos personales", {
  documentos <- c("5.836.595-5", "4.112.987-2", "7.965.431-K",
                  "1.234.567-8", "9.876.543-2")
  datos <- data.frame(
    documento = rep(documentos, 8L),
    atributo = rep(c("edad", "sueldo", "altura", "peso"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )

  # Primera mitad obligatoria: el mecanismo se activa. Si `perfilar()` no
  # clasificara la columna como personal, todo lo de abajo pasaria sin medir.
  perfil <- perfilar(datos, analizar_dependencias = FALSE)
  expect_true(any(perfil$datos_personales$columna == "documento" &
                    perfil$datos_personales$proteger))

  agrupado <- suppressWarnings(
    perfilar_por(datos, "documento", min_filas = 2L)
  )
  declarado <- attr(agrupado, "etiquetas_personales")
  expect_equal(nrow(declarado), 1L)
  expect_equal(declarado$columna, "documento")
  expect_equal(declarado$n_grupos, length(documentos))
  expect_match(declarado$motivo, "seudonimizada", fixed = TRUE)

  # Y avisa al ejecutar, no solo en un atributo que nadie mira.
  expect_message(
    perfilar_por(datos, "documento", min_filas = 2L),
    "documento",
    fixed = TRUE
  )

  # Control 1: una columna de agrupacion que NO lleva datos personales no
  # declara nada. Sin esto, un atributo siempre lleno pasaria el test.
  neutros <- data.frame(
    region = rep(c("norte", "sur", "este", "oeste", "centro"), 8L),
    atributo = rep(c("a", "b", "c", "d"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )
  sin_personales <- perfilar_por(neutros, "region", min_filas = 2L)
  expect_equal(nrow(attr(sin_personales, "etiquetas_personales")), 0L)

  # Control 2: con la proteccion desactivada el usuario ya declaro que quiere
  # los valores, asi que no se avisa de algo que pidio.
  sin_proteccion <- perfilar_por(datos, "documento", min_filas = 2L,
                                 proteger_datos_personales = FALSE)
  expect_equal(nrow(attr(sin_proteccion, "etiquetas_personales")), 0L)
})

# Nombrar la columna de agrupacion en un argumento que se reenvia a `perfilar()`
# hacia fallar la corrida entera, porque esa columna se recorta de cada rebanada
# y `perfilar()` la denuncia como inexistente. Se destapo al agregar el canal de
# declaracion: `columnas_personales = <la columna de agrupacion>` es la unica
# forma de decir que las etiquetas llevan datos personales cuando el lexico no
# reconoce el nombre, y era justamente la que reventaba.
test_that("perfilar_por acepta argumentos que nombran la columna de agrupacion", {
  datos <- data.frame(
    codigo_interno = rep(sprintf("X%04d", 1:5), 8L),
    atributo = rep(c("a", "b", "c", "d"), 10L),
    valor = as.character(20:59),
    stringsAsFactors = FALSE
  )

  # Primera mitad: sin declarar, el lexico NO reconoce este nombre. Si lo
  # reconociera, lo de abajo pasaria sin probar el canal de declaracion.
  sin_declarar <- perfilar_por(datos, "codigo_interno", min_filas = 2L)
  expect_equal(nrow(attr(sin_declarar, "etiquetas_personales")), 0L)

  declarado <- suppressWarnings(perfilar_por(
    datos, "codigo_interno", min_filas = 2L,
    columnas_personales = "codigo_interno"
  ))
  etiquetas <- attr(declarado, "etiquetas_personales")
  expect_equal(nrow(etiquetas), 1L)
  expect_equal(etiquetas$tipo, "declarada_por_el_usuario")
  # Y la corrida sigue produciendo los grupos: el argumento se recorta, no anula.
  expect_equal(length(unique(declarado$grupo)), 5L)

  # Los otros tres argumentos que validaban existencia y por eso reventaban.
  for (extra in list(
    list(columnas_opcionales = "codigo_interno"),
    list(aplicabilidad = list(codigo_interno = function(x) rep(TRUE, length(x))))
  )) {
    salida <- suppressWarnings(do.call(
      perfilar_por,
      c(list(datos, "codigo_interno", min_filas = 2L), extra)
    ))
    expect_equal(length(unique(salida$grupo)), 5L,
                 info = names(extra)[1L])
  }

  # Control: un argumento que nombra una columna que SI esta en la rebanada
  # sigue llegando a `perfilar()` y no se recorta por error.
  con_opcional <- suppressWarnings(perfilar_por(
    datos, "codigo_interno", min_filas = 2L, columnas_opcionales = "valor"
  ))
  expect_equal(length(unique(con_opcional$grupo)), 5L)
})

# Los ausentes forman un grupo con la etiqueta `(ausente)`. Si la columna trae
# ese texto como valor real, los dos caen en el mismo grupo: no se pierde
# ninguna fila -la suma se conserva- pero se publica un grupo que junta dos
# cosas distintas. No se cambia la etiqueta; se declara la colision.
test_that("perfilar_por declara cuando el grupo (ausente) junta dos cosas", {
  datos <- data.frame(
    g = c(rep(NA_character_, 20L), rep("(ausente)", 20L), rep("real", 20L)),
    v = as.character(1:60),
    stringsAsFactors = FALSE
  )
  salida <- perfilar_por(datos, "g", min_filas = 5L)
  cobertura <- attr(salida, "cobertura_grupos")

  fila <- cobertura[cobertura$grupo == "(ausente)" &
                      grepl("valor real", cobertura$motivo, fixed = TRUE), ,
                    drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$n_filas_grupo, 40L)
  expect_match(fila$motivo, "20 fila(s) con la columna de agrupacion ausente",
               fixed = TRUE)

  # No se pierde ni se duplica ninguna fila: la invariante que importa.
  por_grupo <- unique(salida[, c("grupo", "n_filas_grupo")])
  expect_equal(sum(por_grupo$n_filas_grupo), nrow(datos))

  # Control: sin el literal en los datos no se declara nada. Sin esta mitad, una
  # fila de cobertura que se escribiera siempre pasaria el test.
  sin_colision <- data.frame(
    g = c(rep(NA_character_, 20L), rep("real", 20L)),
    v = as.character(1:40),
    stringsAsFactors = FALSE
  )
  cobertura_limpia <- attr(perfilar_por(sin_colision, "g", min_filas = 5L),
                           "cobertura_grupos")
  expect_equal(sum(grepl("valor real", cobertura_limpia$motivo, fixed = TRUE)), 0L)
})

# `x[[""]]` devuelve NULL aunque el elemento exista: R no resuelve la cadena
# vacia como nombre. `perfilar_por()` recorria los grupos por nombre, asi que el
# grupo de los blancos `""` recibia `NULL`, quedaba con cero filas y se publicaba
# como "El grupo tiene 0 filas". Medido sobre 200 filas con 30 blancos: la suma
# de `n_filas_grupo` daba 170 y esas 30 filas no aparecian en ningun lado.
#
# Lo que fija esta prueba es la INVARIANTE, no el caso: ninguna fila puede
# perderse ni contarse dos veces entre los grupos perfilados y los declarados.
test_that("ninguna fila se pierde al agrupar, con blancos o sin ellos", {
  tablas <- list(
    con_vacia = c(rep("A", 40L), rep("B", 40L), rep("", 30L), rep(" ", 20L),
                  rep("\t", 10L), rep(NA_character_, 30L), rep("C", 30L)),
    solo_vacia = c(rep("", 25L), rep("A", 25L)),
    sin_blancos = c(rep("A", 30L), rep("B", 20L))
  )
  for (nombre in names(tablas)) {
    etiquetas <- tablas[[nombre]]
    datos <- data.frame(
      grupo = etiquetas,
      valor = seq_along(etiquetas),
      estado = rep("S/D", length(etiquetas)),
      stringsAsFactors = FALSE
    )
    salida <- suppressWarnings(perfilar_por(
      datos, por = "grupo", min_filas = 5L, proteger_datos_personales = FALSE
    ))
    perfilados <- unique(salida[, c("grupo", "n_filas_grupo")])
    cobertura <- attr(salida, "cobertura_grupos")
    declarados <- if (nrow(cobertura)) {
      unique(cobertura[, c("grupo", "n_filas_grupo")])
    } else perfilados[0, ]

    total <- sum(perfilados$n_filas_grupo) + sum(declarados$n_filas_grupo)
    expect_equal(total, nrow(datos), info = nombre)
    # Y ningun grupo publicado puede tener cero filas: si esta publicado, existe.
    expect_true(all(perfilados$n_filas_grupo > 0L), info = nombre)
  }

  # Primera mitad, para que esto pruebe algo: el caso con vacia tiene de verdad
  # un grupo `""` con filas, y se perfila.
  con_vacia <- data.frame(
    grupo = tablas$con_vacia, valor = seq_along(tablas$con_vacia),
    estado = rep("S/D", length(tablas$con_vacia)), stringsAsFactors = FALSE
  )
  salida <- suppressWarnings(perfilar_por(
    con_vacia, por = "grupo", min_filas = 5L, proteger_datos_personales = FALSE
  ))
  fila_vacia <- unique(salida[salida$grupo %in% "", c("grupo", "n_filas_grupo")])
  expect_equal(nrow(fila_vacia), 1L)
  expect_equal(fila_vacia$n_filas_grupo, 30L)
})

# La regla del paquete es excluir lo que el usuario declara. `medir()` aceptaba
# `proteger_datos_personales` pero no tenia por donde recibir QUE proteger, asi
# que una columna que solo es personal porque el usuario lo dice -un nombre
# propio de la organizacion, un identificador interno- quedaba sin proteger en
# el camino de metricas, y el usuario no tenia donde decirlo.
test_that("medir() protege lo que el usuario declara personal", {
  skip_if_not_installed("stringdist")
  metrica <- metricas_referencial()$CorrectitudSemFuerte

  filtra <- function(medida, secreto) {
    con_candidato <- grepl(
      "candidato_referencial=", medida$objeto_medible, fixed = TRUE
    )
    # Primero: el mecanismo se activo? Sin candidatos no se prueba nada.
    expect_true(any(con_candidato))
    grepl(secreto, medida$objeto_medible[con_candidato][[1L]], fixed = TRUE)
  }

  # a. Un legajo interno, sin forma que ningun lexico pueda reconocer.
  ref <- referencial(
    data.frame(
      legajo = c("LEG-0001", "LEG-0002", "LEG-0003"), stringsAsFactors = FALSE
    ),
    "legajo"
  )
  datos <- data.frame(
    legajo = c("LEG-0001", "LEG-000X", "ZZZ-9999"),
    oficina = c("central", "norte", "sur"),
    stringsAsFactors = FALSE
  )
  instancia <- instanciar(
    especializar(metrica, proximidad = TRUE), "t", "legajo", referencial = ref
  )
  expect_true(filtra(medir(modelo(instancia), datos), "LEG-0001"))
  expect_false(filtra(
    medir(modelo(instancia), datos, columnas_personales = "legajo"),
    "LEG-0001"
  ))

  # b. Un identificador con forma de documento que el lexico por omision no
  #    verifica y un validador propio si.
  ref2 <- referencial(
    data.frame(
      codigo = c("9912345678", "9987654321", "9955555555"),
      stringsAsFactors = FALSE
    ),
    "codigo"
  )
  datos2 <- data.frame(
    codigo = c("9912345678", "9912345679", "1234567890"),
    stringsAsFactors = FALSE
  )
  instancia2 <- instanciar(
    especializar(metrica, proximidad = TRUE), "t", "codigo", referencial = ref2
  )
  expect_true(filtra(medir(modelo(instancia2), datos2), "9912345678"))
  expect_false(filtra(
    medir(
      modelo(instancia2), datos2,
      validadores_personales = list(
        interno = function(x) grepl("^99[0-9]{8}$", x)
      )
    ),
    "9912345678"
  ))

  # Los controles: la declaracion tiene que ser la que decide, y no cualquier
  # declaracion. Sin esto, una version que protegiera siempre pasaria igual.
  # Declarar OTRA columna -que existe- no protege esta. Sin este control, una
  # version que protegiera ante cualquier declaracion pasaria igual.
  expect_true(filtra(
    medir(modelo(instancia), datos, columnas_personales = "oficina"),
    "LEG-0001"
  ))
  expect_true(filtra(
    medir(
      modelo(instancia), datos,
      columnas_personales = "legajo", proteger_datos_personales = FALSE
    ),
    "LEG-0001"
  ))
  expect_true(filtra(
    medir(
      modelo(instancia2), datos2,
      validadores_personales = list(nada = function(x) rep(FALSE, length(x)))
    ),
    "9912345678"
  ))

  # La declaracion se valida con la misma funcion que perfilar(), asi que
  # falla igual: un tipo equivocado y un nombre mal escrito.
  expect_error(
    medir(modelo(instancia), datos, columnas_personales = 1:3),
    "vector de texto"
  )
  expect_error(
    medir(modelo(instancia), datos, columnas_personales = "legjo"),
    "inexistentes"
  )
  # Y la forma con tipo declarado vale igual que en perfilar().
  expect_false(filtra(
    medir(
      modelo(instancia), datos,
      columnas_personales = c(legajo = "identificador_interno")
    ),
    "LEG-0001"
  ))
})

# La proteccion reemplazaba los valores protegidos en TODO el texto del perfil,
# incluidos campos que no son datos. Medido sobre `datos_administrativos`, con
# `id_persona` protegida y valores de un digito: `meta$version` salia como
# "0.[valor protegido].0" y un patron `9+-9+-9+` salia como
# "[valor protegido]+[valor protegido]+[valor protegido]+".
test_that("la version del paquete no se enmascara", {
  perfil <- perfilar(datos_administrativos)
  # Primero: la proteccion se activo. Sin columnas protegidas no prueba nada.
  expect_true(any(perfil$datos_personales$proteger))
  expect_true("id_persona" %in%
                perfil$datos_personales$columna[perfil$datos_personales$proteger])

  expect_equal(
    perfil$meta$version,
    perfilar(
      datos_administrativos, proteger_datos_personales = FALSE
    )$meta$version
  )
  expect_false(grepl("valor protegido", perfil$meta$version, fixed = TRUE))

  # Y ningun campo de `meta` queda con el marcador: `meta` describe la corrida,
  # no los datos, y tiene su propio paso dirigido.
  marcado <- function(x) {
    if (is.list(x)) return(any(vapply(x, marcado, logical(1))))
    is.character(x) && any(grepl("valor protegido", x, fixed = TRUE), na.rm = TRUE)
  }
  expect_false(marcado(perfil$meta))
})

test_that("el patron es una forma y no se enmascara; los ejemplos si", {
  perfil <- perfilar(datos_administrativos)
  abierto <- perfilar(datos_administrativos, proteger_datos_personales = FALSE)
  protegidas <- perfil$datos_personales$columna[perfil$datos_personales$proteger]
  expect_true(length(protegidas) > 0L)

  for (columna in names(perfil$patrones)) {
    # El patron dice la FORMA -`9+-9+-9+` es "digitos, guion, digitos"- y se
    # construye sobre un alfabeto fijo: nunca lleva un valor de la tabla.
    expect_identical(
      perfil$patrones[[columna]]$patron,
      abierto$patrones[[columna]]$patron,
      info = columna
    )
  }

  # Los ejemplos son valores y siguen enmascarados en las columnas protegidas.
  for (columna in protegidas) {
    if (is.null(perfil$patrones[[columna]])) next
    ejemplos <- unlist(perfil$patrones[[columna]]$ejemplos)
    if (!length(ejemplos)) next
    expect_true(
      all(grepl("valor protegido", ejemplos, fixed = TRUE)),
      info = columna
    )
  }
  # Y la moda de una columna protegida tampoco se publica.
  for (columna in protegidas) {
    expect_equal(
      perfil$columnas$moda[perfil$columnas$columna == columna],
      "[valor protegido]", info = columna
    )
  }
})

# La mitad que decide que el arreglo no abrio una fuga: ningun valor de una
# columna protegida puede aparecer en ninguna parte del perfil.
test_that("ningun valor protegido se escapa al perfil", {
  casos <- list(
    administrativos = datos_administrativos,
    operativos = datos_operativos,
    documentos = data.frame(
      cedula = c("48123456", "51987654", "48123456", "12345678", "S/D"),
      correo = c("juan.perez@x.uy", "ana@y.uy", "b@z.uy", NA, "juan.perez@x.uy"),
      sexo = c("F", "M", "S/D", "F", "M"),
      monto = c(100, 200, 100, 300, 100),
      stringsAsFactors = FALSE
    )
  )
  textos_de <- function(x, acumulado = character()) {
    if (is.list(x)) {
      for (parte in x) acumulado <- textos_de(parte, acumulado)
      return(acumulado)
    }
    if (is.character(x) || is.factor(x)) acumulado <- c(acumulado, as.character(x))
    acumulado
  }
  for (nombre in names(casos)) {
    datos <- casos[[nombre]]
    perfil <- perfilar(datos)
    protegidas <- perfil$datos_personales$columna[perfil$datos_personales$proteger]
    expect_true(length(protegidas) > 0L, info = nombre)

    # Los valores largos de una columna protegida: los que identifican. Se
    # excluyen los centinelas cortos, que son vocabulario compartido y no
    # identifican a nadie.
    secretos <- unique(unlist(lapply(protegidas, function(col) {
      valores <- as.character(datos[[col]])
      valores[!is.na(valores) & nchar(valores) >= 6L]
    })))
    if (!length(secretos)) next
    publicado <- textos_de(perfil)
    publicado <- publicado[!is.na(publicado)]
    for (secreto in secretos) {
      expect_false(
        any(grepl(secreto, publicado, fixed = TRUE)),
        info = paste(nombre, secreto)
      )
    }
  }
})

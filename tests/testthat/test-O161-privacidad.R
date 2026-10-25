# O161: la proteccion de datos personales. Cinco casos -H1, la regla de las
# columnas "hermanas" de `perfilar_dbi()`; H2, el resto de un nombre en
# `otras_tablas`, escrito despues del repaso; H3, la cedula sin su verificador
# en un extremo numerico; H4, la regla de nombre por la palabra de persona; H5,
# el cierre borraba el nombre de la tabla-. Cada bloque rehace la cifra a mano
# y falla contra el codigo anterior al arreglo.

# Las hojas de un objeto, por tipo, con el nombre del campo de donde vienen.
# Pesada para CRAN (mas de 1 s en la medicion sin red del 2026-10-23): corre en la
# suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

.r34p_hojas <- function(x) {
  numeros <- list()
  textos <- character()
  recorrer <- function(y, campo) {
    if (is.null(y) || is.environment(y) || is.function(y)) return(invisible())
    if (is.list(y)) {
      nombres <- names(y)
      for (k in seq_along(y)) {
        propio <- if (!is.null(nombres) && nzchar(nombres[[k]])) nombres[[k]] else campo
        recorrer(y[[k]], propio)
      }
    } else if (is.numeric(y)) {
      numeros[[length(numeros) + 1L]] <<- data.frame(
        campo = rep(if (is.null(campo)) "" else as.character(campo), length(y)),
        valor = as.numeric(y), stringsAsFactors = FALSE
      )
    } else if (is.character(y) || is.factor(y)) {
      textos <<- c(textos, as.character(y))
    }
    invisible()
  }
  recorrer(unclass(x), "")
  list(numeros = do.call(rbind, c(list(data.frame(campo = character(),
                                                 valor = numeric())), numeros)),
       textos = textos[!is.na(textos)])
}

# El digito verificador de la cedula uruguaya, a mano.
.r34p_dv <- function(x) {
  d <- as.integer(strsplit(sprintf("%07d", x), "")[[1L]])
  (10L - sum(d * c(2L, 9L, 8L, 7L, 6L, 3L, 4L)) %% 10L) %% 10L
}

.r34p_cedula <- function(x, dv) {
  sprintf("%d.%03d.%03d-%d", x %/% 1000000L, (x %/% 1000L) %% 1000L,
          x %% 1000L, dv)
}

test_that("H1: perfilar_dbi() publica los estadisticos de una columna debil, como perfilar()", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # 40 filas: `cedula` protegida -su verificador no es el real, como en una
  # base sucia, y la protege el nombre-; `importe`, pesos enteros de siete
  # cifras redondeados a miles, y `factura`, de ocho cifras: comparten con la
  # cedula el tipo `documento_identidad` debil y no son de nadie. `copia`
  # repite la cedula de cada fila en digitos: tambien es debil, y su piso es el
  # que la regla de las hermanas suplia.
  set.seed(3401L)
  n <- 40L
  base <- sample(1000000L:6999999L, n)
  cedula <- .r34p_cedula(base, base %% 10L)
  d <- data.frame(
    id = seq_len(n), cedula = cedula,
    importe = sample(1500L:9000L, n) * 1000,
    factura = 30000000 + sample(1:900000, n),
    copia = as.numeric(gsub("[^0-9]", "", cedula)),
    stringsAsFactors = FALSE
  )
  memoria <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  dp <- memoria$datos_personales
  debiles <- dp$columna[dp$tipo == "documento_identidad" &
                          dp$poder_discriminante == "debil" & !dp$proteger]
  # Es el caso del hallazgo: las tres comparten el tipo de la protegida.
  expect_setequal(debiles, c("importe", "factura", "copia"))
  expect_true("cedula" %in% dp$columna[dp$proteger])

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "movimientos", d)
  a_mano <- function(x) c(min(x), max(x), stats::median(x), mean(x), stats::sd(x))
  campos <- c("minimo", "maximo", "mediana", "media", "desvio")
  fila <- function(tabla, columna) {
    as.numeric(unlist(tabla[tabla$columna == columna, campos]))
  }
  for (bloque in list(NULL, 10L)) {
    info <- paste("bloque_filas =", format(bloque))
    pd <- suppressWarnings(perfilar_dbi(con, "movimientos", bloque_filas = bloque, proteger_datos_personales = TRUE))
    columnas <- pd$resumen_tabla$columnas
    for (columna in c("importe", "factura")) {
      expect_equal(fila(columnas, columna), a_mano(d[[columna]]),
                   tolerance = 1e-9, info = paste(info, columna))
      # Lo mismo que `perfilar()` y que el perfil de la muestra del mismo objeto.
      expect_equal(fila(columnas, columna), fila(memoria$columnas, columna),
                   tolerance = 1e-9, info = paste(info, columna))
      expect_equal(fila(columnas, columna),
                   fila(pd$perfil_muestra$columnas, columna),
                   tolerance = 1e-9, info = paste(info, columna))
      detalle <- columnas$detalle_proteccion_personal
      expect_true(is.null(detalle) || is.na(detalle[columnas$columna == columna]),
                  info = paste(info, columna))
      sql <- pd$resumen_tabla$sql
      expect_false(any(sql$columna == columna &
                         sql$estado == "omitido_por_privacidad"),
                   info = paste(info, columna))
    }
    # La copia de la cedula sigue tapada, ahora por el piso por valor: ningun
    # numero del objeto es una cedula.
    hojas <- .r34p_hojas(pd)
    expect_false(any(hojas$numeros$valor %in% d$copia), info = info)
    expect_true(is.na(fila(columnas, "copia")[[2L]]), info = info)
  }
})

test_that("H3: la columna que copia la cedula protegida sin su verificador se protege entera; la que coincide por azar no", {
  # 30 filas: `cedula` protegida con su verificador real; `clave`, entera, y
  # `clave_txt`, la misma cifra como texto, son la cedula de cada fila SIN su
  # verificador, con un nombre que no la delata; `clave_tipeos` es la misma
  # copia con dos tipeos (28 de 30, el 93 %). `redondo` y `cuotas` son el
  # control. Y en otra tabla, sin ninguna copia, `importe`: 30 importes de siete
  # cifras de los que solo DOS -su minimo y su maximo, el peor caso del azar-
  # coinciden con una cedula sin verificador.
  set.seed(3403L)
  n <- 30L
  base <- sample(1000000L:6999999L, n)
  dv <- vapply(base, .r34p_dv, integer(1L))
  medio <- setdiff(sample((min(base) + 1L):(max(base) - 1L), n + 10L), base)
  importe <- c(min(base), medio[seq_len(n - 2L)], max(base))
  tipeos <- base
  tipeos[1:2] <- setdiff(sample(1000000L:6999999L, 10L), base)[1:2]
  d <- data.frame(
    id = seq_len(n), cedula = .r34p_cedula(base, dv), clave = base,
    clave_txt = as.character(base), clave_tipeos = tipeos,
    redondo = sample(1:6, n, TRUE) * 1e6, cuotas = sample(1:36, n, TRUE),
    stringsAsFactors = FALSE
  )
  azar <- data.frame(id = seq_len(n), cedula = d$cedula, importe = importe,
                     stringsAsFactors = FALSE)
  # La cifra a mano: el minimo y el maximo de `clave` -y de `importe`- son las
  # siete primeras cifras de dos cedulas protegidas; de `importe`, son las
  # unicas dos que coinciden.
  primeras <- substr(gsub("[^0-9]", "", d$cedula), 1L, 7L)
  expect_true(all(as.character(range(base)) %in% primeras))
  expect_identical(sum(as.character(azar$importe) %in% primeras), 2L)
  expect_identical(range(azar$importe), range(base))
  expect_identical(sum(as.character(d$clave_tipeos) %in% primeras), 28L)

  copias <- c("clave", "clave_txt", "clave_tipeos")
  revisar <- function(objeto, info) {
    hojas <- .r34p_hojas(objeto)
    # Ningun numero del objeto es una cedula sin verificador.
    publicados <- hojas$numeros$valor %in% base
    expect_identical(sum(publicados), 0L, info = paste(
      info, paste(unique(hojas$numeros$campo[publicados]), collapse = ", ")
    ))
    escritos <- vapply(as.character(base), function(v) {
      any(grepl(v, hojas$textos, fixed = TRUE))
    }, logical(1L))
    expect_identical(sum(escritos), 0L, info = info)
  }
  memoria <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  revisar(memoria, "perfilar()")
  dp <- memoria$datos_personales
  fila <- dp[dp$columna %in% copias, , drop = FALSE]
  expect_setequal(fila$columna, copias)
  expect_true(all(fila$proteger))
  expect_true(all(fila$poder_discriminante == "alto"))
  expect_true(all(grepl("`cedula` sin su verificador", fila$fundamento,
                        fixed = TRUE)))
  expect_equal(fila$proporcion_verificada[fila$columna == "clave_tipeos"],
               28 / 30)
  columnas <- memoria$columnas
  expect_true(all(is.na(unlist(columnas[columnas$columna %in% copias,
                                        c("minimo", "maximo", "media")]))))
  expect_equal(columnas$maximo[columnas$columna == "redondo"], max(d$redondo))
  expect_equal(columnas$maximo[columnas$columna == "cuotas"], max(d$cuotas))
  # Los cuantiles que `analizar()` publica salen de `distribucion_valores()`.
  revisar(suppressWarnings(distribucion_valores(d, proteger_datos_personales = TRUE)), "distribucion_valores()")
  # El control publica sus extremos, aunque coincidan: el diagnostico de rango
  # de una columna que coincide por azar no se pierde.
  extremos <- function(columnas) {
    unlist(columnas[columnas$columna == "importe", c("minimo", "maximo")])
  }
  sin_copia <- suppressWarnings(perfilar(azar, proteger_datos_personales = TRUE))
  expect_false(any(sin_copia$datos_personales$proteger[
    sin_copia$datos_personales$columna == "importe"
  ]))
  expect_equal(unname(extremos(sin_copia$columnas)), range(azar$importe))

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", d)
  DBI::dbWriteTable(con, "importes", azar)
  pd_azar <- suppressWarnings(perfilar_dbi(con, "importes", proteger_datos_personales = TRUE))
  expect_equal(unname(extremos(pd_azar$resumen_tabla$columnas)),
               range(azar$importe))
  for (bloque in list(NULL, 10L)) {
    info <- paste("perfilar_dbi() bloque_filas =", format(bloque))
    pd <- suppressWarnings(perfilar_dbi(con, "personas", bloque_filas = bloque, proteger_datos_personales = TRUE))
    revisar(pd, info)
    expect_true(all(copias %in%
                      pd$resumen_tabla$meta$proteccion_personal$columnas),
                info = info)
    columnas <- pd$resumen_tabla$columnas
    expect_equal(columnas$maximo[columnas$columna == "redondo"], max(d$redondo),
                 info = info)
  }
})

test_that("H4: una palabra de persona -o de documento, telefono, correo- no protege una categoria, una clave ni una fecha", {
  # 60 filas: `cliente_id` es una clave, `tipo_cliente` y `estado_socio`
  # categorias, `cliente_desde` una fecha; `segmento` y `obs` repiten las
  # categorias. `nombre_cliente` y `socio` traen nombres: esas si se protegen.
  set.seed(3404L)
  n <- 60L
  pila <- c("Florencia", "Martina", "Ximena", "Joaquin", "Ramon")
  apellidos <- c("Rodriguez", "Olivera", "Pereyra", "Silveira", "Acosta")
  nombres <- function() paste(sample(pila, n, TRUE), sample(apellidos, n, TRUE))
  d <- data.frame(
    id = seq_len(n), cliente_id = sample(1:25, n, TRUE),
    tipo_cliente = rep(c("minorista", "minorista", "mayorista", "empresa"),
                       length.out = n),
    segmento = rep(c("mayorista", "mayorista", "minorista", "gobierno"),
                   length.out = n),
    cliente_desde = as.character(as.Date("2015-01-01") + sample(0:3000, n)),
    estado_socio = rep(c("activo", "suspendido", "suspendido"), length.out = n),
    obs = rep(c("pedido mayorista urgente", "reclamo de empresa", "sin novedad",
                "reclamo de empresa"), length.out = n),
    nombre_cliente = nombres(), socio = nombres(),
    # La misma propiedad en las demas reglas de nombre: una categoria con la
    # palabra del documento, del telefono o del correo; y un celular que si lo
    # es.
    tipo_documento = rep(c("CI", "PASAPORTE", "CI"), length.out = n),
    medio_contacto = rep(c("llamada", "llamada", "visita"), length.out = n),
    estado_correo = rep(c("verificado", "rebotado"), length.out = n),
    numero_celular = sprintf("09%07d", 1234500L + seq_len(n)),
    stringsAsFactors = FALSE
  )
  moda <- function(x) names(sort(table(x), decreasing = TRUE))[[1L]]
  p <- suppressWarnings(perfilar(d, proteger_datos_personales = TRUE))
  dp <- p$datos_personales
  expect_setequal(dp$columna[dp$proteger],
                  c("nombre_cliente", "socio", "numero_celular"))
  columnas <- p$columnas
  para <- function(columna, campo) columnas[[campo]][columnas$columna == columna]
  for (columna in c("tipo_cliente", "segmento", "estado_socio", "obs",
                    "tipo_documento", "medio_contacto", "estado_correo")) {
    expect_identical(para(columna, "moda"), moda(d[[columna]]), info = columna)
  }
  expect_identical(para("cliente_id", "moda"), moda(d$cliente_id))
  expect_equal(para("cliente_id", "maximo"), max(d$cliente_id))
  expect_equal(para("cliente_id", "media"), mean(d$cliente_id))
  # Los ejemplos de `segmento` no pierden las palabras que comparte con
  # `tipo_cliente`.
  expect_false(grepl("[valor protegido]", p$patrones$segmento$ejemplos,
                     fixed = TRUE))
  # Y los nombres no se publican.
  hojas <- .r34p_hojas(p)
  expect_false(any(vapply(unique(c(d$nombre_cliente, d$socio, d$numero_celular)),
                          function(v) any(grepl(v, hojas$textos, fixed = TRUE)),
                          logical(1L))))

  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "ventas", d)
  pd <- suppressWarnings(perfilar_dbi(con, "ventas", proteger_datos_personales = TRUE))
  expect_setequal(pd$resumen_tabla$meta$proteccion_personal$columnas,
                  c("nombre_cliente", "socio", "numero_celular"))
  resumen <- pd$resumen_tabla$columnas
  expect_identical(resumen$moda[resumen$columna == "segmento"], moda(d$segmento))
  expect_equal(resumen$maximo[resumen$columna == "cliente_id"], max(d$cliente_id))
})

test_that("H5: el cierre no borra el nombre de la tabla", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  # 30 filas: `cedula` y `nombre` protegidas; `origen`, no personal, vale
  # "pedidos" -el nombre de la tabla- en la mitad de las filas.
  set.seed(3405L)
  n <- 30L
  base <- sample(1000000L:6999999L, n)
  d <- data.frame(
    id = seq_len(n), cedula = .r34p_cedula(base, base %% 10L),
    nombre = paste(sample(c("Florencia", "Ximena"), n, TRUE),
                   sample(c("Rodriguez", "Pereyra"), n, TRUE)),
    origen = rep(c("pedidos", "mostrador", "pedidos", "telefonico"),
                 length.out = n),
    stringsAsFactors = FALSE
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "pedidos", d)
  nombrada <- function(pd, info) {
    expect_identical(pd$resumen_tabla$meta$tabla, "pedidos", info = info)
    sql <- pd$resumen_tabla$sql$sql
    sql <- sql[!is.na(sql)]
    expect_gt(length(sql), 0L)
    # Cada sentencia guardada lee la tabla por su nombre.
    expect_identical(sum(grepl("`pedidos`", sql, fixed = TRUE)), length(sql),
                     info = info)
    expect_false(any(grepl("[valor protegido]", sql, fixed = TRUE)), info = info)
  }
  # Sin muestra: se cierran todas las columnas, y la tabla sigue nombrada.
  solo <- suppressWarnings(perfilar_dbi(con, "pedidos",
                                        bloque_muestra = "solo_agregados", proteger_datos_personales = TRUE))
  nombrada(solo, "solo_agregados")

  # Y con la lectura fuera de la muestra fallida: el cierre de las columnas no
  # personales.
  local_mocked_bindings(
    .valores_protegidos_fuera_de_muestra_dbi = function(...) {
      list(valores = character(), completo = FALSE, motivo = "simulado",
           fechas = character())
    }
  )
  for (bloque in list(NULL, 10L)) {
    info <- paste("bloque_filas =", format(bloque))
    pd <- suppressWarnings(perfilar_dbi(con, "pedidos", muestra = 12,
                                        orden_muestra = "id",
                                        bloque_filas = bloque, proteger_datos_personales = TRUE))
    expect_match(pd$resumen_tabla$meta$proteccion_personal$fuera_de_muestra,
                 "^No verificada: simulado", info = info)
    nombrada(pd, info)
    expect_identical(pd$perfil_muestra$meta$origen_dbi$tabla, "pedidos",
                     info = info)
    # El cierre sigue tapando lo que no es el nombre de la tabla.
    ejemplos <- pd$perfil_muestra$patrones$origen$ejemplos
    expect_false(grepl("mostrador", ejemplos, fixed = TRUE), info = info)
    expect_false(grepl("telefonico", ejemplos, fixed = TRUE), info = info)
  }
})

test_that("H2: con la vista antes que su base, otras_tablas no publica el resto de un nombre", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  # 24 filas de clientes con `nombre` y `celular_txt` protegidas; en la fila 20
  # las dos traen "Juan 'Pocho' Campos Cadena". La vista castea el celular a
  # entero: lee su muestra de 12 filas y falla al leer en la coleccion los
  # valores de su columna protegida, y duckdb cita la celda. Tras el apodo
  # entre comillas quedan dos palabras que el mensaje permite.
  n <- 24L
  d <- data.frame(
    id = seq_len(n),
    nombre = sprintf("Cliente Apellidoso Numero %03d", seq_len(n)),
    celular_txt = sprintf("09%07d", 1234500L + seq_len(n)),
    stringsAsFactors = FALSE
  )
  d$nombre[20] <- d$celular_txt[20] <- "Juan 'Pocho' Campos Cadena"
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "clientes", d)
  DBI::dbExecute(con, paste(
    "CREATE VIEW clientes_v AS",
    "SELECT id, CAST(celular_txt AS INTEGER) AS celular FROM clientes"
  ))
  correr <- function(tablas) {
    avisos <- character()
    valor <- withCallingHandlers(
      perfilar_coleccion(coleccion(con, tablas), conservar_perfiles = TRUE,
                         muestra = 12, metricas = "validos", proteger_datos_personales = TRUE),
      warning = function(w) {
        avisos <<- c(avisos, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(valor = valor, avisos = avisos)
  }
  otras <- function(r) {
    r$valor$perfiles$clientes$resumen_tabla$meta$proteccion_personal$otras_tablas
  }
  vista_primero <- correr(c("clientes_v", "clientes"))
  base_primero <- correr(c("clientes", "clientes_v"))
  for (r in list(vista_primero, base_primero)) {
    # El camino del hallazgo: la base se cerro porque la vista no se pudo leer,
    # y el motivo lleva el mensaje del motor.
    expect_match(otras(r), "Could not convert string", fixed = TRUE)
    # Ni el nombre ni su resto en ninguna hoja de texto del objeto -tambien
    # `print()` y los avisos-.
    textos <- c(.r34p_hojas(r$valor)$textos, r$avisos,
                salida_cli(print(r$valor)))
    expect_false(any(grepl("Campos Cadena", textos, fixed = TRUE)))
    expect_false(any(grepl("Pocho", textos, fixed = TRUE)))
  }
  # El orden de las tablas no cambia lo que se publica.
  expect_identical(otras(vista_primero), otras(base_primero))
})

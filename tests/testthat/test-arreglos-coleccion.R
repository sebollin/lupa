# Los arreglos que salieron de correr `lupa` contra bases reales: §2.46 a §2.49
# y la regla de §2.51.
#
# La regla que gobierna todo lo que sigue: ante un fallo PARCIAL se devuelve lo
# medido con su alcance declarado. Nunca el todo descartado, y nunca `0` -ni
# `-Inf`- por ausencia de medición. En el vocabulario de este paquete `0`
# significa «medido y ninguno»; usarlo para «no se midió» es una afirmación
# falsa.
#
# La otra causa estructural, §2.50: toda la superficie DBI estaba probada contra
# un solo motor, y SQLite comparte justamente las propiedades cuya ausencia es
# el modo de falla —acepta `LIMIT`, calcula el desvio sin quejarse—. Acá se
# construyen conexiones
# simuladas que NO las comparten.

.con_arreglos <- function() {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  DBI::dbConnect(RSQLite::SQLite(), ":memory:")
}

# --- §2.46 (a) y (b): la tabla vacía ---------------------------------------
#
# Una tabla vacía no es un caso exótico: es rutina institucional —tabla recién
# creada, staging truncada, partición del mes que viene—.

test_that("una tabla vacia no informa -Inf ni ceros inventados", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE padron_vacio (id INTEGER, nombre TEXT, monto REAL)")
  DBI::dbExecute(con, "CREATE TABLE con_datos (id INTEGER, nombre TEXT)")
  DBI::dbExecute(con, "INSERT INTO con_datos VALUES (1,'a'),(2,NULL)")

  perfil <- perfilar_coleccion(
    coleccion(con, c("padron_vacio", "con_datos"), nombre = "base_con_vacia"),
    muestra = 100
  )
  resumen <- perfil$resumen_coleccion
  vacia <- resumen[resumen$tabla == "padron_vacio", ]

  # Antes: -Inf, fuera del [0,1] que promete el paquete.
  expect_true(is.na(vacia$prop_faltantes_maxima))
  expect_false(isTRUE(vacia$prop_faltantes_maxima == -Inf))
  # Antes: 0, o sea «tres columnas sin ausencias» sobre una tabla de la que no
  # se sabe nada.
  expect_true(is.na(vacia$n_columnas_sin_faltantes))
  # Y el alcance queda declarado: de cero columnas se conoce la proporcion.
  expect_equal(vacia$n_columnas_medidas, 0)
  expect_equal(vacia$n_columnas, 3)

  # La convencion de proporciones del paquete se respeta en toda la columna.
  proporciones <- resumen$prop_faltantes_maxima
  expect_true(all(is.na(proporciones) | (proporciones >= 0 & proporciones <= 1)))
  # Y `mean`/`sum`/`range` ya no quedan envenenados por un -Inf.
  expect_false(is.infinite(suppressWarnings(range(proporciones, na.rm = TRUE)[[1L]])))
  # `order()` ya no la pone como la tabla de MEJOR calidad de la base: `NA` va
  # al final, no primera.
  expect_equal(resumen$tabla[order(proporciones)][[1L]], "con_datos")
})

test_that("la tabla vacia se declara en la cobertura en vez de pasar por exito", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE staging (id INTEGER, nombre TEXT)")

  perfil <- perfilar_coleccion(coleccion(con, "staging"), muestra = 10)
  falta <- perfil$cobertura_coleccion
  expect_equal(nrow(falta), 1L)
  expect_equal(falta$alcance, "tabla_vacia")
  expect_true(grepl("cero filas", falta$motivo, fixed = TRUE))
  expect_true(nzchar(falta$como_resolverlo))
  expect_equal(perfil$meta$n_vacias, 1L)
  # Pero la tabla SI se perfilo: no cuenta como sin perfilar, y la identidad
  # n_declaradas = n_perfiladas + n_sin_perfilar se mantiene.
  expect_equal(perfil$meta$n_perfiladas, 1L)
  expect_equal(perfil$meta$n_sin_perfilar, 0L)
})

test_that("una proporcion parcialmente conocida declara sobre cuantas columnas se midio", {
  # No es todo o nada: si se midieron dos columnas de tres, se informan las dos
  # con su alcance, no `NA` para todo ni un maximo que finge cubrir las tres.
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mixta", data.frame(
    a = c(1, NA, 3), b = c("x", "y", NA), stringsAsFactors = FALSE
  ))
  resumen <- perfilar_coleccion(coleccion(con, "mixta"), muestra = 10)$resumen_coleccion
  expect_equal(resumen$n_columnas_medidas, 2)
  expect_equal(resumen$prop_faltantes_maxima, 1 / 3)
  expect_equal(resumen$n_columnas_sin_faltantes, 0)
})

# --- §2.46 (c): la cobertura por metrica ------------------------------------
#
# Un motor que rechaza un agregado producia un informe INDISTINGUIBLE de uno
# donde todo se midio. El backend simulado rechaza las tres formas del desvio
# —las dos nativas y el calculo casero con `SQRT`—, que es exactamente
# la propiedad que SQLite no comparte con Oracle.

# Un motor que rechaza una construccion SQL, sin necesitar un motor real: se
# intercepta `DBI::dbGetQuery()` y se rechaza lo que coincida con el patron.
# `dbSendQuery()`/`dbFetch()` quedan intactos, que es justamente la via de
# reserva que se quiere probar.
.rechazar_sql <- function(patron, env = parent.frame()) {
  skip_if_not_installed("testthat", "3.2.0")
  original <- DBI::dbGetQuery
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (is.character(statement) &&
          grepl(patron, statement, ignore.case = TRUE)) {
        stop("ORA-00904: construccion no soportada por el motor", call. = FALSE)
      }
      original(conn, statement, ...)
    },
    .package = "DBI", .env = env
  )
  invisible(NULL)
}

test_that("un agregado rechazado por el motor llega a la coleccion sin conservar perfiles", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "expedientes", data.frame(
    id = c(1, 2, 3), texto = c("a", "b", "a"), stringsAsFactors = FALSE
  ))
  .rechazar_sql("SQRT|STDDEV_SAMP|STDEV")
  col <- coleccion(con, "expedientes", nombre = "base_rechazada")

  liviano <- perfilar_coleccion(col, muestra = 10, conservar_perfiles = FALSE)

  # La cobertura por metrica se calcula ANTES de descartar el perfil, asi que
  # existe tambien en el modo liviano. Antes no llegaba en NINGUN modo.
  expect_true("cobertura_metricas" %in% names(liviano))
  rechazadas <- liviano$cobertura_metricas[
    liviano$cobertura_metricas$estado == "no_disponible", , drop = FALSE
  ]
  expect_equal(nrow(rechazadas), 1L)
  expect_equal(rechazadas$metrica, "desvio")
  expect_equal(rechazadas$columna, "id")
  expect_true(nzchar(rechazadas$como_resolverlo))

  # Y el resumen de la tabla ya no es indistinguible de uno completo.
  resumen <- liviano$resumen_coleccion
  expect_equal(resumen$n_metricas_no_disponibles, 1)
  expect_true(resumen$n_metricas_calculadas < resumen$n_metricas_declaradas)

  # La linea de resumen tambien sube a `cobertura_coleccion`, que es la promesa
  # literal del roxygen de perfilar_coleccion().
  fila <- liviano$cobertura_coleccion
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$alcance, "metricas")
  expect_true(grepl("rechazo", fila$motivo, fixed = TRUE))
  # Pero no cuenta como tabla sin perfilar: la tabla se perfilo, incompleta.
  expect_equal(liviano$meta$n_sin_perfilar, 0L)
  expect_equal(liviano$meta$n_con_metricas_rechazadas, 1L)
})

test_that("la cobertura por metrica es la misma con y sin perfiles, y el descarte sigue pagando", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "expedientes", data.frame(
    id = c(1, 2, 3), texto = c("a", "b", "a"), stringsAsFactors = FALSE
  ))
  .rechazar_sql("SQRT|STDDEV_SAMP|STDEV")
  col <- coleccion(con, "expedientes")

  liviano <- perfilar_coleccion(col, muestra = 10)
  pesado <- perfilar_coleccion(col, muestra = 10, conservar_perfiles = TRUE)
  expect_identical(liviano$cobertura_metricas, pesado$cobertura_metricas)
  # El descarte del perfil sigue ahorrando lo que justificaba el valor por
  # omision: la cobertura por metrica es acotada y no lo anula.
  expect_true(
    as.numeric(utils::object.size(pesado)) >
      3 * as.numeric(utils::object.size(liviano))
  )
})

test_that("los modos de cobertura por metrica y su tope se respetan", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "expedientes", data.frame(
    id = c(1, 2, 3), texto = c("a", "b", "a"), stringsAsFactors = FALSE
  ))
  col <- coleccion(con, "expedientes")

  completa <- perfilar_coleccion(col, muestra = 10, cobertura_metricas = "completa")
  no_medidas <- perfilar_coleccion(col, muestra = 10)
  ninguna <- perfilar_coleccion(col, muestra = 10, cobertura_metricas = "ninguna")

  expect_true(nrow(completa$cobertura_metricas) > nrow(no_medidas$cobertura_metricas))
  expect_true(all(no_medidas$cobertura_metricas$estado != "calculado"))
  expect_equal(nrow(ninguna$cobertura_metricas), 0L)
  # El total real no depende del modo ni del tope.
  expect_equal(ninguna$meta$n_metricas_no_medidas,
               no_medidas$meta$n_metricas_no_medidas)

  recortada <- perfilar_coleccion(col, muestra = 10, tope_cobertura_metricas = 2)
  expect_equal(nrow(recortada$cobertura_metricas), 2L)
  expect_true(recortada$meta$cobertura_metricas_truncada)
  expect_equal(recortada$meta$n_metricas_no_medidas,
               no_medidas$meta$n_metricas_no_medidas)
})

# --- §2.47: el costo se estima en O(n), no en O(n^2) ------------------------

test_that("el costo de todos los pares sale por formula cerrada y coincide", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  anchos <- c(2L, 3L, 5L, 7L)
  for (i in seq_along(anchos)) {
    DBI::dbWriteTable(
      con, paste0("t", i),
      as.data.frame(stats::setNames(
        rep(list(1:2), anchos[[i]]), paste0("c", seq_len(anchos[[i]]))
      ))
    )
  }
  col <- coleccion(con, paste0("t", seq_along(anchos)))
  costo <- estimar_costo_coleccion(col)

  # (sum c)^2 - sum c^2, verificado contra el recuento explicito.
  explicito <- sum(outer(anchos, anchos) [!diag(length(anchos))])
  expect_equal(costo$n_comparaciones_columnas, explicito)
  expect_equal(costo$n_pares_dirigidos, 12)
  expect_equal(costo$n_pares_estimados, 12)
  expect_equal(costo$n_pares_sin_estimar, 0)
  expect_true(is.na(costo$n_pares_declarados))
})

test_that("estimar el costo no materializa los pares ni tarda con muchas tablas", {
  # El punto de §2.47: estimar el costo no puede costar mas que medirlo. Con
  # 1700 tablas eran 22 s y 245 MB para devolver cuatro numeros.
  skip_on_cran()
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n <- 400L
  DBI::dbBegin(con)
  for (i in seq_len(n)) {
    DBI::dbExecute(con, sprintf("CREATE TABLE m%04d (a INTEGER, b TEXT, c REAL)", i))
  }
  DBI::dbCommit(con)
  col <- coleccion(con, sprintf("m%04d", seq_len(n)))

  transcurrido <- system.time(costo <- estimar_costo_coleccion(col))[["elapsed"]]
  expect_equal(costo$n_comparaciones_columnas, (3 * n)^2 - n * 9)
  expect_equal(costo$n_pares_dirigidos, n * (n - 1))
  # El recorrido cuadratico tardaba segundos con este tamano.
  expect_lt(transcurrido, 5)
})

test_that("n_pares_dirigidos no desborda el entero", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:2))
  col <- coleccion(con, "personas")
  # 46.342 tablas ya devolvian NA con warning; el catalogo de un data lake lo
  # alcanza.
  col$n_declaradas <- 100000L
  col$tablas <- col$tablas[0, , drop = FALSE]
  costo <- estimar_costo_coleccion(col)
  expect_false(is.na(costo$n_pares_dirigidos))
  expect_equal(costo$n_pares_dirigidos, 100000 * 99999)
})

test_that("una tabla sin esquema legible se declara en vez de sumarse como cero", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3, nombre = letters[1:3]))
  DBI::dbWriteTable(con, "visitas", data.frame(p = 1:3, d = 1:3))
  col <- coleccion(con, c("personas", "visitas", "sin_permiso"))

  costo <- estimar_costo_coleccion(col, pares = data.frame(
    tabla_1 = c("personas", "personas"),
    tabla_2 = c("visitas", "sin_permiso"), stringsAsFactors = FALSE
  ))
  # El par ilegible no desaparece sumado como 0: se declara.
  expect_equal(costo$n_comparaciones_columnas, 4)
  expect_equal(costo$n_pares_estimados, 1)
  expect_equal(costo$n_pares_sin_estimar, 1)
  expect_equal(costo$n_tablas_sin_esquema, 1)
  expect_equal(costo$tablas_sin_esquema, "sin_permiso")

  # Si NINGUN par se pudo estimar, el total es NA y no 0.
  solo_ilegible <- estimar_costo_coleccion(col, pares = data.frame(
    tabla_1 = "sin_permiso", tabla_2 = "personas", stringsAsFactors = FALSE
  ))
  expect_true(is.na(solo_ilegible$n_comparaciones_columnas))
})

test_that("solo se consulta el esquema de las tablas que los pares nombran", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3))
  DBI::dbWriteTable(con, "visitas", data.frame(p = 1:3))
  col <- coleccion(con, c("personas", "visitas", "no_existe_a", "no_existe_b"))

  acotado <- estimar_costo_coleccion(col, pares = data.frame(
    tabla_1 = "personas", tabla_2 = "visitas", stringsAsFactors = FALSE
  ))
  # Las dos tablas ilegibles ni se tocaron: no aparecen como sin esquema.
  expect_equal(acotado$n_tablas_sin_esquema, 0)
  todos <- estimar_costo_coleccion(col)
  expect_equal(todos$n_tablas_sin_esquema, 2)
})

test_that("cero pares es una declaracion valida y el mensaje no miente", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3))
  DBI::dbWriteTable(con, "visitas", data.frame(p = 1:3))
  col <- coleccion(con, c("personas", "visitas"))
  vacios <- data.frame(
    tabla_1 = character(), tabla_2 = character(), stringsAsFactors = FALSE
  )

  costo <- estimar_costo_coleccion(col, pares = vacios)
  expect_equal(costo$n_pares_declarados, 0)
  # Cero pares son cero comparaciones: eso SI se midio.
  expect_equal(costo$n_comparaciones_columnas, 0)

  relaciones <- relaciones_coleccion(col, pares = vacios)
  expect_equal(nrow(relaciones$relaciones), 0L)
  expect_equal(relaciones$meta$pares_declarados, 0L)

  # El mensaje anterior decia que faltaban `tabla_1` y `tabla_2` cuando el
  # data.frame SI las traia. Ahora nombra la causa real.
  expect_error(
    estimar_costo_coleccion(col, pares = data.frame(otra = 1)),
    "columnas `tabla_1` y `tabla_2`"
  )
  expect_error(
    estimar_costo_coleccion(col, pares = data.frame(
      tabla_1 = "personas", tabla_2 = NA_character_
    )),
    "no admite `NA`"
  )
})

test_that("los pares repetidos se descartan y los autorreferenciales se rechazan", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:5))
  DBI::dbWriteTable(con, "visitas", data.frame(p = 1:5))
  col <- coleccion(con, c("personas", "visitas"))

  expect_error(
    relaciones_coleccion(col, pares = data.frame(
      tabla_1 = "personas", tabla_2 = "personas", stringsAsFactors = FALSE
    )),
    "consigo misma"
  )
  repetidos <- relaciones_coleccion(col, pares = data.frame(
    tabla_1 = c("personas", "personas"), tabla_2 = c("visitas", "visitas"),
    stringsAsFactors = FALSE
  ))
  expect_equal(repetidos$meta$pares_declarados, 1L)
  expect_equal(repetidos$meta$pares_repetidos_descartados, 1)
})

# --- §2.48: cada tabla se lee una sola vez ----------------------------------

test_that("las tablas se leen una vez y no una vez por par", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:20, nombre = letters[1:20],
                                                stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1:15, 1:5)))
  DBI::dbWriteTable(con, "hogares", data.frame(id = 1:5, x = 1:5))
  col <- coleccion(con, c("personas", "visitas", "hogares"))

  pares <- data.frame(
    tabla_1 = c("personas", "personas", "visitas"),
    tabla_2 = c("visitas", "hogares", "hogares"), stringsAsFactors = FALSE
  )
  resultado <- relaciones_coleccion(col, pares = pares)

  # Antes: dos lecturas por par, sin cache. Seis lecturas para tres tablas.
  expect_equal(resultado$meta$lecturas_realizadas, 3L)
  expect_equal(resultado$meta$lecturas_evitadas_por_cache, 3L)
  expect_equal(resultado$meta$tablas_distintas, 3L)
  expect_true(resultado$meta$cache_completa)

  # Y queda registrado el SQL, el orden, la estabilidad y el momento.
  bitacora <- resultado$meta$lecturas
  expect_equal(nrow(bitacora), 3L)
  expect_true(all(grepl("SELECT", bitacora$sql, fixed = TRUE)))
  expect_true(inherits(bitacora$momento, "POSIXct"))
  expect_false(resultado$meta$estable)
  expect_true(grepl("no es repetible", resultado$meta$nota_orden, fixed = TRUE))
})

test_that("declarar el orden hace la lectura repetible y lo dice", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:20, nombre = letters[1:20],
                                                stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1:15, 1:5)))
  col <- coleccion(con, c("personas", "visitas"))

  resultado <- relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "personas", tabla_2 = "visitas",
                            stringsAsFactors = FALSE),
    orden = list(personas = "id", visitas = "persona_id")
  )
  expect_true(resultado$meta$estable)
  expect_true(all(grepl("ORDER BY", resultado$meta$lecturas$sql, fixed = TRUE)))
  expect_true(all(resultado$meta$lecturas$orden_declarado))
  expect_equal(nrow(resultado$relaciones), 1L)
  expect_true(inherits(resultado$relaciones$momento, "POSIXct"))
})

test_that("un motor que rechaza LIMIT ya no tira todos los pares", {
  # §2.50: el entorno de prueba compartia la propiedad cuya ausencia es el modo
  # de falla. Este backend simulado no la comparte.
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:20, nombre = letters[1:20],
                                                stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1:15, 1:5)))
  .rechazar_sql("LIMIT")
  col <- coleccion(con, c("personas", "visitas"))

  resultado <- relaciones_coleccion(
    col,
    pares = data.frame(tabla_1 = "personas", tabla_2 = "visitas",
                       stringsAsFactors = FALSE),
    muestra = 10
  )
  # Antes: todos los pares a `sin_comparar` -honesto, e inutil-.
  expect_equal(nrow(resultado$cobertura_pares), 0L)
  expect_equal(resultado$meta$pares_comparados, 1L)
  expect_true(all(resultado$meta$lecturas$via == "fetch_acotado"))
  expect_true(all(resultado$meta$lecturas$filas_leidas == 10))
})

# --- §2.49: identificadores -------------------------------------------------

test_that("un identificador de tres partes se rechaza nombrando la causa real", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Antes: `catalogo`.`tabla`, un SQL bien formado contra una tabla inexistente,
  # y el usuario iba a pedirle al DBA permisos que ya tenia.
  expect_error(
    coleccion(con, "catalogo.esquema.tabla"),
    "3 partes"
  )
  expect_error(coleccion(con, "srv.catalogo.esquema.tabla"), "4 partes")
  # Y el mensaje ofrece la salida correcta, no una teoria sobre permisos.
  mensaje <- tryCatch(
    coleccion(con, "catalogo.esquema.tabla"),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("data.frame(esquema", mensaje, fixed = TRUE))
  expect_false(grepl("permisos parciales", mensaje, fixed = TRUE))
})

test_that("una frontera legitima ya no colisiona por perder el medio", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # `cat.esq_ventas.personas` y `cat.esq_rrhh.personas` colapsaban en la misma
  # tabla y daban stop("repite una tabla"): dos tablas distintas rechazadas.
  col <- coleccion(con, data.frame(
    esquema = c("esq_ventas", "esq_rrhh"), tabla = c("personas", "personas"),
    stringsAsFactors = FALSE
  ))
  expect_equal(col$n_declaradas, 2L)
  expect_equal(
    col$tablas$identificador,
    c("esq_ventas.personas", "esq_rrhh.personas")
  )
})

test_that("las comillas y los corchetes se quitan al construir el identificador", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  normalizar <- lupa:::.normalizar_tablas_coleccion

  entrecomillado <- normalizar('"esquema"."tabla"')
  expect_equal(entrecomillado$esquema, "esquema")
  expect_equal(entrecomillado$tabla, "tabla")
  corchetes <- normalizar("[dbo].[personas]")
  expect_equal(corchetes$esquema, "dbo")
  expect_equal(corchetes$tabla, "personas")
  invertidas <- normalizar("`ods`.`personas`")
  expect_equal(invertidas$esquema, "ods")
  expect_equal(invertidas$tabla, "personas")

  # El SQL resultante ya no cita las comillas dentro de las comillas.
  col <- coleccion(con, '"esquema"."tabla"')
  expect_equal(
    as.character(DBI::dbQuoteIdentifier(con, col$tablas$referencia[[1L]])),
    "`esquema`.`tabla`"
  )
})

test_that("un punto dentro de comillas pertenece al nombre y no separa", {
  normalizar <- lupa:::.normalizar_tablas_coleccion
  con_punto <- normalizar('"mi.tabla"')
  expect_true(is.na(con_punto$esquema))
  expect_equal(con_punto$tabla, "mi.tabla")

  doble <- normalizar('"a""b".t')
  expect_equal(doble$esquema, 'a"b')
  expect_equal(doble$tabla, "t")

  expect_error(normalizar('"sin cerrar.t'), "sin cerrar")
})

test_that("un punto al principio, al final o repetido se rechaza", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # `esquema.` daba la tabla `esquema` -otra tabla distinta- y `.tabla` producia
  # el SQL ``.`tabla`.
  expect_error(coleccion(con, "esquema."), "vacia")
  expect_error(coleccion(con, ".tabla"), "vacia")
  expect_error(coleccion(con, "."), "vacia")
  expect_error(coleccion(con, "a..b"), "3 partes")
})

test_that("un fallo sobre un nombre partido nombra el parseo antes que los permisos", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:3))

  partido <- perfilar_coleccion(coleccion(con, "otro.personas"), muestra = 10)
  salida <- partido$cobertura_coleccion$como_resolverlo
  expect_true(grepl("Antes de sospechar de los permisos", salida, fixed = TRUE))
  expect_true(grepl("lo partio en el punto", salida, fixed = TRUE))
  expect_true(grepl("data.frame(esquema", salida, fixed = TRUE))

  # Un nombre declarado literalmente no se partio, asi que el mensaje sigue
  # siendo el de permisos, que ahi si es la causa probable.
  literal <- perfilar_coleccion(
    coleccion(con, data.frame(esquema = NA, tabla = "no_existe",
                              stringsAsFactors = FALSE)),
    muestra = 10
  )
  expect_true(grepl(
    "permisos parciales", literal$cobertura_coleccion$como_resolverlo,
    fixed = TRUE
  ))
})

test_that("el data.frame conserva el nombre literal con punto, y el texto no puede", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, 'CREATE TABLE "informe.2024" (a INTEGER)')
  DBI::dbExecute(con, 'INSERT INTO "informe.2024" VALUES (1)')

  literal <- perfilar_coleccion(
    coleccion(con, data.frame(esquema = NA, tabla = "informe.2024",
                              stringsAsFactors = FALSE)),
    muestra = 10
  )
  expect_equal(literal$resumen_coleccion$identificador, "informe.2024")
  expect_equal(literal$resumen_coleccion$n_filas, 1)

  # Por texto es una ambiguedad genuina y `lupa` toma la lectura esquema.tabla.
  normalizado <- lupa:::.normalizar_tablas_coleccion("informe.2024")
  expect_equal(normalizado$esquema, "informe")
  expect_equal(normalizado$tabla, "2024")
  expect_equal(normalizado$declaracion, "texto_partido")
})

test_that("un DBI::Id declara la tabla sin pasar por el parseo", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, 'CREATE TABLE "raro.nombre" (a INTEGER)')
  DBI::dbExecute(con, 'INSERT INTO "raro.nombre" VALUES (1)')

  col <- coleccion(con, list(DBI::Id(table = "raro.nombre")))
  expect_equal(col$tablas$tabla, "raro.nombre")
  expect_true(is.na(col$tablas$esquema))
  expect_equal(col$tablas$declaracion, "dbi_id")
  expect_equal(
    perfilar_coleccion(col, muestra = 10)$resumen_coleccion$n_filas, 1
  )

  suelto <- coleccion(con, DBI::Id(table = "raro.nombre"))
  expect_equal(suelto$n_declaradas, 1L)
  # Tres componentes no se admiten, con la causa nombrada.
  expect_error(coleccion(con, DBI::Id("a", "b", "c")), "dos componentes")
})

test_that("el unicode y las palabras reservadas siguen funcionando", {
  # Se verifico que NO eran el problema; queda como regresion.
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  normalizar <- lupa:::.normalizar_tablas_coleccion

  reservada <- normalizar("from.order")
  expect_equal(reservada$esquema, "from")
  expect_equal(reservada$tabla, "order")

  DBI::dbExecute(con, 'CREATE TABLE "niños" ("año" INTEGER)')
  DBI::dbExecute(con, 'INSERT INTO "niños" VALUES (2024)')
  resumen <- perfilar_coleccion(
    coleccion(con, "niños"), muestra = 10
  )$resumen_coleccion
  expect_equal(resumen$identificador, "niños")
  expect_equal(resumen$n_columnas, 1)

  # La inyeccion queda inerte porque todo pasa por dbQuoteIdentifier().
  inyectada <- coleccion(con, "public.personas;DROP TABLE x")
  expect_equal(
    as.character(DBI::dbQuoteIdentifier(con, inyectada$tablas$referencia[[1L]])),
    "`public`.`personas;DROP TABLE x`"
  )
})

# --- §2.52 vecino: la entrada data.frame tambien se valida ------------------

test_that("el data.frame de tablas valida NA, cadenas vacias y tipos", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # El vector de texto ya validaba; el data.frame no.
  expect_error(coleccion(con, data.frame(tabla = c("a", NA))), "no admite `NA`")
  expect_error(coleccion(con, data.frame(tabla = c("a", ""))), "cadenas vacias")
  expect_error(coleccion(con, data.frame(tabla = 1:2)), "debe ser de texto")
  expect_error(
    coleccion(con, data.frame(esquema = "", tabla = "a", stringsAsFactors = FALSE)),
    "cadenas vacias"
  )
  # Y el `NA` logico de `data.frame(esquema = NA)` sigue siendo la forma normal
  # de declarar «sin esquema».
  expect_silent(coleccion(con, data.frame(esquema = NA, tabla = "a")))
})

# --- §2.51: la regla, comprobada como invariante ----------------------------

test_that("ningun na.rm colapsa un conjunto entero de NA en un numero", {
  con <- .con_arreglos()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE vacia_a (x INTEGER, y TEXT)")
  DBI::dbExecute(con, "CREATE TABLE vacia_b (z REAL)")
  DBI::dbWriteTable(con, "llena", data.frame(a = 1:3))

  perfil <- perfilar_coleccion(
    coleccion(con, c("vacia_a", "vacia_b", "llena", "no_existe")), muestra = 10
  )
  resumen <- perfil$resumen_coleccion
  numericas <- resumen[, c("prop_faltantes_maxima", "n_columnas_sin_faltantes")]
  # Ni un -Inf, ni un Inf, en ninguna columna de proporciones.
  expect_false(any(vapply(numericas, function(x) any(is.infinite(x)), logical(1L))))
  # Las vacias van a NA; la llena a su valor medido.
  vacias <- resumen$tabla %in% c("vacia_a", "vacia_b")
  expect_true(all(is.na(resumen$prop_faltantes_maxima[vacias])))
  expect_true(all(is.na(resumen$n_columnas_sin_faltantes[vacias])))
  expect_equal(resumen$prop_faltantes_maxima[!vacias], 0)
  expect_equal(resumen$n_columnas_sin_faltantes[!vacias], 1)

  # Lo medido se devuelve con su alcance: la tabla ilegible no se lleva puesto
  # el resto, y queda declarada.
  expect_equal(perfil$meta$n_perfiladas, 3L)
  expect_equal(perfil$meta$n_sin_perfilar, 1L)
  expect_equal(perfil$meta$n_vacias, 2L)
  expect_true(nzchar(perfil$meta$nota_cobertura))
})

# Ronda 167 - La clave publicada tiene que ser de la tabla que se midio.
#
# Encontrado contra SQL Server 2022 el 2026-08-30, con el driver ODBC ya
# instalado. Un usuario cuyo esquema por omision no es `dbo`, una tabla homonima
# en los dos esquemas, y la clave primaria declarada SOLO en `dbo`: el motor lee
# la del esquema propio -sin clave, con filas repetidas- y el paquete publicaba
# la clave de la de `dbo`, nombrando una columna que en lo medido ni existe.
#
# Es el invariante en su forma mas directa: informar como medido lo que no se
# midio. La correccion tiene DOS capas, y las dos se prueban aca.

test_that("la clave se descarta entera cuando nombra columnas ajenas", {
  catalogo <- list(
    columnas = c("y", "x"), fuente = "information_schema",
    motivo = NA_character_, garantia = "garantizada",
    estado = list(visible = TRUE, aplicada = TRUE, validada = TRUE)
  )
  r <- lupa:::.clave_de_otra_relacion(catalogo, "x")
  # No se recorta para publicar el resto: `y` sola no es una clave que ningun
  # catalogo haya declarado, y publicarla seria inventar una respuesta nueva a
  # partir de una equivocada.
  expect_length(r$columnas, 0L)
  expect_identical(r$garantia, "desconocida")
  expect_match(r$motivo, "no esta entre las columnas de la tabla medida")
  # El estado tambien se limpia: describia la restriccion de la OTRA relacion.
  expect_true(is.na(r$estado$aplicada))
  expect_true(is.na(r$estado$validada))
})

test_that("el motivo concuerda en numero con las columnas ajenas", {
  base <- list(columnas = "a", fuente = "f", motivo = NA_character_,
               garantia = "garantizada", estado = list())
  expect_match(lupa:::.clave_de_otra_relacion(base, "x")$motivo, "que no esta ")
  expect_match(
    lupa:::.clave_de_otra_relacion(base, c("x", "z"))$motivo, "que no estan "
  )
})

test_that("el estado vacio de la clave es uno solo para todos sus usos", {
  # Habia dos copias de la misma lista de campos. Dos copias se desincronizan
  # solas, y un estado incompleto en una rama se lee como un dato ausente en vez
  # de como un campo que falta.
  e <- lupa:::.estado_clave_vacio(visible = TRUE)
  expect_true(e$visible)
  expect_setequal(
    names(e),
    c("visible", "aplicada", "validada", "unicidad", "unicidad_aplica_a",
      "ausencia_de_nulos", "consultado", "valores")
  )
  expect_false(any(lupa:::.estado_clave_vacio()$consultado))
})

test_that("la red no confunde mayusculas con una columna ajena", {
  # Los catalogos devuelven el nombre con la caja que guardaron, que no siempre
  # es la que devuelve el `SELECT`. Si la red no lo contemplara, callaria claves
  # buenas en cualquier motor que normalice distinto: una guarda que silencia lo
  # real es peor que el fallo que arregla.
  expect_false(any(is.na(lupa:::.resolver_columnas_dbi("ID", c("id", "otra")))))
  expect_true(is.na(lupa:::.resolver_columnas_dbi("x", c("y", "dato"))))
})

# --- Las sondas de la mediana consolidada -------------------------------------
#
# Una sonda tiene que parecerse a la consulta que habilita. Estas ordenaban por
# la constante `1.0`, que parece lo mas neutral y no lo es: dejaba DOS caminos
# declarados sin ejercitarse nunca, y sin sintoma, porque al fallar la sonda el
# paquete degrada a la via por columna y publica valores correctos. Medido el
# 2026-08-30 contra los motores reales: SQL Server rechaza constantes en el
# `ORDER BY` de una funcion de ventana, y MariaDB 11.8 solo implementa
# `PERCENTILE_CONT` como funcion de ventana, asi que su sonda fallaba por estar
# clasificada con el candidato que no lleva `OVER`.

# La eleccion de candidatos solo mira las SENAS de la conexion -su clase y lo
# que declara `dbGetInfo`-, asi que no hace falta un motor ni una conexion real:
# un objeto con la clase adecuada alcanza, y la prueba no depende de que haya
# ningun controlador instalado.
.candidatos_ronda167 <- function(senas) {
  lupa:::.candidatos_mediana_consolidada_dbi(
    structure(list(), class = senas)
  )
}

test_that("ninguna sonda de mediana ordena por una constante", {
  for (senas in c("PostgreSQL", "Microsoft SQL Server", "MariaDB")) {
    for (cand in .candidatos_ronda167(senas)) {
      sonda <- cand$sonda("\"s\"")
      expect_false(
        grepl("ORDER BY 1.0", sonda, fixed = TRUE),
        info = paste(senas, cand$nombre)
      )
      # Y ordena por la columna que la propia subconsulta define, para que lo
      # que se prueba sea la forma que despues se emite.
      expect_match(sonda, "ORDER BY lupa_valor", fixed = TRUE)
    }
  }
})

test_that("MariaDB se sondea con la forma de ventana, que es la que acepta", {
  nombres <- vapply(.candidatos_ronda167("MariaDB"), function(x) x$nombre, "")
  expect_true("PERCENTILE_CONT_OVER" %in% nombres)
  # La forma sin `OVER` no es sintaxis valida en MariaDB: ofrecerla solo gasta
  # una sonda que no puede pasar.
  expect_false("PERCENTILE_CONT" %in% nombres)
  # El control: PostgreSQL sigue con la forma sin ventana.
  expect_identical(
    vapply(.candidatos_ronda167("PostgreSQL"), function(x) x$nombre, ""),
    "PERCENTILE_CONT"
  )
})

test_that("DuckDB recibe la forma consolidada, que acepta", {
  # DuckDB es un motor declarado del paquete y acepta `PERCENTILE_CONT`, pero no
  # estaba en ningun patron, asi que ni siquiera se lo sondeaba. No era un
  # defecto -no publicaba nada falso- sino una capacidad real sin usar, y se vio
  # al hacer visible en la matriz de motores que caminos se encienden y cuales
  # no. Verificado contra DuckDB real: la mediana consolidada coincide con la
  # de R. Aca se fija la eleccion, porque en CRAN no hay motor que la demuestre.
  nombres <- vapply(.candidatos_ronda167("duckdb"), function(x) x$nombre, "")
  expect_true("PERCENTILE_CONT" %in% nombres)
})

test_that("los motores sin la funcion no reciben candidato", {
  # El control del caso anterior: agregar un motor a un patron no puede terminar
  # ofreciendo la forma a cualquiera. SQLite y MySQL no implementan
  # `PERCENTILE_CONT`, y ofrecersela solo gastaria una sonda que no puede pasar.
  expect_length(.candidatos_ronda167("SQLite"), 0L)
  expect_length(.candidatos_ronda167("MySQL"), 0L)
})

test_that("el motivo de una sonda rechazada conserva el error del motor", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "d", data.frame(x = 1:10, y = 11:20))
  mensaje <- "PERCENTILE_CONT is not allowed in the current compatibility mode"
  consultar_original <- lupa:::.consultar_dbi
  testthat::local_mocked_bindings(
    .candidatos_mediana_consolidada_dbi = function(conexion) list(
      list(
        nombre = "PERCENTILE_CONT",
        sonda = function(alias) paste0("SELECT PERCENTILE_CONT(0.5) AS ", alias)
      )
    ),
    .consultar_dbi = function(conexion, sql, presupuesto = NULL, filas = -1L,
                              etapa = "consulta") {
      if (identical(etapa, "sonda_mediana_consolidada")) {
        return(list(ok = FALSE, datos = NULL, motivo = mensaje))
      }
      consultar_original(
        conexion, sql, presupuesto = presupuesto, filas = filas, etapa = etapa
      )
    },
    .package = "lupa"
  )

  resultado <- suppressMessages(perfilar_dbi(
    con, "d", metricas = "mediana", modo = "exacto",
    bloque_muestra = "solo_agregados", instrumentar = FALSE,
    avisar_costo_mediana = FALSE, avisar_derrame_estimado = FALSE
  ))
  motivo <- resultado$resumen_tabla$meta$mediana_consolidada$motivo

  expect_false(resultado$resumen_tabla$meta$mediana_consolidada$disponible)
  expect_match(motivo, mensaje, fixed = TRUE)
  expect_true(is.list(resultado$resumen_tabla$meta$mediana_escalar))
})

test_that("el plan no imprime dos veces sus supuestos", {
  # Con magnitud desconocida, la rama del aviso imprimia los supuestos y el
  # bloque final -que corre para toda magnitud distinta de "baja"- los volvia a
  # imprimir: el plan mostraba dos veces los mismos dos parrafos. Un texto
  # repetido se lee como un error de quien lo escribio, y ademas empuja la
  # tabla de consultas -que es el dato- fuera de la pantalla.
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE d (a INTEGER, b TEXT)")
  DBI::dbExecute(con, "INSERT INTO d VALUES (1, 'x'), (2, 'y')")
  plan <- plan_perfilado_dbi(con, "d")
  # El texto de cli sale por el flujo de mensajes, no por stdout.
  texto <- paste(
    utils::capture.output(print(plan), type = "message"),
    collapse = "\n"
  )
  contar <- function(frase) {
    length(gregexpr(frase, texto, fixed = TRUE)[[1L]][
      gregexpr(frase, texto, fixed = TRUE)[[1L]] > 0
    ])
  }
  expect_identical(contar("El trabajo es una estimaci"), 1L)
  expect_identical(contar("Referencias: unos cinco millones"), 1L)
})

# La politica de ausencia declarada en el resumen de tabla completa cuando una
# columna no se mide dentro de la consulta consolidada y se mide con su propia
# consulta. Pasa por dos caminos: el motor rechaza la consulta consolidada que
# nombra a la columna, o la acepta pero el controlador devuelve uno de sus
# alias con otro nombre. En los dos, la columna tiene que publicar las mismas
# cifras que perfilar() sobre la tabla leida, tambien las cuentas de lo que la
# politica deja afuera de las magnitudes y de las filas que la regla excluye.
testthat::skip_on_cran()
skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

library(DBI)

.acp_estado <- new.env(parent = emptyenv())
.acp_estado$disparos <- 0L

if (!methods::isClass("SQLiteLoteRechazadoAusencia")) {
  setClass("SQLiteLoteRechazadoAusencia", contains = "SQLiteConnection")
}
if (!methods::isClass("SQLiteAliasRenombradoAusencia")) {
  setClass("SQLiteAliasRenombradoAusencia", contains = "SQLiteConnection")
}

# La consulta consolidada se reconoce por sus alias (`lupa_l<lote>_c<columna>_`).
# Se rechaza la que nombra a la columna `c`; la consulta propia de `c` pasa.
setMethod(
  "dbSendQuery", c("SQLiteLoteRechazadoAusencia", "character"),
  function(conn, statement, ...) {
    if (grepl("`c`", statement, fixed = TRUE) &&
        grepl("lupa_l[0-9]+_c[0-9]+_", statement) &&
        grepl("MIN(", statement, fixed = TRUE)) {
      .acp_estado$disparos <- .acp_estado$disparos + 1L
      stop("El motor rechazo la consulta.", call. = FALSE)
    }
    callNextMethod(conn, statement, ...)
  }
)

# La consulta consolidada corre, pero el minimo de `c` vuelve con otro alias.
setMethod(
  "dbSendQuery", c("SQLiteAliasRenombradoAusencia", "character"),
  function(conn, statement, ...) {
    patron <- "MIN\\([^,]*`c`[^,]*\\) AS `lupa_l[0-9]+_c[0-9]+_minimo`"
    if (grepl(patron, statement)) {
      original <- regmatches(statement, regexpr(patron, statement))
      statement <- sub(
        original, sub("_minimo`$", "_otro`", original), statement, fixed = TRUE
      )
      .acp_estado$disparos <- .acp_estado$disparos + 1L
    }
    callNextMethod(conn, statement, ...)
  }
)

.acp_datos <- function() {
  data.frame(
    a = c(-9, 1, 2, 3, -9, 4, 5, 6),
    b = c(5, -9, 6, 7, 8, NA, 9, -9),
    c = c(1.5, 2.5, -9, -9, -9, 0, -3.5, 4),
    stringsAsFactors = FALSE
  )
}

.acp_conexion <- function(clase) {
  cruda <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(cruda, "datos", .acp_datos())
  derivada <- methods::new(clase)
  for (ranura in methods::slotNames(cruda)) {
    methods::slot(derivada, ranura) <- methods::slot(cruda, ranura)
  }
  list(cruda = cruda, conexion = derivada)
}

.acp_campos <- c(
  "n_validos", "minimo", "maximo", "media", "n_ceros", "n_negativos",
  "n_valores_excluidos_resumen", "n_aplicables", "n_no_aplica"
)

.acp_politicas <- list(
  centinelas = list(sentinelas_numericos = -9),
  centinelas_y_regla = list(
    sentinelas_numericos = -9, aplicabilidad = list(c = ~ a > 0)
  )
)

for (clase in c("SQLiteLoteRechazadoAusencia",
                "SQLiteAliasRenombradoAusencia")) {
  test_that(paste0(
    "una columna medida con su propia consulta publica lo que la politica ",
    "excluye (", clase, ")"
  ), {
    bases <- .acp_conexion(clase)
    on.exit(DBI::dbDisconnect(bases$cruda), add = TRUE)
    leida <- DBI::dbReadTable(bases$cruda, "datos")
    expect_gt(length(.acp_politicas), 0L)
    for (nombre in names(.acp_politicas)) {
      politica <- .acp_politicas[[nombre]]
      memoria <- suppressWarnings(do.call(perfilar, c(
        list(leida, analizar_dependencias = FALSE), politica
      )))$columnas
      for (bloque in c("con_muestra", "solo_agregados")) {
        .acp_estado$disparos <- 0L
        r <- suppressWarnings(do.call(perfilar_dbi, c(
          list(
            bases$conexion, "datos", bloque_muestra = bloque,
            fecha = as.POSIXct("2026-01-01", tz = "UTC"),
            proteger_datos_personales = FALSE, analizar_dependencias = FALSE
          ),
          politica
        )))
        info <- paste(clase, nombre, bloque)
        # La guarda corrio: la columna `c` no se midio en el lote.
        expect_gt(.acp_estado$disparos, 0L)
        sql <- r$resumen_tabla$sql
        propia <- sql$sql[sql$columna == "c" & sql$metrica == "minimo"]
        expect_length(propia, 1L)
        expect_false(grepl("lupa_l[0-9]+_c[0-9]+_", propia), info = info)
        columnas <- r$resumen_tabla$columnas
        # Las cuentas del universo se publican solo con un universo declarado.
        del_universo <- c("n_aplicables", "n_no_aplica")
        con_universo <- !is.null(politica$aplicabilidad)
        campos <- setdiff(
          intersect(.acp_campos, names(memoria)),
          if (!con_universo) del_universo
        )
        expect_true(all(c(
          "minimo", "n_valores_excluidos_resumen", if (con_universo) del_universo
        ) %in% campos), info = info)
        expect_true(all(campos %in% names(columnas)), info = info)
        for (columna in c("a", "b", "c")) {
          for (campo in campos) {
            expect_equal(
              as.numeric(columnas[[campo]][columnas$columna == columna]),
              as.numeric(memoria[[campo]][memoria$columna == columna]),
              tolerance = 1e-12,
              info = paste(info, columna, campo)
            )
          }
        }
        pol <- r$resumen_tabla$meta$politica_ausencia
        expect_identical(nrow(pol$sin_aplicar), 0L, info = info)
      }
    }
  })
}

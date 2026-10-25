# Fuera de CRAN: el check en Windows llego a 13 min (CRAN pide menos de 10) y
# esta prueba tarda mas de 0,2 s en modo CRAN (medicion del 2026-10-25). Corre en
# la suite completa, en el CI y en R-hub, que usan NOT_CRAN=true.
testthat::skip_on_cran()

# El vocabulario de `tipo_hallazgo` se publica en el articulo "Alcance y
# cobertura" del sitio, que lo CALCULA del codigo al construirse: recorre las
# funciones del paquete y junta los nombres que llegan como tipo al constructor
# de hallazgos y a los campos `tipo_hallazgo` o `hallazgo`. Una lista escrita a
# mano en la documentacion envejecio dos veces (58 cuando eran 59); calculada no
# puede, pero el calculo puede perder un nombre si el codigo arma el tipo de otra
# forma. Esta prueba lo ata con una lista explicita: un tipo nuevo, o uno que
# deje de emitirse, exige actualizarla, y el articulo no se construye si su
# calculo y esta lista no coinciden.
#
# La primera prueba recorre el espacio de nombres instalado y corre en todas
# partes, tambien bajo R CMD check. La segunda necesita el arbol de desarrollo
# (el articulo no viaja en el paquete): fuera de el se saltea diciendo por que;
# dentro, que falte el articulo es una falla.

ARTICULO_VOCABULARIO_PUBLICADO <- file.path(
  "vignettes", "articles", "alcance-y-cobertura.Rmd"
)

# Superficie completa que emite el paquete en esta version.
.tipos_hallazgo_emitidos <- sort(c(
  "alta_cardinalidad", "anio_de_dos_digitos",
  "bloqueo_por_con_perdida", "casi_clave",
  "casi_duplicados_vocabulario", "celdas_multivaluadas",
  "ceros_no_permitidos", "clave_con_ausentes", "clave_no_unica",
  "codificacion_invalida", "codificacion_rota", "columnas_duplicadas",
  "constante", "controles_invisibles", "coordenada_fuera_dominio",
  "crs_no_declarado", "dato_personal_posible", "desviacion_benford",
  "duplicados_aproximados", "duplicados_exactos_columnas",
  "duplicados_exactos_normalizados", "entidades_html",
  "espacios_sobrantes", "faltantes", "faltantes_disfrazados",
  "fecha_fallecimiento_fuera_rango", "fecha_nacimiento_fuera_rango",
  "fecha_partida_columnas", "filas_duplicadas", "formato_fecha_ambiguo",
  "formatos_fecha_mixtos", "geometria_invalida", "geometria_vacia",
  "integer64_fuera_precision_double", "mayusculas_inconsistentes",
  "monedas_mixtas", "negativos_no_permitidos",
  "nombres_columnas_problematicos", "normalizacion_unicode",
  "numero_como_texto", "outliers", "patron_raro",
  "posible_ausencia_estructural", "posible_centinela_numerico",
  "posible_identificador", "regla_silencia_ausencia",
  "relacion_aritmetica_columnas", "relacion_orden_columnas",
  "separadores_en_campo", "tipo_compuesto_no_analizado",
  "tipo_declarado_distinto", "tipos_geometria_mixtos", "unidades_mixtas",
  "valor_concentrado", "valor_fuera_de_aplicabilidad", "valores_no_finitos",
  "valores_subnormales", "variantes_equifrecuentes_vocabulario",
  "zona_horaria_fecha_hora"
))

# El mismo calculo que el articulo: las cadenas que puede devolver la expresion
# del tipo -un literal, las ramas de un `if` o un `ifelse`, los elementos de
# `c()`- en cada llamada al constructor de hallazgos y en cada argumento
# `tipo_hallazgo` o `hallazgo`.
.tipos_emitidos_por_el_codigo <- function(espacio = asNamespace("lupa")) {
  ramas <- function(e) {
    if (is.character(e)) return(e)
    if (!is.call(e)) return(character())
    f <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
    partes <- as.list(e)[-1L]
    if (f %in% c("if", "ifelse")) return(unlist(lapply(partes[-1L], ramas)))
    if (f == "c") return(unlist(lapply(partes, ramas)))
    if (f %in% c("rep", "(")) return(ramas(partes[[1L]]))
    if (f == "{") return(ramas(partes[[length(partes)]]))
    character()
  }
  tipos <- character()
  recorrer <- function(e) {
    if (!is.call(e)) return(invisible())
    nombres <- names(e)
    if (is.null(nombres)) nombres <- rep("", length(e))
    if (is.symbol(e[[1L]]) && identical(as.character(e[[1L]]), ".nuevo_hallazgo")) {
      posicionales <- which(nombres[-1L] == "") + 1L
      tipo <- if ("tipo" %in% nombres) e[["tipo"]] else e[[posicionales[[2L]]]]
      tipos <<- c(tipos, ramas(tipo))
    }
    for (i in seq_along(e)) {
      parte <- e[[i]]
      if (missing(parte)) next
      if (nombres[[i]] %in% c("tipo_hallazgo", "hallazgo")) {
        tipos <<- c(tipos, ramas(parte))
      }
      recorrer(parte)
    }
  }
  for (nombre in ls(espacio, all.names = TRUE)) {
    objeto <- get(nombre, envir = espacio)
    if (is.function(objeto) && !is.primitive(objeto)) recorrer(body(objeto))
  }
  sort(unique(tipos[grepl("^[a-z][a-z0-9_]*$", tipos)]))
}

test_that("el calculo encuentra un tipo nuevo y uno escrito en una rama", {
  # Primera mitad: el recorrido encuentra lo que busca. Sin esto, un calculo que
  # no mirara nada coincidiria con una lista vacia.
  espacio <- new.env()
  espacio$f <- function(x) .nuevo_hallazgo("col", if (x) "tipo_a" else "tipo_b")
  espacio$g <- function() data.frame(tipo_hallazgo = c("tipo_c", "tipo_d"))
  expect_identical(.tipos_emitidos_por_el_codigo(espacio),
                   c("tipo_a", "tipo_b", "tipo_c", "tipo_d"))
})

test_that("la lista explicita es exactamente lo que emite el codigo", {
  emitidos <- .tipos_emitidos_por_el_codigo()
  expect_gt(length(emitidos), 50L)
  expect_identical(setdiff(emitidos, .tipos_hallazgo_emitidos), character())
  expect_identical(setdiff(.tipos_hallazgo_emitidos, emitidos), character())
})

test_that("el articulo publica el vocabulario calculando la lista del codigo", {
  raiz <- normalizePath(testthat::test_path("..", ".."), winslash = "/",
                        mustWork = FALSE)
  skip_if_not(
    file.exists(file.path(raiz, "DESCRIPTION")) && dir.exists(file.path(raiz, "R")) &&
      dir.exists(file.path(raiz, "vignettes")),
    "Fuera del arbol de desarrollo: el articulo no viaja en el paquete."
  )
  ruta <- file.path(raiz, ARTICULO_VOCABULARIO_PUBLICADO)
  expect_true(file.exists(ruta), info = paste("No existe", ARTICULO_VOCABULARIO_PUBLICADO))
  if (!file.exists(ruta)) return(invisible())
  texto <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  # El articulo calcula la lista (no la escribe) y se compara con la de aca.
  expect_true(any(grepl(".nuevo_hallazgo", texto, fixed = TRUE)))
  expect_true(any(grepl(".tipos_hallazgo_emitidos", texto, fixed = TRUE)))
})

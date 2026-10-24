# Código de los ejemplos de los dos README de lupa.
#
# README.es.Rmd (castellano) y README.Rmd (inglés) leen este archivo con
# knitr::read_chunk(). Cada trozo empieza con una línea `## ---- nombre` y llega
# hasta el siguiente. Así los dos idiomas muestran el mismo código y la misma
# salida, y sólo la prosa se escribe dos veces.
#
# Hay dos clases de trozos:
#   - los que se muestran: el código que quien lee copia y corre;
#   - los ocultos (`include = FALSE` o `echo = FALSE` en el README): calculan las
#     cifras que la prosa cita con código en línea y comprueban con stopifnot()
#     lo que la prosa afirma. Si una afirmación deja de ser cierta, el README no
#     se teje.
#
# Los ocultos leen `params$idioma` ("es" o "en"), que declara el YAML de cada
# README, sólo para escribir los números y las listas en la forma de cada idioma.
#
# No se corre solo: lo teje data-raw/tejer-readmes.R, con el paquete cargado
# desde el árbol con pkgload::load_all().

## ---- preparar
options(width = 90)

## ---- instalar
install.packages("lupa")

## ---- instalar-desarrollo
# install.packages("pak")
pak::pak("sebollin/lupa")

## ---- treinta-segundos
library(lupa)
perfil <- perfilar(datos_operativos)
h <- hallazgos(perfil)
errores_primero <- order(h$severidad, decreasing = TRUE)
print(head(h[errores_primero, c("columna", "tipo_hallazgo", "severidad")], 8),
      row.names = FALSE)

## ---- treinta-segundos-cobertura
as.data.frame(table(diagnostico = cobertura(perfil)$diagnostico), responseName = "columnas")

## ---- cifras
idioma <- params$idioma
num <- function(x) {
  format(x, big.mark = if (idioma == "es") "." else ",",
         decimal.mark = if (idioma == "es") "," else ".",
         scientific = FALSE, trim = TRUE)
}
lista <- function(x) {
  if (length(x) < 2L) return(x)
  y <- if (idioma == "es") " y " else " and "
  paste0(paste(x[-length(x)], collapse = ", "), y, x[[length(x)]])
}
hallazgos_30s <- h
n_hallazgos <- nrow(hallazgos_30s)
n_errores <- sum(hallazgos_30s$severidad == "error")
n_sin_evaluar <- nrow(cobertura(perfil))
n_filas_datos <- nrow(datos_operativos)
n_columnas_datos <- ncol(datos_operativos)
# Los valores por omisión se leen de las firmas, no se copian.
por_omision <- function(funcion, argumento) {
  eval(formals(funcion)[[argumento]], asNamespace("lupa"))
}
muestra_perfilar <- num(por_omision(perfilar, "muestra"))
celdas_aviso_ancha <- num(por_omision(perfilar, "umbral_celdas_aviso_tabla_ancha"))
# Supuesto del código para estimar la espera; no tiene argumento propio.
celdas_por_segundo <- num(lupa:::.VELOCIDAD_REFERENCIA_TABLA_ANCHA)
max_celdas <- num(por_omision(perfilar_dbi, "max_celdas_muestra"))
max_mib <- num(por_omision(perfilar_dbi, "max_bytes_muestra") / 1024^2)
umbral_cardinalidad <- num(por_omision(perfilar_dbi, "umbral_cardinalidad"))
max_pares <- num(por_omision(detectar_duplicados_aproximados, "max_pares"))
requisitos <- requisitos_motor()
motores_probados <- lista(sort(unique(
  requisitos$motor[requisitos$estado_prueba == "probado"]
)))
# Evidencia real de un tipo de hallazgo de la corrida de los treinta segundos:
# las dos primeras partes, en código, para la tabla «Qué deja visible».
ev <- function(tipo, columna = NULL) {
  fila <- hallazgos_30s$tipo_hallazgo == tipo
  if (!is.null(columna)) fila <- fila & hallazgos_30s$columna %in% columna
  stopifnot(sum(fila) >= 1L)
  partes <- strsplit(hallazgos_30s$evidencia[which(fila)[[1L]]], "; ",
                     fixed = TRUE)[[1L]]
  if (length(partes) > 2L) partes <- c(partes[1:2], "...")
  paste0("`", paste(partes, collapse = "; "), "`")
}
# Cobertura de los tres marcos incluidos, contada sobre sus filas.
marcos <- list(AGESIC = marco_agesic(), CEPAL = marco_cepal(),
               ISO = marco_iso25012())
cob <- t(vapply(marcos, function(m) {
  d <- as.data.frame(m)
  c(filas = nrow(d),
    declarando = sum(d$disponibilidad == "disponible"),
    fuera = sum(d$disponibilidad == "fuera_de_alcance"),
    solo = sum(d$perfil_mide))
}, numeric(4L)))

## ---- comprobar-perfil
# Lo que la prosa de los treinta segundos y de «Cómo se lee» afirma.
stopifnot(
  is.ordered(hallazgos_30s$severidad),
  identical(levels(hallazgos_30s$severidad), c("ok", "sospechoso", "error")),
  n_errores >= 1L, n_sin_evaluar >= 1L,
  cob["ISO", "fuera"] == 0,
  isFALSE(por_omision(perfilar, "duplicados_aproximados")),
  identical(por_omision(perfilar, "max_celdas_muestra"),
            por_omision(perfilar_dbi, "max_celdas_muestra")),
  identical(por_omision(perfilar, "max_bytes_muestra"),
            por_omision(perfilar_dbi, "max_bytes_muestra")),
  all(cob[, "declarando"] + cob[, "fuera"] == cob[, "filas"])
)

## ---- clave
con_clave <- perfilar(datos_operativos, clave = "id_evento")
h <- hallazgos(con_clave)
fila <- which(h$tipo_hallazgo == "faltantes_disfrazados" &
                h$columna == "fecha_evento")
claves <- h$trazabilidad[[fila]]$claves
claves
datos_operativos[datos_operativos$id_evento %in% claves$id_evento,
                 c("id_evento", "fecha_evento")]

## ---- comprobar-clave
stopifnot(
  "clave_no_unica" %in% h$tipo_hallazgo,
  identical(h$trazabilidad[[fila]]$localizador, "clave_declarada")
)

## ---- base
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "eventos", datos_operativos)
en_la_base <- perfilar_dbi(con, "eventos")
names(en_la_base)

## ---- comprobar-base
# La tabla de las tres opciones y la falta de lectura instantánea, medidas.
stopifnot(
  identical(por_omision(perfilar_dbi, "universo")[[1L]], "tabla_completa"),
  identical(por_omision(perfilar_dbi, "muestra"), Inf),
  identical(por_omision(perfilar_dbi, "bloque_muestra")[[1L]], "con_muestra"),
  identical(por_omision(perfilar_dbi, "politica_costo")[[1L]], "todas"),
  isFALSE(en_la_base$resumen_tabla$meta$snapshot),
  isTRUE(en_la_base$perfil_muestra$meta$origen_dbi$muestreo$tabla_completa)
)
acotada <- perfilar_dbi(con, "eventos", muestra = 5)
stopifnot(
  acotada$resumen_tabla$meta$filas == n_filas_datos,
  acotada$perfil_muestra$meta$origen_dbi$muestreo$filas_obtenidas == 5,
  is.null(acotada$resumen_tabla$meta$muestreo)
)
en_el_motor <- perfilar_dbi(con, "eventos", universo = "muestra_motor",
                            muestra_motor = 5)
stopifnot(
  identical(en_el_motor$resumen_tabla$meta$universo, "muestra_motor"),
  isFALSE(en_el_motor$resumen_tabla$meta$snapshot),
  en_el_motor$resumen_tabla$meta$muestreo$filas_obtenidas == 5
)
solo_agregados <- perfilar_dbi(con, "eventos",
                               bloque_muestra = "solo_agregados")
stopifnot(is.null(solo_agregados$perfil_muestra))
# Un tope que recorta la lectura queda en cobertura(), con el tope que mandó.
recortada <- perfilar_dbi(con, "eventos", max_celdas_muestra = 50)
recorte <- cobertura(recortada)
stopifnot(
  identical(recortada$perfil_muestra$meta$tope_que_mando, "celdas"),
  any(recorte$bloque == "perfil_muestra" & recorte$estado == "degradado")
)
DBI::dbDisconnect(con)

## ---- informe
analisis <- analizar(datos_operativos)
reportar(analisis, archivo = "informe-lupa.html")

## ---- comprobar-informe
# El trozo de arriba se muestra sin correr para no escribir en el árbol; acá
# se corre igual, sobre un temporal.
analisis <- analizar(datos_operativos)
archivo <- tempfile(fileext = ".html")
reportar(analisis, archivo = archivo)
stopifnot(
  file.exists(archivo), file.size(archivo) > 0,
  c("perfil", "distribuciones", "asociaciones", "temporal",
    "propuesta_modelo", "tablero", "plan_limpieza") %in% names(analisis)
)
unlink(archivo)

## ---- cita
citation("lupa")

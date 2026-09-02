## Firma de las mediciones: que commit produjo los numeros de un CSV.
##
## Lo usan `medir_figuras.R` y `perdida_lsh.R`, que escriben CSV con una columna
## `commit`. Vive aparte porque la logica estaba copiada en los dos y una copia
## se arregla sin la otra: el hueco que motivo este archivo -el medidor mismo
## fuera de la lista de rutas firmadas- estaba en las dos y se corrigio en una.
##
## Que promete la firma, exactamente:
##
##   - `commit` es el `HEAD` del arbol desde el que se corrio;
##   - `+sucio` avisa que alguna de las rutas firmadas tiene cambios sin
##     commitear, o sea que ese commit no describe lo que se midio;
##   - la guarda del `Built` rechaza medir con una instalacion ARMADA ANTES del
##     ultimo commit de esas rutas.
##
## Que NO promete: que la instalacion medida sea ese commit. Es una condicion
## necesaria y no suficiente -un armado posterior puede venir de otro arbol-.
## Atrapa el caso frecuente, que es la instalacion vieja olvidada en la
## biblioteca por omision; no atrapa a quien quiera enganarla.

# Las rutas cuyo cambio sin commitear invalida la firma: el codigo que se
# instala, el codigo que mide, y el codigo que firma. Que el medidor quedara
# afuera fue un hueco real -un CSV podia firmarse con un commit teniendo el
# guion que lo produjo modificado, o sin versionar-.
#
# Este archivo se incluye a si mismo, y no es cosmetico: una refutacion mostro
# que con `_firma.R` afuera bastaba con sacarle la rama `+sucio` para que un
# arbol sucio se firmara limpio, sin que nada lo notara. El archivo que decide
# el veredicto era justo el que el veredicto no cubria.
#
# `.Rbuildignore` tambien entra, y por una razon que se demostro construyendo:
# agregandole una linea sin commitear se puede sacar del tarball un archivo de
# `R/` que si esta commiteado. Quien instale ese tarball mide un paquete al que
# le falta codigo del commit firmado, con la firma diciendo que esta limpio.
#
# `benchmark/datos` queda afuera a proposito: lo escribe la propia corrida y
# marcaria siempre.
.rutas_firma <- function(guion) {
  c("R", "DESCRIPTION", "NAMESPACE", "src", "inst", ".Rbuildignore",
    "benchmark/_firma.R", file.path("benchmark", guion))
}

.commit_actual <- function(directorio_repositorio, rutas_firma) {
  git <- Sys.which("git")
  # Sin git no hay firma. El `NA` viaja igual en la columna `commit`, pero
  # callarlo en la consola deja creer que la corrida quedo identificada.
  sin_firma <- function(motivo) {
    cat("AVISO: los CSV van a quedar SIN FIRMA (commit = NA): ", motivo, "\n",
        sep = "")
    NA_character_
  }
  if (!nzchar(git)) return(sin_firma("no hay git en el PATH"))
  x <- tryCatch(
    suppressWarnings(system2(
      git,
      c("-C", shQuote(directorio_repositorio), "rev-parse", "--short", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  x <- trimws(x)
  if (!length(x) || !nzchar(x[[1L]])) {
    return(sin_firma(paste0("git no devolvio un commit en ",
                            directorio_repositorio)))
  }
  sucio <- tryCatch(
    suppressWarnings(system2(
      git,
      c("-C", shQuote(directorio_repositorio), "status", "--porcelain", "--",
        rutas_firma),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  if (length(sucio) && any(nzchar(trimws(sucio)))) {
    paste0(x[[1L]], "+sucio")
  } else {
    x[[1L]]
  }
}

# Fecha del sello `Built` de una instalacion, en UTC. `Built` tiene la forma
# "R 4.6.1; ; 2026-09-02 14:02:35 UTC; unix": el tercer campo es la fecha.
.fecha_built <- function(built) {
  tryCatch({
    partes <- strsplit(built, ";", fixed = TRUE)[[1L]]
    if (length(partes) < 3L) return(NA)
    as.POSIXct(
      sub("[[:space:]]+UTC$", "", trimws(partes[[3L]])),
      tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
    )
  }, error = function(e) NA)
}

.fecha_ultimo_commit <- function(directorio_repositorio, rutas_firma) {
  git <- Sys.which("git")
  if (!nzchar(git)) return(NA)
  x <- tryCatch(
    suppressWarnings(system2(
      git,
      c("-C", shQuote(directorio_repositorio), "log", "-1", "--format=%ct", "--",
        rutas_firma),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  x <- suppressWarnings(as.numeric(trimws(x)))
  if (!length(x) || is.na(x[[1L]])) return(NA)
  as.POSIXct(x[[1L]], origin = "1970-01-01", tz = "UTC")
}

# Devuelve TRUE si la guarda tenia que parar y se saltea a pedido: quien llama
# le agrega `+singuarda` al commit, para que quede en el dato y no solo en la
# consola. Para y no sigue si la instalacion es anterior al commit.
.guarda_built <- function(built, directorio_repositorio, rutas_firma,
                          instalacion, commit,
                          variable = "LUPA_FIGURAS_SIN_GUARDA_BUILT") {
  sin_guarda <- identical(Sys.getenv(variable), "1")
  fecha_built <- .fecha_built(built)
  fecha_commit <- .fecha_ultimo_commit(directorio_repositorio, rutas_firma)
  if (is.na(fecha_built) || is.na(fecha_commit)) {
    # No se inventa un veredicto: se dice que la guarda no pudo correr.
    cat("guarda del Built: no se pudo comparar (sello o commit ilegibles)\n")
    return(FALSE)
  }
  if (fecha_built >= fecha_commit) {
    cat(
      "guarda del Built: OK, el armado es posterior al ultimo commit del codigo (",
      format(fecha_commit, "%Y-%m-%d %H:%M:%S", tz = "UTC"), " UTC)\n",
      sep = ""
    )
    return(FALSE)
  }
  mensaje <- paste0(
    "La instalacion que se va a medir es ANTERIOR al codigo que se firmaria.\n",
    "  armada:      ", format(fecha_built, "%Y-%m-%d %H:%M:%S", tz = "UTC"), " UTC\n",
    "  ultimo commit de las rutas firmadas: ",
    format(fecha_commit, "%Y-%m-%d %H:%M:%S", tz = "UTC"), " UTC (", commit, ")\n",
    "  instalacion: ", instalacion, "\n",
    "Los CSV quedarian firmados por un commit que no produjo esos numeros.\n",
    "Reinstale el arbol (R CMD INSTALL .) o apunte R_LIBS a la biblioteca correcta.\n",
    "Para medir igual, a sabiendas: ", variable, "=1 (los CSV se firman con el ",
    "sufijo +singuarda)."
  )
  if (!sin_guarda) stop(mensaje, call. = FALSE)
  cat("guarda del Built: SALTEADA a pedido.\n", mensaje, "\n", sep = "")
  TRUE
}

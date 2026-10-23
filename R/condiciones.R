# Los errores del paquete se levantan con `.detener()`, que arma la condicion con
# el texto en su campo `message`, y no con el `stop()` de base. Con texto, el `stop()` de
# base pasa el mensaje por `gettext()` -que, sin catalogo, lo devuelve en la
# codificacion nativa- y despues lo traduce al nativo otra vez: bajo el locale
# `C` lo que no es ASCII sale escapado, `conditionMessage()` devolvia
# "num<U+00E9>rico", y ese texto entraba como dato al plan -`justificacion`,
# `motivo_no_reversible`-, al registro de `aplicar()`, a la guia, a `analizar()`
# y al HTML. El paquete no trae catalogo de traducciones, asi que
# el mensaje se arma sin `gettext()` -salvo que quien llama nombre un dominio- y
# viaja en una condicion armada, que no se traduce: llega a quien la atrapa con
# sus bytes y su marca, en cualquier locale. Sin atrapar, R la imprime como a
# cualquier error. Una condicion ya armada -un error que se vuelve a levantar-
# pasa como esta. El ayudante tiene nombre propio: ningun nombre de base queda
# tapado en el espacio de nombres, y una prueba recorre el codigo del paquete y no encuentra otro `stop()` ni
# `warning()` de base fuera de estos dos ayudantes.
.detener <- function(..., call. = TRUE, domain = NULL) {
  argumentos <- list(...)
  if (length(argumentos) == 1L && inherits(argumentos[[1L]], "condition")) {
    return(base::stop(argumentos[[1L]]))
  }
  mensaje <- .mensaje_condicion(..., domain = domain)
  llamada <- if (isTRUE(call.) && sys.nframe() > 1L) sys.call(-1L) else NULL
  base::stop(simpleError(mensaje, llamada))
}

# El texto de la condicion, como lo arma `stop()` de base -cada argumento como
# texto, unidos sin separador- pero sin `gettext()` salvo que se nombre un
# dominio. `.makeMessage(domain = NA)` pegaba la LISTA de argumentos, y un
# argumento `NULL` -un `if` sin `else`- salia escrito "character(0)": "No se
# pudo leer como fecha: \"...\"character(0). Se espera...". En
# base el `NULL` desaparece porque `gettext()` aplana la lista; aca se aplana
# sin traducir, con las marcas de cada pieza intactas.
.mensaje_condicion <- function(..., domain = NULL) {
  piezas <- unlist(lapply(list(...), as.character), use.names = FALSE)
  if (!length(piezas)) return("")
  if (!is.null(domain) && !is.na(domain)) {
    piezas <- gettext(piezas, domain = domain)
  }
  paste(piezas, collapse = "")
}

# `.avisar()`: lo mismo para los avisos. `warning()` de base pasa el texto por
# `gettext()` igual que `stop()`, y bajo `C` el aviso de `medir()` llegaba a quien
# lo atrapa como "1 m<U+00E9>trica(s) no se midieron...". Y el texto de un aviso
# es dato: `perfilar_coleccion()` junta los de cada tabla, los pasa por el piso
# de proteccion y los vuelve a emitir, y `analizar()` reconoce el de `medir()`
# por su texto. Un aviso ya armado -una condicion- pasa como esta. Ningun sitio
# del paquete pide `immediate.` ni `noBreaks.`: se aceptan para no romper la
# firma de base, y con texto se respetan pasandolos a base.
.avisar <- function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE,
                    domain = NULL) {
  argumentos <- list(...)
  if (length(argumentos) == 1L && inherits(argumentos[[1L]], "condition")) {
    return(base::warning(argumentos[[1L]]))
  }
  if (isTRUE(immediate.) || isTRUE(noBreaks.)) {
    return(base::warning(..., call. = call., immediate. = immediate.,
                         noBreaks. = noBreaks., domain = domain))
  }
  mensaje <- .mensaje_condicion(..., domain = domain)
  llamada <- if (isTRUE(call.) && sys.nframe() > 1L) sys.call(-1L) else NULL
  invisible(base::warning(simpleWarning(mensaje, llamada)))
}

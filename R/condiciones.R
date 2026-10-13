# Los errores del paquete se levantan como una condicion armada aca, con el texto
# en su campo `message`, y no con el `stop()` de base. Con texto, el `stop()` de
# base pasa el mensaje por `gettext()` -que, sin catalogo, lo devuelve en la
# codificacion nativa- y despues lo traduce al nativo otra vez: bajo el locale
# `C` lo que no es ASCII sale escapado, `conditionMessage()` devolvia
# "num<U+00E9>rico", y ese texto entraba como dato al plan -`justificacion`,
# `motivo_no_reversible`-, al registro de `aplicar()`, a la guia, a `analizar()`
# y al HTML (ronda 31-S). El paquete no trae catalogo de traducciones, asi que
# el mensaje se arma sin `gettext()` -salvo que quien llama nombre un dominio- y
# viaja en una condicion armada, que no se traduce: llega a quien la atrapa con
# sus bytes y su marca, en cualquier locale. Sin atrapar, R la imprime como a
# cualquier error. Este `stop()` tapa al de base en todo el espacio de nombres,
# asi que ningun sitio tiene que acordarse; una condicion ya armada -un error
# que se vuelve a levantar- pasa como esta.
stop <- function(..., call. = TRUE, domain = NULL) {
  argumentos <- list(...)
  if (length(argumentos) == 1L && inherits(argumentos[[1L]], "condition")) {
    return(base::stop(argumentos[[1L]]))
  }
  mensaje <- .makeMessage(..., domain = if (is.null(domain)) NA else domain)
  llamada <- if (isTRUE(call.) && sys.nframe() > 1L) sys.call(-1L) else NULL
  base::stop(simpleError(mensaje, llamada))
}

.fijar_locale_n61 <- function(locale) {
  categorias <- c("LC_CTYPE", "LC_COLLATE")
  ok <- vapply(categorias, function(categoria) {
    suppressWarnings(Sys.setlocale(categoria, locale))
    identical(Sys.getlocale(categoria), locale)
  }, logical(1L))
  all(ok)
}

.primer_locale_utf8_n61 <- function() {
  candidatos <- c(
    "es_UY.UTF-8", "es_UY.utf8", "es_ES.UTF-8", "es_ES.utf8",
    "en_US.UTF-8", "en_US.utf8", "C.UTF-8", "C.utf8"
  )
  for (candidato in candidatos) {
    if (.fijar_locale_n61(candidato)) {
      return(candidato)
    }
  }
  NULL
}

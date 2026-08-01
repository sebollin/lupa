.registrar_as_tibble <- function(...) {
  registerS3method(
    "as_tibble", "perfil", as_tibble.perfil,
    envir = asNamespace("tibble")
  )
}

.onLoad <- function(libname, pkgname) {
  if ("tibble" %in% loadedNamespaces()) {
    .registrar_as_tibble()
  } else {
    setHook(packageEvent("tibble", "onLoad"), .registrar_as_tibble)
  }
}

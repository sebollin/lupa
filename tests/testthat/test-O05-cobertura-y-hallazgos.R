# O05. Un diagnostico declinado por escrito no puede salir "resuelto".
#
# La cobertura declina `proximidad_vocabulario` y el hallazgo que ese
# diagnostico produce se llama `casi_duplicados_vocabulario`. La deriva armaba
# la clave con el primer nombre y la comparaba contra el segundo: nunca
# casaban, y un diagnostico que el perfil nuevo decia no haber evaluado se
# informaba **resuelto, severidad ok**, con `Norte` y `norte` todavia en la
# columna.
#
# No era un sitio sino una familia: al enumerar TODOS los nombres que la
# cobertura escribe, dos tenian hallazgo con otro nombre. La guarda de mas
# abajo recorre esos nombres leyendo las fuentes, asi que un diagnostico nuevo
# que se desencuentre falla aca en vez de callar en la deriva.

.o05_raiz <- function() {
  raiz <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  if (!file.exists(file.path(raiz, "DESCRIPTION"))) return(NULL)
  raiz
}

test_that("un diagnostico que el perfil nuevo declino sale no_evaluado", {
  largo <- paste(rep("X", 10001L), collapse = "")
  antes <- data.frame(v = c("Norte", "norte", "Sur", "sur", "Este", "Oeste"),
                      stringsAsFactors = FALSE)
  despues <- data.frame(v = c("Norte", "norte", "Sur", "sur", "Este", "Oeste", largo),
                        stringsAsFactors = FALSE)
  deriva <- as.data.frame(comparar_perfiles(
    perfilar(antes, analizar_dependencias = FALSE,
             fecha = as.POSIXct("2026-01-01", tz = "UTC")),
    perfilar(despues, analizar_dependencias = FALSE,
             fecha = as.POSIXct("2026-01-02", tz = "UTC"))
  ))
  fila <- deriva[as.character(deriva$aspecto) == "hallazgo" &
                   as.character(deriva$valor_anterior) == "casi_duplicados_vocabulario", ,
                 drop = FALSE]
  expect_equal(nrow(fila), 1L)
  expect_identical(as.character(fila$cambio), "no_evaluado")
})

test_that("cada nombre de la cobertura casa con un hallazgo, esta mapeado o no produce ninguno", {
  raiz <- .o05_raiz()
  skip_if(is.null(raiz), "las fuentes del paquete no estan a la vista")
  fuentes <- vapply(
    list.files(file.path(raiz, "R"), pattern = "[.]R$", full.names = TRUE),
    function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    character(1L)
  )
  todo <- paste(fuentes, collapse = "\n")

  tipos <- unique(regmatches(
    todo, gregexpr('\\.nuevo_hallazgo\\(\\s*[^,]+?,\\s*"[a-z_]+"', todo, perl = TRUE)
  )[[1L]])
  tipos <- unique(sub('^.*"([a-z_]+)"$', "\\1", tipos))
  # Control de la sonda: si no leyo nada, todo lo de abajo pasaria en falso.
  expect_true("outliers" %in% tipos)
  expect_gt(length(tipos), 20L)

  # TRES constructores declaran cobertura, no uno. La primera version de esta
  # guarda miraba solo `.nuevo_diagnostico_no_evaluado` y veia 17 de 54
  # declaraciones: `validez_geometria`, escrita con `agregar_cobertura()`, se
  # desencontraba de `geometria_invalida` sin que esto se enterara. Una guarda
  # con un punto ciego es peor que ninguna, porque da confianza.
  constructores <- c(
    "\\.nuevo_diagnostico_no_evaluado", "agregar_cobertura",
    "\\.registro_cobertura_dbi"
  )
  diagnosticos <- character()
  for (constructor in constructores) {
    encontrados <- regmatches(
      todo, gregexpr(paste0(constructor, '\\(\\s*"[a-z_]+"'), todo, perl = TRUE)
    )[[1L]]
    # Cada constructor tiene que aportar algo: si uno deja de encontrarse, la
    # sonda dejo de leerlo, y eso no puede pasar en silencio.
    expect_gt(length(encontrados), 0L, label = constructor)
    diagnosticos <- c(diagnosticos, sub('^.*"([a-z_]+)"$', "\\1", encontrados))
  }
  diagnosticos <- unique(diagnosticos)
  expect_true(all(c("proximidad_vocabulario", "validez_geometria") %in% diagnosticos))

  mapeados <- names(lupa:::.hallazgos_por_diagnostico)
  # Declaran cobertura sin producir un hallazgo propio: no hay nada con que
  # desencontrarse. `texto_no_descifrable` en particular NO se mapea: avisa
  # que ciertos valores quedaron fuera de TODOS los diagnosticos de texto, y
  # atarlo a uno solo seria falso.
  sin_hallazgo <- c(
    "relacion_aritmetica_columnas", "texto_no_descifrable",
    "dependencias_funcionales", "comparar_equivalencia",
    # Notas PARCIALES: el chequeo se hizo, sobre menos valores o menos
    # dimensiones. No declinan un hallazgo entero.
    "resumen_cuantitativo", "dimensiones_geometria_no_evaluadas",
    # Una de tres condiciones de `posible_identificador`: sin ella el hallazgo
    # igual puede salir por las otras dos. Mapearla haria decir `no_evaluado`
    # sobre algo que tal vez se resolvio, y mapear de mas tambien miente.
    "secuencia_entera",
    # Notas de proceso de la lectura por DBI, sin hallazgo propio.
    "consistencia", "corroboracion", "fuente_bloques", "perfil_muestra",
    "resumen_tabla"
  )

  huerfanos <- setdiff(diagnosticos, c(tipos, mapeados, sin_hallazgo))
  expect_identical(huerfanos, character())

  # Y lo mapeado apunta a hallazgos que existen.
  expect_true(all(unlist(lupa:::.hallazgos_por_diagnostico) %in% tipos))
})

test_that("las dos listas de la familia del vocabulario dicen lo mismo", {
  # `.hallazgos_por_diagnostico` cuelga dos hallazgos de
  # `proximidad_vocabulario` y `.diagnosticos_relacion_textual` nombraba uno
  # solo: `variantes_equifrecuentes_vocabulario` quedaba afuera del motivo de
  # `no_evaluado` por cambio de tipo. Una regla escrita dos veces.
  expect_setequal(
    lupa:::.diagnosticos_relacion_textual,
    lupa:::.hallazgos_por_diagnostico$proximidad_vocabulario
  )
})

test_that("la geometria declinada se mapea a los hallazgos que suprime", {
  mapa <- lupa:::.hallazgos_por_diagnostico
  expect_identical(mapa$validez_geometria, "geometria_invalida")
  expect_identical(mapa$dominio_geometria, "coordenada_fuera_dominio")
  # Sin perfil geometrico no se evalua ninguno.
  expect_setequal(
    mapa$perfil_geometria,
    c("coordenada_fuera_dominio", "crs_no_declarado", "geometria_invalida",
      "geometria_vacia", "tipos_geometria_mixtos")
  )
})

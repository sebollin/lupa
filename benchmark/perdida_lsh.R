# Cuanto pierde el tamiz LSH: medicion externa, fuera del objeto.
#
# En el camino LSH hay dos medidas seguidas. El tamiz arma candidatos con
# Jaccard de q-gramas y **no conoce `metodo`**; recien despues se mide con el
# metodo pedido. Los pares que el metodo final habria aceptado pero el tamiz no
# propuso no se pierden en silencio: hoy ni se cuentan.
#
# Por que esto NO va dentro del objeto: la perdida real solo se puede medir
# corriendo tambien el camino exhaustivo sobre la misma entrada. Publicarla en
# el `alcance` de una corrida LSH normal seria informar como medida una perdida
# que esa corrida no midio, que es exactamente lo que el paquete no hace. Asi
# que vive aca, como banco, con su corpus y sus parametros declarados.
#
# Diseno experimental: se neutralizan las otras causas de perdida -presupuesto,
# tope de resultados, muestreo, bloqueo- poniendolas en valores no vinculantes,
# de modo que la diferencia entre los dos caminos sea atribuible al tamiz. Sin
# eso, "el LSH perdio N pares" mezclaria seis causas distintas.

library(lupa)

.vocabulario_sintetico <- function(n, semilla) {
  set.seed(semilla)
  base <- c("MONTEVIDEO", "CANELONES", "MALDONADO", "SALTO", "PAYSANDU",
            "RIVERA", "TACUAREMBO", "ARTIGAS", "DURAZNO", "FLORIDA")
  nombres <- paste0(
    sample(base, n, replace = TRUE), " ",
    sprintf("%05d", sample.int(99999L, n, replace = TRUE))
  )
  # Se siembran variantes cercanas: una edicion sobre valores existentes.
  cuantos <- max(2L, floor(n * 0.05))
  posiciones <- sample.int(n, cuantos)
  nombres[posiciones] <- sub("O", "0", nombres[posiciones], fixed = TRUE)
  nombres
}

medir_perdida_lsh <- function(n, semilla, metodo = "jw", umbral = 0.1,
                              lsh_bandas = 12L, lsh_filas = 4L, lsh_q = 3L) {
  datos <- data.frame(valor = .vocabulario_sintetico(n, semilla),
                      stringsAsFactors = FALSE)

  comunes <- list(
    datos = datos, columnas = "valor", metodo = metodo, umbral = umbral,
    muestra = Inf, max_pares = Inf, max_resultados = Inf,
    proteger_datos_personales = FALSE
  )

  exhaustivo <- do.call(
    detectar_duplicados_aproximados,
    c(comunes, list(estrategia = "teselas"))
  )
  aproximado <- do.call(
    detectar_duplicados_aproximados,
    c(comunes, list(
      estrategia = "lsh", lsh_bandas = lsh_bandas, lsh_filas = lsh_filas,
      lsh_q = lsh_q, presupuesto_pares = Inf
    ))
  )

  clave <- function(pares) {
    if (!nrow(pares)) return(character())
    paste(pmin(pares$fila_1, pares$fila_2), pmax(pares$fila_1, pares$fila_2))
  }
  encontrados_exhaustivo <- clave(exhaustivo$pares)
  encontrados_lsh <- clave(aproximado$pares)
  perdidos <- setdiff(encontrados_exhaustivo, encontrados_lsh)
  agregados <- setdiff(encontrados_lsh, encontrados_exhaustivo)

  data.frame(
    n = n,
    semilla = semilla,
    metodo = metodo,
    umbral = umbral,
    lsh_bandas = lsh_bandas,
    lsh_filas = lsh_filas,
    lsh_q = lsh_q,
    pares_exhaustivo = length(encontrados_exhaustivo),
    pares_lsh = length(encontrados_lsh),
    perdidos = length(perdidos),
    agregados = length(agregados),
    perdida = if (length(encontrados_exhaustivo)) {
      length(perdidos) / length(encontrados_exhaustivo)
    } else NA_real_,
    stringsAsFactors = FALSE
  )
}

if (identical(environment(), globalenv())) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Este banco necesita el paquete opcional 'stringdist'.")
  }
  argumentos <- commandArgs(trailingOnly = FALSE)
  marca_archivo <- "--file="
  rutas_script <- argumentos[startsWith(argumentos, marca_archivo)]
  directorio_script <- if (length(rutas_script) == 1L) {
    ruta_script <- sub(marca_archivo, "", rutas_script, fixed = TRUE)
    ruta_script <- gsub("~+~", " ", ruta_script, fixed = TRUE)
    dirname(normalizePath(ruta_script, mustWork = TRUE))
  } else {
    getwd()
  }
  # La firma y su guarda van ANTES de medir: de nada sirve enterarse de que se
  # midio la instalacion equivocada despues de pagar la corrida.
  source(file.path(directorio_script, "_firma.R"), local = TRUE)
  directorio_repositorio <- dirname(directorio_script)
  rutas_firma <- .rutas_firma("perdida_lsh.R")
  commit_actual <- .commit_actual(directorio_repositorio, rutas_firma)
  descripcion_lupa <- utils::packageDescription("lupa")
  ruta_lupa <- tryCatch(find.package("lupa"), error = function(e) NA_character_)
  cat("lupa medida: version ", as.character(descripcion_lupa[["Version"]]), "\n", sep = "")
  cat("Built: ", descripcion_lupa[["Built"]], "\n", sep = "")
  cat("instalacion: ", ruta_lupa, "\n", sep = "")
  cat("commit: ", commit_actual, "\n", sep = "")
  if (isTRUE(.guarda_built(descripcion_lupa[["Built"]], directorio_repositorio,
                           rutas_firma, ruta_lupa, commit_actual))) {
    commit_actual <- paste0(commit_actual, "+singuarda")
  }
  configuraciones <- expand.grid(
    n = c(400L, 800L),
    semilla = c(1L, 2L, 3L),
    lsh_bandas = c(8L, 12L, 20L),
    KEEP.OUT.ATTRS = FALSE
  )
  filas <- lapply(seq_len(nrow(configuraciones)), function(i) {
    fila <- configuraciones[i, ]
    medir_perdida_lsh(
      n = fila$n, semilla = fila$semilla, lsh_bandas = fila$lsh_bandas
    )
  })
  resultados <- do.call(rbind, filas)

  cat("\n=== Perdida del tamiz LSH ===\n\n")
  cat("Las otras causas de perdida estan neutralizadas: muestra, max_pares,\n")
  cat("max_resultados y presupuesto_pares en Inf, sin bloqueo. La diferencia\n")
  cat("entre los dos caminos es atribuible al tamiz.\n\n")
  print(resultados[, c("n", "semilla", "lsh_bandas", "pares_exhaustivo",
                       "pares_lsh", "perdidos", "perdida")],
        row.names = FALSE)

  resumen <- aggregate(
    perdida ~ lsh_bandas, data = resultados,
    FUN = function(x) c(media = mean(x), maxima = max(x))
  )
  cat("\n=== Perdida por numero de bandas ===\n\n")
  print(resumen, row.names = FALSE)
  cat("\nMas bandas es un tamiz mas permisivo: propone mas candidatos y pierde\n")
  cat("menos, a cambio de mas comparaciones.\n")

  # El contraste que impide leer el numero de arriba como una propiedad del
  # metodo. La perdida depende de cuanto MAS permisivo sea el metodo final que
  # el tamiz: con un umbral estricto los pocos pares reales son muy parecidos,
  # y el tamiz de Jaccard los propone casi todos.
  umbrales <- c(0.02, 0.05, 0.10, 0.20)
  por_umbral <- do.call(rbind, lapply(umbrales, function(u) {
    medir_perdida_lsh(n = 800L, semilla = 1L, umbral = u, lsh_bandas = 12L)
  }))

  directorio_datos <- file.path(directorio_script, "datos")
  dir.create(directorio_datos, recursive = TRUE, showWarnings = FALSE)
  salida_bandas <- data.frame(
    filas = resultados$n,
    semilla = resultados$semilla,
    lsh_bandas = resultados$lsh_bandas,
    umbral = resultados$umbral,
    metodo = resultados$metodo,
    pares_exhaustivo = resultados$pares_exhaustivo,
    pares_lsh = resultados$pares_lsh,
    perdidos = resultados$perdidos,
    perdida = resultados$perdida,
    stringsAsFactors = FALSE
  )
  salida_bandas$commit <- rep(commit_actual, nrow(salida_bandas))
  utils::write.csv(
    salida_bandas,
    file = file.path(directorio_datos, "perdida_lsh_bandas.csv"),
    row.names = FALSE
  )
  salida_umbral <- data.frame(
    filas = por_umbral$n,
    lsh_bandas = por_umbral$lsh_bandas,
    umbral = por_umbral$umbral,
    pares_exhaustivo = por_umbral$pares_exhaustivo,
    pares_lsh = por_umbral$pares_lsh,
    perdidos = por_umbral$perdidos,
    perdida = por_umbral$perdida,
    stringsAsFactors = FALSE
  )
  salida_umbral$commit <- rep(commit_actual, nrow(salida_umbral))
  utils::write.csv(
    salida_umbral,
    file = file.path(directorio_datos, "perdida_lsh_umbral.csv"),
    row.names = FALSE
  )
  cat("\n=== Perdida segun cuan permisivo sea el metodo final ===\n\n")
  print(por_umbral[, c("umbral", "pares_exhaustivo", "pares_lsh", "perdidos",
                       "perdida")],
        row.names = FALSE)
  cat("\nLa perdida NO es una propiedad del tamiz: es la distancia entre lo que\n")
  cat("el tamiz propone y lo que el metodo final acepta. Con un umbral estricto\n")
  cat("los pocos pares reales son muy parecidos y el tamiz los propone; con uno\n")
  cat("permisivo el metodo acepta pares que Jaccard de q-gramas no acerca.\n")
  cat("\nPor eso este numero no puede ir en el `alcance` de una corrida: depende\n")
  cat("del corpus y de los parametros, y esa corrida no lo midio.\n")
}

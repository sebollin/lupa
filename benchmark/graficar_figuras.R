# Figuras del banco de mediciones, para el README y el sitio.
#
#   Rscript benchmark/graficar_figuras.R [dir_datos] [dir_salida]
#
# Lee los CSV que deja `benchmark/medir_figuras.R` (y los dos de
# `benchmark/perdida_lsh.R`) y dibuja cuatro figuras con R base y cairo:
#
#   banco-escala.png         candidatos, tiempo, memoria e hilos segun filas
#   banco-estimacion.png     el error de lo previsto antes de recorrer, y lo
#                            que el bloqueo por clave pierde y declara
#   banco-cardinalidad.png   mismas filas, distinta repeticion de valores
#   banco-tamiz.png          lo que pierde el tamiz LSH segun umbral y bandas
#
# Toda cifra dibujada sale del CSV; el guion no tiene numeros propios. El pie
# de cada figura declara commit, fecha y maquina de la corrida que la produjo,
# tomados de `entorno.csv`. Solo R base: no agrega dependencias graficas.
#
# El archivo es UTF-8: los rotulos de las figuras llevan acentos.

args <- commandArgs(TRUE)
aqui <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(a)) return(getwd())
  # Rscript codifica los espacios de la ruta como "~+~"
  dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", a[[1L]]), fixed = TRUE)))
})
dir_datos  <- if (length(args) >= 1L) args[[1L]] else file.path(aqui, "datos")
dir_salida <- if (length(args) >= 2L) args[[2L]] else file.path(aqui, "..", "man", "figures")
dir.create(dir_salida, showWarnings = FALSE, recursive = TRUE)

leer <- function(nombre, columnas) {
  ruta <- file.path(dir_datos, nombre)
  if (!file.exists(ruta)) stop("falta ", ruta, ": correr antes benchmark/medir_figuras.R")
  d <- utils::read.csv(ruta, stringsAsFactors = FALSE, check.names = FALSE)
  faltan <- setdiff(columnas, names(d))
  if (length(faltan)) stop(nombre, " no tiene las columnas ", paste(faltan, collapse = ", "))
  if (!nrow(d)) stop(nombre, " no tiene filas: la medicion no dejo nada que dibujar")
  d
}

entorno      <- leer("entorno.csv", c("fecha", "version_lupa", "r_version", "hilos_stringdist",
                                      "metodo", "umbral", "p_jw", "commit"))
escala       <- leer("escala.csv", c("filas", "candidatos", "error_estimacion", "recall_techo",
                                     "tiempo_s_1", "tiempo_s_mediana", "rss_mib", "commit"))
hilos        <- leer("hilos.csv", c("filas", "nucleos", "tiempo_s_1", "tiempo_s_mediana",
                                    "candidatos", "pares_informados", "commit"))
cardinalidad <- leer("cardinalidad.csv", c("nivel", "filas", "candidatos", "candidatos_por_fila",
                                           "veces_respecto_alta", "commit"))
bloqueo      <- leer("bloqueo.csv", c("filas", "clave", "pares_sin_bloqueo", "pares_con_bloqueo",
                                      "perdidos_reales", "perdidos_estimados", "error_estimacion",
                                      "commit"))
lsh_bandas   <- leer("perdida_lsh_bandas.csv", c("filas", "semilla", "lsh_bandas", "umbral",
                                                 "metodo", "pares_exhaustivo", "perdida", "commit"))
lsh_umbral   <- leer("perdida_lsh_umbral.csv", c("filas", "lsh_bandas", "umbral",
                                                 "pares_exhaustivo", "perdida", "commit"))

# El pie publica el commit de entorno.csv como el de toda la figura: cada CSV
# tiene que venir de esa misma revision, o la figura mezclaria corridas.
local({
  tablas <- list(escala = escala, hilos = hilos, cardinalidad = cardinalidad, bloqueo = bloqueo,
                 perdida_lsh_bandas = lsh_bandas, perdida_lsh_umbral = lsh_umbral)
  commits <- unique(unlist(lapply(tablas, function(d) unique(as.character(d$commit)))))
  if (!all(commits == as.character(entorno$commit[[1L]]))) {
    stop("los CSV mezclan commits: entorno.csv dice ", entorno$commit[[1L]],
         " y los datos traen ", paste(commits, collapse = ", "),
         ". Rehacer la corrida completa sobre una sola revision.")
  }
})
if (any(!is.finite(escala$candidatos) | escala$candidatos <= 0)) {
  stop("escala.csv trae candidatos no positivos: no hay escala logaritmica que los dibuje")
}
if (all(!is.finite(lsh_umbral$perdida)) || all(lsh_umbral$pares_exhaustivo <= 0)) {
  stop("perdida_lsh_umbral.csv no tiene pares aceptados: no hay perdida que dibujar")
}

# ------------------------------------------------------- guardas de contenido
# Las guardas de arriba miran que los CSV existan y hablen del mismo commit.
# Estas miran lo que traen adentro, y salen de una refutacion que le dio de
# comer al graficador CSV rotos de a uno: con un `NA` en `perdida` dibujaba el
# titulo "entre NA % y NA %", con `perdida` 1,5 anunciaba "150 %", con dos filas
# en `bloqueo.csv` usaba la primera y callaba la otra, y con un nivel de
# cardinalidad desconocido dibujaba una fila rotulada "NA". Ninguno de esos
# casos sale de una corrida sana del medidor; todos dibujaban una figura que
# afirmaba algo que el CSV no dice, que es peor que no dibujar nada.
exigir_finito <- function(tabla, nombre, columnas) {
  for (k in columnas) {
    malos <- which(!is.finite(tabla[[k]]))
    if (length(malos)) {
      stop(nombre, " trae ", length(malos), " valor(es) no finito(s) en `", k,
           "` (fila(s) ", paste(malos, collapse = ", "),
           "): la figura diria NA donde tiene que decir una cifra",
           call. = FALSE)
    }
  }
}
exigir_proporcion <- function(tabla, nombre, columnas) {
  for (k in columnas) {
    malos <- which(tabla[[k]] < 0 | tabla[[k]] > 1)
    if (length(malos)) {
      stop(nombre, " trae `", k, "` fuera de [0, 1] en la(s) fila(s) ",
           paste(malos, collapse = ", "), ": ",
           paste(tabla[[k]][malos], collapse = ", "),
           ". Es una proporcion; con 1,5 la figura anunciaria 150 %",
           call. = FALSE)
    }
  }
}
exigir_una_fila <- function(tabla, nombre) {
  if (nrow(tabla) != 1L) {
    stop(nombre, " tiene ", nrow(tabla),
         " filas y la figura dibuja una sola: las demas quedarian sin dibujar",
         " y sin decirlo", call. = FALSE)
  }
}
exigir_una_fila(entorno, "entorno.csv")
exigir_una_fila(bloqueo, "bloqueo.csv")
exigir_finito(escala, "escala.csv",
              c("filas", "candidatos", "error_estimacion", "recall_techo",
                "tiempo_s_1", "tiempo_s_mediana"))
exigir_finito(hilos, "hilos.csv", c("nucleos", "tiempo_s_mediana", "candidatos",
                                    "pares_informados"))
exigir_finito(cardinalidad, "cardinalidad.csv",
              c("candidatos", "candidatos_por_fila", "veces_respecto_alta"))
exigir_finito(bloqueo, "bloqueo.csv",
              c("pares_sin_bloqueo", "pares_con_bloqueo", "perdidos_reales",
                "perdidos_estimados", "error_estimacion"))
exigir_finito(lsh_bandas, "perdida_lsh_bandas.csv", c("lsh_bandas", "perdida"))
exigir_finito(lsh_umbral, "perdida_lsh_umbral.csv",
              c("umbral", "perdida", "pares_exhaustivo"))
exigir_proporcion(escala, "escala.csv", "recall_techo")
exigir_proporcion(lsh_bandas, "perdida_lsh_bandas.csv", "perdida")
exigir_proporcion(lsh_umbral, "perdida_lsh_umbral.csv", "perdida")
# El panel de hilos titula con el tamano medido: si las filas no son todas del
# mismo tamano, ese titulo describe a una sola de ellas.
if (length(unique(hilos$filas)) != 1L) {
  stop("hilos.csv mezcla tamanos (",
       paste(format(unique(hilos$filas), scientific = FALSE), collapse = ", "),
       ") y el panel titula con uno solo: medir cada tamano por separado",
       call. = FALSE)
}
if (anyDuplicated(hilos$nucleos)) {
  stop("hilos.csv repite configuraciones de nucleos: ",
       paste(hilos$nucleos[duplicated(hilos$nucleos)], collapse = ", "),
       call. = FALSE)
}
# La descripcion de cada nivel se escribe aca y no se toma del CSV -que la trae
# sin acentos, para que el medidor no dependa de la codificacion-, asi que el
# riesgo es que el CSV cambie y la figura siga diciendo lo de antes. Por eso el
# guion se detiene ante un nivel que no conozca, en vez de dibujar "NA".
NIVELES_CARDINALIDAD <- c("baja", "media", "alta")
if (!all(cardinalidad$nivel %in% NIVELES_CARDINALIDAD)) {
  stop("cardinalidad.csv trae niveles que esta figura no sabe describir: ",
       paste(setdiff(cardinalidad$nivel, NIVELES_CARDINALIDAD), collapse = ", "),
       ". Agregar su descripcion antes de dibujarlos", call. = FALSE)
}

# ------------------------------------------------------------- sistema visual
# Paleta de Okabe-Ito, legible con vision deficiente del color, sobre tinta
# gris oscura: nada de negro puro ni de rejillas fuertes.
TINTA      <- "#1f1f1f"
GRIS       <- "#6b6b6b"
GRIS_CLARO <- "#b9b9b9"
REJILLA    <- "#e7e7e7"
AZUL       <- "#0072B2"
NARANJA    <- "#E69F00"
VERDE      <- "#009E73"
BERMELLON  <- "#D55E00"
CELESTE    <- "#56B4E9"

elegir_fuente <- function(candidatas = c("Fira Sans", "Source Sans 3", "Noto Sans",
                                         "DejaVu Sans")) {
  if (!nzchar(Sys.which("fc-list"))) return("sans")
  instaladas <- tryCatch(system2("fc-list", c(":", "family"), stdout = TRUE),
                         error = function(e) character())
  for (f in candidatas) if (any(grepl(f, instaladas, fixed = TRUE))) return(f)
  "sans"
}
CAIRO  <- isTRUE(capabilities("cairo"))
FUENTE <- if (CAIRO) elegir_fuente() else "sans"

# ---------------------------------------------------------- formato de cifras
# Convencion del castellano rioplatense: punto de miles, coma decimal.
num <- function(x, dec = 0L) {
  if (length(dec) == 1L) {
    return(formatC(x, format = "f", digits = dec, big.mark = ".", decimal.mark = ","))
  }
  mapply(function(v, d) formatC(v, format = "f", digits = d, big.mark = ".",
                                decimal.mark = ","), x, dec, USE.NAMES = FALSE)
}
# "1,0 min" -> "1 min"; "0,50 M" -> "0,5 M". Quita los decimales que no
# aportan: un cero final se lee como precision que la cifra no tiene. El
# corchete de la busqueda acepta los dos espacios, el comun y el duro.
limpio <- function(s) {
  s <- sub("(,[0-9]*?)0+(?=([  ]|$))", "\\1", s, perl = TRUE)
  sub(",(?=([  ]|$))", "", s, perl = TRUE)
}
# Espacio duro entre la cifra y su unidad. envolver() corta en el espacio
# comun: "140 M" repartido entre dos renglones deja de leerse como una cifra,
# y "4 mil" partido no se lee de ninguna manera.
UNIDADES <- c("M", "mil", "MiB", "GiB", "s", "min", "h", "filas", "hilos",
              "bandas", "pares", "semillas", "corridas", "candidatos")
duro <- function(s) {
  s <- gsub(sprintf("([0-9]) (%s)(?![[:alnum:]])", paste(UNIDADES, collapse = "|")),
            "\\1\u00a0\\2", s, perl = TRUE)
  gsub("([0-9]) (%|\u00d7)", "\\1\u00a0\\2", s, perl = TRUE)
}
# Una sola magnitud para toda una serie de cifras (los rotulos de un eje, o
# los de una familia de puntos hermanos): mezclar "mil" y "M" en la misma
# serie obliga a comparar unidades en vez de cifras. Se toma la mayor magnitud
# en la que la cifra mas chica todavia se lee, y cada cifra lleva dos digitos
# significativos.
serie_compacta <- function(x) {
  finitos <- abs(x[is.finite(x) & x != 0])
  escalas <- c(1e6, 1e3, 1)
  sufijos <- c(" M", " mil", "")
  i <- length(escalas)
  for (j in seq_along(escalas)) {
    if (length(finitos) && max(finitos) >= escalas[[j]] &&
        min(finitos) >= escalas[[j]] / 10) {
      i <- j
      break
    }
  }
  vapply(x / escalas[[i]], function(u) {
    if (!is.finite(u)) return("")
    if (u == 0) return("0")
    dec <- max(0L, 2L - (floor(log10(abs(u))) + 1L))
    limpio(paste0(num(u, dec), sufijos[[i]]))
  }, character(1L))
}
# "1 pares" delata que el rotulo se armo pegando una palabra fija a un numero.
# Con los datos de esta corrida nunca pasa -el minimo son 2-, pero el rotulo se
# arma igual desde el CSV y un CSV puede traer 1.
pares <- function(n) sprintf("%s par%s", num(n), ifelse(n == 1, "", "es"))

# Un solo formato para el signo de multiplicacion en las cuatro figuras: la
# cifra primero, un decimal y el signo pegado detras. "referencia" dice lo
# mismo con otra convencion y obliga a leer dos.
veces_rotulo <- function(x) paste0(num(x, 1L), "\u00d7")
# 6,2 M / 140 M / 3.640 M / 411 mil / 1 mil / 21
compacto <- function(x) {
  vapply(x, function(v) {
    if (v == 0) return("0")
    limpio(if (v >= 1e7) paste0(num(v / 1e6), " M")
           else if (v >= 1e6) paste0(num(v / 1e6, 1L), " M")
           else if (v >= 1e4) paste0(num(v / 1e3), " mil")
           else if (v >= 1e3) paste0(num(v / 1e3, 1L), " mil")
           else if (v >= 100) num(v)
           else num(v, 1L))
  }, character(1L))
}
tiempo <- function(s, eje = FALSE) {
  r <- vapply(s, function(v) {
    if (v < 60) paste0(num(v, 1L), " s")
    else if (v < 3600) paste0(num(v / 60, 1L), " min")
    else paste0(num(v / 3600, 1L), " h")
  }, character(1L))
  if (eje) limpio(r) else r
}
memoria <- function(mib, eje = FALSE) {
  r <- vapply(mib, function(v) if (v < 1024) paste0(num(v), " MiB")
              else paste0(num(v / 1024, 1L), " GiB"), character(1L))
  if (eje) limpio(r) else r
}
porcentaje <- function(p, dec = 2L, signo = TRUE) {
  s <- num(abs(100 * p), dec)
  s <- paste0(if (signo) ifelse(p < 0, "−", "+") else "", s, " %")
  # el cero no tiene signo ni decimales que digan algo: "+0,0 %" es ruido en
  # una marca de eje que solo esta para decir donde cae el acierto exacto
  ifelse(p == 0, "0 %", s)
}
# La clave se nombra siempre igual: "anio" es el nombre de la columna, no una
# palabra, y sin la traduccion se lee como una errata.
clave_rotulo <- function(clave) {
  glosa <- c(anio = "año")
  if (clave %in% names(glosa)) sprintf("clave %s (%s)", clave, glosa[[clave]]) else
    sprintf("clave %s", clave)
}
filas_rotulo <- function(n) ifelse(n >= 1e6, paste0(num(n / 1e6), " M"),
                                   paste0(num(n / 1e3), " mil"))
nombre_metodo <- function(m) {
  c(jw = "Jaro-Winkler", osa = "distancia OSA", lv = "Levenshtein", dl = "Damerau-Levenshtein",
    cosine = "coseno de q-gramas", jaccard = "Jaccard de q-gramas", qgram = "q-gramas")[m]
}
# "Jaro-Winkler (p = 0,1) con umbral 0,10": la medida final tal como se midio,
# leida de entorno.csv y no escrita aca.
medida_final <- function() {
  e <- entorno[1L, ]
  peso <- if (identical(e$metodo, "jw") && is.finite(e$p_jw) && e$p_jw > 0) {
    sprintf(" (p = %s)", num(e$p_jw, 1L))
  } else ""
  sprintf("%s%s con umbral %s", nombre_metodo(e$metodo), peso, num(e$umbral, 2L))
}

potencias <- function(lim) 10^seq(floor(log10(lim[[1L]])), ceiling(log10(lim[[2L]])))

# Parte un texto en lineas que entren en `ancho_in` pulgadas, midiendo con la
# fuente y el tamano con que se va a dibujar. Necesita un dispositivo abierto.
envolver <- function(texto, ancho_in, cex = 1, font = 1L) {
  # Un 5 % de margen: medido contra el borde exacto, el ultimo renglon queda
  # muchas veces con una sola palabra colgando.
  ancho_in <- ancho_in * 0.95
  # duro() pega cada cifra a su unidad antes de cortar en los espacios.
  palabras <- strsplit(duro(texto), " ", fixed = TRUE)[[1L]]
  lineas <- character()
  actual <- ""
  for (p in palabras) {
    prueba <- if (nzchar(actual)) paste(actual, p) else p
    ancho <- graphics::strwidth(prueba, units = "inches", cex = cex, font = font)
    if (ancho > ancho_in && nzchar(actual)) {
      lineas <- c(lineas, actual)
      actual <- p
    } else {
      actual <- prueba
    }
  }
  c(lineas, actual)
}
ancho_figura <- function() sum(graphics::par("din")[[1L]], -graphics::par("omi")[c(2L, 4L)])
ancho_panel  <- function() graphics::par("pin")[[1L]] + graphics::par("mai")[[4L]]

# ------------------------------------------------------------------ lienzo
# Margenes exteriores fijos para las cuatro figuras: arriba titulo y subtitulo
# (hasta dos lineas), abajo el pie de dos lineas. Margenes de panel con lugar
# para titulo y dos lineas de subtitulo.
OMA <- c(3.3, 0.6, 5.0, 0.8)
MAR <- c(3.6, 4.8, 4.2, 1.5)

# `oma3` corre el borde de arriba: un titulo de dos lineas necesita un renglon
# mas, y una figura de tres filas no puede gastar el encabezado de una de
# cuatro paneles.
abrir <- function(nombre, ancho, alto, oma3 = OMA[[3L]]) {
  # cairo dibuja la fuente elegida y el antialias; sin cairo, el tipo por
  # omision de la plataforma y la fuente "sans".
  tipo <- if (CAIRO) "cairo" else getOption("bitmapType")
  grDevices::png(file.path(dir_salida, nombre), width = ancho, height = alto,
                 units = "in", res = 200, type = tipo, bg = "white",
                 pointsize = 13)
  graphics::par(family = FUENTE, las = 1, xaxs = "r", yaxs = "r", lend = 1,
                ljoin = 1, col = TINTA, col.axis = GRIS, col.lab = GRIS,
                cex.axis = 0.82, mgp = c(2.2, 0.45, 0), tcl = 0,
                oma = c(OMA[[1L]], OMA[[2L]], oma3, OMA[[4L]]), mar = MAR)
}
# layout() achica par("cex") con dos o mas filas o columnas, pero mtext() lo
# ignora: se repone a 1 para que texto, ejes y titulos midan lo mismo en las
# cuatro figuras y para que envolver() mida lo que se dibuja.
disponer <- function(mat, widths = rep.int(1, ncol(mat))) {
  graphics::layout(mat, widths = widths)
  graphics::par(cex = 1)
}

# Titulo y subtitulo en el margen exterior superior, anclados al borde de la
# figura: el titulo queda siempre a la misma altura y el subtitulo, envuelto
# al ancho de la figura, cuelga debajo con una o dos lineas.
encabezado <- function(titulo, subtitulo) {
  # Un titulo con cifras medidas no siempre entra en un renglon; se envuelve y
  # el subtitulo baja con el, en vez de achicar la letra del titulo.
  lineas_t <- envolver(titulo, ancho_figura() - 0.15, cex = 1.25, font = 2L)
  linea_titulo <- graphics::par("oma")[[3L]] - 1.55 - 1.5 * (length(lineas_t) - 1L)
  for (i in seq_along(lineas_t)) {
    graphics::mtext(lineas_t[[i]], side = 3, outer = TRUE, adj = 0,
                    line = linea_titulo + 1.5 * (length(lineas_t) - i),
                    cex = 1.25, font = 2, col = TINTA)
  }
  lineas <- envolver(subtitulo, ancho_figura() - 0.15, cex = 0.9)
  for (i in seq_along(lineas)) {
    graphics::mtext(lineas[[i]], side = 3, outer = TRUE, adj = 0,
                    line = linea_titulo - 1.5 - 1.1 * (i - 1L), cex = 0.9, col = GRIS)
  }
}
# El pie nombra el guion que midio esa figura: tres salen de medir_figuras.R y
# la del tamiz de perdida_lsh.R, cuyos conteos no dependen de los hilos.
pie <- function(guion = "benchmark/medir_figuras.R", con_hilos = TRUE) {
  e <- entorno[1L, ]
  cpu <- if (is.null(e$cpu) || is.na(e$cpu) || !nzchar(e$cpu)) "" else
    gsub("\\(R\\)|\\(TM\\)|CPU|@.*$", "", e$cpu)
  commit <- if (is.na(e$commit) || !nzchar(e$commit)) "sin commit" else e$commit
  # La figura del tamiz cuenta pares, no tiempos: los hilos no cambian nada.
  # Se dice, en el mismo lugar donde las otras declaran cuantos usaron; el
  # hueco haria pensar en un dato que falta.
  hilos <- if (con_hilos) sprintf(", %s hilos de stringdist", e$hilos_stringdist) else
    ", hilos: no aplica"
  linea1 <- sprintf("Medido sobre lupa %s (%s), %s, %s%s · %s",
                    e$version_lupa, commit, substr(e$fecha, 1L, 10L),
                    sub("^R version ", "R ", e$r_version), hilos, cpu)
  linea1 <- sub("\\s*·\\s*$", "", gsub("\\s+", " ", trimws(linea1)))
  graphics::mtext(linea1, side = 1, outer = TRUE, adj = 0, line = 1.0,
                  cex = 0.8, col = GRIS)
  graphics::mtext(sprintf("Reproducible: %s → benchmark/datos/ → benchmark/graficar_figuras.R", guion),
                  side = 1, outer = TRUE, adj = 0, line = 2.1, cex = 0.8, col = GRIS)
}

# Ejes sin marcas: los rotulos alcanzan. axis() calla sin aviso los rotulos
# que se solapan mas de gap.axis emes; con un valor muy negativo los dibuja
# todos, y un solapamiento se ve en la revision en vez de esconderse. Un
# rotulo de dos lineas (con "\n") se apoya en la linea de abajo, asi que se
# corre una linea por cada salto.
eje_x <- function(at, labels, line = -0.55) {
  extra <- max(lengths(regmatches(labels, gregexpr("\n", labels, fixed = TRUE))))
  graphics::axis(1, at = at, labels = labels, tick = FALSE, line = line + extra,
                 gap.axis = -1e3)
}
eje_y <- function(at, labels) {
  graphics::axis(2, at = at, labels = labels, tick = FALSE, line = -0.35, gap.axis = -1e3)
}
unidad_x <- function(texto, line = 1.05) {
  graphics::mtext(texto, side = 1, line = line, adj = 1, cex = 0.72, col = GRIS)
}
# Rotulo del eje y, horizontal y apoyado en el rincon superior izquierdo del
# panel: girado no entra en paneles de poca altura, y en el margen izquierdo
# no queda lugar despues de los rotulos del eje. Ese rincon esta vacio en
# todas las curvas crecientes.
unidad_y <- function(texto) {
  usr <- graphics::par("usr")
  borde <- if (graphics::par("xlog")) 10^usr[[1L]] else usr[[1L]]
  # un pelo adentro del borde: pegado al eje se confunde con el rotulo de la
  # marca de arriba, que termina justo ahi
  x <- graphics::grconvertX(graphics::grconvertX(borde, "user", "inches") + 0.10,
                            "inches", "user")
  y <- if (graphics::par("ylog")) 10^usr[[4L]] else usr[[4L]]
  graphics::text(x, y, texto, adj = c(0, 1.15), col = GRIS, cex = 0.72, xpd = NA)
}
# El titulo del panel se envuelve como el subtitulo: cuando lleva la lectura
# de la figura no entra en un renglon. La ultima linea queda siempre a la
# misma altura y las anteriores se apilan hacia arriba.
titular <- function(titulo, subtitulo = NULL) {
  if (!is.null(titulo)) {
    lineas <- envolver(titulo, ancho_panel(), cex = 0.98, font = 2L)
    for (i in seq_along(lineas)) {
      graphics::mtext(lineas[[i]], side = 3, adj = 0,
                      line = 2.6 + 1.15 * (length(lineas) - i),
                      cex = 0.98, font = 2, col = TINTA)
    }
  }
  if (!is.null(subtitulo)) {
    lineas <- envolver(subtitulo, ancho_panel(), cex = 0.78)
    for (i in seq_along(lineas)) {
      graphics::mtext(lineas[[i]], side = 3, adj = 0, line = 1.5 - 1.05 * (i - 1L),
                      cex = 0.78, col = GRIS)
    }
  }
}

# Un panel sin caja, con rejilla tenue, titulo y subtitulo arriba a la
# izquierda.
panel <- function(xlim, ylim, log = "", titulo = NULL, subtitulo = NULL,
                  x_at = NULL, x_lab = NULL, y_at = NULL, y_lab = NULL,
                  rejilla = "h") {
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim, log = log)
  if (is.null(y_at)) y_at <- if (grepl("y", log)) potencias(ylim) else pretty(ylim)
  if (is.null(y_lab)) y_lab <- if (grepl("y", log)) compacto(y_at) else num(y_at)
  dentro <- y_at >= ylim[[1L]] & y_at <= ylim[[2L]]
  y_at <- y_at[dentro]
  y_lab <- y_lab[dentro]
  if (is.null(x_at)) x_at <- if (grepl("x", log)) potencias(xlim) else pretty(xlim)
  if (is.null(x_lab)) x_lab <- if (grepl("x", log)) compacto(x_at) else num(x_at)
  dentro <- x_at >= xlim[[1L]] & x_at <= xlim[[2L]]
  x_at <- x_at[dentro]
  x_lab <- x_lab[dentro]
  if (grepl("h", rejilla)) graphics::abline(h = y_at, col = REJILLA, lwd = 0.9)
  if (grepl("v", rejilla)) graphics::abline(v = x_at, col = REJILLA, lwd = 0.9)
  eje_y(y_at, y_lab)
  eje_x(x_at, x_lab)
  graphics::abline(h = if (grepl("y", log)) ylim[[1L]] else min(0, ylim[[1L]]),
                   col = GRIS_CLARO, lwd = 0.9)
  titular(titulo, subtitulo)
}

punto <- function(x, y, color = AZUL, cex = 1.15) {
  graphics::points(x, y, pch = 21, bg = color, col = "white", lwd = 1.2, cex = cex)
}
rotulo <- function(x, y, texto, pos = 4, color = TINTA, cex = 0.8, ...) {
  graphics::text(x, y, texto, pos = pos, col = color, cex = cex, xpd = NA, ...)
}
# Los rotulos de una serie van arriba del punto, salvo el que se pisaria con
# el del punto siguiente: ese baja. Se mide el solape real en pulgadas, con la
# fuente y el tamano con que se dibuja, y grconvert() se encarga de los ejes
# logaritmicos. El margen extra evita los rotulos que apenas se rozan.
posicion_rotulo <- function(x, y, textos, cex = 0.8, margen_in = 0.10) {
  pos <- rep.int(3L, length(x))
  if (length(x) < 2L) return(pos)
  xi <- graphics::grconvertX(x, "user", "inches")
  yi <- graphics::grconvertY(y, "user", "inches")
  ancho <- graphics::strwidth(textos, units = "inches", cex = cex)
  alto <- graphics::strheight("X", units = "inches", cex = cex) * 2
  for (i in seq_len(length(x) - 1L)) {
    if (pos[[i]] != 3L) next
    if (abs(xi[[i + 1L]] - xi[[i]]) < (ancho[[i]] + ancho[[i + 1L]]) / 2 + margen_in &&
        abs(yi[[i + 1L]] - yi[[i]]) < alto) {
      pos[[i]] <- 1L
    }
  }
  pos
}
nota <- function(x, y, texto, color = GRIS, cex = 0.76, adj = c(0, 0.5)) {
  graphics::text(x, y, texto, adj = adj, col = color, cex = cex, xpd = NA)
}
# Nota de lectura debajo del eje x: dentro del area de datos cruzaria las
# lineas de referencia y se leeria como si fuera un dato mas.
nota_bajo_eje <- function(texto, desde = 3.4, cex = 0.74) {
  lineas <- envolver(texto, ancho_panel(), cex = cex)
  for (i in seq_along(lineas)) {
    graphics::mtext(lineas[[i]], side = 1, adj = 0, line = desde + 1.05 * (i - 1L),
                    cex = cex, col = GRIS)
  }
}
# Una nota envuelta y anclada en un rincon del panel: `adj_x` 0 pega a la
# izquierda y 1 a la derecha; `desde` "arriba" cuelga desde y hacia abajo,
# "abajo" apila desde y hacia arriba. En un eje y logaritmico las lineas se
# separan multiplicando, para que el paso se vea igual.
nota_bloque <- function(x, y, texto, color, ancho_in, cex = 0.76, paso = 1.45,
                        adj_x = 0, desde = "arriba", log_y = FALSE) {
  lineas <- envolver(texto, ancho_in, cex = cex)
  k <- length(lineas)
  alto_in <- graphics::strheight("X", units = "inches", cex = cex) * paso
  usr <- graphics::par("usr")
  # altura de una linea en unidades del eje y (decadas si es logaritmico)
  alto <- alto_in / graphics::par("pin")[[2L]] * (usr[[4L]] - usr[[3L]])
  for (i in seq_len(k)) {
    pasos <- if (desde == "arriba") -(i - 1L) else (k - i)
    y_i <- if (log_y) 10^(log10(y) + pasos * alto) else y + pasos * alto
    nota(x, y_i, lineas[[i]], color = color, cex = cex,
         adj = c(adj_x, if (desde == "arriba") 1 else 0))
  }
}
# Limites de un eje logaritmico con aire a ambos lados del dato.
lim_log <- function(x, abajo = 1.6, arriba = 2.2) c(min(x) / abajo, max(x) * arriba)
# Cuantas corridas de tiempo guarda una tabla: las columnas tiempo_s_1, _2, ...
corridas_de <- function(d) sum(grepl("^tiempo_s_[0-9]+$", names(d)))
palabra_corridas <- function(k) {
  palabras <- c("una", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  if (k >= 1L && k <= length(palabras)) palabras[[k]] else num(k)
}
# El rango de las corridas de cada configuracion, en un segmento fino y gris
# detras del punto de la mediana: dibujadas como puntos sueltos, corridas que
# difieren en unidades por ciento se apilan encima de la mediana y no se ven.
rango_corridas <- function(x, m) {
  graphics::segments(x, apply(m, 1L, min), x, apply(m, 1L, max),
                     col = GRIS_CLARO, lwd = 1.6)
}
# Cuanto se separan entre si las corridas de una misma configuracion, en por
# ciento de su mediana y hacia arriba: es la cifra que promete el subtitulo,
# y sale de las columnas medidas, no de una redaccion.
dispersion_corridas <- function(m, mediana) {
  d <- (apply(m, 1L, max) - apply(m, 1L, min)) / mediana
  ceiling(max(d[is.finite(d)]) * 100)
}

# ========================================================= 1. escala
escala <- escala[order(escala$filas), ]
n <- escala$filas
xlim_n <- c(min(n) / 1.6, max(n) * 1.9)
x_lab_n <- num(n / 1e3)

# El titulo de la figura lleva dos lineas de cifras medidas: el borde de
# arriba necesita ese renglon de mas.
abrir("banco-escala.png", 10, 7.4, oma3 = 6.6)
disponer(matrix(1:4, 2, byrow = TRUE))

# Cada eje logaritmico lo dice en su propio rotulo: repetir "escala
# logaritmica" cuatro veces gasta el renglon que necesita la unidad.
FILAS_LOG <- "miles de filas (log)"

# Referencia cuadratica anclada en el primer punto: dice si la curva medida
# crece como n² o menos. Con un solo tamano no hay nada que comparar.
referencia_n2 <- function(x, y1) {
  if (length(x) < 2L) return(invisible(NULL))
  graphics::lines(xlim_n, y1 * (xlim_n / x[[1L]])^2, col = GRIS_CLARO,
                  lty = "22", lwd = 1.1)
  # Apoyado sobre la punteada y terminando entre los dos primeros tamanos:
  # ahi la referencia todavia corre por debajo de los rotulos de los puntos,
  # que a la derecha ya estan altos.
  x_ref <- sqrt(as.numeric(x[[1L]]) * x[[2L]])
  nota(x_ref, y1 * (x_ref / x[[1L]])^2, "crece como n²", color = TINTA,
       cex = 0.8, adj = c(1, -0.35))
}

# (a) candidatos
cand <- escala$candidatos
ylim_c <- lim_log(cand, 2.4, 2.2)
# ninguna marca por encima del dato: una decada vacia arriba obliga a leer
# "10.000 M" para nada, y rompe la convencion de magnitud del eje
y_at_c <- potencias(ylim_c)
y_at_c <- y_at_c[y_at_c <= max(cand)]
panel(xlim_n, ylim_c, log = "xy", titulo = "Comparaciones candidatas",
      subtitulo = "pares que el tamiz propone y mide el método final; recall contra el techo: lo que acepta de lo sembrado",
      x_at = n, x_lab = x_lab_n, y_at = y_at_c, y_lab = serie_compacta(y_at_c))
unidad_y("Candidatos (log)")
referencia_n2(n, cand[[1L]])
graphics::lines(n, cand, col = AZUL, lwd = 2)
punto(n, cand)
rotulo(n, cand, serie_compacta(cand), pos = 3, cex = 0.8)
recall <- escala$recall_techo
# El recall va en por ciento: "1,0000" obliga a traducirlo. El denominador no
# son los duplicados sembrados sino el techo —los sembrados que la medida
# final acepta—, y el CSV no trae ese conteo: por eso el rotulo dice la
# proporcion y el subtitulo dice de que.
texto_recall <- if (all(is.finite(recall) & recall == 1)) {
  sprintf("recall %s en todos los tamaños", porcentaje(1, 0L, signo = FALSE))
} else {
  sprintf("recall %s", paste(porcentaje(recall, 1L, signo = FALSE), collapse = ", "))
}
# abajo a la derecha, donde la curva creciente deja lugar
nota_bloque(xlim_n[[2L]], ylim_c[[1L]] * 1.5, texto_recall, TINTA,
            ancho_in = graphics::par("pin")[[1L]] * 0.8, adj_x = 1, desde = "abajo",
            log_y = TRUE)
unidad_x(FILAS_LOG)

# (b) tiempo
k_corridas <- corridas_de(escala)
t_med <- escala$tiempo_s_mediana
t_all <- as.matrix(escala[, sprintf("tiempo_s_%d", seq_len(k_corridas))])
# aire arriba para que la punteada de referencia salga por el borde y no
# cruce el rotulo del ultimo punto
ylim_t <- lim_log(t_all, 2, 4.2)
y_at_t <- c(1, 10, 60, 600, 3600, 36000)
panel(xlim_n, ylim_t, log = "xy", titulo = "Tiempo de reloj",
      subtitulo = sprintf("mediana de %s corridas en procesos separados; el segmento es su rango (difieren en menos de %s %%)",
                          palabra_corridas(k_corridas),
                          num(dispersion_corridas(t_all, t_med))),
      x_at = n, x_lab = x_lab_n, y_at = y_at_t, y_lab = tiempo(y_at_t, eje = TRUE))
unidad_y("Tiempo (log)")
# la misma punteada que el panel de candidatos: el tiempo sigue a las
# comparaciones, y se ve que crece un poco menos que n²
referencia_n2(n, t_med[[1L]])
rango_corridas(n, t_all)
graphics::lines(n, t_med, col = AZUL, lwd = 2)
punto(n, t_med)
rotulo(n, t_med, tiempo(t_med), pos = 3, cex = 0.8)
unidad_x(FILAS_LOG)

# (c) memoria. Sin /proc (macOS, Windows) el medidor deja rss_mib en NA: el
# panel lo dice en vez de inventar un cero.
rss <- escala$rss_mib
if (all(is.na(rss))) {
  panel(xlim_n, c(1, 10), log = "x", titulo = "Memoria pico del proceso",
        subtitulo = "RSS máximo (VmHWM) de una corrida", x_at = n, x_lab = x_lab_n,
        y_at = numeric(), y_lab = character(), rejilla = "")
  nota(10^mean(log10(xlim_n)), 5.5,
       "sin medición: esta corrida no leyó /proc/self/status", adj = c(0.5, 0.5))
} else {
  ylim_m <- lim_log(rss, 1.8, 2.6)
  y_at_m <- 2^(6:15)
  panel(xlim_n, ylim_m, log = "xy", titulo = "Memoria pico del proceso",
        subtitulo = "RSS máximo (VmHWM) de una corrida", x_at = n, x_lab = x_lab_n,
        y_at = y_at_m, y_lab = memoria(y_at_m, eje = TRUE))
  unidad_y("Memoria (log)")
  graphics::lines(n, rss, col = AZUL, lwd = 2)
  punto(n, rss)
  # dos tamanos seguidos gastan casi la misma memoria: el rotulo del primero
  # baja para no pegarse al del segundo
  rotulo(n, rss, memoria(rss), pos = posicion_rotulo(n, rss, memoria(rss)), cex = 0.8)
}
unidad_x(FILAS_LOG)

# (d) hilos
hilos <- hilos[order(hilos$nucleos), ]
k_hilos <- corridas_de(hilos)
h_all <- as.matrix(hilos[, sprintf("tiempo_s_%d", seq_len(k_hilos))])
h_med <- hilos$tiempo_s_mediana
xlim_h <- c(min(hilos$nucleos) / 1.35, max(hilos$nucleos) * 1.5)
# aire arriba para el renglon de la nota: si la nota baja, se apoya sobre los
# rotulos de los puntos que compara
ylim_h <- c(0, max(h_all) * 1.58)
# hilos.csv guarda candidatos y pares de la corrida registrada de cada
# configuracion; el medidor se detiene si las corridas de una misma
# configuracion no coinciden, asi que el dato vale por las tres
mismo_resultado <- length(unique(hilos$candidatos)) == 1L &&
  length(unique(hilos$pares_informados)) == 1L
panel(xlim_h, ylim_h, log = "x",
      titulo = sprintf("Hilos de stringdist, %s filas", filas_rotulo(hilos$filas[[1L]])),
      subtitulo = sprintf("mediana de %s corridas por configuración; el segmento es su rango (difieren en menos de %s %%)",
                          palabra_corridas(k_hilos),
                          num(dispersion_corridas(h_all, h_med))),
      x_at = hilos$nucleos, x_lab = num(hilos$nucleos), y_at = pretty(ylim_h),
      y_lab = paste0(num(pretty(ylim_h)), " s"))
rango_corridas(hilos$nucleos, h_all)
graphics::lines(hilos$nucleos, h_med, col = AZUL, lwd = 2)
punto(hilos$nucleos, h_med)
rotulo(hilos$nucleos, h_med, paste0(num(h_med), " s"), pos = 3, cex = 0.8)
i16 <- which(hilos$nucleos == 16L)
i2 <- which(hilos$nucleos == 2L)
if (length(i2) && length(i16)) {
  # pegada al borde de arriba y a la derecha: mas abajo se apoya sobre los
  # rotulos de los ultimos puntos, que son los que la nota compara
  nota_bloque(xlim_h[[2L]], ylim_h[[2L]],
              sprintf("2 hilos, el valor por omisión, tardan %s lo que 16",
                      veces_rotulo(h_med[[i2]] / h_med[[i16]])),
              TINTA, ancho_in = graphics::par("pin")[[1L]] * 0.98, cex = 0.74, adj_x = 1)
}
unidad_x("hilos (log)")
# el resultado no depende de los hilos: se dice debajo del eje, que es donde
# hay lugar, y no encima de la curva
if (mismo_resultado && nrow(hilos) > 1L) {
  graphics::mtext(duro(sprintf("las %s configuraciones dan los mismos %s candidatos y %s pares",
                               num(nrow(hilos)), serie_compacta(hilos$candidatos[[1L]]),
                               num(hilos$pares_informados[[1L]]))),
                  side = 1, line = 2.15, adj = 1, cex = 0.72, col = GRIS)
}

# El titulo dice cuanto creció cada cosa entre el primer tamano medido y el
# ultimo: las tres razones salen de escala.csv, no de la redaccion. Con un
# solo tamano no hay crecimiento que contar y el titulo lo dice de otro modo.
titulo_escala <- if (length(n) >= 2L) {
  crecio <- function(x) veces_rotulo(x[[length(x)]] / x[[1L]])
  sprintf("De %s a %s filas (%s), los candidatos crecen %s, el tiempo %s%s",
          filas_rotulo(n[[1L]]), filas_rotulo(n[[length(n)]]), crecio(n),
          crecio(cand), crecio(t_med),
          if (all(is.na(rss))) "" else sprintf(" y la memoria %s", crecio(rss)))
} else {
  "El costo crece con las comparaciones, no con las filas"
}
encabezado(titulo_escala,
           sprintf("Padrón sintético con homónimos y erratas sembradas, estrategia LSH, %s, todos los pares informados. Ejes logarítmicos, salvo el del tiempo por hilos.",
                   medida_final()))
pie()
invisible(grDevices::dev.off())

# ==================================================== 2. estimacion previa
abrir("banco-estimacion.png", 10, 5.2)
disponer(matrix(1:2, 1), widths = c(1.12, 1))
# El mismo margen en los dos paneles: el titulo de cada uno arranca donde
# arranca su area de datos, y los dos arrancan a la misma altura. Arriba, un
# renglon de mas: el titulo del panel derecho lleva la lectura y ocupa dos.
graphics::par(mar = MAR + c(0, 0, 1.2, 0))

# (a) error de la estimacion: candidatos LSH por tamano, y pares perdidos por
# el bloqueo
err <- escala$error_estimacion
b <- bloqueo[1L, ]
# sin pares perdidos no hay error relativo que medir: el medidor deja NA (o
# NaN en versiones anteriores) y el panel lo dice en vez de caerse
err_b <- if (is.finite(b$error_estimacion) && b$perdidos_reales > 0) b$error_estimacion else NA_real_
xs <- seq_along(n)
xb <- length(xs) + 1
errores <- abs(c(err, err_b))
errores <- errores[is.finite(errores)]
tope <- max(0.03, ceiling(max(errores) * 100 * 1.5) / 100)
xlim_e <- c(0.45, xb + 0.55)
y_at_e <- pretty(c(-tope, tope))
# Sin banda de tolerancia: era una linea editorial, no una medida, y el ojo
# la lee como si el banco hubiera fijado un limite. Quedan el cero y las
# marcas del eje, que alcanzan para ver cuanto se aparta cada punto.
panel(xlim_e, c(-tope, tope), titulo = "Error de la estimación",
      subtitulo = "previsto respecto de lo real; el cero es el acierto exacto",
      x_at = c(xs, xb), x_lab = c(filas_rotulo(n), "bloqueo"),
      y_at = y_at_e, y_lab = porcentaje(y_at_e, 1L))
graphics::abline(h = 0, col = GRIS_CLARO, lwd = 1)
graphics::abline(v = xb - 0.5, col = GRIS_CLARO, lty = "12", lwd = 0.9)
graphics::segments(xs, 0, xs, err, col = AZUL, lwd = 2.2)
punto(xs, err)
rotulo(xs, err, porcentaje(err), pos = ifelse(err < 0, 1, 3), cex = 0.8)
if (is.finite(err_b)) {
  graphics::segments(xb, 0, xb, err_b, col = NARANJA, lwd = 2.2)
  punto(xb, err_b, color = NARANJA)
  rotulo(xb, err_b, porcentaje(err_b), pos = ifelse(err_b < 0, 1, 3), cex = 0.8)
} else {
  rotulo(xb, 0, "sin pérdida", pos = 3, color = TINTA, cex = 0.8)
}
# Los dos rotulos van en tinta: el ambar de los puntos no tiene contraste
# suficiente como texto sobre blanco, y el eje ya separa los dos grupos.
graphics::mtext("candidatos del tamiz LSH, según las filas", side = 1, at = xs[[1L]] - 0.45,
                adj = 0, line = 1.05, cex = 0.72, col = TINTA)
graphics::mtext("pares perdidos", side = 1, at = xb, adj = 0.5, line = 1.05,
                cex = 0.72, col = TINTA)
graphics::mtext(sprintf("por %s", clave_rotulo(b$clave)), side = 1, at = xb, adj = 0.5,
                line = 2.05, cex = 0.72, col = TINTA)

# (b) bloqueo por clave, en pares
vals <- c(b$pares_sin_bloqueo, b$pares_con_bloqueo, b$perdidos_reales, b$perdidos_estimados)
etiq <- c("sin bloqueo", sprintf("bloqueando por %s", clave_rotulo(b$clave)),
          "perdidos de verdad", "perdidos según la estimación previa")
cols <- c(AZUL, AZUL, BERMELLON, NARANJA)
ys_b <- c(4.95, 3.75, 2.25, 1.05)
graphics::plot.new()
# xaxs = "i": sin el 4 % de aire que agrega R, el cero de las barras cae
# exactamente donde arranca el area de datos y donde arranca el titulo
graphics::plot.window(xlim = c(0, max(vals) * 1.3), ylim = c(0.6, 5.85), xaxs = "i")
# pocas marcas: cada barra lleva su cifra, la rejilla solo da la escala
x_at_b <- pretty(c(0, max(vals)), n = 3)
x_at_b <- x_at_b[x_at_b <= max(vals) * 1.3]
graphics::abline(v = x_at_b, col = REJILLA, lwd = 0.9)
# una sola magnitud para todo el eje: "500 mil" junto a "1 M" obliga a
# convertir de cabeza para comparar dos barras
eje_x(x_at_b, serie_compacta(x_at_b))
graphics::rect(0, ys_b - 0.25, vals, ys_b + 0.25, col = cols, border = NA)
rotulo(vals, ys_b, num(vals), pos = 4, cex = 0.8)
graphics::text(0, ys_b + 0.36, etiq, adj = c(0, 0), cex = 0.78, col = TINTA, xpd = NA)
# La lectura de la figura es el titulo del panel, no una nota gris debajo del
# eje: es la conclusion, y sus dos cifras salen de bloqueo.csv.
perdida_b <- porcentaje(b$perdidos_reales / b$pares_sin_bloqueo, 0L, signo = FALSE)
titulo_b <- if (is.finite(err_b)) {
  sprintf("Bloquear por %s pierde el %s; la estimación lo anticipa a %s",
          clave_rotulo(b$clave), perdida_b, porcentaje(err_b))
} else if (b$perdidos_reales == 0) {
  sprintf("Bloquear por %s no pierde ningún par", clave_rotulo(b$clave))
} else {
  sprintf("Bloquear por %s pierde el %s de los pares; la estimación previa no dejó un error comparable",
          clave_rotulo(b$clave), perdida_b)
}
titular(titulo_b,
        sprintf("Una corrida: %s filas, %s; pares informados con comparación exhaustiva",
                num(b$filas), clave_rotulo(b$clave)))
unidad_x("pares")

encabezado("La estimación previa acierta antes de pagar el recorrido",
           "lupa estima cuántos pares va a generar con una muestra determinista de firmas, y al bloquear por clave estima además cuántos pares deja de encontrar; las dos cifras se conocen antes de recorrer.")
pie()
invisible(grDevices::dev.off())

# ======================================================= 3. cardinalidad
cardinalidad$nivel <- factor(cardinalidad$nivel, levels = NIVELES_CARDINALIDAD)
cardinalidad <- cardinalidad[order(cardinalidad$nivel), ]
cpf <- cardinalidad$candidatos_por_fila
ys <- seq_along(cpf)
# Tres renglones no necesitan el alto de una figura de cuatro paneles, y con
# los margenes de las otras el area de datos ocupa el ancho entero en vez de
# un tercio: el nivel va en el margen izquierdo, corto, y la descripcion
# adentro, sobre la guia de su fila.
# Mas alto que las 3,2 pulgadas de la primera version por dos razones que se
# vieron recien al mirar el PNG: con tres renglones apretados la descripcion de
# la fila de arriba se cruzaba con su propio punto -que cae a la izquierda-, y
# el pie, cuyo margen se mide en lineas y no en proporcion, dejaba una banda
# muerta de casi un tercio del alto.
abrir("banco-cardinalidad.png", 10, 3.9, oma3 = 3.6)
graphics::par(mar = c(2.8, 4.8, 0.6, 1.5))
xlim_k <- c(10^floor(log10(min(cpf))), 10^ceiling(log10(max(cpf))) * 1.8)
graphics::plot.new()
graphics::plot.window(xlim = xlim_k, ylim = c(0.55, length(ys) + 0.75), log = "x")
x_at_k <- potencias(xlim_k)
x_at_k <- x_at_k[x_at_k <= xlim_k[[2L]]]
graphics::abline(v = x_at_k, col = REJILLA, lwd = 0.9)
eje_x(x_at_k, serie_compacta(x_at_k))
unidad_x("candidatos por fila (log)")
# guia punteada hasta el punto: en escala logaritmica una barra no mide nada
graphics::segments(xlim_k[[1L]], ys, cpf, ys, col = GRIS_CLARO, lty = "12", lwd = 1.2)
# Los tres puntos del mismo azul: el color no codifica nada en esta figura, y
# tres colores hacen creer que si.
punto(cpf, ys, color = AZUL, cex = 2)
descripcion <- c(alta = "vocabulario amplio y un número",
                 media = "padrón sintético, vocabulario chico",
                 baja = "la mitad de las filas con el mismo valor")
graphics::mtext(as.character(cardinalidad$nivel), side = 2, at = ys, las = 1,
                line = 0.8, adj = 1, cex = 0.95, font = 2, col = TINTA)
# Justo encima de su guia, mas cerca de ella que de la fila de arriba. La
# descripcion y el punto de la misma fila comparten franja horizontal -el punto
# de cardinalidad alta cae sobre las ultimas palabras-, asi que lo unico que los
# separa es el aire vertical: se mide el alto del texto y se lo aparta del punto
# mas de medio disco, en vez de fijar una fraccion de fila a ojo.
alto_fila_in <- graphics::par("pin")[[2L]] / diff(graphics::par("usr")[c(3L, 4L)])
aire <- (graphics::strheight("Xg", units = "inches", cex = 0.78) * 0.75 +
           0.5 * 2 * graphics::par("cin")[[2L]] * 0.5) / alto_fila_in
nota(rep(xlim_k[[1L]], length(ys)), ys + aire,
     descripcion[as.character(cardinalidad$nivel)], cex = 0.78)
veces <- cardinalidad$veces_respecto_alta
# Los candidatos por fila ya los dice la posicion en el eje; el rotulo trae
# lo que no se ve, el total y la razon contra la cardinalidad alta.
rotulo(cpf, ys, sprintf("%s candidatos  ·  %s", serie_compacta(cardinalidad$candidatos),
                        veces_rotulo(veces)),
       pos = 4, cex = 0.84, offset = 0.9)
encabezado("Lo que manda no es la cantidad de filas: es cuánto se repiten los valores",
           sprintf("Las mismas %s filas, estrategia LSH; sólo cambia la cardinalidad de la clave. De ahí el parámetro bloquear_por.",
                   num(cardinalidad$filas[[1L]])))
pie()
invisible(grDevices::dev.off())

# ============================================================== 4. tamiz
# El titulo lleva el rango medido y ocupa dos renglones; abajo, lugar para la
# nota de lectura de cada panel, que no puede ir sobre los datos. Media pulgada
# mas que la primera version: la nota de la izquierda pasa a tres renglones al
# declarar por que el punto hueco no cuenta, y con 5,6 el tercero se montaba
# sobre el pie.
abrir("banco-tamiz.png", 10, 6.1, oma3 = 6.6)
disponer(matrix(1:2, 1))
graphics::par(mar = MAR + c(2.2, 0, 0, 0))

# (a) por umbral del metodo final. Eje logaritmico: los umbrales probados son
# casi geometricos. El tamano del punto dice cuantos pares aceptados habia:
# perder 1 de 2 no mide lo mismo que perder 22 mil de 31 mil.
# Cuantos pares aceptados hacen falta para que la fraccion se lea como tasa.
# Vive en una sola constante porque la usan tres cosas: el punto hueco, el
# rotulo que cuenta en vez de porcentuar, y el rango del titulo de la figura.
MIN_SOPORTE <- 10L
lsh_umbral <- lsh_umbral[order(lsh_umbral$umbral), ]
u <- lsh_umbral$umbral
p <- lsh_umbral$perdida
soporte <- lsh_umbral$pares_exhaustivo
cex_u <- 0.7 + 1.6 * sqrt(soporte / max(soporte))
panel(c(min(u) / 1.5, max(u) * 1.5), c(0, 1), log = "x",
      titulo = "Según el umbral del método final",
      subtitulo = sprintf("%s filas, %s bandas; el punto crece con la cantidad de pares que el método final acepta",
                          num(lsh_umbral$filas[[1L]]), lsh_umbral$lsh_bandas[[1L]]),
      x_at = u, x_lab = sprintf("%s\n%s", num(u, 2L), num(soporte)),
      y_at = seq(0, 1, 0.25), y_lab = porcentaje(seq(0, 1, 0.25), 0L, signo = FALSE))
unidad_y("Pérdida del tamiz")
# Con un punado de pares aceptados la fraccion no dice nada: 1 de 2 es el
# 50 % y es tambien un solo par. Ese punto va hueco, sin linea que lo una a
# los demas, y con el conteo en vez del porcentaje.
firme <- soporte >= MIN_SOPORTE
perdidos_u <- if (is.null(lsh_umbral$perdidos)) round(p * soporte) else lsh_umbral$perdidos
graphics::lines(u[firme], p[firme], col = BERMELLON, lwd = 2)
punto(u[firme], p[firme], color = BERMELLON, cex = cex_u[firme])
graphics::points(u[!firme], p[!firme], pch = 21, bg = "white", col = BERMELLON,
                 lwd = 1.6, cex = cex_u[!firme])
rotulo(u, p, ifelse(firme, porcentaje(p, 0L, signo = FALSE),
                    sprintf("%s de %s", num(perdidos_u), num(soporte))),
       pos = 3, cex = 0.8, offset = 0.35 + 0.22 * cex_u)
unidad_x("umbral (log) · pares que acepta el método final", line = 2.05)
# El titulo de la figura da un rango que deja afuera las configuraciones sin
# soporte, y este panel dibuja una de ellas: quien mire ve 50 % dibujado y 51 %
# escrito. La convencion tiene que leerse en la figura, no solo en la consola,
# y va aca abajo porque en el subtitulo del panel ocupaba cuatro renglones y se
# metia en el area de datos.
nota_bajo_eje(paste0(
  "el tamiz no conoce el umbral: cuanto más acepta el método final, más se le escapa",
  if (any(!firme)) {
    sprintf(". El punto hueco son %s: un conteo, no una tasa",
            paste(pares(soporte[!firme]), collapse = " y "))
  } else ""
))

# (b) por numero de bandas
ban <- sort(unique(lsh_bandas$lsh_bandas))
media <- tapply(lsh_bandas$perdida, lsh_bandas$lsh_bandas, mean)[as.character(ban)]
maximo <- tapply(lsh_bandas$perdida, lsh_bandas$lsh_bandas, max)[as.character(ban)]
minimo <- tapply(lsh_bandas$perdida, lsh_bandas$lsh_bandas, min)[as.character(ban)]
por_banda <- max(table(lsh_bandas$lsh_bandas))
semillas <- length(unique(lsh_bandas$semilla))
xlim_b <- c(min(ban) - 2, max(ban) + 3)
panel(xlim_b, c(0, 1), titulo = "Según el número de bandas",
      subtitulo = sprintf("%s filas, %s, umbral %s; %s",
                          paste(num(sort(unique(lsh_bandas$filas))), collapse = " y "),
                          if (semillas == 1L) "una semilla" else paste(num(semillas), "semillas"),
                          num(lsh_bandas$umbral[[1L]], 2L),
                          if (por_banda > 1L) "la media, el rango y cada corrida" else "una corrida por punto"),
      x_at = ban, x_lab = num(ban), y_at = seq(0, 1, 0.25),
      y_lab = porcentaje(seq(0, 1, 0.25), 0L, signo = FALSE))
unidad_y("Pérdida del tamiz")
# Rango en gris fino detras, cada corrida hueca y la media llena encima: tres
# cosas distintas, tres marcas distintas, y ninguna tapa a la otra.
graphics::segments(ban, minimo, ban, maximo, col = GRIS_CLARO, lwd = 2)
graphics::points(lsh_bandas$lsh_bandas, lsh_bandas$perdida, pch = 21, bg = "white",
                 col = BERMELLON, lwd = 1.2, cex = 1)
graphics::lines(ban, media, col = BERMELLON, lwd = 2)
punto(ban, media, color = BERMELLON)
rotulo(ban, media, porcentaje(media, 0L, signo = FALSE), pos = 4, cex = 0.8)
unidad_x("bandas LSH")
nota_bajo_eje("más bandas: tamiz más permisivo, menos pérdida y más comparaciones que pagar")

# El titulo trae el rango medido: minimo y maximo de las perdidas medias de
# los dos CSV. Quedan afuera las configuraciones sin soporte —las que el
# camino exhaustivo apenas acepta—, que se dibujan huecas y con su conteo
# justamente porque su fraccion no es comparable.
# El rango del titulo cubre TODO lo que la figura dibuja con soporte: las
# corridas individuales del panel de bandas, no solo sus medias. Con las medias
# el titulo decia "entre 51 % y 76 %" mientras el panel de al lado mostraba
# puntos en 48 % y en 80 %: un rango que la propia figura desmentia.
perdidas_dibujadas <- c(lsh_bandas$perdida, p[firme])
encabezado(sprintf("El tamiz LSH pierde entre %s y %s de los pares que el camino exhaustivo acepta, según umbral y bandas",
                   porcentaje(min(perdidas_dibujadas), 0L, signo = FALSE),
                   porcentaje(max(perdidas_dibujadas), 0L, signo = FALSE)),
           sprintf("El tamiz propone candidatos por Jaccard de q-gramas sin conocer el método final (aquí %s); la pérdida es la fracción de pares que el camino exhaustivo acepta —de cualquier origen, no sólo los sembrados— y el tamiz no propuso.",
                   nombre_metodo(lsh_bandas$metodo[[1L]])))
pie(guion = "benchmark/perdida_lsh.R", con_hilos = FALSE)
invisible(grDevices::dev.off())

cat("figuras escritas en", normalizePath(dir_salida), "\n")

# ================================================ cifras que cita el README
# El README repite algunas de estas cifras en prosa. Se imprimen con el mismo
# formato que llevan las figuras para que, tras otra corrida, un `diff` de esta
# salida diga qué frases quedaron viejas.
i_min <- which.min(n); i_max <- which.max(n)
cifras <- c(
  "candidatos, menor tamaño" = sprintf("%s a %s filas", num(cand[[i_min]]), num(n[[i_min]])),
  "candidatos, mayor tamaño" = sprintf("%s a %s filas", num(cand[[i_max]]), num(n[[i_max]])),
  "tiempo y memoria, menor tamaño" = sprintf("%s y %s", tiempo(t_med[[i_min]]),
                                             if (is.na(rss[[i_min]])) "sin medición" else memoria(rss[[i_min]])),
  "tiempo y memoria, mayor tamaño" = sprintf("%s y %s", tiempo(t_med[[i_max]]),
                                             if (is.na(rss[[i_max]])) "sin medición" else memoria(rss[[i_max]])),
  "crecimiento, primero a último" = if (length(n) >= 2L) {
    sprintf("filas %s, candidatos %s, tiempo %s, memoria %s",
            veces_rotulo(n[[length(n)]] / n[[1L]]),
            veces_rotulo(cand[[length(cand)]] / cand[[1L]]),
            veces_rotulo(t_med[[length(t_med)]] / t_med[[1L]]),
            if (all(is.na(rss))) "sin medición" else
              veces_rotulo(rss[[length(rss)]] / rss[[1L]]))
  } else "un solo tamaño",
  "recall del techo" = paste(porcentaje(recall, 0L, signo = FALSE), collapse = ", "),
  "hilos, 2 respecto de 16" = if (length(i2) && length(i16)) {
    sprintf("%s s con 2, %s s con 16, %s", num(h_med[[i2]]), num(h_med[[i16]]),
            veces_rotulo(h_med[[i2]] / h_med[[i16]]))
  } else "sin las dos configuraciones",
  "error de estimación, rango" = sprintf("de %s a %s", porcentaje(min(err)), porcentaje(max(err))),
  "bloqueo: pérdida" = sprintf("%s (%s de %s), error %s", perdida_b, num(b$perdidos_reales),
                               num(b$pares_sin_bloqueo),
                               if (is.finite(err_b)) porcentaje(err_b) else "sin error"),
  "cardinalidad" = paste(sprintf("%s: %s candidatos, %s", as.character(cardinalidad$nivel),
                                 num(cardinalidad$candidatos), veces_rotulo(veces)),
                         collapse = "; "),
  "tamiz, rango dibujado" = sprintf("de %s a %s (todas las corridas de bandas y los umbrales con al menos %s pares aceptados)",
                                    porcentaje(min(perdidas_dibujadas), 0L, signo = FALSE),
                                    porcentaje(max(perdidas_dibujadas), 0L, signo = FALSE),
                                    MIN_SOPORTE),
  "tamiz por umbral" = paste(sprintf("%s → %s (%s)", num(u, 2L),
                                     porcentaje(p, 1L, signo = FALSE), pares(soporte)), collapse = "; "),
  "tamiz por bandas, media" = paste(sprintf("%s bandas: %s (de %s a %s)", ban,
                                            porcentaje(media, 0L, signo = FALSE),
                                            porcentaje(minimo, 0L, signo = FALSE),
                                            porcentaje(maximo, 0L, signo = FALSE)), collapse = "; ")
)
cat("\ncifras que cita el texto:\n")
cat(sprintf("  %s %s\n", format(paste0(names(cifras), ":"), width = 32L), cifras), sep = "")

# Padron sintetico con verdad conocida.
# Vocabulario chico, cadenas cortas, homonimos: la forma que llena cubetas.

NOMBRES <- c("Ana", "Maria", "Jose", "Luis", "Carlos", "Laura", "Marta", "Juan",
             "Silvia", "Pedro", "Rosa", "Diego", "Gabriela", "Fernando", "Beatriz",
             "Alvaro", "Natalia", "Ruben", "Cecilia", "Gustavo")
APELLIDOS <- c("Perez", "Rodriguez", "Gonzalez", "Fernandez", "Silva", "Martinez",
               "Sosa", "Techera", "Bentancur", "Olivera", "Nunez", "Cabrera",
               "Fagundez", "Ottonelli", "Pintos", "Berrutti", "Rossi", "Diaz",
               "Suarez", "Machado")
CALLES <- c("Av Italia", "Calle Uruguay", "Bulevar Artigas", "18 de Julio",
            "Av Rivera", "Camino Maldonado", "Calle Colonia", "Av Millan",
            "Calle Mercedes", "Av Garzon", "Ruta 8", "Calle Canelones")

.mutar <- function(x, tipo) {
  ch <- strsplit(x, "", fixed = TRUE)[[1L]]
  n <- length(ch)
  if (tipo == 1L) {                       # transposicion
    i <- max(2L, floor(n / 3))
    ch[c(i, i + 1L)] <- ch[c(i + 1L, i)]
  } else if (tipo == 2L) {                # sustitucion s/z, b/v, c/s
    j <- which(ch %in% c("s", "z", "b", "v", "c"))
    if (!length(j)) j <- floor(n / 2)
    j <- j[[1L]]
    ch[j] <- switch(ch[j], "s" = "z", "z" = "s", "b" = "v", "v" = "b",
                    "c" = "s", "e")
  } else if (tipo == 3L) {                # letra faltante
    ch <- ch[-max(2L, floor(n / 2))]
  } else {                                # letra duplicada
    i <- max(2L, floor(n / 2))
    ch <- append(ch, ch[i], after = i)
  }
  paste(ch, collapse = "")
}

padron <- function(n, semilla = 11L, plantados = 200L) {
  set.seed(semilla)
  m <- n - plantados
  base <- paste(
    sample(NOMBRES, m, TRUE), sample(NOMBRES, m, TRUE),
    sample(APELLIDOS, m, TRUE), sample(APELLIDOS, m, TRUE),
    sample(CALLES, m, TRUE), sample(100:2999, m, TRUE)
  )
  origen <- sample.int(m, plantados)
  tipos <- sample.int(4L, plantados, TRUE)
  copia <- vapply(seq_len(plantados),
                  function(k) .mutar(base[origen[[k]]], tipos[[k]]),
                  character(1L))
  v <- c(base, copia)
  list(
    datos = data.frame(v = v, stringsAsFactors = FALSE),
    verdad = data.frame(
      fila_1 = pmin(origen, m + seq_len(plantados)),
      fila_2 = pmax(origen, m + seq_len(plantados)),
      stringsAsFactors = FALSE
    )
  )
}

# Equivale a la normalizacion por omision de .texto_fila_aproximada() SOLO para
# este vocabulario: letras ASCII, digitos y espacios. La del paquete ademas
# quita acentos, puntuacion, ligaduras y ancho completo; si el padron
# incorporara algo de eso, el techo dejaria de medirse con el criterio del
# paquete, y techo() se detiene antes de que pase inadvertido.
norm <- function(x) gsub("[[:space:]]+", " ", trimws(tolower(x)), perl = TRUE)

# El techo: pares plantados que realmente pasan el umbral de la medida final.
# Se llama con los valores por omision de la instalacion medida (p_jw es el
# peso del prefijo de Jaro-Winkler), para que el techo y el paquete acepten
# con el mismo criterio.
techo <- function(p, umbral, metodo = "jw", p_jw = 0) {
  if (any(grepl("[^A-Za-z0-9[:space:]]", p$datos$v, perl = TRUE))) {
    stop(
      "El padron trae caracteres fuera de [A-Za-z0-9 ] y norm() ya no ",
      "equivale a la normalizacion del paquete: ajuste norm() antes de medir.",
      call. = FALSE
    )
  }
  d <- stringdist::stringdist(norm(p$datos$v[p$verdad$fila_1]),
                              norm(p$datos$v[p$verdad$fila_2]),
                              method = metodo, p = p_jw)
  p$verdad[d <= umbral, , drop = FALSE]
}

qg <- function(x, q = 3L) {
  x <- norm(x)
  if (nchar(x) < q) return(x)
  unique(substring(x, seq_len(nchar(x) - q + 1L),
                   seq_len(nchar(x) - q + 1L) + q - 1L))
}

jacc <- function(a, b, q = 3L) {
  A <- qg(a, q)
  B <- qg(b, q)
  length(intersect(A, B)) / length(union(A, B))
}

# Vocabularios para el contraste de cardinalidad.
SIL1 <- c("ma", "jo", "lu", "ca", "la", "si", "pe", "ro", "di", "ga", "fe", "be", "al", "na",
          "ru", "ce", "gu", "va", "he", "ni", "to", "mi", "sa", "te", "za", "bo", "cu", "re")
SIL2 <- c("ri", "na", "se", "lo", "ta", "ve", "cha", "llo", "gue", "fre", "tin", "dro", "bel",
          "nes", "ler", "mun", "dez", "rez", "gal", "vic", "ram", "sol", "fer", "bar", "zon",
          "cor", "pin", "tur")
SIL3 <- c("a", "o", "es", "in", "an", "el", "ez", "ia", "io", "ur", "al", "or")

voc <- function(k, s) {
  set.seed(s)
  v <- unique(paste0(sample(SIL1, k * 3L, TRUE),
                     sample(SIL2, k * 3L, TRUE),
                     sample(SIL3, k * 3L, TRUE)))
  v[seq_len(min(k, length(v)))]
}

gen_cardinalidad <- function(n, nivel) {
  set.seed(11)
  if (nivel == "alta") {
    nm <- voc(300L, 1L)
    ap <- voc(300L, 2L)
    v <- paste(sample(nm, n, TRUE), sample(ap, n, TRUE), sample(ap, n, TRUE),
               sample(100000:999999, n, TRUE))
  } else if (nivel == "media") {
    nm <- voc(20L, 1L)
    ap <- voc(20L, 2L)
    ca <- voc(12L, 3L)
    v <- paste(sample(nm, n, TRUE), sample(nm, n, TRUE), sample(ap, n, TRUE),
               sample(ap, n, TRUE), sample(ca, n, TRUE),
               sample(100:2999, n, TRUE))
  } else {
    nm <- voc(300L, 1L)
    ap <- voc(300L, 2L)
    v <- paste(sample(nm, n, TRUE), sample(ap, n, TRUE), sample(ap, n, TRUE),
               sample(100000:999999, n, TRUE))
    v[sample.int(n, floor(n / 2L))] <- "SIN DATO"
  }
  data.frame(v = v, stringsAsFactors = FALSE)
}

# Rehace la cifra de `?perfilar` sobre lo que la regla de digitos de la
# proteccion de datos personales tapa DE MAS: textos que no son el valor de
# nadie, frente a un millon de cedulas, 300.000 telefonos fijos y 300.000
# celulares protegidos. Cuenta que fraccion de cada familia se tapa.
#
# Se corre desde la raiz del paquete y tarda varios minutos: `pkgload` carga el
# paquete tal como esta en el arbol. La cifra publicada sale de esta salida; si
# la regla cambia, se vuelve a correr y se actualiza la documentacion.
#
# Lo que mide: el documento PARCIAL -sin su verificador- no se distingue de un
# numero entero de siete cifras que coincide con el por azar, y la fraccion que
# se tapa es la de documentos protegidos entre los posibles de su rango. El
# resto de las familias tiene que dar cero.

pkgload::load_all(".", quiet = TRUE)
set.seed(7)
n_documentos <- 1e6
documentos <- unique(sprintf(
  "%d.%03d.%03d-%d", sample(1:6, n_documentos, TRUE),
  sample(0:999, n_documentos, TRUE), sample(0:999, n_documentos, TRUE),
  sample(0:9, n_documentos, TRUE)
))
fijos <- unique(sprintf("%d%07d", sample(c(2, 4), 3e5, TRUE),
                        sample(0:9999999, 3e5, TRUE)))
celulares <- unique(sprintf("09%d%06d", sample(1:9, 3e5, TRUE),
                            sample(0:999999, 3e5, TRUE)))
valores <- c(documentos, fijos, celulares)
m <- 400
familias <- list(
  coordenada_7_decimales = sprintf("%.7f", runif(m, -35, -30)),
  coordenada_6_decimales = sprintf("%.6f", runif(m, -35, -30)),
  punto_wkt = sprintf("POINT (%.6f %.6f)", runif(m, -58, -53), runif(m, -35, -30)),
  hora_con_fraccion = sprintf("2024-05-17 %02d:%02d:%02d.%07d", sample(0:23, m, TRUE),
                              sample(0:59, m, TRUE), sample(0:59, m, TRUE),
                              sample(0:9999999, m, TRUE)),
  direccion_ip = sprintf("%d.%d.%d.%d", sample(1:255, m, TRUE), sample(0:255, m, TRUE),
                         sample(0:255, m, TRUE), sample(0:255, m, TRUE)),
  isbn = sprintf("978-%d-%02d-%06d-%d", sample(0:9, m, TRUE), sample(0:99, m, TRUE),
                 sample(0:999999, m, TRUE), sample(0:9, m, TRUE)),
  tras_un_asterisco = sprintf("pedido *%d", sample(1000001:6999999, m)),
  tras_un_numeral = sprintf("ticket #%d", sample(1000001:6999999, m)),
  ip_fin_de_frase = sprintf("servidor %d.%d.%d.%d.", sample(1:255, m, TRUE),
                            sample(0:255, m, TRUE), sample(0:255, m, TRUE),
                            sample(0:255, m, TRUE)),
  isbn10 = sprintf("ISBN %d-%03d-%05d-%d", sample(0:9, m, TRUE), sample(0:999, m, TRUE),
                   sample(0:99999, m, TRUE), sample(0:9, m, TRUE)),
  lista_barra_vertical = sprintf("%d|%d|%d|%d", sample(10:99, m, TRUE),
                                 sample(10:999, m, TRUE), sample(10:99, m, TRUE),
                                 sample(10:99, m, TRUE)),
  razon = sprintf("razon %d:%d", sample(100:999, m, TRUE), sample(1000:9999, m, TRUE)),
  lista_con_espacios = sprintf("codigos %d %d %d %d", sample(10:999, m, TRUE),
                               sample(10:999, m, TRUE), sample(10:99, m, TRUE),
                               sample(10:999, m, TRUE)),
  p_valor_7_decimales = sprintf("p=%.7f", runif(m, 0.1, 0.9)),
  monto_con_decimales = sprintf("%.2f", runif(m, 1e6, 7e6)),
  monto_con_miles = formatC(runif(m, 1e6, 7e6), format = "f", digits = 2,
                            big.mark = ".", decimal.mark = ","),
  hora = sprintf("2024-05-17 %02d:%02d:%02d", sample(0:23, m, TRUE),
                 sample(0:59, m, TRUE), sample(0:59, m, TRUE)),
  lista = sprintf("%d, %d, %d", sample(10:999, m, TRUE),
                  sample(10:999, m, TRUE), sample(10:999, m, TRUE)),
  conteo_6_cifras = sprintf("%d registros", sample(100000:999999, m)),
  conteo_7_cifras = sprintf("faltantes: %d (12.3%%)", sample(1000001:6999999, m)),
  conteo_7_con_miles = sprintf("%s filas", formatC(sample(1000001:6999999, m),
                                                   format = "d", big.mark = ".",
                                                   decimal.mark = ",")),
  entero_8_cifras = sprintf("expediente %d", sample(10000000:69999999, m)),
  # Ronda 24: las familias que la refutacion encontro tapadas de mas. Van al
  # final para no cambiar los numeros de las de arriba.
  tras_una_equis = sprintf("serie X%d", sample(1000001:6999999, m)),
  por_una_equis = sprintf("%d x %d", sample(2:99, m, TRUE), sample(1000001:6999999, m)),
  coordenada_cero_adelante = sprintf("lon -0%.6f", runif(m, 53, 58)),
  isbn10_espacios = sprintf("ISBN %d %03d %05d %d", sample(0:9, m, TRUE),
                            sample(0:999, m, TRUE), sample(0:99999, m, TRUE),
                            sample(0:9, m, TRUE)),
  isbn10_puntos = sprintf("ISBN %d.%03d.%05d.%d", sample(0:9, m, TRUE),
                          sample(0:999, m, TRUE), sample(0:99999, m, TRUE),
                          sample(0:9, m, TRUE)),
  # Sin la etiqueta, con su digito de control valido: un ISBN de verdad.
  isbn10_sin_etiqueta = vapply(seq_len(m), function(i) {
    cifras <- sample(0:9, 9, TRUE)
    control <- (11 - sum(cifras * 10:2) %% 11) %% 11
    paste0(cifras[1], "-", paste(cifras[2:4], collapse = ""), "-",
           paste(cifras[5:9], collapse = ""), "-",
           if (control == 10) "X" else control)
  }, character(1L)),
  fecha_con_espacios = sprintf("%02d %02d %d", sample(1:28, m, TRUE),
                               sample(1:12, m, TRUE), sample(1990:2025, m, TRUE)),
  fecha_hora_dos_espacios = sprintf("%d-%02d-%02d  %02d:%02d:%02d",
                                    sample(2015:2025, m, TRUE), sample(1:12, m, TRUE),
                                    sample(1:28, m, TRUE), sample(0:23, m, TRUE),
                                    sample(0:59, m, TRUE), sample(0:59, m, TRUE)),
  # Ronda 25: las familias que la refutacion midio. Al final, otra vez, para no
  # cambiar los numeros de las de arriba.
  ip_con_mascara = sprintf("%d.%d.%d.%d/%d", sample(1:255, m, TRUE),
                           sample(0:255, m, TRUE), sample(0:255, m, TRUE),
                           sample(0:255, m, TRUE), sample(8:32, m, TRUE)),
  ip_con_puerto = sprintf("%d.%d.%d.%d:%d", sample(1:255, m, TRUE),
                          sample(0:255, m, TRUE), sample(0:255, m, TRUE),
                          sample(0:255, m, TRUE), sample(1:65535, m, TRUE)),
  monto_10_cifras_miles = formatC(runif(m, 1e9, 1e10), format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
  monto_11_cifras_miles = formatC(runif(m, 1e10, 1e11), format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
  hora_fraccion_8 = sprintf("2024-05-17 %02d:%02d:%02d.%08d", sample(0:23, m, TRUE),
                            sample(0:59, m, TRUE), sample(0:59, m, TRUE),
                            sample(0:99999999, m, TRUE)),
  lista_coma_sin_espacio = sprintf("%d,%d,%d", sample(10:999, m, TRUE),
                                   sample(10:999, m, TRUE), sample(10:999, m, TRUE)),
  lista_coma_ideografica = sprintf("%d\u3001%d\u3001%d", sample(10:999, m, TRUE),
                                   sample(10:999, m, TRUE), sample(10:999, m, TRUE)),
  isbn10_control_invalido = vapply(seq_len(m), function(i) {
    cifras <- sample(0:9, 9, TRUE)
    control <- (11 - sum(cifras * 10:2) %% 11) %% 11
    malo <- (if (control == 10) 0 else control + 1) %% 10
    paste0(cifras[1], "-", paste(cifras[2:4], collapse = ""), "-",
           paste(cifras[5:9], collapse = ""), "-", malo)
  }, character(1L)),
  fecha_hora_barra = sprintf("%d-%02d-%02d / %02d:%02d:%02d",
                             sample(2015:2025, m, TRUE), sample(1:12, m, TRUE),
                             sample(1:28, m, TRUE), sample(0:23, m, TRUE),
                             sample(0:59, m, TRUE), sample(0:59, m, TRUE)),
  asterisco_ocho_rayos = sprintf("pedido \u2733%d", sample(1000001:6999999, m)),
  # Y las decisiones que la ayuda declaraba sin cifra.
  rango_de_anios = sprintf("periodo %d-%d", sample(1990:2010, m, TRUE),
                           sample(2011:2025, m, TRUE)),
  rango_raya_cp1252 = sprintf("%d\u0096%d", sample(1000:9999, m, TRUE),
                              sample(1000:9999, m, TRUE)),
  lista_tabulador = sprintf("%d\t%d\t%d", sample(10:999, m, TRUE),
                            sample(10:999, m, TRUE), sample(10:999, m, TRUE)),
  dos_numeros_en_dos_lineas = sprintf("%d\n%d", sample(1000:9999, m, TRUE),
                                      sample(1000:9999, m, TRUE)),
  # Ronda 26: las familias que la refutacion midio sin cifra en la ayuda, y
  # los importes de cuatro grupos que comparten forma con un celular con su
  # prefijo de pais. Al final, para no cambiar los numeros de las de arriba.
  version_cuatro_partes = sprintf("v%d.%d.%d.%d", sample(1:9, m, TRUE),
                                  sample(0:99, m, TRUE), sample(0:999, m, TRUE),
                                  sample(0:9999, m, TRUE)),
  rango_de_redes = sprintf("%d.%d.%d.0/24-%d.%d.%d.0/24", sample(1:255, m, TRUE),
                           sample(0:255, m, TRUE), sample(0:255, m, TRUE),
                           sample(1:255, m, TRUE), sample(0:255, m, TRUE),
                           sample(0:255, m, TRUE)),
  expediente_con_guiones = sprintf("exp %d-%d-%06d", sample(2015:2024, m, TRUE),
                                   sample(1:9, m, TRUE), sample(0:999999, m, TRUE)),
  monto_12_cifras_miles = formatC(runif(m, 1e11, 1e12), format = "f", digits = 0,
                                  big.mark = ".", decimal.mark = ","),
  monto_10_cifras_coma = formatC(runif(m, 1e9, 1e10), format = "f", digits = 0,
                                 big.mark = ","),
  monto_10_cifras_apostrofo = formatC(runif(m, 1e9, 1e10), format = "f", digits = 0,
                                      big.mark = "'"),
  # Ronda 27: las familias que la refutacion midio -los importes de cuatro grupos
  # cuyo segundo empieza con cero, el telefono con su prefijo de pais pegado o
  # con espacios, la version de cinco partes, el correo en palabras- y, para
  # comparar, el telefono con guiones, pegado y sin el prefijo. Al final, para
  # no cambiar los numeros de las de arriba.
  monto_segundo_grupo_09 = sprintf("$ %d.09%d.%03d.%03d", sample(1:9, m, TRUE),
                                   sample(1:9, m, TRUE), sample(0:999, m, TRUE),
                                   sample(0:999, m, TRUE)),
  monto_segundo_grupo_02 = sprintf("$ %d.02%d.%03d.%03d", sample(1:9, m, TRUE),
                                   sample(0:9, m, TRUE), sample(0:999, m, TRUE),
                                   sample(0:999, m, TRUE)),
  monto_miles_con_decimales_4g = sprintf("$ %d.%03d.%03d.%03d,%02d",
                                         sample(1:9, m, TRUE), sample(0:999, m, TRUE),
                                         sample(0:999, m, TRUE), sample(0:999, m, TRUE),
                                         sample(0:99, m, TRUE)),
  celular_e164_no_protegido = sprintf("+5989%d%06d", sample(1:9, m, TRUE),
                                      sample(0:999999, m, TRUE)),
  fijo_intl_espacios = sprintf("+598 %d%03d %04d", sample(c(2, 4), m, TRUE),
                               sample(0:999, m, TRUE), sample(0:9999, m, TRUE)),
  celular_intl_espacios = sprintf("+598 9%d %03d %03d", sample(1:9, m, TRUE),
                                  sample(0:999, m, TRUE), sample(0:999, m, TRUE)),
  version_cinco_partes = sprintf("v%d.%d.%d.%d.%d", sample(1:9, m, TRUE),
                                 sample(0:99, m, TRUE), sample(0:999, m, TRUE),
                                 sample(0:9999, m, TRUE), sample(0:99, m, TRUE)),
  correo_palabras_no_protegido = sprintf("u%05d arroba dominio%02d punto com",
                                         sample(0:99999, m, TRUE),
                                         sample(0:99, m, TRUE)),
  fijo_e164_no_protegido = sprintf("+598%d%07d", sample(c(2, 4), m, TRUE),
                                   sample(0:9999999, m, TRUE)),
  fijo_nacional_espacios = sprintf("%d%03d %04d", sample(c(2, 4), m, TRUE),
                                   sample(0:999, m, TRUE), sample(0:9999, m, TRUE)),
  celular_nacional_espacios = sprintf("09%d %03d %03d", sample(1:9, m, TRUE),
                                      sample(0:999, m, TRUE), sample(0:999, m, TRUE)),
  fijo_intl_guiones = sprintf("+598-%d%03d-%04d", sample(c(2, 4), m, TRUE),
                              sample(0:999, m, TRUE), sample(0:9999, m, TRUE))
)
cat("valores protegidos:", length(valores), "\n")
for (familia in names(familias)) {
  x <- familias[[familia]]
  y <- .reemplazar_valores_protegidos(x, valores, exigir_limites = FALSE)
  cat(sprintf("%-24s %5.1f %%   ej: %s\n", familia, 100 * mean(y != x), x[[1L]]))
}
control <- c(documentos[1:3], sub("-[0-9]$", "", documentos[4:6]), fijos[1:3],
             celulares[1:3])
cat("control, deben taparse los 12:",
    sum(.reemplazar_valores_protegidos(control, valores) != control), "\n")

# El telefono fijo que no es de nadie, por ESCRITURA, sobre las mismas cifras y
# con una muestra diez veces mayor. Las familias de arriba sortean cifras
# distintas para cada escritura y con 400 de cada una la diferencia entre ellas
# es ruido: la ayuda decia "entre tres y seis de cada cien, igual con su prefijo"
# y estas mismas familias daban 5,5 sin el prefijo, 2,8 con el prefijo y 1,5
# pegado. Escritos con las mismas cifras se tapan exactamente los mismos: la
# regla prueba la parte nacional, se escriba como se escriba. Ronda 28. Al
# final y con su propia semilla, para no cambiar los numeros de arriba.
set.seed(2809)
m_fijos <- 4000
a <- sample(c(2, 4), m_fijos, TRUE)
b <- sample(0:999, m_fijos, TRUE)
c4 <- sample(0:9999, m_fijos, TRUE)
fijos_por_escritura <- list(
  fijo_nacional_espacios = sprintf("%d%03d %04d", a, b, c4),
  fijo_nacional_guiones = sprintf("%d%03d-%04d", a, b, c4),
  fijo_intl_espacios = sprintf("+598 %d%03d %04d", a, b, c4),
  fijo_intl_guiones = sprintf("+598-%d%03d-%04d", a, b, c4),
  fijo_e164_no_protegido = sprintf("+598%d%03d%04d", a, b, c4),
  fijo_nacional_pegado = sprintf("%d%03d%04d", a, b, c4)
)
tapados_fijos <- sapply(fijos_por_escritura, function(x) {
  .reemplazar_valores_protegidos(x, valores, exigir_limites = FALSE) != x
})
cat("\nfijos por escritura, las mismas", m_fijos, "cifras en cada una:\n")
for (familia in names(fijos_por_escritura)) {
  p <- mean(tapados_fijos[, familia])
  cat(sprintf("%-24s %5.1f %%  (+- %.1f, dos errores estandar)\n", familia,
              100 * p, 200 * sqrt(p * (1 - p) / m_fijos)))
}
cat("tapados en todas las escrituras:", sum(rowSums(tapados_fijos) == ncol(tapados_fijos)),
    "| en alguna:", sum(rowSums(tapados_fijos) > 0), "\n")

# Y las listas de telefonos unidos por una barra: desde la ronda 28 cada numero
# E.164 de la lista se prueba por su parte nacional, como con " / " o con ";",
# y la lista pegada se tapa lo mismo que la separada. Al final, con su semilla.
set.seed(2810)
listas <- list(
  lista_e164_barra = sprintf("+5989%d%06d/+598%d%07d", sample(1:9, m, TRUE),
                             sample(0:999999, m, TRUE), sample(c(2, 4), m, TRUE),
                             sample(0:9999999, m, TRUE)),
  lista_e164_barra_esp = sprintf("+5989%d%06d / +598%d%07d", sample(1:9, m, TRUE),
                                 sample(0:999999, m, TRUE),
                                 sample(c(2, 4), m, TRUE),
                                 sample(0:9999999, m, TRUE)),
  lista_nacional_barra = sprintf("09%d%06d/%d%07d", sample(1:9, m, TRUE),
                                 sample(0:999999, m, TRUE),
                                 sample(c(2, 4), m, TRUE),
                                 sample(0:9999999, m, TRUE))
)
cat("\nlistas de dos telefonos:\n")
for (familia in names(listas)) {
  x <- listas[[familia]]
  y <- .reemplazar_valores_protegidos(x, valores, exigir_limites = FALSE)
  cat(sprintf("%-24s %5.1f %%   ej: %s\n", familia, 100 * mean(y != x), x[[1L]]))
}

# Y los importes con centavos, por ESCRITURA, sobre las mismas cifras enteras:
# la familia `monto_con_decimales` de arriba sortea centavos al azar, y uno de
# cada cien termina en `.00`. Ronda 30: la ronda 29 leia el decimal de ceros como
# el entero y el importe REDONDO de siete cifras -`"5460537.00"`, un sueldo, una
# cuota- se tapaba como el entero de siete cifras, uno de cada cinco, contra
# ninguno en la ronda 28. La cifra entera entra ahora solo como documento entero.
# El de ocho cifras con centavos se tapa como el entero de ocho: su parte entera
# coincide con un documento entero, y eso ya pasaba en la ronda 28. Al final, con
# su semilla, para no cambiar los numeros de arriba.
set.seed(3012)
m_importes <- 4000
ent7 <- sample(1000001:6999999, m_importes)
ent8 <- sample(10000000:69999999, m_importes)
centavos <- sample(1:99, m_importes, TRUE)
importes <- list(
  monto_7_con_centavos = sprintf("%d.%02d", ent7, centavos),
  monto_7_cero_centavos = sprintf("%d.00", ent7),
  monto_7_un_cero = sprintf("%d.0", ent7),
  monto_7_coma_00 = sprintf("%d,00", ent7),
  frase_monto_7_00 = sprintf("transferencia por %d.00 pesos", ent7),
  monto_7_cuatro_ceros = sprintf("%d.0000", ent7),
  monto_7_entero = sprintf("%d", ent7),
  monto_8_con_centavos = sprintf("%d.%02d", ent8, centavos),
  monto_8_cero_centavos = sprintf("%d.00", ent8),
  monto_8_entero = sprintf("%d", ent8)
)
cat("\nimportes, las mismas", m_importes, "cifras enteras en cada escritura:\n")
for (familia in names(importes)) {
  x <- importes[[familia]]
  p <- mean(.reemplazar_valores_protegidos(x, valores, exigir_limites = FALSE) != x)
  cat(sprintf("%-24s %5.1f %%  (+- %.1f, dos errores estandar)   ej: %s\n",
              familia, 100 * p, 200 * sqrt(p * (1 - p) / m_importes), x[[1L]]))
}

# Ronda 31: el numero de planilla con separador de miles -"52.317.219,00",
# "59.898.617.353"- y la cientifica con un espacio antes del exponente
# -"5.2317219 e+07"- se leen como la misma cifra entera que sin miles y con el
# exponente pegado, y el celular con su prefijo de pais escrito asi, por su
# parte nacional. Las mismas cifras enteras de los importes de arriba en cada
# escritura, y celulares que no son de nadie con las mismas cifras pegados y
# con miles. Al final, con su semilla, para no cambiar los numeros de arriba.
set.seed(3101)
con_miles <- function(x, marca) {
  formatC(x, format = "f", digits = 0, big.mark = marca,
          decimal.mark = if (identical(marca, ".")) "," else ".")
}
celulares_libres <- as.numeric(sprintf("5989%d%06d", sample(1:9, m_importes, TRUE),
                                       sample(0:999999, m_importes, TRUE)))
planilla <- list(
  monto_7_miles_cero_centavos = paste0(con_miles(ent7, "."), ",00"),
  monto_8_miles = con_miles(ent8, "."),
  monto_8_miles_cero_centavos = paste0(con_miles(ent8, "."), ",00"),
  monto_8_miles_coma_punto00 = paste0(con_miles(ent8, ","), ".00"),
  monto_8_cientifica = sprintf("%.7fe+07", ent8 / 1e7),
  monto_8_cientifica_espacio = sprintf("%.7f e+07", ent8 / 1e7),
  celular_e164_pegado = paste0("+", formatC(celulares_libres, format = "f",
                                            digits = 0)),
  celular_e164_miles = con_miles(celulares_libres, "."),
  celular_e164_miles_cero_centavos = paste0(con_miles(celulares_libres, "."),
                                            ",00"),
  celular_e164_miles_espacio = con_miles(celulares_libres, " ")
)
cat("\nnumero de planilla, las mismas", m_importes, "cifras en cada escritura:\n")
for (familia in names(planilla)) {
  x <- planilla[[familia]]
  p <- mean(.reemplazar_valores_protegidos(x, valores, exigir_limites = FALSE) != x)
  cat(sprintf("%-32s %5.1f %%  (+- %.1f, dos errores estandar)   ej: %s\n",
              familia, 100 * p, 200 * sqrt(p * (1 - p) / m_importes), x[[1L]]))
}

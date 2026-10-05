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
                                    sample(0:59, m, TRUE), sample(0:59, m, TRUE))
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

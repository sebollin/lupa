# Una columna que R declara numerica y que trae clase propia. Es el minimo que
# necesitan las pruebas que solo miden la DECLARACION -que el diagnostico no se
# evalua y se dice por que-, sin depender de ningun paquete opcional.
#
# La version que ademas ABORTA al dividirse -el mecanismo de
# `lubridate::Period`- vive en `test-O26-la-clase-que-no-es-un-numero.R`, en el
# propio archivo: los metodos S4 definidos en un helper no sobreviven al
# despacho desde el espacio de nombres del paquete.
.columna_con_clase_numerica <- function(valores) {
  structure(as.numeric(valores), class = "lupaMagnitud")
}

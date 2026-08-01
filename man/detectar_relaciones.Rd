% Please edit documentation in R/claves-relaciones.R
\name{detectar_relaciones}
\alias{detectar_relaciones}
\title{Detectar relaciones entre dos tablas}
\usage{
detectar_relaciones(tabla1, tabla2, muestra = 1e+05)
}
\arguments{
\item{tabla1, tabla2}{Objetos que heredan de \code{data.frame}.}

\item{muestra}{Máximo de filas del lado verificado que se usan para estimar
cada cobertura. El muestreo es sistemático y reproducible; el lado de
referencia no se muestrea.}
}
\value{
Data frame con columnas comparadas, cardinalidad, coincidencias y
coberturas de integridad referencial en ambas direcciones. Los atributos
\code{filas_totales}, \code{filas_analizadas} y \code{muestreado} documentan el muestreo.
}
\description{
Examina todos los pares de columnas y describe su cardinalidad a partir de
la unicidad completa de cada lado. La cobertura \code{tabla1_en_tabla2} es la proporción de
valores no ausentes de la primera columna que existe en la segunda; la
cobertura inversa se informa de forma simétrica. Así se puede escoger la
dirección PK/FK sin imponerla de antemano.
}
\details{
Cuando una tabla supera \code{muestra}, la función estima cada cobertura con una
muestra sistemática del lado que se verifica y conserva completo el conjunto
de referencia. La cardinalidad y la cantidad de valores comunes siempre se
calculan con ambas columnas completas.
}
\examples{
personas <- data.frame(id = 1:3)
tramites <- data.frame(persona_id = c(1, 1, 3, 4))
detectar_relaciones(personas, tramites)
}

% Please edit documentation in R/fechas.R
\name{detectar_formatos_fecha}
\alias{detectar_formatos_fecha}
\title{Detectar formatos de fecha}
\usage{
detectar_formatos_fecha(x, muestra = 1e+05)
}
\arguments{
\item{x}{Vector de texto, fechas o fechas-hora.}

\item{muestra}{Máximo de valores que se analizan.}
}
\value{
Data frame con formato, frecuencia, proporción, estado y conteos de
casos inequívocos y ambiguos. Los atributos informan el muestreo, la
cantidad de valores compatibles y la presencia de formatos mixtos.
}
\description{
Reconoce formatos de fecha y fecha-hora sin escoger arbitrariamente entre
día/mes y mes/día. Cuando todos los valores con barras son ambiguos, devuelve
ambos formatos con estado \code{"candidato"}. El atributo \code{formatos_mixtos}
indica si hay evidencia de dos o más representaciones en la columna.
El formato compacto \verb{\%Y\%m\%d} exige un año entre 1800 y 2100 para evitar que
identificadores de ocho dígitos se clasifiquen parcialmente como fechas.
}
\examples{
detectar_formatos_fecha(c("2020-01-31", "31/01/2020"))
detectar_formatos_fecha(c("01/02/2020", "02/03/2020"))
}

% Please edit documentation in R/tipos.R
\name{inferir_tipo}
\alias{inferir_tipo}
\title{Inferir el tipo implícito de un vector}
\usage{
inferir_tipo(x, umbral = 0.8, muestra = 1e+05)
}
\arguments{
\item{x}{Vector que se desea examinar.}

\item{umbral}{Proporción mínima para asignar un tipo implícito.}

\item{muestra}{Máximo de valores que se analizan.}
}
\value{
Lista de clase \code{inferencia_tipo} con \code{tipo}, \code{proporcion}, conteos,
candidatos evaluados y, cuando corresponde, formatos de fecha.
}
\description{
Para columnas de texto distingue valores lógicos, enteros, dobles, fechas,
fechas-hora, horas, identificadores y texto. Además de la etiqueta devuelve
la proporción de valores compatibles, siempre en la escala \verb{[0, 1]}.
}
\examples{
inferir_tipo(c("1", "2", "3"))
inferir_tipo(c("2020-01-01", "31/12/2020"))
}

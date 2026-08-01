% Please edit documentation in R/patrones.R
\name{descubrir_patrones}
\alias{descubrir_patrones}
\title{Descubrir patrones de formato}
\usage{
descubrir_patrones(
  x,
  distinguir_mayusculas = TRUE,
  expandir = FALSE,
  max_patrones = 20,
  na.rm = TRUE,
  muestra = 1e+05
)
}
\arguments{
\item{x}{Vector que se convertirá a texto.}

\item{distinguir_mayusculas}{Si es \code{TRUE}, distingue \code{a} de \code{A}.}

\item{expandir}{Si es \code{FALSE}, colapsa tokens repetidos (\code{9999} a \verb{9+}).}

\item{max_patrones}{Número máximo de patrones que se muestran.}

\item{na.rm}{Si es \code{TRUE}, excluye los valores ausentes.}

\item{muestra}{Máximo de valores que se analizan.}
}
\value{
Un data frame de clase \code{patrones} con patrón, frecuencia, proporción
y ejemplos. Los atributos \code{total}, \code{analizados} y \code{muestreado} describen
el posible muestreo. Las proporciones siempre están en \verb{[0, 1]}.
}
\description{
Generaliza un vector de texto mediante la convención del \emph{Pattern Finder}
de DataCleaner: \code{9} representa un dígito, \code{a} una letra minúscula y \code{A} una
letra mayúscula. Los símbolos y espacios se conservan literalmente.
}
\details{
El cálculo aplica reemplazos vectorizados sobre el vector completo. Si el
vector supera \code{muestra}, usa una muestra sistemática reproducible y registra
esa decisión en los atributos del resultado.
}
\examples{
descubrir_patrones(
  c("2020-01-31", "2021-12-01", "31/01/2020"),
  expandir = TRUE
)
}

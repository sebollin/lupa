% Please edit documentation in R/agregacion.R
\name{granularidades}
\alias{granularidades}
\alias{transiciones_granularidad}
\title{Granularidades y transiciones de agregación}
\usage{
granularidades()

transiciones_granularidad()
}
\value{
Data frames con niveles o aristas del grafo de granularidad.
}
\description{
\code{granularidades()} declara los diez niveles del marco. Los primeros seis
están implementados; los restantes quedan registrados para extender el
modelo sin convertir la granularidad en una escala lineal.
}
\details{
\code{transiciones_granularidad()} devuelve el grafo dirigido de agregaciones.
La transición \code{instanciaAtributo} a \code{instanciaEntidad} se incorpora porque
el propio marco la usa aunque no aparezca en su tabla no exhaustiva.
}
\examples{
granularidades()
transiciones_granularidad()
}

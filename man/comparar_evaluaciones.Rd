% Please edit documentation in R/evaluacion.R
\name{comparar_evaluaciones}
\alias{comparar_evaluaciones}
\title{Comparar evaluaciones de perfil}
\usage{
comparar_evaluaciones(anterior, actual)
}
\arguments{
\item{anterior, actual}{Objetos creados por \code{evaluar()}.}
}
\value{
Data frame con resultados anterior y actual, y \code{delta}.
}
\description{
Calcula el cambio de \code{EvaluacionPerfil} entre dos corridas. Cada objeto debe
contener una sola \code{id_medicion}; no persiste los resultados. Para una serie
de N corridas use \code{historico_calidad()} y \code{detectar_deriva_calidad()}.
}
\examples{
# Ver ejemplos de evaluar().
}

% Please edit documentation in R/evaluacion.R
\name{reglas_evaluacion}
\alias{reglas_evaluacion}
\alias{regla_evaluacion}
\alias{perfil_evaluacion}
\alias{perfiles_madurez}
\title{Reglas y perfiles de evaluación}
\usage{
regla_evaluacion(nombre, condicion, metricas = NULL)

perfil_evaluacion(nombre, ...)

perfiles_madurez(metricas = NULL)
}
\arguments{
\item{nombre}{Nombre de la regla o del perfil.}

\item{condicion}{Función que recibe resultados en \verb{[0, 1]} y devuelve un
vector lógico sin ausentes de la misma longitud.}

\item{metricas}{Nombres de métricas instanciadas a las que se aplica la
regla. \code{NULL} aplica la condición a todas.}

\item{...}{Reglas creadas por \code{regla_evaluacion()} o una única lista que las
contenga.}
}
\value{
\code{regla_evaluacion()} devuelve una \code{regla_evaluacion};
\code{perfil_evaluacion()} devuelve un \code{perfil_evaluacion}; y
\code{perfiles_madurez()} devuelve una lista de tres perfiles.
}
\description{
Una regla aplica una condición a los resultados de una o más métricas
instanciadas. Un perfil reúne reglas y su evaluación es la media aritmética
simple de las evaluaciones de esas reglas; no es un índice de dimensión ni
un índice global de calidad.
}
\details{
\code{perfiles_madurez()} crea los perfiles \code{Básico}, \code{Intermedio} y \code{Avanzado}
con condiciones estrictas \verb{> 0.5}, \verb{> 0.7} y \verb{> 0.9} sobre las mismas
métricas instanciadas.
}

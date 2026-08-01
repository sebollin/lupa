% Please edit documentation in R/faltantes.R
\name{sentinelas_naniar}
\alias{sentinelas_naniar}
\title{Sentinelas numéricos publicados por naniar}
\format{
Vector numérico de ocho elementos.
}
\source{
\code{naniar::common_na_numbers}, versión 1.1.0.
}
\usage{
sentinelas_naniar
}
\description{
Vector para solicitar explícitamente la lista numérica completa publicada
por \code{naniar}. Incluye \code{66}, \code{77}, \code{88} y \code{9999}, que pueden ser edades,
códigos o años legítimos y por eso no se aplican de forma predeterminada.
Se usa como \code{perfilar(datos, sentinelas_numericos = sentinelas_naniar)}.
Tanto este vector como las cadenas de ausencia incorporadas en el paquete
están congelados con referencia a naniar 1.1.0; no cambian según la versión
de naniar instalada.
}

% Please edit documentation in R/claves-relaciones.R
\name{detectar_claves}
\alias{detectar_claves}
\title{Detectar claves candidatas}
\usage{
detectar_claves(datos, max_combinacion = 3)
}
\arguments{
\item{datos}{Objeto que hereda de \code{data.frame}.}

\item{max_combinacion}{Máximo de columnas por combinación, entre 1 y 3.}
}
\value{
Data frame de claves candidatas con las columnas combinadas,
cantidad de columnas y marcas de redundancia.
}
\description{
Busca primero claves simples y luego combinaciones mínimas de dos o tres
columnas. No prueba una combinación si ya contiene una clave candidata más
pequeña. Una clave exige ausencia de \code{NA} y unicidad en todas las filas.
}
\details{
Dos claves simples se marcan como redundantes cuando sus contenidos son
idénticos, aunque tengan nombres distintos. Los pares también quedan en el
atributo \code{claves_redundantes}.
}
\examples{
detectar_claves(data.frame(id = 1:4, grupo = c("a", "a", "b", "b")))
}

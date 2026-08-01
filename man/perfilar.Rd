% Please edit documentation in R/perfilar.R
\name{perfilar}
\alias{perfilar}
\title{Perfilar un conjunto de datos}
\usage{
perfilar(
  datos,
  nombre = deparse(substitute(datos)),
  muestra = 1e+05,
  max_patrones = 20,
  distinguir_mayusculas = TRUE,
  expandir = FALSE,
  umbral_alta_cardinalidad = 0.5,
  umbral_faltantes_sospechoso = 0.1,
  umbral_faltantes_error = 0.4,
  umbral_patron_raro = 0.05,
  umbral_patron_dominante = 0.5,
  columnas_sin_ceros = character(),
  columnas_no_negativas = character()
)
}
\arguments{
\item{datos}{Objeto que hereda de \code{data.frame}.}

\item{nombre}{Nombre descriptivo del objeto.}

\item{muestra}{Máximo de filas usadas para patrones e inferencia de tipos.}

\item{max_patrones}{Máximo de patrones mostrados por columna.}

\item{distinguir_mayusculas}{Si se distinguen mayúsculas y minúsculas.}

\item{expandir}{Si se emite un token por carácter en los patrones.}

\item{umbral_alta_cardinalidad}{Umbral para columnas categóricas.}

\item{umbral_faltantes_sospechoso}{Umbral inferior de faltantes.}

\item{umbral_faltantes_error}{Umbral a partir del cual son un error.}

\item{umbral_patron_raro}{Máxima frecuencia de un patrón raro.}

\item{umbral_patron_dominante}{Frecuencia mínima del patrón dominante.}

\item{columnas_sin_ceros}{Nombres de columnas donde cero no es admisible.}

\item{columnas_no_negativas}{Nombres de columnas que deben ser no negativas.}
}
\value{
Objeto S3 de clase \code{perfil}.
}
\description{
Examina un \code{data.frame}, \code{tibble} o \code{data.table} y devuelve estadísticas
generales, métricas por columna, patrones, formatos de fecha y hallazgos
accionables. Todas las proporciones se expresan en \verb{[0, 1]}.
}
\details{
Los umbrales de faltantes se aplican a la suma de ausentes reales y
faltantes disfrazados. Estos últimos se informan además como error propio.
La lista de representaciones se basa en \code{naniar::common_na_strings} y
\code{naniar::common_na_numbers}, con extensiones habituales en datos
administrativos uruguayos. \code{naniar} es opcional y se consulta solamente si
está instalado.
}
\examples{
perfil <- perfilar(datos_administrativos)
perfil
summary(perfil)
}

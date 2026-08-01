% Please edit documentation in R/perfilar.R
\name{perfilar}
\alias{perfilar}
\title{Perfilar un conjunto de datos}
\usage{
perfilar(
  datos,
  nombre = deparse(substitute(datos)),
  fecha = Sys.time(),
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
  columnas_no_negativas = character(),
  sentinelas_numericos = numeric()
)
}
\arguments{
\item{datos}{Objeto que hereda de \code{data.frame}.}

\item{nombre}{Nombre descriptivo del objeto.}

\item{fecha}{Fecha y hora de la corrida. Se puede fijar para construir series
reproducibles; se normaliza a UTC.}

\item{muestra}{Máximo de filas usadas para patrones e inferencia de tipos.
Use \code{Inf} para analizar todas las filas.}

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

\item{sentinelas_numericos}{Vector de sentinelas numéricos adicionales que
representan ausencia. Se combina con la lista predeterminada.}
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
faltantes disfrazados. La lista de cadenas está congelada con referencia a
\code{naniar::common_na_strings} 1.1.0 y suma extensiones habituales en datos
administrativos uruguayos. Las entradas que naniar expresa como patrones
escapados se adaptan a los signos literales de interrogación, asterisco y
punto porque aquí se comparan por igualdad. La lista no depende de la
versión instalada.
Los sentinelas numéricos predeterminados son \code{-9}, \code{-99}, \code{-999}, \code{-9999} y
\code{999}. La lista es deliberadamente más corta que
\code{naniar::common_na_numbers} 1.1.0: \code{66}, \code{77}, \code{88} y \code{9999} también pueden
ser edades, códigos o años legítimos. Para solicitar explícitamente esa
lista completa, use \code{sentinelas_numericos = sentinelas_naniar}.

\code{muestra} limita sólo el descubrimiento de patrones, la inferencia de tipos y
la detección de formatos de fecha. Las demás métricas y hallazgos se calculan
sobre todas las filas. Por eso \code{meta$filas_analizadas} describe el máximo
usado por los análisis muestreados, no el alcance del perfil completo.
}
\examples{
perfil <- perfilar(datos_administrativos)
perfil
summary(perfil)
}

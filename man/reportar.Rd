% Please edit documentation in R/reportar.R
\name{reportar}
\alias{reportar}
\title{Crear un reporte HTML autocontenido}
\usage{
reportar(
  x,
  ...,
  archivo = tempfile("reporte-lupa-", fileext = ".html"),
  sobrescribir = FALSE,
  titulo = "Reporte de calidad de datos",
  fecha = Sys.time(),
  max_filas = 100L,
  max_patrones = 20L
)
}
\arguments{
\item{x}{Un objeto compatible o una lista de objetos compatibles.}

\item{...}{Objetos adicionales de clase \code{perfil}, \code{medicion},
\code{evaluacion_calidad}, \code{historico_calidad}, \code{deriva_perfil},
\code{deriva_calidad} o \code{plan_limpieza}.}

\item{archivo}{Ruta de salida. De forma predeterminada crea un archivo en
\code{tempdir()}.}

\item{sobrescribir}{Si se permite reemplazar un archivo existente.}

\item{titulo}{Título visible del reporte.}

\item{fecha}{Fecha y hora de generación, inyectable para obtener resultados
reproducibles. Se normaliza a UTC.}

\item{max_filas}{Máximo de filas por tabla y de columnas del perfil cuyos
patrones se detallan. Las omisiones se informan dentro del reporte.}

\item{max_patrones}{Máximo de patrones mostrados por columna. Las omisiones
se informan dentro del reporte.}
}
\value{
La ruta normalizada del archivo, de forma invisible.
}
\description{
Genera un único archivo HTML en español usando sólo funciones de R base. El
estilo se incluye dentro del documento: no requiere red, navegador especial,
conversor externo ni archivos auxiliares. Cada valor dinámico se escapa antes
de incorporarlo al documento.
}
\details{
Se pueden combinar objetos producidos por el profiling, la medición, la
evaluación, el histórico, las comparaciones de deriva y la planificación de
limpieza. Cada tipo añade su sección; el reporte no modifica datos ni aplica
planes.
}
\examples{
perfil <- perfilar(datos_administrativos)
archivo <- reportar(perfil)
unlink(archivo)
}

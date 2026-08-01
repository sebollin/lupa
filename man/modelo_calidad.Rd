% Please edit documentation in R/modelo-calidad.R
\name{modelo_calidad}
\alias{modelo_calidad}
\alias{metrica}
\alias{especializar}
\alias{instanciar}
\alias{modelo}
\alias{metricas_nucleo}
\title{Construir métricas y modelos de calidad}
\usage{
metrica(
  nombre,
  semantica,
  granularidad,
  tipo_resultado,
  propiedades = character(),
  dimension = NA_character_,
  factor = NA_character_,
  metodo = NULL,
  validar_propiedades = NULL
)

especializar(metrica, nombre_especifico = NULL, ...)

instanciar(
  metrica_especifica,
  entidad,
  atributos = character(),
  nombre_instancia = NULL,
  metodo = NULL,
  referencial = NULL
)

modelo(...)

metricas_nucleo()
}
\arguments{
\item{nombre}{Nombre estable y legible.}

\item{semantica}{Descripción de lo que mide la métrica.}

\item{granularidad}{Uno de los niveles devueltos por \code{granularidades()}.}

\item{tipo_resultado}{\code{"booleano"} o \code{"real"}. Los reales deben estar en
\verb{[0, 1]}.}

\item{propiedades}{Nombres de las propiedades que fija \code{especializar()}.}

\item{dimension, factor}{Metadatos taxonómicos; no se usan para calcular
puntuaciones.}

\item{metodo}{Método predeterminado opcional. Es una función de \code{tablas} e
\code{instancia} que cumple el contrato descrito en Detalles.}

\item{validar_propiedades}{Función opcional que valida y devuelve la lista de
configuración.}

\item{metrica}{Objeto de clase \code{metrica_generica}.}

\item{nombre_especifico}{Nombre de la especialización. Si se omite, conserva
el nombre genérico.}

\item{...}{En \code{especializar()}, propiedades con nombre. En \code{modelo()},
métricas instanciadas o una única lista que las contenga.}

\item{metrica_especifica}{Objeto de clase \code{metrica_especifica}.}

\item{entidad}{Nombres de las tablas ligadas, en el orden que espera el
método.}

\item{atributos}{Nombres de las columnas ligadas, en el mismo orden.}

\item{nombre_instancia}{Nombre de la instancia. Si se omite, se deriva de la
especialización y los objetos ligados.}

\item{referencial}{Objeto de referencia opcional, conservado como argumento
de primera clase en la instancia.}
}
\value{
\code{metrica()} y \code{especializar()} devuelven closures S3;
\code{instanciar()} devuelve una \code{metrica_instanciada}; \code{modelo()} devuelve un
\code{modelo_calidad}; \code{metricas_nucleo()} devuelve una lista de métricas
genéricas.
}
\description{
\code{metrica()} declara una métrica genérica y devuelve una fábrica de closures.
\code{especializar()} fija sus propiedades de configuración; la closure específica
resultante puede instanciarse sobre distintas columnas o tablas.
\code{instanciar()} liga la métrica a objetos concretos y materializa el método de
medición. \code{modelo()} reúne métricas instanciadas sin calcular un índice global.
}
\details{
\code{metricas_nucleo()} devuelve las seis métricas iniciales: \code{NoNulo}, \code{Formato},
\code{ValoresPosiblesPorExtension}, \code{ReglaIntegridadIntraEntidad},
\code{ReglaIntegridadInterEntidad} y \code{ErrorEstandar}.

El método de medición devuelve un data frame con \code{resultado}, \code{entidad},
\code{atributo}, \code{fila} y \code{objeto}. Una métrica booleana produce valores lógicos;
una real produce números finitos en \verb{[0, 1]}.

En \code{ReglaIntegridadInterEntidad}, \code{entidad} y \code{atributos} se ligan como
\code{c(referencia, dependiente)} y \code{c(clave_primaria, clave_foranea)}.
\code{ErrorEstandar} informa el error estándar de la media dividido por el rango
observado, acotado a \verb{[0, 1]}; exige al menos dos valores numéricos válidos.

\code{Formato} acepta exactamente una de las propiedades \code{expresion_regular},
\code{diccionario} o \code{validador}. Esta última permite conectar validadores
externos sin incorporarlos como dependencias.

Tanto \code{Formato} como \code{ValoresPosiblesPorExtension} omiten los valores \code{NA}:
un ausente no genera una medida en esas métricas y, por lo tanto, tampoco
integra el denominador de sus agregaciones. La completitud se mide por
separado con \code{NoNulo}; así un mismo ausente no se penaliza en dos factores.

\code{tipo_resultado} es el contrato canónico que consultan las agregaciones. Las
unidades no forman parte de este núcleo porque el marco presenta ambas
nociones de forma inconsistente y sólo el tipo permite validar las fórmulas.
El argumento referencial se conserva sin transformación dentro de la instancia;
cada métrica que lo use debe declarar y validar allí su contrato específico.
}
\references{
AGESIC (2020). \emph{Marco de trabajo para la Gestión de la Calidad
de Datos en Gobierno Digital}, versión 1.6, Presidencia de la República,
Uruguay.
}
\examples{
nucleo <- metricas_nucleo()
no_nulo <- especializar(
  nucleo$NoNulo, nombre_especifico = "NoNuloEdad"
)
instancia <- instanciar(no_nulo, entidad = "personas", atributos = "edad")
modelo_calidad <- modelo(instancia)
medir(modelo_calidad, data.frame(edad = c(20, NA, 35)))

}

# lupa

`lupa` implementa un modelo de calidad de datos de uso general: dimensiones y
factores declarables, métricas con granularidad explícita, agregación tipada y
una cadena de evaluación auditable. Además examina datos tabulares, propone
métricas y planes editables, conserva la trazabilidad de las acciones y compara
entregas a lo largo del tiempo.

El usuario puede declarar su propia taxonomía con `marco_calidad()`. De fábrica,
el paquete trae los 17 factores y la correspondencia con las 49 métricas del
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno Digital
v1.6* de AGESIC. También ofrece las quince características de ISO/IEC 25012:2008
como una taxonomía opcional; ninguno de los dos marcos limita el núcleo. Sólo
`cli` es una dependencia obligatoria; el reporte HTML autocontenido se genera
con R base y no requiere navegador, LaTeX ni servicios externos.

## Instalación

Antes de la publicación, el paquete puede instalarse desde un archivo fuente
construido localmente:

```sh
R CMD build lupa
R CMD INSTALL lupa_0.1.0.tar.gz
```

Desde R también se puede instalar una ruta local:

```r
install.packages("ruta/al/archivo/lupa_0.1.0.tar.gz", repos = NULL)
```

## Recorrido mínimo

```r
library(lupa)
data(datos_operativos)

factores <- data.frame(
  dimension = "Estructura",
  factor = c("Ausencias observadas", "Duplicación exacta"),
  perfil_mide = TRUE,
  como_resolverlo = c("Revisar ausencias.", "Revisar duplicados.")
)
marco_operativo <- marco_calidad("Marco operativo", factores)

# 1. Analizar. Una llamada reúne el recorrido descriptivo y su alcance.
analisis <- analizar(datos_operativos, marco = marco_operativo)
analisis
subset(analisis$perfil$hallazgos, severidad != "ok")
analisis$cobertura

# Las distribuciones, asociaciones y propuestas siguen siendo objetos de datos.
analisis$distribuciones$cuantiles
analisis$asociaciones
analisis$variables

# 2. Proponer qué medir. La propuesta se revisa antes de materializarla.
propuesta <- analisis$propuesta_modelo
propuesta[, c("metrica", "origen", "justificacion", "incluir")]
modelo_calidad <- modelo_desde_propuesta(propuesta)

# 3. Medir y evaluar con una regla explícita.
medidas <- medir(modelo_calidad, datos_operativos)
medida_entidad <- agregar(medidas, "entidad", "ratio")
regla <- regla_evaluacion(
  "Duplicación menor al 20 %",
  function(x) x < 0.2
)
evaluacion <- evaluar(
  medida_entidad,
  perfil_evaluacion("Control operativo", regla)
)

# 4. Revisar y aplicar un plan sobre una copia de los datos.
plan <- analisis$plan_limpieza
plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
resultado <- aplicar(plan, datos_operativos)
resultado$registro

# 5. Compartir un único archivo sin recursos externos.
archivo <- reportar(analisis, medidas, evaluacion, plan)
```

Las proporciones siempre usan la escala `[0, 1]`. Las severidades forman el
factor ordenado `ok < sospechoso < error`. El diagnóstico nunca modifica los
datos y ninguna eliminación se recomienda o ejecuta sin consentimiento
adicional.

## Guías

- [Empezar con lupa](vignettes/empezar-con-lupa.Rmd): recorrido completo sobre
  los datos sintéticos incluidos.
- [El modelo de calidad](vignettes/el-modelo-de-calidad.Rmd): marcos propios,
  métricas, granularidad, agregaciones y el catálogo incluido de AGESIC.
- [Limpiar con un plan](vignettes/limpiar-con-un-plan.Rmd): decisiones,
  alternativas excluyentes, salvaguardas y trazabilidad.
- [Histórico y deriva](vignettes/historico-y-deriva.Rmd): acumular evaluaciones
  y comparar entregas.

Después de instalar el paquete, las mismas guías se abren con:

```r
vignette("empezar-con-lupa", package = "lupa")
vignette("el-modelo-de-calidad", package = "lupa")
vignette("limpiar-con-un-plan", package = "lupa")
vignette("historico-y-deriva", package = "lupa")
```

## Capacidades principales

- patrones de formato vectorizados, tipos implícitos, fechas mixtas y ausentes
  disfrazados;
- puerta de entrada integral, frecuencias acotadas, cuantiles, asociaciones y
  diagnóstico de regularidad temporal;
- escalas de medición y roles como propuestas confirmables, preservando niveles
  declarados ausentes;
- hallazgos accionables, claves, relaciones y dependencias funcionales;
- métricas genéricas, específicas e instanciadas con granularidad explícita;
- taxonomías dimensión-factor declarables y cobertura contra el marco elegido;
- marcos incluidos de AGESIC e ISO/IEC 25012 como opciones consultables;
- referenciales tabulares para correctitud semántica y cobertura;
- cuatro agregaciones tipadas y perfiles de evaluación sin índice global;
- propuesta editable del modelo y plan de limpieza auditable;
- histórico plano, deriva del modelo y comparación estructural de perfiles;
- reporte HTML autocontenido, con cobertura conceptual y protección
  predeterminada de valores personales concretos.

Un análisis completo se conserva entre sesiones con `guardar_analisis()` y
`leer_analisis()`. Por omisión no guarda la tabla de entrada y sustituye reglas
funcionales por declaraciones pequeñas para no serializar sus entornos.

`catalogo_agesic()` expone como tabla el estado de las 49 entradas del catálogo,
incluidas las métricas obtenibles por agregación, las que requieren insumos
externos y las que permanecen fuera de alcance. La columna `motivo` distingue
una semántica parcial, un insumo que debe aportar el usuario, un motor pendiente
y una decisión explícita de alcance.

`marco_iso25012()` devuelve una adaptación operativa de las quince
características de ISO/IEC 25012:2008. Sus tres perspectivas se representan
como dimensiones y las características como factores; esa forma sirve a la API
de `lupa` y no pretende convertir la norma en una jerarquía que no declara.

El núcleo no presupone país ni organismo. Para datos uruguayos, el catálogo de
AGESIC, sus especializaciones documentadas y los puntos de extensión para
validadores y referenciales nacionales quedan disponibles como una instancia de
referencia, no como una restricción para otros usuarios.

## Referencia conceptual

Batini C, Scannapieco M (2016). *Data and Information Quality: Dimensions,
Principles and Techniques*. Springer.

AGESIC (2020). *Marco de trabajo para la Gestión de la Calidad de Datos en
Gobierno Digital*, versión 1.6. Presidencia de la República, Uruguay, con la
Facultad de Ingeniería de la Universidad de la República.

ISO/IEC (2008). *ISO/IEC 25012:2008 Software engineering — Software product
Quality Requirements and Evaluation (SQuaRE) — Data quality model*.

# lupa

[![Licencia:
GPL-3](https://img.shields.io/badge/licencia-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![R-CMD-check](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sebollin/lupa/actions/workflows/R-CMD-check.yaml)
[![Estado del repositorio:
activo](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![README in
English](https://img.shields.io/badge/README-English-1f6feb.svg)](https://sebollin.github.io/lupa/README.md)

`lupa` es un conjunto de herramientas auditables para perfilar datos
tabulares, definir qué significa calidad para un uso concreto, medirla,
limpiar una copia sin cambiar la entrada en silencio y encontrar
duplicados aproximados a escala. Cada resultado declara su alcance,
evidencia e incertidumbre.

La API pública, la ayuda y las viñetas están en español. Los nombres son
estables y se pueden copiar de esta guía; el [README en
inglés](https://sebollin.github.io/lupa/README.md) cuenta lo mismo para
lectores que prefieren ese idioma. La internacionalización está en el
núcleo, no en la interfaz: traducir los nombres públicos rompería
código, pruebas y viñetas. Por eso el contrato se mantiene en español,
mientras la documentación que lo rodea puede leerse en inglés.

## 🌎 Idioma de la API

Los nombres públicos son españoles tanto en los ejemplos como en la
ayuda:

| API en español | Significado en inglés |
|----|----|
| [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) | profile |
| [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) | analyse |
| [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md) | quality framework |
| [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | plan a cleanup |
| [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md) | guide a cleanup |
| [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | apply a selected cleanup |
| [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) / [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md) / [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md) | measure / dashboard / evaluate |
| [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md) | find approximate duplicates |
| [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md) | create a report |

## ✨ Qué hace lupa

- Perfila una entrega y muestra faltantes, tipos, patrones, fechas y
  evidencia de datos personales.
- Perfila geometrías `sf` y declara CRS, familias geométricas, vacíos,
  validez planar, dominio de coordenadas y alcance de la caja
  envolvente; no hace análisis espacial.
- Evalúa la ley de Benford sólo cuando se cumplen sus precondiciones y
  registra los casos no aplicables en `cobertura_diagnosticos`.
- Informa `unidades_mixtas` y `monedas_mixtas` en una columna sin
  convertir valores ni suponer tasas de cambio.
- Informa `celdas_multivaluadas` sólo cuando las partes homogéneas
  coinciden con los patrones de la columna.
- Encuentra `relacion_aritmetica_columnas` como identidades o
  proporciones observadas entre columnas numéricas, no como reglas del
  dominio.
- Encuentra `relacion_orden_columnas` entre columnas comparables y
  declara el alcance de la comparación.
- Encuentra claves, relaciones, dependencias y granularidades de
  medición que nunca fueron declaradas.
- Informa `casi_clave` cuando una columna no temporal tiene al menos 100
  filas, es casi única y sus colisiones se concentran en pocos valores,
  que es lo que separa una clave con duplicados de un texto libre de
  alta cardinalidad.
- Permite que cada proyecto defina su marco de calidad, sin imponer un
  puntaje global.
- Mide y evalúa métricas, escalas, reglas de validez y dominios
  referenciales explícitos.
- Produce planes de limpieza editables, aplica sólo acciones elegidas
  sobre una copia y conserva un registro de auditoría.
- Encuentra duplicados aproximados con teselas exactas, MinHash/LSH
  determinista, bloqueo, estimación previa del costo y lotes en disco.
- Repara codificación dañada en R, incluido mojibake repetido y CESU-8,
  y rechaza conversiones con pérdida inseguras.
- Sigue la calidad en el tiempo y crea informes HTML autocontenidos.

## 📦 Instalación

Hasta la primera publicación en CRAN, instalá la versión de desarrollo
directamente desde GitHub:

``` r

# install.packages("pak")
pak::pak("sebollin/lupa")
```

Para un clon local o un tarball de la versión publicada, usá:

``` sh
R CMD build .
R CMD INSTALL lupa_0.1.0.tar.gz
```

También se puede instalar un tarball local desde R:

``` r

install.packages("lupa_0.1.0.tar.gz", repos = NULL)
```

## ⚡ Inicio en cinco minutos

``` r

library(lupa)
data(datos_operativos)

perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
head(perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")], 5)

analisis <- analizar(datos_operativos)
analisis$tablero
archivo <- tempfile(fileext = ".html")
reportar(analisis, archivo = archivo)
stopifnot(file.exists(archivo))
unlink(archivo)
```

[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
mide por omisión todas las métricas listas de su propuesta no
confirmada, las agrega enseguida y conserva el tablero pequeño en vez
del detalle fila a fila. `medir_propuesta = FALSE` apaga ese paso y
`conservar_detalle_medicion = TRUE` conserva el detalle cuando se
necesita.

El perfilado es de sólo lectura: nunca cambia la tabla de entrada. Los
hallazgos son data frames inspeccionables y la evidencia de datos
personales se enmascara cuando la clasificación lo justifica. Abajo se
ve una salida real de consola.

![Salida capturada de perfilar()](reference/figures/perfil-console.png)

Salida capturada de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)

## 🧭 Qué se puede hacer con lupa

La [referencia de pkgdown](https://sebollin.github.io/lupa/reference/) y
las viñetas enlazadas son el manual detallado. Esta tabla es el mapa
breve:

| Tarea | Funciones principales | Para leer más |
|----|----|----|
| Mirar los datos por primera vez | [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md), [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md), [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md), [`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md), [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md), [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md), [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md), [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md), `sentinelas_naniar` | [Empezar con lupa](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Perfilar contra una base | [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md) — agregados SQL de toda la tabla y un perfil de 99 campos sobre una muestra declarada; los alcances quedan separados | [Referencia](https://sebollin.github.io/lupa/reference/) |
| Encontrar estructura no declarada | [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md), [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md), [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md), [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md), [`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md) | [Empezar con lupa](https://sebollin.github.io/lupa/articles/empezar-con-lupa.html) |
| Definir la calidad | [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md), [`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md), [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md), [`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md), [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md), [`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md), [`perfiles_madurez()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md), [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md) | [Modelo de calidad](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Medir y evaluar | [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md), [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md), [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md) con pesos del proyecto, [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md), [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md) con la instrucción `desenlace = "suprimir"` declarada por quien usa el paquete (no un umbral de fábrica), [`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md), [`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md), [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md), [`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md) | [Modelo de calidad](https://sebollin.github.io/lupa/articles/el-modelo-de-calidad.html) |
| Limpiar sin romper nada | [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md), [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md), [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | [Plan de limpieza](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.html) |
| Encontrar duplicados aproximados | [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md), [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md) | [Escala y duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.html) |
| Reparar codificación dañada | `reparar_codificacion` mediante [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) y [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) | [Referencia de limpieza](https://sebollin.github.io/lupa/reference/planificar_limpieza.html) |
| Seguir la calidad en el tiempo | [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md), [`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md), [`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md), [`leer_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md), [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md), [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md), [`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md) | [Histórico y deriva](https://sebollin.github.io/lupa/articles/historico-y-deriva.html) |
| Compartir resultados | [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md), [`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md), [`leer_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md) | [Referencia de informes](https://sebollin.github.io/lupa/reference/reportar.html) |
| Validar y extender | [`validadores_internacionales()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`validadores_uruguay()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md), [`validar_ci_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md), [`validar_rut_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md), [`validar_luhn()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_mod97()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_iso3166()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_iso4217()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_correo()`](https://sebollin.github.io/lupa/reference/validadores_formato.md), [`validar_url()`](https://sebollin.github.io/lupa/reference/validadores_formato.md) | [Referencia](https://sebollin.github.io/lupa/reference/) |

``` r

library(lupa)
data(datos_operativos)
marco <- marco_calidad(
  "Marco operativo",
  list(Estructura = c("Ausencias observadas", "Duplicacion exacta"))
)
propuesta <- proponer_modelo(perfilar(datos_operativos,
                                      analizar_dependencias = FALSE))
list(marco = marco, propuesta = propuesta)
```

La API tiene algunos límites importantes. No hay un puntaje de fábrica:
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
devuelve el tablero mientras el proyecto no declare pesos nombrados
completos, y todo índice calculado conserva cobertura, pesos,
transformaciones y universos heterogéneos. El núcleo es universal y los
catálogos son enchufables;
[AGESIC](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
v1.6 es una implementación de referencia, no un límite nacional. El
único import obligatorio es
[`cli`](https://cran.r-project.org/package=cli). Los paquetes sugeridos
habilitan estas capacidades:
[`sf`](https://cran.r-project.org/package=sf) habilita el perfilado de
geometrías; [`DBI`](https://cran.r-project.org/package=DBI) aporta la
interfaz de bases y
[`RSQLite`](https://cran.r-project.org/package=RSQLite) un backend para
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
[`stringdist`](https://cran.r-project.org/package=stringdist) es
opcional para comparar texto por proximidad.

El trabajo que se puede paralelizar usa **dos hilos por defecto**, que
es el tope que CRAN pide respetar. En su propia máquina puede subirlo,
por llamada con `nucleos = 8` o para toda la sesión con
`options(lupa.nucleos = 8)`; el resultado no cambia, sólo cuánto tarda.

## 🔍 Dónde se ubica

[`skimr`](https://cran.r-project.org/package=skimr) y
[`DataExplorer`](https://cran.r-project.org/package=DataExplorer)
exploran; [`pointblank`](https://cran.r-project.org/package=pointblank),
[`validate`](https://cran.r-project.org/package=validate) y
[`dataquieR`](https://cran.r-project.org/package=dataquieR) expresan o
evalúan reglas;
[`zoomerjoin`](https://cran.r-project.org/package=zoomerjoin),
[`textreuse`](https://cran.r-project.org/package=textreuse) y
[`reclin2`](https://cran.r-project.org/package=reclin2) se enfocan en
comparar texto o enlazar registros.
[`calidad`](https://github.com/inesscc/calidad), mantenido por [Klaus
Lehmann](https://github.com/Klauslehmann) y [Ricardo
Pizarro](https://github.com/ricardoflopiza), es un eje complementario:
evalúa la calidad de **estimaciones de encuestas**, mientras `lupa`
evalúa los datos tabulares que producen una estimación.

La reparación de codificación sigue en R el enfoque y los datos
congelados de [`ftfy`](https://github.com/rspeer/python-ftfy) 6.3.1, de
[Robyn Speer](https://github.com/rspeer). Incluye once tablas de bytes,
CESU-8, el caso Java `C0 80` y cinco extensiones deliberadas
documentadas en [NEWS](https://sebollin.github.io/lupa/NEWS.md).
Reproduce 159 de los 161 casos del corpus distribuido y deja intactos
los 31 casos negativos. Deliberadamente no ofrece los pasos de estilo de
`fix_text` de [`ftfy`](https://github.com/rspeer/python-ftfy), como
deshacer HTML, curvar comillas, normalizar ancho o normalizar Unicode:
cambiar datos legítimos en silencio no es reparar.

## 📖 Cita y referencias

``` r

citation("lupa")
```

Las referencias conceptuales son [Batini y Scannapieco
(2016)](https://doi.org/10.1007/978-3-319-24106-7), el [Marco de trabajo
de AGESIC para la Gestión de la Calidad de Datos en Gobierno Digital
v1.6](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/)
y [ISO/IEC 25012:2008](https://www.iso.org/standard/35736.html).

## 🤝 Contribuir y reportar

Usá el [issue tracker](https://github.com/sebollin/lupa/issues) para
errores, propuestas y correcciones de documentación. Los contratos
estables son las unidades declaradas, el alcance, la protección y el
registro de auditoría; los detalles de implementación y los tiempos de
benchmark pueden cambiar entre versiones mientras esos contratos sigan
siendo ciertos.

## 📄 Licencia

`lupa` se distribuye bajo la
[GPL-3](https://www.gnu.org/licenses/gpl-3.0.html). Consultá
[`LICENSE.note`](https://sebollin.github.io/lupa/LICENSE.note) por los
datos Apache-2.0 derivados de
[`ftfy`](https://github.com/rspeer/python-ftfy) y los datos MIT
derivados de [`naniar`](https://github.com/njtierney/naniar).

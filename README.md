# lupa

<img src="man/figures/lupa.png" align="right" width="240" alt="lupa logo" />

[![License: GPL (>= 2)](https://img.shields.io/badge/license-GPL--2%20%7C%20GPL--3-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![README en español](https://img.shields.io/badge/README-español-2e7d32.svg)](https://github.com/sebollin/lupa/blob/main/README.es.md)

<code>lupa</code> is an auditable R toolkit for profiling tabular data, defining
what quality means for a particular use, measuring it, and improving a copy of
the data without hiding what changed. It also searches for approximate duplicate
records at scale, estimates the work before starting, and reports the limits of
what was compared.

## Language notice

The public API, help pages, and vignettes are written in Spanish. The English
README is a guide to the same API; the names are stable and can be copied as
they are:

| Spanish API | English meaning |
| --- | --- |
| <code>perfilar()</code> | profile |
| <code>analizar()</code> | analyse |
| <code>marco_calidad()</code> | quality framework |
| <code>planificar_limpieza()</code> | plan a cleanup |
| <code>guiar_limpieza()</code> | guide a cleanup |
| <code>aplicar()</code> | apply a selected cleanup |
| <code>medir()</code> | measure |
| <code>evaluar()</code> | evaluate |
| <code>detectar_duplicados_aproximados()</code> | find approximate duplicates |
| <code>reportar()</code> | create a report |

The [Spanish README](https://github.com/sebollin/lupa/blob/main/README.es.md) and <code>vignette()</code> provide the full
narrative in Spanish. The code examples below are deliberately small and
executable.

## Install locally

Until the first public release, build and install the source package locally:

~~~sh
R CMD build lupa
R CMD INSTALL lupa_0.1.0.tar.gz
~~~

Or install a local source tarball from R:

~~~r
install.packages("lupa_0.1.0.tar.gz", repos = NULL)
~~~

## A five-minute start

~~~r
library(lupa)
data(datos_operativos)

analisis <- analizar(datos_operativos)
analisis$perfil$hallazgos

archivo <- reportar(analisis, archivo = tempfile(fileext = ".html"))
unlink(archivo)
~~~

<code>analizar()</code> is a read-only diagnostic. It does not turn an
observation into a requirement, and it never changes the input table. The
returned objects are inspectable data frames and lists, so a user can filter
findings or send selected tables to a downstream process.

## What can I do with lupa?

### Look at a delivery for the first time

<code>perfilar()</code> is the small entry point; <code>analizar()</code> adds
distributions, associations, temporal diagnostics, proposed scales, and
coverage. Supporting functions include <code>distribucion_valores()</code>,
<code>detectar_asociaciones()</code>, <code>analizar_tiempo()</code>,
<code>clasificar_variables()</code>, <code>inferir_tipo()</code>,
<code>descubrir_patrones()</code>, <code>detectar_formatos_fecha()</code>, and
the <code>sentinelas_naniar</code> catalogue.

~~~r
data(datos_operativos)
perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
analisis <- analizar(datos_operativos)

list(
  valores = distribucion_valores(datos_operativos),
  asociaciones = detectar_asociaciones(datos_operativos, umbral = 0.3,
                                       max_pares = 10),
  tiempo = analizar_tiempo(datos_operativos),
  variables = clasificar_variables(datos_operativos),
  tipo = inferir_tipo(datos_operativos$cedula),
  patrones = descubrir_patrones(datos_operativos$contacto),
  fechas = detectar_formatos_fecha(datos_operativos$fecha_evento),
  sentinelas = perfilar(
    datos_operativos,
    sentinelas_numericos = sentinelas_naniar,
    analizar_dependencias = FALSE
  )$columnas
)
~~~

Every proportion is in [0, 1]. Personal-data evidence is protected by default:
concrete modes, examples, quantiles, and ranges are masked when the
classification is sufficiently strong. A suppression is marked in the output;
it is never silent.

### Find structure nobody declared

<code>detectar_claves()</code> looks for candidate keys,
<code>detectar_relaciones()</code> compares two table domains, and
<code>detectar_dependencias()</code> reports functional regularities with their
support. <code>granularidades()</code> and
<code>transiciones_granularidad()</code> make allowed measurement levels
explicit.

~~~r
data(datos_operativos)
claves <- detectar_claves(datos_operativos)
relaciones <- detectar_relaciones(datos_operativos, datos_operativos, muestra = 1000)
dependencias <- detectar_dependencias(datos_operativos, min_observaciones = 10)

list(
  claves = claves,
  relaciones = relaciones,
  dependencias = dependencias,
  niveles = granularidades(),
  transiciones = transiciones_granularidad()
)
~~~

These are observations about a delivery, not proof of a business rule. A
regularity can be confirmed later in a quality model or left as a finding.

### Define quality for these data

<code>marco_calidad()</code> accepts a user-owned dimension/factor taxonomy.
<code>marco_agesic()</code> and <code>marco_iso25012()</code> are inspectable
alternatives; <code>catalogo_agesic()</code> exposes the 49 catalogue entries
and their implementation status. The model vocabulary is built with
<code>metrica()</code>, <code>especializar()</code>, <code>instanciar()</code>,
and <code>modelo()</code>. <code>metricas_nucleo()</code> and
<code>metricas_referencial()</code> provide reusable definitions, while
<code>proponer_modelo()</code> and <code>modelo_desde_propuesta()</code> keep a
proposal editable. <code>perfiles_madurez()</code> and
<code>cobertura_analisis()</code> show what was measured and what was not.

~~~r
data(datos_operativos)
marco_propio <- marco_calidad(
  "Marco operativo",
  list(Estructura = c("Ausencias observadas", "Duplicación exacta"))
)
marco_agesic()
marco_iso25012()
catalogo_agesic()

nucleo <- metricas_nucleo()
no_nulo <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato")
instancia <- instanciar(no_nulo, "entrega", "dato")
modelo_calidad <- modelo(instancia)

perfil <- perfilar(datos_operativos, analizar_dependencias = FALSE)
propuesta <- proponer_modelo(perfil)
modelo_confirmado <- modelo_desde_propuesta(propuesta)
cobertura_analisis(perfil, modelo = marco_propio)
~~~

The framework is a contract, not a score generator. <code>lupa</code> deliberately
has no global quality score: averaging dimensions would hide priorities, units,
and uncertainty that the user has not specified.

### Measure and evaluate

<code>medir()</code> runs a confirmed model; <code>agregar()</code> moves results
only through a declared transition. <code>regla_evaluacion()</code> and
<code>perfil_evaluacion()</code> express conditions, and <code>evaluar()</code>
applies them. <code>escala()</code> and <code>vigencia()</code> declare
measurement contracts; <code>referencial()</code> records an external complete
domain.

~~~r
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
medidas <- medir(
  modelo(instancia),
  data.frame(dato = c("A", NA, "C")),
  id_medicion = "entrega-001",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
medida_entidad <- agregar(medidas, "atributo", "ratio")
regla <- regla_evaluacion("Completitud mayor al 60 %", function(x) x > 0.6)
evaluacion <- evaluar(medida_entidad, perfil_evaluacion("Operativo", regla))

list(
  evaluacion = evaluacion,
  escala = escala(error = 0.1),
  vigencia = vigencia("fecha_actualizacion"),
  dominio = referencial(
    data.frame(codigo = c("01", "02"), nombre = c("A", "B")),
    clave = "codigo", valor = "nombre", completo = TRUE,
    alcance = "dominio del ejemplo"
  )
)
~~~

The result is an evaluation of explicit rules, not a second hidden measurement
and not a global index.

### Clean without breaking anything

<code>planificar_limpieza()</code> proposes actions from a profile;
<code>guiar_limpieza()</code> is an optional interactive layer; and
<code>aplicar()</code> executes only the rows selected in the plan. The plan is
an editable data frame, the input is copied, and the result contains a
registry of changes. Destructive operations require an additional explicit
permission.

~~~r
data(datos_administrativos)
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
plan <- planificar_limpieza(perfil, datos_administrativos)

plan$aplicar[] <- FALSE
seleccion <- which(plan$recomendada)[1]
if (length(seleccion) == 1L && !is.na(seleccion)) {
  plan$aplicar[seleccion] <- TRUE
  plan$decision_grupo[seleccion] <- "elegida"
}
copia <- datos_administrativos
resultado <- aplicar(plan, datos_administrativos)
stopifnot(identical(datos_administrativos, copia))
resultado$registro[, c("estrategia", "n_cambiadas")]
~~~

The package returns cleaned data in <code>resultado$datos</code> and leaves the
input untouched. A second profile can verify the effect.

### Find approximate duplicates at scale

<code>detectar_duplicados_aproximados()</code> has an exact tiled strategy below
its configured limit and a deterministic MinHash/LSH strategy above it.
The comparison can be blocked with the user-supplied <code>bloquear_por</code>
key, or split into disk-backed lots with <code>lotes = TRUE</code>; both
choices declare their retained or lost scope. <code>estimar_costo()</code> is
the deliberate pre-flight check. <code>nucleos</code> controls the number of
<code>stringdist</code> threads (two by default); changing it changes time,
never pairs or findings. <code>stringdist</code> is optional: if it is not
installed, the result explains that no comparison was made.

~~~r
if (requireNamespace("stringdist", quietly = TRUE)) {
  datos <- data.frame(
    nombre = c("Ana Perez", "Ana Peres", "Luis Silva", "Luis Silva"),
    domicilio = c("Calle 1", "Calle 1", "Ruta 5", "Ruta 5"),
    anio = c(2022, 2022, 2021, 2021),
    stringsAsFactors = FALSE
  )

  estimacion <- estimar_costo(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    lsh_muestra_estimacion = 10, nucleos = 2
  )
  exacto <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    max_resultados = 10, nucleos = 2
  )
  lsh <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
    max_resultados = 10, nucleos = 2
  )
  bloqueado <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    bloquear_por = "anio", max_resultados = 10, nucleos = 2
  )
  directorio <- tempfile("lupa-lotes-")
  por_lotes <- detectar_duplicados_aproximados(
    datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
    lotes = TRUE, tamano_lote = 2, directorio_lotes = directorio,
    max_resultados = 10, nucleos = 2
  )
  unlink(directorio, recursive = TRUE)
  list(estimacion = estimacion, exacto = exacto$pares,
       lsh = lsh$pares, bloqueo = bloqueado$alcance,
       lotes = por_lotes$lotes)
}
~~~

An approximate pair is never presented as identity and the package never
suggests deleting or merging records. The optional time notice is a condition
<code>lupa_tiempo_lsh</code> in an interactive session, and
<code>resultado$estimacion</code> is the non-deterministic measurement object.
Its time is a floor for the <code>stringdist</code> comparison stage only, not a
promise for the complete run.

The scale vignette records the measured machine and thread control. On an Intel
Core i9-14900HX, 32 cores, Pop!_OS 22.04 LTS (Linux), R 4.6.1, a 100,000-row
difficult register with 140,097,499 candidates gave these medians from three
isolated runs:

| <code>nucleos</code> | median seconds | relative to 2 |
| ---: | ---: | ---: |
| 2 | 133.28 | 1.00x |
| 4 | 97.44 | 0.73x |
| 8 | 76.19 | 0.57x |
| 16 | 70.31 | 0.53x |
| 31 | 71.69 | 0.54x |

Past sixteen threads there was no measured gain. The default of two is a
deliberately conservative choice for shared machines; it can be raised when
the local workload justifies it. Thread count does not change the result.
The deterministic scale controls are:

| rows | LSH candidates | recall of exhaustive ceiling | threads |
| ---: | ---: | ---: | ---: |
| 20,000 | 6,201,626 | 1.0000 | 2 |
| 100,000 | 140,097,499 | 1.0000 | 2 |
| 200,000 | 582,388,482 | 1.0000 | 2 |

These are reference measurements, not portable timing promises. Candidate
counts, estimates, and recall are deterministic; machine time depends on the
processor, effective threads, and bucket shape.

### Follow quality through time

<code>historico_calidad()</code> and <code>acumular_historico()</code> make a
versioned flat series; <code>guardar_historico()</code> and
<code>leer_historico()</code> persist it with RDS. <code>detectar_deriva_calidad()</code>
finds changes in the series, while <code>comparar_perfiles()</code> and
<code>comparar_evaluaciones()</code> distinguish structural from evaluative
change.

~~~r
nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
modelo_calidad <- modelo(instancia)
enero <- agregar(
  medir(modelo_calidad, data.frame(dato = c("A", NA, "C", NA)),
        id_medicion = "enero", fecha = as.POSIXct("2026-01-31", tz = "UTC")),
  "atributo", "ratio"
)
febrero <- agregar(
  medir(modelo_calidad, data.frame(dato = c("A", "B", "C", NA)),
        id_medicion = "febrero", fecha = as.POSIXct("2026-02-28", tz = "UTC")),
  "atributo", "ratio"
)
regla <- regla_evaluacion("Completitud mayor al 60 %", function(x) x > 0.6)
perfil_e <- perfil_evaluacion("Operativo", regla)
enero_eval <- evaluar(enero, perfil_e)
febrero_eval <- evaluar(febrero, perfil_e)
historico <- historico_calidad(enero_eval, febrero_eval)
detectar_deriva_calidad(historico)

archivo <- tempfile(fileext = ".rds")
guardar_historico(historico, archivo)
recuperado <- leer_historico(archivo)
stopifnot(identical(historico, recuperado))
unlink(archivo)
~~~

### Share the result safely

<code>reportar()</code> creates one self-contained HTML document with escaped
dynamic values and no network references. <code>guardar_analisis()</code> and
<code>leer_analisis()</code> keep an analysis between sessions; by default they
do not serialize the input table.

~~~r
data(datos_operativos)
analisis <- analizar(datos_operativos)
archivo_rds <- tempfile(fileext = ".rds")
guardar_analisis(analisis, archivo_rds)
analisis_recuperado <- leer_analisis(archivo_rds)

archivo_html <- reportar(
  analisis, archivo = tempfile(fileext = ".html"),
  titulo = "Operational data quality"
)
stopifnot(file.exists(archivo_html), file.exists(archivo_rds))
unlink(c(archivo_html, archivo_rds))
~~~

Protection in a report is independent of the object-level switch: a report
re-masks personal evidence before it circulates. Protection is not a substitute
for access control over the input data.

### Validators and extension packs

<code>validadores_internacionales()</code> provides ISO 3166, ISO 4217, e-mail,
Luhn, and modulo-97 validators. <code>validadores_uruguay()</code> adds
Uruguayan identity and RUT rules. The scalar/vector interfaces are also
exported as <code>validar_ci_uy()</code>, <code>validar_rut_uy()</code>,
<code>validar_luhn()</code>, <code>validar_mod97()</code>,
<code>validar_iso3166()</code>, <code>validar_iso4217()</code>, and
<code>validar_correo()</code>.

~~~r
internacionales <- validadores_internacionales()
uruguay <- validadores_uruguay()
internacionales$iso4217(c("UYU", "CLP", "ZZZ"))
uruguay$cedula(c("1.234.567-2", "1.234.567-3"))
validar_iso3166(c("UY", "XX"))
validar_correo(c("persona@example.org", "no-es-correo"))

pack_ejemplo <- pack_validadores(
  "Ejemplo", list(codigo = function(x) !is.na(x) & x == "OK"), pais = "CL",
  descripcion = "Validator owned by the consuming project."
)
pack_ejemplo$codigo(c("OK", "otro"))
~~~

Packs are plain named functions and are not registered globally. A project can
pass one to <code>perfilar(validadores_personales = ...)</code> or attach it to
the <code>Formato</code> metric. AGESIC v1.6 is the reference catalogue, not a
country lock.

## The design in four commitments

* **No global score.** A single number would hide dimensions, priorities, units,
  and uncertainty. Results stay in their declared units and rules.
* **Universal core, pluggable catalogues.** Users declare a taxonomy with
  <code>marco_calidad()</code>. AGESIC v1.6 and ISO/IEC 25012 are inspectable
  instances; <code>pack_validadores()</code> adds another country or domain
  without changing the engine.
* **One required dependency.** <code>cli</code> is the only package in
  <code>Imports</code>; <code>stringdist</code> is optional in
  <code>Suggests</code> for approximate duplicates.
* **No knowledge is overstated.** Partial samples, structural losses,
  unmeasured counts, and time floors are named in the object. Unknown counts are
  <code>NA</code>, not zero; a measured machine and thread count are reported
  with the measurement.

## Guides and reference

After installation, open the executable vignettes with:

~~~r
vignette("empezar-con-lupa", package = "lupa")
vignette("el-modelo-de-calidad", package = "lupa")
vignette("limpiar-con-un-plan", package = "lupa")
vignette("historico-y-deriva", package = "lupa")
vignette("escala-y-duplicados", package = "lupa")
~~~

The package is intentionally positioned alongside, rather than against, other
tools: <code>skimr</code> and <code>DataExplorer</code> summarize and explore;
<code>pointblank</code>, <code>validate</code>, and <code>dataquieR</code>
express or evaluate rules; <code>zoomerjoin</code>, <code>textreuse</code>, and
<code>reclin2</code> cover text comparison or record linkage.
[<code>calidad</code>](https://github.com/inesscc/calidad), maintained by Klaus
Lehmann and Ricardo Pizarro, is a valuable adjacent axis: it implements CEPAL
criteria for the quality of **survey estimates** (Estudios Estadísticos 101),
while <code>lupa</code> evaluates the tabular data that produces an estimate.

To cite the package and its AGESIC reference, use:

~~~r
citation("lupa")
~~~

Conceptual references include Batini and Scannapieco (2016), AGESIC's *Marco
de trabajo para la Gestión de la Calidad de Datos en Gobierno Digital* v1.6,
and ISO/IEC 25012:2008. The repository's issue tracker is the place for bugs
and proposals; the release is not yet on CRAN, so this README intentionally
does not display CRAN or R-CMD-check badges.

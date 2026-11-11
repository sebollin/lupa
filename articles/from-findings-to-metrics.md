# From findings to metrics

> This guide is the English version of [7. Del hallazgo a la
> métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.md).
> `lupa` writes its messages and tables in Spanish, so the output below
> is in Spanish.

The profile of `datos_operativos` says that the column `canal`
(*channel*) has missing values. They are disguised: one is `"S/D"` and
another, an empty text. How does that finding become a measurement that
repeats with each delivery and is compared with a requirement?

> **In this guide:**
>
> - going from the findings to a proposal of metrics with
>   [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md);
> - deciding what is measured and measuring it with
>   [`medir()`](https://sebollin.github.io/lupa/reference/medir.md);
> - aggregating the measures of each cell up to the column with
>   [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md);
> - evaluating the aggregated measure against a threshold with
>   [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md);
> - seeing which part of a quality framework was covered with
>   [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).

## The starting finding

The table is profiled and the findings of the profile in `canal` are
inspected. The date is fixed so that the example always gives the same
result.

``` r

library(lupa)

perfil <- perfilar(datos_operativos, fecha = "2026-01-15")
encontrados <- hallazgos(perfil)
encontrados[encontrados$columna %in% "canal", c("tipo_hallazgo", "severidad")]
#>                  tipo_hallazgo  severidad
#> 8                    faltantes sospechoso
#> 9        faltantes_disfrazados      error
#> 10          espacios_sobrantes sospechoso
#> 11   mayusculas_inconsistentes sospechoso
#> 19 casi_duplicados_vocabulario sospechoso
```

The one with severity `error` is `faltantes_disfrazados` (*disguised
missing values*). Its evidence says which ones they are:

``` r

disfrazados <- encontrados$columna %in% "canal" &
  encontrados$tipo_hallazgo == "faltantes_disfrazados"
writeLines(encontrados$evidencia[disfrazados])
#> <blanco> (1); S/D (1)
```

A finding describes this delivery. To know whether the next one is
better or worse, and whether it reaches the required level, a metric is
needed.

## From the findings to a proposal of metrics

A metric is a named way of measuring. It is bound to a table and to one
or more columns, and it returns a result for each object it measures.
`NoNulo` (*not null*), the completeness metric, returns a 1 for each
cell with a value and a 0 for each empty cell. What counts as empty is
part of its configuration: besides `NA`, the values declared to it in
`valores_nulos` (*null values*).

`lupa` does not decide what to measure: it proposes.
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
(*propose a model*) goes through the findings and returns an editable
table, with one row per suggested metric and the finding it came from.
It measures nothing.

``` r

propuesta <- proponer_modelo(perfil, datos_operativos)
propuesta[, c("metrica", "atributos", "origen", "incluir")]
#>                        metrica      atributos                             origen incluir
#> 1             EntidadDuplicada                         hallazgo:filas_duplicadas    TRUE
#> 2                       NoNulo          canal                 hallazgo:faltantes    TRUE
#> 3                       NoNulo codigo_usuario     hallazgo:faltantes_disfrazados    TRUE
#> 4                       NoNulo   fecha_evento     hallazgo:faltantes_disfrazados    TRUE
#> 5                       NoNulo          monto     hallazgo:faltantes_disfrazados    TRUE
#> 6                      Formato       contacto perfil:patron_dominante:a+9+@a+.a+   FALSE
#> 7                      Formato      id_evento       perfil:patron_dominante:A+9+   FALSE
#> 8                      Formato        sistema         perfil:patron_dominante:a+   FALSE
#> 9                      Formato           zona        perfil:patron_dominante:Aa+   FALSE
#> 10 ValoresPosiblesPorExtension           zona           perfil:dominio_observado   FALSE
```

The column `incluir` (*include*) says which rows stay active. `lupa`
marks them like this:

- it activates the ones that come from an observed defect: the missing
  values of 4 columns and the duplicated rows;
- it leaves off the ones that come from a habit of this delivery, such
  as a format pattern or the domain of `zona` (*zone*).

A single delivery does not show that those habits are requirements. And
measuring them on the same delivery they came from would say little.

The row of `canal` shows the step from the finding to the metric. The
values that the profile recognized as disguised missing values move into
the configuration of `NoNulo`:

``` r

fila_canal <- which(propuesta$id_sugerencia == "NoNulo:canal")
writeLines(strwrap(propuesta$justificacion[fila_canal], 88))
#> La columna contiene 2 ausentes totales, incluidos 2 disfrazados. Se normalizan: sus
#> valores se declaran en `valores_nulos` y la métrica los cuenta como faltantes.
propuesta$configuracion[[fila_canal]]
#> $valores_nulos
#> [1] ""    "S/D"
```

## Deciding what is measured

The proposal is edited: the decision belongs to whoever uses it. Here
the interest is completeness: only the 4 rows of `NoNulo` stay active,
and the one for duplicates is turned off.
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
(*model from proposal*) turns the active rows into an operational model.
It is the list of metrics, already bound to their columns, that
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
(*measure*) is going to run. It does not measure either.

``` r

propuesta$incluir <- propuesta$metrica == "NoNulo"
propuesta[propuesta$incluir, c("metrica", "atributos")]
#>   metrica      atributos
#> 2  NoNulo          canal
#> 3  NoNulo codigo_usuario
#> 4  NoNulo   fecha_evento
#> 5  NoNulo          monto
modelo_completitud <- modelo_desde_propuesta(propuesta)
```

## Measuring

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) takes
the model first and the data after. The identifier and the date of the
run stay in each measure; with them, deliveries are compared later.

``` r

medicion <- medir(
  modelo_completitud, datos_operativos,
  id_medicion = "entrega-enero",
  fecha = "2026-01-15"
)
nrow(medicion)
#> [1] 52
medicion[medicion$atributo == "canal", c("objeto_medible", "resultado")]
#>                objeto_medible resultado
#> 1   datos_operativos$canal[1]         1
#> 2   datos_operativos$canal[2]         1
#> 3   datos_operativos$canal[3]         1
#> 4   datos_operativos$canal[4]         1
#> 5   datos_operativos$canal[5]         0
#> 6   datos_operativos$canal[6]         1
#> 7   datos_operativos$canal[7]         1
#> 8   datos_operativos$canal[8]         1
#> 9   datos_operativos$canal[9]         1
#> 10 datos_operativos$canal[10]         1
#> 11 datos_operativos$canal[11]         1
#> 12 datos_operativos$canal[12]         1
#> 13 datos_operativos$canal[13]         0
```

Each row is a measure: a result for an object, here a cell. The 4
columns by 13 rows give 52 measures. In `canal`, rows 5 and 13 are 0.
They are the `"S/D"` and the empty text, which `NoNulo` counted as
missing because the configuration asked it to.

## Aggregating up to the column

One measure per cell does not yet answer the underlying question: how
complete each column is. For that, the measures are aggregated. The
vocabulary of the levels is the one of the quality framework, and
[`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
(*granularities*) translates it into the one of tables:

``` r

granularidades()[c(1, 2, 4, 5), c("granularidad", "relacional")]
#>        granularidad relacional
#> 1 instanciaAtributo      celda
#> 2          atributo    columna
#> 4  instanciaEntidad      tupla
#> 5           entidad      tabla
```

In a table:

- an attribute instance is a cell;
- an attribute, a column;
- an entity instance, a row (a tuple);
- an entity, a table.

These terms are also in [the
glossary](https://sebollin.github.io/lupa/articles/leer-un-perfil.html#glosario)
(in Spanish). From the cell to the column the step is `ratio`, which
computes the proportion of ones:

``` r

completitud <- agregar(medicion, "atributo", "ratio")
completitud[, c("atributo", "resultado", "agregacion")]
#>         atributo resultado agregacion
#> 1          canal 0.8461538      ratio
#> 2 codigo_usuario 0.9230769      ratio
#> 3   fecha_evento 0.9230769      ratio
#> 4          monto 0.9230769      ratio
```

[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
(*aggregate*) combines measures of a single metric and a single run. It
only follows the steps that
[`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md)
(*granularity transitions*) declares:

- from the cell to the column or to the row;
- from the column or the row to the table;
- from the table upwards.

A cell can be aggregated towards its column or towards its row: they are
2 different directions, not rungs of a single ladder. The available
aggregations are:

- `ratio`, for boolean results;
- `ratio_umbral` (*threshold ratio*), for real results compared with a
  threshold;
- `promedio` (*mean*);
- `promedio_ponderado` (*weighted mean*), with weights in `[0, 1]` that
  add up to 1 per target.

Unbounded results keep their unit and do not enter these aggregations.
They are, for example, the deviation of `ErrorEstandar` (*standard
error*) or a staleness measured in days.

## Evaluating against a threshold

A rule expresses a condition on results. An evaluation profile gathers
rules. That profile is not the object
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
returns: it is the second argument of
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
(*evaluate*). Each column is required to be at least 90% complete:

``` r

regla <- regla_evaluacion(
  "Completitud de al menos 90 %",
  function(x) x >= 0.9
)
publicable <- perfil_evaluacion("Publicable", regla)
evaluacion <- evaluar(completitud, publicable)
evaluacion$perfiles[, c("perfil", "n_reglas", "resultado")]
#>       perfil n_reglas resultado
#> 1 Publicable        1      0.75
```

The rule was applied to each of the 4 aggregated measures. The result of
a rule is the proportion of measures that meet it. The result of a
profile is the simple mean of its rules. These are the ones that met it:

``` r

cumple <- evaluacion$medidas$resultado[
  match(completitud$id_medida, evaluacion$medidas$id_medida)
]
data.frame(
  columna = completitud$atributo,
  completitud = round(completitud$resultado, 3),
  cumple = cumple
)
#>          columna completitud cumple
#> 1          canal       0.846  FALSE
#> 2 codigo_usuario       0.923   TRUE
#> 3   fecha_evento       0.923   TRUE
#> 4          monto       0.923   TRUE
```

Every column reaches 90% except `canal`, which has 0.846: the result is
3 out of 4. The rule really compares the completeness of each column
with its threshold. Another threshold gives another result:

``` r

resultado_con_umbral <- function(umbral, medidas) {
  regla <- regla_evaluacion("Completitud", function(x) x >= umbral)
  evaluar(medidas, perfil_evaluacion("Publicable", regla))$perfiles$resultado
}
umbrales <- c(0.80, 0.90, 0.95)
data.frame(
  umbral = umbrales,
  agregada = vapply(umbrales, resultado_con_umbral, numeric(1), completitud),
  por_celda = vapply(umbrales, resultado_con_umbral, numeric(1), medicion)
)
#>   umbral agregada por_celda
#> 1   0.80     1.00 0.9038462
#> 2   0.90     0.75 0.9038462
#> 3   0.95     0.00 0.9038462
```

With the aggregated measure, the result follows the threshold:

- with 80%, all 4 columns meet it;
- with 90%, 3 meet it;
- with 95%, none.

The column `por_celda` (*per cell*) shows what happens if the
measurement is evaluated without aggregating. The rule asks each cell
whether its 0 or its 1 reaches the threshold. The result is the
proportion of cells with a value, 0.904, whatever the threshold. That is
why the measures are aggregated before evaluating.

The evaluation applies a condition to measures that already exist. It
does not read `datos_operativos` again or replace the measurement.

## 🔎 What was covered

The absence of findings does not prove that a table is fine: maybe
nobody measured what matters.
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
(*analysis coverage*) crosses the profile and the measurement with a
quality framework. It says, factor by factor, what was measured. The
third argument is called `modelo` (*model*), but it takes a framework
such as the one
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
(*quality framework*) creates. By default, the AGESIC one.

``` r

cobertura <- cobertura_analisis(perfil, medicion, modelo = marco_agesic())
cobertura[, c("dimension", "factor", "estado")]
#>       dimension                        factor           estado
#> 1     Exactitud         Correctitud semántica     no_declarada
#> 2     Exactitud        Correctitud sintáctica     no_declarada
#> 3     Exactitud                     Precisión     no_declarada
#> 4     Exactitud Exactitud posicional absoluta        no_aplica
#> 5     Exactitud Exactitud posicional relativa        no_aplica
#> 6     Exactitud                     Fidelidad fuera_de_alcance
#> 7  Consistencia      Integridad inter-entidad     no_declarada
#> 8  Consistencia      Integridad intra-entidad     no_declarada
#> 9  Consistencia         Integridad de dominio     no_declarada
#> 10 Consistencia       Consistencia topológica        no_aplica
#> 11  Completitud                     Cobertura     no_declarada
#> 12  Completitud                      Densidad           medida
#> 13  Completitud                      Comisión        no_aplica
#> 14     Unicidad                No-duplicación           medida
#> 15     Unicidad              No-contradicción     no_declarada
#> 16     Frescura                    Actualidad     no_declarada
#> 17     Frescura                   Oportunidad     no_declarada
```

`estado` (*status*) tells these cases apart:

- `medida` (*measured*);
- `no_declarada` (*not declared*), when nobody asked to measure it;
- `no_aplica` (*does not apply*), when it does not fit the types of the
  table, like positional accuracy when there are no geometries;
- `fuera_de_alcance` (*out of scope*), when `lupa` cannot measure it and
  says so.

Each row explains why:

``` r

medidos <- cobertura[cobertura$estado == "medida", ]
writeLines(paste0(medidos$factor, ": ", medidos$motivo))
#> Densidad: La corrida contiene al menos una métrica instanciada para este factor.
#> No-duplicación: El perfil examinó duplicación de valores, columnas y filas exactas.
```

Density was backed by the measurement; non-duplication, by what the
profile examines on its own. What is missing carries how to solve it:

``` r

sintaxis <- cobertura$factor == "Correctitud sintáctica"
writeLines(cobertura$como_resolverlo[sintaxis])
#> Aplicar `especializar()` a `metricas_nucleo()$Formato` con `expresion_regular`, o bien `diccionario`, o bien `validador`; luego `instanciar()` sobre un atributo; después `medir()`.
```

## Summary

- [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
  turns the findings into an editable proposal: the disguised missing
  values the profile found move into the configuration of `NoNulo`.
- The column `incluir` is decided by whoever uses the proposal; what was
  learned from a single delivery arrives turned off.
- [`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
  returns one measure per object, with the `id_medicion` and the date of
  the run.
- Aggregate before evaluating: `agregar(medicion, "atributo", "ratio")`
  gives the completeness of each column, and the rule compares that
  number with its threshold.
- [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  says which factors of the framework were measured and how to cover the
  missing ones.

## 🧭 Next step

[8. Marcos y modelos de
calidad](https://sebollin.github.io/lupa/articles/marcos-y-modelos.md)
(*Quality frameworks and models*, in Spanish) declares a framework of
its own and builds custom metrics. Among them is the one that the row of
syntactic correctness asks for.

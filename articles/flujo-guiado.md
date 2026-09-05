# Flujo guiado: del perfil a la evaluación

Esta es la ruta cuando ya se tiene una tabla y se quiere decidir qué
medir. El perfil descriptivo y el perfil de evaluación son objetos
distintos: el primero sale de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
el segundo se construye con
[`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md).

## 1. Perfilar

``` r

library(lupa)

datos <- data.frame(
  id = 1:4,
  edad = c(20, NA, 35, 40),
  zona = c("Norte", "Norte", "Sur", "Sur"),
  stringsAsFactors = FALSE
)

perfil <- perfilar(datos, analizar_dependencias = FALSE)
perfil$hallazgos[, c("columna", "tipo_hallazgo", "severidad")]
#>   columna tipo_hallazgo  severidad
#> 1    edad     faltantes sospechoso
```

El perfil responde qué señales aparecen, pero no decide por sí solo qué
requisito debe quedar vigente.

## 2. Proponer y editar

[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
convierte señales del perfil en una tabla editable. La columna `incluir`
es la decisión que se confirma o se rechaza antes de medir.

``` r

propuesta <- proponer_modelo(perfil, datos)
propuesta[, c("metrica", "origen", "incluir", "estado")]
#>                       metrica                      origen incluir estado
#> 1                      NoNulo          hallazgo:faltantes    TRUE  lista
#> 2                     Formato perfil:patron_dominante:Aa+   FALSE  lista
#> 3 ValoresPosiblesPorExtension    perfil:dominio_observado   FALSE  lista
```

En este ejemplo se confirma sólo la métrica de no nulidad; las demás
propuestas, si aparecen, quedan fuera explícitamente.

``` r

propuesta$incluir <- propuesta$metrica == "NoNulo"
stopifnot(any(propuesta$incluir))
```

## 3. Materializar y medir

[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
no vuelve a perfilar ni mide: materializa las filas activas en un modelo
operativo. Ese modelo va primero en
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) y los
datos van segundos.

``` r

modelo_operativo <- modelo_desde_propuesta(propuesta)
medicion <- medir(
  modelo_operativo, datos,
  id_medicion = "ejemplo-guiado"
)
medicion[, c("metrica", "metrica_instanciada", "resultado")]
#>   metrica metrica_instanciada resultado
#> 1  NoNulo     sugerencia-0001         1
#> 2  NoNulo     sugerencia-0001         0
#> 3  NoNulo     sugerencia-0001         1
#> 4  NoNulo     sugerencia-0001         1
```

## 4. Evaluar

Se declara una regla y se la reúne en un perfil de evaluación. Ese
perfil no es el objeto que devuelve
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
es el segundo argumento de
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md).

``` r

regla <- regla_evaluacion(
  "Completitud minima",
  function(x) x >= 0.75
)
perfil_para_evaluar <- perfil_evaluacion("Publicable", regla)
evaluacion <- evaluar(medicion, perfil_para_evaluar)
evaluacion$perfiles
#>      id_medicion               fecha     perfil n_reglas resultado
#> 1 ejemplo-guiado 2026-09-05 04:45:07 Publicable        1      0.75
```

La evaluación aplica una condición a las medidas existentes. No vuelve a
leer `datos` ni reemplaza la medición.

## 5. Consultar la cobertura

Si además se quiere ver cómo queda el recorrido frente a una taxonomía,
el orden de
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
es `perfil`, `medicion`, `modelo`, donde el tercer objeto es el marco
conceptual creado por
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).

``` r

cobertura <- cobertura_analisis(perfil, medicion, marco_agesic())
cobertura[, c("marco", "dimension", "factor", "estado")]
#>                                  marco    dimension                        factor
#> 1  Marco de calidad de datos de AGESIC    Exactitud         Correctitud semántica
#> 2  Marco de calidad de datos de AGESIC    Exactitud        Correctitud sintáctica
#> 3  Marco de calidad de datos de AGESIC    Exactitud                     Precisión
#> 4  Marco de calidad de datos de AGESIC    Exactitud Exactitud posicional absoluta
#> 5  Marco de calidad de datos de AGESIC    Exactitud Exactitud posicional relativa
#> 6  Marco de calidad de datos de AGESIC    Exactitud                     Fidelidad
#> 7  Marco de calidad de datos de AGESIC Consistencia      Integridad inter-entidad
#> 8  Marco de calidad de datos de AGESIC Consistencia      Integridad intra-entidad
#> 9  Marco de calidad de datos de AGESIC Consistencia         Integridad de dominio
#> 10 Marco de calidad de datos de AGESIC Consistencia       Consistencia topológica
#> 11 Marco de calidad de datos de AGESIC  Completitud                     Cobertura
#> 12 Marco de calidad de datos de AGESIC  Completitud                      Densidad
#> 13 Marco de calidad de datos de AGESIC  Completitud                      Comisión
#> 14 Marco de calidad de datos de AGESIC     Unicidad                No-duplicación
#> 15 Marco de calidad de datos de AGESIC     Unicidad              No-contradicción
#> 16 Marco de calidad de datos de AGESIC     Frescura                    Actualidad
#> 17 Marco de calidad de datos de AGESIC     Frescura                   Oportunidad
#>              estado
#> 1      no_declarada
#> 2      no_declarada
#> 3      no_declarada
#> 4         no_aplica
#> 5         no_aplica
#> 6  fuera_de_alcance
#> 7      no_declarada
#> 8      no_declarada
#> 9      no_declarada
#> 10        no_aplica
#> 11     no_declarada
#> 12           medida
#> 13        no_aplica
#> 14           medida
#> 15     no_declarada
#> 16        no_aplica
#> 17        no_aplica
```

La tabla permite distinguir una medición ejecutada, un factor no
declarado y un factor fuera de alcance. La ausencia de un hallazgo no se
interpreta como una certificación de calidad.

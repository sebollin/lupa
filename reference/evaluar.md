# Evaluar medidas, reglas y perfiles

Ejecuta la cadena formal: condición por medida, proporción de medidas
que cumplen cada regla y media aritmética simple, no ponderada, de las
reglas del perfil. Los resultados por regla se conservan en `reglas`; el
resumen de `perfiles` no sustituye esa distribución.

## Usage

``` r
evaluar(medicion, perfil)
```

## Arguments

- medicion:

  **Primer argumento.** Data frame producido por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md); puede
  reunir varias corridas si conserva sus `id_medicion`. No es el
  `perfil` descriptivo que devuelve
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- perfil:

  **Segundo argumento.** Objeto creado por
  [`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md),
  que reúne las reglas que se aplican a la medición. Es un perfil de
  evaluación, distinto del objeto `perfil` creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

## Value

Objeto `evaluacion_calidad` con tres data frames filtrables: `medidas`,
`reglas` y `perfiles`. Si alguna regla declara un desenlace, contiene
además `desenlaces`, un plan que identifica las medidas incumplidas, el
valor medido, el motivo y la regla que lo produjo. Cuando una métrica no
pudo medirse, conserva `cobertura_metricas` y deja en `NA` el resumen
afectado, en lugar de tratar la ausencia como éxito. Conserva además, en
atributos, la configuración del modelo, la aplicabilidad y el perfil de
evaluación que produjo el resultado.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo)
instancia <- instanciar(especifica, "personas", "edad")
medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
regla <- regla_evaluacion("Al menos 90%", function(x) x > 0.9)
evaluar(medidas, perfil_evaluacion("Avanzado", regla))
#> $medidas
#>                                     id_medida
#> 1 medicion-20260904T152409.780907-7226-000001
#> 2 medicion-20260904T152409.780907-7226-000002
#> 3 medicion-20260904T152409.780907-7226-000003
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260904T152409.780907-7226 2026-09-04 15:24:09 Avanzado
#> 2 medicion-20260904T152409.780907-7226 2026-09-04 15:24:09 Avanzado
#> 3 medicion-20260904T152409.780907-7226 2026-09-04 15:24:09 Avanzado
#>          regla  metrica_instanciada orientacion resultado
#> 1 Al menos 90% NoNulo@personas.edad conformidad      TRUE
#> 2 Al menos 90% NoNulo@personas.edad conformidad     FALSE
#> 3 Al menos 90% NoNulo@personas.edad conformidad      TRUE
#> 
#> $reglas
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260904T152409.780907-7226 2026-09-04 15:24:09 Avanzado
#>          regla n_medidas resultado
#> 1 Al menos 90%         3 0.6666667
#> 
#> $perfiles
#>                            id_medicion               fecha   perfil n_reglas
#> 1 medicion-20260904T152409.780907-7226 2026-09-04 15:24:09 Avanzado        1
#>   resultado
#> 1 0.6666667
#> 
#> attr(,"configuracion_modelo")
#> attr(,"configuracion_modelo")$version
#> [1] 1
#> 
#> attr(,"configuracion_modelo")$entidades
#> [1] "personas"
#> 
#> attr(,"configuracion_modelo")$metricas
#>                                                                                                                                                                                                      NoNulo@personas.edad 
#> "list{nombre=character[NoNulo@personas.edad];metrica=character[NoNulo];metrica_especifica=character[NoNulo];entidad=character[personas];atributos=character[edad];configuracion=list{valores_nulos=NULL;aplicable=NULL}}" 
#> 
#> attr(,"configuracion_aplicabilidad")
#> [1] "NULL"
#> attr(,"configuracion_perfil")
#> attr(,"configuracion_perfil")$version
#> [1] 1
#> 
#> attr(,"configuracion_perfil")$nombre
#> [1] "Avanzado"
#> 
#> attr(,"configuracion_perfil")$reglas
#>                                                                                                                                                     Al menos 90% 
#> "list{nombre=character[Al menos 90%];metricas=NULL;nivel=character[medida];proporcion_minima=NULL;desenlace=NULL;umbrales=NULL;condicion=function (x)  x > 0.9}" 
#> 
#> attr(,"perfil_evaluacion")
#> $nombre
#> [1] "Avanzado"
#> 
#> $reglas
#> $reglas$`Al menos 90%`
#> $nombre
#> [1] "Al menos 90%"
#> 
#> $condicion
#> function (x) 
#> x > 0.9
#> <environment: 0x559bce3c8f60>
#> 
#> $metricas
#> NULL
#> 
#> attr(,"class")
#> [1] "regla_evaluacion"
#> 
#> 
#> attr(,"class")
#> [1] "perfil_evaluacion"
#> attr(,"class")
#> [1] "evaluacion_calidad"
```

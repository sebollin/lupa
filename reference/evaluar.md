# Evaluar medidas, reglas y perfiles

Ejecuta la cadena formal: condición por medida, proporción de medidas
que cumplen cada regla y media aritmética simple de las reglas del
perfil.

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
valor medido, el motivo y la regla que lo produjo.

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
#> 1 medicion-20260830T174637.496950-7712-000001
#> 2 medicion-20260830T174637.496950-7712-000002
#> 3 medicion-20260830T174637.496950-7712-000003
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260830T174637.496950-7712 2026-08-30 17:46:37 Avanzado
#> 2 medicion-20260830T174637.496950-7712 2026-08-30 17:46:37 Avanzado
#> 3 medicion-20260830T174637.496950-7712 2026-08-30 17:46:37 Avanzado
#>          regla  metrica_instanciada orientacion resultado
#> 1 Al menos 90% NoNulo@personas.edad conformidad      TRUE
#> 2 Al menos 90% NoNulo@personas.edad conformidad     FALSE
#> 3 Al menos 90% NoNulo@personas.edad conformidad      TRUE
#> 
#> $reglas
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260830T174637.496950-7712 2026-08-30 17:46:37 Avanzado
#>          regla n_medidas resultado
#> 1 Al menos 90%         3 0.6666667
#> 
#> $perfiles
#>                            id_medicion               fecha   perfil n_reglas
#> 1 medicion-20260830T174637.496950-7712 2026-08-30 17:46:37 Avanzado        1
#>   resultado
#> 1 0.6666667
#> 
#> attr(,"class")
#> [1] "evaluacion_calidad"
```

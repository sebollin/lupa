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
#> 
#> ── Evaluación de calidad ───────────────────────────────────────────────────────
#> 
#> ── Evaluaciones de medidas ──
#> 
#>                                    id_medida
#>  medicion-20260922T034424.418313-7729-000001
#>  medicion-20260922T034424.418313-7729-000002
#>  medicion-20260922T034424.418313-7729-000003
#>                           id_medicion               fecha   perfil        regla
#>  medicion-20260922T034424.418313-7729 2026-09-22 03:44:24 Avanzado Al menos 90%
#>  medicion-20260922T034424.418313-7729 2026-09-22 03:44:24 Avanzado Al menos 90%
#>  medicion-20260922T034424.418313-7729 2026-09-22 03:44:24 Avanzado Al menos 90%
#>   metrica_instanciada orientacion resultado
#>  NoNulo@personas.edad conformidad      TRUE
#>  NoNulo@personas.edad conformidad     FALSE
#>  NoNulo@personas.edad conformidad      TRUE
#> ── Evaluaciones de reglas ──
#> 
#>                           id_medicion               fecha   perfil        regla
#>  medicion-20260922T034424.418313-7729 2026-09-22 03:44:24 Avanzado Al menos 90%
#>  n_medidas resultado
#>          3 0.6666667
#> ── Perfiles de madurez ──
#> 
#>                           id_medicion               fecha   perfil n_reglas
#>  medicion-20260922T034424.418313-7729 2026-09-22 03:44:24 Avanzado        1
#>  resultado
#>  0.6666667
```

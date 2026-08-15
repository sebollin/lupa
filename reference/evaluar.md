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

  Data frame producido por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md). Puede
  reunir varias corridas si conserva sus `id_medicion`.

- perfil:

  Objeto creado por
  [`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md).

## Value

Objeto `evaluacion_calidad` con tres data frames filtrables: `medidas`,
`reglas` y `perfiles`.

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
#> 1 medicion-20260815T173801.262563-7585-000001
#> 2 medicion-20260815T173801.262563-7585-000002
#> 3 medicion-20260815T173801.262563-7585-000003
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260815T173801.262563-7585 2026-08-15 17:38:01 Avanzado
#> 2 medicion-20260815T173801.262563-7585 2026-08-15 17:38:01 Avanzado
#> 3 medicion-20260815T173801.262563-7585 2026-08-15 17:38:01 Avanzado
#>          regla  metrica_instanciada resultado
#> 1 Al menos 90% NoNulo@personas.edad      TRUE
#> 2 Al menos 90% NoNulo@personas.edad     FALSE
#> 3 Al menos 90% NoNulo@personas.edad      TRUE
#> 
#> $reglas
#>                            id_medicion               fecha   perfil
#> 1 medicion-20260815T173801.262563-7585 2026-08-15 17:38:01 Avanzado
#>          regla n_medidas resultado
#> 1 Al menos 90%         3 0.6666667
#> 
#> $perfiles
#>                            id_medicion               fecha   perfil n_reglas
#> 1 medicion-20260815T173801.262563-7585 2026-08-15 17:38:01 Avanzado        1
#>   resultado
#> 1 0.6666667
#> 
#> attr(,"class")
#> [1] "evaluacion_calidad"
```

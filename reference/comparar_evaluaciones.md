# Comparar evaluaciones de perfil

Calcula el cambio de `EvaluacionPerfil` entre dos corridas. Cada objeto
debe contener una sola `id_medicion`; no persiste los resultados. Para
una serie de N corridas use
[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
y
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md).

## Usage

``` r
comparar_evaluaciones(anterior, actual)
```

## Arguments

- anterior, actual:

  Objetos creados por
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md).

## Value

Data frame con resultados anterior y actual, `delta` y `comparacion`. El
delta es el que publica
[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
sobre el histórico de las dos evaluaciones, con la misma regla: queda en
`NA` cuando las corridas miden tablas distintas, cambia el marco o los
tipos de resultado, cambia la parte medida de la frontera o un resultado
no se evaluó, y `comparacion` dice por qué con la palabra de la deriva.
El orden lo dan los argumentos, no la fecha: `anterior` es siempre el
primero.

## Examples

``` r
nucleo <- metricas_nucleo()
instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
perfil <- perfil_evaluacion(
  "Completa", regla_evaluacion("Al menos 90%", function(x) x >= 0.9)
)
medir_en <- function(edades, id, fecha) {
  medir(modelo(instancia), data.frame(edad = edades), id_medicion = id,
        fecha = as.POSIXct(fecha, tz = "UTC"))
}
enero <- evaluar(medir_en(c(20, NA, 35), "enero", "2026-01-31"), perfil)
febrero <- evaluar(medir_en(c(20, 41, 35), "febrero", "2026-02-28"), perfil)
comparar_evaluaciones(enero, febrero)
#>     perfil id_medicion_anterior fecha_anterior resultado_anterior
#> 1 Completa                enero     2026-01-31          0.6666667
#>   id_medicion_actual fecha_actual resultado_actual     delta
#> 1            febrero   2026-02-28                1 0.3333333
#>                             comparacion
#> 1 Cambió el resultado de la evaluación.
```

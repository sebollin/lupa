# Agregar medidas entre granularidades

Aplica exactamente una de las cuatro agregaciones del marco: `ratio`,
`ratio_umbral`, `promedio` o `promedio_ponderado`. La transición se
valida contra el grafo de
[`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md).

## Usage

``` r
agregar(
  medidas,
  destino,
  funcion = c("ratio", "ratio_umbral", "promedio", "promedio_ponderado"),
  umbral = NULL,
  pesos = NULL
)
```

## Arguments

- medidas:

  Data frame producido por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) o una
  agregación anterior. Debe contener una sola métrica específica, una
  corrida y una granularidad.

- destino:

  Granularidad de destino.

- funcion:

  Una de `"ratio"`, `"ratio_umbral"`, `"promedio"` o
  `"promedio_ponderado"`.

- umbral:

  Umbral en `[0, 1]` requerido por `ratio_umbral`.

- pesos:

  Vector numérico requerido por `promedio_ponderado`, con una entrada
  por fila de `medidas`.

## Value

Objeto `medicion` agregado, con una fila por objeto de destino.

## Details

`ratio` sólo acepta medidas booleanas. `ratio_umbral` sólo acepta
medidas reales. Los promedios aceptan ambos tipos y siempre producen
resultado real en `[0, 1]`. Para el promedio ponderado, los pesos deben
estar en `[0, 1]` y sumar uno dentro de cada objeto de destino.

No existe una transición hacia factor, dimensión o modelo: esos campos
son taxonómicos y esta función no calcula un índice global.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo)
instancia <- instanciar(especifica, "personas", "edad")
medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
agregar(medidas, "atributo", "ratio")
#>                                               id_medida
#> 1 medicion-20260815T142919.441723-7576-agg-ratio-000001
#>                            id_medicion               fecha metrica
#> 1 medicion-20260815T142919.441723-7576 2026-08-15 14:29:19  NoNulo
#>   metrica_especifica   metrica_instanciada   dimension   factor granularidad
#> 1             NoNulo agregada:ratio:NoNulo Completitud Densidad     atributo
#>   tipo_resultado  entidad atributo fila objeto_medible resultado agregacion
#> 1           real personas     edad   NA  personas$edad 0.6666667      ratio
```

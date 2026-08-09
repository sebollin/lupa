# Agregar medidas entre granularidades

Aplica exactamente una de las cuatro agregaciones del marco: `ratio`,
`ratio_umbral`, `promedio` o `promedio_ponderado`. La transición se
valida contra el grafo de `transiciones_granularidad()`.

## Uso

``` r
agregar(
  medidas,
  destino,
  funcion = c("ratio", "ratio_umbral", "promedio", "promedio_ponderado"),
  umbral = NULL,
  pesos = NULL
)
```

## Argumentos

  - medidas:
    
    Data frame producido por `medir()` o una agregación anterior. Debe
    contener una sola métrica específica, una corrida y una
    granularidad.

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

## Valor

Objeto `medicion` agregado, con una fila por objeto de destino.

## Detalles

`ratio` sólo acepta medidas booleanas. `ratio_umbral` sólo acepta
medidas reales. Los promedios aceptan ambos tipos y siempre producen
resultado real en `[0, 1]`. Para el promedio ponderado, los pesos deben
estar en `[0, 1]` y sumar uno dentro de cada objeto de destino.

No existe una transición hacia factor, dimensión o modelo: esos campos
son taxonómicos y esta función no calcula un índice global.

## Ejemplos

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo)
instancia <- instanciar(especifica, "personas", "edad")
medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
agregar(medidas, "atributo", "ratio")
#>                                                 id_medida
#> 1 medicion-20260808T211916.722253-303753-agg-ratio-000001
#>                              id_medicion               fecha metrica
#> 1 medicion-20260808T211916.722253-303753 2026-08-08 21:19:16  NoNulo
#>   metrica_especifica   metrica_instanciada   dimension   factor granularidad
#> 1             NoNulo agregada:ratio:NoNulo Completitud Densidad     atributo
#>   tipo_resultado  entidad atributo fila objeto_medible resultado agregacion
#> 1           real personas     edad   NA  personas$edad 0.6666667      ratio
```

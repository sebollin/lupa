# Métricas que consumen un referencial tabular

Devuelve las tres métricas base que pueden medirse con el contrato de
`referencial()`. `CorrectitudSemFuerte` verifica que la identificación
exista; `CorrectitudSemDebil` comprueba el par identificación–valor;
`RatioCobertura` mide qué proporción del universo completo de claves
aparece en la entidad. Los ratios de correctitud se obtienen mediante
`agregar()` con `"ratio"`.

## Uso

``` r
metricas_referencial()
```

## Valor

Lista con tres objetos `metrica_generica`.

## Detalles

Los valores ausentes no generan medidas de correctitud: corresponden a
la dimensión Completitud. La cobertura ignora claves ausentes en el
objetivo y no permite que duplicados inflen el resultado.

## Ver también

`referencial()`, `metricas_nucleo()`, `agregar()`

## Ejemplos

``` r
ref <- referencial(
  data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla")),
  "id", "nombre", completo = TRUE, alcance = "padrón de ejemplo"
)
m <- metricas_referencial()
fuerte <- instanciar(especializar(m$CorrectitudSemFuerte),
  "personas", "id", referencial = ref)
medir(modelo(fuerte), data.frame(id = c(1, 4)))
#>                                       id_medida
#> 1 medicion-20260808T211924.462189-303753-000001
#> 2 medicion-20260808T211924.462189-303753-000002
#>                              id_medicion               fecha
#> 1 medicion-20260808T211924.462189-303753 2026-08-08 21:19:24
#> 2 medicion-20260808T211924.462189-303753 2026-08-08 21:19:24
#>                metrica   metrica_especifica              metrica_instanciada
#> 1 CorrectitudSemFuerte CorrectitudSemFuerte CorrectitudSemFuerte@personas.id
#> 2 CorrectitudSemFuerte CorrectitudSemFuerte CorrectitudSemFuerte@personas.id
#>   dimension                factor      granularidad tipo_resultado  entidad
#> 1 Exactitud Correctitud semántica instanciaAtributo       booleano personas
#> 2 Exactitud Correctitud semántica instanciaAtributo       booleano personas
#>   atributo fila objeto_medible resultado agregacion
#> 1       id    1 personas[1,id]         1       <NA>
#> 2       id    2 personas[2,id]         0       <NA>
```

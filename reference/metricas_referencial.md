# Métricas que consumen un referencial tabular

Devuelve las tres métricas base que pueden medirse con el contrato de
[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md).
`CorrectitudSemFuerte` verifica que la identificación exista;
`CorrectitudSemDebil` comprueba el par identificación–valor;
`RatioCobertura` mide qué proporción del universo completo de claves
aparece en la entidad. Los ratios de correctitud se obtienen mediante
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) con
`"ratio"`. Las tres métricas aceptan `normalizar`, `proximidad`,
`metodo`, `p`, `umbral`, `max_pares` y `nucleos`. `normalizar = NULL`
hereda el perfil declarado por
[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md).
La normalización sólo cambia la representación usada para emparejar: no
modifica los datos. La proximidad es evidencia para los valores ausentes
y nunca cambia su veredicto; si el paquete opcional
[stringdist](https://cran.r-project.org/package=stringdist) no está
instalado, se declara que no se calculó. Se calcula una sola vez por
valor fallido distinto y la evidencia se reparte a las filas repetidas.
El alcance conserva por separado `n_fallos` (filas),
`n_valores_fallidos_distintos`, `n_valores_fallidos_comparados` y los
pares comparados; así el límite no depende del orden ni de la frecuencia
de las filas.

## Usage

``` r
metricas_referencial()
```

## Value

Lista con tres objetos `metrica_generica` —`CorrectitudSemFuerte`,
`CorrectitudSemDebil` y `RatioCobertura`—, listos para instanciar contra
un `referencial`. Son definiciones: no miden hasta que se las instancia.

## Details

Los números se emparejan por su valor. El mismo número guardado como
entero en una tabla y como doble en la otra coincide —`100000L` y
`1e5`—, y dos dobles distintos no coinciden aunque se escriban igual con
15 cifras —`0.1 + 0.2` y `0.3`—. Lo mismo vale para
[`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
cuando compara filas por sus atributos, como en `EntidadDuplicada`.

Los valores ausentes no generan medidas de correctitud: corresponden a
la dimensión Completitud. La cobertura ignora claves ausentes en el
objetivo y no permite que duplicados inflen el resultado. Sobre un
referencial sin claves, `RatioCobertura` no se mide: la cobertura de un
universo vacío no es una proporción, y la métrica queda `sin_valores`
con ese motivo, igual que cuando la entidad no tiene filas.

## See also

[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md),
[`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)

## Examples

``` r
ref <- referencial(
  data.frame(id = 1:3, nombre = c("Ana", "Bruno", "Carla")),
  "id", "nombre", completo = TRUE, alcance = "padr\u00f3n de ejemplo"
)
m <- metricas_referencial()
fuerte <- instanciar(especializar(m$CorrectitudSemFuerte),
  "personas", "id", referencial = ref)
medir(modelo(fuerte), data.frame(id = c(1, 4)))
#>                                     id_medida
#> 1 medicion-20261002T015203.114435-7620-000001
#> 2 medicion-20261002T015203.114435-7620-000002
#>                            id_medicion               fecha              metrica
#> 1 medicion-20261002T015203.114435-7620 2026-10-02 01:52:03 CorrectitudSemFuerte
#> 2 medicion-20261002T015203.114435-7620 2026-10-02 01:52:03 CorrectitudSemFuerte
#>     metrica_especifica              metrica_instanciada dimension
#> 1 CorrectitudSemFuerte CorrectitudSemFuerte@personas.id Exactitud
#> 2 CorrectitudSemFuerte CorrectitudSemFuerte@personas.id Exactitud
#>                  factor orientacion      granularidad tipo_resultado  entidad
#> 1 Correctitud semántica conformidad instanciaAtributo       booleano personas
#> 2 Correctitud semántica conformidad instanciaAtributo       booleano personas
#>   atributo fila objeto_medible resultado agregacion
#> 1       id    1 personas[1,id]         1       <NA>
#> 2       id    2 personas[2,id]         0       <NA>
```

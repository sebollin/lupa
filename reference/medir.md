# Medir un modelo de calidad

Ejecuta todas las métricas instanciadas de un `modelo_calidad`. Cada
fila es una medida reutilizable y conserva el identificador y la fecha
de la corrida.

## Usage

``` r
medir(modelo, datos, id_medicion = NULL, fecha = Sys.time())
```

## Arguments

- modelo:

  Objeto creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

- datos:

  Data frame para una sola entidad o lista con nombre de data frames
  para varias entidades.

- id_medicion:

  Identificador de corrida. Si se omite, se genera uno.

- fecha:

  Fecha y hora de la corrida.

## Value

Data frame S3 de clase `medicion`, con una fila por objeto medido. Los
booleanos se almacenan como `0` y `1` en la columna común `resultado`, y
su semántica permanece declarada en `tipo_resultado`. Algunas métricas
que trabajan con un vocabulario o un alcance parcial agregan un atributo
`alcance_metricas` con sus conteos y límites.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
instancia <- instanciar(especifica, "personas", "edad")
medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#>                                     id_medida
#> 1 medicion-20260815T142931.228434-7576-000001
#> 2 medicion-20260815T142931.228434-7576-000002
#> 3 medicion-20260815T142931.228434-7576-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260815T142931.228434-7576 2026-08-15 14:29:31  NoNulo
#> 2 medicion-20260815T142931.228434-7576 2026-08-15 14:29:31  NoNulo
#> 3 medicion-20260815T142931.228434-7576 2026-08-15 14:29:31  NoNulo
#>   metrica_especifica      metrica_instanciada   dimension   factor
#> 1         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad
#> 2         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad
#> 3         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad
#>        granularidad tipo_resultado  entidad atributo fila   objeto_medible
#> 1 instanciaAtributo       booleano personas     edad    1 personas$edad[1]
#> 2 instanciaAtributo       booleano personas     edad    2 personas$edad[2]
#> 3 instanciaAtributo       booleano personas     edad    3 personas$edad[3]
#>   resultado agregacion
#> 1         1       <NA>
#> 2         0       <NA>
#> 3         1       <NA>
```

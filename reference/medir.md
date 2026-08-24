# Medir un modelo de calidad

Ejecuta todas las métricas instanciadas de un `modelo_calidad`. Cada
fila es una medida reutilizable y conserva el identificador y la fecha
de la corrida.

## Usage

``` r
medir(
  modelo,
  datos,
  id_medicion = NULL,
  fecha = Sys.time(),
  aplicabilidad = NULL
)
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

- aplicabilidad:

  Lista con nombre por columna, donde cada elemento es una fórmula que
  dice en qué filas esa columna corresponde —por ejemplo
  `list(marca_auto = ~ tiene_auto == "Si")`—. Las filas fuera de ese
  universo salen de la medición en vez de contarse como ausencia.

  Es la misma declaración que recibe
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  y hace falta porque una métrica de completitud sobre una columna
  condicionada mide lo que no corresponde: con universo de 300 filas
  sobre 1.000 y 30 vacías de verdad, sin declararlo da `0,270` y
  declarándolo `0,900`. El histórico y la deriva consumen mediciones,
  así que heredan el número que salga de acá.

  Sin declaración toda la tabla aplica y el resultado es el de siempre.

## Value

Data frame S3 de clase `medicion`, con una fila por objeto medido. Los
booleanos se almacenan como `0` y `1` en la columna común `resultado`.
`orientacion` conserva si un valor alto expresa conformidad, si un valor
alto expresa defecto o si esa lectura no aplica. Algunas métricas que
trabajan con un vocabulario o un alcance parcial agregan un atributo
`alcance_metricas` con sus conteos y límites.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
instancia <- instanciar(especifica, "personas", "edad")
medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#>                                     id_medida
#> 1 medicion-20260824T011526.497491-7624-000001
#> 2 medicion-20260824T011526.497491-7624-000002
#> 3 medicion-20260824T011526.497491-7624-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260824T011526.497491-7624 2026-08-24 01:15:26  NoNulo
#> 2 medicion-20260824T011526.497491-7624 2026-08-24 01:15:26  NoNulo
#> 3 medicion-20260824T011526.497491-7624 2026-08-24 01:15:26  NoNulo
#>   metrica_especifica      metrica_instanciada   dimension   factor orientacion
#> 1         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 2         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#> 3         NoNuloEdad NoNuloEdad@personas.edad Completitud Densidad conformidad
#>        granularidad tipo_resultado  entidad atributo fila   objeto_medible
#> 1 instanciaAtributo       booleano personas     edad    1 personas$edad[1]
#> 2 instanciaAtributo       booleano personas     edad    2 personas$edad[2]
#> 3 instanciaAtributo       booleano personas     edad    3 personas$edad[3]
#>   resultado agregacion
#> 1         1       <NA>
#> 2         0       <NA>
#> 3         1       <NA>
```

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
  aplicabilidad = NULL,
  proteger_datos_personales = TRUE
)
```

## Arguments

- modelo:

  **Primer argumento.** Objeto operativo creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md);
  reúne las métricas instanciadas que se van a ejecutar. No es el objeto
  `perfil` que devuelve
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  ni el marco conceptual de
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).

- datos:

  **Segundo argumento.** Data frame para una sola entidad o lista con
  nombre de data frames para varias entidades. Es la tabla que se mide,
  no un perfil ni un modelo.

  Con una lista, tienen que estar las tablas de **todas** las entidades
  que el modelo mide; si falta alguna se rechaza antes de medir,
  nombrando todas las que faltan y cuáles se recibieron. Una tabla que
  el modelo no pide **se ignora**: no hace falta que la lista coincida
  exactamente con las entidades, sólo que no falte ninguna.

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

- proteger_datos_personales:

  Si se enmascaran los candidatos de proximidad que corresponden a
  columnas personales. Por omisión `TRUE`.

## Value

Data frame S3 de clase `medicion`, con una fila por objeto medido. Los
booleanos se almacenan como `0` y `1` en la columna común `resultado`.
`orientacion` conserva si un valor alto expresa conformidad, si un valor
alto expresa defecto o si esa lectura no aplica. Algunas métricas que
trabajan con un vocabulario o un alcance parcial agregan un atributo
`alcance_metricas` con sus conteos y límites. Si una métrica no puede
medirse por falta de valores en su universo, no crea filas ni ceros:
deja el motivo en el atributo `cobertura_metricas`. También conserva
`configuracion_modelo` y `configuracion_aplicabilidad`, descripciones de
la política usada para que una deriva posterior pueda distinguir modelo
de datos.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
instancia <- instanciar(especifica, "personas", "edad")
medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#>                                     id_medida
#> 1 medicion-20260904T162209.724918-7435-000001
#> 2 medicion-20260904T162209.724918-7435-000002
#> 3 medicion-20260904T162209.724918-7435-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260904T162209.724918-7435 2026-09-04 16:22:09  NoNulo
#> 2 medicion-20260904T162209.724918-7435 2026-09-04 16:22:09  NoNulo
#> 3 medicion-20260904T162209.724918-7435 2026-09-04 16:22:09  NoNulo
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

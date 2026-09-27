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
  proteger_datos_personales = TRUE,
  columnas_personales = character(),
  validadores_personales = NULL
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

- columnas_personales:

  Columnas que traen datos personales, declaradas con la misma forma que
  acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
  nombres de columna, o un vector con nombre donde el nombre es la
  columna y el valor es el tipo. Se protegen aunque el léxico por
  omisión no las reconozca —un nombre propio de la organización, un
  identificador interno—. Sólo tiene efecto con
  `proteger_datos_personales = TRUE`.

- validadores_personales:

  Pack o lista nombrada de funciones de validación, con la misma forma
  que acepta
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
  Sin esto, la medición usa el léxico por omisión.

## Value

Data frame S3 de clase `medicion`, con una fila por objeto medido. Los
booleanos se almacenan como `0` y `1` en la columna común `resultado`.
`orientacion` conserva si un valor alto expresa conformidad, si un valor
alto expresa defecto o si esa lectura no aplica. Algunas métricas que
trabajan con un vocabulario o un alcance parcial agregan un atributo
`alcance_metricas` con sus conteos y límites. Si una métrica no puede
medirse —porque su universo no tiene valores, o porque su contrato no
trae un campo que necesita—, no crea filas ni ceros: deja el motivo y
cómo resolverlo en el atributo `cobertura_metricas`, con un estado que
distingue las dos causas. Y cuando **sí** pudo medirse pero sobre
**menos** elementos de los que hay en su universo aplicable —una métrica
por celda no mide la celda vacía, que no produce medida ni cuenta como
incumplimiento—, el atributo `alcance_medidas` publica cuántos midió de
cuántos, con su unidad: sin ese número, el agregado de tres celdas de
cuatro no se distingue del de cuatro. Viaja al tablero, se imprime con
la medición y se publica en el informe. También conserva
`configuracion_modelo` y `configuracion_aplicabilidad`, descripciones de
la política usada para que una deriva posterior pueda distinguir modelo
de datos. Si el modelo declara un marco, la medición conserva tambien
`marco_calidad`; la configuracion del modelo registra su nombre, sus
pares dimension-factor y el `tipo_resultado` de cada metrica.

Esa descripcion es **insensible al orden de la declaracion**: declarar
las mismas metricas, o los mismos pares del marco, en otro orden no
cambia el modelo, porque un modelo es un conjunto de instrumentos y un
marco un conjunto de pares. El orden de los `atributos` de una instancia
**sí** es parte del modelo, porque el metodo recibe las columnas en ese
orden. Y el `metodo` de medicion entra en la descripcion solo si lo
declaro quien llama: el metodo por omision de una metrica es del paquete
y su identidad ya viaja en el nombre de la metrica, de modo que un
cambio interno de lupa no se lee como un cambio del modelo del usuario.

## Examples

``` r
nucleo <- metricas_nucleo()
especifica <- especializar(nucleo$NoNulo, nombre_especifico = "NoNuloEdad")
instancia <- instanciar(especifica, "personas", "edad")
medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
#>                                     id_medida
#> 1 medicion-20260927T112546.278486-7323-000001
#> 2 medicion-20260927T112546.278486-7323-000002
#> 3 medicion-20260927T112546.278486-7323-000003
#>                            id_medicion               fecha metrica
#> 1 medicion-20260927T112546.278486-7323 2026-09-27 11:25:46  NoNulo
#> 2 medicion-20260927T112546.278486-7323 2026-09-27 11:25:46  NoNulo
#> 3 medicion-20260927T112546.278486-7323 2026-09-27 11:25:46  NoNulo
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

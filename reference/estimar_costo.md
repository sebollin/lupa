# Estimar el costo de una comparación de duplicados

Construye las firmas y calcula el pronóstico de candidatos sin recorrer
las cubetas ni comparar los pares. Es una operación deliberada: la
medición del reloj queda en el resultado de esta función y no es
necesaria para obtener `alcance` reproducible en
`detectar_duplicados_aproximados()`.

## Uso

``` r
estimar_costo(
  datos,
  columnas = NULL,
  metodo = "jw",
  umbral = 0.12,
  muestra = Inf,
  max_pares = 50000000L,
  max_resultados = 100L,
  normalizar = TRUE,
  perfil = NULL,
  proteger_datos_personales = TRUE,
  bloque = 1000L,
  estrategia = "auto",
  lsh_bandas = 12L,
  lsh_filas = 3L,
  lsh_q = 3L,
  lsh_max_cubeta = 1000L,
  lsh_muestra_estimacion = 400000L,
  presupuesto_pares = Inf,
  bloquear_por = NULL,
  lotes = FALSE,
  tamano_lote = 1000L,
  directorio_lotes = NULL,
  nucleos = getOption("lupa.nucleos", 2L)
)
```

## Argumentos

  - datos:
    
    Tabla con una fila por entidad observada.

  - columnas:
    
    Columnas atomicas a combinar. `NULL` aplica la seleccion automatica
    descrita arriba; no se incluyen matrices ni listas.

  - metodo:
    
    Medida admitida por `stringdist::stringdistmatrix()`. Por defecto,
    `"jw"`.

  - umbral:
    
    Distancia maxima para informar un par. Por defecto `0.12`.

  - muestra:
    
    Máximo de filas candidatas. En el camino exacto queda sujeto a
    `max_pares`; con LSH, `Inf` usa todas las filas.

  - max\_pares:
    
    Máximo de pares comparados en el camino exacto. Por defecto
    `50000000`, que permite recorrer exhaustivamente hasta 10.000 filas
    con el método y el bloque predeterminados; se puede reducir para
    limitar el tiempo. En LSH el alcance se expresa con candidatos y
    cubetas, por lo que este límite no se usa para recortar filas; el
    resultado lo marca explícitamente.

  - max\_resultados:
    
    Maximo de pares devueltos. Por defecto `100`.

  - normalizar:
    
    Si se recortan espacios, se pasa a minusculas y se colapsan espacios
    antes de calcular la distancia.

  - perfil:
    
    Perfil de los mismos datos para reutilizar su clasificacion de datos
    personales y no volver a inferirla.

  - proteger\_datos\_personales:
    
    Si la evidencia de columnas protegidas se reemplaza por `[valor
    protegido]`. La supresion queda indicada en cada par.

  - bloque:
    
    Cantidad de filas por tesela de comparación. Por defecto `1000`;
    controla la memoria temporal, no el número de pares comparados.

  - estrategia:
    
    Estrategia de comparación: `"auto"` (por omisión), `"teselas"`,
    `"muestra"` o `"lsh"`. MinHash/LSH sólo se activa automáticamente
    por encima del tope exhaustivo; se puede forzar con `"lsh"`.

  - lsh\_bandas:
    
    Número de bandas del esquema LSH. Por defecto, 12.

  - lsh\_filas:
    
    Número de filas de firma por banda. Por defecto, 3.

  - lsh\_q:
    
    Longitud de los q-gramas usados para MinHash. Por defecto, 3.

  - lsh\_max\_cubeta:
    
    Umbral a partir del cual una cubeta se considera grande y se procesa
    por el mismo troceo acotado del camino exhaustivo. No se descartan
    pares por este umbral; el alcance informa cuántas cubetas y cuántos
    pares se procesaron de esta forma. Por defecto, 1000.

  - lsh\_muestra\_estimacion:
    
    Cantidad máxima de pares de filas usados para estimar la proporción
    de candidatos, el tiempo del camino LSH y, si hay `bloquear_por`, la
    pérdida de candidatos del bloqueo. La muestra es interna,
    reproducible y su tamaño efectivo queda en `alcance`. Por defecto se
    intentan 400.000 pares.

  - presupuesto\_pares:
    
    Presupuesto de pares candidatos. Por defecto es `Inf`; si la
    estimación previa lo supera, una sesión no interactiva aborta antes
    del recorrido y una interactiva pregunta si se continúa. También
    limita la comparación exacta: allí el número de pares se conoce
    antes de empezar.

  - bloquear\_por:
    
    Nombre de una columna declarada por el usuario para restringir la
    comparación a filas con la misma clave. La clave no tiene
    significado incorporado en `lupa`; sus tamaños, ausentes y pares que
    quedan fuera se registran en `alcance`. Los `NA` forman un bloque
    propio.

  - lotes:
    
    Se acepta por simetría de la firma, pero la estimación no escribe
    parciales ni modifica el directorio indicado.

  - tamano\_lote:
    
    Se acepta por simetría; no cambia el pronóstico.

  - directorio\_lotes:
    
    Se acepta por simetría y no se crea ni se usa al estimar.

  - nucleos:
    
    Cantidad máxima de hilos que `stringdist` puede usar. Por defecto es
    `getOption("lupa.nucleos", 2L)`; `NULL` usa esa misma opción y un
    valor mayor que los núcleos disponibles se limita de forma segura.
    El resultado no depende de esta cantidad, pero el tiempo sí. El
    valor efectivo queda declarado en `alcance$nucleos_usados`.

## Valor

Lista de clase `estimacion_costo_lupa` con los campos de la estimación,
`alcance`, `disponible` y `razon`.

## Detalles

En el camino LSH, `candidatos_previstos` es una estimación reproducible
a partir de una muestra de firmas. `tiempo_estimado_segundos` es un piso
de la medida aislada y no incluye firmas, cubetas ni troceo; sus campos
de reloj tienen `tiempo_determinista = FALSE`. Con `bloquear_por`, los
pares entre bloques no entran en el pronóstico y la pérdida estructural
y estimada queda en `alcance`. En el camino exacto,
`candidatos_previstos` es la cantidad de pares que se compararán, sujeta
a `muestra`, `max_pares` y `bloquear_por`.

La función no escribe archivos ni modifica el estado del generador de R.

## Ver también

`detectar_duplicados_aproximados()`

## Ejemplos

``` r
datos <- data.frame(
  nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
  grupo = c("A", "A", "B")
)
if (requireNamespace("stringdist", quietly = TRUE)) {
  costo <- estimar_costo(datos, columnas = "nombre", estrategia = "lsh")
  costo$candidatos_previstos
}
#> [1] 1
```

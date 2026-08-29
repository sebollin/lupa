# Estimar el costo de una comparación de duplicados

Construye las firmas y calcula el pronóstico de candidatos sin recorrer
las cubetas ni comparar los pares. Es una operación deliberada: la
medición del reloj queda en el resultado de esta función y no es
necesaria para obtener `alcance` reproducible en
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).

## Usage

``` r
estimar_costo(
  datos,
  columnas = NULL,
  metodo = "jw",
  umbral = 0.1,
  p = 0.1,
  muestra = Inf,
  max_pares = 50000000L,
  max_resultados = 100L,
  normalizar = NULL,
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
  nucleos = getOption("lupa.nucleos", 2L),
  max_largo_valor = .MAX_LARGO_VALOR_CASI_DUPLICADOS
)
```

## Arguments

- datos:

  Tabla con una fila por entidad observada.

- columnas:

  Columnas atomicas a combinar. `NULL` aplica la seleccion automatica
  descrita arriba; no se incluyen matrices ni listas.

- metodo:

  Medida admitida por
  [`stringdist::stringdistmatrix()`](https://rdrr.io/pkg/stringdist/man/stringdist.html).
  Por defecto, `"jw"`.

- umbral:

  Distancia maxima para informar un par. Por defecto `0.10`.

- p:

  Factor de prefijo de Jaro–Winkler, entre 0 y 0.25. Por defecto `0.1`;
  sólo tiene efecto con `metodo = "jw"`.

- muestra:

  Máximo de filas candidatas. En el camino exacto queda sujeto a
  `max_pares`; con LSH, `Inf` usa todas las filas.

- max_pares:

  Máximo de pares comparados en el camino exacto. Por defecto
  `50000000`, que permite recorrer exhaustivamente hasta 10.000 filas
  con el método y el bloque predeterminados; se puede reducir para
  limitar el tiempo. En LSH el alcance se expresa con candidatos y
  cubetas, por lo que este límite no se usa para recortar filas; el
  resultado lo marca explícitamente.

- max_resultados:

  Maximo de pares devueltos. Por defecto `100`. Se conservan los mas
  cercanos; entre pares empatados en distancia, el desempate usa el
  orden canonico de los valores y no la posicion de las filas, de modo
  que reordenar la tabla no cambia que pares sobreviven. Un corte que
  cae dentro de un empate deja afuera pares igual de cercanos, y eso se
  declara en `alcance$corte_en_empate`.

- normalizar:

  Perfil de comparación. `TRUE` conserva el perfil predeterminado,
  `FALSE` desactiva sus pasos configurables, `"amplio"` activa
  puntuación, ligaduras y ancho, y
  [`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
  permite declarar cada paso. Una lista nombrada puede resolver perfiles
  por columna. `NULL` hereda el perfil guardado en `perfil`; si no se
  recibe uno, usa `TRUE`. La normalización cambia sólo la representación
  usada para comparar, no los datos guardados. El umbral se aplica sobre
  esa cadena normalizada. El informe de fusiones sólo se calcula cuando
  algún paso configurable está activo; con `FALSE` se omite. Si se
  entrega `perfil`, se reutiliza su informe ya calculado.

- perfil:

  Perfil de los mismos datos para reutilizar su clasificacion de datos
  personales y no volver a inferirla.

- proteger_datos_personales:

  Si la evidencia de columnas protegidas se reemplaza por
  `[valor protegido]`. La supresion queda indicada en cada par.

- bloque:

  Cantidad de filas por tesela de comparación. Por defecto `1000`;
  controla la memoria temporal, no el número de pares comparados.

- estrategia:

  Estrategia de comparación: `"auto"` (por omisión), `"teselas"`,
  `"muestra"` o `"lsh"`. MinHash/LSH sólo se activa automáticamente por
  encima del tope exhaustivo; se puede forzar con `"lsh"`.

- lsh_bandas:

  Número de bandas del esquema LSH. Por defecto, 12.

- lsh_filas:

  Número de filas de firma por banda. Por defecto, 3.

- lsh_q:

  Longitud de los q-gramas usados para MinHash. Por defecto, 3.

- lsh_max_cubeta:

  Umbral a partir del cual una cubeta se considera grande y se procesa
  por el mismo troceo acotado del camino exhaustivo. No se descartan
  pares por este umbral; el alcance informa cuántas cubetas y cuántos
  pares se procesaron de esta forma. Por defecto, 1000.

- lsh_muestra_estimacion:

  Cantidad máxima de pares de filas usados para estimar la proporción de
  candidatos, el tiempo del camino LSH y, si hay `bloquear_por`, la
  pérdida de candidatos del bloqueo. La muestra es interna, reproducible
  y su tamaño efectivo queda en `alcance`. Por defecto se intentan
  400.000 pares.

- presupuesto_pares:

  Se acepta por simetría de la firma y **no aborta aquí**: estimar el
  costo es justamente lo que se hace antes de fijar un presupuesto, así
  que interrumpir la estimación por superarlo dejaría sin respuesta la
  pregunta que motivó la llamada. Quien aborta es
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
  con el número que devuelve esta función.

- bloquear_por:

  Nombre de una columna declarada por el usuario para restringir la
  comparación a filas con la misma clave. La clave no tiene significado
  incorporado en `lupa`; sus tamaños, ausentes y pares que quedan fuera
  se registran en `alcance`. Los `NA` forman un bloque propio.

- lotes:

  Se acepta por simetría de la firma, pero la estimación no escribe
  parciales ni modifica el directorio indicado.

- tamano_lote:

  Se acepta por simetría; no cambia el pronóstico.

- directorio_lotes:

  Se acepta por simetría y no se crea ni se usa al estimar.

- nucleos:

  Cantidad máxima de hilos que `stringdist` puede usar. Por defecto es
  `getOption("lupa.nucleos", 2L)`; `NULL` usa esa misma opción y un
  valor mayor que los núcleos disponibles se limita de forma segura. El
  resultado no depende de esta cantidad, pero el tiempo sí. El valor
  efectivo queda declarado en `alcance$nucleos_usados`.

- max_largo_valor:

  Maximo de caracteres permitido en cada valor de las columnas
  combinadas. Por defecto es `10000`, umbral elegido porque la distancia
  normalizada deja de distinguir de forma estable una diferencia local
  de muchas diferencias en textos largos. Si una columna supera el tope,
  la combinacion completa se declara fuera de alcance en
  `alcance$columnas_excluidas_largo`; no se recortan valores en
  silencio. `Inf` recupera explicitamente el comportamiento anterior sin
  tope.

## Value

Lista de clase `estimacion_costo_lupa` con los campos de la estimación,
`alcance`, `disponible` y `razon`.

## Details

En el camino LSH, `candidatos_previstos` es una estimación reproducible
a partir de una muestra de firmas. `tiempo_estimado_segundos` es un piso
de la medida aislada y no incluye firmas, cubetas ni troceo; sus campos
de reloj tienen `tiempo_determinista = FALSE`. Con `bloquear_por`, los
pares entre bloques no entran en el pronóstico y la pérdida estructural
y estimada queda en `alcance`. En el camino exacto,
`candidatos_previstos` es la cantidad de pares que se compararán, sujeta
a `muestra`, `max_pares` y `bloquear_por`.

La función no escribe archivos ni modifica el estado del generador de R.

## See also

[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)

## Examples

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

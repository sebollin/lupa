# Detectar pares de filas con similitud aproximada

Compara filas seleccionadas de una tabla con `stringdist` y devuelve
pares cuya distancia esta bajo el umbral. El resultado describe
similitud, distancia, medida y alcance; nunca afirma que dos filas
representen la misma entidad. Por omision se combinan como maximo dos
columnas de texto o factores, despues de excluir nombres que parecen
identificadores (`id`, `codigo`, `uuid`, entre otros). Si quedan mas de
dos columnas, la funcion pide indicar `columnas` explicitamente en vez
de mezclar campos que pueden diluir la similitud. La medida
predeterminada es Jaro–Winkler (`"jw"`), con `p = 0.1`: Winkler favorece
coincidencias con el mismo prefijo y, por eso, premia una errata al
final mas que una al comienzo. El umbral predeterminado es `0.10`. Estos
dos valores cambian respecto de versiones anteriores y pueden cambiar
los pares informados; el umbral se eligio como una precision prudente,
no como una calibracion fina. Ambos argumentos se pueden cambiar.

## Usage

``` r
detectar_duplicados_aproximados(
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

  Presupuesto de pares candidatos. Por defecto es `Inf`; si la
  estimación previa lo supera, una sesión no interactiva aborta antes
  del recorrido y una interactiva pregunta si se continúa. También
  limita la comparación exacta: allí el número de pares se conoce antes
  de empezar.

- bloquear_por:

  Nombre de una columna declarada por el usuario para restringir la
  comparación a filas con la misma clave. La clave no tiene significado
  incorporado en `lupa`; sus tamaños, ausentes y pares que quedan fuera
  se registran en `alcance`. Los `NA` forman un bloque propio.

- lotes:

  Si es `TRUE`, procesa la comparación exacta por pares de grupos de
  filas y guarda cada resultado parcial en RDS. Por omisión es `FALSE`;
  el camino LSH ya administra sus cubetas en memoria y no admite este
  modo.

- tamano_lote:

  Cantidad de filas por grupo de trabajo cuando `lotes` es `TRUE`. Por
  defecto, `1000`.

- directorio_lotes:

  Directorio base elegido por el usuario para los parciales. Si es
  `NULL`, se crea un subdirectorio dentro de
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html); nunca se escribe
  en el directorio de trabajo por omisión. Los parciales no son
  reanudables y se conservan al terminar para auditoría; si el
  directorio base fue elegido por el usuario, éste debe eliminarlos
  cuando ya no los necesite. Su ruta, cantidad y tamaño quedan en
  `resultado$lotes`.

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

Lista de clase `duplicados_aproximados` con `pares`, `hallazgos`,
`alcance`, `columnas`, `metodo`, `umbral`, `disponible`, `razon` y
`estimacion`. `alcance` es reproducible; en el camino LSH,
`estimacion$tiempo_determinista` es `FALSE` y reúne la velocidad,
duración y tiempo de referencia medidos en esa corrida. El campo
`estimacion$tiempo_estimado_etapa` indica que ese piso cubre sólo la
comparación `stringdist`, no la firma, las cubetas ni el troceo. En el
camino exacto o cuando no se puede comparar, `estimacion` es `NULL`. Si
`lotes = TRUE`, se agrega `lotes` con el directorio, los archivos RDS,
sus tamaños, el estado de completitud y `reanudable = FALSE`. El loteo
cruza todos los grupos, por lo que no pierde pares; su resultado de
`pares`, `hallazgos` y `alcance` es el mismo que el recorrido exacto sin
lotes. Cada fila de `hallazgos` declara `n_evaluados`, `n_afectados` y
`unidad_conteo`; los conteos desconocidos son `NA`, nunca cero.

## Details

`tipo_par = "exacto"` indica que los textos guardados son iguales en las
columnas comparadas. `tipo_par = "exacto_normalizado"` indica que
coinciden despues de la normalizacion declarada, aunque los textos
guardados difieran. Los restantes son `"aproximado"`. La columna logica
`igualo_normalizar` es `TRUE` unicamente para el segundo caso. La
clasificacion no depende de que una medida de distancia devuelva cero:
por ejemplo, `soundex` puede dar distancia cero para textos distintos.
Ningun par demuestra identidad.

Como la normalizacion por omision iguala mayusculas, espacios, acentos y
comillas, el objeto conserva esta distincion tanto en `pares` como en
los hallazgos. `duplicados_exactos_normalizados` es un tipo de hallazgo
propio: no afirma que los valores guardados sean iguales y tampoco
oculta la causa de la coincidencia bajo `duplicados_aproximados`.

En `alcance`, `n_pares_exactos` cuenta solo textos guardados iguales,
`n_pares_exactos_normalizados` cuenta coincidencias producidas por la
normalizacion y `n_pares_aproximados` cuenta los pares restantes. Son
conteos disjuntos y explican `n_pares_hallados`.

La comparacion usa teselas de `bloque` filas: cada matriz temporal se
descarta antes de continuar, por lo que la memoria no crece con el
tamaño de la tabla. El recorrido es exacto para las filas seleccionadas.
`muestra` y `max_pares` recortan el camino exacto; en el camino LSH,
`muestra = Inf` conserva todas las filas y `max_pares` no se aplica a
las comparaciones. En ese camino `alcance$limite_pares` es `NA`,
mientras `limite_pares_configurado` conserva el valor pedido como
referencia y `limite_pares_aplica` permite distinguir ambos casos sin
inferirlo. El objeto informa cuantos pares eran posibles, cuantos se
compararon, el modo, el tamaño de las teselas o los parámetros LSH y los
que quedaron fuera. Solo se muestran `max_resultados` coincidencias; el
truncamiento tambien queda declarado.

El recorte conserva los pares mas cercanos y, entre los empatados en
distancia, desempata por el **rango canonico del valor** con una clave
simetrica. Es la misma regla que gobierna el recorte del vocabulario, y
existe por lo mismo: desempatar por posicion de fila hacia que cuales
pares sobrevivieran dependiera de como viniera ordenado el archivo.
Medido sobre 60 pares empatados con el corte en 30, cinco ordenes
distintos devolvian 30 grupos cada uno sin compartir ninguno; ahora
devuelven los mismos.

Lo que ningun orden evita es que un corte dentro de un empate deje
afuera pares igual de cercanos, asi que se declara. `alcance` publica
`distancia_corte` —la distancia donde cayo el corte—,
`n_en_distancia_corte` —cuantos de los conservados la comparten— y
`corte_en_empate`, que **no** es `truncado` con otro nombre: vale
`FALSE` cuando el corte cae en una distancia unica. Si vale `TRUE`,
subir `max_resultados` por encima del empate devuelve todos los pares de
esa distancia.

**Queda un limite que ningun orden puede sacar**: si varias filas tienen
el mismo valor en la columna comparada, sus pares empatan tambien en el
rango, y ahi el desempate cae en la posicion. Medido: con cuatro filas y
solo dos valores distintos, el conjunto de **pares de valores** es
identico en los cinco ordenes, pero **cuales instancias de fila** los
representan cambia. Es irreducible: dos filas con el mismo valor son
indistinguibles en esa columna, y su unica identidad es la posicion, que
es justamente lo que varia al reordenar. Si importa que instancia se
informa, hace falta una clave que las distinga.

En el camino LSH el conjunto de **candidatos** depende del orden de las
filas, porque el vocabulario de q-gramas se numera por orden de primera
aparicion y esa numeracion alimenta las firmas. Eso ocurre dentro de la
garantia declarada en `lsh_garantia_jaccard_*`: medido sobre 1.200
filas, de los pares que cambian al barajar ninguno supera un Jaccard de
q-gramas de 0,8, donde el recall declarado es 0,9998. Conviene saberlo
antes de comparar dos corridas sobre el mismo contenido exportado en
distinto orden.

`stringdist` es una dependencia opcional. Si no esta instalado, la
funcion devuelve un objeto con `disponible = FALSE`, una tabla vacia y
el motivo explicito; no falla ni presenta silencio como si se hubieran
comparado todos los pares.

Las comparaciones que delegan en `stringdist` usan `nucleos` hilos como
máximo. El valor por omisión es `getOption("lupa.nucleos", 2L)`, se
limita a los núcleos disponibles y se publica como
`alcance$nucleos_usados`. Cambiar la cantidad de hilos no cambia los
pares ni los hallazgos, aunque sí puede cambiar el tiempo de ejecución.
`p` es el factor de prefijo de Jaro–Winkler y admite valores entre 0 y
0.25; sólo tiene efecto cuando `metodo = "jw"`. Para los demás métodos
se valida pero `stringdist` no lo utiliza. El aviso interactivo señala
esta perilla: subir `nucleos` puede acortar la etapa de comparación,
pero la ganancia depende de la máquina y de los datos.

Para tablas que superan el tope exhaustivo, `estrategia = "auto"` usa
MinHash con bandas LSH sobre todas las filas cuando `muestra = Inf`. La
salida declara las bandas, las filas por banda, las cubetas descartadas
y la probabilidad teorica de colision. Esa probabilidad se refiere al
Jaccard de los q-gramas, no a la medida final (`metodo`).
`estrategia = "lsh"` fuerza este camino; `"teselas"` y `"muestra"`
conservan el camino exacto o muestreado de las versiones anteriores. Las
cubetas que superan `lsh_max_cubeta` se procesan igualmente por lotes;
el parámetro identifica cubetas potencialmente costosas, pero no
descarta sus pares. El alcance separa `lsh_teselas_cubetas_grandes`
(matrices del primer tramo) de `lsh_lotes_cubetas_grandes` (lotes de
filas de las bandas posteriores), pues no son la misma unidad. Los pares
aceptados por ambos recorridos entran al resumen de Jaccard. Si alguna
implementación futura descarta una cubeta, la garantía se devuelve como
`NA` y `lsh_garantia_estado` lo deja explícito. La familia MinHash usa
una permutación aleatoria inyectiva del vocabulario, con una semilla
interna fija, y restaura el estado global del generador de R; por eso es
determinista sin depender de
[`set.seed()`](https://rdrr.io/r/base/Random.html) ni de
[`RNGkind()`](https://rdrr.io/r/base/Random.html). La tabla de consulta
de cada hash evita repetir trabajo para cada celda de la matriz de
q-gramas y hace que el resultado no dependa de cómo se numeraron esos
q-gramas. Antes de recorrer las bandas se toma una muestra interna y se
publica una estimación reproducible de candidatos. El cronómetro de la
medida se ejecuta durante al menos 50 ms y queda en
`resultado$estimacion`, separado de `alcance` y marcado como no
determinista. Es un piso de la medida aislada y no estima la firma, las
cubetas ni el troceo; sólo se muestra como aviso en una sesión
interactiva. Fuera de una sesión interactiva se señala una condición
silenciosa de clase `lupa_tiempo_lsh`, sólo en el camino LSH: no se
imprime en `stdout` ni `stderr`. Se puede capturar con
`withCallingHandlers(resultado <- detectar_duplicados_aproximados(...), lupa_tiempo_lsh = function(c) { ... })`.
Como hereda de `message`, un `tryCatch(..., message = ...)` puede
interrumpir la corrida y devolver el valor del manejador; para observar
sin interrumpir use `withCallingHandlers` y, si corresponde,
`invokeRestart("muffleMessage")`. `presupuesto_pares` permite rechazar
el recorrido antes de iniciarlo; sólo se pregunta en una sesión
interactiva. El diagnóstico de Jaccard puede quedar limitado a los
primeros pares del recorrido determinista; `lsh_jaccard_alcance` lo dice
literalmente y no presenta ese prefijo como una muestra representativa.
Con `bloquear_por`, `alcance` separa los pares estructuralmente fuera
del bloqueo de una estimación de los candidatos que se habrían informado
y que quedaron fuera según una muestra determinista. La segunda cantidad
es una estimación, no una cuenta exacta.

## References

Broder, A. Z. (1997)
[doi:10.1109/SEQUEN.1997.666900](https://doi.org/10.1109/SEQUEN.1997.666900)
. On the resemblance and containment of documents. En *Compression and
Complexity of Sequences*, 21–29. Leskovec, J., Rajaraman, A. y Ullman,
J. D. (2020)
[doi:10.1017/9781108684163](https://doi.org/10.1017/9781108684163) .
*Mining of Massive Datasets* (3.ª ed.), capítulo 3.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md),
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)

## Examples

``` r
datos <- data.frame(
  nombre = c("Ana Perez", "Ana Peres", "Luis Diaz"),
  domicilio = c("Calle 1", "Calle 1", "Calle 9")
)
pares <- detectar_duplicados_aproximados(datos)
if (!pares$disponible) pares$razon
```

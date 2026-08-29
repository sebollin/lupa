# Changelog

## lupa 0.1.0

### La garantía de una clave mira el índice que la impone

`pg_constraint` dice que hay una restricción; el que impone la unicidad
es el índice que la respalda. La lectura del catálogo trae ahora
`indisunique` de ese índice, con un `LEFT JOIN` en la consulta que ya se
emitía, y si no es único la garantía baja aunque la restricción figure
validada.

No es un estado alcanzable por DDL normal —al adjuntar una partición el
motor crea solo el índice único y válido—, así que es defensa ante un
catálogo alterado. Entra igual porque cuesta una columna y porque cambia
la garantía de «el catálogo declara una restricción» a «el índice que la
impone es único».

### Ronda 154: el muestreo publica lo que se obtuvo y distingue su incertidumbre

`resumen_tabla$meta$muestreo` ahora publica juntas las filas solicitadas
y las filas obtenidas por la lectura de `perfil_muestra`.
`tamano_muestra` se conserva por compatibilidad como el tamaño efectivo
solicitado a la consulta; `filas_solicitadas` identifica el pedido
original y `filas_obtenidas` el hecho observado. Si la lectura no se
hizo o falló antes de devolver filas, el último campo queda en `NA`.

En las métricas SQL muestreadas, `error_esperado` deja de ser uniforme:
`no_estimado` indica que el error podría calcularse bajo un plan
probabilístico pero no se calculó; `no_estimable` cubre la moda, la
mediana y la cardinalidad observada, sin una cota simple o un estimador
declarado; y `no_aplica` indica que no hubo muestreo efectivo. El motivo
se conserva en cada registro junto con el método, el tamaño y la
fracción. No se agregan cotas numéricas inventadas.

### Cambios en desarrollo

- [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  deja de ejecutar agregados de datos para decidir el costo. Con una
  cardinalidad desconocida publica un rango y conserva separadas
  `estrategia_distintos` y `fuente_cardinalidad_costo`.
- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  ejecuta primero los agregados planos, luego el total exacto, los
  distintos, la moda y la mediana. `tamano_lote_planos` y
  `tamano_lote_distintos` son independientes; este último es 1 por
  omisión hasta contar con mediciones.

### Una clave heredada ya no se declara garantizada sobre otro universo

En PostgreSQL, una consulta sin `ONLY` incluye a las tablas que heredan,
y la clave primaria del padre **no gobierna las filas de los hijos**. El
catálogo sigue informando la restricción como válida, así que
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
publicaba `garantia = "garantizada"` sobre un universo donde la unicidad
puede no cumplirse: medido, una tabla con un hijo que repite un valor da
6001 valores y 6000 distintos en la consulta que ejecuta el paquete.

Ahora la misma consulta de catálogo trae si la tabla tiene descendientes
—sin agregar una ida y vuelta— y en ese caso la garantía baja a
`declarada_no_garantizada`, con el hecho anotado en
`estado$universo_incluye_descendientes`. La restricción existe y es
válida; lo que no vale es sobre las filas que se van a perfilar.

El particionado declarativo **no** pierde la garantía. `pg_inherits`
registra tanto la herencia tradicional como las particiones, y sólo la
primera deja filas fuera del alcance de la clave: el motor exige que la
clave de una tabla particionada incluya sus columnas de partición,
justamente para poder garantizarla sobre el árbol. Medido: una tabla
regular con un hijo que repite un valor da 6001 válidos y 6000
distintos; una particionada con dos particiones da 19999 y 19999. Se
distinguen por `relkind`, en la misma consulta. En PostgreSQL anterior a
la versión 10 no existe el particionado declarativo, y ahí toda
descendencia es herencia.

La misma consulta trae también si la restricción es `DEFERRABLE`. Una
clave diferible puede estar violada mientras una transacción sigue
abierta, y el catálogo la informa validada igual: medido, dentro de una
transacción que inserta un duplicado la tabla tiene 3 valores válidos y
2 distintos. Desde el paquete no se puede saber si hay una transacción
con violaciones pendientes, así que la garantía no se afirma y queda
`estado$restriccion_diferible`.

### El estado publicado queda atado a la consulta ejecutada

Los conteos aproximados sólo se consolidan cuando el adaptador entrega
una expresión que se puede incrustar en el `SELECT`. Una capacidad que
sólo construye consultas completas conserva los conteos de válidos y
distintos por separado, sin presentar `COUNT(DISTINCT ...)` como una
aproximación. Un resultado no emitido no se publica como estimado, y el
registro fuerza `no_disponible` cuando `ok` es falso.

La cota `n_distintos <= n_validos` se valida antes del único registro.
Los valores imposibles no se recortan ni se publican; el valor bruto
queda en el motivo de auditoría.

### Catalogos de claves sin confundir ausencia con falta de visibilidad

La lectura DBI ya no llama `no_declarada` a cualquier consulta de
catalogo que vuelve vacia. Una consulta fallida queda con
`garantia = "desconocida"`, `visible = NA` y su motivo; una vista que
devolvio cero filas pero puede ocultar metadatos queda con
`garantia = "desconocida"`, `visible = NA` y la ambiguedad escrita en
`motivo`. Un error de permisos identificable queda con
`visible = FALSE`; solo las vias cuyo catalogo es visible para una tabla
accesible conservan `no_declarada`.

Medido contra MariaDB 11 y MySQL 8 reales, con una cuenta propietaria y
otra con solo `SELECT`, `SHOW INDEX` devolvio las filas `PRIMARY` en las
dos cuentas para las claves simples y compuestas, y cero filas en las
tablas sin clave. MariaDB usa esa via en una sola consulta y conserva el
orden de las columnas con `Seq_in_index`; su garantia sigue desconocida
porque esa salida no publica un estado comparable de aplicacion y
validacion.

PostgreSQL deja de partir de `information_schema.table_constraints`, que
oculta restricciones a un rol que solo tiene `SELECT`, y consulta
directamente `pg_constraint`, `pg_class`, `pg_namespace` y
`pg_attribute`, conservando el orden de una clave compuesta y el estado
`convalidated`. La ruta contra un servidor PostgreSQL con una credencial
que solo tiene `SELECT` queda pendiente de verificacion en esta entrega.

SQLite pide tambien `notnull`. Su estado separa
`unicidad = "garantizada"` entre los valores no nulos de la PRIMARY KEY
de `ausencia_de_nulos`: es `"garantizada"` cuando todas sus columnas
devuelven `notnull = 1` y `"no_verificada"` en los demas casos. La
garantia conjunta queda desconocida cuando la no-nulidad no se puede
sostener. Esto subclasifica honestamente los casos especiales que pueden
garantizar no-nulidad sin que este camino distinga su declaracion, como
`INTEGER PRIMARY KEY`; no se lanza un recorrido de los datos. Se
verifico con dos `NULL` reales en una PRIMARY KEY de texto sin
`NOT NULL`, y con el rechazo de un `NULL` en otra con `NOT NULL`.

### La selección de columnas pasa por una primitiva con semántica declarada

El paquete deja de escribir `datos[, columnas, drop = FALSE]` en cada
sitio que recibe una tabla y lo hace a través de una función interna
cuya semántica de referencia está declarada, de modo que el significado
no dependa de la clase de la tabla ni del estado del espacio de nombres.
Esto corrige además selecciones de una dimensión que quedaban en
`agregacion`, `claves-relaciones`, `duplicados-aproximados`,
`referencial`, `remediacion` y `tablero-calidad`: `tabla["columna"]`
selecciona una columna en un `data.frame` y en un `tibble`, pero en un
`data.table` intenta un cruce y aborta. No agrega conversiones: sigue
habiendo una sola por llamada.

### SQL Server lee su catálogo por la vista estándar

La clasificación de visibilidad de
`information_schema.table_constraints` deja de tratar a SQL Server como
ambiguo. Medido con un rol de sólo `SELECT` sobre tablas con clave
simple, compuesta y sin clave, la vista devuelve las restricciones: la
vía es exhaustiva para esa credencial, así que un resultado vacío
significa que la clave no está declarada. Lo sostienen dos mediciones
independientes sobre dos versiones distintas del motor.

### La unicidad de una clave se evalúa entre las filas con la clave completa

`perfilar(clave = ...)` evaluaba la unicidad sobre todas las filas con
la semántica de R, donde dos ausentes de la misma posición colisionan.
Eso hacía que una clave cuyas únicas repeticiones venían de filas
incompletas se informara como **«no es única»**, cuando en SQL dos
`NULL` no son iguales y no violarían nada.

Ahora la unicidad se evalúa sólo entre las filas con la clave completa,
y la colisión entre ausentes se informa donde corresponde: en
`trazabilidad`, porque lo que deja ambiguo es el localizador.
`unicidad$semantica` pasa de `"R"` a `"claves_completas"`, y se agrega
`unicidad$filas_totales` junto a `filas_evaluadas`, que ahora cuenta las
filas evaluables.

Cuando **ninguna** fila tiene la clave completa, el estado es
`"sin_casos_evaluables"`. Decir `"verificada"` sería cierto sobre un
conjunto vacío y engañoso a la vez, y `"no_verificada"` sería falso si
se recorrió la tabla entera y se comprobó que no había casos.

### El conteo de duplicados no depende de ajustes globales de la sesión

[`data.table::setNumericRounding()`](https://rdrr.io/pkg/data.table/man/setNumericRounding.html)
es un ajuste global que cambia cuántos bits se comparan de un doble al
ordenar. Como la vía rápida ordena, con los valores 1 y 2 agrupaba
valores que [`duplicated()`](https://rdrr.io/r/base/duplicated.html)
distingue: medido sobre dobles separados por un `eps`, sobre `POSIXct`
con diferencias de microsegundos y sobre magnitudes grandes, los tres
divergían. La vía rápida ahora sólo corre con precisión completa, y sólo
consulta el ajuste si hay columnas dobles. `lupa` no lo modifica:
cambiarlo alteraría el comportamiento de código ajeno.

Se comprobó además que el número de hilos, la configuración regional, la
zona horaria, la codificación y las opciones de `data.table` no cambian
el resultado.

### Un `duplicated()` que ignora `fromLast` ya no cambia el conteo

`bit64` devuelve para una columna `integer64` el mismo vector con
`fromLast = TRUE` que sin él. La cuenta de filas en grupos repetidos
heredaba ese defecto: una tabla de 30 filas con 3 valores distintos daba
27 en vez de 30, y la respuesta cambiaba según el umbral de filas,
porque la vía rápida sí daba 30. El conteo hacia atrás se calcula ahora
dando vuelta las filas, que no depende de que el método respete el
argumento.

### La clase de la tabla ya no cambia el resultado

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) y
el resto de la API que recibe tablas dan el mismo resultado con un
`data.frame`, un `data.table` o un `tibble`. Las entradas se normalizan
en la frontera —una sola vez por llamada— y las tablas internas del
modelo de calidad también, de modo que la sintaxis de selección de
columnas significa lo mismo en todos los caminos.

Esto corrige además cinco selecciones de una dimensión que no eran
portables: `tabla["columna"]` selecciona una columna en un `data.frame`
y en un `tibble`, pero en un `data.table` intenta un cruce y aborta.
Ahora son selecciones explícitas de dos dimensiones.

### Conteo exacto de filas duplicadas sin mutar la entrada

El contador de filas duplicadas de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
obtiene los grupos de filas con
[`data.table::frank()`](https://rdrr.io/pkg/data.table/man/frank.html)
en vez de armar la estructura intermedia que combina todas las columnas,
que es lo que hace
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) sobre un
`data.frame` y lo que hace crecer su costo con el ancho de la tabla. La
entrada no se convierte ni se copia.

`data.table` se llama con `::` y **no** se importa al espacio de
nombres, a propósito: `data.table` decide la semántica de `[` según si
el paquete que llama lo tiene entre sus imports, de modo que importarlo
cambiaría el significado de `tabla[, columnas, drop = FALSE]` en toda
función que recibe una tabla de quien usa el paquete. `frank()` no
depende de esa condición;
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) sobre un
`data.table` sí, y por eso no se usa.

Las columnas de lista o matriz se detectan antes y usan la semántica de
base, y también las tablas con `NaN`: `frank()` ordena y no distingue
`NaN` de `NA`, mientras que
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) sobre un
`data.frame` sí los distingue. La dependencia pasa de `Suggests` a
`Imports`, y el resultado queda cubierto por pruebas de equivalencia
contra base sobre `data.frame`, `data.table` y `tibble`.

### Claves declaradas: unicidad, ausencias y trazabilidad separadas

`perfilar(clave = ...)` informa por separado si la combinación es única
bajo la semántica de R y si sus componentes están completos. Cada eje
queda como `verificada`, `refutada` o `no_verificada`; una ausencia no
se presenta como un duplicado SQL. Si la semántica de R agrupa dos
ausentes para localizar filas, esa colisión y la falta de valores se
informan juntas y quedan en `meta$clave`, junto a la trazabilidad. Una
clave única y completa conserva el objeto histórico sin metadatos
adicionales.

### Garantía de claves primarias según el estado del catálogo

La lectura DBI de claves primarias separa la fuente (`fuente`) de la
garantía (`garantia`) y conserva el estado consultado. Oracle lee
`STATUS` y `VALIDATED`: una restricción deshabilitada o no validada
queda declarada en el catálogo, pero no garantizada; si esos estados no
se pueden consultar, quedan desconocidos. PostgreSQL consulta `enforced`
y su estado de validación, y MySQL `enforced`; MariaDB, SQL Server,
SQLite y DuckDB no ofrecen en esta vía un estado comparable, por lo que
el resultado no inventa una garantía. Los casos Oracle se verifican con
respuestas DBI simuladas; no se afirma una prueba contra un servidor
Oracle real.

### CPU del cliente y política explícita para métricas caras

La instrumentación de DBI publica ahora `cpu_ms` junto a `duracion_ms`
en `resumen_tabla$sql` y en `resumen_tabla$tiempos`. Se calcula con
[`proc.time()`](https://rdrr.io/r/base/proc.time.html) como
`user.self + sys.self`: cerca de cero distingue espera del trabajo del
proceso cliente, y cerca de uno al dividir CPU por tiempo transcurrido
indica que el cliente está trabajando. Cero es una medición válida; `NA`
queda reservado para lo que no se pudo medir, y `instrumentar = FALSE`
deja el campo en `NA` como los demás. En un microbenchmark de un millón
de consultas simuladas, dos lecturas de
[`proc.time()`](https://rdrr.io/r/base/proc.time.html) costaron 1,122
microsegundos por consulta; dos de
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) costaron 1,778
microsegundos. En la tabla ancha de 158 columnas, el agregado no duplicó
el costo de instrumentar: cinco corridas con reloj solamente tuvieron
mediana de 1,093 s y cinco con reloj más CPU de 0,962 s, una diferencia
dominada por la variación de SQLite. El número reproducible que se
publica es el costo directo de 1,122 microsegundos por consulta.

Moda y mediana se pueden controlar con `politica_costo`. El valor por
omisión es `"todas"`, que conserva el perfil anterior;
`"por_cardinalidad"` hace primero los conteos baratos de valores válidos
y distintos y decide luego por columna. Si `n_distintos / n_validos`
alcanza `umbral_cardinalidad` (por omisión `0.95`), omite moda y mediana
de esa columna. La omisión no desaparece ni se vuelve `NA` silencioso:
queda `omitido_por_costo` con el motivo, la proporción observada y la
forma de pedirla igual (`politica_costo = "todas"`) o mover el umbral.

La política hace explícito el plan en dos etapas. En una tabla
reproducible de 158 columnas, 80 numéricas, 200 filas y 60 columnas con
cardinalidad al menos 0,95, `politica_costo = "todas"` emitió 260
consultas y la política selectiva 140: se ahorraron **120 consultas**,
no tiempo. Se omitieron 60 modas y 60 medianas. El valor por omisión no
cambia: esos 60 casos muestran el ahorro posible, pero no autorizan al
paquete a elegir qué métrica sacrificar.

Cuando la sonda reconoce un motor con
`PERCENTILE_CONT(...) WITHIN GROUP`, las medianas numéricas se
consolidan en un `SELECT` por lote; el camino actual, una mediana por
columna, queda como respaldo si la capacidad no está disponible o falla
la consulta consolidada.

### El recorrido que se pagaba sólo por contar, y qué filas vio cada métrica

El `COUNT(*)` exacto ya viaja en la primera consulta de agregados. Si el
lote completo es rechazado, `lupa` emite un `COUNT(*)` solo como
repliegue y continúa con la bisección: la completitud sigue derivando
`n_faltantes` y `prop_faltantes` de un denominador medido, nunca
estimado. El plan sigue pagando su propio conteo porque necesita conocer
el total antes de estimar el trabajo.

En una tabla en memoria de 12 filas y tres columnas, con
`tamano_lote = 4` y `bloque_muestra = "solo_agregados"`, la traza SQL
dio estos conteos. La columna de recorridos cuenta las apariciones de la
fuente en el SQL; no se usó tiempo.

| modo | consultas antes | consultas ahora | recorridos antes | recorridos ahora | ahorro de recorridos |
|----|---:|---:|---:|---:|---:|
| `exacto` | 14 | 13 | 14 | 13 | 1 |
| `seguro` | 8 | 7 | 8 | 7 | 1 |
| `conteos` | 6 | 5 | 6 | 5 | 1 |
| `muestreado` | 23 | 22 | 23 | 23 | 0 |
| `aproximado` | 23 | 22 | 23 | 22 | 1 |

En `resumen_tabla$sql`, `id_muestra` identifica la consulta de datos: el
mismo identificador garantiza exactamente las mismas filas. Moda,
frecuencia de la moda y mediana son métricas por columna y quedan con
`NA`; también queda `NA` cualquier camino que no pueda sostener esa
garantía. Así la comparabilidad se comprueba por comparación directa,
sin cruzar `lote` ni `columnas_compartidas`.

### Aislar la columna culpable en pocas consultas, y no recorrer tres veces lo que cabe en una

Los lotes de agregados que el motor rechaza ya no se reintentan columna
por columna sin información. La vía de agregados reutiliza la bisección
de la lectura de muestras: sondea mitades, conserva los grupos aceptados
como datos medidos y reintenta por métrica sólo las columnas culpables.
El tope sigue siendo de hasta `2n - 1` sondas para un lote de `n`
columnas y, si el presupuesto se agota, las hojas pendientes quedan
`no_disponible` sin ser supuestas. El tamaño mayor de lote aceptado se
guarda sólo en el presupuesto de la corrida y se publica como
`meta$tamano_lote_funciono`; no queda estado global asociado a la
conexión.

Los agregados planos sobre la misma tabla y filtro —`COUNT(col)`,
mínimos, máximos, medias, ceros, negativos y desvío— comparten ahora una
consulta por lote. `COUNT(DISTINCT ...)` conserva su clase separada. La
fusión no cambia las métricas ni su disponibilidad: una falla de un
agregado en una columna todavía se prueba por separado antes de
declararla no disponible.

La medición se hizo sobre una tabla en memoria de 20 columnas y 100
filas, recreando la expresión en cada vuelta, con `muestra = 12` y
`bloque_muestra = "solo_agregados"`. Las consultas emitidas antes y
después fueron, en orden `exacto`, `seguro`, `conteos`, `muestreado`,
`aproximado`:

| modo         | antes | después | ahorro |
|--------------|------:|--------:|-------:|
| `exacto`     |    50 |      49 |      1 |
| `seguro`     |    10 |       8 |      2 |
| `conteos`    |     6 |       6 |      0 |
| `muestreado` |    59 |      58 |      1 |
| `aproximado` |    59 |      58 |      1 |

El ahorro es deliberadamente pequeño en el modo por omisión porque
`COUNT(DISTINCT ...)` y la moda siguen fuera de la consulta plana; la
mediana también, salvo en los motores cuya sonda acepta la consolidación
con `PERCENTILE_CONT`. La fusión paga sobre todo en `seguro`, donde las
tres pasadas planas pasan a una.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
refleja esas clases y su rango de sondas por bisección.

### El plan ya no cobra trabajo de R que no va a ocurrir

Con `bloque_muestra = "solo_agregados"` no se trae ninguna fila a R, así
que el detector de vocabulario no corre. El plan lo reflejaba bien en
cuatro modos y mal en `"muestreado"`: seguía anunciando los pares de
formas a comparar en R, y el texto impreso se contradecía a dos líneas
de distancia —«el plan incluye sólo agregados SQL» y después «más
4.000.000 pares de formas a comparar en R»—.

El conteo colgaba de un conjunto de alcances que mete en la misma bolsa
dos cosas distintas: el muestreo **del motor**, que en ese modo ocurre
igual, y el bloque **del cliente**, que es lo único que trae filas.
Ahora cuelga sólo del segundo.

### Instrumentación de consultas y etapas R

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
agrega a `resumen_tabla$sql` `duracion_ms`, `cpu_ms`,
`n_filas_resultado`, `bytes_resultado_r`, `consulta_id` y `etapa`.
También publica `resumen_tabla$tiempos`, con las duraciones en
milisegundos de la lectura y el perfilado de la muestra, el perfilado
por columna y los análisis opcionales. Las ramas sin consulta dejan
`NA`; no se publican ceros por falta de resolución del reloj.
`instrumentar = FALSE` apaga la medición sin cambiar el plan ni el orden
o la cantidad de consultas.

Un microbenchmark de 158 columnas y 262 consultas pasó de 0,2230 s sin
instrumentar a 0,2368 s con reloj, filas y bytes (+0,0138 s; 6,188 %);
dos lecturas de [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html)
solas costaron aproximadamente 10 µs por consulta. En el flujo real de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
(158 columnas, 10.000 filas y 347 consultas), cinco pares alternados
dieron medianas de 2,398 s y 2,419 s (+0,021 s; 0,876 %). Por eso la
medición queda activa por omisión y conserva un interruptor explícito
para corridas donde ese costo relativo importe.

### La poda y el informe preguntan ahora por la misma función

[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
decide dos veces lo mismo: si un par puede alcanzar el umbral —para
descartarlo sin calcularlo— y si lo alcanzó —para informarlo—. Las dos
decisiones estaban escritas como desigualdades separadas, y dos
expresiones algebraicamente equivalentes no son iguales en punto
flotante: cada forma pierde el borde con su propio conjunto de umbrales,
y ahí la poda descarta un par que el informe habría publicado.

Ahora las dos preguntan por `.alcanza_umbral_dependencia()`. Mientras
fueran expresiones distintas, saber que coinciden exigía probar todos
los umbrales; compartiendo función, no hay nada que coincidir.

No lleva tolerancia a propósito: una tolerancia cambiaría lo que el
umbral significa, y eso sería parte del contrato público y no un detalle
interno.

### Dos podas descartaban lo que igualaba el umbral

[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
y
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
descartan pares sin calcularlos cuando el umbral es inalcanzable. Las
dos comparaciones se escribían multiplicando, y la multiplicación
redondea: `25 * 0.56` da `14.000000000000002`, así que un par cuyo
cumplimiento máximo vale exactamente `0,56` quedaba por debajo y se
descartaba. Como el informe descarta sólo lo que está **por debajo** del
umbral, ese par debía informarse.

Lo mismo en la poda por cardinalidades de
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md):
con siete valores distintos contra veinticinco y umbral `0,28`, el
producto daba `7.0000000000000009` y declaraba
`cardinalidades_imposibles` una cobertura que sí era alcanzable.

Las dos comparan ahora dividiendo. Los umbrales por omisión del paquete
no disparaban el defecto —por eso no se había visto—, pero `umbral` y
`umbral_cobertura` admiten cualquier proporción, y un barrido exhaustivo
con la forma anterior encontró setecientas dos podas de más.

### Pedir sólo agregados sin leer la muestra

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
aceptan `bloque_muestra = "solo_agregados"` para calcular los agregados
SQL sin traer filas a R ni ejecutar el perfil de la muestra. El valor
por omisión `"con_muestra"` conserva el comportamiento anterior.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
omite la fila y el costo de la muestra cuando corresponde, y la
cobertura usa `no_solicitado` para distinguirlo de una muestra que se
intentó leer y no estuvo disponible.

### La inferencia de tipo clasifica el vocabulario repetido una sola vez

`.inferir_tipo()` usa el vocabulario textual cuando su cardinalidad hace
conveniente el recorrido, y pondera cada forma por su frecuencia en las
filas. El camino que no activa el atajo conserva las mismas pasadas
sobre cada valor. La detección de fechas queda sobre las filas
muestreadas: sus atributos `compatibles`, `total`, `analizados`,
`muestreado` y el caché interno de meses forman parte del contrato del
perfilado y no se reemplazan por conteos de formas.

### Dos números que las optimizaciones habían movido

Los cambios que quitaron recorridos internos prometían no mover nada de
lo informado. Movían dos cosas, las dos en el borde.

**La mediana no es el cuantil 0,5 hasta el último bit.**
[`median()`](https://rdrr.io/r/stats/median.html) promedia los dos
valores centrales con `(a + b) / 2` y `quantile(type = 7)` interpola con
`a + 0,5 * (b - a)`; cuando los centrales son de magnitudes muy dispares
redondean distinto. Sobre `c(-1000, 0.000111, 0.25, 1000)` la mediana
informada pasaba de `0,12505549999999999` a `0,12505550000000001`.
Vuelve a salir de [`median()`](https://rdrr.io/r/stats/median.html); los
cuartiles siguen compartiendo una sola llamada, así que el recorrido que
se ahorra sigue ahorrado.

**Y la poda de dependencias callaba el par que iguala el umbral.** La
cota se comparaba como `k_y - k_x > n * (1 - umbral)`, y `1 - 0.8` vale
`0,19999999999999996`: con cinco filas, la resta hacía que
`1 > 0,99999999999999978` y el par se descartaba. Su cumplimiento era
exactamente `0,8`, y el filtro de informe descarta sólo lo que está
**por debajo** del umbral, así que ese par debía informarse. Escrita
como el máximo alcanzable contra lo que el umbral exige, el borde deja
de perderse: medido sobre 200.000 combinaciones, la forma anterior
podaba de más 88 veces y la nueva ninguna.

### La deteccion de dependencias conserva el resultado y reduce el costo

La particion de parejas `(determinante, dependiente)` usa una clave
entera cuando el producto de sus cardinalidades no supera `2^53`; en el
borde o fuera de el conserva el camino con
[`interaction()`](https://rdrr.io/r/base/interaction.html). La clave
solo renumera parejas, por lo que no cambia sus conteos, los grupos en
conflicto ni ninguna dependencia informada.

La deteccion agrega una cota de poda basada en la cantidad `P` de
parejas distintas del subconjunto valido: `P - k_x`. La particion y sus
conteos se reutilizan si el par debe evaluarse, y la bateria de
equivalencia compara el objeto completo con ambos atajos y con sus
caminos de referencia.

### Tres recorridos internos se eliminan sin cambiar lo informado

El resumen cuantitativo comparte una sola llamada a
[`quantile()`](https://rdrr.io/r/stats/quantile.html) para obtener los
cuartiles y la mediana, deriva el IQR de esos valores y pasa `q1` y `q3`
al diagnóstico de sentinelas. La guarda que evita ese diagnóstico con
menos de 20 valores sigue decidiendo antes de sus cuantiles. Sobre un
millón de valores, la expresión equivalente pasó de una mediana de
**0,048 s** a **0,027 s**; el resultado fue
[`identical()`](https://rdrr.io/r/base/identical.html).

El conteo general de duplicados calcula una vez cada dirección de
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) y deriva los
dos conteos. Los `tryCatch` siguen aislando un fallo inicial (`NA, NA`)
de un fallo sólo en `fromLast` (`valor, NA`). Sobre 300.000 × 5, la
mediana pasó de **0,928 s** a **0,707 s**; los dos conteos fueron
idénticos.

La detección de dependencias poda un par sólo cuando ninguna de sus
columnas tiene ausentes y la cota `k_y - k_x > n * (1 - umbral)` hace
inalcanzable el umbral. Las cardinalidades usadas son las de
`estadisticas`, que en ese caso son exactamente las del subconjunto
válido; con ausentes el par se evalúa completo. Sobre 20.000 × 12 se
podaron **36 de 132** pares y se hicieron 96 llamadas al resumen: la
mediana pasó de **0,575 s** a **0,444 s**. El objeto completo fue
[`identical()`](https://rdrr.io/r/base/identical.html) con la ejecución
sin poda, y el caso que cumple 0,996 se conserva.

### El plan declaraba cero trabajo para la tabla entera

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
subdeclaraba el costo justo en el caso por omisión. Con `muestra = Inf`
—que trae la tabla entera— el bloque de muestra se contaba como **cero
filas leídas y cero pares de formas a comparar**, de modo que pedir
todas las filas declaraba menos trabajo que pedir mil. Sobre una tabla
de 200.000 × 4, `muestra = Inf` anunciaba 400.000 lecturas y
`muestra = 200000` —que pide exactamente las mismas filas— anunciaba
600.000.

Como la magnitud del trabajo se decide sobre esos números, el caso por
omisión caía en «baja» y el plan **no imprimía las palancas** para bajar
el costo.

Las dos formas de pedir la tabla entera declaran ahora lo mismo. Los
valores que sí son inválidos siguen tratándose como antes.

Y el hueco simétrico: una `muestra` finita **mayor** que las filas de la
tabla tampoco quedaba acotada. Pedir un millón de filas de una tabla de
cien no trae más de cien —la lectura real es `min(n_total, muestra)`—,
pero el plan imputaba un millón de lecturas. La inconsistencia era
interna: el trabajo del cliente sí se acotaba, así que las dos mitades
de la misma cuenta usaban tamaños de muestra distintos. Ahora el tamaño
efectivo se calcula una sola vez y las dos mitades lo comparten.

### Las geometrías que vienen de una base ya no pierden su trazabilidad

Un hallazgo sobre una columna de geometría —una coordenada fuera del
dominio, una geometría inválida, una vacía— devolvía las filas que lo
respaldan **sólo si la columna era un objeto `sfc`**. Por DBI las
geometrías llegan como texto WKT o como blob WKB, así que la traza salía
`no_disponible` y el paquete avisaba de una incoherencia **que no
existía**: los índices ya estaban calculados, y se descartaban por la
clase de la columna.

Contra una tabla PostGIS real eso producía
`coordenada_fuera_dominio en geom: traza no disponible`.

La condición pasa a ser que el análisis haya dejado sus índices, no de
qué clase es la columna: son posiciones de fila y valen igual esté la
geometría como `sfc`, como texto o como binario. Una columna común sin
análisis de geometría sigue sin traza, que es lo correcto.

### Faltar tablas se rechaza antes, y se nombran todas

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) acepta
los datos como un data frame o como una lista con nombre. La primera
forma exigía que el modelo tuviera una sola entidad; la segunda
comprobaba la forma de la lista pero **no que estuvieran las tablas de
todas las entidades que el modelo necesita**.

El faltante aparecía después, al ligar cada métrica, con un mensaje que
nombraba **una sola** entidad. Con tres ausentes eso son tres vueltas
para el mismo problema: agregar la primera, volver a correr, chocar con
la segunda.

Ahora se rechaza en la frontera, nombrando **todas** las que faltan y
diciendo cuáles se recibieron —que es la mitad que deja ver un error de
tipeo—.

**No es igualdad exacta, a propósito.** Una tabla que el modelo no pide
se sigue ignorando: exigir que sobren cero habría apagado un uso
legítimo.

### El muestreo en el motor declina una muestra infinita, en un solo lugar

Desde que `muestra = Inf` —la tabla entera— es el valor por omisión, el
muestreo **en el motor** no tiene sentido: quedarse con «todas» las
filas no es muestrear. Eso estaba guardado rama por rama, y de tres
ramas se guardaron dos. La tercera —la de
`TABLESAMPLE RESERVOIR (n ROWS)`— pasaba `Inf` directo al constructor y
contra un motor real producía `RESERVOIR (Inf ROWS)` y un error de
sintaxis.

El paquete lo declaraba honestamente —«El resumen SQL se calculó y se
devuelve, pero la muestra no»— pero el usuario se quedaba sin perfil de
muestra sin haber pedido nada raro.

La guarda pasa a estar **en la entrada** de la función y no dentro de
cada rama. Antes de moverla se comprobó que valiera para las tres: con
`Inf` la fracción se satura en 1, o sea `TABLESAMPLE (100 PERCENT)`, que
es la tabla entera. Si no se hubiera saturado, la rama de porcentaje
habría sido un caso legítimo y la guarda única, un error.

Guardar caso por caso es cómo se olvidó una de tres, y además deja sin
proteger a la rama que se agregue después.

### El aviso de trazabilidad dice de quién es el problema

Cuando un hallazgo quedaba con su trazabilidad inconsistente, el aviso
decía «La trazabilidad contiene incoherencias» seguido del detalle
interno. Una corrida contra bases reales lo describió como honesto pero
críptico, y al mirarlo resultó peor que críptico: **estaba dirigido a la
persona equivocada**.

Las seis condiciones que lo disparan comparan dos salidas del propio
`lupa` —lo que el hallazgo afirma contra las filas que se le adjuntaron—
y **ninguna mira los datos del usuario**. Es una guarda de invariante
contra regresiones: la prueba que la cubre la dispara mutando
`n_afectados` a mano después de perfilar, y comprueba que un perfil
intacto no la emite.

Ahora el aviso dice, en ese orden: que es un problema de `lupa` y no de
sus datos, que no hay nada que corregir en la tabla, qué significa para
el hallazgo que está leyendo —las filas que lo respaldan pueden no
corresponderse con su conteo—, que se conserva para no ocultar la
evidencia, y el detalle interno con la etiqueta del hallazgo para poder
reportarlo.

### El alcance LSH declara que sus candidatos dependen del orden

Con MinHash/LSH el conjunto de candidatos depende del orden de las
filas: el vocabulario de q-gramas se numera por orden de primera
aparición y esa numeración alimenta las firmas. **No es un defecto**
—ocurre dentro de la garantía declarada, y está medido: de los pares que
cambian al barajar, ninguno supera un Jaccard de q-gramas de 0,8, donde
el recall declarado es 0,9998— pero era una propiedad del resultado que
vivía sólo en la documentación.

`alcance` gana `lsh_candidatos_dependen_orden_filas` y
`lsh_orden_vocabulario`. El segundo no es decorativo: nombra el
mecanismo, así que si alguna vez la numeración se canoniza, ese valor
cambia y la dependencia deja de declararse sola. Ninguno de los dos
aparece en el camino exhaustivo, que no tiene esa dependencia.

### Una barra de progreso para las corridas largas

Ahora que la tabla entera es el valor por omisión, una corrida sobre
millones de filas puede tardar minutos. Y una corrida callada no se
distingue de una colgada.

En una sesión interactiva
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
muestra una barra que avanza contra **las consultas que el plan dice que
va a emitir**: un total conocido, no una estimación. Y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
avanza **por tabla**, diciendo cuál está perfilando —que es lo que
permite ver si una se trabó— y sin aparecer cuando hay dos tablas o
menos. No aparece cuando la corrida es de menos de una docena de
consultas —termina antes de que sirva— ni fuera de una sesión
interactiva, para que la salida de un guion no traiga ruido.
`options(lupa.progreso = )` manda sobre eso en los dos sentidos.

No cambia ningún valor de lo que se mide, y hay una prueba que compara
el perfil con la barra y sin ella para que siga siendo cierto.

### La tabla entera pasa a ser el valor por omisión

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
traían **1.000 filas** por omisión para el perfil de muestra. Ahora
traen `Inf`: la tabla entera.

Antes ni siquiera se podía pedir. `muestra` exigía una cantidad
**finita**, así que perfilar la tabla completa obligaba a averiguar
cuántas filas tenía y pasarlas a mano: se podía hacerlo, no se podía
*decirlo*. Ahora `Inf` significa «toda, sea del tamaño que sea», la
consulta sale sin `LIMIT` y `tabla_completa` queda en `TRUE`.

El cambio de omisión sigue el criterio del paquete: **un análisis de
calidad no se corre todos los días, y mirar menos filas para terminar
antes es la concesión equivocada**. Quien necesite acotar lo pide con
`muestra = n`, y el plan lo avisa antes de empezar: «El perfil de
muestra trae la tabla entera —N filas—: es el valor por omisión y sobre
una tabla grande puede demorar».

Importa más de lo que parece. El resumen de tabla **no** se muestrea
—con `modo = "exacto"` se agrega en el motor sobre todas las filas—,
pero los diagnósticos que necesitan los valores en R —patrones,
formatos, casi-duplicados— salen de esa muestra, y sin `orden_muestra`
son las **primeras filas que devuelva el motor**, no una muestra
aleatoria. Un defecto que viva al final de la tabla no se ve.

Medido sobre 200.000 filas con un defecto plantado al final: con el
valor por omisión aparecen **tres** hallazgos y con `Inf` aparecen
**cinco**, a cambio de 10 segundos en vez de 2. Un análisis de calidad
no se corre todos los días; si el tiempo no es la restricción, `Inf` es
la opción honesta.

### El plan nombra las palancas también cuando el trabajo es medio

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
avisa antes de pagar, y cuando el trabajo estimado era **alto** listaba
las opciones concretas para acotarlo. Cuando era **medio**, se limitaba
a avisar.

Eso dejaba la decisión a medias justo donde importa. Corriendo contra
motores reales, una tabla de 4,5 millones de filas en PostgreSQL tardó
**6,2 minutos** con las opciones por omisión, y su plan la clasificaba
**media**: el aviso avisaba, pero quien no conociera
`modo = "muestreado"` —que baja esa misma tabla a **39 segundos**, 9,5
veces menos— no tenía cómo enterarse.

Ahora las nombra en los dos casos: `modo`, `metricas`, `muestra`,
`max_consultas`, y `max_trabajo_vocabulario` cuando lo que pesa es la
comparación de formas en R. En una tabla chica no las lista: ahí el
usuario ya tiene su respuesta y cuatro viñetas serían ruido en cada
corrida.

### El recorte de duplicados ya no depende del orden de las filas

[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md)
recorta con `max_resultados` conservando los pares más cercanos. Entre
pares **empatados en distancia** desempataba por posición de fila, así
que cuáles sobrevivían dependía del orden en que llegaron las filas y no
de los datos: la misma tabla exportada con otro `ORDER BY` podía
devolver otro subconjunto.

Medido sobre 60 grupos cuyos pares internos comparten exactamente la
misma distancia, con el corte en 30: cinco órdenes distintos —natural,
inverso y tres barajados— devolvían 30 grupos cada uno y **no compartían
ninguno**. Ahora los cinco devuelven exactamente los mismos.

El desempate usa el **rango canónico del valor**, que es la misma
decisión que ya gobernaba el recorte del vocabulario —ahí tomar las
formas en orden de llegada daba 26 grupos y en orden alfabético 148—. La
clave es simétrica, `min` y `max` del rango, para que tampoco dependa de
cuál fila quedó primera dentro del par, y el rango se calcula una vez
sobre el universo completo de la corrida: una numeración por lote sería
local y haría que la comparación entre lotes dependiera del reparto.

Lo que ordenar no arregla —que un corte dentro de un empate deje afuera
pares igual de cercanos— se declara. `alcance` gana `distancia_corte`,
`n_en_distancia_corte` y `corte_en_empate`, y ese último no es
`truncado` con otro nombre: da `FALSE` cuando el corte cae en una
distancia única.

`corte_en_empate` se mide **contra lo que el recorte descartó**, no
contra lo que quedó. Contar cuántos de los conservados comparten la
distancia del borde no alcanza: si en el borde sobrevive uno solo, el
conteo da 1 y la señal caería en `FALSE` aunque se hubieran tirado pares
a esa misma distancia. Pasaba con cuatro filas y `max_resultados = 1`.

Y queda un límite que ningún orden saca: si varias filas comparten el
valor comparado, el conjunto de **pares de valores** es idéntico en
cualquier orden, pero **cuáles instancias de fila** los representan
cambia. Es irreducible —esas filas son indistinguibles en esa columna— y
está declarado en la documentación.

### El modo aproximado dice qué métrica aproximó, y con qué función

`perfilar_dbi(modo = "aproximado")` sondea las funciones aproximadas
nativas del motor y cae a la medida exacta cuando el motor las rechaza.
Esa contabilidad ya estaba completa por métrica en `resumen_tabla$sql`
—`estado`, `metodo` y `error_esperado`—, pero al imprimir el perfil se
leía `Métricas: calculado 24, estimado 6` sin saber **cuáles** eran las
seis ni con qué función se calcularon.

Ahora el print lo dice:
`Aproximaciones aplicadas: distintos por APPROX_COUNT_DISTINCT y mediana por approx_quantile`,
o bien
`Sin aproximación para distintos y mediana: el motor no la aceptó y las métricas se midieron exactas`.
No cambia ningún número: publica donde se lee lo que ya estaba medido.

`error_esperado` sigue respondiendo `desconocido` cuando el motor no
documenta una cota. Medir un error sobre una tabla no autoriza a
declararlo como garantía sobre todas.

### Los motores reales se verifican en cada push

La fila «probado contra el motor real» de la tabla de motores dependia
de que alguien se acordara de correrlo a mano: se media una vez, quedaba
en notas, y la tabla envejecia sin avisar.

`benchmark/verificar_motor.R` comprueba contra un motor real que las
columnas se perfilan, que el dialecto se resuelve por sonda, que **la
media del motor coincide con la de R sobre la misma columna** -que corra
no alcanza-, que la clave primaria se lee del catalogo, y que la
cobertura existe. Se configura por variables de entorno, asi que el
mismo guion sirve en una maquina y en integracion continua, y sale con
codigo distinto de cero si algo falla.

El flujo `motores.yaml` lo corre contra PostgreSQL 16 y MariaDB 11
levantados como servicios. Los que exigen un cliente propietario se
siguen verificando a mano, y su fila de la tabla dice con que fecha.

### El catalogo entra en la identidad de una tabla

`catalogo.esquema.tabla` es el nombre de tres partes que usan SQL Server
y otros motores, y `lupa` perdia el del medio: se quedaba con la primera
y la ultima. Como las dos que quedaban eran sintacticamente validas, el
SQL salia bien formado contra una tabla inexistente y el error volvia al
usuario como **un problema suyo de permisos** —lo mandaba a pedirle al
DBA acceso a una tabla que ya podia leer—.

Ahora el catalogo es una columna estructurada, siempre presente y `NA`
cuando no existe. El identificador une las partes que hay y sigue siendo
inyectivo: `t`, `esq.t`, `cat.esq.t` y `cat.t` son cuatro identidades
distintas.

**Y la unicidad pasa a mirar la identidad completa.** Antes, dos tablas
con el mismo nombre y esquema en catalogos distintos colapsaban y la
coleccion las rechazaba como repetidas: se rechazaba una frontera
valida.

Lo que **no** cambia: sin catalogo el identificador es identico al de
antes, asi que ninguna coleccion guardada deja de cruzar con su
frontera; y un identificador mal formado -cuatro partes, comillas sin
cerrar, punto inicial o final- se sigue rechazando **nombrando la
causa**.

### Una clave declarada excluye Benford, y queda declarado

Sobre una clave primaria se emitia una desviacion de Benford: se le
afirmaba un problema de calidad a una columna que no tiene distribucion
que analizar.

Se intento adivinar cual columna era clave por la forma de sus valores y
ese camino se retiro, porque el criterio terminaba dependiendo de
cuantas filas se habian cargado y callaba magnitudes reales. **Declarada
no hay nada que adivinar**: si se pasa `clave` a
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), o
se lee del catalogo de la base, Benford no corre sobre esas columnas y
la cobertura lo declara. Sin declararla el comportamiento no cambia.

Y el motivo dice cual de las dos cosas paso. «Parece un identificador»
es una inferencia del paquete; «la clave fue declarada» es un hecho que
trajo el usuario, y publicar la primera cuando corresponde la segunda le
atribuye al paquete una deduccion que no hizo.

### Menos codigo repetido, y el que quedaba ya habia divergido

Seis reglas estaban escritas mas de una vez. Dos de ellas **tenian que
coincidir o el paquete mentia**, y una ya habia divergido en tres cosas
a la vez.

- **La identidad de una tabla** en una coleccion estaba en dos lugares.
  La frontera se declara con una y se lee con la otra, asi que si
  divergian la cobertura de una coleccion dejaba de cuadrar.
- **La validacion del destino de un archivo**, en tres. Ya diferian en
  el orden de las comprobaciones, en si el mensaje nombra el directorio
  que falta —una no lo nombraba— y en la redaccion del aviso de
  sobrescritura.
- **`datos` debe heredar de data.frame**, once veces. **`perfil` debe
  corresponder a las columnas**, seis, con dos redacciones que convivian
  en el mismo archivo a trescientas lineas de distancia.
- **La interpretacion de numeros escritos como texto** —convencion
  decimal, unidad y moneda— estaba dos veces, cuarenta y ocho lineas
  cada una. Las dos tienen que dar el mismo veredicto o la misma columna
  se describiria distinto segun por que camino se la miro.
- **Los dos metodos de correctitud referencial** eran la misma funcion
  escrita dos veces: 48 de sus 68 lineas coincidian y lo unico que
  cambiaba era que columnas de la referencia se usan.

### Los ejemplos no nombran organismos

El ejemplo de
[`organizacion()`](https://sebollin.github.io/lupa/reference/organizacion.md)
y las pruebas de esa granularidad usaban el nombre de organismos reales.
Un ejemplo de roxygen viaja al `.Rd` y al sitio publicado, y las pruebas
viajan en el tarball. Ahora usan nombres genericos -`"Organismo A"`,
`"Organismo B"`-, que es lo que corresponde: la frontera de una
organizacion la declara quien la conoce, y el paquete no tiene por que
nombrar a ninguno.

### Lo que no se evaluo llega a las cuatro puertas

Cuando `lupa` decide no correr un diagnostico lo anota con su motivo,
pero esa tabla solo llegaba a quien mirara el perfil. Ahora llega a las
cuatro puertas desde las que se trabaja:

- [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md)
  la lleva **por grupo**. Cada grupo se perfila por separado, asi que
  cada uno declina los suyos: una columna puede tener bastantes filas en
  un grupo y muy pocas en otro. Sin esa tabla, un grupo sin hallazgos se
  lee como un grupo sano, cuando puede ser un grupo sobre el que no se
  miro.
- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  la lleva y la anuncia al imprimir el plan.
- [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
  la consulta para no informar como `resuelto` un diagnostico que
  simplemente dejo de correrse.
- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  ya la tenia en `resumen_tabla$cobertura`.

### Una gramatica de entidades HTML, no dos

La misma expresion estaba escrita dos veces: una para **detectar**
entidades y otra para **repararlas**. Si divergian se detectaba lo que
no se reparaba, o al reves, y ninguna de las dos avisaba. Es el mismo
acoplamiento que tenian las tres copias de la generalizacion de
patrones.

### En Oracle la cadena vacia es el nulo, y eso se declara

Medido contra Oracle Free 23 real: las mismas tres filas -`""`, `NA`,
`"x"`- dan `n_faltantes = 2` por Oracle y `1` por un motor que las
distingue. La misma columna tiene una completitud distinta segun el
motor, y no porque el dato cambie.

No es un defecto que se pueda arreglar -es la semantica del motor- pero
callarlo si lo seria: quien compare completitud entre entregas de
motores distintos leeria una diferencia que no esta en los datos. Queda
en la cobertura del resumen, y solo en los motores donde corresponde.

En la misma corrida se verifico contra el motor real que el dialecto se
resuelve solo en `fetch_first`, que las 54 metricas se calculan sin
ninguna no disponible, y que la media, la mediana y el desvio calculados
por Oracle coinciden con los de R sobre la tabla entera.

### La clave primaria se lee del catalogo cuando esta declarada

Sobre un `data.frame` no hay a quien preguntarle cual es la clave, y por
eso
[`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md)
ordena candidatas. En una base relacional esa pregunta **ya tiene
respuesta escrita**: la clave primaria esta en el catalogo del motor.
Ahi no hay nada que sugerir, hay que leerla.

Se resuelve en **una sola consulta**, elegida por el controlador y no
probando una tras otra: probar hasta acertar gastaria un numero de
consultas que depende del motor, y
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
promete exactamente cuantas emite. Es el mismo criterio por el que la
sonda del desvio gasta siempre dos aunque acierte en la primera.

Y se distinguen dos respuestas que se confundian. SQLite devuelve cero
filas **sin error** para una tabla que no existe, asi que filtrando en
el SQL “no declara clave” y “no se pudo preguntar” llegaban iguales. Se
piden todas las columnas y se filtra despues: cero filas significa que
la tabla no esta, y filas sin ninguna marcada significa que no tiene
clave.

### Lo que no se evaluo se dice donde se decide, y la clave se pregunta

- **Una comprobacion de no-ASCII que la suite si puede hacer.**
  `R CMD check` avisa cuando un archivo de `R/` trae un caracter no
  ASCII fuera de un comentario, y la suite no podia verlo: el barrido
  que existia mira los literales del espacio de nombres ya cargado,
  donde una cadena escrita con acento crudo y otra escrita con `\uXXXX`
  son **la misma cadena**. La unica forma de distinguirlas es mirar el
  fuente. La comprobacion nueva lo hace, se saltea diciendo por que
  cuando `R/` no esta a la vista -bajo `R CMD check`, donde el trabajo
  lo hace el check nativo-, y se comprobo que falla: se le introdujo un
  acento crudo a proposito.

- **El plan de limpieza dice lo que no se evaluo.** El plan se arma
  desde los hallazgos, asi que por construccion no puede tener una
  accion para un diagnostico que se declino. Quien trabajaba desde el
  plan leia tres acciones y concluia que lo demas estaba bien, cuando lo
  que habia pasado es que sobre esa columna no se miro. Ahora la
  cobertura viaja con el plan y se anuncia al imprimirlo, con el motivo
  medido de cada diagnostico.

- **Una clave declarada que repite es un hallazgo, no un aviso.** Se
  informaba con `cli_warn`, que no tiene severidad, no viaja al informe
  ni al plan y no dice que filas repiten. Ahora es un hallazgo de
  severidad `error` con la trazabilidad a las filas.

- **[`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md)
  y
  [`elegir_clave()`](https://sebollin.github.io/lupa/reference/elegir_clave.md).**
  Adivinar si una columna es clave mirando solo sus valores no funciona
  -un monto tambien es casi unico-. Preguntar si funciona, y preguntar
  bien es ofrecer las candidatas ordenadas en vez de una casilla en
  blanco. El orden combina tres senales que se publican por separado,
  para poder discutirlo en vez de aceptarlo: si identifica cada fila, si
  no tiene ausentes, y cuanto se parece su nombre al de una clave.
  `Otra` permite escribir una que no este en la lista, o varias para una
  clave compuesta. En sesion no interactiva no pregunta ni elige sola.

  Se distinguen dos motivos que antes se confundian: una columna que
  **repite** tiene duplicados de carga o la clave es otra; una que **no
  esta en todas las filas** esta incompleta. Piden arreglos distintos.

### Dos arreglos medidos y retirados, y los limites que quedan declarados

Se probaron dos reglas para cerrar dos falsos hallazgos conocidos. Las
dos funcionaban sobre el caso que las motivo y las dos **callaban algo
real**, asi que ninguna entra: la regla del paquete es que una guarda
solo se acepta si no calla nada verdadero. Lo que se midio queda escrito
en el codigo, y los limites quedan fijados en `test-ronda124.R` para que
esten declarados y no se redescubran.

- **Reconocer una clave dispersa por unicidad.** El problema es real:
  sobre una clave repartida en un rango ancho se emite
  `desviacion_benford`, que afirma un problema de calidad sobre una
  columna que no tiene distribucion que analizar. Y la densidad no puede
  resolverlo, porque una clave de 1 a 2.300.000 tiene densidad 0,0043,
  **mas dispersa que un monto** (0,0096).

  El criterio probado fue “unicidad que el azar no explica”. Fallo por
  dos motivos medidos. Primero, **depende de cuantas filas se
  cargaron**: el estadistico crece con el cuadrado de las filas, asi que
  la misma clave de cedulas cambia de veredicto al pasar las ~5.200
  filas, y un padron de 2.000 filas recibe `desviacion_benford` mientras
  el mismo padron con 30.000 se reconoce bien. Eso es una propiedad de
  la consulta, no del dato. Segundo, **calla magnitudes reales**: una
  lectura acumulada de medidor, un timestamp en segundos, una coordenada
  UTM redondeada y un monto que solo se llena en algunos expedientes son
  unicos por su mecanismo, y sobre la lectura de medidor con un valor
  absurdo adentro el criterio se tragaba el valor absurdo.

- **Descartar el centinela que tiene vecinos.** La idea era que un
  centinela esta solo y un codigo de catalogo vive en un tramo, y sobre
  `222` entre `221` y `223` funcionaba. Lo tumbo la codificacion mas
  comun de los microdatos de encuesta: `9999` = “no sabe” junto a `9998`
  = “no contesta”. Medido, el `9998` contaba como vecino y descartaba al
  `9999`, y como el `9998` no tiene forma de digito repetido tampoco
  entraba por su cuenta: los dos centinelas quedaban invisibles y salian
  **cincuenta y cinco codigos de ausencia informados como valores
  extremos de una magnitud**.

  No hay senal de forma que separe `221/222/223` de `9998/9999`: en los
  dos casos son valores contiguos, extremos y repetidos. La diferencia
  es semantica.

- **Una comprobacion que no podia fallar.** La prueba que cuida que una
  columna protegida no publique el valor centinela se salteaba a si
  misma cuando el hallazgo no aparecia: si el detector dejaba de emitir,
  la suite quedaba verde. Ahora asevera que el hallazgo esta.

### Once senales falsas sobre una base real, y ninguna se apaga en silencio

Una corrida contra tres tablas administrativas reales dio 24 senales, de
las cuales **once eran falsas**. El calculo estaba bien en las once; lo
que fallaba era el juicio de si la prueba corresponde. Un identificador
no es una magnitud, y hay pruebas que solo describen magnitudes.

- **Benford y limites de Tukey sobre numeraciones** (seis falsos). La
  guarda que ya tenia Benford exigia una corrida consecutiva *sin
  huecos*, y un identificador real tiene huecos: los que se dieron de
  baja. Un `MotId` de 1 a 4557 sobre 3.159 filas no la pasaba y Benford
  se corria igual.

  Lo que separa una numeracion de una magnitud no es la unicidad -un
  monto tambien es casi unico- sino la **densidad**: un identificador
  ocupa un tramo compacto de los enteros (0,69 en ese caso) y una
  magnitud se reparte por varios ordenes (0,00005 para montos entre 9 y
  9.999.999).

  Un valor fuera de escala rompe esa compacidad, asi que los casos que
  hay que ver se siguen viendo: un `10000` entre identificadores de 1 a
  100, o un ano centinela 1900 entre anos 2000-2030, bajan la densidad y
  vuelven a senalarse.

- **`alta_cardinalidad` sobre texto libre** (tres falsos). Un nombre,
  una descripcion o un objetivo tienen cardinalidad alta por definicion.
  Ahora una columna de texto cuyos valores promedian 40 caracteres o mas
  no se marca.

- **Relacion de orden entre dos numeraciones** (un falso).
  `MotId <= MEsId` se cumplia en el 99,1 % de las filas porque los dos
  contadores avanzan juntos, y el 0,9 % restante “violaba” una regla que
  no existe. Lo que separa ese par de un `inicio`/`fin` legitimo
  -tambien entero, tambien denso- es la brecha: constante fila a fila
  cuando hay una regla detras, erratica cuando solo hay dos contadores.
  La guarda respeta el rescate por brecha estable que ya existia.

- **`constante` medido sobre una muestra** (un falso). Una tabla de 200
  filas con tres valores, muestreada en 50, informaba “la columna
  contiene un unico valor”. Una proporcion estimada sobre una muestra
  sigue siendo honesta; una cuantificacion universal no: basta una fila
  no leida para desmentirla.

**Ninguno de los seis se apaga en silencio.** Bajar el ruido callando
seria mejorar el numero sin mejorar el paquete, asi que cada prueba que
no se corre deja su fila en `cobertura_diagnosticos`, con el motivo
medido -que porcentaje de los enteros cubre la columna, cuantos valores
se habrian senalado, cuantas filas de cuantas trae la muestra- y que
hacer si el usuario no esta de acuerdo. Los pares de orden descartados
quedan nombrados en
`meta$orden_columnas$pares_identificador_descartados`.

### La ganancia por hilos no era una propiedad del paquete

La vinieta de escala publicaba una tabla -133,28 s con dos hilos, 70,31
con dieciseis- medida sobre un padron que no se distribuye, asi que
nadie podia rehacerla. Ahora `benchmark/medir_escala_hilos.R` genera un
padron sintetico y mide la misma curva, y **no da lo mismo**: sobre
100.000 filas con 23.800 nombres distintos, de dos a dieciseis hilos se
gana un 12 %, no la mitad del reloj.

La explicacion es que los hilos los usa `stringdist` al comparar, y
generar los candidatos y armar los grupos no los usa. Segun cuanto pese
cada parte en un conjunto concreto, la ganancia va del 90 % al 12 %. La
vinieta publica las dos tablas y dice que quien vaya a subir el valor
conviene que mida su caso.

Lo que si vale en las dos: pasados dieciseis hilos no hay ganancia
medible, y el resultado no cambia. **Y esto ultimo recien ahora esta
comprobado de verdad.** El banco comparaba cuantos pares devolvia cada
configuracion, y como el tope de resultados se alcanza en todas, el
numero era siempre el mismo: la comprobacion se cumplia sola. Ahora
compara los 50.000 pares uno por uno.

Otros dos defectos del mismo banco, encontrados antes de que publicara
ningun numero: el generador daba 80 nombres distintos para 100.000 filas
-el detector trabaja sobre formas distintas, no sobre filas, asi que no
ejercitaba nada- y [`nrow()`](https://rdrr.io/r/base/nrow.html) sobre el
resultado devolvia `NULL`, porque es una lista con `$pares` y no un data
frame.

### Un diagnostico nuevo: el centinela que ninguna lista puede declarar

Un `9999` es una edad imposible y un codigo postal perfectamente valido.
Por eso la lista de `sentinelas_numericos` no lo trae por omision, y
hace bien: marcarlo siempre romperia cualquier columna donde ese numero
es un dato.

Lo que si lo distingue es cumplir **las tres cosas a la vez**: quedar
fuera de los limites de la columna, repetirse cinco veces o mas, y tener
forma de digito repetido. `posible_centinela_numerico` lo informa sin
contarlo como ausencia -esa decision es de quien conoce la columna,
agregandolo a la lista- y nombra las filas donde esta.

Medido sobre doce columnas con la respuesta conocida, acierta las doce.
Los tres controles que podrian haberlo roto: un codigo postal `9999`
repetido treinta veces no es extremo dentro de su columna, un monto real
de `9999` no se repite, y un ano `1999` no tiene esa forma.

Es el unico diagnostico del paquete que **ninguna senal sola podia
dar**: la forma sin los limites marca codigos validos, los limites sin
la forma marcan cualquier extremo, y la repeticion sin las otras dos
marca cualquier valor frecuente.

### Un valor centinela se escapaba de la proteccion de datos personales

Es lo mas serio de la tanda y lo introdujo la tanda misma. El perfil
pasa a medir cual es el valor que una columna usa para decir “sin dato”
-un `9999` entre edades- y ese campo publicaba un valor de celda sin
pasar por el enmascarado. Sobre una columna de documentos protegida se
veia asi:

    moda                    [valor protegido]
    minimo                  NA
    centinela_valor         9999

Y la descripcion del hallazgo lo nombraba en texto, con lo cual llegaba
hasta el informe HTML. La evidencia si salia protegida: la fuga entraba
por la puerta de al lado.

Ahora `centinela_valor` se enmascara igual que el minimo, la moda y la
media, y la descripcion se limpia en la misma capa que ya limpiaba la
evidencia. Las repeticiones se siguen informando, porque son un conteo y
no un valor.

Que el valor sea casi seguro un centinela y no un documento no cambia la
regla: **la proteccion no adivina cuales valores son inocentes**.

### Dos diagnosticos que se contradecian sobre la misma columna

`999` esta en la lista de centinelas declarados, asi que
`faltantes_disfrazados` lo cuenta como ausencia con severidad `error`.
El diagnostico nuevo decia, sobre esa misma columna, que “no se cuenta
como ausencia porque no esta declarado”. Los valores que la lista ya
cubre dejan de producirlo: el diagnostico existe para los que **no**
estan declarados, como un `8888`.

### `perfilar()` dentro de `do.call()` rompia el informe

[`deparse()`](https://rdrr.io/r/base/deparse.html) de una expresion
larga devuelve varias lineas, y con
`do.call(perfilar, list(tabla, ...))` la expresion es la tabla entera:
el nombre del conjunto salia con ocho elementos y
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
fallaba con “values must be length 1”. Pasar los argumentos en una lista
es lo natural cuando se perfila en un bucle. Si la expresion no cabe en
una linea, no la escribio nadie y se usa una etiqueta generica.

### Lo que el idioma no deja resolver

La comparacion que separa una errata de la misma palabra escrita de otra
manera ignora acentos, y en español el acento distingue palabras. El
mismo mecanismo que junta `Jose` con `José` -que si es la misma- junta
`papa` con `papá` y `ano` con `año`, que no lo son. Separarlos pide un
diccionario del idioma.

Asi que se corrigio la afirmacion, no la medida: el texto decia “son la
forma dominante escrita distinto” y ahora dice **en que difieren**, que
es verificable, y avisa de que en español el acento puede cambiar la
palabra.

### La misma numeracion, descrita de dos maneras

Aparecio al intentar romper el arreglo de arriba. Un codigo 1..284 con
179 valores fuera de los limites de Tukey:

- guardado como `integer`, se callaba y quedaba declarado en cobertura;
- guardado como `double`, se senalaba.

La guarda dependia de **como estaba almacenado el numero, no de que
es**. Y eso pesa mas de lo que parece: por la puerta DBI casi todo llega
como `doble` -SQLite entrega asi hasta las fechas- y varios lectores de
CSV tambien, asi que el arreglo no cubria el caso que lo motivo cuando
el identificador venia de una base.

La causa estaba en la raiz. `.resumen_secuencia_entera` exigia tipo
`entero` y **dos lineas mas abajo comprobaba que todos los valores
fueran enteros**. La segunda condicion es la real; la primera solo
dejaba columnas afuera, y con ellas su `densidad_secuencia_entera`, que
quedaba en `NA`.

### Lo que dijeron las cinco auditorias externas

Cinco revisiones independientes -coherencia entre las dos puertas,
prueba de las afirmaciones publicadas, barrido de codigo, bancos y
redaccion- dejaron sus informes. Lo que sigue es lo que quedaba abierto
de ellas.

- **La tasa de referencia no era una tasa del motor.** `supuesto_costo`
  decia “unos cinco millones de lecturas de fila por segundo sobre
  PostgreSQL 16”, y ese cociente esta en las unidades que cuenta el
  plan, no en filas que el motor haya leido: la cuenta supone que ningun
  indice ayuda y cobra el desvio como dos pasadas aunque un motor con
  desvio nativo lo resuelva en una. El numero sirve para lo que existe
  -convertir `filas_leidas` en segundos- y ahora lo dice. Se midio:
  26.001.000 lecturas en las unidades del plan sobre 5,3 segundos.

- **La tasa de pares se remidio con el banco.** Da de 660.000 a
  1.150.000 pares por segundo con valores de cuarenta caracteres -la
  banda cubre dos maquinas- y unos 80.000 con valores de doscientos,
  contando los pares que se comparan de verdad. Con doscientos
  caracteres el detector recorta por `max_trabajo` a 1.005 formas de
  2.000, asi que dividir por los pares que el plan contaria inflaba la
  cifra cuatro veces.

- **Las dos puertas describen tipos distintos, y ahora se declara.**
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  informa el tipo que declara el motor, y un motor que no preserva
  `DATE` ni `BOOLEAN` hace que esas columnas se midan como numeros: la
  misma columna de fechas da el desvio en dias por una puerta y en
  segundos por la otra, y la moda como entero crudo en vez de fecha
  formateada. Cada puerta describe lo que tiene delante; lo que faltaba
  era decirlo.

- **El desvio de una columna temporal esta en segundos** y no lo decia
  ninguna parte. Los demas momentos viajan formateados en
  `minimo_fecha`, `media_fecha` y compania; el desvio no es un momento
  sino una duracion, queda como numero, y un `136610.4` sobre fechas son
  1,6 dias.

- **Cuatro funciones internas que solo llamaba la suite** se sacaron del
  paquete. Viajaban en cada instalacion sin que nada las usara, y sus
  pruebas daban la impresion de que estaban vivas. La unica que servia
  de algo -una version escalar contra la que medir la vectorizada- sigue
  existiendo como oraculo dentro de su propia prueba.

- **Dos etiquetas exigian saber estadistica para poder descartarlas.**
  Un hallazgo que hay que traducir antes de juzgarlo cuesta mas que uno
  que se entiende: quien no sabe que es Benford no lo puede descartar
  con criterio, o le cree. Ahora `desviacion_benford` explica que en
  muchas magnitudes el 1 encabeza cerca del 30 % de los valores y el 9
  menos del 5 %, y `outliers` dice que hay valores muy alejados del
  grueso de la columna antes de nombrar a Tukey.

### Una prueba que pasaba sin probar nada

`test-formatos-adicionales.R` verifica que los meses escritos se
detecten igual en cualquier `LC_TIME`. Recorria tres locales con
`try(Sys.setlocale(...))` y comparaba contra la base medida en `C`. En
una maquina donde esos locales no estan generados -una imagen minima de
contenedor no los trae- la llamada falla, el `try` se traga el fallo, el
locale nunca cambia y el test compara el resultado contra si mismo.

Pasaba. Y la unica senal de que no habia medido nada era un aviso suelto
en el resumen de la suite: “OS reports request to set locale cannot be
honored”.

Ahora comprueba que el locale quedo puesto, y si ninguno se puede poner
se saltea declarando el motivo en vez de contar un exito vacio.

### El plan de consultas dice rango en todas partes

`attr(plan, "supuesto")` ya declaraba un rango, pero el metodo de
impresion, el `@return` de la ayuda, los dos README y la vinieta seguian
diciendo “techo”. Un techo que el propio objeto desmiente dos lineas mas
abajo es peor que no decir nada.

- [`print()`](https://rdrr.io/r/base/print.html) ahora dice “entre N y M
  consultas”, y sobre un plan subconjuntado -que conserva la clase y
  pierde los atributos- imprime la tabla y avisa, en vez de titular “sin
  dato consultas sobre sin dato filas”.
- `total_lotes_rechazados` aparece en el `@return`, que no lo
  documentaba.

### Una tabla con acentos rompia el perfil entero

Es el defecto mas serio de la tanda, y **lo introdujo el arreglo del
orden del vocabulario de ayer**. Sobre cualquier tabla leida con
[`read.csv()`](https://rdrr.io/r/utils/read.table.html) que tuviera
acentos:

    Error: Character encoding must be UTF-8, Latin-1 or bytes

El orden por bytes de R rechaza una cadena marcada `unknown` que
contenga bytes no ASCII, **aunque sean UTF-8 perfectamente validos**, y
asi llega cualquier CSV en espanol por el camino mas comun que hay:
`"Combustibles liquidos"`, `"Energia Electrica"`. El mismo error estaba
en el desempate de la moda.

- Ahora la codificacion se marca antes de ordenar. Lo que despues de eso
  siga sin ser valido pasa por `iconv(sub = "byte")`: no es bonito, pero
  es determinista y ordenable. Caer al orden del entorno habria devuelto
  la dependencia de la maquina que este orden existe para sacar.
- Los otros tres usos de orden por bytes del paquete ordenan enteros,
  que no tienen requisito de codificacion.
- **Ni las cuatro auditorias externas ni las 15.696 comprobaciones de la
  suite lo encontraron, porque todos los fixtures son ASCII.** Aparecio
  buscando otra cosa: el registro publico con el que se cierra una fila
  de la tabla de evidencia. La prueba nueva construye la cadena con
  [`rawToChar()`](https://rdrr.io/r/base/rawConversion.html), porque un
  literal en el fuente lo marca el parser de R y el caso no se ejercita.

### La tabla de evidencia dice ahora con que se reproduce cada fila

Llego a publicar tres numeros que nadie podia comprobar desde el
repositorio. El problema de fondo no eran los tres numeros sino que la
tabla no obligaba a que cada afirmacion tuviera un reproductor. Ahora
tiene una columna que lo dice.

- **Controles limpios**: decia 43 tablas y 25 senales. El generador esta
  en el repositorio, se redujo a 31 tablas y el ruido bajo a 8 -el
  paquete mejoro y el texto seguia diciendo lo viejo-. Los tres numeros
  quedan fijados en `test-ronda107.R`.
- **Defectos plantados**: se saca la fila. El numero es real, se midio
  en tres rondas, pero su banco no esta en el repositorio y
  reconstruirlo de memoria daria nueve defectos parecidos y no los
  mismos. Vuelve cuando exista su test.
- **Registro real de sanciones**: ahora hay
  `benchmark/medir_sanciones.R`, que baja el registro publico del
  catalogo nacional -2.556 filas- y contrasta cada hallazgo de severidad
  `error` contra una comprobacion escrita a mano en R base. Da **9 de
  9**, uno mas que cuando se midio. Ese archivo es ademas la regresion
  del defecto de codificacion de arriba: es el que lo destapo.

### Un token que es marca de formato ya no genera un falso duplicado

Mirando **que** reportaba el detector de vocabulario sobre una tabla
real de vuelos aparecieron dos familias mezcladas:

    [1:48 p.m. (27)  / 1:48 p.m.            Delayed (1)]   <- el estado del vuelo pegado
    [12:00 a.m. (5)  / 12:00 p.m. (42)]                    <- doce horas de diferencia

La primera es un hallazgo real. La segunda es un falso positivo:
`12:00 a.m.` y `12:00 p.m.` son **dos valores legitimos distintos**, y
no hay forma de saber mirando la columna cual fue tipeado mal. Marcarlos
a todos no es detectar, es sospechar en bloque de todos los valores de
una forma y acertar por casualidad los que estaban mal: la precision de
ese diagnostico era **0,259**, tres de cada cuatro marcados eran valores
correctos.

- El detector descarta un par cuando **todos los tokens que lo
  distinguen aparecen en buena parte de la columna**. `a.m.` y `p.m.`
  estan en casi todos los valores; `Delayed` esta en uno.
- Tres condiciones lo acotan, y las tres salieron de romper la suite con
  una version que no las tenia (44 pruebas caidas): el valor tiene que
  tener **mas de un token** —si no, el token que difiere es el valor
  entero y la regla borra el caso central del detector—, la cantidad de
  tokens tiene que coincidir —cuando cambia, como al pegar `Delayed`,
  hay que conservarlo— y el vocabulario tiene que tener al menos 20
  formas, porque “aparece en toda la columna” no significa nada sobre
  cinco.
- El descarte se declara en `n_pares_descartados_formato`.
- **El costo esta medido y publicado.** Sobre el banco de vuelos la
  precision sube de 0,524 a 0,658 y la cobertura baja de 0,281 a 0,238:
  se pierden 111 aciertos porque el banco inyecto erratas que son
  exactamente un cambio de meridiano. Esas caen debajo del techo
  estructural —un `p.m.` mal tipeado es indistinguible de uno correcto
  sin una referencia externa— y el lugar correcto para atraparlas es una
  regla entre columnas, no la proximidad de cadenas.
- Sobre el banco de hospitales **no cambia nada**: lo que distingue dos
  nombres es contenido y no una marca de formato. La regla actua solo
  donde la marca existe.

### Una columna en Latin-1 perdia sus acentos en silencio

Es el defecto mas grave que encontro esta tanda, y no es un caso de
borde: es un CSV viejo en espanol, que es la mayoria de lo que hay en
datos publicos de la region. Sobre una columna con cinco valores
distintos, el perfil informaba:

    n_distintos: 2        <- son 5
    n_faltantes: 0        <- dice que no falta nada
    cobertura:   0 filas  <- no declara nada

[`validUTF8()`](https://rdrr.io/r/base/validUTF8.html) mira los bytes, y
los de un texto marcado `latin1` no son UTF-8 validos, asi que `CAFE`,
`ANO` y `NUMERO` con tilde se volvian `NA` antes de llegar a cualquier
diagnostico. El invariante del paquete roto en su forma mas directa:
informar como medido lo que se descarto.

- **Lo que R sabe convertir ahora se convierte.**
  [`Encoding()`](https://rdrr.io/r/base/Encoding.html) dice `latin1`
  cuando R conoce la codificacion, y
  [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html) convierte sin
  perder nada. El paquete estaba tirando informacion que podia recuperar
  con una llamada. Con eso, la misma columna informa `n_distintos = 5`.
- **Y lo que no se puede convertir, se declara.** Un texto cuya
  codificacion nadie declaro y cuyos bytes no son UTF-8 validos se sigue
  descartando —no hay forma de adivinar si `0xE9` era una `e` con tilde
  o basura— pero ahora `cobertura_diagnosticos` gana una fila
  `texto_no_descifrable` que dice cuantos valores quedaron afuera, y por
  que no cuentan ni como distintos ni como faltantes.

### La proteccion de datos personales dependia de por que puerta entraras

La media de una columna de cedulas salia **expuesta** por
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
**tapada** por
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):

    perfilar()      media = 5108024      detalle: [estadisticos de orden protegidos]
    perfilar_dbi()  media = NA           detalle: [estadisticos de orden y momentos protegidos]

El argumento estaba escrito del lado DBI desde antes —“la media de las
cedulas de una tabla chica reconstruye demasiado”— y el camino principal
no lo aplicaba. Ahora las dos tapan la media, y el texto distingue si se
taparon estadisticos de orden, momentos o los dos, para no declarar una
proteccion que no se aplico.

### El total del plan no era un techo, y ahora se publica como rango

`attr(plan, "supuesto")` decia que el total era un techo y declaraba
solo la direccion “menos”: una columna sin valores validos no emite sus
metricas. Nunca declaraba la direccion “mas”. Medido contra un motor que
rechaza lotes —el caso exacto que motivo la consolidacion—:

    plan (techo) = 22 consultas        real emitidas = 30

Si un lote falla, se emite la consulta del lote y ademas una por
columna. Quien decide la viabilidad de una corrida con ese numero se
quedaba corto justo en el escenario de degradacion. Ahora el plan
publica `total` —lo que cuesta si ningun lote se rechaza— y
`total_lotes_rechazados` —si se rechazaran todos—, y el costo real cae
entre los dos.

### Dos numeros publicados que no resistieron que otro los midiera

Los dos eran nuestros y de esta semana, y los dos son la misma forma de
error: **medir una cosa y publicarla como otra.**

- **“Una columna corriente de dos mil valores se compara entera”** es
  falso sin calificar el largo. El tope por trabajo muerde cuando
  `L^2 x n(n-1)/2` supera `2e10`, o sea a partir de **101 caracteres**
  para dos mil valores distintos. Y falso en el peor lugar: la columna
  de WKT de 900 caracteres que motivo el presupuesto cae del lado
  recortado, asi que la frase tranquilizadora no alcanzaba justo a los
  datos que hicieron falta el tope.
- **La tasa de 200 caracteres estaba inflada cuatro veces.** El banco
  dividia el tiempo por los pares que el *plan* contaria y no por los
  que de verdad se compararon: sobre 2000x200 el plan cuenta 1.999.000
  pares y se comparan 499.500, porque `max_trabajo` recorta a 1.000
  formas. La tasa real es unos **70.000** pares/seg, no “70.000 a
  270.000”.

### La tabla de evidencia del README, medida de nuevo

Publicaba **43 tablas de control y 25 senales**. El generador esta en el
repositorio, se redujo a 31 tablas, y el ruido bajo a 8 —el paquete
mejoro— pero el README siguio publicando los numeros viejos **porque
ninguna prueba los ataba**. Ahora dice 31 tablas, 0 errores y 8 senales,
y los tres estan fijados en `test-ronda107.R`: si cambian, la suite
falla y hay que actualizarlos a proposito.

Se saco la fila de los nueve defectos plantados. El numero es real —se
midio en tres rondas— pero el fixture no esta en el repositorio, y
reconstruirlo de memoria daria nueve defectos parecidos y no los mismos.
En la tabla que sostiene el argumento del paquete, una fila menos es
mejor que una fila que no se puede comprobar. Vuelve cuando exista su
test.

### Restos de correcciones anteriores

- Una trazabilidad sin filas se declaraba **disponible**: el condicional
  tenia las dos ramas iguales
  (`if (total) "disponible" else "disponible"`), resto de una
  correccion. Con cero indices el objeto prometia una localizacion que
  no existe, con `localizador = "ninguno"` al lado. La rama vacia va a
  `no_disponible`, que es el valor por omision de la propia funcion.
- `.reparar_mojibake_uno` estaba definida **dos veces con algoritmos
  distintos** y topes distintos (4 y 20 iteraciones). El orden
  alfabetico de carga decidia cual corria; la otra era codigo muerto que
  alguien podia “arreglar” creyendo que era la que se usa.
- `.bit64_disponible` y `.bit64_disponible_dbi` eran la misma funcion
  con dos nombres. Queda un solo punto de verdad.
- `.detectar_orden_columnas()` recibia un argumento `resultados` que su
  cuerpo no usaba, y el llamador lo construia para nada.
- `muestra = 1.5` se aceptaba en memoria y perfilaba **una** fila en
  silencio, mientras la via DBI daba error. Ahora las dos lo rechazan.
  La unica diferencia que queda es deliberada: `Inf` vale en memoria y
  no contra un motor.

### El veredicto ya no depende de como venga ordenado el archivo

Cuando el vocabulario de una columna de texto excede el presupuesto, hay
que elegir que formas comparar. Se elegian **las primeras en aparecer**,
y eso hacia que el resultado dependiera del orden de las filas.

Medido sobre la columna `nombre` de *Ejes de vias de circulacion* de
Montevideo —45.400 filas, 8.318 formas distintas, del catalogo nacional
de datos abiertos—, las **mismas filas** daban:

| orden de las filas                   | grupos de casi-duplicados |
|--------------------------------------|---------------------------|
| tal como viene el archivo            | **26**                    |
| desordenado (semillas 11, 202, 7777) | 71, 85, 70                |
| alfabetico                           | **148**                   |

De 26 a 148 segun como estuviera ordenado el archivo. Un perfilador que
hace eso mide la forma fisica de la tabla, no los datos, que es
exactamente lo que el paquete promete no hacer.

- Ahora las formas se **ordenan antes de recortar**. Los cinco ordenes
  de arriba dan **148** grupos: el resultado es el mismo venga como
  venga el archivo.
- Ordenar tiene ademas una razon de fondo: los casi-duplicados quedan
  **adyacentes** —`CAMINO CARRASCO` junto a `CAMINO AGRARIOS`—, asi que
  el corte cae entre familias en vez de partirlas. Una muestra al azar
  rompe pares: si de un grupo de dos sobrevive uno, el grupo desaparece.
  Por eso el azar rinde 70-85 y el orden rinde 148 **con el mismo
  presupuesto**.
- El orden es por bytes (`method = "radix"`) y no el del entorno: la
  intercalacion por omision cambia de una maquina a otra, y eso habria
  cambiado el defecto de lugar en vez de sacarlo.
- El mensaje de cobertura dice ahora que las formas comparadas son las
  primeras del alfabeto y que lo que queda afuera es su tramo final.
  Antes recomendaba “desordenar la tabla antes”, que era el mejor
  consejo posible mientras el defecto estuviera.

Lo destapo la tercera vuelta contra bases reales, que dejo esta
afirmacion sin verificar por no encontrar una columna que la ejercitara.
La columna existia.

### Un conteo del perfil ya no cambia de clase segun que tenga instalado el usuario

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
devolvia `n_validos`, `n_faltantes`, `n_distintos`, `frecuencia_moda`,
`meta$filas` y `filas_totales_fuente` como **`integer64`** siempre que
`bit64` estuviera instalado, incluido un conteo de 20.

- Eso no agregaba precision: por debajo de 2^53 un `double` ya
  representa el entero exacto.

- Y agregaba tres problemas. La clase del mismo campo dependia de un
  `Suggests`.
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  devolvia `integer` para `n_distintos` y
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  devolvia `integer64`: dos puertas del mismo paquete en desacuerdo
  sobre el mismo campo.

- El tercero es el que decide. Un perfil guardado en una maquina con
  `bit64` y leido en una que no lo tiene mostraba:

        columna     n_validos   n_distintos
      1      id 9.881313e-323 9.881313e-323

  donde midio `20`. Sin error, sin aviso, y sumando como si fuera un
  numero. Informar como medido algo que no lo es, en el paquete cuyo
  argumento es justamente ese.

- Los conteos salen ahora `numeric`. `integer64` se conserva **solo
  donde compra exactitud**: un conteo por encima de 2^53 que el motor
  entrega como texto o como `integer64`. Para un conteo de filas eso
  significa una tabla de mas de nueve mil billones.

- De paso se corrige el error simetrico: `conteo_exacto` decia `FALSE`
  para un conteo entregado como texto por encima de 2^53, que es justo
  el caso donde si se guarda exacto. El paquete se declaraba menos
  preciso de lo que era.

### El R minimo declarado ahora es un R medido

`DESCRIPTION` declaraba `R (>= 3.6.0)`, y era una promesa que el paquete
**no cumple**. Pasa a `R (>= 4.1.0)`.

- Se venia afirmando que la suite **no puede** correr bajo R 3.6, porque
  `testthat` declara `R (>= 4.1.0)`. Eso es cierto contra CRAN de hoy y
  falso contra el snapshot de la epoca: ahi `testthat 3.1.7` instala sin
  problema. La suite corre, y da `[ FAIL 18 | PASS 15356 ]`. La
  generalizacion tapo dieciocho fallos.
- **Seis de los dieciocho salen de una sola causa.** Bajo R \< 4.0,
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) trae
  `stringsAsFactors = TRUE`, y el paquete tiene 275 llamadas a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) que no lo
  declaran: ahi las columnas de texto nacen factor. No es cosmetico: la
  proteccion de datos personales escribe `"[valor protegido]"` en una
  columna factor, R lo rechaza por nivel invalido y queda `NA`. No hay
  fuga, pero la promesa sobre esa celda no se cumple, y falla en
  silencio. Bajo R 4.0.5 con los mismos paquetes esos seis desaparecen.
- **`4.1.0` y no `4.0.0`** porque es lo que pide `testthat` actual: en
  el piso declarado la suite corre con las herramientas de hoy, que es
  lo que hace verificable la promesa. Medido en contenedor antes de
  declararlo: `rocker/r-ver:4.1.3`, `checking tests ... OK`,
  `[ FAIL 0 | PASS 14613 ]`. Nada obligaba al numero viejo: `cli`, el
  unico Import, pide `R (>= 3.4)`.
- Los doce fallos restantes bajo versiones viejas de los `Suggests` no
  son del R: con `RSQLite 2.3.1` y `bit64 4.0.5` los conteos vuelven
  como `integer64` y viajan asi hasta el perfil, asi que un mismo campo
  cambia de clase segun que tenga instalado el usuario. Queda anotado
  como trabajo abierto, no como resuelto.

### Tres pruebas que exigian justo lo que decian no tener

Las tres tenian la misma forma, y solo se ven en un entorno donde el
paquete opcional de verdad no esta.

- La normalizacion Unicode juntaba en un bloque la mitad que **mide**
  —que necesita `stringi`— y la mitad que **declara su ausencia**,
  simulada con un mock. Sin `stringi`, la primera fallaba y se llevaba
  puesta a la segunda. Poner una guarda habria salteado las dos,
  incluida la que no la necesitaba: van separadas, con la guarda donde
  corresponde.
- Las dos pruebas de `integer64_sin_soporte` armaban el `data.frame` con
  la columna ya marcada como `integer64`, y eso obliga a
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) a
  buscar el metodo de `bit64`. La prueba de que falta `bit64` exigia que
  `bit64` estuviera. Ahora la clase se pone despues de armar la tabla,
  que ademas es el camino realista: la columna llega marcada dentro de
  una tabla que ya existe.
- [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) acepta
  argumentos sueltos recien desde 1.2. Con la version anterior el error
  que salta es el de DBI y la prueba mide otra cosa; van nombrados.

Nota sobre el instrumento: `_R_CHECK_DEPENDS_ONLY_=true` ocultaba
`bit64` para `skip_if_not_installed()` —saltaba ocho pruebas por eso— y
aun asi las dos de `integer64` pasaban ahi, mientras fallaban en el
contenedor. **El check con dependencias recortadas no es equivalente a
un entorno que no tiene el paquete.**

### El reintento que contra el driver real no se disparaba nunca

La version anterior agrego un reintento: si la lectura de la muestra
falla y hay columnas de tipo largo declaradas, se reintenta sin ellas y
se declara que quedaron afuera. Contra una base real no se disparo **ni
una vez**. En la tabla que motivo el arreglo —158 columnas, 90 de ellas
`varchar(max)`— el patron de tipos reconocio **0 de 90**, la muestra se
perdio entera y no aparecio el aviso de alcance: exactamente lo que el
arreglo prometia evitar.

- La causa no es un tipo que falte en la lista. El driver **no informa
  nombres**: `odbc` resuelve `dbColumnInfo()` con
  `nanodbc::result::column_datatype()`, que devuelve el **codigo
  numerico de ODBC**. Contra `{SQL Server}` las noventa columnas llegan
  como noventa veces `"-1"`. Comprobado en las dos puntas: la biblioteca
  compilada no expone `column_datatype_name` ni contiene un solo literal
  de nombre de tipo SQL, y el patron de nombres no matchea `"-1"`.
- El arreglo se habia probado contra un banco que hablaba en nombres,
  que es la unica forma de tipo que el patron sabe leer. El banco
  compartia con el patron justo la propiedad cuya ausencia era el fallo.
- **El reintento ya no infiere: pregunta.** Cuando la lectura falla, se
  aisla por descarte cuales columnas no se pueden leer —biseccion sobre
  el conjunto, podando los subconjuntos que si se leen— y se declaran
  esas. Es independiente del controlador: funciona sin reconocer ningun
  tipo.
- El patron queda como **atajo optimista**: si reconoce el tipo, ahorra
  el descarte. Se le agregaron los codigos ODBC (`-1`, `-4`, `-10`) y
  las variantes de nombre (`LONG VARCHAR`, `SQL_LONGVARCHAR`,
  `WLONGVARCHAR`) que tampoco veia. Pero la correccion ya no cuelga de
  el.
- Lo que se declara cambia segun como se supo. `omision_comprobada`
  distingue **medido** de **supuesto**: por descarte, cada columna fallo
  sola y el resto se leyo junto, y el aviso lo dice; por el atajo, sigue
  diciendo “no se comprobo que sean la causa”. Y `sondas_descarte`
  publica cuantas consultas costo averiguarlo.
- El descarte esta acotado por los dos lados: como mucho `2n` sondas
  sobre `n` columnas —que es lo que cuesta la biseccion en el peor
  caso—, tope absoluto de 512, y **nunca mas de la mitad del saldo de
  `max_consultas`**, para no recuperar la muestra a costa de quedarse
  sin presupuesto para el resto.
- Si el descarte no aisla nada, no se inventa una culpable: la muestra
  se declara no disponible y el motivo dice cuantos subconjuntos se
  sondearon y como termino —ninguna falla sola, o fallan todas, o el
  tope corto antes—.
- De paso: el reintento rearmaba el SQL a mano y perdia el muestreo del
  motor por el camino —volvia a una lectura de primeras filas mientras
  `metodo` seguia declarando el muestreo nativo—. Ahora la lectura
  original y el reintento salen de la misma receta, y `metodo`,
  `acotado_en` y `fraccion` se corrigen con lo que de verdad se emitio.

### Un plan que contaba la mitad del reloj

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
le ponia magnitud **`"baja"`** a una tabla de 3.912 filas que tardaba
**35 segundos**. Cada numero que informaba era cierto —64.592 lecturas
de fila, cero ordenaciones— y el juicio era falso: el trabajo no estaba
en el motor sino en R, comparando formas de una columna de geometria en
texto. Medir una mitad y llamarla el total es informar como completo
algo parcial.

- La magnitud se estima ahora en **dos mitades**. La del motor sigue en
  `filas_leidas` y `ordenaciones_completas`, resumida en
  `magnitud_motor`. La del cliente esta en `columnas_texto` y
  `pares_texto` —cuantos pares de formas podria comparar el detector de
  vocabulario sobre la muestra—, resumida en `magnitud_texto`.
  `magnitud` es **la mayor de las dos**.
- La unidad es el par de formas comparadas, que es una cuenta y no un
  indice: la muestra trae `m` filas, las formas distintas son a lo sumo
  `m`, y el detector nunca compara mas de `max_pares` por columna. El
  tope se lee de la firma del detector, no se copia, asi que no puede
  quedar estimando contra un numero viejo.
- Los umbrales (2e6 y 2e8 pares) estan anclados a la misma escala de
  segundos que los del motor, con la tasa medida: **de 660.000 a 960.000
  pares por segundo sobre valores de cuarenta caracteres**, contra los
  cinco millones de lecturas de fila por segundo de la referencia de
  PostgreSQL. La medicion esta en `benchmark/medir_costo_texto.R`,
  seccion 5, para que el umbral no sea un numero elegido a dedo.
- Lo que el plan **no** puede saber queda dicho, no escondido: el conteo
  de pares es exacto, pero cuanto cuesta cada uno depende del largo de
  los valores, que el plan no leyo. Sobre valores de doscientos
  caracteres la tasa cae a unos 70.000 pares por segundo, asi que con
  textos muy largos el tiempo real es varias veces el que sugiere la
  referencia. `supuesto_costo` lo declara en vez de prometer segundos.
- La impresion muestra las dos mitades, y cuando la que pesa es la de R
  nombra la palanca de ese lado —`max_trabajo_vocabulario`—, que las
  palancas del motor no tocan.

### Presupuestos que miden trabajo, no que cuentan unidades

Una tabla del catalogo de PostGIS —`spatial_ref_sys`, **3.912 filas y 5
columnas**— tardaba **243 segundos**. No era la geometria: eran cadenas
largas, WKT de proyecciones, y el detector de vocabulario se llevaba el
99,6 % del costo. Tenia dos topes, `max_valores = 5000` y
`max_pares = 2000000`, y **ninguno de los dos miraba cuanto costaba cada
unidad**: 800 valores son 319.600 pares, muy por debajo del tope, pero
cada comparacion era una Jaro-Winkler sobre 900 caracteres.

- La unidad del presupuesto es ahora la **comparacion de un caracter
  contra otro**, que es el bucle interno de la distancia: comparar dos
  valores de largos L1 y L2 cuesta del orden de `L1 x L2`. La suma sobre
  todos los pares de un prefijo sale **exacta y en tiempo lineal**, sin
  materializar la matriz.

- Contar pares por largo medio no alcanzaba. Medido, ese modelo compraba
  **5,3 millones de unidades por segundo con valores de 900 caracteres y
  44 millones con valores de 40**: ocho veces de diferencia es no tener
  modelo. Con el producto de largos la dispersion baja a 4,25 veces, y
  lo que queda es a favor de las columnas de valores cortos, que son el
  caso comun.

- `max_trabajo_vocabulario` vale `2e10` por omision, calibrado contra la
  medicion y no contra la intuicion:

  | valores | largo | sin tope | con tope  | comparado |
  |---------|-------|----------|-----------|-----------|
  | 400     | 900   | 15,1 s   | **4,3 s** | 55,5 %    |
  | 500     | 900   | 23,0 s   | **4,3 s** | 44,4 %    |
  | 800     | 900   | 61,3 s   | **4,6 s** | 27,8 %    |
  | 2000    | 80    | 5,0 s    | 5,1 s     | **100 %** |

  El ultimo renglon es el que importa tanto como el tercero: **una
  columna corriente de dos mil valores no se recorta**, siempre que sus
  valores midan menos de unos cien caracteres: el tope por trabajo
  muerde cuando `L^2 x n(n-1)/2` supera `2e10`, o sea a partir de 101
  caracteres para dos mil valores distintos. Tampoco 500x20 ni 1000x30.
  El riesgo del arreglo era romper el caso comun para arreglar el
  patologico.

- Una aclaracion que hay que hacer, porque la primera version de esta
  nota afirmaba de mas: **3000x20 si se recorta**, pero no por el
  presupuesto nuevo sino por `max_pares`, el tope viejo, que acota en
  2.000 formas sin mirar el largo. Sigue puesto porque acota la memoria
  de la matriz de pares. El recorte se declara con su motivo, asi que no
  se pierde en silencio, pero decir “no se recorta” era falso. Salio de
  que el banco apagaba `max_pares` para aislar el efecto del tope nuevo
  y despues se leyo esa medicion como si fuera lo que recibe un usuario.
  El banco separa ahora las dos cosas.

- Lo recortado **se declara**, con las dos cuentas separadas: cuantas
  formas normalizadas quedaron sin comparar y cuanto trabajo era, en el
  alcance del hallazgo y en `cobertura_diagnosticos`, junto con cual de
  los dos topes recorto. Si aprietan los dos, el motivo los nombra a los
  dos: el usuario tiene que poder elegir cual aflojar.

- El recorte toma las **primeras formas en aparecer**, no una muestra, y
  ahora lo dice. Con los mismos 300 valores y el mismo presupuesto,
  poniendo primero los largos entran 8 formas y poniendo primero los
  cortos entran 150: sobre una tabla ordenada lo que queda afuera es un
  tramo del orden. Decir cuantas quedaron sin comparar y callar cuales
  dejaba suponer un muestreo que no hubo.

- [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
  gana `max_trabajo`, en unidades **fila-par**, porque ahi el costo es
  del orden de `columnas^2 x filas` y `max_comparaciones` no lo veia:
  158 columnas son 24.806 pares, muy por debajo de las 200.000 del tope.
  Se combina con `max_comparaciones` y manda el mas restrictivo.
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  lo expone como `max_trabajo_dependencias`.

### Un plan que dice cuanto cuesta, no solo cuantas consultas son

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
contaba consultas, y contar consultas no responde la pregunta que trae
quien lo mira: si la corrida tarda segundos, minutos u horas. Catorce
consultas sobre dos millones de filas son mucho mas trabajo que
doscientas sobre mil.

- El plan estima ahora la **magnitud** en dos numeros que son cuentas de
  verdad y no un indice inventado: `filas_leidas` —cuantas filas habria
  que leer— y `ordenaciones_completas` —cuantas veces habria que ordenar
  la tabla entera—. De ahi sale `magnitud`: `"baja"`, `"media"`,
  `"alta"`, o `"desconocida"` si no se conoce el numero de filas. El
  peso de cada clase de consulta sale de su `alcance`, que ya venia
  declarado.

- Al imprimirlo, un trabajo alto viene con las **palancas concretas**
  para acotarlo —`modo = "muestreado"`, recortar `metricas`, bajar
  `muestra`, `max_consultas`—. Avisar que algo es grande sin decir que
  hacer no le sirve a nadie.

- Es una estimacion y lo dice en `supuesto_costo`: cuenta las filas que
  habria que leer **si ningun indice ayudara**, y cada ordenacion
  completa como `log2(filas)` pasadas. Los dos numeros publicados no
  dependen de ese supuesto, asi que quien no lo comparta puede rehacer
  la cuenta. La referencia esta medida: PostgreSQL 16 local, 2 millones
  de filas por 40 columnas en modo seguro, 14 consultas y 5,3 segundos.

- Sobre la forma que se midio contra PostgreSQL 16 —2 millones de filas
  por 40 columnas—, la clasificacion cae donde tiene que caer, y el
  ultimo renglon es la razon de ser de todo esto:

  | modo         | consultas | lecturas de fila | ordenaciones | magnitud |
  |--------------|-----------|------------------|--------------|----------|
  | `conteos`    | 8         | 6.001.000        | 0            | baja     |
  | `seguro`     | 14        | 26.001.000       | 0            | media    |
  | `exacto`     | 94        | 186.001.000      | 80           | alta     |
  | `muestreado` | 94        | 2.445.000        | 0            | **baja** |

  Las mismas **94 consultas** son «alta» en `exacto` y «baja» en
  `muestreado`: el conteo es identico y el trabajo difiere por setenta y
  seis veces. Contar consultas no podia distinguirlos. Y los conteos de
  `conteos` y `seguro` son los mismos 8 y 14 que se cronometraron en 2,4
  y 5,3 segundos.

- Si aparece una clase de consulta cuyo alcance no tiene peso declarado,
  la magnitud queda **`"desconocida"`** en vez de estimarse de menos en
  silencio.

- Sobre una tabla chica los parrafos de supuestos no se imprimen: tapan
  la respuesta en vez de matizarla. Siguen en los atributos, y la
  palabra «techo» viaja con el conteo en todos los casos.

### Una columna que el controlador no sabe traer ya no se lleva la muestra entera

Hay columnas que muchos controladores no pueden devolver en una lectura
corriente: `TEXT` y `NTEXT` en SQL Server, `CLOB` y `BLOB` en Oracle,
`bytea` en PostgreSQL. Pedirlas junto con el resto hace fallar la
consulta completa, y con ella se perdia toda la muestra. En una de las
tablas de la corrida real, 90 de 158 columnas eran de esos tipos.

- Cuando la lectura de la muestra falla y hay columnas declaradas con
  esos tipos, se **reintenta sin ellas**. La muestra vuelve con las
  columnas que si se pudieron leer, en vez de no volver.
- Lo que quedo afuera se declara: `resumen_tabla$cobertura` gana una
  fila `alcance_distinto` que nombra las columnas omitidas y **conserva
  el motivo textual del motor**, mas la via para incluirlas
  —convertirlas a texto acotado en una vista y perfilar la vista—.
- El aviso cuenta la **secuencia y no atribuye la causa**. El reintento
  salta ante cualquier fallo de lectura habiendo columnas de esos tipos
  declaradas; que ellas sean el motivo es lo probable, no lo comprobado,
  y un corte de red que se recupera en el segundo intento daria el mismo
  camino. Decir “el controlador las rechazo” seria informar como sabido
  algo que no se midio.
- El resumen por columna las cubre igual, porque esos agregados se
  calculan en el motor. Lo que falta es su perfil por fila, y eso es lo
  que dice la cobertura.
- **`meta` tambien se corrige, y esto lo encontro la refutacion.** El
  bloque de metadatos del muestreo se arma antes de leer, asi que
  quedaba congelado con la lectura que fallo: `columnas_leidas`
  declaraba haber leido justamente la columna que no se pudo leer, y
  `sql_muestra` publicaba la consulta original en vez de la que de
  verdad se emitio. La cobertura decia la verdad y `meta` decia otra
  cosa, que es informar como medido lo que no se midio, en el lugar
  donde se mira para saber que se hizo. Ahora `meta` trae las columnas
  que realmente se leyeron, el SQL del reintento, y ademas
  `columnas_omitidas` con su motivo.
- El reintento es **portable**: no emite conversiones propias de un
  motor, solo vuelve a pedir la consulta sin las columnas rechazadas. Un
  `CAST` distinto por dialecto habria sido otra superficie que mantener
  y probar contra cada motor.

### Un mensaje que se leia mal

El aviso de acciones destructivas de `plan_limpieza` mostraba
`p\u00e9rdida` en pantalla: la cadena tenia la barra invertida
duplicada, asi que el escape nunca se resolvia. Es un error que no ve
nadie —el paquete instala, la suite pasa y `R CMD check` no protesta,
porque la cadena es ASCII perfectamente valida— y solo se nota leyendo
el mensaje. Hay ahora un barrido que recorre los literales de cadena del
espacio de nombres y falla si alguno lleva un escape sin resolver. Era
el unico caso en el paquete.

La primera version del barrido miraba solo el cuerpo de cada funcion, y
la refutacion mostro que eso dejaba fuera dos sitios donde de verdad
viven mensajes: los **valores por omision de los argumentos** y los
**atributos**. Ahora los recorre. Lo que sigue sin ver es una constante
capturada por closure desde un ambito local, y eso queda dicho en el
propio test en vez de suponerse cubierto: en `lupa` no es un agujero,
porque las constantes del paquete son enlaces del espacio de nombres y
el barrido las recorre una por una.

### El costo a escala: de una consulta por columna a una por lote

La segunda corrida contra bases reales dejo una sola reserva seria, y
era esta: una tabla de decenas de millones de filas no terminaba de
perfilarse ni en `modo = "muestreado"`. La causa no era el muestreo sino
la cantidad de escaneos: el paquete emitia **una consulta por columna**
para cada bloque de metricas.

- Los agregados planos —conteos, minimo/maximo/media/ceros/negativos, y
  desvio— se piden ahora para **varias columnas en una sola consulta**,
  por lotes. La moda y la mediana siguen siendo una por columna, porque
  agrupan y ordenan.

- Medido contra PostgreSQL 16 con **2 millones de filas por 40
  columnas**:

  | modo      | antes                 | despues                 |
  |-----------|-----------------------|-------------------------|
  | `conteos` | 46 consultas, 5,4 s   | **8 consultas, 2,4 s**  |
  | `seguro`  | 128 consultas, 15,2 s | **14 consultas, 5,3 s** |

  Con las mismas 160 y 400 metricas calculadas.

- **Y los numeros no cambian**: sobre la misma tabla sembrada una sola
  vez, el perfil consolidado y el anterior coinciden en los dieciseis
  campos del resumen para seis tipos de columna, y en los noventa
  estados por metrica.

- **Si un lote falla, no se pierde el lote.** Se reintenta columna por
  columna, y lo que igual falle queda `no_disponible` con su motivo
  mientras las vecinas se calculan. Una consulta compartida es la forma
  perfecta de reintroducir el reflejo de todo-o-nada que el paquete
  corrigio en cinco lugares, asi que la degradacion se construyo desde
  el principio y tiene sus propios tests.

- `resumen_tabla$sql` conserva **una fila por columna y metrica** con
  todos sus campos, y agrega `lote` y `columnas_compartidas` para que se
  vea cual consulta fue compartida.
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  publica `tamano_lote`, y su total pasa a estar declarado como
  **techo** en `attr(plan, "supuesto")`: se contaba una mediana y un
  desvio por columna numerica, y una columna sin valores validos no los
  emite. Se afirmo durante varias rondas que el plan predecia exacto;
  era cierto sobre tablas con datos en todas las columnas y falso en
  cuanto aparece una vacia. La version anterior erraba por tres
  consultas en ese caso y esta por una, asi que no es una regresion: es
  una afirmacion que venia siendo mas fuerte que el codigo.

- En SQLite con tablas chicas el ahorro de tiempo es casi nulo: ahi
  domina el costo de R y no los escaneos. Queda dicho porque una
  medicion que no distingue las dos cosas invita a concluir de mas.

### Lo que rompio la refutacion sobre estos mismos cambios

- **Una conversion que pierde el valor ya no se publica como
  `calculado`.** SQLite responde el `MIN` de una columna declarada
  `DATE` como el texto `"2020-01-01"`;
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) lo convierte en
  `NA` y el estado quedaba `calculado`. Decir “se midio” y “no se midio”
  a la vez sobre el mismo campo. Ahora la metrica queda `no_disponible`
  con el valor original del motor en el motivo. Lo mismo para un
  `integer64` cuyo paso a doble lo cambiaria: el maximo publicado no
  estaria en la columna.
- **La guarda de exactitud de `integer64` tenia un agujero de un solo
  numero**: comparaba el doble ya convertido contra 2^53, y 2^53+1
  redondea justo a 2^53, asi que pasaba. Ahora se comprueba con la
  vuelta completa -a doble y de vuelta a entero-, que no depende de
  donde caiga el redondeo.
- **`meta$muestras_independientes` decia algo que la consolidacion
  volvio falso.** Las columnas de un mismo lote comparten consulta y por
  lo tanto comparten filas: sus metricas son comparables entre si, y las
  de lotes distintos no. El campo dice ahora las dos mitades.
- **El total de
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  pasa a estar declarado como techo.** Se cuenta una mediana y un desvio
  por columna numerica, y una columna sin valores validos no los emite.
  La version anterior a la consolidacion erraba por tres consultas en
  ese caso y esta por una: no es una regresion, es una afirmacion que
  venia siendo mas fuerte que el codigo.

### Cuatro correcciones de honestidad

Las cuatro salieron de mirar los datos crudos de la corrida real, y tres
de ellas de reproducir lo que el informe atribuia a otra causa.

- **La trazabilidad acepta `integer64`.** Un `bigint` llegaba a R como
  `integer64` y la trazabilidad lo rechazaba: el hallazgo se publicaba y
  la guarda tenia que avisar que no habia con que nombrar las filas. Se
  atribuyo a las geometrias, pero la lista incluia `outliers` sobre
  columnas que no tienen nada de espacial. **Afecta a cualquier
  `bigint`.** Por encima de 2^53 la traza no se entrega, porque la
  conversion deja de ser exacta y una fila mal senalada es peor que una
  sin senalar.
- **Cuando el motor dice que es permiso, el mensaje lo dice.**
  `dbExistsTable()` no distingue una tabla inexistente de una sin
  permiso, y el mensaje repetia esa duda incluso cuando el motor habia
  respondido `permiso denegado a la relacion`. En una corrida real
  fueron veintitres tablas descritas como inciertas con la respuesta en
  la mano. El texto del motor se conserva: el diagnostico no reemplaza
  la evidencia.
- **El hallazgo `faltantes` nombra la senal estructural.** Cuando
  `posible_ausencia_estructural` dispara sobre una columna, el
  `faltantes` de esa columna dice en su evidencia que existe esa lectura
  alternativa. **La severidad no se toca**, y eso se decidio con un caso
  en contra: en una tabla pivoteada la correlacion entre el mes y la
  columna del ano es real, y un mes sin dato puede ser un hueco genuino.
  Degradar ahi lo esconderia.
- **El objeto declara que las metricas muestreadas no comparten filas.**
  Estaba en la vineta, y un consumidor automatico lee el objeto. Aparece
  en `meta$muestras_independientes` solo en `muestreado` y `aproximado`;
  en los modos que miden sobre la tabla entera no hay nada que advertir.

### Leer un perfil sin conocer su forma, y saber que falta para cada motor

- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  y
  [`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  leen las cuatro formas de salida del paquete sin depender de como
  estan armadas. `perfil$general$filas` funcionaba sobre la salida en
  memoria y devolvia `NULL` sobre la salida DBI, donde el conteo vive en
  `resumen_tabla$meta$filas`; un `NULL` silencioso en un guion de
  medicion no avisa, y lo que sigue calcula sobre nada. **No inventan lo
  que no hay**: un perfil DBI sin muestra leida devuelve una tabla de
  hallazgos vacia con su aviso, y
  [`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  sobre un perfil en memoria devuelve `NULL` porque no se emitio SQL.
- [`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
  contesta que hace falta para hablar con cada motor antes de chocarse:
  el paquete de R, la biblioteca del sistema con su nombre en Debian y
  en Fedora, **la alternativa sin permisos de administrador** cuando
  existe, el dialecto esperado y si esta probado contra motor real. Los
  errores de conexion se traducen: un
  `Can't open lib ... file not found` de ODBC pasa a decir que falta
  `unixodbc-dev` y cual es la salida sin `sudo`.
- Un controlador que no implementa `dbIsValid()` ya no se toma por
  conexion rota: se prueba `dbGetInfo()` antes de rendirse. El `ROracle`
  archivado es el caso, y por eso Oracle quedaba fuera de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  aunque el SQL funcionara.

### Oracle contra motor real

- Verificado contra Oracle Free 23. Importa aparte porque **sus dos
  dialectos -`fetch_first` y `rownum`- nunca habian corrido contra un
  motor real**; los otros seis usan `limit` o `top`. Encontro cuatro
  cosas: la sonda del desvio necesitaba `FROM DUAL`; Oracle rechaza
  `TABLESAMPLE` y usa `SAMPLE (p)`, que se agrego como forma candidata
  con su sonda; `dbExistsTable()` del `ROracle` archivado devuelve falso
  para nombres calificados aunque el SQL funcione; y una columna `CLOB`
  no se puede agrupar ni ordenar, cosa que el paquete ya declaraba como
  no disponible sin haberlo previsto.
- Con esto son **siete motores probados contra motor real**, y los siete
  encontraron algo que ningun motor simulado habia encontrado.

### Lo que encontro una refutacion adversarial

Esta tanda se reviso al reves: buscando romper cada afirmacion en vez de
confirmarla. Encontro ocho defectos, y los tres peores tenian la misma
forma: **la tabla con la que se verificaban los motores era comoda**.
Tenia 5.000 filas de tipos faciles, sin fecha nativa, sin enteros sin
signo y nunca mas chica que la muestra pedida. Es el mismo error que el
paquete ya persigue en los demas —el fixture que comparte la propiedad
cuya ausencia es el fallo— aplicado al propio verificador.

- **Una columna `DATE` se media como numero, con estado `calculado`.**
  El `dbFetch(n = 0)` de RMariaDB devuelve `numeric(0)` para una fecha:
  la clase se pierde junto con las filas, e
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html) decia que si.
  Salian `minimo` en dias desde 1970 y `media` en YYYYMMDD -dos unidades
  distintas, las dos publicadas como la misma-. Ahora el tipo declarado
  por el motor manda sobre el prototipo.
- **Un `BIGINT UNSIGNED` cerca del tope daba `maximo` menor que
  `minimo`**, los dos `calculado`. Habia guarda de coherencia para “mas
  distintos que validos” y no para un rango imposible. Ahora tambien.
- **El muestreo extrapolaba dividiendo por las filas pedidas y no por
  las obtenidas.** Pedir mil filas de una tabla de diez daba
  `n_validos = 0` y `sin_valores` sobre una columna llena, con
  `fraccion = 1` al lado contradiciendolo. El tamano que se informa y
  que divide es el efectivo.
- **El objeto declara con que criterio se comparo.** Un cotejamiento que
  ignora la caja hace que el resumen SQL cuente dos valores distintos
  donde el perfil de muestra cuenta cuatro, sobre las mismas filas. Los
  dos numeros son ciertos en su propia comparacion; faltaba que el
  objeto dijera cual usa cada bloque.
- **La cobertura de una parte incompleta ya no se pierde al subir de
  nivel.** Un conjunto armado con una organizacion a la que le falto una
  coleccion decia cobertura 1. Ahora hereda `cobertura_de_partes` y
  marca `completo = FALSE`.
- **El renombre de partes rompia la composicion**:
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  escribia el nombre del objeto y el nivel de arriba comparaba contra el
  declarado. Los dos identifican a la misma parte.
- Una parte con **peso cero** entraba a la cobertura sin aportar al
  numero, y ahora se declara.
- **`posible_ausencia_estructural` no disparaba con mas de 20 niveles**,
  o sea no veia `edad >= 65`, que es el caso mas frecuente de todos. De
  ahi salio una capacidad nueva: un corte numerico o de fecha tambien se
  ofrece como regla, con la formula escrita en el tipo correcto
  -`~ alta >= as.Date("2021-01-01")`, no `>= 18628`-.

`benchmark/verificar_motor.R` incorpora ahora los tipos y tamanos que
escondian esos defectos, para que la proxima tabla comoda no certifique
un motor que no lo esta.

### MariaDB, y las diez granularidades del marco

- Verificado contra MariaDB 11 real: los cinco modos sin ninguna metrica
  no disponible, los tres estadisticos coincidiendo con R, el plan
  exacto en los cinco. **Es el primer motor real que no encontro ningun
  defecto**, y tiene explicacion: habla el mismo protocolo que MySQL 8,
  que ya estaba verificado.
- [`organizacion()`](https://sebollin.github.io/lupa/reference/organizacion.md)
  declara que colecciones pertenecen a un organismo, y con eso
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  mide las granularidades novena y decima del marco. Que bases
  pertenecen a que organismo **no esta en los datos**, asi que lo
  declara quien lo sabe; es el mismo mecanismo que ya usaba el conjunto
  de colecciones.
- **Los dos niveles institucionales son opcionales.** Un analisis de
  calidad no siempre tiene una organizacion detras -una entrega suelta,
  un archivo que alguien mando, una base sin dueno declarado- y nada
  obliga a pasar por ellos. Sin declaracion,
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md) se
  niega y explica como declararla, que es distinto de inventar una
  frontera que nadie nombro.
- La politica de pesos vale para los cuatro niveles con frontera:
  promediar organismos de tamano distinto sin declararlo es el mismo
  juicio inventado que el paquete se niega a hacer un piso mas abajo. Y
  el numero viaja con su cobertura: cuantas de las partes declaradas
  entraron efectivamente.

### DuckDB, y una sonda que mentia

- Verificado contra DuckDB 1.5 real: los cinco modos corren sin ninguna
  metrica no disponible, la media, la mediana y el desvio coinciden con
  los calculados en R sobre la tabla entera, y los nombres calificados
  con punto y las colecciones de dos esquemas funcionan.
- **Y encontro un defecto que ningun motor simulado podia encontrar.**
  DuckDB acepta `TABLESAMPLE SYSTEM (10) WHERE 1 = 0` y rechaza la misma
  clausula sin el filtro: con un filtro trivialmente falso el parser no
  llega a validar el metodo de muestreo. La sonda de capacidad usaba
  justo ese filtro para salir barata, asi que declaraba disponible una
  forma que el motor despues rechazaba. Ahora la sonda emite la forma
  real acotada por el limite del dialecto. **Una sonda que no ejercita
  la forma que despues se emite no prueba nada**, que es la misma
  leccion de la sonda del desvio, una ronda antes.
- El muestreo prefiere las formas de tamano predecible: primero la de
  **cantidad fija** —`TABLESAMPLE RESERVOIR (n ROWS)`—, despues la de
  **nivel de fila** —`TABLESAMPLE BERNOULLI (p)`—, y solo al final las
  de bloque. Medido contra PostgreSQL 16 pidiendo el 20 % de una tabla
  de 5.000 filas: `SYSTEM` devolvio 678, 904, 452 y 1.384 filas en
  cuatro corridas; `BERNOULLI`, 1.011, 1.017, 981 y 1.050. Un tamano que
  no se puede anticipar hace que dos metricas del mismo perfil dejen de
  ser comparables. Medido: `TABLESAMPLE (20 PERCENT)` en DuckDB es a
  nivel de bloque y devuelve `0` o `2048` filas sobre una tabla de
  5.000, asi que dos consultas del mismo perfil veian muestras de tamano
  distinto, y la guarda de coherencia declaraba no disponible una moda
  cuya frecuencia superaba unos validos que valian cero. Con la forma de
  cantidad fija, las cuatro metricas que caian vuelven a calcularse.
- El motor simulado que reproduce la trampa esta en la suite, asi que la
  regresion queda cubierta sin necesidad de DuckDB instalado.

### La senal que faltaba: nadie declara lo que no sabe que existe

- `posible_ausencia_estructural`, severidad `ok`. `aplicabilidad`
  resolvia el vacio por diseno y funcionaba, pero exigia que el usuario
  supiera que existe: quien perfilaba una tabla con columnas
  condicionadas sin declarar nada recibia el mismo informe enganoso que
  antes. Ahora, cuando el valor de una columna decide que filas tienen
  otra —`cumplimiento >= 0.99`—, o cuando dos o mas columnas se reparten
  las filas sin pisarse, el hallazgo lo dice con la evidencia medida y
  **la linea exacta que habria que escribir**. Sugiere; no decide, y no
  reescribe el universo por su cuenta. Las columnas ya declaradas quedan
  fuera del examen.
- Medido antes de encenderlo: sobre veinte conjuntos que vienen con R y
  sesenta tablas al azar con ausencia independiente produce **cero**
  senales, y dispara en el modelo entidad-atributo-valor, en el salto de
  patron de una encuesta y en las columnas excluyentes. Con 10 % de las
  filas fuera de la regla se calla, porque entonces la relacion existe y
  no es una regla. Cuesta 0,11 s sobre 200 columnas por 20.000 filas.
- `regla_silencia_ausencia`, tambien `ok`. Declarar opcional una columna
  con 80 % de ausentes dejaba el perfil limpio y la cobertura lo
  documentaba, pero quien no la leyera no se enteraba. El aviso existe
  para que eso sea una decision y no un efecto de la declaracion.
- `columnas_personales` declara que columnas traen datos personales, con
  tipo o sin el. Ningun lexico de nombres puede ser completo —una
  columna con documentos se puede llamar `cod_benef`— y esta es la
  salida correcta a ese limite: lo declara quien conoce el dato, gana
  sobre lo inferido, y no se vuelve a examinar.
- `dato_personal_protegido` dice si el valor **quedo** protegido, no si
  la clasificacion pensaba protegerlo. Con
  `proteger_datos_personales = FALSE` la moda se ve, y decir `TRUE` al
  lado de un valor visible era informar como hecho algo que no paso. La
  intencion sigue en `datos_personales$proteger`.
- Un correo ofuscado —`usuario at ejemplo punto com`— vuelve a ser un
  correo para la clasificacion, aunque
  [`validar_correo()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  siga diciendo con razon que no es un correo valido. Son dos preguntas
  distintas: una mide la forma, la otra decide si hay dato personal. La
  frase `lunes at casa` no entra: el dominio tiene que traer su
  separador.
- Una matriz de dos dimensiones es una tabla y
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  la acepta. La conversion queda declarada en `meta$entrada_convertida`.

### La via DBI sobre tablas que no entran en memoria

- Modos `muestreado` y `aproximado`. El primero muestrea **en el motor**
  —`TABLESAMPLE` donde existe, orden pseudoaleatorio con limite donde
  no—; el segundo usa las funciones aproximadas nativas
  —`APPROX_COUNT_DISTINCT`, `PERCENTILE_CONT`, `approx_quantile`— con la
  misma mecanica de capacidad declarada y resuelta por sonda que ya
  usaba el dialecto.
- Toda metrica muestreada o aproximada viaja diciendolo: `estado`
  distingue `calculado`, `estimado` y `no_disponible`, y cada fila lleva
  `universo`, `tamano_muestra`, `fraccion`, `metodo` y `error_esperado`,
  que es `desconocido` cuando el motor no documenta una cota. **Nunca
  una cota inventada.**
- El conteo de distintos tiene estado propio, `observado_muestra`. La
  cardinalidad de una muestra no estima la del universo sin un estimador
  declarado, asi que se informa por lo que es, con el universo al lado.
- [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
  predice **exactamente** las consultas de los cinco modos. Las sondas
  nuevas gastan un numero fijo aunque acierten en la primera forma, por
  la misma razon que la del desvio: un costo que dependa del motor hace
  que el plan deje de predecir.
- Los conteos conservan `integer64` cuando `bit64` esta instalado, asi
  que un conteo por encima de 2^53 deja de perder exactitud. Sin
  `bit64`, `meta$conteo_exacto` lo sigue declarando.

### Bases enteras, y el costo de compararlas

- [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
  y
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  aceptan `columnas_candidatas`: declarar que columnas pueden participar
  es lo que hace manejable un costo que crece con el producto de anchos.
  En una prueba de 32 por 32 columnas, 1.024 combinaciones bajan a 9.
- **Dos clases de poda, y no se tratan igual.** Dos columnas de la misma
  familia con rangos numericos disjuntos no comparten ningun valor y eso
  se sabe sin comparar: la fila sale como siempre y la comparacion se
  ahorra. Esa poda esta siempre activa porque no cambia nada de lo
  informado. Las otras dos —familias distintas, cardinalidad imposible—
  si lo cambiarian: una columna de texto puede guardar `"2020-01-05"` y
  coincidir con una de fecha, y una cardinalidad imposible no dice que
  no haya coincidencias sino que no llegan al umbral. Van detras de
  `podar = TRUE`, y cuando se aplican **el par no desaparece**: sale con
  `cardinalidad = "sin_comparar"`, coberturas `NA` y su motivo. Un par
  que no se evaluo no es un par sin relacion.
- `tope_memoria_mb` acota las filas comparadas y declara los pares
  pendientes en vez de devolver menos sin decirlo.
- La granularidad `conjuntoColecciones` pasa a medirse, con la frontera
  declarada por el usuario y pesos explicitos: agregar entre colecciones
  sin pesos seria inventar un juicio, que es lo que
  [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  se niega a hacer. `organizacion` y `conjuntoOrganizaciones` siguen sin
  implementar, y por la misma razon de siempre: no falta codigo, falta
  el objeto.
- La entrada `data.frame` de
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  valida `NA`, cadenas vacias y tipos igual que la entrada por vector de
  texto. Dos puertas del mismo paquete dejaron de comportarse distinto
  ante la misma entrada mala.

### Lo que el detector de vocabulario no puede ver, dicho

- `n_grupos_sin_variante_rara` cuenta los grupos de formas cercanas que
  el criterio de variante rara **nunca llego a formar**. Una variante
  mal escrita que ocupa la mitad de la columna no es una variante rara
  para el comparador y no se informaba; ahora el limite se declara
  aunque la deteccion no cambie.
- `variantes_equifrecuentes_vocabulario` es el diagnostico para ese
  caso: dos formas cercanas que se reparten la columna sin que ninguna
  sea dominante, que es la firma de dos operadores, una plantilla rota o
  una migracion parcial. **Queda apagado por omision, y la razon esta
  medida**: sobre la bateria de 31 tablas limpias produce un grupo
  sospechoso donde no hay defecto, y dispara en tablas de menos de
  veinte filas. Es aditivo: encenderlo no cambia ni pierde ninguna
  deteccion de `casi_duplicados_vocabulario`.
- La evidencia de `patron_raro` declara
  `desvio_unicamente_largo_corrida_numerica` cuando el unico desvio es
  la cantidad de digitos. No baja el ruido de trescientos correos
  correlativos —eso no tiene solucion sin dominio— pero convierte una
  lectura de dos segundos en una de cero.
- La razon de permutacion viaja como evidencia descriptiva del detector
  de orden. No filtra nada: el criterio quedo refutado con precision 0 %
  en cuatro tablas reales y no se usa para decidir.

### Costos declarados donde antes solo se tardaba

- `max_comparaciones_dependencias` acota la busqueda de dependencias
  funcionales, cuyo costo es del orden de `columnas^2 x filas` y empeora
  con determinantes casi unicos. Cuando el presupuesto se agota, lo
  comparado se informa y lo que quedo sin comparar se declara.
- La deteccion de fechas partidas dejo de materializar el producto
  cartesiano de los candidatos ano/mes/dia; el detector de vocabulario
  dejo de recorrer el vocabulario completo antes de aplicar su tope. Los
  dos declaran lo que no evaluaron.
- La confirmacion de un validador de documentos deja de recorrer la
  columna entera sin presupuesto. Cuando el tope se alcanza, el
  fundamento dice sobre cuantos valores se confirmo.
- `datos[0, 0]` sobre un objeto `sf` conserva la geometria, porque esa
  columna es pegajosa por diseno de ese paquete. Con las dependencias
  apagadas, el objeto vacio llegaba con una columna y el diagnostico
  declaraba un recorte que nadie pidio.
- Un par no comparado trae cobertura `NA`, y `datos[NA, ]` devuelve una
  fila entera de `NA` en vez de ninguna.
  [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  filtra con [`which()`](https://rdrr.io/r/base/which.html).

### Infraestructura

- `inst/WORDLIST` completa la lista que faltaba:
  `spelling::spell_check_package()` vuelve cero. Las palabras son
  nombres propios, siglas, terminos tecnicos y fragmentos de
  identificadores del paquete.
- `CONTRIBUTING.md` corrige el orden de la verificacion previa.
  `test_dir()` y `test_file()` cargan el paquete con
  [`library(lupa)`](https://github.com/sebollin/lupa), que no expone las
  funciones internas y produce veinte errores falsos de «could not find
  function»; `test_check("lupa")` es lo que corre `R CMD check`, y
  necesita el paquete instalado.

### Cuatro motores reales

- El desvio se pide primero con la funcion nativa del motor
  —`STDDEV_SAMP` del estandar, `STDEV` en SQL Server— y solo cae al
  calculo de dos pasadas donde no existe ninguna de las dos. La forma
  anterior ponia la media como subconsulta escalar para no incrustarla
  como literal en el SQL guardado, y SQL Server rechaza una subconsulta
  dentro de un agregado: el arreglo de privacidad habia roto la
  compatibilidad, y solo un motor real podia mostrarlo.
- Verificado contra PostgreSQL 16, MySQL 8, SQL Server 2022 y SQLite: en
  los cuatro, ninguna metrica queda no disponible, y la media, la
  mediana y el desvio calculados por el motor coinciden con los
  calculados en R sobre la tabla entera. En SQL Server la sonda resuelve
  el dialecto `top` por su cuenta.

### Numeros que no pueden ser

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  resuelve un nombre calificado con punto igual que
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).
  `dbExistsTable()` no lo resuelve, asi que el mismo texto funcionaba en
  una funcion y fallaba en la otra diciendo que la tabla no existe. Un
  nombre literal con punto adentro sigue teniendo prioridad.

- El universo aplicable declarado sale tambien del analisis, no solo de
  los conteos. Con filas no aplicables que tienen valor, `n_distintos`
  las contaba mientras `n_validos` ya no, y `tasa_distintos` podia pasar
  de 1.

- La trazabilidad no nombra filas fuera del universo declarado. El
  conteo ya las excluia y nombrarlas igual producia la incoherencia que
  la guarda detecta.

- La via DBI valida la coherencia interna de lo que informa el motor:
  mas valores distintos que validos, o una frecuencia de moda mayor que
  las filas validas, son imposibles y se declaran no disponibles en vez
  de publicarse como calculados.

- El lexico de nombres de columna con datos personales cubre `persona`,
  `cliente`, `paciente`, `socio`, `beneficiario`, `titular`,
  `funcionario`, `usuario`, `solicitante`, `responsable`,
  `contribuyente`, `residencia`, `lugar_residencia` y `barrio`. Ningun
  lexico puede ser completo; estos son los frecuentes en registros
  administrativos.

### La via DBI deja de asumir un dialecto y de tirar lo que ya midio

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  resuelve el dialecto con una sonda de cero filas **antes** de emitir
  el bloque de agregados: `limit`, `top`, `fetch_first`, `rownum` y una
  via portable con `dbSendQuery()` + `dbFetch(n)`. Se puede declarar con
  `dialecto =` si la sonda no acierta.
- Las cuatro consultas obligatorias —campos, conteo, esquema y muestra—
  dejaron de ser fatales. Si la muestra falla, el objeto vuelve con
  `resumen_tabla` completo, `perfil_muestra = NULL` y una fila de
  cobertura con el motivo. Antes, un motor que no acepta `LIMIT`
  descartaba las 777 consultas ya pagadas.
- El esquema y la muestra enumeran columnas en vez de usar `SELECT *`, y
  si la lectura conjunta falla sondean columna por columna para
  descartar solo la que el motor rechaza.
- Los [`stop()`](https://rdrr.io/r/base/stop.html) de la via DBI tienen
  clase de condicion propia, asi que un fallo se puede atrapar y el
  resumen rescatar.
- Los alias se comillan y se comparan sin distinguir caja. Un motor que
  los pliega a mayusculas ya no produce metricas con estado `calculado`
  y valor vacio, que era peor que declararlas no disponibles.
- Argumentos nuevos para acotar el costo: `modo`, `metricas` y
  `max_consultas`, mas
  [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
  que dice cuantas consultas va a costar el perfilado antes de
  emitirlas.
- `resumen_tabla` pasa por la proteccion de datos personales, que antes
  solo alcanzaba al perfil de la muestra: el bloque sin proteger era
  justamente el de alcance completo. El SQL guardado del desvio ya no
  incrusta la media observada. Nuevo `print.perfil_dbi`, que no imprime
  ningun valor de celda.

### El nivel coleccion deja de informar cero donde no midio

- Una tabla vacia ya no produce `prop_faltantes_maxima = -Inf` ni
  `n_columnas_sin_faltantes = 0`: son `NA`, con una fila de cobertura
  que declara que no hay nada que medir. Antes esa tabla se ordenaba
  como la de mejor calidad de la base.
- Componente nuevo `cobertura_metricas`: la declaracion de lo que el
  motor rechazo sube al nivel coleccion antes de descartar los perfiles,
  y existe tambien con `conservar_perfiles = FALSE`.
- [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  usa la formula cerrada en vez de materializar los pares: 27,4 s y 233
  MB con 1700 tablas pasaron a 0,42 s. El resultado es identico. Acepta
  cero pares, deduplica y rechaza los autorreferenciales.
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  cachea cada tabla en vez de releerla una vez por par.
- Los identificadores de mas de dos partes se rechazan nombrando la
  causa real. Antes se aceptaban y el fallo se le devolvia al usuario
  como un problema de permisos sobre una tabla que si podia leer.

### El perfilado espacial deja de ser inviable por tiempo

- Las columnas no atomicas ya no pasan por la maquinaria de texto.
  Convertirlas no producia sus valores sino su representacion como
  codigo, una vez por cada etapa que las tocaba: era el 85 % del costo
  de perfilar una capa espacial. Perfilar 62 poligonos de 200.000
  vertices paso de 323 s a 2,3 s.
- La transformacion de coordenadas se hace en una sola llamada y no una
  por geometria.
- Presupuesto de geometrias y de vertices, con el recorte declarado.
- WKT, WKB y hexadecimal se detectan, se convierten y se miden. Antes
  quedaban todas las metricas en `NA` con `cobertura_diagnosticos`
  vacia, y
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  llegaba a afirmar que la geometria no aplicaba sobre datos que si eran
  geometricos.
- Un fallo parcial ya no descarta la columna entera: una geometria
  intransformable o un `NA` de la validez dejan de borrar el conteo y
  los indices de todas las demas.

### El vacio por diseno se declara y deja de contarse como defecto

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `aplicabilidad`, una lista de formulas por columna que declara
  en que filas la columna corresponde. Las filas fuera del universo
  salen de `n_faltantes` y de `prop_faltantes` en vez de contarse como
  ausencia. Antes, una tabla completa en las filas donde el dato
  corresponde podia informar completitud baja: el conteo era correcto y
  la lectura falsa.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `columnas_opcionales` para el caso mas simple, donde la
  ausencia nunca es defecto y no hay una regla que escribir.
- La regla declarada, el universo resultante y las filas donde la regla
  no se pudo determinar quedan en `cobertura_diagnosticos`. Un universo
  recortado sin constancia seria el mismo defecto al reves.
- Las filas donde la regla no se puede evaluar no se cuentan como
  aplicables ni como no aplicables: van a
  `n_aplicabilidad_indeterminada`, porque no saber no es lo mismo que no
  corresponder.
- Nuevo hallazgo `valor_fuera_de_aplicabilidad`: un valor presente donde
  la regla dice que la columna no corresponde. Es el error simetrico y
  sin universo declarado no tenia forma de aparecer.
- La metrica `NoNulo` acepta `aplicable` con el mismo criterio, para que
  el universo declarado llegue al tablero y no solo al hallazgo.
- Nueva funcion
  [`perfilar_por()`](https://sebollin.github.io/lupa/reference/perfilar_por.md):
  perfila cada grupo de filas por separado y devuelve los hallazgos de
  todos los grupos en una tabla. Es la respuesta al formato largo, donde
  una sola columna mezcla dominios sin relacion. Las columnas
  enteramente ausentes dentro de cada grupo se descartan antes de
  perfilar, y el descarte se declara.
- Nueva vineta `vacio-por-diseno`, que documenta el supuesto tabular del
  paquete y las seis formas de tabla donde no vale.

### Privacidad: ante la duda se protege

- El clasificador de datos personales dejaba sin proteger una columna
  cuya forma era compatible con un documento de identidad cuando el
  validador no podia verificarla. Ese es justamente el caso de una base
  sucia, y los valores reales terminaban escritos en la evidencia de los
  hallazgos. Ahora se protege igual; la evidencia sigue declarando que
  la clasificacion es debil.

### Conteos que no se pueden contar

- `.moda_columna()` distinguia mal dos ausencias: la frecuencia cero de
  una columna sin valores validos y la imposibilidad de contar sobre una
  columna no atomica. La segunda ahora es `NA`.
- El hallazgo `constante` sobre una columna no atomica informa la
  frecuencia que se deduce de las filas validas y las nombra en la
  trazabilidad, en vez de informar cero afectados. El discriminador dejo
  de ser la etiqueta del tipo, que dejaba afuera a las columnas
  espaciales.

### Recortes declarados donde se los busca

- El recorte por `max_columnas_dependencias` se declara en
  `cobertura_diagnosticos`, como ya lo hacia el recorte hermano de la
  busqueda aritmetica. El tope aplicado se conserva como atributo, y el
  motivo aclara que la seleccion de columnas es por posicion.

### Patrones raros: ventana de operacion visible

- `patron_raro` declara en `cobertura_diagnosticos` cuando no puede
  ejecutarse porque el patron dominante no alcanza
  `umbral_patron_dominante`. La fila conserva la proporcion observada y
  explica como ajustar ese argumento.
- La evidencia de cada hallazgo `patron_raro` publica la proporcion del
  patron dominante y cuantas filas quedaron en patrones no dominantes
  excluidos por superar `umbral_patron_raro`. Ese conteo queda en la
  evidencia, no en la cobertura, porque no es una no medicion del
  diagnostico.

### Patrones raros y trazas accionables

- `patron_raro` conserva separado el tope de presentación y el alcance
  de la trazabilidad: `resumen_patrones` y la evidencia siguen mostrando
  como máximo seis patrones, mientras la traza usa sus nombres raros
  completos hasta un límite de 5.000. Si se alcanza ese límite,
  `cobertura_diagnosticos` y el alcance de la traza lo declaran.
- Las ausencias de una columna de lista se nombran, no sólo se cuentan.
  Una columna de listas —o un BLOB leído por
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)—
  informaba cuántos valores faltaban sin decir en qué filas, aunque
  [`is.na()`](https://rdrr.io/r/base/NA.html) los identifica elemento a
  elemento y es el mismo criterio con el que se contaron. Lo encontró la
  propia guarda de coherencia, que era exactamente para lo que se
  agregó.
- `casi_duplicados_vocabulario` entrega primero las filas de formas no
  dominantes y después las de formas dominantes. La unidad sigue siendo
  `valor_distinto`, el grupo sigue incluyendo la forma dominante y la
  evidencia informa cuántas filas mostradas pertenecen a cada tipo de
  forma.

### La traza de vocabulario y la guarda de coherencia cierran el circuito

- `casi_duplicados_vocabulario` conserva
  `unidad_conteo = "valor_distinto"` y ahora enumera las filas que
  contienen los valores de cada grupo seleccionado, incluida la forma
  dominante. La distancia sigue siendo una señal heurística, no una
  afirmación de identidad.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  conserva cualquier hallazgo cuya traza no coincida y emite la
  advertencia de clase `lupa_trazabilidad_incoherente`. La guarda
  compara el total anterior al límite de presentación, funciona en ambas
  direcciones y adapta la comparación a la unidad declarada.

### Conteos y trazabilidad dejan de mezclar unidades

- `mayusculas_inconsistentes` y `normalizacion_unicode` declaran
  `unidad_conteo = "valor_distinto"` y cuentan en `n_evaluados` los
  valores distintos evaluados. `n_afectados` ya contaba esos valores; su
  traza sigue siendo por fila y enumera todas las filas que contienen
  los valores afectados, no sólo las defectuosas.
- `filas_duplicadas` cuenta ahora todas las filas participantes de los
  grupos, en línea con `EntidadDuplicada` y con
  `marcar_filas_duplicadas`. La evidencia conserva el número de
  excedentes para la acción que elimina repeticiones.
- Una constante de listas cuya frecuencia no puede contarse informa `NA`
  en `n_afectados` y deja el motivo en `cobertura_diagnosticos`; una
  matriz no analizada enumera todas sus filas en la trazabilidad.

### La trazabilidad deja de recalcular lo que el detector ya decidió

Un hallazgo dice cuántas unidades afecta y, cuando puede, cuáles. Ese
«cuáles» lo resolvía una rama de índices aparte que en varios casos
aplicaba un criterio **distinto** del detector que había producido el
hallazgo. Los dos no coincidían, y el desacuerdo no se veía porque nada
comparaba la evidencia contra los índices.

- `patron_raro` no nombraba ninguna fila cuando la columna tenía algún
  patrón de frecuencia intermedia. La guarda comparaba el total de
  patrones distintos contra el tamaño del resumen, que son cosas
  distintas: el resumen es el patrón dominante **más los patrones
  raros**, no un top-N, así que se disparaba en una situación
  perfectamente normal. Ahora
  [`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
  expone `n_patrones_raros` y la guarda pregunta lo único que
  corresponde —si ese conjunto fue recortado por el tope de seis—. Sin
  recorte la enumeración es completa y `n_afectados` toma su valor real;
  con recorte se enumera igual y el alcance es `patrones_parciales`,
  declarado en la cobertura.
- `outliers`, `valores_no_finitos`, `ceros_no_permitidos` y
  `negativos_no_permitidos` condicionaban la enumeración a que la
  columna fuera numérica **en su tipo declarado**. Al leer un CSV como
  texto —el caso más común que hay— el perfilador infiere numérico,
  convierte y cuenta bien, pero la rama miraba un `character` y no
  devolvía nada: se informaban diez atípicos y no se nombraba ninguna
  fila. Ahora rastrean sobre la vista cuantitativa inferida, la misma
  que usó el detector.
- `codificacion_rota` reimplementaba la detección con una clase de
  caracteres más angosta que la del detector, de modo que un valor con
  el mojibake del carácter de reemplazo se contaba y no se nombraba —y
  ese vacío salía declarado con alcance `completo`, que es justo lo que
  este paquete no hace—. Ahora reutiliza la máscara del detector.
- `patron_raro` nombraba, además, filas que su propia evidencia acababa
  de descartar. En una secuencia entera densa, un patrón que difiere
  sólo por el largo —`9` frente a `9+`— no es un desvío: es el mismo
  número con menos dígitos. El detector lo filtraba al armar la
  evidencia; la rama de índices y `n_afectados` recorrían el resumen
  crudo. El conjunto filtrado se calcula ahora una sola vez y viaja con
  el resultado, de modo que no puede haber dos criterios.

**El principio que unifica los cuatro: la trazabilidad no recalcula lo
que el detector ya resolvió.** Cada vez que lo recalculaba, los dos
criterios se separaban en silencio.

**Y la prueba que faltaba.** La suite verificaba conteos, no
identidades: una prueba que comprueba `n_afectados == 10` pasa igual si
el paquete nombra diez filas equivocadas, ninguna, o seiscientas. Por
eso ninguno de estos desajustes se veía con toda la suite en verde.
Ahora hay fixtures que construyen tablas con índices corrompidos
**conocidos de antemano** y verifican aciertos, falsos positivos y
pérdidas.

### El piso de asimetría del vocabulario declara lo que deja afuera

- El comparador de vocabulario abría grupos por distancia con cualquier
  desbalance de frecuencias, y así señalaba `este` frente a `oeste` —dos
  puntos cardinales— como posibles variantes de un mismo valor. Medido
  sobre tablas limpias y sobre erratas sembradas, los falsos positivos
  quedan entre `1,0` y `1,5` de asimetría y las erratas reales desde
  `9,0`, así que ahora se exige una asimetría mínima de `2`,
  configurable con `min_asimetria_vocabulario`.
- **El piso no se aplica a los grupos formados por normalización.**
  `Montevideo`, `MONTEVIDEO` y `Montevideo` son tres grafías del mismo
  valor y con una aparición cada una su asimetría es `1,0`: ahí la
  equivalencia está comprobada y no es una conjetura sobre una errata.
- **Y lo que el piso deja afuera se declara.** En la banda de asimetría
  baja cae también una errata sistemática que afecte a una fracción
  grande de los registros, y por la forma es indistinguible de dos
  valores legítimamente parecidos. Elegir en silencio cuál se sacrifica
  sería justo lo que este paquete no hace: `cobertura_diagnosticos`
  informa cuántos grupos quedaron bajo el piso y cómo bajarlo.

### Colecciones: el séptimo nivel de granularidad deja de estar sólo declarado

- [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  declara qué tablas componen una base de datos, con su esquema, y
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  devuelve una fila por tabla con sus agregados exactos, más
  `cobertura_coleccion` con lo que no se pudo medir. La granularidad
  `coleccion` del marco estaba declarada y no se medía: lo que faltaba
  no era código sino el objeto.
- **La frontera se declara, nunca se descubre.** Recorrer el catálogo
  convertiría un error de permisos en un resultado, y una colección real
  pasa de mil tablas repartidas en decenas de esquemas.
- **El esquema es parte de la identidad de la tabla**, así que el mismo
  nombre en dos esquemas son dos tablas y no una repetida.
- **Lo que no se pudo leer se declara y nunca queda en cero**: una tabla
  sin permiso, un objeto declarado como vista, un motor que rechaza un
  agregado. En bases institucionales los permisos parciales son el caso
  normal.
- **Cada tabla declara su propio muestreo**, y no se promedian alcances
  distintos como si fueran uno.
- **No hay lectura instantánea.** Perfilar una colección son muchas
  consultas y la base puede cambiar entre ellas, así que cada fila trae
  el `momento` en que se midió y `meta$snapshot` declara que no lo hubo.
- El perfil pesado de cada tabla no se retiene salvo que se pida con
  `conservar_perfiles = TRUE`: con cientos de tablas no entraría en
  memoria.
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  mide ahora esta granularidad, con tres condiciones. Exige la
  **frontera declarada**, porque sin saber sobre qué tablas se agrega el
  número no significa nada. Admite **sólo `promedio_ponderado`**: sin
  esa restricción bastaba pedir `promedio` para obtener un número entre
  tablas de universos distintos sin declarar nada, que es el juicio que
  el paquete se niega a inventar. Y **la cobertura viaja pegada al
  número**.
- Esa última condición es la que más importa, y salió de refutar el
  diseño. Un número sobre «la colección» calculado sólo con las tablas
  que se pudieron medir **informa como medido lo que no se midió**: el
  peso de la tabla ausente desaparece en vez de manifestar la falta de
  cobertura. Con quince tablas declaradas y seis sin permiso, el número
  describe nueve y se presenta como si describiera la colección. Ahora
  el resultado trae `tablas_declaradas`, `tablas_en_el_numero`,
  `tablas_sin_medir` con su motivo, la `cobertura` y la advertencia de
  que leerlo sin ella sería exactamente ese error.
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  busca claves foráneas candidatas entre **los pares que se declaren**,
  y
  [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  permite ver el costo antes. Los pares se declaran por la misma razón
  que la frontera: una clave foránea es **dirigida**, así que mil tablas
  dan casi un millón de direcciones, y el costo real no lo da el número
  de tablas sino el de comparaciones entre columnas. Cada par se compara
  sobre una muestra, y el objeto declara que **una relación candidata
  sobre una muestra no es una clave foránea comprobada**: es un indicio
  que hay que confirmar contra el diccionario de datos. Un par que no se
  pudo leer se declara en `cobertura_pares` en vez de desaparecer.
- [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  declara el séptimo nivel como implementado: siete de diez. Los tres
  últimos siguen sin objeto, y no por falta de código: qué bases
  componen un conjunto y qué bases pertenecen a un organismo son
  decisiones de gobernanza que no están en ningún dato.

### Evaluar estimaciones que calculó otra herramienta

- [`medicion_desde_estimaciones()`](https://sebollin.github.io/lupa/reference/medicion_desde_estimaciones.md)
  recibe estimaciones ya calculadas —por `survey`, por
  [`calidad`](https://github.com/inesscc/calidad) del INE de Chile, o
  por cualquier otra fuente— y las lleva al contrato de
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), para
  poder evaluarlas contra un marco declarado. **`lupa` no estima**: eso
  necesita diseño muestral, estimación de varianza y otra disciplina; lo
  que sabe hacer es evaluar contra un marco, y eso es lo que ofrece.
- Cada estadístico se convierte en **su propia medida canónica**, con su
  métrica, su tipo, su unidad y su orientación, porque los siete tienen
  dominios distintos: un coeficiente de variación de `0,30` y un tamaño
  de muestra de `0,30` no se leen igual.
  [`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md)
  publica el catálogo.
- **La procedencia viaja en cada medida** y es obligatoria, para que
  nadie lea el resultado como si `lupa` lo hubiera calculado. Los
  estadísticos que la tabla no traiga no se rellenan con ceros: se
  declaran ausentes.

### Señales redundantes: la contradicción que ninguna columna muestra sola

- [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  declara que varias columnas de una tabla codifican el mismo hecho, y
  [`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md)
  informa las filas donde no concuerdan dentro de la ventana declarada.
  El caso típico son el año de la fecha, el año fiscal y el año del
  archivo: los tres pueden ser plausibles por separado y aun así
  contradecirse.
- **El grupo se declara, nunca se adivina.** Dos columnas de año pueden
  ser el de nacimiento y el de ingreso, y no tienen por qué coincidir;
  suponerlo sería inventar conocimiento del dominio.
- `transformacion` lleva columnas guardadas de formas distintas a una
  escala comparable —extraer el año de una fecha, por ejemplo—, y
  `ventana` es la tolerancia **en las unidades del valor comparado**,
  que no se adivina.
- Una fila con alguna columna ausente **no cuenta como desacuerdo**:
  sale del universo, y `n_evaluadas` lo declara. Si ninguna fila tiene
  todas las columnas presentes, `n_discordantes` queda en `NA` y la
  señal se declara no evaluada, en vez de informar cero discordancias.

### Los umbrales de una regla salen del closure y se pueden consultar

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `umbrales`, una lista con nombres que se le pasan a la
  condición al evaluarla. Antes el umbral quedaba encerrado en el
  *closure*: para mover un número había que escribir otra regla, y nadie
  podía consultar cuál era. Ahora la misma función evalúa distinto con
  dos umbrales —0,67 y 0,33 sobre los mismos valores— sin reconstruir la
  lógica.
- Una condición que no recibe un umbral declarado se rechaza enumerando
  los argumentos que sí acepta, en vez de ignorarlo en silencio. Una
  condición con `...` los recibe todos.
- **[`propiedades_regla()`](https://sebollin.github.io/lupa/reference/propiedades_regla.md)**
  muestra lo que una regla declara: métricas, nivel, proporción mínima,
  desenlace y umbrales. Es la contraparte de
  [`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
  que describe métricas: un umbral pertenece a una regla y no cabía
  allí.

### Trazabilidad por clave declarada: del hallazgo que se lee al que se verifica

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  acepta `clave` con las columnas que identifican una fila. La
  trazabilidad de cada hallazgo trae además el valor de esas columnas
  para las filas señaladas, así que el caso se puede buscar en el
  sistema de origen sin abrir la tabla.
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  lo traslada por `...` y por `argumentos_perfil`.
- La trazabilidad separa dos ejes que antes se confundían: `estado`
  —`disponible`, `truncada`, `no_disponible`, `no_aplica`— dice si se
  pudo localizar y hasta dónde, y **`localizador`** —`indice_fila`,
  `clave_declarada`, `ninguno`— dice con qué. Una trazabilidad puede ser
  al mismo tiempo por clave y truncada.
- Las claves viajan como data frame, una fila por índice mostrado y una
  columna por componente: concatenarlas perdería los tipos y haría
  ambigua una clave compuesta.
- **La clave que permite verificar es la que identifica a una persona.**
  Si alguna de sus columnas se clasifica como dato personal y la
  protección está activa, sus valores salen enmascarados igual que la
  evidencia, en todos los hallazgos —la clave viaja con la fila, no con
  la columna del hallazgo— y `claves_protegidas` declara cuáles se
  enmascararon.
- Una clave que nombra columnas inexistentes se rechaza enumerando las
  disponibles; una que no es única avisa y sigue, porque sirve igual
  para localizar aunque deje de ser una clave.

### La cobertura del vocabulario deja de contradecir al hallazgo

- `casi_duplicados_vocabulario` nombraba dos diagnósticos distintos:
  agrupar valores por su forma normalizada, que no depende de nada, y
  medir proximidad por distancia de edición, que necesita `stringdist`.
  Sin ese paquete el primero medía y el segundo se declaraba **bajo el
  mismo nombre y para la misma columna**, así que cruzar
  `cobertura_diagnosticos` con `hallazgos` por `(diagnostico, columna)`
  —el uso natural para un consumidor automático— devolvía una
  contradicción: el mismo diagnóstico declarado como no evaluado y
  reportado como medido.
- La cobertura pasa a llamarse **`proximidad_vocabulario`** en las tres
  razones que le corresponden: falta `stringdist`, el vocabulario excede
  el alcance de comparación, y el grupo candidato mayor abarca tanto que
  el diagnóstico no aplica. El hallazgo conserva su nombre. Quien filtre
  la cobertura por el nombre viejo tiene que actualizar el filtro.

### Dos cosas más que el objeto ahora declara

- `patron_raro` distingue en la evidencia las dos clases de desvío:
  `clase_desvio=largo_de_corrida` cuando el valor señalado sigue el
  mismo patrón con un número de otro largo —`persona9@` frente a
  `persona300@`— y `clase_desvio=estructural` cuando es otra forma
  —`SIN CODIGO` frente a `AB-12345`—. **La severidad no cambia**: los
  dos casos son indistinguibles por la forma y eso está medido. Lo que
  cambia es que quien lee el hallazgo lo resuelve de un vistazo en vez
  de comparar patrones a ojo.
- [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  acepta el nombre relacional de la granularidad —`celda`, `columna`,
  `tupla`, `tabla`— igual que
  [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
  Su propio mensaje de error ya los enumeraba, así que rechazarlos era
  una inconsistencia. El objeto sigue guardando el nombre canónico del
  marco.

### Spearman para relaciones monótonas que no son lineales

- [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md)
  acepta `metodo_numerico = "spearman"` y mide asociación monótona sobre
  los rangos, sin suponer linealidad. Sobre una relación cúbica con
  ruido, Pearson da 0,918 y Spearman 0,997. Pearson sigue siendo el
  valor por omisión, y el método elegido viaja en la columna `metodo`
  con su supuesto en `supuesto`, así que ninguna lectura depende de
  recordar cuál se pidió.
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  lo traslada con `metodo_asociacion_numerica`.
- Los dos README explican ahora dónde viven la distribución de valores y
  las correlaciones —en
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  no en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)—
  y por qué esa separación es deliberada.

### Tres afirmaciones que el paquete hacía sin fundamento suficiente

- `alta_cardinalidad` se apoyaba sólo en la tasa de valores distintos, y
  con pocas filas esa tasa está dominada por el tamaño: una columna de
  dos valores en tres filas daba 0,67 y superaba el umbral, aunque una
  columna de dos valores no puede tener cardinalidad alta. Ahora el
  hallazgo exige además al menos diez valores distintos. Las columnas
  con cardinalidad alta real —treinta valores distintos en cuarenta
  filas— se siguen informando igual.
- `columnas_duplicadas` afirmaba que dos columnas tienen el mismo
  contenido en tablas **sin ninguna fila**, donde dos columnas vacías
  coinciden sin que eso sea evidencia. Ese caso pasó a
  `cobertura_diagnosticos` con su motivo. Cuando sí hay filas, la
  evidencia declara ahora sobre cuántas se comparó.
- `relacion_aritmetica_columnas` se salteaba en silencio cuando la tabla
  no llegaba al mínimo de filas comparables. Ahora se declara en
  `cobertura_diagnosticos`, y sólo cuando había combinaciones de
  columnas numéricas que evaluar.

### El vínculo entre una acción del plan y su hallazgo ya no depende de la prosa

- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  recuperaba el par de columnas duplicadas comparando la cadena de
  evidencia completa del hallazgo. Al enriquecerse ese texto, dos pares
  distintos colapsaban en el mismo y el plan perdía una acción. El
  vínculo se hace ahora contra el primer tramo de la evidencia, que es
  el que identifica el par.

### El perfilado no toca los datos, y ahora está probado

- Ninguna función de análisis altera la tabla que recibe: ni sus
  valores, ni sus tipos, ni sus nombres, ni sus atributos. Una prueba de
  regresión lo verifica en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
  [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md),
  [`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md),
  [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md),
  [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md),
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
  [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  y
  [`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md).
  El caso que importa es `data.table`, que R permite modificar por
  referencia: la prueba compara además la dirección de memoria del
  objeto.
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  devuelve una copia y deja intacta la original.

### Duplicados: el hallazgo no afirma una igualdad que produjo la normalización

- La normalización por omisión iguala mayúsculas, espacios, acentos y
  comillas, así que un par puede coincidir después de normalizar sin que
  los valores guardados sean iguales. `tipo_par` distingue ahora
  `exacto`, `exacto_normalizado` y `aproximado`; `igualo_normalizar`
  deja esa causa visible en cada fila. El hallazgo
  `duplicados_exactos_normalizados` evita afirmar que dos filas tienen
  los mismos valores y la trazabilidad lo busca entre los pares de ese
  tipo. `n_pares_exactos` cuenta sólo texto guardado igual y
  `n_pares_exactos_normalizados` completa la explicación junto con
  `n_pares_aproximados`.

### Casi-claves y precedencia de ausencias

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  informa una `casi_clave` cuando una columna tiene al menos 100 filas,
  supera 90 % de valores distintos y al menos la mitad de sus duplicados
  excedentes se concentra en un valor. Las fechas y fecha-hora se
  excluyen por su rol propuesto. La evidencia enumera las colisiones,
  sus frecuencias y los criterios aplicados. Los vectores `double` con
  algún valor finito fraccionario se excluyen, mientras que los formados
  por valores enteros se conservan para admitir identificadores
  importados desde archivos de texto. Los vectores `integer64` cuentan
  como enteros semánticos.
  [`detectar_claves()`](https://sebollin.github.io/lupa/reference/detectar_claves.md)
  las expone sin confundirlas con claves exactas, y
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  las reitera en sus advertencias.
- `casi_duplicados_vocabulario` retira primero los valores ya detectados
  como `faltantes_disfrazados`. Un centinela de ausencia deja de
  presentarse como posible errata de otro valor; las variantes que no
  son centinelas conservan el diagnóstico.

### Tablero, indice declarado y medicion agregada

- [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
  resume una corrida por metrica y objeto, declara la agregacion
  aplicada en cada fila y conserva el alcance completo del marco.
- [`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
  no calcula nada sin pesos del usuario. Con una declaracion completa
  conserva cobertura, pesos por dimension, combinaciones internas,
  inversiones de defectos, exclusiones `no_aplica` y la advertencia de
  que los componentes provienen de universos distintos.
- [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  mide por omision la propuesta en estado `lista`, declara que no fue
  confirmada, agrega las medidas y conserva el tablero. El detalle fila
  a fila solo se retiene con `conservar_detalle_medicion = TRUE`; la
  medicion automatica se desactiva con `medir_propuesta = FALSE`.

### Secuencias enteras densas y vocabularios breves

- El perfil de columna publica si los enteros observados cubren
  densamente su rango, junto con densidad, posiciones y huecos. En esa
  condicion los centinelas numericos y los desvios que solo expresan el
  largo de una corrida de digitos no interpretan el contenido del
  identificador; los ausentes, duplicados y restantes diagnosticos
  siguen activos. Una secuencia densa y unica se presenta como
  `posible_identificador` y no recomienda convertir el texto numerico a
  una medida cuantitativa.
- `casi_duplicados_vocabulario` cubre una sustitucion en valores de
  hasta seis caracteres cuando la variante ocupa como maximo `0.05` de
  la columna y la forma dominante es al menos `10` veces mas frecuente y
  ocupa al menos `0.5` de la columna. El limite y los tres umbrales
  quedan en la evidencia y se pueden ajustar en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

### Orientacion explicita de las metricas

- [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  declara si un resultado expresa `"conformidad"`, `"defecto"` o
  `"no_aplica"`. La orientacion viaja por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  y [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
  sin invertir los valores; una regla puede recibirla como segundo
  argumento. El historico conserva el esquema 1 y sigue leyendo archivos
  anteriores.
- `Formato` queda alineada con el factor `Correctitud sintactica` de
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  y una prueba contrasta todos los pares dimension-factor del nucleo
  contra el marco.

### Marco CEA/CEPAL de aseguramiento de la calidad

- [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  incorpora los cuatro niveles y diecinueve principios del marco
  nacional de aseguramiento de la calidad de las Naciones Unidas,
  adoptado y adaptado para América Latina y el Caribe por la CEA/CEPAL.
  Los principios 1 a 13 quedan declarados fuera del alcance de una
  tabla; los principios 14 a 19 quedan disponibles para documentar
  productos estadísticos, sin afirmar que el profiling genérico los
  mida.

### Severidad del vocabulario y escala de las relaciones

- `casi_duplicados_vocabulario` queda como señal `sospechoso` sólo
  cuando encuentra grupos; un resultado negativo queda como `ok` con
  cero afectados, y un diagnóstico que no aplica se registra en
  `cobertura_diagnosticos`.
- `relacion_orden_columnas` separa la escala de la relación fila a fila
  con un solapamiento intercuartil mínimo de `0.1`. Una brecha con IQR
  cero conserva una relación estable aunque los rangos no se solapen;
  ambos criterios y los pares descartados o recuperados quedan en el
  alcance.

### Perfil de una muestra DBI con universo explícito

- Se agrega
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  para separar los agregados SQL exactos sobre una tabla completa del
  perfil de 99 campos calculado sobre una muestra declarada. La salida
  registra el motor informado por DBI, cada consulta, los agregados no
  disponibles y la reproducibilidad efectiva del orden, sin escribir en
  la base.

### Desenlaces declarados por reglas

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `desenlace = "suprimir"` para que una regla declarada por el
  usuario produzca un plan sobre las medidas que no cumplen su
  condición. La evaluación conserva objeto, valor medido, motivo y regla
  sin modificar la medición ni los datos de origen. Sin esa declaración
  no crea desenlaces ni aplica umbrales de publicación.
- [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  enmascara los valores alcanzados por ese plan tanto en la evaluación
  como en las mediciones incluidas en el mismo reporte. El enmascarado
  se hace sobre copias usadas para renderizar.

### Ley de Benford con aplicabilidad explícita

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  evalúa la ley de Benford solamente en columnas numéricas con
  suficiente evidencia inicial. Antes de comparar exige variación,
  ausencia de apariencia de identificador (incluidas secuencias
  correlativas), al menos 100 valores positivos, todos los valores
  finitos positivos y tres órdenes de magnitud. Las precondiciones y sus
  umbrales quedan en `meta$benford`; las que fallan se declaran en
  `cobertura_diagnosticos` y no producen hallazgos.
- Cuando aplica, el perfil conserva la distribución observada y esperada
  por primer dígito, el chi-cuadrado de Pearson y su valor p. Una
  desviación se presenta como señal descriptiva para revisar, nunca como
  acusación de fraude o manipulación.

### URLs, unidades y celdas multivaluadas

- [`validar_url()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)
  valida de forma vectorizada URLs `http` y `https`, con esquema
  obligatorio por omisión, soporte para IDN y puertos, y rechazo
  deliberado de `javascript:`, `data:`, espacios y controles literales.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  informa `unidades_mixtas` cuando una columna numérica escrita como
  texto combina sufijos de unidad, conservando sus frecuencias y sin
  convertir datos. Reconoce además monedas como prefijos o sufijos y
  emite `monedas_mixtas` con sus frecuencias, sin convertir ni suponer
  tasas de cambio. También informa `celdas_multivaluadas` sólo cuando
  las partes homogéneas pasan el control de patrones y tipo, incluidos
  identificadores numéricos con puntuación interna; nombres y
  direcciones con comas no se presentan como listas.

### Relaciones aritméticas entre columnas

- Reconoce una regularidad mediante un único soporte declarado
  (`umbral_aritmetica = 0.9`) dentro de la tolerancia y, una vez
  reconocida, informa todas sus discrepancias sin aplicar un segundo
  filtro por su cantidad absoluta: `max_violaciones_aritmetica` se
  elimina. El soporte, el universo mínimo y la tolerancia quedan en la
  evidencia y el alcance.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  descubre identidades aditivas y proporcionalidades estables entre
  columnas numéricas y las presenta como evidencia observada, no como
  reglas del dominio. Cada hallazgo declara proporción de cumplimiento,
  universo de filas finitas, tolerancia numérica, constante proporcional
  y filas discrepantes.
- `umbral_aritmetica`, `min_filas_aritmetica`, `tolerancia_aritmetica` y
  `max_columnas_aritmetica` hacen visibles los supuestos y el costo del
  diagnóstico. Si el límite de columnas recorta combinaciones,
  `cobertura_diagnosticos` lo declara explícitamente.

### Capa de marcos

- [`regla_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md)
  acepta `proporcion_minima` para declarar un veredicto sobre la
  proporción de medidas que cumple la condición. El objeto conserva el
  umbral; la evaluación muestra proporción, veredicto, componentes y
  universo, sin ponderar medidas ni crear un puntaje global. Las reglas
  por medida conservan su contrato y su estructura de salida.
- [`metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  acepta las etiquetas relacionales de
  [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  —por ejemplo, `"columna"`— y guarda siempre su equivalente canónico de
  la ontología (`"atributo"`). Un valor inválido muestra ambos
  vocabularios.
- El error de una regla que no engancha ninguna medida enumera lo
  solicitado y las métricas instanciadas disponibles, incluidos sus
  nombres calificados.

### Perfilado de geometrías

- Las columnas `sfc` informan CRS, tipo de geometría, geometrías vacías
  e inválidas, coordenadas fuera del dominio declarado y caja
  envolvente. Una geometría sin CRS deja el conteo de dominio en `NA`:
  no se supone EPSG:4326. Las geometrías vacías se cuentan aparte y no
  integran el universo del chequeo de dominio.
- Los tipos mixtos se comparan por familia: las variantes simples y
  `MULTI` compatibles conviven sin hallazgo, mientras que familias
  distintas y `GEOMETRYCOLLECTION` se señalan. La validez declara
  `validez_criterio = "planar"`; sobre CRS geográficos un fallo planar
  es sospechoso y no afirma invalidez esférica.
- `n_dominio_evaluados` y `n_bbox_evaluados` hacen públicos los
  universos no vacíos de sus métricas; `bbox_alcance` declara que la
  caja usa las coordenadas crudas, incluidas las que estén fuera de
  dominio. `n_validez_evaluados` publica por separado el universo de
  GEOS, incluidas las geometrías vacías. `dimension_geometria` declara
  `XY`, `XYZ`, `XYM` o `XYZM`; Z y M quedan enumeradas en
  `dimensiones_no_evaluadas` y generan una fila de cobertura. Para `XYM`
  y `XYZM`, la validez topológica se calcula en XY después de `st_zm()`
  y `validez_preprocesamiento` declara ese paso.
- El control de dominio compara también las coordenadas transformadas
  con la `BBOX` del área de uso del WKT. Detecta, entre otros casos,
  grados donde el CRS espera metros; no detecta una zona UTM equivocada
  cuando las coordenadas interpretadas caen dentro del área de esa zona.
  Una caja mundial es un no-op evaluado y un WKT sin `BBOX` produce una
  fila de cobertura, sin asumir alcance global.
- Los nuevos hallazgos distinguen CRS ausente, geometrías inválidas o
  vacías, coordenadas imposibles y tipos geométricos mixtos. Si falta el
  paquete opcional `sf`, el perfil no inventa ceros ni hallazgos:
  registra una fila con `dependencia = "sf"` en
  `cobertura_diagnosticos`.

### Fechas con meses escritos

- [`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md)
  reconoce fechas con meses escritos en español (incluye `setiembre` y
  `set`) y en inglés, además de los formatos numéricos existentes. La
  tabla de nombres es propia y no depende de `LC_TIME`, y sólo acepta la
  estructura completa de una fecha o de un mes con año: encontrar
  `marzo` dentro de una oración no convierte el texto en fecha. Los
  meses escritos desambiguan el día y el mes; los años de dos dígitos
  siguen siendo candidatos y no se les asigna un siglo en silencio.
- Los períodos expresados sólo como mes y año declaran
  `granularidad = "mes"` y no inventan el día 1 para calcular mínimos,
  medias o conversiones; esos resúmenes quedan en `NA` con estado
  `granularidad_incompleta`. Los años escritos en meses también se
  limitan al rango 1800–2100, como las fechas compactas.
- La detección de meses sólo ejecuta sus expresiones regulares sobre los
  valores candidatos y reutiliza ese resultado al calcular el resumen de
  la columna. Así el texto libre que menciona meses no paga el costo
  completo ni se vuelve a analizar.
- Ese resultado intermedio se mantiene sólo durante el perfilado y no
  queda adjunto al objeto público `formatos_fecha`. En columnas mixtas,
  los resúmenes de fecha se calculan sobre las fechas completas y
  declaran cuántas fechas de mes-año quedaron fuera; una columna
  compuesta sólo por períodos conserva el estado
  `granularidad_incompleta`.
- [`inferir_tipo()`](https://sebollin.github.io/lupa/reference/inferir_tipo.md)
  tampoco conserva el caché interno de detección de meses. El
  diagnóstico de variantes del vocabulario sigue siendo una señal
  heurística: Jaro–Winkler puede acercar nombres de calles o códigos con
  prefijos compartidos y sus grupos deben revisarse como sospechosos, no
  como identidades.
- El hallazgo de variantes del vocabulario sólo atribuye el límite de
  proporción cuando existe un grupo compatible que retener; si todas las
  cercanías fueron descartadas por secuencias numéricas incompatibles,
  lo informa con ese motivo.

### Variantes del vocabulario

- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  agrega el hallazgo `casi_duplicados_vocabulario`: agrupa, por columna,
  variantes que la normalización funde o que quedan bajo el umbral de
  Jaro–Winkler y conserva la frecuencia de cada forma. La unidad es el
  valor distinto, no la fila; no se elige una forma canónica ni se
  modifica el dato. Las aristas de distancia forman estrellas alrededor
  de un valor de frecuencia estrictamente mayor y único; los empates no
  se fuerzan y no se cierra transitivamente una cadena de vecinos. Cada
  grupo declara su distancia mínima y máxima, y
  `max_proporcion_grupo_vocabulario` permite declarar que el diagnóstico
  no aplica cuando un componente abarca demasiado vocabulario; el filtro
  se activa desde 20 valores distintos o cuando el grupo mayor tiene al
  menos 10 variantes, y sólo suprime si la proporción también supera el
  umbral. Así no oculta grupos pequeños, pero tampoco entrega una
  columna entera como una sola familia. Cuando hay pares cercanos pero
  no una frecuencia central única, el alcance declara la falta de
  asimetría y apunta a
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).
  Las aristas de distancia con secuencias numéricas distintas se
  descartan; los ceros de relleno y separadores de miles se consideran
  equivalentes, pero una errata dentro de un número puede quedar sin
  agrupar deliberadamente. El alcance informa los pares descartados por
  números y separa el tamaño potencial del componente del tamaño que
  queda compatible con esa regla. `casi_duplicados_vocabulario = FALSE`
  lo desactiva. El alcance declara los valores y pares comparados, los
  recortes y la ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist); las
  fusiones exactas se siguen informando sin ese paquete. Los resultados
  del perfil pueden cambiar porque ahora se señalan estas variantes como
  evidencia para una revisión de vocabulario.

### Referenciales

- Las métricas de referenciales heredan el perfil de `normalizar`
  declarado en
  [`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md)
  (o aceptan uno explícito), por lo que variantes de caja, acentos y
  espacios pueden pasar a reconocerse como presentes. Esto cambia los
  resultados de correctitud y cobertura de forma deliberada; las claves
  siguen evaluándose por identidad exacta.
- `CorrectitudSemFuerte` y `CorrectitudSemDebil` pueden agregar, sin
  cambiar el veredicto, el candidato más cercano y su distancia como
  evidencia. La proximidad usa Jaro–Winkler por omisión (`p = 0.1`,
  umbral `0.10`), sólo se calcula para fallos y declara sus límites o la
  ausencia de
  [`stringdist`](https://cran.r-project.org/package=stringdist). Se
  calcula sobre los valores fallidos distintos y se reparte a las filas
  repetidas; el alcance distingue filas fallidas, valores distintos y
  valores comparados.

### Perfil de normalización para comparar

- `normalizar` deja de ser sólo un interruptor lógico: `TRUE` conserva
  el caso común con minúsculas, espacios, acentos protegidos y comillas;
  `FALSE` desactiva esos pasos configurables; `"amplio"`,
  \[normalizacion()\] y una lista nombrada permiten elegirlos por
  columna. La representación normalizada sólo decide qué valores se
  comparan: nunca modifica los datos guardados.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  conserva el perfil resuelto y los análisis de duplicados y claves lo
  heredan cuando reciben `normalizar = NULL`. Los resultados pueden
  cambiar porque el umbral se aplica sobre la cadena normalizada; el
  perfil informa, por vocabulario, cuántos valores fundió cada paso.
- La comparación aplica siempre descomposición y orden canónicos en el
  subconjunto latino cubierto; no reordena palabras ni aplica
  abreviaturas de vías. Las claves siguen descubriéndose por identidad
  exacta y agregan la unicidad normalizada como métrica informativa.
- El informe de fusiones compara el perfil completo con una versión que
  apaga cada paso por separado: sus cifras no son aditivas y el total
  normalizado se informa aparte. Ahora usa el vocabulario completo (las
  fusiones son una propiedad de pares que una muestra de valores puede
  ocultar) y la normalización se aplica de forma vectorizada; `n_usados`
  y el estado `exacto` dejan explícito el alcance real.
- El informe de fusiones no se calcula cuando `normalizar = FALSE`,
  porque no hay pasos configurables que evaluar. Cuando
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  ya lo calculó, `detectar_duplicados_aproximados(perfil = ...)` lo
  reutiliza en lugar de recorrer de nuevo el vocabulario.
- `proteger` acepta grafemas compuestos y el valor predeterminado
  conserva `g̃` además de `ñ` y `ü`, para no borrar letras guaraníes al
  comparar.

### Diagnósticos de texto invisible

- Amplía la detección a los espacios Unicode, marcas direccionales, BOM
  y otros invisibles de transporte. Los espacios Unicode se pueden
  colapsar a espacio ASCII sólo mediante una acción explícita y
  destructiva; ZWJ/ZWNJ se informan pero se conservan. La comparación
  normalizada usa estas mismas clases sin borrar caracteres
  semánticamente significativos.
- El hallazgo de separadores en campo, su acción y su conteo usan
  nombres específicos para cubrir tabulaciones, saltos, avances de
  página y tabulaciones verticales.
- [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  identifica controles C0/C1 e invisibles Unicode, entidades HTML
  reconocibles y separadores dentro de campos. La evidencia escapa esos
  caracteres (`<U+200B>`, `\\t`, `\\n`, `\\r`, `\\f`, `\\v`) y conserva
  los conteos por fila.
- Los controles invisibles que no son separadores se pueden eliminar y
  se recomiendan por defecto; decodificar entidades HTML y reemplazar
  separadores de línea quedan como acciones explícitas porque pueden
  cambiar contenido legítimo. Las tres dejan el número de valores
  cambiados en el registro.

### Reparación de texto y licencia

- La medida predeterminada de duplicados ahora aplica Jaro–Winkler con
  `p = 0.1` (el valor anterior era Jaro puro por `p = 0`) y el umbral
  pasa de `0.12` a `0.10`. Las dos decisiones pueden cambiar los pares
  informados al actualizar; el cambio es deliberado y queda declarado en
  la ayuda.

- Declara `cli (>= 3.0.0)`. El motor usa la interfaz de barras de
  progreso (`cli_progress_bar()` y sus compañeras), que existe recién
  desde esa versión; antes el requisito estaba supuesto y no escrito.

- Clasifica los duplicados exactos comparando los textos que realmente
  entran a la medida, después de normalizarlos, y no mediante igualdad
  exacta de un flotante. Esto hace el resultado independiente de la
  arquitectura y mantiene como `aproximado` un par de textos distintos
  aunque `soundex` devuelva distancia cero.

- Cierra el motor de reparación de texto: `decode_inconsistent_utf8`
  trabaja por subcadenas con el detector de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy), conserva los estados
  parciales con U+FFFD y agrega tres extensiones deliberadas de badness
  sobre ftfy 6.3.1: la regla de inicio del issue
  [\#222](https://github.com/rspeer/python-ftfy/issues/222), también
  discutida en el [PR](https://github.com/rspeer/python-ftfy/pull/232)
  [\#232](https://github.com/sebollin/lupa/issues/232); la regla de caja
  que detecta mojibake de KOI8-R del issue
  [\#231](https://github.com/rspeer/python-ftfy/issues/231); y la regla
  específica para `â` del issue
  [\#233](https://github.com/rspeer/python-ftfy/issues/233). La tabla de
  bytes KOI8-R es la cuarta extensión y la puerta literal `Ã` para
  formas portuguesas y francesas es la quinta.

- Incorpora un motor R puro para detectar y reparar mojibake en varias
  codificaciones, inspirado en el diseño y las tablas de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy) de [Robyn
  Speer](https://github.com/rspeer). Los resultados distinguen
  reparaciones completas, parciales y casos irrecuperables; los estados
  llegan al hallazgo, al plan y al registro.

- Completa el port de las reglas de detección y de los
  transcodificadores de [ftfy](https://github.com/rspeer/python-ftfy):
  las transformaciones de bytes se encadenan antes de decodificar, las
  pérdidas quedan como U+FFFD y estado `reparado_parcialmente`, y nunca
  se introduce un control invisible nuevo.

- Completa `restore_byte_a0` de [ftfy
  6.3.1](https://github.com/rspeer/python-ftfy): conserva la frontera de
  la palabra `à`, respeta las excepciones portuguesas y cubre las seis
  formas de bytes alterados, sin partir ni pegar palabras.

- Conserva los espacios no separables y agrega el decodificador R puro
  de variantes UTF-8 de [ftfy](https://github.com/rspeer/python-ftfy):
  combina pares CESU-8 y reconoce `C0 80`, e incorpora la tabla de bytes
  KOI8-R adicional, con los estados y pérdidas ya declarados.

- Declara como quinta extensión deliberada la puerta adicional para la
  secuencia literal `Ã`, que conserva las formas portuguesas y francesas
  observadas en padrones; el decodificador de variantes rechaza
  secuencias que producirían un NUL, en vez de omitir un carácter al
  materializar el texto.

- La licencia del paquete pasa de `GPL-2 | GPL-3` a `GPL-3`; las partes
  derivadas del diseño de [ftfy](https://github.com/rspeer/python-ftfy)
  se atribuyen en `LICENSE.note` bajo Apache-2.0.

- La estrategia de reparación de texto se registra como
  `reparar_codificacion`.

### Recursos de comparación

- Fija por omisión en dos los hilos que
  [`stringdist`](https://cran.r-project.org/package=stringdist) puede
  usar en las comparaciones aproximadas y declara el valor efectivo en
  `alcance`.
- El aviso interactivo del camino LSH identifica `nucleos` como la
  perilla que puede acortar la etapa de comparación, sin prometer una
  ganancia fija.
- La viñeta de escala documenta el rendimiento observado entre dos y
  treinta y un hilos y deja explícito que después de dieciséis no hubo
  una mejora medida.
- Documenta que el piso de tiempo de LSH cubre sólo la comparación de
  cadenas, no la firma, las cubetas ni el troceo; los resultados no
  dependen de la cantidad de hilos.
- Actualiza las mediciones de escala para anotar la configuración de
  hilos y evita presentar tiempos dependientes de la máquina como cifras
  exactas.

### Marcos declarables y alcance internacional

- Incorpora validadores vectorizados de ISO 3166, ISO 4217, correo, Luhn
  y módulo 97, junto con un pack uruguayo de cédula y RUT. Los packs
  territoriales se pueden extender sin registrar estado global ni
  modificar el núcleo.
- Separa clasificar de proteger datos personales: las formas numéricas
  poco discriminantes se informan sin suprimir estadísticos, mientras
  nombres semánticos, correos y documentos verificados conservan la
  protección.
- Documenta los contratos de todos los puntos de extensión y añade
  [`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  para consultar la configuración admitida sin inspeccionar closures.
- Incorpora
  [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  como adaptación opcional y explícita de las quince características de
  ISO/IEC 25012:2008.
- Identifica el marco activo en cada fila de
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).
- Permite declarar taxonomías dimensión-factor con
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  validar modelos contra ellas y calcular cobertura con AGESIC sólo como
  valor de fábrica mediante
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
- Permite construir familias de madurez con nombres y umbrales propios
  sin cambiar los tres perfiles incluidos.
- Hace que el vector de sentinelas numéricos sea una política completa:
  [`numeric()`](https://rdrr.io/r/base/numeric.html) los desactiva
  explícitamente.
- Reconoce coma y punto decimal, separadores de miles simétricos,
  símbolos monetarios y prefijos con forma de código ISO 4217.
- Clasifica RUT, DNI y otros documentos con la etiqueta neutral
  `documento_identidad`.
- Permite conectar packs personales territoriales al mismo clasificador
  de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  con tolerancia explícita de errores de digitación; los nombres
  semánticos (`telefono`, `fecha_nacimiento`, entre otros) conservan
  prioridad sobre formas numéricas genéricas.

### Examinar datos

- Detecta relaciones de orden sospechosas entre columnas numéricas o
  temporales comparables (por ejemplo, `inicio <= fin` y
  `monto_bruto <= monto_neto`). El hallazgo conserva los conteos y las
  filas fuera de orden, sugiere formalizar la regla con
  `ReglaIntegridadIntraEntidad` y declara en `meta$orden_columnas` las
  columnas y pares efectivamente comparados. Expone un filtro opcional
  de solapamiento intercuartil para tablas anchas; está apagado por
  omisión (umbral `0`) porque activarlo puede ocultar relaciones reales
  entre magnitudes de rangos distintos. Los pares descartados quedan
  contados en el alcance.

- Protege los estadísticos de orden y cuantiles de columnas personales,
  marca cada supresión en el objeto y conserva alertas de plausibilidad
  para fechas de nacimiento sin publicar sus extremos.

- Añade `datos_operativos`, un segundo conjunto sintético y neutral,
  reproducible desde `data-raw/`, con problemas de calidad sembrados.

- Añade
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  como puerta de entrada al recorrido descriptivo, con cobertura
  conceptual y advertencias de alcance en el propio objeto.

- Incorpora distribuciones de valores acotadas, cuantiles, asociaciones
  de Pearson, V de Cramér y eta cuadrado, además de regularidad,
  duplicación, monotonicidad, cobertura, días de semana y huecos
  temporales.

- Propone escalas de medición y roles sin confirmar lo que sólo se
  infiere de los valores; conserva niveles declarados, observados y
  ausentes.

- Perfila tablas administrativas con métricas generales y por columna,
  proporciones en `[0, 1]` y hallazgos filtrables.

- Descubre patrones de formato, tipos implícitos, formatos de fecha
  mixtos y ambiguos, años de dos dígitos, números regionales y problemas
  de codificación.

- Detecta claves candidatas, relaciones, cobertura referencial, columnas
  y filas duplicadas, y dependencias funcionales exactas o aproximadas.

- Conserva la ambigüedad día/mes con barra, guion y punto; reconoce
  fracciones de segundo y offsets ISO 8601.

- Distingue NaN e infinitos, evita aproximar `integer64` fuera del rango
  exacto de `double` y cuenta valores distintos en columnas de listas y
  geometrías.

- Clasifica posibles datos personales sin juzgar su presencia y protege
  por defecto los valores concretos cuando la evidencia es
  discriminante.

- Normaliza factores a texto sólo en la operación, conserva `factor` en
  el perfil y devuelve texto al transformar columnas factor con
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md).

- Mantiene claves históricas estables en R 3.6 y fija explícitamente en
  UTC las fechas convertidas desde `Date`.

- Las claves históricas tratan el texto ilegible (UTF-8 inválido) como
  ausente: comparte con `NA` la marca `~`, en vez de intentar
  codificarlo como texto literal.

- Añade conteos explícitos de evaluados y afectados, con la unidad de
  conteo, a cada hallazgo; conserva NA cuando el alcance no permite
  conocerlos.

- Añade trazabilidad acotada por hallazgo mediante índices de fila, con
  estados explícitos para lo disponible, truncado, no aplicable y no
  disponible; el reporte resume el estado sin imprimir los índices.

### Medir y evaluar calidad

- Declara métricas genéricas, específicas e instanciadas con tipo de
  resultado y granularidad explícitos.
- Incluye veintiuna métricas automatizables, tres métricas tabulares
  basadas en referenciales y una correspondencia verificable con las 49
  entradas del catálogo de AGESIC.
- Separa en el catálogo la disponibilidad de cada métrica de la causa o
  el matiz de esa disponibilidad, y documenta las 49 correspondencias
  sin vacíos.
- Ajusta las métricas oficiales de oportunidad al resultado booleano del
  marco y conserva la fórmula continua del curso CPAP bajo nombres
  `GradoOportunidad*`.
- Incorpora contratos explícitos
  [`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
  y
  [`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md),
  y una tabla de cobertura que distingue lo medido, no declarado, no
  aplicable y fuera de alcance.
- Implementa las cuatro agregaciones del marco y la cadena de evaluación
  de medidas, reglas y perfiles de madurez. No calcula un índice global.
- Propone modelos editables a partir del perfil sin convertir
  observaciones de una sola entrega en requisitos silenciosos.
- Estima el costo antes de comparar, aplica un presupuesto de pares en
  los caminos exhaustivo y LSH, y publica el alcance de la estimación.
- Incorpora MinHash y LSH deterministas para generar candidatos a
  escala, con deduplicación por banda, garantía declarada y degradación
  explícita.
- Permite bloquear por una columna elegida por el usuario y estima los
  pares que el bloqueo puede dejar fuera, incluidos los ausentes como
  bloque propio.

### Mejorar y monitorear

- Construye planes de limpieza editables con alternativas mutuamente
  excluyentes, justificación, modo guiado opcional y consentimiento
  adicional para eliminaciones.
- Aplica sólo acciones activas sobre una copia, conserva un registro y
  permite imputaciones confirmadas mediante dependencias funcionales
  exactas.
- Acumula evaluaciones en un histórico plano y versionado; detecta
  deriva del modelo y cambios estructurales entre perfiles.
- Procesa comparaciones exhaustivas por lotes con parciales en un
  directorio declarado, cruza los lotes sin pérdida de pares y deja
  constancia de que no son reanudables.

### Informar

- Guarda y recupera análisis versionados sin datos de entrada por
  omisión y sin serializar entornos completos de reglas funcionales.
- Genera un único HTML autocontenido, en español, sin navegador, LaTeX
  ni recursos externos; los valores se escapan y la evidencia personal
  se enmascara por defecto.

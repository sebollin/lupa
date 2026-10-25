# 2. Leer un perfil

Ya tenemos un perfil. ¿Qué quiere decir cada fila de
`hallazgos(perfil)`, cuándo un `ok` es una buena noticia, y qué hacemos
con una clave que no se cumple?

> **En esta guía:**
>
> - qué significan las tres severidades y por qué sólo `error` puede
>   frenar un proceso;
> - qué trae cada hallazgo: evidencia, sugerencia, conteos y
>   trazabilidad;
> - cómo leer cualquier perfil con los mismos accesores;
> - cómo se declara y se comprueba una clave;
> - cómo encontrar filas donde dos columnas que dicen lo mismo se
>   contradicen.
>
> Al final hay un glosario con las palabras del paquete.

## Un perfil para leer

Seguimos con `datos_operativos` y la clave de la guía 1.

``` r

library(lupa)
perfil <- perfilar(datos_operativos, clave = "id_evento")
#> Warning: La clave declarada no es única: entre las 13 filas con la clave completa, 1 repite su
#> valor (2 filas participan en colisiones). La clave cumple la ausencia de nulos: no tiene
#> valores ausentes.
h <- hallazgos(perfil)
```

El aviso es el mismo de la guía 1: `EVT001` se repite. Lo retomamos al
hablar de claves.

## Tres severidades

`severidad` es un factor ordenado, `ok < sospechoso < error`, así que se
puede contar y comparar: `h$severidad >= "sospechoso"` deja los
hallazgos que piden atención.

``` r

levels(h$severidad)
#> [1] "ok"         "sospechoso" "error"
table(h$severidad)
#> 
#>         ok sospechoso      error 
#>          6         11          6
nrow(h[h$severidad >= "sospechoso", ])
#> [1] 17
```

Cada nivel dice una cosa distinta:

- **`ok`** registra una lectura informativa o una condición aceptable.
  No es un defecto, y tampoco es una aprobación.
- **`sospechoso`** es evidencia que merece revisión. Es heurística o
  necesita conocer el dominio, y no alcanza por sí sola para rechazar,
  reparar ni suprimir datos.
- **`error`** afirma que un control cruzó su criterio declarado.

Los `ok` de este perfil son lecturas de la estructura, no buenas
noticias:

``` r

h[h$severidad == "ok", c("columna", "tipo_hallazgo", "n_afectados")]
#>           columna               tipo_hallazgo n_afectados
#> 1     id_registro       posible_identificador           1
#> 15       id_copia       posible_identificador           1
#> 16      id_evento       posible_identificador           1
#> 17 codigo_usuario casi_duplicados_vocabulario           0
#> 18   fecha_evento casi_duplicados_vocabulario           0
#> 23       contacto       dato_personal_posible           1
```

`posible_identificador` dice que una columna tiene forma de
identificador o de código; `dato_personal_posible`, que parece contener
datos personales; `casi_duplicados_vocabulario` con cero afectados, que
se buscaron variantes de escritura y no se formó ningún grupo.

El mismo tipo de hallazgo puede tener dos severidades. Los ausentes
disfrazados de texto son `error`; el de `monto`, `sospechoso`:

``` r

h[h$tipo_hallazgo == "faltantes_disfrazados", c("columna", "severidad", "evidencia")]
#>           columna  severidad             evidencia
#> 3  codigo_usuario      error               S/D (1)
#> 5    fecha_evento      error              NULL (1)
#> 9           canal      error <blanco> (1); S/D (1)
#> 12          monto sospechoso               -99 (1)
```

`"S/D"`, `"NULL"` y la celda vacía son marcas de ausencia sin
ambigüedad. El `-99` está en la lista de centinelas numéricos que `lupa`
usa por omisión, pero en otra columna podría ser un dato real: es una
conjetura del paquete. Si nosotros lo declaramos en
`sentinelas_numericos`, la conjetura pasa a criterio y la severidad
sube.

``` r

declarado <- perfilar(datos_operativos["monto"], sentinelas_numericos = -99)
hd <- hallazgos(declarado)
hd[hd$tipo_hallazgo == "faltantes_disfrazados", c("columna", "severidad", "evidencia")]
#>   columna severidad evidencia
#> 1   monto     error   -99 (1)
```

Declararlo tiene además consecuencias en los números: el `-99` sale del
resumen.

``` r

resumen <- c("minimo", "mediana", "n_valores_excluidos_resumen")
rbind(
  por_omision = columnas(perfil)[columnas(perfil)$columna == "monto", resumen],
  declarado = columnas(declarado)[, resumen]
)
#>             minimo mediana n_valores_excluidos_resumen
#> por_omision    -99    1490                           0
#> declarado      -50    1545                           1
```

### Por qué sólo `error` frena

De los tres niveles, `error` es el único candidato para alimentar una
barrera automática —un proceso que rechaza una entrega—, y sólo después
de que aceptemos su criterio y verifiquemos su alcance. Un `sospechoso`
pide una persona que mire.

Pero buscar errores no alcanza. Lo que no se pudo evaluar queda fuera de
la escala, en
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
y un guion que decide solo tiene que mirar las dos cosas: cero errores
con diagnósticos sin ejecutar no es un perfil limpio.

``` r

c(errores = sum(h$severidad == "error"), no_evaluados = nrow(cobertura(perfil)))
#>      errores no_evaluados 
#>            6            9
```

## Qué trae cada hallazgo

Cada fila de
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
tiene los mismos campos.

| campo | qué dice |
|----|----|
| `columna` | la columna; `NA` si el hallazgo es de la tabla entera |
| `tipo_hallazgo` | la clase de señal, de un vocabulario fijo |
| `severidad` | `ok`, `sospechoso` o `error` |
| `descripcion` | la señal, en una oración |
| `evidencia` | lo que se midió para afirmarla |
| `sugerencia` | qué se podría hacer; la decisión es nuestra |
| `n_evaluados`, `n_afectados` | cuántas unidades se miraron y en cuántas está la señal |
| `unidad_conteo` | en qué se cuentan las dos anteriores: filas, columnas, formatos, valores distintos |
| `estado_reparacion` | en los hallazgos de codificación, si el texto se puede reparar; en los demás, `NA` |
| `trazabilidad` | en qué filas está: sus índices y, si declaramos `clave`, sus valores |

El vocabulario completo de `tipo_hallazgo`, con lo que mide cada uno,
está en el artículo [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

Veamos uno entero: las mayúsculas de `canal`. Los textos largos se leen
mejor fuera de la tabla.

``` r

mayus <- h[h$tipo_hallazgo == "mayusculas_inconsistentes", ]
for (campo in c("descripcion", "evidencia", "sugerencia")) {
  writeLines(strwrap(paste0(campo, ": ", mayus[[campo]]), 88, exdent = 2))
}
#> descripcion: Conviven valores que sólo se diferencian por mayúsculas y minúsculas.
#> evidencia: "web"; "Web"
#> sugerencia: Definir y aplicar una convención de capitalización para la columna.
mayus[, c("n_evaluados", "n_afectados", "unidad_conteo")]
#>    n_evaluados n_afectados  unidad_conteo
#> 11           8           2 valor_distinto
mayus$trazabilidad[[1]]$claves$id_evento
#> [1] "EVT003" "EVT001" "EVT006" "EVT010" "EVT012" "EVT001"
```

La unidad importa. `n_afectados` cuenta valores distintos: `"web"` y
`"Web"`, dos de los 8 que tiene la columna. La trazabilidad, en cambio,
enumera filas, y todas las que contienen esos valores: `lupa` no sabe
cuál de las dos formas es la correcta. Por eso la traza tiene 6 filas,
primero la variante y después la forma más frecuente.

### Alta cardinalidad

`alta_cardinalidad` aparece tres veces en este perfil:

``` r

h[h$tipo_hallazgo == "alta_cardinalidad", c("columna", "severidad", "evidencia")]
#>           columna  severidad                        evidencia
#> 2  codigo_usuario sospechoso Tasa de valores distintos: 0.846
#> 4    fecha_evento sospechoso Tasa de valores distintos: 0.923
#> 14       contacto sospechoso            [evidencia protegida]
```

Una columna de texto con muchos valores distintos (por omisión, más de
la mitad: `umbral_alta_cardinalidad`) admite tres lecturas: es texto
libre, es un identificador o es una categoría mal normalizada. El
hallazgo no afirma que sea un defecto; las ofrece para que decida quien
conoce la columna. En este perfil, ninguna de las tres columnas tiene un
problema de cardinalidad: `codigo_usuario` es un identificador,
`fecha_evento` son fechas guardadas como texto y `contacto` son correos.
Lo sabemos porque conocemos las columnas; los datos solos no lo dicen.
Por eso se informa siempre: el largo de los valores no distingue un
catálogo de un texto libre.

La evidencia de `contacto` sale como `[evidencia protegida]`: la columna
parece contener correos (`dato_personal_posible`), y la evidencia de una
columna así se enmascara. Qué se enmascara y con qué alcance está en el
artículo [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

## Leer cualquier perfil

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
devuelve un perfil plano.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
devuelve un contenedor con dos bloques, y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
una fila por tabla. Las formas son distintas: `perfil$general$filas`
existe en el primero y es `NULL` en el segundo, donde el conteo vive en
otro lugar. Un `NULL` que no avisa es la peor forma de fallar en un
guion: lo que sigue calcula sobre nada.

Los accesores le ponen un solo nombre a lo mismo, y andan igual sobre
los resultados de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md):

``` r

n_filas(perfil)
#> [1] 13
nrow(hallazgos(perfil))
#> [1] 23
nrow(cobertura(perfil))
#> [1] 9
campos <- c("columna", "tipo_inferido", "n_faltantes_totales", "n_distintos")
columnas(perfil)[1:4, campos]
#>          columna tipo_inferido n_faltantes_totales n_distintos
#> 1    id_registro         doble                   0          11
#> 2 codigo_usuario         texto                   1          11
#> 3   fecha_evento         fecha                   1          12
#> 4          canal         texto                   2           8
```

[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
trae, además del nombre de cada columna, 117 campos analíticos; su
significado, por familias, está en
[`?perfilar`](https://sebollin.github.io/lupa/reference/perfilar.md).
Los accesores no inventan lo que no hay:
[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
lista las consultas que se enviaron al motor, y sobre un perfil en
memoria devuelve `NULL`, porque una tabla vacía sugeriría que se
consultó y no se encontró nada.

``` r

is.null(sql_perfil(perfil))
#> [1] TRUE
```

Del mismo modo, un perfil de una base sin filas leídas devuelve una
tabla de hallazgos vacía con su aviso. Cómo se leen los dos bloques de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
está en [10. Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.html).

## Las claves

### Declarar y comprobar

`clave` dice qué columnas identifican una fila en el sistema de origen.
Con ella, `lupa` hace dos cosas. La trazabilidad trae los valores de la
clave, y `trazabilidad$localizador` dice con qué se localizó
(`clave_declarada`, o `indice_fila` si no hay clave). Además comprueba
dos garantías por separado y las deja en `perfil$meta$clave`: que la
clave no se repita y que no tenga ausentes.

``` r

perfil$meta$clave$unicidad$estado
#> [1] "refutada"
perfil$meta$clave$ausencia_nulos$estado
#> [1] "verificada"
repetida <- h[h$tipo_hallazgo == "clave_no_unica", ]
repetida$trazabilidad[[1]]$localizador
#> [1] "clave_declarada"
repetida$trazabilidad[[1]]$claves
#>   id_evento
#> 1    EVT001
#> 2    EVT001
writeLines(strwrap(repetida$sugerencia, 88))
#> Revisar si sobra una columna en la clave, si hay duplicados de carga o si la clave real
#> es otra. La trazabilidad sigue localizando por ella, pero un valor puede señalar más de
#> una fila.
```

`clave_no_unica` es un `error` con las filas que repiten. Acá las dos
filas con `EVT001` son la misma fila cargada dos veces; la sugerencia
enumera las otras causas posibles.

`id_registro` también parece una clave, y su repetición es de otra
clase. Declarada como clave, `clave_no_unica` señala más filas que
`filas_duplicadas`:

``` r

por_registro <- perfilar(datos_operativos, clave = "id_registro")
#> Warning: La clave declarada no es única: entre las 13 filas con la clave completa, 2 repiten su
#> valor (4 filas participan en colisiones). La clave cumple la ausencia de nulos: no tiene
#> valores ausentes.
hr <- hallazgos(por_registro)
hr[hr$tipo_hallazgo %in% c("clave_no_unica", "filas_duplicadas"),
   c("tipo_hallazgo", "n_afectados")]
#>       tipo_hallazgo n_afectados
#> 20   clave_no_unica           4
#> 21 filas_duplicadas           2
datos_operativos[datos_operativos$id_registro == 10,
                 c("id_registro", "monto", "id_evento")]
#>    id_registro monto id_evento
#> 10          10  1700    EVT010
#> 11          10   -50    EVT012
```

Las filas 1 y 12 son copias exactas, y las dos señales las cuentan. Las
dos filas con `id_registro` 10, en cambio, no son copias: dicen montos
distintos para el mismo registro. Sólo la clave declarada las muestra.

### Ausentes en la clave

Que una clave no se repita y que no tenga ausentes son preguntas
distintas. La unicidad se mira sólo entre las filas con la clave
completa, porque en una base de datos dos ausentes (`NULL`) no son
iguales entre sí; los ausentes van a otro hallazgo,
`clave_con_ausentes`.

``` r

tramites <- data.frame(
  id_tramite = c("T1", "T2", NA, NA, "T5"),
  monto = c(10, 20, 30, 40, 50)
)
p_tramites <- perfilar(tramites, clave = "id_tramite")
hallazgos(p_tramites)[, c("columna", "tipo_hallazgo", "severidad", "n_afectados")]
#>      columna         tipo_hallazgo  severidad n_afectados
#> 1 id_tramite posible_identificador         ok           1
#> 2 id_tramite             faltantes sospechoso           2
#> 3       <NA>    clave_con_ausentes      error           2
p_tramites$meta$clave$unicidad$estado
#> [1] "verificada"
p_tramites$meta$clave$ausencia_nulos$estado
#> [1] "refutada"
```

Entre las tres filas completas la clave no se repite (`verificada`), y
aun así no cumple `NOT NULL` (`refutada`): dos filas no tienen clave.
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) lo
avisa además con una advertencia, que acá no mostramos.

Si ninguna fila tiene la clave completa, no hay nada que comparar. Decir
que la clave es única sería cierto sobre un conjunto vacío, y engañoso:
por eso ese caso tiene su propio estado.

``` r

vacia <- data.frame(id_tramite = rep(NA_character_, 3), monto = c(1, 2, 3))
perfilar(vacia, clave = "id_tramite")$meta$clave$unicidad$estado
#> [1] "sin_casos_evaluables"
```

### Sugerir y elegir una clave

Si no sabemos cuál es la clave,
[`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md)
ordena las columnas candidatas por tres señales que publica por
separado: si la columna identifica cada fila, si no tiene ausentes y
cuánto se parece su nombre al de una clave.

``` r

nrow(sugerir_clave(datos_operativos))
#> [1] 0
candidatas <- sugerir_clave(datos_operativos, umbral_casi = 0.9)
candidatas[, c("columna", "identifica", "sin_faltantes", "tasa_distintos",
               "parecido_nombre")]
#>        columna identifica sin_faltantes tasa_distintos parecido_nombre
#> 1    id_evento      FALSE          TRUE      0.9230769               2
#> 2 fecha_evento      FALSE          TRUE      0.9230769               0
#> 3        monto      FALSE          TRUE      0.9230769               0
#> 4     contacto      FALSE          TRUE      0.9230769               0
writeLines(candidatas$motivo[1])
#> no identifica: 92.3% de valores distintos, el resto repite; su nombre es el de una clave
```

Con la fila duplicada, ninguna columna identifica las 13 filas, y
ninguna llega al 95 % de valores distintos que pide `umbral_casi` por
omisión: la primera llamada no devuelve nada. Con un umbral menor
aparecen las que casi identifican, que suelen ser claves con duplicados
de carga. `id_evento` va primera por su nombre. `monto` también aparece:
un importe que casi no se repite. Ordenar no es decidir; esa diferencia
no está en los datos.

[`elegir_clave()`](https://sebollin.github.io/lupa/reference/elegir_clave.md)
muestra esas candidatas numeradas, con su motivo, y una opción para
escribir otra (o varias separadas por coma, para una clave compuesta).
Devuelve la elegida, lista para
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):

``` r

clave <- elegir_clave(datos_operativos)
perfil <- perfilar(datos_operativos, clave = clave)
```

En un guion que corre solo no pregunta: devuelve `NULL` y avisa qué
habría ofrecido. En una base de datos, la clave primaria declarada en el
catálogo del motor se lee en vez de sugerirse; qué garantiza cada motor
está en el artículo [Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html).

## Contradicciones entre columnas

A veces cada columna está bien y la fila se contradice. El año de la
fecha de ingreso, el año fiscal y el año del archivo pueden ser
plausibles cada uno por su cuenta y no coincidir entre sí. Ninguna
columna sola lo muestra, así que el perfil no lo ve.

El grupo lo declaramos nosotros con
[`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md);
`lupa` no lo adivina, porque dos columnas de año pueden ser el de
nacimiento y el de ingreso, y no tienen por qué coincidir.
`transformacion` lleva la fecha a su año para compararla con las otras
dos.

``` r

expedientes <- data.frame(
  expediente = sprintf("EXP-%02d", 1:6),
  fecha_ingreso = as.Date(c(
    "2023-03-10", "2023-11-02", "2024-01-15", "2023-12-30", "2024-02-01", NA
  )),
  anio_fiscal = c(2023L, 2023L, 2024L, 2024L, 2024L, 2024L),
  anio_archivo = c(2023L, 2023L, 2024L, 2023L, 2024L, 2024L)
)
anio <- senal_redundante(
  c("fecha_ingreso", "anio_fiscal", "anio_archivo"),
  transformacion = list(fecha_ingreso = function(x) as.integer(format(x, "%Y"))),
  nombre = "año del expediente"
)
discordancias <- detectar_discordancias(expedientes, anio)
discordancias[, c("senal", "n_filas", "n_evaluadas", "n_discordantes")]
#>                senal n_filas n_evaluadas n_discordantes
#> 1 año del expediente       6           5              1
writeLines(strwrap(discordancias$evidencia, 88))
#> fila 4: fecha_ingreso=2023; anio_fiscal=2024; anio_archivo=2023; universo: 5 de 6 filas
#> con todas las columnas presentes
```

La fila 4 entró el 30 de diciembre de 2023, figura en el año fiscal 2024
y se archivó en 2023. La fila 6 no tiene fecha: no se cuenta ni como
discordante ni como concordante, y sale del universo evaluado
(`n_evaluadas`). Ausencia no es desacuerdo. Para tolerar diferencias,
`ventana` fija cuánto pueden separarse los valores numéricos (con años,
`ventana = 1` acepta un año).

## Glosario

Las palabras que usan el paquete y sus guías, en orden alfabético.

- **alcance**: sobre qué filas y con qué método se calculó una cifra. En
  memoria,
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  cuenta ausentes, distintos y duplicados sobre todas las filas, e
  infiere patrones, tipos, formatos de fecha y dependencias sobre una
  muestra de hasta 100.000 filas (`muestra`).
- **`alta_cardinalidad`**: hallazgo `sospechoso` de una columna de texto
  con muchos valores distintos. Puede ser texto libre, un identificador
  o una categoría mal normalizada; lo decide quien conoce la columna.
- **centinela**: un valor que se usa para decir “sin dato”, como `-99`,
  `"S/D"` o `"NULL"`. En la API se escriben `sentinelas_*`
  (`sentinelas_numericos`, `sentinelas_naniar`). Los que trae el paquete
  por omisión se informan; los que declaramos, además, salen del resumen
  numérico.
- **cobertura**: tiene dos sentidos. La *cobertura de diagnósticos*
  (`cobertura(perfil)`) lista los controles que no se pudieron ejecutar,
  con su motivo. La *cobertura de un marco*
  ([`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md))
  dice qué factores del marco quedaron medidos, sin declarar o fuera de
  alcance.
- **dimensión y factor**: los dos niveles de un marco. Por ejemplo, la
  dimensión Exactitud agrupa el factor Correctitud sintáctica.
- **entidad, atributo, instancia**: el vocabulario de los marcos de
  calidad. Entidad es una tabla, atributo es una columna, instancia de
  entidad es una fila (una tupla) e instancia de atributo es una celda.
  Así, `instanciar(m, "personas", "documento")` se lee “aplicar la
  métrica `m` a la columna `documento` de la tabla `personas`”. La
  correspondencia completa está en
  [`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
  (abajo).
- **evidencia, sugerencia, trazabilidad**: en un hallazgo, lo que se
  vio, lo que se podría hacer y en qué filas está.
- **granularidad**: el nivel al que se mide una métrica: celda, columna,
  fila, tabla, base de datos.
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
  sube de un nivel a otro.
- **hallazgo**: una fila de `hallazgos(perfil)`: una señal sobre los
  datos, con su tipo, su severidad, su evidencia y las filas afectadas.
- **marco**: una taxonomía de dimensiones y factores contra la que se
  lee la cobertura de un análisis
  ([`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  o los publicados:
  [`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  [`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
  [`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)).
  Clasifica; no calcula nada.
- **métrica**: una regla que se aplica a una celda, una fila, una
  columna o una tabla y devuelve un resultado. `NoNulo`, por ejemplo, da
  1 si la celda tiene valor y 0 si falta.
- **modelo de calidad**: el conjunto de métricas instanciadas que se van
  a medir
  ([`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)).
  No calcula un índice global. El argumento `modelo` de
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  recibe un marco, no un modelo.
- **perfil**: el resultado de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md);
  con otra forma, el de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  y el de
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md).
- **perfil de evaluación**: otra cosa: un conjunto de reglas con umbral
  ([`perfil_evaluacion()`](https://sebollin.github.io/lupa/reference/reglas_evaluacion.md))
  que
  [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
  aplica a una medición.
- **plan de limpieza**: una tabla editable de acciones posibles
  ([`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)).
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  ejecuta sólo las que tienen `aplicar = TRUE`, sobre una copia, y deja
  un registro.
- **severidad**: `ok` (informativo), `sospechoso` (merece revisión) o
  `error` (cruzó un criterio declarado), en ese orden.
- **universo**: tiene dos sentidos. El *universo de una columna* son las
  filas donde corresponde que tenga valor: por omisión todas, y se
  recorta con `aplicabilidad`. En
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
  el argumento `universo` dice sobre qué filas del motor se calculan los
  agregados SQL: `"tabla_completa"`, por omisión, o `"muestra_motor"`.
- **vacío por diseño**: una celda vacía que es correcta, porque la
  columna no corresponde a esa fila: quien no tiene auto no tiene marca
  de auto. Se declara; lo explica [5. El vacío por
  diseño](https://sebollin.github.io/lupa/articles/vacio-por-diseno.md).

La tabla que traduce las granularidades al vocabulario de las bases de
datos:

``` r

granularidades()[, c("nivel", "granularidad", "relacional")]
#>    nivel           granularidad           relacional
#> 1      1      instanciaAtributo                celda
#> 2      2               atributo              columna
#> 3      3      conjuntoAtributos conjunto de columnas
#> 4      4       instanciaEntidad                tupla
#> 5      5                entidad                tabla
#> 6      6      conjuntoEntidades   conjunto de tablas
#> 7      7              coleccion        base de datos
#> 8      8    conjuntoColecciones                 <NA>
#> 9      9           organizacion                 <NA>
#> 10    10 conjuntoOrganizaciones                 <NA>
```

Las métricas, los modelos y los marcos se usan en [7. Del hallazgo a la
métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.html)
y [8. Marcos y modelos de
calidad](https://sebollin.github.io/lupa/articles/marcos-y-modelos.html).

## Resumen

- La severidad se ordena `ok < sospechoso < error`: `ok` informa,
  `sospechoso` pide revisión y sólo `error` puede alimentar una barrera
  automática, siempre junto con
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md).
- Cada hallazgo trae evidencia, sugerencia, conteos con su unidad y
  trazabilidad.
- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  y
  [`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  leen igual cualquier perfil.
- Una clave declarada se comprueba: `clave_no_unica`,
  `clave_con_ausentes` y el estado `sin_casos_evaluables`.
  [`sugerir_clave()`](https://sebollin.github.io/lupa/reference/sugerir_clave.md)
  ordena candidatas; no decide.
- [`senal_redundante()`](https://sebollin.github.io/lupa/reference/senal_redundante.md)
  declara columnas que dicen lo mismo, y
  [`detectar_discordancias()`](https://sebollin.github.io/lupa/reference/detectar_discordancias.md)
  encuentra las filas donde se contradicen.

## Paso siguiente

[3. Explorar columnas con
`analizar()`](https://sebollin.github.io/lupa/articles/explorar-columnas.md)
([`vignette("explorar-columnas")`](https://sebollin.github.io/lupa/articles/explorar-columnas.md))
recorre lo que el perfil no muestra en una tabla: patrones, tipos,
distribuciones, asociaciones y tiempo.

# Escala y duplicados

Esta guía muestra cómo elegir una estrategia para comparar registros sin
ocultar el costo ni la pérdida de alcance. La comparación aproximada
está apagada en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md);
se solicita de forma explícita y siempre informa qué pares se miraron.

Las llamadas a `stringdist` usan por omisión
`nucleos = getOption("lupa.nucleos", 2L)`. El valor efectivo queda en
`alcance$nucleos_usados`; se puede cambiar por llamada o mediante la
opción, pero la cantidad de hilos no cambia los pares ni los hallazgos,
sólo el tiempo. Las mediciones de escala fijan dos hilos para que los
tiempos sean comparables entre corridas.

Los tiempos de las dos tablas siguientes se midieron en la máquina que
describe `benchmark/datos/entorno.csv` (un Intel Core i9-14900HX, Linux,
R 4.6.1). Son una referencia de escala, no una predicción para otra
máquina.

Sobre el padrón sintético de `benchmark/_padron_sintetico.R` —100.000
filas, vocabulario chico con homónimos y erratas sembradas, la forma que
más llena las cubetas del LSH— con 140.097.499 candidatos y 205.865
pares informados, la cantidad de hilos produjo estas medianas (tres
procesos separados por configuración; `benchmark/medir_figuras.R` las
deja en `benchmark/datos/hilos.csv` y es lo que dibuja el cuarto panel
de la figura de escala del README):

| hilos | mediana (s) | relativo a 2 |
|------:|------------:|-------------:|
|     2 |      153,20 |        1,00x |
|     4 |      118,38 |        0,77x |
|     8 |       99,29 |        0,65x |
|    16 |       85,01 |        0,55x |
|    29 |       83,27 |        0,54x |

Sobre otro padrón sintético —100.000 filas, 23.800 nombres distintos, la
misma máquina; lo genera `benchmark/medir_escala_hilos.R`— la curva es
mucho más plana:

| hilos | mediana (s) | relativo a 2 |
|------:|------------:|-------------:|
|     2 |      324,36 |        1,00x |
|     4 |      303,84 |        0,94x |
|     8 |      296,93 |        0,92x |
|    16 |      290,42 |        0,90x |
|    31 |      302,18 |        0,93x |

**La ganancia no es una propiedad del paquete: depende de qué fracción
del trabajo cae en la parte que se paraleliza.** Los hilos los usa
`stringdist` al comparar; generar los candidatos y armar los grupos no
los usa. En el primer padrón la comparación domina el reloj y dieciséis
hilos lo bajan a 0,55× de lo que tarda con dos; en el segundo domina el
resto y la ganancia es de un 10 % (290,42 s frente a 324,36 s). Quien
vaya a subir el valor conviene que mida su propio caso, no que copie una
de estas dos tablas.

Lo que **sí** vale en las dos: pasados dieciséis hilos la ganancia queda
dentro del ruido entre corridas (un 2 % en la primera tabla, con
corridas de 16 y de 29 hilos que se solapan; ninguna en la segunda) —el
valor predeterminado de `stringdist`, todos los núcleos menos uno, no es
el mejor— y el resultado no cambia, sólo el reloj. En la primera tabla,
las cinco configuraciones dan los mismos 140.097.499 candidatos y los
mismos 205.865 pares, y el medidor se detiene si las tres corridas de
una configuración no coinciden; en la segunda, la comprobación compara
los 50.000 pares informados uno por uno entre las cinco configuraciones,
y no su cantidad: como el tope de resultados se alcanza en todas, contar
pares habría dado siempre el mismo número y la comprobación se habría
cumplido sola. El valor por omisión de dos hilos es deliberadamente
conservador.

## Un conjunto pequeño

``` r

library(lupa)
datos <- data.frame(
  id = 1:8,
  nombre = c(
    "Ana Perez", "Ana Peres", "Luis Silva", "Luis Silva",
    "Marta Gómez", "Marta Gomez", "Rosa Díaz", "Rosa Diaz"
  ),
  domicilio = c(
    "Calle 1 123", "Calle 1 123", "Ruta 5 40", "Ruta 5 40",
    "Plaza 2 8", "Plaza 2 8", "Calle 9 20", "Calle 9 20"
  ),
  anio = c(2022, 2022, 2021, 2021, 2020, 2020, NA, NA),
  stringsAsFactors = FALSE
)
```

## Estimar antes de comparar

[`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
es un acto deliberado: devuelve la cantidad prevista de candidatos, la
muestra usada y, cuando corresponde, un pronóstico de tiempo marcado
como no determinista. En el camino exacto la cuenta de pares es exacta.

``` r

estimacion <- estimar_costo(
  datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
  lsh_muestra_estimacion = 100
)
estimacion[c(
  "candidatos_previstos", "probabilidad_candidato_estimada",
  "muestra_estimacion", "vocabulario"
)]
#> $candidatos_previstos
#> [1] 4
#> 
#> $probabilidad_candidato_estimada
#> [1] 0.1428571
#> 
#> $muestra_estimacion
#> [1] 28
#> 
#> $vocabulario
#> [1] 72
```

El pronóstico de tiempo, cuando aparece, es un piso de la etapa de
comparación `stringdist`: no incluye la firma MinHash, el bandeo, las
cubetas ni el troceo. Su campo `estimacion$tiempo_estimado_etapa` lo
deja explícito.

Con dos hilos, ese piso cubrió aproximadamente el 80 % del tiempo total
en el control de 100.000 filas (106,18 s estimados frente a 133,28 s
medidos). Sigue siendo un piso de la comparación `stringdist`, no una
promesa de duración de la corrida completa.

Si el pronóstico supera el límite elegido, `presupuesto_pares` corta
antes de comparar. En una sesión interactiva se puede decidir
explícitamente continuar; en un script no se solicita entrada.

## Un tope de largo para la señal de proximidad

La distancia normalizada sirve para nombres, direcciones e
identificadores, pero pierde capacidad de distinguir documentos largos.
En cinco semillas de textos aleatorios, un par con una diferencia y otro
con 1.000 diferencias dio estas medianas sobre cinco semillas:

|  largo | una diferencia | 1.000 diferencias |
|-------:|---------------:|------------------:|
| 10.000 |       0,000400 |          0,193195 |
| 20.000 |       0,000150 |          0,125923 |
| 25.000 |       0,000347 |          0,104511 |
| 50.000 |       0,000053 |          0,064559 |

Con el umbral predeterminado `0,10`, el segundo par cruza hacia un falso
parecido entre 25.000 y 50.000 caracteres. Por eso el tope
predeterminado es `max_largo_valor = 10000`: deja margen antes de ese
cruce y no aplica una distancia sin señal a textos largos. Una columna
que lo supera queda fuera de la combinacion completa y se declara en
`alcance$columnas_excluidas_largo`. En
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
la cobertura explica la columna, el largo observado y el umbral. `Inf`
es la eleccion explicita para recuperar el comportamiento anterior; en
el diagnostico de vocabulario se escribe como
`max_largo_valor_vocabulario = Inf`.

### Qué largo es el que cuenta

El tope no se mide sobre el valor tal como está guardado, sino sobre
**la cadena que entra a la comparación**: las columnas ya combinadas y
ya normalizadas. La diferencia no es teórica y se ve en dos casos que
parecen inocentes.

Dos columnas de nueve mil caracteres cada una están las dos por debajo
del tope, y la comparación no las mira por separado: las une con un
separador y compara los dieciocho mil de la fila entera. Y la
normalización `amplio` **expande ligaduras** —la tipográfica de f-f-l es
un solo carácter que se convierte en tres—, de modo que un valor puede
quedar por debajo del tope guardado y por encima del comparado.

Por eso `alcance$largo_maximo` publica el largo **comparado** y no el
guardado. Cuando el tope no aplica —`Inf`— no se mide ningún largo, y
ese campo vale `NA`: un cero ahí sería afirmar una medición que no
ocurrió.

### Dos topes, dos perillas

`max_largo_valor_vocabulario` gobierna la regla de vocabulario dentro de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
El detector de pares de filas tiene el suyo y **no se hereda**: se
configura por separado, con
`duplicados_aproximados = list(max_largo_valor = ...)`. Son dos análisis
distintos sobre los mismos datos, y ponerle a uno el tope del otro sería
decidir por quien perfila.

## Avisar el costo de tablas anchas

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
calcula una proyeccion antes de empezar las etapas costosas. Desde
`100.000` celdas avisa en una sesion interactiva, con una referencia de
10.000 celdas por segundo y un texto que dice que es una estimacion y
cual es su fuente. El aviso queda en silencio en scripts no interactivos
y se puede desactivar con `avisar_costo_tabla_ancha = FALSE`; usar
`umbral_celdas_aviso_tabla_ancha = Inf` tambien es una decision
explicita. La proyeccion completa queda en `meta$costo_tabla_ancha`.

## Comparación exacta y LSH

Hasta el tope de pares, la estrategia por teselas es exhaustiva y
conserva la misma respuesta que una corrida sin lotes. Para tablas
grandes, LSH genera candidatos mediante MinHash y declara en `alcance`
su muestra, sus bandas y la garantía de colisión.

``` r

exacto <- detectar_duplicados_aproximados(
  datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
  max_resultados = 100
)
lsh <- detectar_duplicados_aproximados(
  datos, columnas = c("nombre", "domicilio"), estrategia = "lsh",
  max_resultados = 100
)
#> LSH: 4 candidatos previstos; referencia de 0,000 s (piso, no incluye firma ni cubetas;
#> subir nucleos puede acortar esta etapa; hoy usa 2 hilos), medida con 37.044 pares en
#> 0,050 s.
exacto$pares[, c(
  "fila_1", "fila_2", "distancia", "tipo_par", "igualo_normalizar"
)]
#>   fila_1 fila_2 distancia           tipo_par igualo_normalizar
#> 1      3      4 0.0000000             exacto             FALSE
#> 2      5      6 0.0000000 exacto_normalizado              TRUE
#> 3      7      8 0.0000000 exacto_normalizado              TRUE
#> 4      1      2 0.0173913         aproximado             FALSE
lsh$alcance[, c("modo_comparacion", "n_pares_comparados", "n_pares_hallados")]
#>   modo_comparacion n_pares_comparados n_pares_hallados
#> 1      lsh_minhash                  4                4
```

`tipo_par = "exacto"` se reserva para textos guardados iguales.
`"exacto_normalizado"` identifica una coincidencia producida por la
normalización y `igualo_normalizar` la marca fila a fila; `"aproximado"`
queda para los pares que siguen siendo similares. Un par parecido nunca
se presenta como identidad y no se propone eliminar ni fusionar
registros. Si `stringdist` no está disponible, el resultado declara que
la comparación no se ejecutó en vez de fallar con un error críptico.

Los duplicados exactos que informa
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
siguen otra unidad: el hallazgo `filas_duplicadas` cuenta todas las
filas que participan en grupos duplicados y las enumera en la traza. La
evidencia conserva además cuántas son excedentes, porque esa cifra sólo
corresponde a la acción que conserva la primera fila.

## Bloquear declarando la pérdida

Una clave indicada por el usuario puede bajar la constante del trabajo,
pero es con pérdida: dos filas cuya clave difiere no se comparan. Las
filas con `NA` forman un bloque propio y también quedan contabilizadas.
`alcance` informa los pares fuera del alcance y su estimación sobre la
muestra.

En `perfilar(clave = ...)`, la unicidad y la ausencia de nulos son
comprobaciones separadas. `meta$clave` conserva el estado de cada una
cuando alguna no queda verificada: una colisión entre `NA` puede ser
útil para seguir la traza con la semántica de R, pero no prueba una
colisión de `NULL` en SQL; los ausentes son la evidencia independiente
contra `NOT NULL`.

Por eso la unicidad se evalúa **sólo entre las filas con la clave
completa**, y `filas_evaluadas` cuenta esas filas frente a
`filas_totales`. Si ninguna fila tiene la clave completa, el estado no
es `verificada` —sería cierto sobre un conjunto vacío— sino
`sin_casos_evaluables`. Una clave sin ausentes y única no agrega ese
metadato al perfil histórico.

``` r

bloqueado <- detectar_duplicados_aproximados(
  datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
  bloquear_por = "anio", max_resultados = 100
)
bloqueado$alcance[, c(
  "bloqueo_por", "bloqueo_pares_fuera_alcance",
  "bloqueo_perdida", "bloqueo_severidad"
)]
#>   bloqueo_por bloqueo_pares_fuera_alcance bloqueo_perdida bloqueo_severidad
#> 1        anio                          24            TRUE        sospechoso
```

El paquete no adivina una clave por su nombre ni por una jerarquía
territorial: la columna la elige quien conoce el dato.

## Lotes sin pérdida

`lotes = TRUE` escribe parciales en un directorio elegido por el usuario
(por omisión se usa uno temporal), cruza los grupos y une la misma
respuesta exacta. El objeto declara los archivos, bytes, directorio y
que los parciales no son reanudables. La comparación sigue siendo
exacta; sólo cambia cuánta memoria se usa a la vez.

``` r

dir <- tempfile("lupa-lotes-")
por_lotes <- detectar_duplicados_aproximados(
  datos, columnas = c("nombre", "domicilio"), estrategia = "teselas",
  lotes = TRUE, tamano_lote = 3, directorio_lotes = dir,
  max_resultados = 100
)
por_lotes$lotes[c(
  "directorio", "n_parciales", "bytes_totales", "reanudable", "perdida"
)]
#> $directorio
#> [1] "/tmp/Rtmpp4oKv6/lupa-lotes-21831eb6cb5a/lupa-lotes-21834c8486c3"
#> 
#> $n_parciales
#> [1] 6
#> 
#> $bytes_totales
#> [1] 1608
#> 
#> $reanudable
#> [1] FALSE
#> 
#> $perdida
#> [1] FALSE
unlink(dir, recursive = TRUE)
```

Para decidir si la operación cabe, se recomienda llamar primero a
[`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
y establecer `presupuesto_pares` en la detección. Así la decisión queda
separada del recorrido y el alcance permanece auditable.

## Presupuestos que miden trabajo y no unidades

Un tope que cuenta unidades trata igual a una columna de códigos de diez
caracteres y a una de WKT de mil. Una tabla del catálogo de PostGIS
—3.912 filas, 5 columnas— tardaba **243 segundos**, y el detector de
vocabulario era el 99,6 % del costo: 800 valores distintos son 319.600
pares, muy por debajo del tope de dos millones, pero cada comparación
era una Jaro-Winkler sobre 900 caracteres.

Por eso `max_trabajo_vocabulario` se mide en **comparaciones de
carácter**, que es el bucle interno de la distancia: comparar dos
valores de largos `L1` y `L2` cuesta del orden de `L1 × L2`. Contar
pares por longitud media no alcanzaba —el mismo presupuesto compraba 5,3
millones de unidades por segundo con valores de 900 caracteres y 44
millones con valores de 40—.

El valor por omisión está calibrado contra la medición, no contra la
intuición, y el criterio no fue sólo cortar el caso patológico sino **no
tocar el común**:

| valores | largo | sin tope | con tope | comparado |
|---------|-------|----------|----------|-----------|
| 800     | 900   | 61,3 s   | 4,6 s    | 27,8 %    |
| 2000    | 80    | 5,0 s    | 5,1 s    | 100 %     |

Una columna corriente de dos mil valores se compara entera **mientras
sus valores midan menos de unos cien caracteres**. El tope por trabajo
muerde cuando `L² · n(n-1)/2` supera `2e10`, así que para dos mil
valores distintos el corte está en 101 caracteres:

| valores | largo | ¿se recorta?              | formas comparadas |
|--------:|------:|---------------------------|------------------:|
|    2000 |    80 | no                        |              2000 |
|    2000 |   100 | no                        |              2000 |
|    2000 |   101 | **sí**, por `max_trabajo` |              1980 |
|    2000 |   200 | sí, por `max_trabajo`     |              1000 |

Conviene tenerlo presente porque la columna que motivó el presupuesto
—WKT de 900 caracteres— cae del lado recortado: la tranquilidad de «dos
mil valores se comparan enteros» no alcanza a los datos que hicieron
falta el tope.

Y lo que sí se recorta **se declara**, con las dos cuentas separadas:
cuántas formas normalizadas quedaron sin comparar y cuánto trabajo eran,
junto con cuál de los topes recortó, en el alcance del hallazgo y en
`cobertura_diagnosticos`. Si aprietan los dos, el motivo los nombra a
los dos, porque hay que poder elegir cuál aflojar.

Un detalle que conviene saber antes de confiar en un recorte: las formas
que sí se comparan son **las primeras en orden alfabético**, no una
muestra ni las primeras en aparecer. Lo que queda afuera es el tramo
final del alfabeto, y eso se declara.

Que sea el alfabeto y no el orden de llegada no es una preferencia de
estilo: **tomarlas en orden de llegada hacía que el veredicto dependiera
de cómo viniera ordenado el archivo.** Medido sobre la columna `nombre`
de *Ejes de vías de circulación* de Montevideo —45.400 filas, 8.318
formas distintas, del catálogo nacional de datos abiertos—, las mismas
filas daban:

| orden de las filas          | grupos de casi-duplicados |
|-----------------------------|---------------------------|
| tal como viene el archivo   | 26                        |
| desordenado (tres semillas) | 70, 71, 85                |
| alfabético                  | 148                       |

Un perfilador que da 26 o 148 según el orden de las filas está midiendo
la forma física de la tabla y no los datos. Ordenando antes de recortar,
los cinco órdenes dan **148**.

El alfabeto además no es una elección arbitraria entre órdenes estables:
deja los casi-duplicados **adyacentes** —`CAMINO CARRASCO` al lado de
`CAMINO AGRARIOS`—, así que el corte cae entre familias en vez de
partirlas. Una muestra al azar rompe pares: si de un grupo de dos
sobrevive uno, el grupo desaparece. Por eso el azar rinde 70–85 y el
orden rinde 148 con el mismo presupuesto.

[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)
tiene el suyo, `max_trabajo`, en unidades **fila-par**: ahí el costo es
del orden de `columnas² × filas`, y `max_comparaciones` no lo veía —158
columnas son 24.806 pares, muy por debajo de las 200.000 del tope—. Se
combina con `max_comparaciones` y manda el más restrictivo. Desde
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
los dos presupuestos se llaman `max_trabajo_vocabulario` y
`max_trabajo_dependencias`, y `Inf` desactiva cualquiera de ellos.

El banco `benchmark/medir_costo_texto.R` reproduce el barrido completo,
para que la calibración se pueda rehacer si cambia la implementación de
la distancia.

## El recorte de pares, y por qué también se ordena por contenido

Hay un segundo recorte, y hasta acá lo veníamos usando sin mirarlo:
`max_resultados`, que en todos los ejemplos de arriba vale `100`. Ese
tope no decide **qué formas se comparan** —eso era el presupuesto de
vocabulario— sino **cuántos pares se devuelven** una vez comparadas.

Conserva los más cercanos, ordenando por distancia. El problema aparecía
entre pares **empatados**: ahí desempataba por posición de fila, así que
cuáles sobrevivían al corte dependía del orden en que llegaron las
filas.

No es un caso raro. Los empates son la regla cuando las variantes se
parecen del mismo modo —un espacio de más, un sufijo societario, una
letra caída—, porque la distancia sale idéntica. Medido sobre 60 grupos
construidos para que sus 60 pares internos compartan exactamente la
misma distancia, con el corte en 30:

| orden de las filas       | grupos representados | en común con el natural |
|--------------------------|----------------------|-------------------------|
| natural                  | 30                   | —                       |
| inverso                  | 30                   | **0**                   |
| barajado (tres semillas) | 30                   | **0**                   |

Cinco órdenes, treinta grupos cada uno, ni uno compartido. Es el mismo
defecto que la sección anterior ya había encontrado y resuelto para el
recorte de formas, así que la respuesta es la misma: **ordenar por
contenido**. El desempate usa ahora el rango canónico del valor, con una
clave simétrica —`min` y `max` del rango— para que tampoco dependa de
cuál fila quedó primera dentro del par. Los cinco órdenes devuelven
exactamente los mismos grupos.

El rango se calcula una sola vez sobre el universo completo de la
corrida, no por lote ni por tesela: una numeración local volvería a
hacer que la comparación entre lotes dependiera de cómo se repartieron.

### Lo que ordenar no arregla

Un corte dentro de un empate deja afuera pares **igual de cercanos** que
los que conserva. Eso no lo arregla ningún orden, y por eso se declara:

``` r

alcance$distancia_corte        # la distancia donde cayó el corte
alcance$n_en_distancia_corte   # cuántos de los conservados la comparten
alcance$corte_en_empate        # TRUE si el corte cayó dentro de un empate
```

`corte_en_empate` **no es `truncado` con otro nombre**: cuando el corte
cae en una distancia única, da `FALSE` aunque haya habido recorte. Si da
`TRUE`, el número de grupos que ve es un subconjunto de los que había
—estable y reproducible, pero subconjunto—, y subir `max_resultados` por
encima del empate los trae a todos.

Se mide contra **lo que el recorte descartó**, no contra lo que quedó:
si en el borde sobrevive un solo par, contar los conservados daría 1 y
la señal diría que no hubo empate, cuando puede haber tirado varios a
esa misma distancia.

Y hay un límite que ningún orden saca: si varias filas comparten el
valor comparado, el conjunto de **pares de valores** sale idéntico en
cualquier orden, pero **cuáles filas** los representan cambia. Esas
filas son indistinguibles en la columna que se compara, así que su única
identidad es la posición —justo lo que varía al reordenar—. Si importa
qué instancia se informa, hace falta una clave que las distinga.

### Y una advertencia sobre el camino LSH

Con más de diez mil valores distintos entra MinHash/LSH, y ahí el
**conjunto de candidatos** depende del orden de las filas: el
vocabulario de q-gramas se numera por orden de primera aparición y esa
numeración alimenta las firmas. Reordenar la tabla cambia qué pares se
proponen.

Eso está **dentro de la garantía declarada**, no es un defecto: medido
sobre 1.200 filas, de los 11.822 pares que cambian al barajar, el
Jaccard de q-gramas más alto es 0,7857 y no se pierde **ninguno** por
encima de 0,8, donde el alcance declara un recall de 0,9998. La rotación
está entera en la cola de pares apenas parecidos, donde LSH dice que no
promete nada.

Pero es una propiedad del resultado, así que el alcance la declara y no
hay que venir a buscarla acá:

``` r

alcance$lsh_candidatos_dependen_orden_filas   # TRUE bajo LSH
alcance$lsh_orden_vocabulario                 # "primera_aparicion"
```

El segundo no es decorativo. Nombra **el mecanismo**: el vocabulario se
numera por orden de primera aparición, y si alguna vez esa numeración se
canoniza, ese valor cambia y la dependencia deja de declararse sola.
Ninguno de los dos aparece en el camino exhaustivo, que no tiene esa
dependencia.

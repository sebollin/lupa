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

Los tiempos de la tabla siguiente se midieron en un Intel Core
i9-14900HX de 32 núcleos, Pop!\_OS 22.04 LTS (Linux) y R 4.6.1. Son una
referencia de escala, no una predicción para otra máquina.

En un control de 100.000 filas del padrón difícil, con 140.097.499
candidatos y 29.844 pares informados, la cantidad de hilos produjo estas
medianas (tres procesos separados por configuración). Ese padrón no se
distribuye, así que la tabla no se puede rehacer tal cual;
`benchmark/medir_escala_hilos.R` genera un padrón sintético equivalente
y vuelve a medir la curva, que es lo que esta sección afirma. Conviene
leer las dos tablas juntas, porque no dicen lo mismo:

| hilos | mediana (s) | relativo a 2 |
|------:|------------:|-------------:|
|     2 |      133,28 |        1,00x |
|     4 |       97,44 |        0,73x |
|     8 |       76,19 |        0,57x |
|    16 |       70,31 |        0,53x |
|    31 |       71,69 |        0,54x |

Sobre el padrón sintético del banco —100.000 filas, 23.800 nombres
distintos, la misma máquina— la curva es mucho más plana:

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
los usa. En el padrón difícil la comparación dominaba el reloj y
dieciséis hilos lo bajaban a la mitad; en el sintético domina el resto y
la ganancia es del 12 %. Quien vaya a subir el valor conviene que mida
su propio caso, no que copie una de estas dos tablas.

Lo que **sí** vale en las dos: pasados dieciséis hilos no hubo ganancia
medible —el valor predeterminado de `stringdist`, todos los núcleos
menos uno, no es el mejor— y el resultado no cambia, sólo el reloj. Esto
último está comprobado comparando los 50.000 pares informados uno por
uno entre las cinco configuraciones, y no su cantidad: como el tope de
resultados se alcanza en todas, contar pares habría dado siempre el
mismo número y la comprobación se habría cumplido sola. El valor por
omisión de dos hilos es deliberadamente conservador.

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
#> subir nucleos puede acortar esta etapa; hoy usa 2 hilos), medida con 16.576 pares en
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
#> [1] "/tmp/RtmpT6dEGm/lupa-lotes-22a335895900/lupa-lotes-22a34c8e3767"
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

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
medianas (tres procesos separados por configuración):

| hilos | mediana (s) | relativo a 2 |
|------:|------------:|-------------:|
|     2 |      133,28 |        1,00x |
|     4 |       97,44 |        0,73x |
|     8 |       76,19 |        0,57x |
|    16 |       70,31 |        0,53x |
|    31 |       71,69 |        0,54x |

Pasados dieciséis hilos no hubo una ganancia medible: el valor
predeterminado de `stringdist` (todos los núcleos menos uno) no es el
mejor. El resultado fue el mismo en las cinco configuraciones; sólo
cambió el reloj. El valor por omisión de dos hilos es deliberadamente
conservador y se puede subir cuando la máquina y la carga lo justifican.

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
#> subir nucleos puede acortar esta etapa; hoy usa 2 hilos), medida con 25.368 pares en
#> 0,050 s.
exacto$pares[, c("fila_1", "fila_2", "distancia", "tipo_par")]
#>   fila_1 fila_2 distancia   tipo_par
#> 1      3      4 0.0000000     exacto
#> 2      5      6 0.0000000     exacto
#> 3      7      8 0.0000000     exacto
#> 4      1      2 0.0173913 aproximado
lsh$alcance[, c("modo_comparacion", "n_pares_comparados", "n_pares_hallados")]
#>   modo_comparacion n_pares_comparados n_pares_hallados
#> 1      lsh_minhash                  4                4
```

Un par parecido nunca se presenta como identidad y no se propone
eliminar ni fusionar registros. Si `stringdist` no está disponible, el
resultado declara que la comparación no se ejecutó en vez de fallar con
un error críptico.

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
#> [1] "/tmp/RtmpbQweyP/lupa-lotes-213716664496/lupa-lotes-213733c253df"
#> 
#> $n_parciales
#> [1] 6
#> 
#> $bytes_totales
#> [1] 1516
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

# 12. Escala y duplicados aproximados

Un registro de proveedores puede tener al mismo proveedor dos veces,
escrito de dos maneras. ¿Cómo encontramos esas filas, cuánto cuesta
buscarlas en una tabla grande y qué dejamos de mirar cuando el costo
obliga a recortar?

> **En esta guía:**
>
> - pedimos la comparación aproximada, que está apagada por omisión;
> - estimamos el costo antes de comparar y fijamos un presupuesto;
> - vemos cuándo se compara por teselas y cuándo entra LSH;
> - acotamos el trabajo con bloqueo y con lotes, y leemos qué pierde
>   cada uno;
> - reconocemos los topes que recortan la comparación y dónde lo
>   declaran.

## Un registro de proveedores

``` r

library(lupa)

proveedores <- data.frame(
  id = 1:8,
  proveedor = c(
    "Ferretería Central", "Ferreteria Central", "Distribuidora Sur",
    "Distribuidora Sur", "Lácteos del Este", "Lacteos del Este",
    "Imprenta Norte", "Imprenta Nortes"
  ),
  rubro = c(
    "ferretería", "ferretería", "alimentos", "alimentos", "alimentos",
    "alimentos", "gráfica", "gráfica"
  ),
  anio_alta = c(2022, 2022, 2021, 2021, 2020, 2020, NA, NA),
  stringsAsFactors = FALSE
)
proveedores
#>   id          proveedor      rubro anio_alta
#> 1  1 Ferretería Central ferretería      2022
#> 2  2 Ferreteria Central ferretería      2022
#> 3  3  Distribuidora Sur  alimentos      2021
#> 4  4  Distribuidora Sur  alimentos      2021
#> 5  5   Lácteos del Este  alimentos      2020
#> 6  6   Lacteos del Este  alimentos      2020
#> 7  7     Imprenta Norte    gráfica        NA
#> 8  8    Imprenta Nortes    gráfica        NA
```

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) ya
informa las filas idénticas (`filas_duplicadas`). Las parecidas son otra
pregunta, más cara, y por eso no se responden por omisión: se piden con
`perfilar(datos, duplicados_aproximados = TRUE)` o, como acá,
directamente con
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).
La comparación usa el paquete `stringdist`.

## La primera detección

``` r

pares <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro")
)
pares$pares[, c("fila_1", "fila_2", "distancia", "tipo_par", "igualo_normalizar")]
#>   fila_1 fila_2 distancia           tipo_par igualo_normalizar
#> 1      3      4     0.000             exacto             FALSE
#> 2      1      2     0.000 exacto_normalizado              TRUE
#> 3      5      6     0.000 exacto_normalizado              TRUE
#> 4      7      8     0.008         aproximado             FALSE
pares$alcance[, c("modo_comparacion", "n_pares_comparados", "n_pares_hallados")]
#>         modo_comparacion n_pares_comparados n_pares_hallados
#> 1 exhaustiva_por_bloques                 28                4
```

El detector unió `proveedor` y `rubro` en una sola cadena por fila,
normalizó mayúsculas, espacios y acentos, y comparó los 28 pares
posibles. `tipo_par` separa tres casos:

- `exacto`: los textos guardados son iguales (filas 3 y 4);
- `exacto_normalizado`: son iguales después de normalizar, y
  `igualo_normalizar` lo marca (las tildes de las filas 1 y 2, y de las
  5 y 6);
- `aproximado`: siguen siendo distintos, pero quedan a una distancia
  menor que el umbral (`umbral = 0.10` con Jaro-Winkler, por omisión).

Un par parecido nunca se presenta como identidad, y el detector no
propone eliminar ni fusionar filas: la sugerencia de cada hallazgo es
revisarlas.

## Estimar antes de comparar

En el camino exhaustivo la cuenta es exacta: con `n` filas hay
`n(n - 1)/2` pares.
[`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
la devuelve sin comparar nada, y `presupuesto_pares` corta la detección
**antes** de empezar si la cuenta lo supera:

``` r

estimar_costo(proveedores, columnas = c("proveedor", "rubro"))$candidatos_previstos
#> [1] 28

corte <- tryCatch(
  detectar_duplicados_aproximados(
    proveedores, columnas = c("proveedor", "rubro"), presupuesto_pares = 10
  ),
  error = conditionMessage
)
writeLines(strwrap(corte, 88))
#> La estimación exacta (28 pares) supera `presupuesto_pares` (10). No se inició la
#> comparación; aumente el presupuesto, reduzca los datos, suba el umbral o divida el
#> conjunto por una clave.
```

En un guion la detección se detiene con ese mensaje; en una sesión
interactiva se puede decidir continuar. Separar la estimación de la
corrida deja la decisión a la vista, antes de pagar el costo.

## Teselas o LSH

Hasta su tope de pares, la comparación es **exhaustiva**: recorre todos
los pares por teselas de `bloque` filas, y la respuesta es la misma que
sin teselas. El tope es `max_pares`, 50.000.000 pares por omisión.

Con `estrategia = "auto"`, el valor por omisión, el detector pasa a
MinHash con LSH cuando se cumplen tres condiciones a la vez:

- la tabla tiene **más de diez mil filas** (diez mil filas son
  49.995.000 pares, justo por debajo del tope por omisión);
- no se pidió una muestra de filas (`muestra = Inf`, el valor por
  omisión);
- `max_pares` es infinito o vale al menos 50.000.000.

Si `max_pares` se baja de ese valor, `auto` no usa LSH: compara una
muestra sistemática de filas y lo declara en `alcance$modo_comparacion`.
Con `estrategia = "lsh"` se fuerza LSH con cualquier tamaño de tabla, y
con `estrategia = "teselas"` se evita.

LSH no compara todos los pares: propone candidatos y compara sólo ésos.
Lo forzamos sobre la misma tabla para ver qué declara:

``` r

lsh <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro"), estrategia = "lsh"
)
str(lsh$alcance[c(
  "modo_comparacion", "lsh_pares_comparados", "lsh_garantia_aplica_a",
  "lsh_candidatos_dependen_orden_filas"
)])
#> 'data.frame':    1 obs. of  4 variables:
#>  $ modo_comparacion                   : chr "lsh_minhash"
#>  $ lsh_pares_comparados               : num 4
#>  $ lsh_garantia_aplica_a              : chr "Jaccard de q-gramas; no garantiza la medida final"
#>  $ lsh_candidatos_dependen_orden_filas: logi TRUE
signif(lsh$alcance$lsh_garantia_jaccard_08, 4)
#> [1] 0.9998
```

La garantía, `lsh_garantia_jaccard_08`, es una probabilidad: un par
cuyos q-gramas tienen un Jaccard de 0,8 o más llega a ser candidato al
menos con esa probabilidad; uno menos parecido puede no llegar. Se
refiere a los q-gramas, no a la distancia final, y el alcance lo dice.
Además, los candidatos dependen del orden de las filas, y el alcance
también lo declara. La estimación de costo del camino LSH se calcula
sobre una muestra reproducible de pares:

``` r

estimacion <- estimar_costo(
  proveedores, columnas = c("proveedor", "rubro"), estrategia = "lsh",
  lsh_muestra_estimacion = 100
)
estimacion[c("candidatos_previstos", "muestra_estimacion", "tiempo_estimado_etapa")]
#> $candidatos_previstos
#> [1] 4
#> 
#> $muestra_estimacion
#> [1] 28
#> 
#> $tiempo_estimado_etapa
#> [1] "comparacion_stringdist"
```

Cuando hay pronóstico de tiempo, es una cota inferior de la etapa de
comparación de `stringdist`: no incluye las firmas, el bandeo ni las
cubetas, y su valor no es determinista, así que no lo imprimimos. Las
mediciones de la pérdida de LSH y de su dependencia del orden están en
el artículo [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

## Bloquear declarando la pérdida

Bloquear compara sólo las filas que comparten el valor de una columna.
Baja el trabajo, pero con pérdida: dos filas de distinto bloque no se
comparan nunca. La columna la elige quien conoce el dato; `lupa` no
adivina una clave de bloqueo.

``` r

bloqueado <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro"), estrategia = "teselas",
  bloquear_por = "anio_alta"
)
str(bloqueado$alcance[c(
  "bloqueo_n_bloques", "bloqueo_n_filas_na", "bloqueo_pares_alcanzables",
  "bloqueo_pares_fuera_alcance", "bloqueo_perdida",
  "bloqueo_candidatos_perdidos_estimados", "bloqueo_perdida_estimacion_estado"
)])
#> 'data.frame':    1 obs. of  7 variables:
#>  $ bloqueo_n_bloques                    : int 4
#>  $ bloqueo_n_filas_na                   : int 2
#>  $ bloqueo_pares_alcanzables            : num 4
#>  $ bloqueo_pares_fuera_alcance          : num 24
#>  $ bloqueo_perdida                      : logi TRUE
#>  $ bloqueo_candidatos_perdidos_estimados: num 0
#>  $ bloqueo_perdida_estimacion_estado    : chr "estimada_por_muestra"
```

Las dos filas sin año forman un bloque propio y también se cuentan. De
los 28 pares, 24 quedaron fuera de alcance, y `bloqueo_perdida` lo
marca. En esta tabla ningún par parecido cruzaba bloques, pero eso es
una estimación sobre una muestra de pares, y el alcance dice que lo es.

## Lotes sin pérdida

Los lotes no cambian qué se compara, sino cuánta memoria se usa a la
vez: los resultados parciales se escriben en un directorio y después se
unen.

``` r

directorio <- tempfile("lupa-lotes-")
por_lotes <- detectar_duplicados_aproximados(
  proveedores, columnas = c("proveedor", "rubro"), estrategia = "teselas",
  lotes = TRUE, tamano_lote = 3, directorio_lotes = directorio
)
por_lotes$lotes[c("n_parciales", "reanudable", "perdida")]
#> $n_parciales
#> [1] 6
#> 
#> $reanudable
#> [1] FALSE
#> 
#> $perdida
#> [1] FALSE
identical(por_lotes$pares, pares$pares)
#> [1] TRUE
unlink(directorio, recursive = TRUE)
```

La respuesta es idéntica a la de la primera detección. El objeto declara
además los archivos, sus tamaños y el directorio, y que los parciales no
sirven para retomar una corrida cortada.

## Qué recorta cada tope

Después de los lotes, cada límite queda resumido en la señal que se
puede leer en la salida. El detalle reproducible está en los artículos
de referencia:

- **Largo de los valores.** `max_largo_valor` deja fuera la combinación
  completa cuando la cadena ya unida y normalizada supera el límite;
  `alcance$columnas_excluidas_largo` lo declara y
  `n_pares_hallados = NA` distingue «no se comparó» de cero. El detalle
  está en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Trabajo del vocabulario.** `max_trabajo_vocabulario` recorta las
  comparaciones entre formas distintas;
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  conserva el diagnóstico y el motivo del recorte, sin presentar los
  valores no revisados como limpios. El detalle está en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Pares devueltos.** `max_resultados` limita la salida a los pares más
  cercanos, y `alcance$truncado` junto con `alcance$corte_en_empate`
  muestran si quedó fuera otro par igual de cercano. El desempate y el
  detalle están en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Tablas anchas.**
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  proyecta el costo en `meta$costo_tabla_ancha` y, sólo en una sesión
  interactiva, emite el aviso cuando corresponde; la salida y los
  interruptores están en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#aviso-tabla-ancha).
- **Hilos.** `nucleos` cambia el tiempo de la comparación de
  `stringdist`, no sus pares; `alcance$nucleos_usados` muestra el valor
  efectivo. Las mediciones sobre distintos números de hilos están en
  [Evidencia y
  rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html#hilos-medidos).

## Resumen

- La comparación aproximada está apagada en
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md);
  se pide con `duplicados_aproximados = TRUE` o con
  [`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md).
- [`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
  cuenta los pares antes de comparar, y `presupuesto_pares` corta antes
  de empezar.
- Por omisión la comparación es exhaustiva; LSH entra con más de diez
  mil filas, sin muestra y sin bajar `max_pares`, y declara su garantía.
- El bloqueo baja el trabajo con pérdida declarada; los lotes no pierden
  nada.
- Cada tope que recorta —largo, trabajo del vocabulario, resultados
  devueltos— lo dice en el alcance o en la cobertura.

## Paso siguiente

Las mediciones de escala, con su fecha, su entorno y el guion que las
reproduce, están en el artículo [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

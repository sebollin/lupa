# 12. Escala y duplicados aproximados

Un registro de proveedores puede tener al mismo proveedor 2 veces,
escrito de 2 maneras. ¿Cómo se encuentran esas filas? ¿Cuánto cuesta
buscarlas en una tabla grande? ¿Y qué se deja de mirar cuando el costo
obliga a recortar?

> **En esta guía:**
>
> - pedir la comparación aproximada, que está apagada por omisión;
> - estimar el costo antes de comparar y fijar un presupuesto;
> - distinguir cuándo se compara por teselas y cuándo entra LSH;
> - acotar el trabajo con bloqueo y con lotes, y ver qué pierde cada
>   uno;
> - leer qué topes recortan la comparación y dónde lo declaran.

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

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
informa siempre las filas idénticas (`filas_duplicadas`). Las parecidas
son otra pregunta, más cara, y por eso no se responden por omisión. Se
piden con `perfilar(datos, duplicados_aproximados = TRUE)` o
directamente con
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
como acá. La comparación usa el paquete `stringdist`.

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

El detector hizo 3 cosas:

- unió `proveedor` y `rubro` en una sola cadena por fila;
- normalizó mayúsculas, espacios y acentos;
- comparó los 28 pares posibles.

`tipo_par` separa 3 casos:

- `exacto`: los textos guardados son iguales (filas 3 y 4);
- `exacto_normalizado`: son iguales después de normalizar, y
  `igualo_normalizar` lo marca. Pasa con las tildes de las filas 1 y 2,
  y de las 5 y 6;
- `aproximado`: siguen siendo distintos, pero quedan a una distancia
  menor que el umbral (`umbral = 0.10` con Jaro-Winkler, por omisión).

Un par parecido nunca se presenta como identidad. El detector no propone
eliminar ni fusionar filas: la sugerencia de cada hallazgo es
revisarlas.

## ⏱ Estimar antes de comparar

En el camino exhaustivo la cuenta es exacta: con `n` filas hay
`n(n - 1)/2` pares.
[`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
la devuelve sin comparar nada. `presupuesto_pares` corta la detección
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
#> comparación; aumentar el presupuesto, reducir los datos, subir el umbral o dividir el
#> conjunto por una clave.
```

En un guion la detección se detiene con ese mensaje; en una sesión
interactiva se puede decidir continuar. Separar la estimación de la
corrida deja la decisión a la vista, antes de pagar el costo.

## Teselas o LSH

Hasta su tope de pares, la comparación es **exhaustiva**: recorre todos
los pares por teselas de `bloque` filas. La respuesta es la misma que
sin teselas. El tope es `max_pares`, 50.000.000 pares por omisión.

El valor por omisión, `estrategia = "auto"`, pasa a MinHash con LSH
cuando se cumplen a la vez 3 condiciones:

- la tabla tiene **más de 10.000 filas** (con 10.000 filas hay
  49.995.000 pares, justo por debajo del tope por omisión);
- no se pidió una muestra de filas (`muestra = Inf`, el valor por
  omisión);
- `max_pares` es infinito o vale al menos 50.000.000.

Si se baja `max_pares` de ese valor, `auto` no usa LSH. Compara una
muestra sistemática de filas y lo declara en `alcance$modo_comparacion`.
Con `estrategia = "lsh"` se fuerza LSH con cualquier tamaño de tabla;
con `estrategia = "teselas"`, se evita.

LSH no compara todos los pares: propone candidatos y compara sólo ésos.
Se lo fuerza sobre la misma tabla para ver qué declara:

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

La garantía, `lsh_garantia_jaccard_08`, es una probabilidad. Un par
cuyos q-gramas tienen un Jaccard de 0,8 o más llega a ser candidato al
menos con esa probabilidad. Uno menos parecido puede no llegar. El
alcance declara 2 propiedades de lo que devuelve LSH:

- la garantía se refiere a los q-gramas, no a la distancia final;
- los candidatos dependen del orden de las filas, dentro de esa
  garantía.

La estimación de costo del camino LSH se calcula sobre una muestra
reproducible de pares:

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
comparación de `stringdist`. No incluye las firmas, el bandeo ni las
cubetas. Su valor no es determinista, y por eso esta página no lo
imprime. Las mediciones de la pérdida de LSH y de su dependencia del
orden están en el artículo [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

## Bloquear declarando la pérdida

Bloquear compara sólo las filas que comparten el valor de una columna.
Baja el trabajo, pero con pérdida: las filas de bloques distintos nunca
se comparan entre sí. La columna la elige quien conoce el dato; `lupa`
no adivina una clave de bloqueo.

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

Las 2 filas sin año forman un bloque propio y también se cuentan. De los
28 pares, 24 quedaron fuera de alcance, y `bloqueo_perdida` lo marca. En
esta tabla ningún par parecido cruzaba bloques. Pero eso es una
estimación sobre una muestra de pares, y el alcance dice que lo es.

## Lotes sin pérdida

Los lotes no cambian qué se compara, sino cuánta memoria se usa a la
vez. Los resultados parciales se escriben en un directorio y después se
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
los archivos, sus tamaños, el directorio y que los parciales no sirven
para retomar una corrida cortada.

## ⚠️ Qué recorta cada tope

Cada límite, con la señal que lo muestra en la salida y el enlace a su
detalle reproducible:

- **Largo de los valores.** `max_largo_valor` deja fuera la combinación
  completa cuando la cadena unida y normalizada supera el límite.
  `alcance$columnas_excluidas_largo` lo declara, y
  `n_pares_hallados = NA` distingue «no se comparó» de cero. El detalle
  está en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Trabajo del vocabulario.** `max_trabajo_vocabulario` recorta las
  comparaciones entre formas distintas.
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  conserva el diagnóstico y el motivo del recorte, sin presentar como
  limpios los valores no revisados. El detalle está en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Pares devueltos.** `max_resultados` limita la salida a los pares más
  cercanos. `alcance$truncado` y `alcance$corte_en_empate` muestran si
  quedó fuera otro par igual de cercano. El desempate y el detalle están
  en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#topes-duplicados).
- **Tablas anchas.**
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  proyecta el costo en `meta$costo_tabla_ancha` y, sólo en una sesión
  interactiva, emite el aviso cuando corresponde. La salida y los
  interruptores están en [Motores y
  escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#aviso-tabla-ancha).
- **Hilos.** `nucleos` cambia el tiempo de la comparación de
  `stringdist`, no sus pares. `alcance$nucleos_usados` muestra el valor
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
- Por omisión la comparación es exhaustiva. También por omisión, LSH
  entra con más de 10.000 filas, sin muestra y sin bajar `max_pares`, y
  declara su garantía.
- El bloqueo baja el trabajo con pérdida declarada; los lotes no pierden
  nada.
- Cada tope que recorta —largo, trabajo del vocabulario, resultados
  devueltos— lo dice en el alcance o en la cobertura.

## 🧭 Paso siguiente

Las mediciones de escala están en el artículo [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).
Cada una trae su fecha, su entorno y el guion que la reproduce.

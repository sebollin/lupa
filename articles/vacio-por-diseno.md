# 5. El vacío por diseño

¿Por qué un perfil puede decir que falta la mayor parte de una columna
que está completa en todas las filas donde corresponde? Porque nadie le
dijo al paquete dónde corresponde.

> **En esta guía:**
>
> - por qué una celda vacía puede ser correcta, y qué informa el perfil
>   si no se declara;
> - la señal con que `lupa` avisa que hay algo para declarar;
> - cómo declaramos el universo de una columna, y qué error nuevo se
>   vuelve visible;
> - todo lo que se le puede declarar al paquete, en una sola tabla.

## El supuesto que `lupa` hace

Todo perfilador de datos asume una forma de tabla. `lupa` asume tres
cosas a la vez:

1.  una fila es un hecho sobre una entidad;
2.  una columna es un dominio semántico;
3.  una celda vacía debería tener un valor y no lo tiene.

Las tres son razonables y ninguna es universal. La tercera es la que
hace daño, porque una base administrativa está llena de vacíos
legítimos: la celda que no corresponde que tenga valor. Sin declarar
nada, `lupa` cuenta esos vacíos como ausencia y los informa como
defecto. El conteo es correcto y la lectura es falsa.

## Qué pasa si no se declara nada

Para que el vacío por diseño se vea entero usamos una encuesta
construida acá, con una columna que sólo corresponde a parte de las
filas; `datos_operativos` no tiene ninguna. Pregunta si la persona tiene
auto y, sólo si tiene, de qué marca:

``` r

tiene_auto <- rep(c("Si", "No"), c(600, 1400))
encuesta <- data.frame(
  tiene_auto = tiene_auto,
  marca_auto = ifelse(tiene_auto == "Si", "Ford", NA),
  stringsAsFactors = FALSE
)

perfil <- perfilar(encuesta, analizar_dependencias = FALSE)
h <- hallazgos(perfil)
h[h$tipo_hallazgo == "faltantes", c("columna", "severidad", "n_afectados", "n_evaluados")]
#>      columna severidad n_afectados n_evaluados
#> 3 marca_auto     error        1400        2000
```

Falta el 70 % de `marca_auto`, con severidad `error`, en una columna que
está completa en todas las filas donde corresponde que tenga valor.

## Seis formas de tabla donde el vacío es la estructura

La encuesta no es un caso raro. En estas formas de tabla, los vacíos son
parte del diseño:

| forma | por qué hay vacíos |
|----|----|
| historial con vigencia abierta | `valido_hasta` vacío significa “sigue vigente” |
| encuesta con salto de patrón | quien no tiene auto no responde nada del auto |
| columnas excluyentes por subtipo | una persona jurídica no tiene fecha de nacimiento |
| documento por tipo | quien tiene cédula no tiene pasaporte |
| entidad-atributo-valor | cada fila llena una sola de las columnas de valor |
| formato largo | la misma tabla de arriba, apilada |

## La señal que dice que hay algo para declarar

El arreglo, que se ve más abajo, exige saber que existe `aplicabilidad`,
el argumento que declara en qué filas corresponde una columna. Quien no
lo sabe recibe el informe engañoso y nada más. Por eso el mismo perfil
trae otra fila:

``` r

estructural <- h[h$tipo_hallazgo == "posible_ausencia_estructural", ]
estructural[, c("columna", "severidad", "n_afectados")]
#>      columna severidad n_afectados
#> 5 marca_auto        ok        1400
writeLines(strwrap(estructural$evidencia, 88))
#> `tiene_auto` predice la presencia de `marca_auto` en 100.0 % de 2000 filas, con 2
#> valores distintos. La regla explica 1400 de las 1400 ausencias de la columna. La
#> columna corresponde cuando tiene_auto es "Si".
writeLines(strwrap(estructural$sugerencia, 88))
#> Si es así, declararlo y volver a perfilar: `perfilar(datos, aplicabilidad =
#> list(marca_auto = ~ tiene_auto == "Si"))`. Con la regla declarada, la ausencia fuera de
#> ese universo deja de contarse como defecto y el alcance queda escrito en
#> `cobertura_diagnosticos`.
```

Severidad `ok`: no es un defecto, es una lectura de la estructura. La
evidencia es un hecho medido sobre los datos: qué proporción de las
filas queda explicada por el valor de la otra columna, y cuántas de las
ausencias caen donde la regla dice que la columna no corresponde, que es
lo que cuenta `n_afectados`. La sugerencia trae la línea exacta que
habría que escribir. La decisión sigue siendo nuestra: el paquete no
cambia por su cuenta el *universo* de la columna (las filas donde
corresponde que tenga valor; ver el
[glosario](https://sebollin.github.io/lupa/articles/leer-un-perfil.html#glosario)
de *Leer un perfil*), y las columnas ya declaradas quedan fuera del
examen.

La señal sale por dos caminos. Uno es el de la encuesta: **una columna
determina la presencia de otra**, con un cumplimiento de al menos 0,99,
y entonces se puede escribir la fórmula. El otro es el reparto: **dos o
más columnas se turnan las filas sin pisarse** y entre todas cubren la
tabla, que es la forma de los atributos excluyentes por subtipo. Ahí no
hay fórmula que ofrecer, porque falta la columna que diga cuál
corresponde, y la sugerencia es la honesta: si existe esa columna,
`aplicabilidad`; si no existe y la ausencia es por diseño,
`columnas_opcionales`.

Y no dispara por cualquier cosa. Si una parte de las filas rompe el
patrón, se calla, porque entonces la relación existe y no es una regla,
y ofrecer una fórmula sería afirmar de más. El sorteo de abajo pone
marca de auto a algunas personas que dicen no tener:

``` r

set.seed(4)
ruidosa <- encuesta
fuera <- sample(which(ruidosa$tiene_auto == "No"), 200)
ruidosa$marca_auto[fuera] <- "Ford"
sum(hallazgos(perfilar(ruidosa, analizar_dependencias = FALSE))$tipo_hallazgo ==
      "posible_ausencia_estructural")
#> [1] 0
```

Con el 10 % de las filas fuera del patrón, la señal no aparece.

## Declarar el universo

`aplicabilidad` toma una fórmula por columna. Las filas donde el
predicado no se cumple salen del universo de esa columna:

``` r

declarado <- perfilar(
  encuesta,
  analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)

sum(hallazgos(declarado)$tipo_hallazgo == "faltantes")
#> [1] 0
columnas(declarado)[
  columnas(declarado)$columna == "marca_auto",
  c("n", "n_aplicables", "n_no_aplica", "n_faltantes", "prop_faltantes")
]
#>      n n_aplicables n_no_aplica n_faltantes prop_faltantes
#> 2 2000          600        1400           0              0
```

El recorte no es silencioso. La regla queda escrita donde se busca lo
que no se midió:

``` r

cobertura_declarada <- cobertura(declarado)
writeLines(strwrap(
  cobertura_declarada$motivo[cobertura_declarada$columna == "marca_auto"], 88
))
#> La completitud se midió sobre 600 filas aplicables de 2000; la regla declarada es
#> `~tiene_auto == "Si"`.
```

Cuando la ausencia nunca es defecto y no hay una regla que escribir (el
historial con vigencia abierta es el caso típico), alcanza con
`columnas_opcionales`:

``` r

historial <- data.frame(
  id = 1:100,
  valido_hasta = c(rep(NA, 40), 41:100)
)
opcional <- perfilar(
  historial, analizar_dependencias = FALSE,
  columnas_opcionales = "valido_hasta"
)
sum(hallazgos(opcional)$tipo_hallazgo == "faltantes")
#> [1] 0
```

Con el 40 % de la columna vacío, el perfil suma además un aviso de
severidad `ok`, `regla_silencia_ausencia`, que se explica al final de la
guía.

## El error simétrico

Declarar el universo hace visible un diagnóstico que sin la declaración
no puede existir: el valor que aparece donde la regla dice que no
corresponde.

``` r

inconsistente <- data.frame(
  tiene_auto = c("Si", "No", "No", "Si"),
  marca_auto = c("Ford", NA, "Fiat", "VW"),
  stringsAsFactors = FALSE
)
p <- perfilar(
  inconsistente, analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)
fuera_de_universo <- hallazgos(p)[
  hallazgos(p)$tipo_hallazgo == "valor_fuera_de_aplicabilidad",
]
fuera_de_universo[, c("columna", "severidad", "n_afectados")]
#>      columna  severidad n_afectados
#> 2 marca_auto sospechoso           1
writeLines(strwrap(fuera_de_universo$evidencia, 88))
#> 1 valores presentes fuera del universo aplicable de 2 filas.
fuera_de_universo$trazabilidad[[1]]$indices_fila
#> [1] 3
```

La tercera fila declara no tener auto y tiene marca. Es un error en los
datos, o en la regla, y sin el universo declarado `lupa` no puede verlo.

## Cuando la regla no se puede determinar

Si la columna que discrimina también está ausente, no se sabe si la
columna aplica. Esas filas no se cuentan como aplicables ni como no
aplicables: se declaran aparte, porque no saber no es lo mismo que no
corresponder.

``` r

dudoso <- data.frame(
  tiene_auto = c("Si", "No", NA, "Si"),
  marca_auto = c("Ford", NA, NA, "VW"),
  stringsAsFactors = FALSE
)
pd <- perfilar(
  dudoso, analizar_dependencias = FALSE,
  aplicabilidad = list(marca_auto = ~ tiene_auto == "Si")
)
columnas(pd)[
  columnas(pd)$columna == "marca_auto",
  c("n_aplicables", "n_no_aplica", "n_aplicabilidad_indeterminada")
]
#>   n_aplicables n_no_aplica n_aplicabilidad_indeterminada
#> 2            2           1                             1
```

## El formato largo: `perfilar_por()`

Una tabla en formato largo no es una tabla: son muchas apiladas.
Perfilarla entera mezcla dominios que no tienen nada que ver, y el
resultado no describe a ninguno. La tabla de abajo apila tres atributos
de cada entidad; sus valores se sortean:

``` r

set.seed(9)
n <- 900
atributo <- rep(c("pais", "edad", "correo"), each = n / 3)
largo <- data.frame(
  entidad = rep(seq_len(n / 3), 3),
  atributo = atributo,
  valor_texto = ifelse(
    atributo == "edad", NA,
    ifelse(atributo == "pais", sample(c("UY", "uy", "AR"), n, TRUE),
           paste0("a", seq_len(n), "@x.uy"))
  ),
  valor_num = ifelse(atributo == "edad", sample(18:80, n, TRUE), NA),
  stringsAsFactors = FALSE
)

plano <- perfilar(largo, analizar_dependencias = FALSE)
table(as.character(hallazgos(plano)$tipo_hallazgo))
#> 
#>            alta_cardinalidad  casi_duplicados_vocabulario        dato_personal_posible 
#>                            1                            1                            1 
#>                    faltantes    mayusculas_inconsistentes posible_ausencia_estructural 
#>                            2                            1                            2
```

Por grupo, cada atributo se perfila contra su propio dominio. Las
columnas enteramente ausentes dentro del grupo se descartan antes de
perfilar, y ése es el paso que desactiva el faltante falso.

``` r

por_grupo <- perfilar_por(
  largo, "atributo", clave = "entidad", min_filas = 30,
  analizar_dependencias = FALSE
)
sum(por_grupo$tipo_hallazgo == "faltantes")
#> [1] 0
table(as.character(por_grupo$tipo_hallazgo))
#> 
#> casi_duplicados_vocabulario       dato_personal_posible   mayusculas_inconsistentes 
#>                           2                           1                           1 
#>       posible_identificador 
#>                           3
```

El argumento `clave` no es cosmético. Sin la columna de identidad, cada
repetición del valor de un atributo se lee como fila duplicada y el
diagnóstico de unicidad informa la tabla entera. Si se declara,
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
separa la unicidad de la ausencia de nulos: `meta$clave` conserva ambos
estados cuando alguno no queda verificado, y la trazabilidad deja
explícita su semántica de R cuando agrupa ausentes.

Lo que se descartó queda declarado:

``` r

head(attr(por_grupo, "cobertura_grupos"), 3)
#>    grupo grupo_perfilado n_filas_grupo
#> 1   pais            TRUE           300
#> 2   edad            TRUE           300
#> 3 correo            TRUE           300
#>                                                                            motivo
#> 1 Se descartaron 1 columnas enteramente ausentes en este grupo antes de perfilar.
#> 2 Se descartaron 1 columnas enteramente ausentes en este grupo antes de perfilar.
#> 3 Se descartaron 1 columnas enteramente ausentes en este grupo antes de perfilar.
#>   columnas_descartadas
#> 1            valor_num
#> 2          valor_texto
#> 3            valor_num
```

## Lo que `lupa` no hace

No infiere el modelo. No mira una tabla y decide por su cuenta que es un
entidad-atributo-valor, ni que dos columnas son mutuamente excluyentes.
Una heurística de ese tipo fallaría sobre tablas anchas legítimas, y por
la misma razón el paquete tampoco adivina claves ni jerarquías.

El universo lo declara quien conoce el dato. Sin declaración, toda la
columna aplica y el resultado es el del principio de esta guía: es la
única omisión que no supone nada.

Lo que sí hace es medir la evidencia y ofrecerla. La diferencia importa:
sugerir con la evidencia a la vista deja la decisión donde estaba, y
adivinar la mueve.

## Y el reverso: cuando la declaración tapa un problema

Una regla declarada puede silenciar algo real. Declarar opcional una
columna casi vacía deja el perfil limpio, y quien no lea la cobertura de
diagnósticos no se entera de nada:

``` r

casi_vacia <- data.frame(
  x = c(rep(NA_character_, 160), rep(letters[1:20], 2)),
  stringsAsFactors = FALSE
)
p <- perfilar(casi_vacia, columnas_opcionales = "x",
              analizar_dependencias = FALSE)
silencio <- hallazgos(p)[hallazgos(p)$tipo_hallazgo == "regla_silencia_ausencia", ]
silencio[, c("columna", "severidad", "n_afectados", "n_evaluados")]
#>   columna severidad n_afectados n_evaluados
#> 2       x        ok         160         200
writeLines(strwrap(silencio$evidencia, 88))
#> Faltan 160 de 200 celdas de la columna (80.0 %), y la columna se declaró con
#> `columnas_opcionales`: la ausencia no es defecto en esta columna.
```

Con el 80 % de la columna vacío, el aviso es `regla_silencia_ausencia`,
el mismo que apareció en el historial. También es `ok`, y por la misma
razón: la declaración hizo su trabajo. El aviso existe para que eso sea
una decisión y no un efecto lateral. Vale igual cuando el universo se
declaró con `aplicabilidad` y la columna sigue casi vacía *dentro* de
ese universo, que es el caso donde la regla resultó más ancha que el
universo real.

## Todo lo que se le declara al paquete

El universo de una columna es una de varias cosas que `lupa` no puede
saber mirando los datos. Todas siguen la misma idea: quien conoce el
dato lo declara, el paquete lo respeta y deja constancia. Son argumentos
de [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
(y, por `argumentos_perfil` o `...`, de
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) y
de las demás vías de perfilado), salvo la unidad, que viaja con la
columna:

| qué declaramos | argumento | qué cambia en el perfil |
|----|----|----|
| en qué filas corresponde una columna | `aplicabilidad` | las filas fuera del universo no cuentan como faltantes; aparece `valor_fuera_de_aplicabilidad`; la regla queda en [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md) |
| que la ausencia nunca es defecto | `columnas_opcionales` | los faltantes no se informan; si la columna sigue muy vacía, `regla_silencia_ausencia` |
| que un número es un código, no una magnitud | `columnas_codigo` | sin media, mediana, desvío, límites de Tukey ni Benford; se conservan mínimo, máximo, moda y conteos; en [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md), escala nominal |
| qué números significan “sin dato” | `sentinelas_numericos` | se cuentan como faltantes disfrazados y salen de media, mediana, mínimo, máximo, desvío y de los cuantiles de [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) |
| qué textos significan “sin dato” | `cadenas_ausencia` | se suman a los que el paquete reconoce y se cuentan como faltantes disfrazados |
| dónde un cero o un negativo no es admisible | `columnas_sin_ceros`, `columnas_no_negativas` | aparecen `ceros_no_permitidos` y `negativos_no_permitidos` |
| qué columnas identifican una fila | `clave` | la trazabilidad trae el valor de la clave de cada fila señalada; una clave repetida o con ausentes es un `error` (`clave_no_unica`, `clave_con_ausentes`) |
| qué columnas traen datos personales | `columnas_personales` | se clasifican como datos personales aunque ni el nombre ni la forma los delaten, y con `proteger_datos_personales = TRUE` sus valores salen enmascarados |
| en qué unidad está una magnitud | atributo `units` de la columna | [`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md) publica la unidad junto a las cifras, y [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md) la usa para proponer la escala |

Dos de ellas cambian lo que el perfil calcula, y conviene verlas. Un
código de rubro con un valor fuera de catálogo, sin declarar y
declarado:

``` r

rubros <- data.frame(rubro = rep(1:19, length.out = 60))
rubros$rubro[c(5, 9)] <- 99

campos <- c("columna", "minimo", "maximo", "media", "n_outliers",
            "estado_resumen_cuantitativo")
rbind(
  columnas(perfilar(rubros, analizar_dependencias = FALSE))[, campos],
  columnas(perfilar(rubros, analizar_dependencias = FALSE,
                    columnas_codigo = "rubro"))[, campos]
)
#>   columna minimo maximo    media n_outliers estado_resumen_cuantitativo
#> 1   rubro      1     99 12.66667          2                  calculados
#> 2   rubro      1     99       NA         NA             omitidos_codigo
```

Sin declarar, el código se mide como magnitud: tiene media y el `99`
sale como valor atípico. Declarado, la media no se calcula
(`omitidos_codigo`) y el `99` sigue a la vista en el máximo, que es
donde aparece un código fuera de catálogo.

Una edad donde `999` significa “no informada”. `999` está en la lista
que el paquete sospecha por omisión, pero una sospecha sólo se informa:
no mueve los números. Declarado, sale del resumen:

``` r

edades <- data.frame(edad = c(34, 51, 999, 27, 999, 45, 62, 999, 38, 29))

campos <- c("columna", "n_faltantes_disfrazados", "media", "maximo",
            "n_valores_excluidos_resumen")
rbind(
  columnas(perfilar(edades, analizar_dependencias = FALSE))[, campos],
  columnas(perfilar(edades, analizar_dependencias = FALSE,
                    sentinelas_numericos = 999))[, campos]
)
#>   columna n_faltantes_disfrazados     media maximo n_valores_excluidos_resumen
#> 1    edad                       3 328.30000    999                           0
#> 2    edad                       3  40.85714     62                           3
```

`sentinelas_numericos` reemplaza la lista entera: declaramos exactamente
los valores que valen como “sin dato” en nuestros datos. El paquete trae
además `sentinelas_naniar`, una lista más amplia que se pide de manera
explícita, porque valores como `66`, `77` y `88` pueden ser datos
legítimos:

``` r

codigos <- data.frame(codigo = c(1, 66, 9999))
c(
  por_omision = columnas(perfilar(codigos, analizar_dependencias = FALSE))$
    n_faltantes_disfrazados,
  naniar = columnas(perfilar(codigos, analizar_dependencias = FALSE,
                             sentinelas_numericos = sentinelas_naniar))$
    n_faltantes_disfrazados
)
#> por_omision      naniar 
#>           0           2
```

Los detalles de cada declaración (qué se excluye de cada diagnóstico,
cómo se reconoce una numeración sin declararla, qué se enmascara) están
en el artículo [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

## Resumen

- Sin declarar nada, `lupa` cuenta como faltante toda celda vacía,
  aunque esté vacía por diseño.
- `posible_ausencia_estructural` (severidad `ok`) avisa cuándo pasa y
  trae la línea para declararlo.
- `aplicabilidad` recorta el universo de una columna;
  `columnas_opcionales` exime la columna entera, y
  `regla_silencia_ausencia` avisa si la declaración tapó algo.
- Declarar el universo hace visible `valor_fuera_de_aplicabilidad`, el
  error del otro lado.
- Códigos, centinelas, claves, datos personales y unidades se declaran
  igual: el paquete no los adivina.

## Paso siguiente

[6. Limpiar con un
plan](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md)
([`vignette("limpiar-con-un-plan")`](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md)):
cómo corregir una copia de los datos sin perder el registro de lo que se
hizo.

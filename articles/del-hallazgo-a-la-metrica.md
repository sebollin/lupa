# 7. Del hallazgo a la métrica

El perfil de `datos_operativos` dice que a la columna `canal` le faltan
valores. Los que faltan están disfrazados: uno es `"S/D"` y otro, un
texto vacío. ¿Cómo se convierte ese hallazgo en una medición que se
repita con cada entrega y se compare con un requisito?

> **En esta guía:**
>
> - pasar de los hallazgos a una propuesta de métricas con
>   [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md);
> - decidir qué se mide y medirlo con
>   [`medir()`](https://sebollin.github.io/lupa/reference/medir.md);
> - agregar las medidas de cada celda hasta la columna con
>   [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md);
> - evaluar la medida agregada contra un umbral con
>   [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md);
> - ver qué parte de un marco de calidad quedó cubierta con
>   [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).

## El hallazgo de partida

Se perfila la tabla y se mira lo que el perfil encontró en `canal`. La
fecha se fija para que el ejemplo dé siempre lo mismo.

``` r

library(lupa)

perfil <- perfilar(datos_operativos, fecha = "2026-01-15")
encontrados <- hallazgos(perfil)
encontrados[encontrados$columna %in% "canal", c("tipo_hallazgo", "severidad")]
#>                  tipo_hallazgo  severidad
#> 8                    faltantes sospechoso
#> 9        faltantes_disfrazados      error
#> 10          espacios_sobrantes sospechoso
#> 11   mayusculas_inconsistentes sospechoso
#> 19 casi_duplicados_vocabulario sospechoso
```

El de severidad `error` es `faltantes_disfrazados`. Su evidencia dice
cuáles son:

``` r

disfrazados <- encontrados$columna %in% "canal" &
  encontrados$tipo_hallazgo == "faltantes_disfrazados"
writeLines(encontrados$evidencia[disfrazados])
#> <blanco> (1); S/D (1)
```

Un hallazgo describe esta entrega. Para saber si la próxima está mejor o
peor, y si alcanza el nivel pedido, hace falta una métrica.

## De los hallazgos a una propuesta de métricas

Una métrica es una forma de medir con nombre propio. Se liga a una tabla
y a una o más columnas, y devuelve un resultado por cada objeto que
mide. `NoNulo`, la métrica de completitud, devuelve un 1 por cada celda
con valor y un 0 por cada celda vacía. Qué cuenta como vacío es parte de
su configuración: además de `NA`, los valores que se le declaren en
`valores_nulos`.

`lupa` no decide qué medir: lo propone.
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
recorre los hallazgos y devuelve una tabla editable, con una fila por
métrica sugerida y el hallazgo que la originó. No mide nada.

``` r

propuesta <- proponer_modelo(perfil, datos_operativos)
propuesta[, c("metrica", "atributos", "origen", "incluir")]
#>                        metrica      atributos                             origen incluir
#> 1             EntidadDuplicada                         hallazgo:filas_duplicadas    TRUE
#> 2                       NoNulo          canal                 hallazgo:faltantes    TRUE
#> 3                       NoNulo codigo_usuario     hallazgo:faltantes_disfrazados    TRUE
#> 4                       NoNulo   fecha_evento     hallazgo:faltantes_disfrazados    TRUE
#> 5                       NoNulo          monto     hallazgo:faltantes_disfrazados    TRUE
#> 6                      Formato       contacto perfil:patron_dominante:a+9+@a+.a+   FALSE
#> 7                      Formato      id_evento       perfil:patron_dominante:A+9+   FALSE
#> 8                      Formato        sistema         perfil:patron_dominante:a+   FALSE
#> 9                      Formato           zona        perfil:patron_dominante:Aa+   FALSE
#> 10 ValoresPosiblesPorExtension           zona           perfil:dominio_observado   FALSE
```

La columna `incluir` dice qué filas quedan activas. `lupa` las marca
así:

- activa las que nacen de un defecto observado: los faltantes de 4
  columnas y las filas duplicadas;
- deja apagadas las que nacen de una costumbre de esta entrega, como un
  patrón de formato o el dominio de `zona`.

Una sola entrega no demuestra que esas costumbres sean requisitos. Y
medirlas sobre la misma entrega de la que salieron diría poco.

La fila de `canal` muestra el paso del hallazgo a la métrica. Los
valores que el perfil reconoció como faltantes disfrazados pasan a la
configuración de `NoNulo`:

``` r

fila_canal <- which(propuesta$id_sugerencia == "NoNulo:canal")
writeLines(strwrap(propuesta$justificacion[fila_canal], 88))
#> La columna contiene 2 ausentes totales, incluidos 2 disfrazados. Se normalizan: sus
#> valores se declaran en `valores_nulos` y la métrica los cuenta como faltantes.
propuesta$configuracion[[fila_canal]]
#> $valores_nulos
#> [1] ""    "S/D"
```

## Decidir qué se mide

La propuesta se edita: la decisión es de quien la usa. Aquí interesa la
completitud: se dejan activas sólo las 4 filas de `NoNulo` y se apaga la
de duplicados.
[`modelo_desde_propuesta()`](https://sebollin.github.io/lupa/reference/modelo_desde_propuesta.md)
convierte las filas activas en un modelo operativo. Es la lista de
métricas, ya ligadas a sus columnas, que
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) va a
ejecutar. Tampoco mide.

``` r

propuesta$incluir <- propuesta$metrica == "NoNulo"
propuesta[propuesta$incluir, c("metrica", "atributos")]
#>   metrica      atributos
#> 2  NoNulo          canal
#> 3  NoNulo codigo_usuario
#> 4  NoNulo   fecha_evento
#> 5  NoNulo          monto
modelo_completitud <- modelo_desde_propuesta(propuesta)
```

## Medir

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) recibe
primero el modelo y después los datos. El identificador y la fecha de la
corrida quedan en cada medida; con ellos se comparan después las
entregas.

``` r

medicion <- medir(
  modelo_completitud, datos_operativos,
  id_medicion = "entrega-enero",
  fecha = "2026-01-15"
)
nrow(medicion)
#> [1] 52
medicion[medicion$atributo == "canal", c("objeto_medible", "resultado")]
#>                objeto_medible resultado
#> 1   datos_operativos$canal[1]         1
#> 2   datos_operativos$canal[2]         1
#> 3   datos_operativos$canal[3]         1
#> 4   datos_operativos$canal[4]         1
#> 5   datos_operativos$canal[5]         0
#> 6   datos_operativos$canal[6]         1
#> 7   datos_operativos$canal[7]         1
#> 8   datos_operativos$canal[8]         1
#> 9   datos_operativos$canal[9]         1
#> 10 datos_operativos$canal[10]         1
#> 11 datos_operativos$canal[11]         1
#> 12 datos_operativos$canal[12]         1
#> 13 datos_operativos$canal[13]         0
```

Cada fila es una medida: un resultado para un objeto, aquí una celda.
Las 4 columnas por 13 filas dan 52 medidas. En `canal`, las filas 5 y 13
valen 0. Son el `"S/D"` y el texto vacío, que `NoNulo` contó como
faltantes porque la configuración se lo pidió.

## Agregar hasta la columna

Una medida por celda no responde todavía la pregunta de fondo: cuán
completa está cada columna. Para eso se agrega. El vocabulario de los
niveles es el del marco de calidad, y
[`granularidades()`](https://sebollin.github.io/lupa/reference/granularidades.md)
lo traduce al de las tablas:

``` r

granularidades()[c(1, 2, 4, 5), c("granularidad", "relacional")]
#>        granularidad relacional
#> 1 instanciaAtributo      celda
#> 2          atributo    columna
#> 4  instanciaEntidad      tupla
#> 5           entidad      tabla
```

En una tabla:

- una instancia de atributo es una celda;
- un atributo, una columna;
- una instancia de entidad, una fila (una tupla);
- una entidad, una tabla.

Estos términos están también en [el
glosario](https://sebollin.github.io/lupa/articles/leer-un-perfil.html#glosario).
De la celda a la columna se pasa con `ratio`, que calcula la proporción
de unos:

``` r

completitud <- agregar(medicion, "atributo", "ratio")
completitud[, c("atributo", "resultado", "agregacion")]
#>         atributo resultado agregacion
#> 1          canal 0.8461538      ratio
#> 2 codigo_usuario 0.9230769      ratio
#> 3   fecha_evento 0.9230769      ratio
#> 4          monto 0.9230769      ratio
```

[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md)
combina medidas de una sola métrica y de una sola corrida. Sólo sigue
los pasos que declara
[`transiciones_granularidad()`](https://sebollin.github.io/lupa/reference/granularidades.md):

- de la celda a la columna o a la fila;
- de la columna o de la fila a la tabla;
- de la tabla hacia arriba.

Una celda puede agregarse hacia su columna o hacia su fila: son 2
direcciones distintas, no peldaños de una misma escala. Las agregaciones
disponibles son:

- `ratio`, para resultados booleanos;
- `ratio_umbral`, para resultados reales comparados con un umbral;
- `promedio`;
- `promedio_ponderado`, con pesos en `[0, 1]` que suman 1 por destino.

Los resultados no acotados conservan su unidad y no entran en estas
agregaciones. Son, por ejemplo, la desviación de `ErrorEstandar` o una
desactualización medida en días.

## Evaluar contra un umbral

Una regla expresa una condición sobre resultados. Un perfil de
evaluación reúne reglas. Ese perfil no es el objeto que devuelve
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md):
es el segundo argumento de
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md). Se
pide que cada columna esté completa al menos en un 90 %:

``` r

regla <- regla_evaluacion(
  "Completitud de al menos 90 %",
  function(x) x >= 0.9
)
publicable <- perfil_evaluacion("Publicable", regla)
evaluacion <- evaluar(completitud, publicable)
evaluacion$perfiles[, c("perfil", "n_reglas", "resultado")]
#>       perfil n_reglas resultado
#> 1 Publicable        1      0.75
```

La regla se aplicó a cada una de las 4 medidas agregadas. El resultado
de una regla es la proporción de medidas que la cumplen. El de un perfil
es la media simple de sus reglas. Estas son las que cumplieron:

``` r

cumple <- evaluacion$medidas$resultado[
  match(completitud$id_medida, evaluacion$medidas$id_medida)
]
data.frame(
  columna = completitud$atributo,
  completitud = round(completitud$resultado, 3),
  cumple = cumple
)
#>          columna completitud cumple
#> 1          canal       0.846  FALSE
#> 2 codigo_usuario       0.923   TRUE
#> 3   fecha_evento       0.923   TRUE
#> 4          monto       0.923   TRUE
```

Todas las columnas alcanzan el 90 % salvo `canal`, que tiene 0.846: el
resultado es 3 de 4. La regla compara de verdad la completitud de cada
columna con su umbral. Otro umbral da otro resultado:

``` r

resultado_con_umbral <- function(umbral, medidas) {
  regla <- regla_evaluacion("Completitud", function(x) x >= umbral)
  evaluar(medidas, perfil_evaluacion("Publicable", regla))$perfiles$resultado
}
umbrales <- c(0.80, 0.90, 0.95)
data.frame(
  umbral = umbrales,
  agregada = vapply(umbrales, resultado_con_umbral, numeric(1), completitud),
  por_celda = vapply(umbrales, resultado_con_umbral, numeric(1), medicion)
)
#>   umbral agregada por_celda
#> 1   0.80     1.00 0.9038462
#> 2   0.90     0.75 0.9038462
#> 3   0.95     0.00 0.9038462
```

Con la medida agregada, el resultado sigue al umbral:

- con 80 %, cumplen las 4 columnas;
- con 90 %, cumplen 3;
- con 95 %, ninguna.

La columna `por_celda` muestra qué pasa si se evalúa la medición sin
agregar. La regla le pregunta a cada celda si su 0 o su 1 alcanza el
umbral. El resultado es la proporción de celdas con valor, 0.904, sea
cual sea el umbral. Por eso se agrega antes de evaluar.

La evaluación aplica una condición a medidas que ya existen. No vuelve a
leer `datos_operativos` ni reemplaza la medición.

## 🔎 Qué quedó cubierto

Que no aparezcan hallazgos no prueba que una tabla esté bien: puede que
nadie haya medido lo que importa.
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
cruza el perfil y la medición con un marco de calidad. Dice, factor por
factor, qué quedó medido. El tercer argumento se llama `modelo`, pero
recibe un marco como el que crea
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
Por omisión, el de AGESIC.

``` r

cobertura <- cobertura_analisis(perfil, medicion, modelo = marco_agesic())
cobertura[, c("dimension", "factor", "estado")]
#>       dimension                        factor           estado
#> 1     Exactitud         Correctitud semántica     no_declarada
#> 2     Exactitud        Correctitud sintáctica     no_declarada
#> 3     Exactitud                     Precisión     no_declarada
#> 4     Exactitud Exactitud posicional absoluta        no_aplica
#> 5     Exactitud Exactitud posicional relativa        no_aplica
#> 6     Exactitud                     Fidelidad fuera_de_alcance
#> 7  Consistencia      Integridad inter-entidad     no_declarada
#> 8  Consistencia      Integridad intra-entidad     no_declarada
#> 9  Consistencia         Integridad de dominio     no_declarada
#> 10 Consistencia       Consistencia topológica        no_aplica
#> 11  Completitud                     Cobertura     no_declarada
#> 12  Completitud                      Densidad           medida
#> 13  Completitud                      Comisión        no_aplica
#> 14     Unicidad                No-duplicación           medida
#> 15     Unicidad              No-contradicción     no_declarada
#> 16     Frescura                    Actualidad     no_declarada
#> 17     Frescura                   Oportunidad     no_declarada
```

`estado` distingue estos casos:

- `medida`;
- `no_declarada`, cuando nadie pidió medirlo;
- `no_aplica`, cuando no corresponde a los tipos de la tabla, como la
  exactitud posicional si no hay geometrías;
- `fuera_de_alcance`, cuando `lupa` no lo puede medir y lo dice.

Cada fila explica por qué:

``` r

medidos <- cobertura[cobertura$estado == "medida", ]
writeLines(paste0(medidos$factor, ": ", medidos$motivo))
#> Densidad: La corrida contiene al menos una métrica instanciada para este factor.
#> No-duplicación: El perfil examinó duplicación de valores, columnas y filas exactas.
```

La densidad quedó respaldada por la medición; la no duplicación, por lo
que el perfil examina por su cuenta. Lo que falta trae cómo resolverlo:

``` r

sintaxis <- cobertura$factor == "Correctitud sintáctica"
writeLines(cobertura$como_resolverlo[sintaxis])
#> Especializar Formato con expresión, diccionario o validador.
```

## Resumen

- [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
  convierte los hallazgos en una propuesta editable: los faltantes
  disfrazados que encontró el perfil pasan a la configuración de
  `NoNulo`.
- La columna `incluir` la decide quien usa la propuesta; lo aprendido de
  una sola entrega llega apagado.
- [`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
  devuelve una medida por objeto, con el `id_medicion` y la fecha de la
  corrida.
- Se agrega antes de evaluar: `agregar(medicion, "atributo", "ratio")`
  da la completitud de cada columna, y la regla compara ese número con
  su umbral.
- [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
  dice qué factores del marco quedaron medidos y cómo cubrir los que
  faltan.

## 🧭 Paso siguiente

En [8. Marcos y modelos de
calidad](https://sebollin.github.io/lupa/articles/marcos-y-modelos.md)
se declara un marco propio y se construyen las métricas a medida. Entre
ellas está la que pide la fila de correctitud sintáctica.

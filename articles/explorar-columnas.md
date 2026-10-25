# 3. Explorar columnas con \`analizar()\`

El perfil dice qué está mal en una tabla. Antes de decidir qué le
exigimos, también queremos saber qué hay en cada columna: qué forma
tienen los valores, qué tipo esconden, cómo se reparten, qué columnas se
mueven juntas y qué período cubren las fechas. ¿Cómo lo vemos todo de
una vez, y cómo lo compartimos?

> **En esta guía:**
>
> - qué arma
>   [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
>   en una sola llamada y dónde queda cada pieza;
> - cómo se leen patrones, tipos, fechas, escalas, distribuciones,
>   asociaciones y tiempo;
> - qué mide
>   [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
>   por su cuenta y cómo se apaga;
> - cuándo conviene
>   [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
>   y cuándo
>   [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md);
> - cómo guardamos el análisis y producimos un informe HTML.

Seguimos con `datos_operativos`, la entrega de 13 registros con defectos
sembrados que presenta [1. Empezar en diez
minutos](https://sebollin.github.io/lupa/articles/lupa.md).

## Una llamada, todos los componentes

Fijamos `fecha` para que el ejemplo dé siempre el mismo resultado.

``` r

library(lupa)

analisis <- analizar(
  datos_operativos,
  nombre = "entrega de ejemplo",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
names(analisis)
#>  [1] "perfil"            "distribuciones"    "asociaciones"      "temporal"         
#>  [5] "variables"         "propuesta_modelo"  "decision_medicion" "tablero"          
#>  [9] "medicion"          "plan_limpieza"     "cobertura"         "advertencias"     
#> [13] "meta"
```

Cada componente es una tabla o una lista de tablas, y casi todos se
pueden obtener sueltos con su propia función:

| componente | qué contiene | función suelta |
|----|----|----|
| `perfil` | hallazgos, resumen por columna, patrones, formatos de fecha, cobertura de diagnósticos | [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) |
| `variables` | tipo implícito, escala y rol propuestos para cada columna | [`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md) |
| `distribuciones` | frecuencias, cuantiles y su alcance | [`distribucion_valores()`](https://sebollin.github.io/lupa/reference/distribucion_valores.md) |
| `asociaciones` | una medida por cada par de columnas que se mueven juntas | [`detectar_asociaciones()`](https://sebollin.github.io/lupa/reference/detectar_asociaciones.md) |
| `temporal` | período observado, huecos y frecuencia propuesta | [`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md) |
| `propuesta_modelo`, `decision_medicion` | las métricas sugeridas y cuáles se midieron | [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md) |
| `tablero`, `medicion` | la medición agregada | [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md), [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md) |
| `cobertura` | qué factores del marco de calidad quedaron medidos | [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md) |
| `plan_limpieza` | acciones posibles, ninguna aplicada | [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md) |
| `advertencias` | lo que el análisis propuso y queda por confirmar | — |
| `meta` | nombre, fecha y qué decisiones tomó la corrida | — |

Los accesores del perfil también leen el análisis: `hallazgos(analisis)`
es lo mismo que `hallazgos(analisis$perfil)`. Miramos los de una columna
que vamos a seguir en toda la guía, `fecha_evento`:

``` r

h <- hallazgos(analisis)
h[h$columna %in% "fecha_evento", c("tipo_hallazgo", "severidad", "n_afectados")]
#>                  tipo_hallazgo  severidad n_afectados
#> 4            alta_cardinalidad sospechoso           1
#> 5        faltantes_disfrazados      error           1
#> 6        formatos_fecha_mixtos      error           5
#> 7      tipo_declarado_distinto sospechoso           1
#> 18 casi_duplicados_vocabulario         ok           0
```

Cómo se lee cada hallazgo está en [2. Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md). Acá
miramos lo que hay debajo: los valores que los producen.

## Patrones: la forma de los valores

Un patrón resume la forma de un valor: `A` es una letra mayúscula, `a`
una minúscula, `9` un dígito, y `+` indica que se repite.
[`descubrir_patrones()`](https://sebollin.github.io/lupa/reference/descubrir_patrones.md)
recibe un vector, no una tabla, y cuenta cuántos valores tienen cada
forma:

``` r

descubrir_patrones(datos_operativos$codigo_usuario)
#>   patron  n proporcion                    ejemplos
#> 1  A+-9+ 10 0.76923077 USR-001 | USR-002 | USR-004
#> 2   A+-9  1 0.07692308                       MAL-5
#> 3   A+9+  1 0.07692308                     USR0003
#> 4    A/A  1 0.07692308                         S/D
```

La mayoría de los códigos tiene la forma `USR-001`. Se apartan `MAL-5`,
`USR0003` y `S/D`: el último ya figura en el perfil como faltante
disfrazado, y los otros dos son los que habría que revisar en el sistema
de origen. El análisis guarda la misma tabla para cada columna en
`analisis$perfil$patrones`.

## Tipos y fechas

`fecha_evento` se guarda como texto. Eso no impide proponer un tipo
implícito, ni contar los formatos de fecha por separado:

``` r

inferir_tipo(datos_operativos$fecha_evento)
#> fecha (84.6%; 11 de 13 valores compatibles)
formatos <- detectar_formatos_fecha(datos_operativos$fecha_evento)
formatos[, c("formato", "n", "proporcion", "estado", "n_ambiguos")]
#>    formato n proporcion     estado n_ambiguos
#> 1 %Y-%m-%d 4 0.30769231 confirmado          0
#> 2 %d/%m/%Y 4 0.30769231 confirmado          0
#> 3   %Y%m%d 1 0.07692308 confirmado          0
#> 4 %Y/%m/%d 1 0.07692308 confirmado          0
#> 5 %d-%m-%Y 1 0.07692308 confirmado          0
```

De los 13 valores, 11 se leen como fecha, en 5 formatos distintos; los
que no se leen son `2024.04.17` y `NULL`. Estas salidas describen
compatibilidad y frecuencia: no convierten la columna ni afirman qué
formato debe aceptar el sistema.

Que todos los valores sean compatibles tampoco alcanza. Una columna
hecha sólo de fechas como `01/02/2020` puede leerse al cien por ciento y
seguir siendo ambigua: sin una convención externa no se sabe si el
primer número es el día o el mes. El perfil conserva esa diferencia en
`estado_tipo_inferido`:

``` r

ambigua <- perfilar(
  data.frame(fecha = c("01/02/2020", "02/03/2020", "03/04/2020")),
  muestra = Inf,
  analizar_dependencias = FALSE
)
campos <- c(
  "columna", "tipo_inferido", "proporcion_tipo_inferido", "estado_tipo_inferido"
)
rbind(
  columnas(analisis)[columnas(analisis)$columna == "fecha_evento", campos],
  columnas(ambigua)[, campos]
)
#>        columna tipo_inferido proporcion_tipo_inferido estado_tipo_inferido
#> 3 fecha_evento         fecha                0.8461538           confirmado
#> 1        fecha         fecha                1.0000000            candidato
```

En `fecha_evento` el estado es `confirmado` aunque haya valores que no
se leen, porque valores como `31/01/2024` sólo admiten una lectura. En
la columna construida, la proporción es 1 y el estado es `candidato`: la
lectura todavía no está establecida.

## Escalas y roles propuestos

`variables` separa el tipo de almacenamiento, el tipo implícito y la
escala de medición, y propone un rol para cada columna. Es la salida de
[`clasificar_variables()`](https://sebollin.github.io/lupa/reference/clasificar_variables.md):

``` r

analisis$variables[, c(
  "columna", "tipo_implicito", "escala_propuesta", "rol", "confianza", "confirmada"
)]
#>           columna tipo_implicito escala_propuesta           rol confianza confirmada
#> 1     id_registro          doble         discreta        medida      0.65      FALSE
#> 2  codigo_usuario          texto          nominal     categoria      0.55      FALSE
#> 3    fecha_evento          fecha         temporal         fecha      0.85      FALSE
#> 4           canal          texto          nominal     categoria      0.55      FALSE
#> 5           monto          doble         discreta        medida      0.65      FALSE
#> 6            zona          texto          nominal     categoria      0.55      FALSE
#> 7         sistema          texto          nominal     categoria      0.55      FALSE
#> 8        contacto          texto          nominal     categoria      0.55      FALSE
#> 9        id_copia          doble         discreta        medida      0.65      FALSE
#> 10      id_evento  identificador          nominal identificador      0.90      FALSE
```

`confianza` expone la fuerza de cada propuesta y `confirmada` distingue
una declaración de una inferencia. Sin metadatos, ninguna está
confirmada. `id_registro` sale como una medida discreta: por sus
valores, un identificador numérico no se distingue de una cantidad.
Quien conoce la columna lo declara, con `metadatos_variables` o con
`columnas_codigo` (ver la tabla final de [5. El vacío por
diseño](https://sebollin.github.io/lupa/articles/vacio-por-diseno.md)).

## Distribuciones

`distribuciones` trae las frecuencias de cada columna y los cuantiles de
las numéricas:

``` r

frecuencias <- analisis$distribuciones$frecuencias
frecuencias[frecuencias$columna == "canal", c("valor", "frecuencia", "proporcion")]
#>         valor frecuencia proporcion
#> 35        web          5 0.38461538
#> 36   telefono          2 0.15384615
#> 37                     1 0.07692308
#> 38        APP          1 0.07692308
#> 39        S/D          1 0.07692308
#> 40        Web          1 0.07692308
#> 41 presencial          1 0.07692308
#> 42       web           1 0.07692308

cuantiles <- analisis$distribuciones$cuantiles
cuantiles[cuantiles$columna == "monto", c("probabilidad", "valor", "n_analizados")]
#>    probabilidad  valor n_analizados
#> 6          0.00    -99           13
#> 7          0.25   1250           13
#> 8          0.50   1490           13
#> 9          0.75   1700           13
#> 10         1.00 999999           13
```

Las frecuencias cuentan lo que hay guardado, sin unir variantes: entre
los valores de `canal` aparecen la celda vacía, `S/D`, `Web` y `"web "`,
con un espacio al final.

Los cuantiles de `monto` van de `-99` a `999999`. El perfil señaló `-99`
como posible faltante disfrazado, pero un valor sospechado sólo se
informa: no sale de los números mientras no se lo declare con
`sentinelas_numericos`. `analisis$distribuciones$alcance` dice, columna
por columna, cuántas filas se analizaron y si la lista de valores se
recortó.

## Asociaciones

Cada fila de `asociaciones` es una medida entre dos columnas, con el
método que se usó: Pearson entre numéricas, eta cuadrado entre una
categoría y un número, V de Cramér entre dos categorías. La columna
`supuesto` dice cómo se trató cada escala.

``` r

analisis$asociaciones[, c("columna_1", "columna_2", "metodo", "asociacion")]
#>     columna_1 columna_2           metodo asociacion
#> 1 id_registro  id_copia pearson_absoluto  1.0000000
#> 2       canal     monto             eta2  0.9999977
#> 3       canal      zona         cramer_v  0.8944272
#> 4 id_registro     monto pearson_absoluto  0.4301569
#> 5       monto  id_copia pearson_absoluto  0.4301569
#> 6       canal  id_copia             eta2  0.3791539
#> 7 id_registro     canal             eta2  0.3791539
#> 8 id_registro      zona             eta2  0.3440367
#> 9        zona  id_copia             eta2  0.3440367
```

Son medidas, no dependencias ni instrucciones de modelado, y conviene
leerlas con los datos a la vista. `id_registro` e `id_copia` se asocian
al máximo porque una es copia de la otra, algo que el perfil ya informó
como `columnas_duplicadas`. La asociación de `canal` con `monto` la
produce casi entera la fila 13, donde `canal` está vacío y `monto` vale
`999999`: sin esa fila, la medida baja a 0,65. Para una relación
monótona que no es lineal, `metodo_asociacion_numerica = "spearman"`
cambia Pearson por Spearman.

## Tiempo

[`analizar_tiempo()`](https://sebollin.github.io/lupa/reference/analizar_tiempo.md)
reconoce `fecha_evento` por su contenido, aunque se guarde como texto, y
resume el período observado:

``` r

analisis$temporal$resumen[, c(
  "columna", "n_presentes", "fecha_minima", "fecha_maxima", "n_fechas_excluidas_parseo"
)]
#>        columna n_presentes fecha_minima fecha_maxima n_fechas_excluidas_parseo
#> 1 fecha_evento          11   2024-01-31   2024-10-23                         2
analisis$temporal$propuestas[, c("columna", "frecuencia_dias", "confirmada")]
#>        columna frecuencia_dias confirmada
#> 1 fecha_evento              32      FALSE
```

Las fechas que no se pudieron leer quedan contadas aparte, en
`n_fechas_excluidas_parseo`. La frecuencia propuesta es la moda de los
intervalos observados y sale con `confirmada = FALSE`: es evidencia para
revisar, no un requisito. `advertencias` lo repite junto con las escalas
sin confirmar, para que no pase inadvertido.

## Lo que `analizar()` mide por su cuenta

[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) no
se queda en describir. Toma la propuesta de métricas que arma
[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
a partir del perfil, mide lo que esa propuesta deja activo y agrega el
resultado en un tablero: la tabla que resume la medición con una fila
por métrica y objeto, y cuya columna `orientacion` dice cómo leer cada
valor.

``` r

analisis$propuesta_modelo[, c("metrica", "atributos", "origen", "incluir")]
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

Las filas activas (`incluir = TRUE`) salen de hallazgos: los duplicados
exactos y los faltantes, contando como faltante lo que el perfil vio
disfrazado. Los formatos y el dominio de `zona` se aprendieron de esta
misma entrega, y medirlos contra ella sería circular: el dominio
observado da 1 por construcción, y cada formato da la proporción del
patrón dominante que el perfil ya mostró. Por eso quedan inactivos hasta
que confirmemos que son requisitos. `decision_medicion` dice, fila por
fila, qué se midió, y el tablero muestra el resultado:

``` r

analisis$decision_medicion[, c("metrica", "objeto", "medida")]
#>                        metrica         objeto medida
#> 1             EntidadDuplicada        (tabla)   TRUE
#> 2                       NoNulo          canal   TRUE
#> 3                       NoNulo codigo_usuario   TRUE
#> 4                       NoNulo   fecha_evento   TRUE
#> 5                       NoNulo          monto   TRUE
#> 6                      Formato       contacto  FALSE
#> 7                      Formato      id_evento  FALSE
#> 8                      Formato        sistema  FALSE
#> 9                      Formato           zona  FALSE
#> 10 ValoresPosiblesPorExtension           zona  FALSE
analisis$tablero[, c("metrica_instanciada", "valor", "orientacion")]
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>    metrica_instanciada     valor orientacion
#>       EntidadDuplicada 0.1538462     defecto
#>           NoNulo:canal 0.8461538 conformidad
#>  NoNulo:codigo_usuario 0.9230769 conformidad
#>    NoNulo:fecha_evento 0.9230769 conformidad
#>           NoNulo:monto 0.9230769 conformidad
```

`valor` es una proporción. Con `orientacion = "conformidad"` se lee “más
es mejor”, y con `"defecto"`, al revés: `EntidadDuplicada` es la
proporción de filas que participan en duplicados.
`meta$propuesta_medida_automaticamente` deja constancia de que la
selección la hizo `lupa` y nadie la confirmó.

Para quedarnos sólo con la descripción, `medir_propuesta = FALSE`. Para
medir lo que decidimos nosotros, editamos la propuesta y la pasamos como
`propuesta_confirmada`; ese camino sigue en [7. Del hallazgo a la
métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.html).
`cobertura`, por su parte, dice qué factores del marco de calidad
quedaron medidos (por omisión, el de AGESIC); los marcos se explican en
[8. Marcos y modelos de
calidad](https://sebollin.github.io/lupa/articles/marcos-y-modelos.html).

## ¿`perfilar()` o `analizar()`?

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) es
la pasada que conviene llevar a todos lados: resúmenes por columna,
patrones, tipos, hallazgos, cobertura de diagnósticos y relaciones entre
columnas.
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
arma alrededor de ese perfil lo que cuesta más y devuelve tablas
propias: distribuciones, asociaciones, tiempo, escalas, la propuesta
medida, el plan y el tablero. Cada pieza está exportada, así que se
puede llamar suelta sin pagar el recorrido entero. Por ejemplo, las
frecuencias de una sola columna, con un tope de tres valores:

``` r

zona <- distribucion_valores(datos_operativos["zona"], max_valores = 3)
zona$frecuencias[, c("valor", "frecuencia")]
#>    valor frecuencia
#> 1 Centro          3
#> 2  Norte          3
#> 3  Oeste          3
zona$alcance[, c("columna", "n_distintos_muestra", "n_mostrados", "truncado")]
#>   columna n_distintos_muestra n_mostrados truncado
#> 1    zona                   5           3     TRUE
```

`zona` tiene 5 valores distintos y se muestran 3: el recorte queda
declarado en `truncado`.

Las dos funciones trabajan en memoria, y sobre tablas de millones de
filas no son una pasada rápida. A esa escala conviene perfilar una
muestra y llamar sueltas las piezas que hagan falta, o perfilar dentro
de la base con
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
que calcula los agregados en el motor y deja elegir cuántas filas trae a
R (ver [10. Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.html)).
Los tiempos medidos sobre tablas de ese tamaño, con su fecha y su
entorno, están en el artículo [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).
La muestra que
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
toma por su cuenta es sistemática y sin azar: sobre los mismos datos, el
mismo perfil se rehace igual.

## Compartir el análisis

[`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
escribe el análisis en un archivo RDS con número de esquema, y
[`leer_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
lo recupera en otra sesión. Por omisión no guarda la tabla de entrada y
reemplaza las reglas escritas como funciones por declaraciones pequeñas,
para no serializar sus entornos. `meta$persistencia` declara qué se
guardó y qué no:

``` r

archivo_rds <- tempfile(fileext = ".rds")
guardar_analisis(analisis, archivo_rds)
recuperado <- leer_analisis(archivo_rds)
str(recuperado$meta$persistencia)
#> List of 4
#>  $ version_esquema      : int 1
#>  $ datos_incluidos      : logi FALSE
#>  $ evidencia_protegida  : logi TRUE
#>  $ funciones_sustituidas: int 0
identical(hallazgos(recuperado), hallazgos(analisis))
#> [1] TRUE
```

[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
convierte el análisis en un HTML autocontenido, que se abre en cualquier
navegador sin instalar nada. También acepta perfiles, mediciones,
evaluaciones y planes sueltos; un análisis ya los trae. Estas son las
secciones del informe:

``` r

archivo_html <- reportar(
  analisis,
  archivo = tempfile(fileext = ".html"),
  titulo = "Calidad de la entrega de ejemplo",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
html <- readLines(archivo_html, encoding = "UTF-8", warn = FALSE)
secciones <- regmatches(html, gregexpr("<h2>[^<]*</h2>", html))[[1]]
gsub("</?h2>", "", secciones)
#>  [1] "Análisis integral"                   "Advertencias de alcance"            
#>  [3] "Perfil de datos: entrega de ejemplo" "Distribuciones y cuantiles"         
#>  [5] "Asociaciones entre columnas"         "Análisis temporal"                  
#>  [7] "Escalas y roles propuestos"          "Propuesta de modelo"                
#>  [9] "Tablero de calidad"                  "Medidas de calidad"                 
#> [11] "Plan de limpieza"
```

En una sesión propia, `browseURL(archivo_html)` lo abre en el navegador;
acá borramos los dos archivos al terminar. Por omisión, el informe
enmascara los valores de las columnas que parecen datos personales; el
alcance de esa protección está en el artículo [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

``` r

unlink(c(archivo_html, archivo_rds))
```

## Resumen

- [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  arma en una llamada el perfil y todo lo que lo rodea; cada componente
  es una tabla, y cada pieza se puede llamar suelta.
- Patrones, tipos y fechas describen compatibilidad y frecuencia: no
  convierten nada ni confirman un formato.
- Escalas y frecuencias temporales son propuestas sin confirmar; las
  asociaciones son medidas, y se leen con los datos a la vista.
- Por omisión,
  [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)
  mide lo que la propuesta deja activo; `decision_medicion` dice qué se
  midió y `medir_propuesta = FALSE` lo apaga.
- [`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
  guarda el recorrido sin los datos, y
  [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
  lo vuelve un HTML autocontenido.

## Paso siguiente

[4. Estructura no
declarada](https://sebollin.github.io/lupa/articles/estructura-no-declarada.md)
([`vignette("estructura-no-declarada")`](https://sebollin.github.io/lupa/articles/estructura-no-declarada.md)):
claves, relaciones y dependencias entre tablas que nadie documentó.

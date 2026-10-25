# Motores y escala

Esta página es la referencia de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
y
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
para quien va a perfilar una base en producción: qué se calcula en el
motor y qué se trae a R, cuánto trabajo pide cada opción, qué queda
declarado y qué no se estima. El recorrido con un ejemplo está en
[Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.md). La
ayuda
([`?perfilar_dbi`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`?plan_perfilado_dbi`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`?requisitos_motor`](https://sebollin.github.io/lupa/reference/requisitos_motor.md))
viaja con el paquete y es la fuente completa de cada argumento y de cada
campo devuelto.

Los ejemplos corren contra SQLite y DuckDB en memoria cada vez que se
construye la página. Lo que necesita otro motor se describe sin ejecutar
y con su fuente.

## Índice

1.  [Valores por omisión](#valores-por-omision)
2.  [Qué se calcula en el motor y qué se trae a R](#universos)
3.  [Metadatos del muestreo: dos rutas](#metadatos)
4.  [Formas de muestreo en el motor](#formas-de-muestreo)
5.  [Topes de la lectura a R](#topes)
6.  [Lo que los agregados SQL no traducen](#no-traducido)
7.  [Métricas y estrategias](#metricas)
8.  [Consultas por lote](#lotes)
9.  [Política de costo](#politica-de-costo)
10. [El plan antes de correr](#plan)
11. [Avisos de costo y derrame](#avisos)
12. [Instrumentación y duraciones](#instrumentacion)
13. [Memoria del procesamiento](#memoria)
14. [Consistencia entre consultas](#consistencia)
15. [Dialectos y sondas](#dialectos)
16. [Lo que el motor guarda distinto que R](#diferencias-con-r)
17. [La clave primaria según el motor](#clave-primaria)
18. [Motores probados](#motores-probados)
19. [Fallos parciales y errores](#fallos)
20. [Dependencias e instalación](#dependencias)
21. [Hilos](#hilos)

## Valores por omisión

La tabla sale de las firmas de las dos funciones al construir esta
página. Un guion indica que la función no tiene ese argumento. En los
argumentos que eligen entre opciones se muestra la que rige por omisión
y, entre paréntesis, las demás.

| argumento | [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md) | [`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md) |
|:---|:---|:---|
| `universo` | `"tabla_completa"` (`"muestra_motor"`) | `"tabla_completa"` (`"muestra_motor"`) |
| `muestra_motor` | `NULL` | `NULL` |
| `muestra` | `Inf` | `Inf` |
| `orden_muestra` | `NULL` | `NULL` |
| `metricas` | `c("validos", "distintos", "moda", "basicos", "mediana", "desvio")` | `c("validos", "distintos", "moda", "basicos", "mediana", "desvio")` |
| `estrategia_distintos` | `"exacta"` | `"exacta"` |
| `estrategia_mediana` | `"exacta"` (`"aproximada_motor"`) | `"exacta"` (`"aproximada_motor"`) |
| `politica_costo` | `"todas"` (`"por_cardinalidad"`) | `"todas"` (`"por_cardinalidad"`) |
| `bloque_muestra` | `"con_muestra"` (`"solo_agregados"`) | `"con_muestra"` (`"solo_agregados"`) |
| `max_consultas` | `Inf` | `Inf` |
| `dialecto` | `"auto"` | `"auto"` |
| `incluir_valores` | `TRUE` | `TRUE` |
| `tamano_lote` | `NULL` | `NULL` |
| `tamano_lote_planos` | `20L` | `20L` |
| `tamano_lote_distintos` | `2L` | `2L` |
| `instrumentar` | `TRUE` | `FALSE` |
| `umbral_cardinalidad` | `0.5` | `0.5` |
| `avisar_costo_distintos` | `TRUE` | — |
| `umbral_segundos_aviso_distintos` | `30` | — |
| `avisar_costo_moda` | `TRUE` | — |
| `umbral_segundos_aviso_moda` | `30` | — |
| `avisar_costo_mediana` | `TRUE` | — |
| `umbral_segundos_aviso_mediana` | `30` | — |
| `avisar_derrame_estimado` | `TRUE` | — |
| `umbral_bytes_aviso_derrame_estimado` | `0` | — |
| `max_celdas_muestra` | `1.000.000` | `1.000.000` |
| `max_bytes_muestra` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `bloque_filas` | `NULL` | `NULL` |
| `max_bytes_procesamiento` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `max_bytes_materializacion` | `536.870.912` (512 MiB) | `536.870.912` (512 MiB) |
| `columnas_opcionales` | `character(0)` | — |
| `aplicabilidad` | `NULL` | — |
| `sentinelas_numericos` | `c(-9, -99, -999, -9999, 999)` | — |

Tres consecuencias para una corrida en producción:

- Con `universo = "tabla_completa"` y `muestra = Inf`, los agregados SQL
  recorren la tabla entera y el bloque `perfil_muestra` intenta traer a
  R **todas** las filas que permiten `max_celdas_muestra` y
  `max_bytes_muestra`. `muestra` acota sólo esa lectura, nunca los
  agregados. Para acotar el trabajo del motor está
  `universo = "muestra_motor"`; para no traer filas,
  `bloque_muestra = "solo_agregados"`.
- `instrumentar` vale `TRUE` en
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  y `FALSE` en el plan: la corrida cronometra cada consulta y el plan
  no.
- `perfil_muestra` se calcula con
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las filas traídas, con `muestra = Inf` y sin topes propios,
  porque los topes ya se aplicaron al leer. El descubrimiento de
  patrones, tipos y fechas recorre entonces todas esas filas, mientras
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  en memoria lo hace por omisión sobre una muestra de 100.000 filas. Los
  demás argumentos de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  llegan por `...`.

## Qué se calcula en el motor y qué se trae a R

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
devuelve un objeto de clase `perfil_dbi` con dos bloques que vienen de
universos distintos y no se reemplazan uno por otro:

- `resumen_tabla`: agregados SQL que calcula el motor (conteos,
  completitud, cardinalidad, moda, extremos, media, mediana, desvío),
  con una fila por columna y métrica en `resumen_tabla$sql`.
- `perfil_muestra`: el perfil completo de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  sobre las filas traídas a R, con sus hallazgos y su cobertura de
  diagnósticos.

Cuatro argumentos deciden qué entra en cada bloque:

| argumento | decide |
|----|----|
| `universo` | sobre qué filas calcula el motor los agregados: la tabla entera (`"tabla_completa"`) o una selección hecha por el propio motor (`"muestra_motor"`, con `muestra_motor = n` obligatorio) |
| `muestra` | cuántas filas se traen a R para `perfil_muestra`; `Inf` trae todas las que permiten los topes |
| `orden_muestra` | qué filas se traen y si la lectura es reproducible |
| `bloque_muestra` | si se trae alguna fila (`"con_muestra"`) o sólo se calculan los agregados (`"solo_agregados"`) |

Las combinaciones habituales:

| configuración | agregados SQL (`resumen_tabla`) | filas traídas a R (`perfil_muestra`) |
|----|----|----|
| por omisión | toda la tabla | todas las que permiten `max_celdas_muestra` y `max_bytes_muestra` |
| `muestra = n` | toda la tabla | hasta `n`: las primeras que devuelve el motor, o las primeras según `orden_muestra` |
| `universo = "muestra_motor"`, `muestra_motor = n` | las `n` filas que selecciona el motor | esas mismas filas, leídas de una copia local, recortadas por `muestra` y los topes |
| `bloque_muestra = "solo_agregados"` | lo que diga `universo` | ninguna: `perfil_muestra` es `NULL` y la cobertura dice `no_solicitado` |

`bloque_muestra = "solo_agregados"` es independiente de `universo`: con
`muestra_motor`, los agregados siguen usando la selección del motor,
pero no se seleccionan, leen ni guardan filas en el cliente;
`meta$materializacion$estado` dice `no_solicitado` y
`meta$bloques$filas_vistas` vale cero.

El ejemplo usa una tabla de 2.000 filas en SQLite en memoria.

``` r

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
set.seed(11)
n <- 2000
tramites <- data.frame(
  id_tramite = seq_len(n),
  categoria = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  monto = round(stats::rlnorm(n, 9, 1.1), 2),
  edad = c(sample(18:85, n - 20, replace = TRUE), rep(NA, 20))
)
DBI::dbWriteTable(con, "tramites", tramites)
```

``` r

por_omision <- perfilar_dbi(con, "tramites")
con_muestra <- perfilar_dbi(con, "tramites", muestra = 500, orden_muestra = "id_tramite")
del_motor <- perfilar_dbi(con, "tramites", universo = "muestra_motor", muestra_motor = 300)
solo_agregados <- perfilar_dbi(con, "tramites", bloque_muestra = "solo_agregados")

alcance <- function(x) {
  r <- x$resumen_tabla
  data.frame(
    universo = r$meta$universo,
    n_resumen = r$columnas$n[r$columnas$columna == "id_tramite"],
    validos_resumen = r$columnas$n_validos[r$columnas$columna == "id_tramite"],
    filas_en_r = if (is.null(x$perfil_muestra)) 0 else n_filas(x$perfil_muestra)
  )
}
rbind(
  por_omision = alcance(por_omision),
  con_muestra = alcance(con_muestra),
  del_motor = alcance(del_motor),
  solo_agregados = alcance(solo_agregados)
)
#>                      universo n_resumen validos_resumen filas_en_r
#> por_omision    tabla_completa      2000            2000       2000
#> con_muestra    tabla_completa      2000            2000        500
#> del_motor       muestra_motor      2000             300        300
#> solo_agregados tabla_completa      2000            2000          0
```

La métrica `n` conserva siempre el total de la tabla, también con
`muestra_motor`. Las demás métricas SQL de ese universo describen la
selección del motor: son observaciones de esa muestra, no
extrapolaciones al universo. `resumen_tabla$columnas` trae 15 campos por
columna y `perfil_muestra` el perfil completo de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
con 117. Los accesores
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
[`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
y
[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
funcionan sobre los dos tipos de objeto; su contrato está en [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md).

Sin muestra, la protección de datos personales, si se pide, no puede
clasificar las columnas y protege los valores de celda de todas: moda,
extremos, media, mediana y desvío quedan `omitido_por_privacidad`.

``` r

con_proteccion <- perfilar_dbi(
  con, "tramites", bloque_muestra = "solo_agregados",
  proteger_datos_personales = TRUE
)
table(sql_perfil(con_proteccion)$estado)
#> 
#>              calculado              no_aplica omitido_por_privacidad 
#>                     34                      7                     19
table(sql_perfil(solo_agregados)$estado)
#> 
#> calculado no_aplica 
#>        53         7
```

Con muestra, la clasificación se hace sobre `perfil_muestra` y alcanza
también al resumen. El detalle de qué se enmascara está en [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

## Metadatos del muestreo: dos rutas

Hay dos lugares con metadatos de muestreo, y describen cosas distintas:

| ruta | existe cuando | describe |
|----|----|----|
| `perfil_muestra$meta$origen_dbi$muestreo` | se pidió y se leyó el bloque de filas (`bloque_muestra = "con_muestra"`), con cualquier `universo` | la lectura a R: `filas_solicitadas`, `filas_obtenidas`, `acotado_en`, `metodo`, `reproducible` y la sentencia `sql_muestra` |
| `resumen_tabla$meta$muestreo` | sólo con `universo = "muestra_motor"` | la selección del motor: `metodo_muestreo`, `sesgo_muestreo`, `tamano_muestra`, `fraccion`, `universo` y los identificadores de la copia local |

En el caso habitual —`tabla_completa` con `muestra`— la segunda ruta es
`NULL`. Leerla por costumbre devuelve `NULL`, y un guion que no lo
compruebe calcula sobre nada:

``` r

is.null(con_muestra$resumen_tabla$meta$muestreo)
#> [1] TRUE
lectura <- con_muestra$perfil_muestra$meta$origen_dbi$muestreo
str(lectura[c("filas_solicitadas", "filas_obtenidas", "acotado_en", "metodo",
              "reproducible")])
#> List of 5
#>  $ filas_solicitadas: num 500
#>  $ filas_obtenidas  : num 500
#>  $ acotado_en       : chr "motor"
#>  $ metodo           : chr "primeras_filas_segun_orden"
#>  $ reproducible     : logi TRUE
```

Con `muestra_motor` aparecen las dos. `filas_obtenidas` de la lectura a
R y `tamano_muestra` de la selección del motor responden preguntas
distintas: cuántas filas recibió
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
cuántas pidió la consulta del motor.

``` r

seleccion <- del_motor$resumen_tabla$meta$muestreo
str(seleccion[c("metodo_muestreo", "sesgo_muestreo", "tamano_muestra",
                "fraccion", "universo", "filas_obtenidas")])
#> List of 6
#>  $ metodo_muestreo: chr "random_limit"
#>  $ sesgo_muestreo : chr "por_fila"
#>  $ tamano_muestra : num 300
#>  $ fraccion       : num 0.15
#>  $ universo       : num 2000
#>  $ filas_obtenidas: num 300
```

En `resumen_tabla$meta$muestreo`, `tamano_muestra` es el tamaño efectivo
pedido a la consulta; `filas_solicitadas` y `filas_pedidas`, el pedido
original; y `filas_obtenidas`, las filas que devolvió la copia local. Si
`muestra` es menor que `muestra_motor`, `meta$materializacion$n_filas`
puede superar a `n_filas(perfil_muestra)`. La cardinalidad de una
muestra se publica con estado `observado_muestra`: es lo visto en la
muestra, con el universo al lado, y no estima la del universo sin un
estimador declarado.

Sin `orden_muestra`, las filas traídas son las primeras que devuelve el
motor, en un orden que SQL no garantiza; `reproducible` lo dice. Con
`orden_muestra`, la lectura sólo se declara reproducible si la
combinación de columnas es única en toda la tabla, y el paquete lo
verifica (`orden_unico_verificado`).

## Formas de muestreo en el motor

Con `universo = "muestra_motor"`, la selección la hace el motor con la
primera forma que su dialecto ofrece y su sonda acepta. El orden está
fijado en el código:

| orden | forma | se ofrece a | tamaño obtenido |
|---:|----|----|----|
| 1 | `TABLESAMPLE RESERVOIR (n ROWS)` | DuckDB | exactamente `n` filas |
| 2 | `TABLESAMPLE SYSTEM (p)` | PostgreSQL, SQL Server | bloques enteros: varía alrededor de lo pedido y puede ser cero en una tabla chica |
| 3 | `TABLESAMPLE BERNOULLI (p)` | PostgreSQL, DuckDB | fila por fila: varía poco alrededor de lo pedido, a cambio de recorrer la tabla |
| 4 | `TABLESAMPLE (p PERCENT)` | SQL Server | por páginas |
| 5 | `SAMPLE (p)` | Oracle | fila por fila |
| 6 | orden pseudoaleatorio con el límite del dialecto | cualquier motor con límite | exactamente `n`, a cambio de ordenar la tabla entera |

La forma de cantidad fija va primero porque dos consultas del mismo
perfil deben ver muestras de tamaño comparable. Las formas por bloques
cuestan menos sobre tablas grandes y por eso preceden a las de fila por
fila, pero su tamaño salta. DuckDB permite verlo en la misma tabla de
5.000 filas, pidiendo el 20 % con seis semillas:

``` r

duck <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
DBI::dbWriteTable(duck, "t", data.frame(id = seq_len(5000)))
contar <- function(muestreo, semillas = 1:6) {
  vapply(semillas, function(s) {
    sql <- paste("SELECT COUNT(*) FROM t TABLESAMPLE", sprintf(muestreo, s))
    DBI::dbGetQuery(duck, sql)[[1]]
  }, numeric(1))
}
tamanos <- rbind(
  bloques = contar("20 PERCENT (system, %d)"),
  fila_por_fila = contar("20 PERCENT (bernoulli, %d)"),
  cantidad_fija = contar("RESERVOIR (1000 ROWS) REPEATABLE (%d)")
)
colnames(tamanos) <- paste0("semilla_", 1:6)
tamanos
#>               semilla_1 semilla_2 semilla_3 semilla_4 semilla_5 semilla_6
#> bloques            2048         0         0      5000      2048         0
#> fila_por_fila      1025       996       984       963       960       985
#> cantidad_fija      1000      1000      1000      1000      1000      1000
```

Sobre 1.000 filas pedidas, el muestreo por bloques dio entre 0 y 5.000,
el de fila por fila entre 960 y 1.025, y el de cantidad fija siempre
1.000.

Lo que cada forma implica queda publicado:

- `TABLESAMPLE SYSTEM` publica `sesgo_muestreo = "por_bloques"` con el
  motivo `sesgo_muestreo:tablesample_system_por_bloques`. Si una
  consulta recibe menos de la mitad de las filas pedidas, sus métricas
  quedan `no_disponible` con
  `muestra_inestable:tablesample_system_tamano_insuficiente`.
- El orden pseudoaleatorio publica `sesgo_muestreo = "por_fila"`. En SQL
  Server, donde la función es `NEWID()`, la mediana bajo `muestra_motor`
  sólo acepta esa forma si la tabla tiene hasta 100.000 filas, la
  muestra pedida es al menos el 20 % de la tabla y la proyección —650 ms
  por millón de filas por mediana pendiente— no pasa de 1.000 ms. Son
  constantes del código, no mediciones de la corrida. Fuera de esos
  límites la mediana queda `no_disponible` con
  `capacidad_no_aceptada:newid_costo_excede_presupuesto`.
- Si ninguna forma es aceptada, las métricas SQL quedan `no_disponible`.
  Nunca se reemplazan por resultados de la tabla completa.
- Si la muestra vuelve vacía, las métricas de alcance `muestra` se
  publican en `NA` con estado `no_disponible` y un motivo
  `muestra_vacia:...`; no se publica cero ni se concluye que la columna
  esté vacía.

## Topes de la lectura a R

Dos topes acotan `perfil_muestra` y no tocan ninguna consulta de
agregados:

- `max_celdas_muestra` (1.000.000 celdas por omisión) se resuelve
  **antes de leer**, con el conteo de filas y el ancho del esquema.
- `max_bytes_muestra` (512 MiB por omisión) se resuelve con una sonda de
  hasta 100 filas; el límite resultante va al SQL final o a
  `dbFetch(n)`. La muestra no se trae entera para recortarla en R.

Si `muestra = n` y un tope es más estricto, manda el menor. La cobertura
de diagnósticos de `perfil_muestra` declara las celdas o bytes
observados, el umbral y cuál tope mandó; `Inf` en un tope lo desactiva.
Con un tope de 1.000 celdas sobre cuatro columnas:

``` r

acotado <- perfilar_dbi(con, "tramites", max_celdas_muestra = 1000)
str(acotado$perfil_muestra$meta$origen_dbi$muestreo[c(
  "filas_solicitadas", "filas_maximas_por_celdas", "celdas_solicitadas",
  "filas_obtenidas", "acotado_en"
)])
#> List of 5
#>  $ filas_solicitadas       : num Inf
#>  $ filas_maximas_por_celdas: num 250
#>  $ celdas_solicitadas      : num 8000
#>  $ filas_obtenidas         : num 250
#>  $ acotado_en              : chr "motor"
"muestra_perfilado" %in% cobertura(acotado$perfil_muestra)$diagnostico
#> [1] TRUE
```

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
anticipa el recorte de celdas en `attr(plan, "tope_muestra")` y avisa
que el de bytes necesitará la sonda.

Otros cuatro límites gobiernan el trabajo:

| argumento | qué acota | al agotarse |
|----|----|----|
| `max_consultas` | cantidad de consultas de la corrida | las métricas restantes quedan `no_disponible` y la cobertura dice `presupuesto_agotado` |
| `max_bytes_materializacion` | bytes de la copia local de `muestra_motor`, comprobados por tramo antes de cada escritura | `spool_presupuesto_excedido` y `muestra_inestable:presupuesto_materializacion`, sin resultado híbrido |
| `max_bytes_procesamiento` | estado retenido por la lectura por bloques y por la relectura de la copia local | se comprueba antes de publicar un bloque o una pasada |
| `bloque_filas` | activa el recorrido de la tabla completa por bloques de ese tamaño | — |

`bloque_filas = n` calcula el resumen de la tabla completa en R, leyendo
la tabla por bloques de `n` filas desde un único result set
(`dbSendQuery()` y `dbFetch(n)`), sin retener todas las filas. La
cobertura se lee en `meta$bloques$filas_vistas`; `perfil_muestra` sigue
siendo la muestra diagnóstica acotada por `muestra` y los topes. Los
controladores sin retención incremental demostrada —DuckDB entre ellos—
quedan `no_disponible` con su motivo.

``` r

por_bloques <- perfilar_dbi(con, "tramites", bloque_filas = 500, muestra = 100)
por_bloques$resumen_tabla$meta$bloques$filas_vistas
#> [1] 2000
n_filas(por_bloques$perfil_muestra)
#> [1] 100
```

## Lo que los agregados SQL no traducen

`sentinelas_numericos`, `aplicabilidad` y `columnas_opcionales` cambian
lo que resume `perfil_muestra`, pero los agregados SQL se calculan sin
ellas: `AVG()` no sabe de centinelas. Cuando se declara alguna,
`resumen_tabla$cobertura` recibe una fila `degradado` que lo dice. Con
treinta edades `-999` declaradas como centinela, los dos bloques
publican dos medias distintas sobre las mismas filas:

``` r

centinelas <- tramites
centinelas$edad[1:30] <- -999L
DBI::dbWriteTable(con, "centinelas", centinelas)
declarado <- perfilar_dbi(con, "centinelas", sentinelas_numericos = -999)

media_edad <- function(cols) cols$media[cols$columna == "edad"]
c(
  resumen_sql = media_edad(columnas(declarado)),
  perfil_muestra = media_edad(columnas(declarado$perfil_muestra))
)
#>    resumen_sql perfil_muestra 
#>       34.63182       50.53385
cobertura(declarado)[, c("bloque", "elemento", "estado")]
#>          bloque             elemento    estado
#> 1 resumen_tabla sentinelas_numericos degradado
```

Sobre un `perfil_dbi`,
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve las del resumen SQL; las del perfil de muestra se piden sobre
`perfil_muestra`.

Cuando existen los dos bloques,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
los cruza. Compara sólo lo que los dos miden con la misma semántica; lo
que una declaración cambia a propósito queda en
`meta$corroboracion_bloques$no_comparadas`, con su causa:

``` r

head(declarado$resumen_tabla$meta$corroboracion_bloques$no_comparadas, 4)
#>   columna metrica                causa
#> 1    edad   media sentinelas_numericos
#> 2    edad mediana sentinelas_numericos
#> 3    edad  minimo sentinelas_numericos
#> 4    edad  maximo sentinelas_numericos
```

Si dos valores comparables difieren más allá de la tolerancia, la
cobertura recibe una fila `divergencia` con los dos valores. No se elige
un ganador. Si la muestra cubre sólo parte de la tabla, una diferencia
ordinaria es esperable y se declara `no_corroborada`, no `divergencia`.
La tolerancia está en `meta$corroboracion_bloques$tolerancia`: `1e-10`
absoluta más 64 épsilon de máquina relativos para los valores leídos, y
`1e-10` relativa para la media y el desvío, que cada lado acumula a su
modo.

`columnas_codigo`, que llega por `...`, sí vale en el resumen: la media,
la mediana y el desvío de una columna declarada quedan en `NA` y la
cobertura los marca `no_aplica`.

## Métricas y estrategias

`metricas` elige grupos; cada grupo publica estos campos de
`resumen_tabla$columnas`:

| grupo         | campos                                                |
|:--------------|:------------------------------------------------------|
| `"validos"`   | `n_validos`, `n_faltantes`, `prop_faltantes`          |
| `"distintos"` | `n_distintos`, `tasa_distintos`                       |
| `"moda"`      | `moda`, `frecuencia_moda`                             |
| `"basicos"`   | `minimo`, `maximo`, `media`, `n_ceros`, `n_negativos` |
| `"mediana"`   | `mediana`                                             |
| `"desvio"`    | `desvio`                                              |

`metricas = c("validos", "basicos", "desvio")` evita la cardinalidad, la
moda y la mediana, que son las consultas que agrupan u ordenan;
`metricas = "validos"` deja sólo la completitud.

Cada fila de `resumen_tabla$sql` lleva `estado`, `alcance`, `universo`,
`tamano_muestra`, `fraccion`, `metodo` y `error_esperado`. `estado`
distingue lo calculado, lo estimado y lo no disponible, además de lo
omitido por costo o por protección. En una muestra del motor,
`error_esperado` vale `no_estimado` cuando el error podría calcularse
bajo un plan probabilístico pero no se calculó, `no_estimable` para la
moda, la mediana y la cardinalidad observada, y `no_aplica` cuando no
hubo muestreo efectivo. No se publica una cota numérica sin una fórmula
que la justifique.

**`estrategia_distintos`** elige de dónde sale `n_distintos`:

- `"exacta"` (por omisión): `COUNT(DISTINCT)` sobre las filas de la
  corrida.
- `"aproximada_motor"`: una función nativa, sólo si la sonda la acepta;
  sus cotas de error no documentadas quedan como `"desconocido"`. Si la
  función sólo sabe construir una consulta completa, válidos y distintos
  se emiten por separado y cada registro conserva el método de la
  consulta que corrió.
- `"catalogo"`: `pg_stats.n_distinct` de PostgreSQL, publicado como
  `estimado_catalogo`, sólo con `universo = "tabla_completa"`.
- `"omitida"`: no emite la consulta.

No hay repliegue entre estrategias: una estrategia sin capacidad queda
`no_disponible` y nunca se convierte en un conteo exacto. En SQLite, el
catálogo no existe:

``` r

por_catalogo <- perfilar_dbi(
  con, "tramites", estrategia_distintos = "catalogo",
  bloque_muestra = "solo_agregados"
)
estrategia <- por_catalogo$resumen_tabla$meta$estrategia_distintos
estrategia$estado
#> [1] "no_disponible"
writeLines(strwrap(estrategia$motivo, 88))
#> `catalogo` requiere PostgreSQL y su `pg_stats`: no hay una estadistica portable de
#> cardinalidad para este motor.
```

Lo pedido, lo resuelto y el estado quedan en `meta$estrategia_distintos`
y en las columnas `estrategia_solicitada` y `estrategia_resuelta` de
`resumen_tabla$sql`.

**`estrategia_mediana`** habilita una aproximación, pero no la impone:
la sonda prueba primero una forma exacta consolidada, después una exacta
por columna y deja las aproximadas para el final.
`resumen_tabla$sql$metodo` dice qué corrió; sólo una aproximación
ejecutada queda `estimado`. `universo = "muestra_motor"` con
`estrategia_mediana = "aproximada_motor"` se rechaza al entrar.

**`incluir_valores = FALSE`** no emite las consultas de moda, extremos y
mediana, y limpia los valores de celda del resumen, la muestra, los
bloques, la cobertura y los motivos. Los conteos y proporciones se
siguen publicando.

## Consultas por lote

El costo de una tabla grande es la cantidad de recorridos, no el
muestreo. Por eso las consultas se agrupan:

- **Agregados planos** —`COUNT(col)`, mínimo, máximo, media, ceros,
  negativos y desvío— viajan en una sola consulta por lote de hasta
  `tamano_lote_planos` columnas (20 por omisión). Cada una trae
  `COUNT(*) AS n_total_consulta`: la completitud usa ese denominador
  local y no el total de otro lote.
- **Cardinalidades exactas** van en su propia clase, por lotes de
  `tamano_lote_distintos` (2 por omisión). Cada `COUNT(DISTINCT col)`
  viaja con `COUNT(col) AS n_validos_guard`, un conteo de control de la
  misma sentencia.
- **La moda** es una consulta por columna, porque agrupa. Intenta traer
  `SUM(COUNT(*)) OVER () AS n_validos_guard` junto a su frecuencia; la
  forma se sondea y, si el motor la rechaza, se usa la consulta sin ese
  conteo y `resumen_tabla$meta$moda_guardian` lo declara.
- **La mediana** es una consulta por columna como respaldo. Si la sonda
  acepta `PERCENTILE_CONT(...) WITHIN GROUP`, varias viajan en un solo
  `SELECT` por lote; en SQL Server eso requiere nivel de compatibilidad
  110 o mayor, y el motivo de un rechazo queda en
  `meta$mediana_consolidada$motivo`. En los dialectos con `LIMIT` que no
  la tienen —SQLite y PostgreSQL 9.3—, el conteo de la mediana es una
  subconsulta escalar de la misma sentencia, si la sonda acepta la
  división entera con `%` y `/`.

`tamano_lote` fija los dos tamaños a la vez. La cantidad de consultas
que cada tamaño produce sobre la tabla del ejemplo, según el plan:

``` r

consultas_por_lote <- function(...) {
  plan <- plan_perfilado_dbi(con, "tramites", ...)
  tabla <- as.data.frame(plan)
  c(
    planos = sum(tabla$n_consultas[grepl("planos", tabla$clase_consulta)]),
    distintos = sum(tabla$n_consultas[grepl("distintos", tabla$clase_consulta)]),
    total = attr(plan, "total")
  )
}
rbind(
  planos_1 = consultas_por_lote(tamano_lote_planos = 1),
  por_omision = consultas_por_lote(),
  distintos_1 = consultas_por_lote(tamano_lote_distintos = 1),
  distintos_4 = consultas_por_lote(tamano_lote_distintos = 4)
)
#>             planos distintos total
#> planos_1         6         2    24
#> por_omision      3         2    21
#> distintos_1      3         4    23
#> distintos_4      3         1    20
```

El valor de `tamano_lote_distintos` es un supuesto del código, anotado
junto a la constante (`R/perfilar-dbi.R`). En PostgreSQL la lectura de
la tabla es la misma con cualquier lote, porque el motor amortiza una
pasada entre los agregados del mismo `SELECT`; lo que crece con el lote
es el derrame a disco. Un lote de dos columnas cuesta por columna
bastante menos que uno de una y casi lo mismo que uno de cuatro, y
derrama menos que los mayores. El óptimo depende de `work_mem`, que
cambia por servidor; para elegir otro valor, `instrumentar = TRUE`
publica la duración de cada consulta.

Si un lote falla, no se pierde el lote. Sus mitades se sondean por
bisección: los grupos aceptados se reutilizan como mediciones y las
columnas culpables se reintentan por métrica, con su denominador local.
Lo que igual falle queda `no_disponible` con su motivo. Si el
presupuesto se agota antes de aislar todo, las columnas pendientes
quedan sin medir: no se las supone culpables ni legibles.
`meta$tamano_lote_funciono` conserva el mayor lote aceptado en esa
corrida; no se guarda estado asociado a la conexión.

`resumen_tabla$sql` agrega `lote` y `columnas_compartidas` para ver qué
consulta fue compartida. El orden de degradación es: agregados planos,
total del universo cuando hace falta, distintos, moda y mediana.

## Política de costo

La moda y la mediana son las métricas caras. `politica_costo` acepta dos
valores, y ninguno más:

``` r

tryCatch(
  perfilar_dbi(con, "tramites", politica_costo = "ninguna"),
  error = function(e) writeLines(strwrap(conditionMessage(e), 88))
)
#> `politica_costo` debe ser un único valor: `todas` o `por_cardinalidad`.
```

- `"todas"` (por omisión) conserva moda y mediana en todas las columnas
  pedidas. El paquete no elige por el usuario.
- `"por_cardinalidad"` resuelve primero las fuentes estructurales —una
  clave primaria garantizada dice cuántos distintos hay sin contarlos— y
  mide `validos` y `distintos` sólo si hace falta y la estrategia lo
  permite. Después omite, por columna, **sólo la moda**, cuando
  `n_distintos / n_validos >= umbral_cardinalidad`. El umbral vale `0.5`
  por omisión y se puede mover en cada llamada. La mediana no se omite
  por cardinalidad: su costo lo gobiernan las filas.

``` r

barato <- perfilar_dbi(con, "tramites", politica_costo = "por_cardinalidad")
s <- sql_perfil(barato)
s[s$estado == "omitido_por_costo", c("columna", "metrica", "estado")]
#>       columna         metrica            estado
#> 10 id_tramite            moda omitido_por_costo
#> 11 id_tramite frecuencia_moda omitido_por_costo
#> 38      monto            moda omitido_por_costo
#> 39      monto frecuencia_moda omitido_por_costo
writeLines(strwrap(barato$resumen_tabla$meta$decisiones_costo$monto$detalle$motivo, 88))
#> Se omitió la moda de la columna `monto`: tiene 2000 valores distintos de 2000 válidos
#> (1.000) sobre la tabla completa; la política optativa considera que agrupar u ordenar
#> toda la columna no justifica el costo. Para pedirla igual, use `politica_costo =
#> "todas"`; para mover el criterio, cambie `umbral_cardinalidad`.
```

Sobre una tabla más ancha, la diferencia se nota en la cantidad de
consultas. La tabla que sigue tiene 200 filas y 158 columnas —60
numéricas, 60 de texto, 20 enteras y 18 fechas guardadas como texto— y
es la misma que usan `benchmark/medir_plan_ancho.R` y
`benchmark/medir_politica_costo.R`:

``` r

set.seed(1)
filas <- 200
generar <- function(prefijo, k, valores) {
  stats::setNames(lapply(seq_len(k), function(i) valores()), paste0(prefijo, seq_len(k)))
}
ancha <- as.data.frame(c(
  generar("num", 60, function() round(stats::rnorm(filas) * 100, 2)),
  generar("txt", 60, function() sample(letters, filas, replace = TRUE)),
  generar("ent", 20, function() sample(seq_len(50L), filas, replace = TRUE)),
  generar("fec", 18, function() {
    as.character(as.Date("2026-01-01") - sample(seq_len(900L), filas, replace = TRUE))
  })
))
DBI::dbWriteTable(con, "ancha", ancha)

corrida <- function(politica) {
  perfilar_dbi(
    con, "ancha", bloque_muestra = "solo_agregados", instrumentar = FALSE,
    politica_costo = politica, proteger_datos_personales = FALSE
  )
}
emitidas_ancha <- function(x) x$resumen_tabla$meta$consultas$emitidas
todas <- corrida("todas")
por_cardinalidad <- corrida("por_cardinalidad")
por_politica <- c(
  todas = emitidas_ancha(todas),
  por_cardinalidad = emitidas_ancha(por_cardinalidad)
)
por_politica
#>            todas por_cardinalidad 
#>              334              256
```

Con `"por_cardinalidad"`, la corrida ahorró 78 de 334 consultas, todas
de moda.

Las fuentes estructurales se resuelven siempre que la política necesita
la cardinalidad, aunque `estrategia_distintos` esté omitida o no
disponible: la estrategia decide si se puede medir, pero no oculta una
clave primaria garantizada. Si no hay fuente estructural y la estrategia
no permite medir, la cardinalidad queda desconocida y la política sigue
su regla para ese caso.

Cada omisión queda en `resumen_tabla$sql` como `omitido_por_costo`, con
qué se omitió, por qué y cómo pedirlo de nuevo
(`politica_costo = "todas"` o otro umbral). `meta$decisiones_costo`
explica por separado la decisión sobre cada métrica de cada columna.
`fuente_cardinalidad_costo` dice de dónde salió la proporción, y es
independiente de `estrategia_distintos`: la política nunca convierte una
petición `"aproximada_motor"` o `"catalogo"` en un `COUNT(DISTINCT)`
exacto.

## El plan antes de correr

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
emite sólo consultas de preparación —el esquema, las sondas de
capacidad, el catálogo de la clave primaria y, en PostgreSQL, sus
estadísticas— y dice cuántas consultas emitiría el perfilado, de qué
clase y con qué alcance. No escanea datos para decidir el costo.

``` r

plan <- plan_perfilado_dbi(con, "tramites")
knitr::kable(as.data.frame(plan))
```

| clase_consulta | n_consultas | alcance | n_consultas_max |
|:---|---:|:---|---:|
| portones (esquema y sondas) | 7 | una vez | 7 |
| agregados planos por lotes (COUNT + MIN/MAX/AVG/SUM CASE + desvio) | 3 | escanea la tabla completa | 3 |
| distintos por lotes (clase separada) | 2 | escanea la tabla completa | 2 |
| moda (GROUP BY + orden + limite) | 4 | ordena o agrupa la tabla completa | 4 |
| mediana (orden total + limite/salto) | 3 | ordena la tabla completa | 3 |
| muestra | 2 | lee las filas pedidas | 2 |

El costo es un **rango**. `attr(plan, "total")` (21 aquí) cuenta las
consultas si ningún lote se rechaza y, con
`politica_costo = "por_cardinalidad"`, supone que se omite la moda cuya
cardinalidad no se conoce. `attr(plan, "total_maximo")` (34) suma el
camino que ejecuta esa moda y las sondas de bisección si el motor
rechazara todos los lotes: hasta `2n − 1` por lote de `n` columnas.
`total_lotes_rechazados` publica el mismo número. La corrida por omisión
del ejemplo emitió 21. El rango vale cuando el muestreo se puede
construir: si `universo = "muestra_motor"` y la forma no se puede
construir, `attr(plan, "muestreo")` lo declara y el plan excluye esas
métricas. En estado `"no_sondeado"`, la forma sólo se construyó
localmente y el rango queda condicionado a que la sonda de la corrida la
acepte.

La cuenta sigue a la composición de la tabla, no a la cantidad de
columnas. Con la tabla ancha de la sección anterior y una de 158
columnas, todas de texto:

``` r

plan_ancho <- plan_perfilado_dbi(con, "ancha")
recorre <- !as.character(plan_ancho$alcance) %in% c("una vez", "lee las filas pedidas")
texto <- as.data.frame(generar("txt", 158, function() sample(letters, filas, replace = TRUE)))
DBI::dbWriteTable(con, "ancha_texto", texto)
c(
  ancha = attr(plan_ancho, "total"),
  ancha_recorren_la_tabla = sum(plan_ancho$n_consultas[recorre]),
  solo_texto = attr(plan_perfilado_dbi(con, "ancha_texto"), "total")
)
#>                   ancha ancha_recorren_la_tabla              solo_texto 
#>                     336                     327                     253
```

De las 336 consultas de la tabla ancha, 327 escanean, ordenan o agrupan
la tabla entera. Las columnas de texto cuestan menos, sobre todo porque
la mediana —que ordena la tabla— se pide sólo sobre columnas numéricas.
`muestra` no acota nada de eso: acota lo que se trae a R, no el trabajo
del motor. `benchmark/medir_plan_ancho.R` rehace estas cifras y deja una
salida fechada con el entorno.

Cada sonda de capacidad gasta un número fijo de consultas aunque acierte
en la primera forma. Es una decisión de diseño: si el costo variara por
motor, el plan no podría decir exactamente cuántas consultas va a
emitir.

Contar consultas no dice cuánto cuestan: catorce consultas sobre dos
millones de filas son más trabajo que doscientas sobre mil. El plan
estima además la **magnitud**, en dos mitades:

- la del motor: `filas_leidas` y `ordenaciones_completas`, resumidas en
  `magnitud_motor`;
- la del cliente: `columnas_texto` y `pares_texto`, los pares de formas
  que el detector de vocabulario podría comparar en R sobre la muestra,
  resumidas en `magnitud_texto`.

`magnitud` es la mayor de las dos: `"baja"`, `"media"`, `"alta"` o
`"desconocida"` si no se conoce el número de filas. El plan no cuenta
filas; sólo en PostgreSQL, un `pg_class.reltuples` positivo aporta una
estimación de catálogo, rotulada como tal. En SQLite queda desconocida:

``` r

attr(plan, "magnitud")
#> [1] "desconocida"
num(attr(plan, "pares_texto"))
#> [1] "2.000.000"
```

La magnitud es una estimación. La del motor cuenta las filas que habría
que leer si ningún índice ayudara; la del cliente cuenta pares, cuyo
costo depende del largo de los valores, que el plan no conoce sin
leerlos. Al imprimir el plan se ven las dos mitades, el aviso de trabajo
alto y las palancas para acotarlo; ese texto cita velocidades de
referencia de otras corridas para convertir cuentas en segundos, que son
supuestos del código y no mediciones de la tabla.

El plan no proyecta segundos para `COUNT(DISTINCT)`: sin leer datos no
tiene una referencia medida. Con `universo = "muestra_motor"`,
`attr(plan, "materializacion")` declara la selección única que se pagará
para la copia local, con `pagado = FALSE`, y `attr(plan, "pasadas")` las
pasadas que leerán esa copia.

## Avisos de costo y derrame

Los avisos de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
llegan también en guiones no interactivos, porque el costo se paga en el
servidor. Cada uno tiene su interruptor y su umbral, y apagarlo no apaga
la medición ni su metadato.

| aviso | cómo proyecta | cuándo avisa | dónde queda |
|----|----|----|----|
| `COUNT(DISTINCT)` | mediana de las duraciones del primer lote de distintos de esta corrida, por la cantidad de lotes | después del primer lote y antes del segundo, si hay más de uno y la proyección alcanza `umbral_segundos_aviso_distintos` | `meta$costo_distintos` |
| moda | ms por valor distinto de la primera moda medida, por la cardinalidad de las demás | antes de la siguiente moda | `meta$costo_moda` |
| mediana | ms por fila de la primera mediana medida | antes de las medianas pendientes | `meta$costo_mediana` |
| derrame estimado (PostgreSQL) | tamaño del hash con `pg_stats` y `pg_class.reltuples` contra `work_mem` | antes del primer `COUNT(DISTINCT)` | `meta$estimacion_derrame` |

Los tres umbrales temporales valen 30 segundos por omisión; `Inf` apaga
cada aviso de forma explícita. Con un solo lote de distintos no hay nada
que proyectar. La proyección de distintos no usa `reltuples`, y si la
moda no tiene cardinalidad la proyección lo declara en vez de suponerla.

``` r

costo <- por_omision$resumen_tabla$meta$costo_distintos
str(costo[c("disponible", "n_lotes")])
#> List of 2
#>  $ disponible: logi TRUE
#>  $ n_lotes   : int 2
writeLines(strwrap(costo$fuente, 88))
#> mediana de 1 consulta(s) del primer lote de distintos medidas en esta corrida
```

Cuando una mediana total no deja una medición local, la proyección usa
una referencia de 68 ms por millón de filas. Es una constante del código
tomada de otra corrida, sin guion que la rehaga, y no se presenta como
medición de la corrida actual. Si la consulta que contó las filas fue
medida y da una cota mayor, esa cota se publica aparte, como lectura
observada.

En PostgreSQL, la preparación lee además `pg_stats.n_distinct`,
`pg_stats.avg_width`, `pg_class.reltuples`, `work_mem` y, desde
PostgreSQL 13, `hash_mem_multiplier`. Si el hash estimado supera el
límite efectivo, avisa que subir `work_mem` en la sesión puede evitar el
derrame; el paquete no cambia la configuración ni espera confirmación.
La estimación de la moda usa `EXPLAIN (FORMAT JSON, COSTS OFF)` sin
`ANALYZE`, y la de la mediana modela un ordenamiento con mínimos de 32
bytes por valor para tipos fijos y 42 para `numeric`. Con
`instrumentar = TRUE` y `pg_stat_statements` disponible, el derrame
medido —bloques temporales escritos y leídos— manda sobre la estimación.
Si la llamada no se puede atribuir, el estado queda no disponible: el
tiempo transcurrido no se presenta como evidencia de derrame.

El bloque `perfil_muestra` pasa por
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md), y
en una sesión interactiva recibe también su aviso de tabla ancha desde
100.000 celdas. Ese aviso divide las celdas por una velocidad de
referencia de 10.000 celdas por segundo: es un supuesto del código, no
una medición de la corrida ni de la máquina, y la espera real puede ser
mayor. El aviso se describe en [Escala y
duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)
y las mediciones fechadas están en [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

## Instrumentación y duraciones

Con `instrumentar = TRUE` (por omisión en la corrida), cada fila de
`resumen_tabla$sql` suma `duracion_ms`, `cpu_ms`, `n_filas_resultado`,
`bytes_resultado_r`, `consulta_id`, `etapa` y `nivel`. `duracion_ms` usa
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html); `cpu_ms` suma
`proc.time()[c("user.self", "sys.self")]` y mide el trabajo de CPU del
cliente, incluida la conversión del resultado que hace el controlador.
Cero es medido; `NA` es no medido. Con `instrumentar = FALSE` se emiten
las mismas consultas en el mismo orden y esos campos quedan en `NA`.

`duracion_ms` es la duración de la **consulta**, repetida en cada fila
que esa consulta produjo. Para sumar, sólo cuentan las filas de
`nivel = 1`, la primera de cada `consulta_id`:

``` r

s <- sql_perfil(por_omision)
table(s$nivel, useNA = "ifany")
#> 
#>  1  2 
#> 17 43
length(unique(stats::na.omit(s$consulta_id)))
#> [1] 10
```

``` r

sum(s$duracion_ms[s$nivel == 1], na.rm = TRUE)
```

Dos identificadores responden preguntas distintas. `consulta_id`
identifica la sentencia que produjo cada medición dentro de la corrida y
define el grupo de consistencia. `id_consulta` identifica la consulta de
datos: dos métricas con el mismo valor vieron exactamente las mismas
filas. `id_consulta` no depende de `instrumentar`, y vale `NA` en la
moda y la mediana, que no comparten filas con otras métricas.
`memoria_trabajo` clasifica el estado de trabajo del motor como
`creciente`, `acotado` o `NA`.

`resumen_tabla$tiempos` reúne las etapas grandes del cliente en la misma
unidad. Las de `nivel = 1` son disjuntas; las de nivel mayor están
contenidas en alguna de ellas:

``` r

por_omision$resumen_tabla$tiempos[, c("etapa", "nivel", "estado")]
#>                            etapa nivel        estado
#> 1           ausencia_estructural     2        medido
#> 2    casi_duplicados_vocabulario     2        medido
#> 3 clasificacion_datos_personales     2        medido
#> 4                   dependencias     2        medido
#> 5         duplicados_aproximados     2 no_solicitado
#> 6                lectura_muestra     1        medido
#> 7             perfilado_columnas     2        medido
#> 8              perfilado_muestra     1        medido
#> 9    proteccion_datos_personales     2 no_solicitado
```

En una sesión interactiva, una barra avanza contra las consultas que el
plan dice que se van a emitir; `options(lupa.progreso = FALSE)` la apaga
y `TRUE` la fuerza. No cambia ningún valor medido.

## Memoria del procesamiento

[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
no estima la memoria del procesamiento en R, y lo declara:

``` r

memoria <- attr(plan, "memoria_procesamiento")
memoria$estado
#> [1] "no_estimada"
writeLines(strwrap(memoria$motivo, 88))
#> no escala de forma predecible con las filas ni con las celdas; se midio.
```

El atributo conserva la magnitud conocida del trabajo —filas, celdas y
pares de texto—, rotulada como magnitud y no como consumo, y referencias
de otras corridas, que son textos fijos del código:

``` r

names(memoria)
#> [1] "estado"            "estimada"          "motivo"            "magnitud"         
#> [5] "referencias"       "distincion"        "reparto_observado"
names(memoria$referencias)
#> [1] "traer"     "procesar"  "variacion"
```

Esas referencias —cuánto costó traer las filas, cuánto procesarlas en R
y cuánto varió eso entre tablas— vienen de una corrida única, fechada el
2026-08-28, contra un motor de producción remoto que este repositorio no
puede rehacer. No son una predicción para la tabla del plan: la
variación entre tablas de la misma magnitud es justamente el motivo por
el que no se usan para estimar. `print(plan)` las muestra, y las cifras
con su fuente están en [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

Ver todas las filas y tenerlas todas en memoria no es lo mismo. Tres
recetas, según lo que se necesite:

| necesidad | configuración |
|----|----|
| agregados acotados, sin filas en R | `universo = "muestra_motor"`, `muestra_motor = n`, `bloque_muestra = "solo_agregados"` |
| hallazgos por fila | `muestra = n`, con `orden_muestra` si se necesita reproducir, y los topes de celdas y bytes |
| resumen de la tabla completa sin retenerla | `bloque_filas = n` |

## Consistencia entre consultas

Los agregados son sentencias separadas, y la tabla puede cambiar entre
ellas. El resumen lo publica:

``` r

por_omision$resumen_tabla$meta$snapshot
#> [1] FALSE
writeLines(strwrap(por_omision$resumen_tabla$meta$nota_snapshot, 88))
#> No hubo lectura instantanea: cada agregado se midio en su propio momento y la tabla
#> pudo cambiar entre consultas. La cobertura declara las comparaciones incoherentes entre
#> grupos de consistencia distintos.
```

Las cotas duras se comprueban sólo dentro de una sentencia: la
frecuencia de la moda contra su conteo de control, los distintos contra
el `n_validos_guard` de su misma consulta. Si `n_validos` y
`n_distintos` exactos son incoherentes y vienen de sentencias distintas,
la cobertura suma una fila `alcance_distinto` con las dos sentencias en
el motivo. Es evidencia de que la tabla cambió durante la corrida, no
una acusación contra el motor ni contra el paquete. Si una aproximación
no puede traer el conteo de control, la comprobación queda declarada
como no disponible.

Con `universo = "muestra_motor"` y `bloque_muestra = "con_muestra"`, la
selección del motor se ejecuta **una sola vez** y se guarda en una copia
local: un archivo temporal de la sesión de R (el *spool*). Todo el
perfil lee esa copia; nunca vuelve a muestrear el motor. Un registro de
cierre verifica `muestra_id`, `snapshot_id`, `orden_id`, `n_filas`,
`bytes` y la suma de control en cada relectura; un registro ausente
publica `spool_incompleto:trailer_ausente` y una suma que no coincide,
`spool_checksum_invalido`. La copia no escribe en la conexión ni crea
objetos temporales en el motor:

``` r

copia <- del_motor$resumen_tabla$meta$materializacion
str(copia[c("backend", "estado", "externo", "objetos_temporales_motor",
            "validado_relectura")])
#> List of 5
#>  $ backend                 : chr "spool_sesion_cliente"
#>  $ estado                  : chr "cerrada_verificada"
#>  $ externo                 : logi TRUE
#>  $ objetos_temporales_motor: logi FALSE
#>  $ validado_relectura      : logi TRUE
```

Releer la copia puede costar más que volver a pedir la selección al
motor en cada pasada; se elige porque garantiza que todas las métricas
describan las mismas filas. `snapshot_id` identifica la copia del
cliente, no una instantánea transaccional del motor.

Para una lectura consistente, la vía se puede correr dentro de una
transacción abierta por el usuario, con el nivel de aislamiento que el
motor necesite. El paquete nunca manda `BEGIN`, `COMMIT` ni `ROLLBACK`;
fuera de una transacción no agrega sentencias de ese tipo. Lo mismo vale
para
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md),
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
y
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).

| motor | dentro de una transacción |
|----|----|
| PostgreSQL | un error aborta la transacción, así que cada sentencia va entre un `SAVEPOINT` propio y su `RELEASE`, con `ROLLBACK TO SAVEPOINT` si falla |
| SQL Server | puede deshacer la transacción ante un error; la vía mide `@@TRANCOUNT` y `XACT_STATE()` antes y después de cada envío y lectura, y si cambian levanta `lupa_error_transaccion_dbi` |
| MySQL, MariaDB, SQLite | el error de una consulta no aborta la transacción |
| DuckDB | un error al preparar la aborta y no hay `SAVEPOINT`: una vista que falla deja la transacción abortada, y hay que cerrarla con `ROLLBACK` |

## Dialectos y sondas

Acotar filas no es SQL estándar. Antes de emitir los agregados,
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
sondea el motor con una consulta de cero filas y elige la primera
capacidad que acepta:

| dialecto | forma | motores |
|:---|:---|:---|
| `"limit"` | LIMIT n \[OFFSET k\] | PostgreSQL, SQLite, MySQL/MariaDB, DuckDB y compatibles |
| `"top"` | TOP (n), y OFFSET k ROWS FETCH NEXT n ROWS ONLY con orden | SQL Server 2012 o posterior, Sybase |
| `"fetch_first"` | OFFSET k ROWS FETCH FIRST n ROWS ONLY | Oracle 12c o posterior, DB2, Derby, H2 |
| `"rownum"` | ROWNUM \<= n; no expresa salto | Oracle anterior a 12c |
| `"portable"` | sin clausula de limite: se acota con dbSendQuery() y dbFetch(n) | cualquier motor con DBI |

``` r

dialecto <- por_omision$resumen_tabla$meta$dialecto
str(dialecto[c("nombre", "declarado")])
#> List of 2
#>  $ nombre   : chr "limit"
#>  $ declarado: logi FALSE
writeLines(strwrap(dialecto$sondas, 88))
#> SELECT `id_tramite`, `categoria`, `monto`, `edad` FROM `tramites` WHERE 1 = 0 LIMIT 1
```

Si ninguna capacidad sirve, queda la vía portable: acota en el cliente
con `dbSendQuery()` y `dbFetch(n)`, y la mediana exacta se declara no
disponible en vez de traer media tabla a memoria. `dialecto =` declara
la capacidad sin sondear, si la sonda no acierta:

``` r

portable <- perfilar_dbi(con, "tramites", dialecto = "portable",
                         bloque_muestra = "solo_agregados")
s <- sql_perfil(portable)
writeLines(strwrap(s$motivo[s$columna == "monto" & s$metrica == "mediana"], 88))
#> El dialecto `portable` no expresa un salto de filas; la mediana exacta exigiria traer
#> 1001 filas a memoria.
```

Cada sonda prueba la misma forma que después se emite. Una forma más
barata no alcanza, porque un motor puede aceptar una cláusula con un
filtro trivialmente falso y rechazarla sin él. DuckDB lo hace con el
muestreo por bloques:

``` r

probar <- function(sql) {
  tryCatch({ DBI::dbGetQuery(duck, sql); "aceptada" },
           error = function(e) "rechazada")
}
c(
  con_filtro_falso = probar("SELECT 1 FROM t TABLESAMPLE SYSTEM (10) WHERE 1 = 0"),
  sin_filtro = probar("SELECT 1 FROM t TABLESAMPLE SYSTEM (10)")
)
#> con_filtro_falso       sin_filtro 
#>       "aceptada"       "aceptada"
```

Otras reglas del dialecto:

- El alias de subconsulta se escribe con `AS` o sin él según el motor;
  los alias de columna van entre comillas y se comparan sin distinguir
  mayúsculas, porque hay motores que los pliegan.
- La mediana bajo `muestra_motor`, en dialectos sin forma consolidada ni
  escalar, usa una sola CTE con `COUNT(*) OVER ()` y
  `ROW_NUMBER() OVER`. Su sonda repite esa construcción sobre
  `VALUES (1.0), (2.0), (3.0), (4.0)`, exige `2.5` y un control negativo
  que falle; no se sondea la tabla de producción.
- Cuando el controlador ya fue reconocido, no se ejecutan las sondas de
  sintaxis de otros motores: en SQL Server con `XACT_ABORT ON`, una
  sentencia que nunca correspondía abortaría la transacción.

## Lo que el motor guarda distinto que R

El resumen SQL describe lo que el motor tiene delante, y a veces no es
lo mismo que la tabla en R. Los casos conocidos, con lo que publica el
paquete (el detalle completo está en la sección «Lo que el motor guarda
distinto que R» de
[`?perfilar_dbi`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)):

| motor | qué pasa | qué publica `lupa` |
|----|----|----|
| SQLite | no tiene tipo fecha: un `Date` se guarda como días desde 1970 | sin más información lo describe como número; con `extended_types = TRUE` al conectar, la columna es temporal y no recibe agregados cuantitativos |
| SQLite | una columna de afinidad numérica admite texto, y RSQLite lo lee como `0` | cuenta con [`typeof()`](https://rdrr.io/r/base/typeof.html) los valores guardados como texto o blob y declara la divergencia con esa causa |
| DuckDB | `NaN` es un valor: cuenta como presente y distinto, y `MAX` o `AVG` responden `NaN` | una métrica respondida con `NaN` queda `no_disponible` con su motivo |
| SQLite, PostgreSQL, DuckDB | `Inf` y `-Inf` entran en `MIN`, `MAX`, `AVG` y la mediana | los estadísticos de magnitud se vuelven a pedir sobre los valores finitos, como en [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) |
| PostgreSQL | RPostgres lee `0` en toda celda `money`, y un `real` por su texto | las dos se leen convertidas en el motor; la conversión queda en `meta$lectura_convertida` |
| PostgreSQL 9.3 a 11, o `extra_float_digits <= 0` | el texto de un `double precision` tiene menos cifras | la lectura decodifica los ocho bytes del valor con `float8send()` |
| SQL Server | `datetimeoffset` puede llegar convertido a UTC por FreeTDS | el desplazamiento perdido no se reconstruye; la corroboración lo deja como divergencia |
| SQL Server | `STDEV` acumula en una pasada y pierde cifras con medias grandes | el desvío se pide sobre los valores centrados en la media |
| SQL Server | no admite `COUNT(DISTINCT)`, `GROUP BY`, `ORDER BY` ni `MIN`/`MAX` sobre `text`, `ntext`, `image`, `xml` y `geography` | esas métricas quedan `no_disponible` sin enviar la sentencia |
| cualquiera | el cotejamiento de la columna agrupa valores que R distingue (`COLLATE NOCASE`, ICU no determinista, espacios finales en SQL Server) | `meta$criterio_comparacion` y `meta$criterio_moda` lo declaran; la corroboración lo deja `no_corroborada`, no `divergencia` |
| cualquiera | enteros de 64 bits o decimales que llegan como doble desde 2^53 | las siete métricas de magnitud quedan `no_disponible`; la muestra lee esas columnas como texto y, con `bit64`, como `integer64` |
| cualquiera | cada motor suma a su modo, y con valores que se cancelan las medias difieren entre motores y con la de R | la media se pide como doble donde la aritmética decimal la recortaría; la corroboración declara la diferencia |

La media de `{1e16, 1, -1e16}` es 1/3. Cada vía la calcula a su modo, y
ninguna se toma por verdadera:

``` r

cancelan <- data.frame(x = c(1e16, 1, -1e16))
DBI::dbWriteTable(con, "cancelan", cancelan)
DBI::dbWriteTable(duck, "cancelan", cancelan)
c(
  sqlite = DBI::dbGetQuery(con, "SELECT AVG(x) FROM cancelan")[[1]],
  duckdb = DBI::dbGetQuery(duck, "SELECT AVG(x) FROM cancelan")[[1]],
  r = mean(cancelan$x)
)
#>    sqlite    duckdb         r 
#> 0.3333333 0.0000000 0.3336589
```

Cuando los dos bloques existen y difieren más allá de la tolerancia, la
corroboración deja la fila `divergencia` con los dos valores.

En SQLite, la diferencia de las fechas se ve así:

``` r

fechas <- data.frame(f = as.Date("2019-03-01") + 0:9)
perfilar_fechas <- function(...) {
  base <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", ...)
  on.exit(DBI::dbDisconnect(base))
  DBI::dbWriteTable(base, "fechas", fechas)
  x <- perfilar_dbi(base, "fechas", bloque_muestra = "solo_agregados",
                    proteger_datos_personales = FALSE)
  columnas(x)[, c("columna", "minimo", "maximo", "media")]
}
perfilar_fechas()
#>   columna minimo maximo   media
#> 1       f  17956  17965 17960.5
perfilar_fechas(extended_types = TRUE)
#>   columna minimo maximo media
#> 1       f     NA     NA    NA
```

## La clave primaria según el motor

La clave primaria no se adivina: se lee del catálogo del motor, en una
consulta de metadatos que no recorre la tabla, en todas las corridas y
con cualquier política de costo. La respuesta queda en
`resumen_tabla$meta$clave`, con `columnas`, `fuente`, `motivo`,
`garantia` y `estado`. `garantia` vale `garantizada`,
`declarada_no_garantizada`, `desconocida` o `no_declarada`. Una consulta
fallida conserva su motivo y no se confunde con una tabla sin clave.

Con un nombre sin calificar, la clave que se publica es la de la
relación que el motor resuelve, con sus mismas reglas de esquema. Una
clave cuyas columnas no están entre las que se acaban de medir se
descarta entera, en cualquier motor, diciendo por qué.

Qué puede afirmar cada catálogo:

| motor | vía de catálogo | `garantizada` cuando |
|----|----|----|
| PostgreSQL | `pg_catalog` | la restricción está aplicada y validada, sin tablas hijas por herencia (las particiones no cuentan), no es `DEFERRABLE` y su índice es único; si no, `declarada_no_garantizada` |
| MySQL | `information_schema` | `ENFORCED` vale `YES` |
| Oracle | `all_constraints` | `STATUS` es `ENABLED`, está `VALIDATED` y no es `DEFERRABLE` (probado con un motor simulado) |
| SQLite | `pragma` | todas las columnas de la clave son `NOT NULL`; si no, la unicidad entre valores no nulos está garantizada pero la garantía queda `desconocida` |
| MariaDB | `show_index` | nunca: el catálogo no expone un estado comparable y la garantía queda `desconocida` |
| SQL Server | `information_schema` | nunca: queda `desconocida` |
| DuckDB | `duckdb_constraints` | nunca: queda `desconocida` |

``` r

crear <- function(conexion, sql) invisible(DBI::dbExecute(conexion, sql))
crear(con, "CREATE TABLE con_nulos (id INTEGER PRIMARY KEY, x TEXT)")
crear(con, "CREATE TABLE sin_nulos (id INTEGER NOT NULL PRIMARY KEY, x TEXT)")
crear(con, "CREATE TABLE sin_clave (id INTEGER, x TEXT)")
crear(duck, "CREATE TABLE padron (id INTEGER PRIMARY KEY, x VARCHAR)")
leer_clave <- function(conexion, tabla) {
  x <- perfilar_dbi(conexion, tabla, bloque_muestra = "solo_agregados")
  k <- x$resumen_tabla$meta$clave
  data.frame(tabla = tabla, columnas = paste(k$columnas, collapse = ", "),
             fuente = k$fuente, garantia = k$garantia)
}
rbind(
  leer_clave(con, "con_nulos"),
  leer_clave(con, "sin_nulos"),
  leer_clave(con, "sin_clave"),
  leer_clave(duck, "padron")
)
#>       tabla columnas             fuente     garantia
#> 1 con_nulos       id             pragma  desconocida
#> 2 sin_nulos       id             pragma  garantizada
#> 3 sin_clave                      pragma no_declarada
#> 4    padron       id duckdb_constraints  desconocida
```

Con `politica_costo = "por_cardinalidad"`, una clave garantizada dice
cuántos valores distintos tiene su columna sin contarlos. La
comprobación de una clave declarada por el usuario sobre datos en
memoria (`perfilar(clave = ...)`) y las sugerencias de clave están en
[Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md).

## Motores probados

La matriz se arma al construir la página con la salida de
`benchmark/verificar_motor.R`, el archivo
`benchmark/datos/verificar-motor.csv`. Cada fila es un motor de una
misma corrida: el motor, la versión que informa el servidor, el
controlador de R y su versión, y el resultado de cada comprobación
(`ok`, o `fallo` con el mensaje). Debajo van la fecha de la corrida, la
revisión del paquete medida y el fixture. Un motor que no respondió
queda `no_medido` con su motivo.

| motor | servidor | controlador | estado | cinco columnas | dialecto | media | clave primaria | cobertura |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| PostgreSQL 16 | 16.15 | RPostgres 1.4.10 | `medido` | ok | ok | ok | ok | ok |
| PostgreSQL 9.3 | 9.3.25 | RPostgres 1.4.10 | `medido` | ok | ok | ok | ok | ok |
| MySQL 8.4 | 8.4.11 | RMySQL 0.11.3 | `medido` | ok | ok | ok | ok | ok |
| MariaDB 11.8 | 11.8.8 | RMariaDB 1.3.5 | `medido` | ok | ok | ok | ok | ok |
| SQL Server 2022 | 16.0.4265.3 | odbc/FreeTDS 1.7.0 | `medido` | ok | ok | ok | ok | ok |
| SQLite | 3.53.3 | RSQLite 3.53.3 | `medido` | ok | ok | ok | ok | ok |
| DuckDB | 1.5.5 | duckdb 1.5.5 | `medido` | ok | ok | ok | ok | ok |

Corrida del 2026-10-24T13:12:59Z (UTC), sobre la revisión
`0.1.0; R/ md5 08c0a1af7ccb` del paquete. Fixture declarado por el
guion: «tramites: cinco columnas; padron: clave primaria id_persona».

Qué hace `benchmark/verificar_motor.R`:

- Se corre desde la raíz del código fuente,
  `Rscript benchmark/verificar_motor.R [motores]`. Sin argumentos
  recorre los siete motores de la matriz (`postgres16`, `postgres93`,
  `mysql84`, `mariadb118`, `sqlserver2022`, `sqlite` y `duckdb`); con
  argumentos, sólo los nombrados.
- Carga el paquete desde la fuente con `pkgload` cuando está disponible,
  así que mide el código del árbol y no el paquete instalado.
- No recibe una conexión cualquiera. Cada motor tiene su conexión
  escrita en el guion —SQLite y DuckDB en memoria; los demás,
  contenedores locales en puertos propios— y las claves de MySQL,
  MariaDB y SQL Server se leen de `LUPA_MYSQL_PWD`, `LUPA_MARIADB_PWD` y
  `LUPA_MSSQL_PWD`. Para otro servidor u otro motor hay que agregar su
  conexión al guion o repetir las comprobaciones a mano.
- Escribe en cada motor el fixture: `tramites`, de 500 filas y cinco
  columnas (identificador, documento, categoría, monto y edad con 20
  ausentes), y `padron`, con clave primaria `id_persona` declarada
  `NOT NULL`. Perfila `tramites` con la llamada que sigue a esta lista.
- Hace cinco comprobaciones: que el resumen tenga cinco columnas; que el
  dialecto se haya resuelto; que la media de `monto` calculada por el
  motor coincida con la de R sobre la misma columna, con tolerancia
  relativa de `1e-8`; que la clave primaria de `padron` se lea del
  catálogo como `id_persona`; y que `resumen_tabla$cobertura` sea una
  tabla.
- Termina con código distinto de cero si alguna comprobación falla.

``` r

perfilar_dbi(
  con, "tramites", muestra = 500L, analizar_dependencias = FALSE,
  duplicados_aproximados = FALSE, proteger_datos_personales = FALSE
)
```

No comprueba cuántas métricas se calculan ni cuántas quedan `no_aplica`,
ni cronometra. La comprobación de la media vale para valores finitos y
de escala ordinaria, como los del fixture: con `NaN`, infinitos o sumas
que pierden precisión, el motor y R no coinciden, y eso lo declara la
corroboración entre bloques ([arriba](#no-traducido)).

Lo que no está en la corrida:

| caso | estado | fuente |
|----|----|----|
| motor que rechaza `LIMIT` (el caso de SQL Server, cuya matriz real no ejerce ese rechazo) | probado con un motor simulado | `tests/testthat/test-arreglos-dbi.R` |
| motor que pliega los alias a mayúsculas, como Oracle, DB2, Firebird o Snowflake | probado con un motor simulado | `tests/testthat/test-arreglos-dbi.R` |
| motor que rechaza una columna (un LOB que el controlador no materializa, un permiso por columna) | probado con un motor simulado | `tests/testthat/test-arreglos-dbi.R` |
| Oracle (`fetch_first`, `rownum`, `SAMPLE`) | probado con un motor simulado; sin corrida contra el motor real | `tests/testthat/test-dialecto-oracle.R` |
| BigQuery | sin probar | — |
| otro motor compatible con DBI | sin probar; corre por la vía portable | — |

El motor simulado es una subclase de la conexión de RSQLite que
intercepta las consultas y se porta como el motor que imita. Un motor
simulado prueba esa restricción, no el motor: con otro motor, otro
controlador u otra versión, la función corre y declara en la cobertura
lo que no pudo medir, pero sus cifras no están comprobadas contra ese
motor.

## Fallos parciales y errores

Ningún bloque descarta al otro:

- Si se pide la muestra y el motor rechaza su consulta, o la muestra no
  se puede perfilar, el resultado sale con `resumen_tabla` completo,
  `perfil_muestra = NULL` y una fila `no_disponible` en la cobertura,
  con el motivo y cómo resolverlo. Si no se pidió, la fila dice
  `no_solicitado`, que no es un fallo.
- Si el motor rechaza una columna, sus métricas quedan `no_disponible` y
  las demás se perfilan enteras. Si la lectura de la muestra falla, la
  columna culpable se busca leyendo las mismas filas con menos columnas,
  y una fila `alcance_distinto` la nombra.
- Una columna que la credencial no puede leer —un permiso por columna—
  se reconoce antes de leer, por el catálogo (`has_column_privilege()`
  en PostgreSQL, `HAS_PERMS_BY_NAME()` en SQL Server): aparece con sus
  métricas en `no_disponible` y no se nombra en ninguna consulta.
- Con enmascaramiento dinámico de SQL Server y una credencial sin
  `UNMASK`, la completitud se publica y lo que depende del valor queda
  `no_disponible`.
- Si una vista falla en una fila bajo `muestra_motor` o `bloque_filas`,
  la vía intenta el mismo resumen sobre la tabla completa y declara la
  muestra o el recorrido como `no_disponible`.
- Con odbc, bajo un locale que no es UTF-8, un nombre de columna con
  tilde no se envía: la columna queda `no_disponible`. Los detalles de
  codificación están en [Codificación y
  locales](https://sebollin.github.io/lupa/articles/codificacion-y-locales.html).

Los errores de la vía llevan clase propia, todas herederas de
`lupa_error_dbi`, para rescatarlos con
[`tryCatch()`](https://rdrr.io/r/base/conditions.html):
`lupa_error_conexion_dbi`, `lupa_error_tabla_dbi`,
`lupa_error_campos_dbi`, `lupa_error_conteo_dbi`,
`lupa_error_esquema_dbi`, `lupa_error_codificacion_dbi`,
`lupa_error_argumento_dbi` y `lupa_error_transaccion_dbi`.

``` r

error <- tryCatch(perfilar_dbi(con, "no_existe"), error = function(e) e)
class(error)
#> [1] "lupa_error_tabla_dbi" "lupa_error_dbi"       "error"               
#> [4] "condition"
```

## Dependencias e instalación

`lupa` tiene dos dependencias obligatorias, `cli` y `data.table`, y no
importa ninguna a su espacio de nombres: las llama con `::`. Es
deliberado: `data.table` decide el significado de `[` según si el
paquete que llama lo importa, y con el import puesto
`tabla[, columnas, drop = FALSE]` dejaría de seleccionar columnas en las
funciones que reciben una tabla del usuario.

`data.table` se usa para tres cosas:

- **Contar filas duplicadas exactas.** Desde 25 filas, el conteo usa
  [`data.table::frank()`](https://rdrr.io/pkg/data.table/man/frank.html)
  con rangos densos. Repliega a
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html) de base, que
  es la que fija el resultado, si hay columnas de lista o compuestas,
  `NaN` en columnas dobles, un método
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html) propio, o si
  el redondeo numérico de `data.table` de la sesión no es exacto
  (`setNumericRounding()`).
- **Copiar entradas `data.table` antes de seleccionar columnas.** Una
  tabla de esa clase se copia a un `data.frame` con
  [`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html),
  así el `[` que corre es el de base.
- **Devolver un `data.table` usable en la remediación.**
  [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  copia la entrada, conserva la clave (`setkeyv()`) y restituye la
  reserva de columnas (`setalloccol()`), para que el resultado siga
  siendo un `data.table` sano.

Los sugeridos son `bit64`, `covr`, `DBI`, `duckdb`, `jsonlite`, `knitr`,
`odbc`, `rmarkdown`, `RPostgres`, `RSQLite`, `sf`, `stringdist`,
`stringi`, `testthat`, `tibble` y `units`. Para la vía DBI importan
`DBI`, el controlador de cada motor (`RSQLite`, `duckdb`, `RPostgres`,
`odbc`) y `bit64`, que permite leer en la muestra enteros de 64 bits sin
perder valores. Los controladores de otros motores (`RMariaDB`,
`RMySQL`, `ROracle`, `bigrquery`) se instalan aparte.

El caso difícil no es el paquete de R sino la biblioteca del sistema que
va debajo: `RMariaDB` no compila sin las cabeceras del cliente de MySQL
o MariaDB, y `ROracle` necesita el Instant Client de Oracle.
[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
lo dice antes de chocarse. Su catálogo tiene 12 entradas: 9 de motores
con nombre, contando por separado las dos versiones de Oracle, y tres
genéricas (`dbi`, `odbc` y `otro_dbi`), que no son motores medidos.

``` r

r <- requisitos_motor()
knitr::kable(r[, c("id_motor", "motor", "version", "paquete_r", "dialecto",
                   "estado_prueba")], row.names = FALSE)
```

| id_motor | motor | version | paquete_r | dialecto | estado_prueba |
|:---|:---|:---|:---|:---|:---|
| dbi | DBI | — | DBI | — | no_aplica |
| sqlite | SQLite | — | RSQLite | limit | probado |
| postgresql | PostgreSQL | 16 | RPostgres | limit | probado |
| mysql | MySQL | 8 | RMySQL | limit | probado |
| sql_server | SQL Server | 2022 | odbc | top | probado |
| duckdb | DuckDB | 1.5 | duckdb | limit | probado |
| mariadb | MariaDB | 11 | RMariaDB | limit | probado |
| oracle_12c | Oracle | Free 23 (12c+) | ROracle | fetch_first | esperado |
| oracle_11 | Oracle | 11 y anteriores | ROracle | rownum | esperado |
| bigquery | BigQuery | — | bigrquery | portable | no_documentado |
| odbc | ODBC | — | odbc | portable | no_documentado |
| otro_dbi | Otro compatible con DBI | — | — | portable | reserva |

Cada entrada declara además la biblioteca del sistema con su paquete en
Debian y en Fedora, y la salida sin permisos de administrador cuando
existe:

``` r

writeLines(strwrap(requisitos_motor("sql_server")$alternativa_sin_administrador, 88))
#> Compilar FreeTDS en un prefijo del usuario y pasar la ruta directa del controlador en
#> la cadena de conexión; no hace falta registrar un DSN.
```

[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
no afirma haber comprobado una biblioteca del sistema que no puede
comprobar: en ese caso su estado es `no_comprobada`. `estado_prueba` es
una declaración del catálogo, con su fuente en `fuente_estado`:
`esperado` significa que el dialecto está construido y probado contra un
motor simulado que reproduce su restricción, no contra el motor real. La
evidencia contra cada motor es la corrida de [Motores
probados](#motores-probados). Las columnas que dependen de la máquina
—si el paquete de R está instalado— se calculan al llamar y no se
muestran aquí.

## Hilos

El trabajo que se puede paralelizar usa **dos hilos por omisión**
(`getOption("lupa.nucleos", 2L)`), que es el tope que CRAN pide
respetar. Se sube por llamada con `nucleos = 8` o para toda la sesión
con `options(lupa.nucleos = 8)`; el valor se limita a los núcleos
disponibles y queda publicado en `alcance$nucleos_usados`. El resultado
no cambia, sólo cuánto tarda.

Los hilos gobiernan las comparaciones de texto que se delegan en
`stringdist`: las de
[`detectar_duplicados_aproximados()`](https://sebollin.github.io/lupa/reference/detectar_duplicados_aproximados.md),
[`estimar_costo()`](https://sebollin.github.io/lupa/reference/estimar_costo.md)
y [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
cuando se piden duplicados aproximados, también para `perfil_muestra` a
través de `...`. El conteo exacto de filas duplicadas usa la
configuración de hilos propia de `data.table`
([`data.table::setDTthreads()`](https://rdrr.io/pkg/data.table/man/openmp-utils.html)),
que el paquete no cambia. Cómo elegir la cantidad de hilos se explica en
[Escala y
duplicados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md),
y los tiempos medidos por cantidad de hilos, con su fecha y su entorno,
están en [Evidencia y
rendimiento](https://sebollin.github.io/lupa/articles/evidencia-y-rendimiento.html).

# 10. Perfilar una base

Hay una tabla de ventas en una base de datos, quizá demasiado grande
para la memoria de la sesión. ¿Cómo se decide qué calcula el motor,
cuántas filas llegan a R y dónde queda escrito cómo se eligieron?

> **En esta guía:**
>
> - perfilar una tabla con
>   [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
>   y separar sus 2 bloques, `resumen_tabla` y `perfil_muestra`;
> - comparar qué trae a R cada opción: la llamada por omisión,
>   `muestra = n`, `universo = "muestra_motor"` y
>   `bloque_muestra = "solo_agregados"`;
> - encontrar el metadato del muestreo, que vive en 2 lugares según la
>   opción;
> - leer el resultado con los mismos accesores que un perfil en memoria;
> - contar fechas centinela, períodos y actualidad, en memoria y en el
>   motor.

## Una tabla en una base

El ejemplo usa SQLite en memoria, con los paquetes `DBI` y `RSQLite`. La
tabla tiene 1.000 filas para que la guía corra en segundos; las
decisiones son las mismas con millones.
[`DBI::dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
sólo prepara el ejemplo.
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
emite consultas de lectura: no escribe en la base ni crea tablas
temporales.

``` r

library(lupa)

n <- 1000
ventas <- data.frame(
  id_venta = seq_len(n),
  sucursal = rep(c("Norte", "Sur", "Este", "Oeste"), length.out = n),
  monto = 100 + (seq_len(n) * 37) %% 800,
  fecha = format(as.Date("2026-01-01") + seq_len(n) %% 180),
  stringsAsFactors = FALSE
)
ventas$monto[seq(7, n, by = 50)] <- NA
head(ventas, 3)
#>   id_venta sucursal monto      fecha
#> 1        1    Norte   137 2026-01-02
#> 2        2      Sur   174 2026-01-03
#> 3        3     Este   211 2026-01-04
```

``` r

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conexion, "ventas", ventas)
```

## Perfilar con los valores por omisión

``` r

completo <- perfilar_dbi(conexion, "ventas")
completo
#> Perfil DBI de ventas
#> Resumen de tabla completa: 4 columnas sobre 1000 filas
#> Clave primaria: no declara clave (fuente `pragma`).
#> Estabilidad de la tabla: sin cambios en los controles verificados (conteo de filas
#> (COUNT(*)); no descarta cambios fuera de esos controles).
#> Duplicados exactos: 0 excedente(s) (calculado).
#> Métricas: calculado 45, no_aplica 15
#> Consultas emitidas: 23 (dialecto limit)
#> Derrame estimado: no se pudo estimar (No se pudo estimar el derrame: la conexión no fue
#> reconocida como PostgreSQL y no hay un `pg_stats` portable.).
#> Derrame: no disponible (El controlador no fue reconocido como PostgreSQL; no se puede
#> consultar una estadística de bloques temporales portable.).
#> Perfil de muestra: 1000 filas, reproducible: FALSE
#> Diagnósticos no evaluados sobre la muestra: 3, en `perfil_muestra$cobertura_diagnosticos`
#> No se imprime ningún valor de celda: están en `resumen_tabla$columnas`.
```

La impresión muestra los 2 bloques:

- «Resumen de tabla completa» es `resumen_tabla`: agregados que el motor
  calcula en SQL sobre todas las filas.
- «Perfil de muestra» es `perfil_muestra`:
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  corrido en R sobre las filas que se leyeron.

Las líneas de derrame sólo tienen contenido en PostgreSQL.

Por omisión, `universo = "tabla_completa"` y `muestra = Inf`. Eso quiere
decir:

- los agregados recorren la tabla entera;
- `lupa` trae además a R **todas las filas que entran en los topes de
  lectura**. Los topes son `max_celdas_muestra` (1.000.000 celdas por
  omisión) y `max_bytes_muestra` (512 MiB).

Con esta tabla entran las 1.000 filas. La llamada por omisión no es una
muestra barata: es la tabla completa, si cabe.

## Qué trae a R cada opción

Las otras 3 decisiones se prueban sobre la misma tabla. El resultado
tiene una fila por opción, con:

- cuántas filas describe el resumen SQL;
- cuántas se leyeron a R;
- en cuál de los 2 lugares quedó el metadato del muestreo.

``` r

acotado <- perfilar_dbi(conexion, "ventas", muestra = 200, orden_muestra = "id_venta")
en_motor <- perfilar_dbi(
  conexion, "ventas", universo = "muestra_motor", muestra_motor = 150
)
agregados <- perfilar_dbi(conexion, "ventas", bloque_muestra = "solo_agregados")

que_trae <- function(perfil, opcion) {
  muestreo_sql <- perfil$resumen_tabla$meta$muestreo
  muestreo_r <- perfil$perfil_muestra$meta$origen_dbi$muestreo
  data.frame(
    opcion = opcion,
    filas_resumen = if (is.null(muestreo_sql)) {
      n_filas(perfil)
    } else {
      muestreo_sql$filas_obtenidas
    },
    filas_en_r = if (is.null(muestreo_r)) 0 else muestreo_r$filas_obtenidas,
    meta_resumen = !is.null(muestreo_sql),
    meta_lectura = !is.null(muestreo_r)
  )
}

rbind(
  que_trae(completo, "por omisión"),
  que_trae(acotado, "muestra = 200"),
  que_trae(en_motor, "universo = \"muestra_motor\""),
  que_trae(agregados, "bloque_muestra = \"solo_agregados\"")
)
#>                              opcion filas_resumen filas_en_r meta_resumen meta_lectura
#> 1                       por omisión          1000       1000        FALSE         TRUE
#> 2                     muestra = 200          1000        200        FALSE         TRUE
#> 3        universo = "muestra_motor"           150        150         TRUE         TRUE
#> 4 bloque_muestra = "solo_agregados"          1000          0        FALSE        FALSE
```

Cada fila contesta una pregunta distinta:

- **`muestra = n`** acota la lectura a R, no los agregados: el resumen
  sigue describiendo las 1.000 filas, y el perfil por fila, 200.
- **`universo = "muestra_motor"`** con `muestra_motor = n` cambia el
  universo de los agregados. El motor elige la muestra una sola vez y
  `lupa` la guarda en una copia de la sesión de R. Todo lo demás se
  calcula sobre esas mismas filas: resumen y perfil describen las mismas
  150.
- **`bloque_muestra = "solo_agregados"`** no trae ninguna fila:
  `perfil_muestra` queda en `NULL` y sólo hay conteos y resúmenes
  calculados en el motor. Vale para los 2 universos.

[`n_filas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve siempre el conteo de la tabla completa, también con
`muestra_motor`.

## Dónde queda escrito cómo se muestreó

Las 2 últimas columnas de la tabla de arriba dicen si existe cada lugar
donde vive el metadato del muestreo:

- `meta_resumen` mira `resumen_tabla$meta$muestreo`;
- `meta_lectura` mira `perfil_muestra$meta$origen_dbi$muestreo`.

Cada uno existe bajo su propia condición:

| ruta | describe | existe cuando |
|----|----|----|
| `perfil_muestra$meta$origen_dbi$muestreo` | las filas leídas a R | hay `perfil_muestra`, con cualquier universo |
| `resumen_tabla$meta$muestreo` | la muestra elegida por el motor | `universo = "muestra_motor"`; si no, es `NULL` |

Para la lectura a R, el metadato dice:

- cuántas filas se pidieron y cuántas llegaron;
- cuántas tiene la tabla;
- cómo se eligieron;
- si la elección se puede repetir.

``` r

lectura <- acotado$perfil_muestra$meta$origen_dbi$muestreo
str(lectura[c(
  "filas_solicitadas", "filas_obtenidas", "filas_totales_fuente", "metodo",
  "reproducible"
)])
#> List of 5
#>  $ filas_solicitadas   : num 200
#>  $ filas_obtenidas     : num 200
#>  $ filas_totales_fuente: num 1000
#>  $ metodo              : chr "primeras_filas_segun_orden"
#>  $ reproducible        : logi TRUE
```

`reproducible` es `TRUE` porque se declaró `orden_muestra = "id_venta"`
y `lupa` comprobó en la tabla completa que ese orden es único. Sin orden
declarado, un `LIMIT` no tiene orden garantizado. Entonces
`reproducible` vale `FALSE`, como en la llamada por omisión:

``` r

sin_orden <- completo$perfil_muestra$meta$origen_dbi$muestreo
sin_orden$reproducible
#> [1] FALSE
writeLines(strwrap(sin_orden$motivo_reproducibilidad, 88))
#> No se declaró `orden_muestra`; SQL no garantiza el orden de las filas.
```

Para la muestra del motor, el metadato está en el resumen. El método
depende de lo que el motor acepte. `lupa` prueba primero las variantes
de `TABLESAMPLE`. Si ninguna sirve, ordena por una función
pseudoaleatoria con el límite del dialecto, como en SQLite.

``` r

del_motor <- en_motor$resumen_tabla$meta$muestreo
str(del_motor[c("metodo", "tamano_muestra", "fraccion", "universo", "reproducible")])
#> List of 5
#>  $ metodo        : chr "random_limit"
#>  $ tamano_muestra: num 150
#>  $ fraccion      : num 0.15
#>  $ universo      : num 1000
#>  $ reproducible  : logi FALSE
```

Ahí `universo` es el tamaño de la tabla de la que salió la muestra y
`fraccion` la parte que se tomó. La muestra pseudoaleatoria cambia en
cada corrida, y `reproducible` lo dice. Cada métrica de
`resumen_tabla$sql` declara además su alcance: el conteo de filas es de
la tabla completa y el resto, de la muestra.

``` r

sql <- en_motor$resumen_tabla$sql
sql[sql$columna == "monto", c("columna", "metrica", "estado", "alcance")][1:6, ]
#>     columna        metrica            estado        alcance
#> 3     monto              n         calculado tabla_completa
#> 110   monto      n_validos observado_muestra        muestra
#> 22    monto    n_faltantes observado_muestra        muestra
#> 32    monto prop_faltantes observado_muestra        muestra
#> 42    monto    n_distintos observado_muestra        muestra
#> 52    monto tasa_distintos observado_muestra        muestra
```

El estado `observado_muestra` avisa que el valor describe la muestra. En
particular, `n_distintos` es la cardinalidad observada en esas 150
filas, no una estimación de la cardinalidad de la tabla.

## Los 2 bloques, uno al lado del otro

``` r

data.frame(
  bloque = c("resumen_tabla", "perfil_muestra"),
  calculado_en = c("el motor", "R"),
  filas = c(n_filas(acotado), n_filas(acotado$perfil_muestra)),
  campos_analiticos = c(
    ncol(acotado$resumen_tabla$columnas) - 1L,
    ncol(acotado$perfil_muestra$columnas) - 1L
  )
)
#>           bloque calculado_en filas campos_analiticos
#> 1  resumen_tabla     el motor  1000                16
#> 2 perfil_muestra            R   200               117
```

El resumen tiene 16 campos analíticos por columna. Son los que SQL puede
calcular sin traer los datos: faltantes, distintos, moda, mínimo,
máximo, media, mediana y desvío, entre otros. El perfil de la muestra
tiene los 117 de
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
pero sobre las filas leídas. Por eso `lupa` no los mezcla. Llamar
*perfil* al resumen afirmaría una completitud que no tiene; leer el
perfil como si fuera de la tabla entera, también.

## Leer el resultado con los accesores

Los accesores de [Leer un
perfil](https://sebollin.github.io/lupa/articles/leer-un-perfil.md)
funcionan igual sobre el resultado de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve el resumen del motor y
[`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md),
los del perfil de la muestra:

``` r

columnas(acotado)[, c("columna", "n", "n_faltantes", "n_distintos", "minimo", "maximo")]
#>    columna    n n_faltantes n_distintos minimo maximo
#> 1 id_venta 1000           0        1000      1   1000
#> 2 sucursal 1000           0           4     NA     NA
#> 3    monto 1000          20         784    100    899
#> 4    fecha 1000           0         180     NA     NA
hallazgos(acotado)[, c("columna", "tipo_hallazgo", "severidad", "n_afectados")]
#>    columna               tipo_hallazgo  severidad n_afectados
#> 1 id_venta       posible_identificador         ok           1
#> 2    fecha           alta_cardinalidad sospechoso           1
#> 3    fecha     tipo_declarado_distinto sospechoso           1
#> 4 sucursal casi_duplicados_vocabulario         ok           0
#> 5    fecha casi_duplicados_vocabulario         ok           0
nrow(sql_perfil(acotado))
#> [1] 64
```

El motor contó los 20 faltantes de `monto` en las 1.000 filas. En sus
200 filas, el perfil por fila encontró que:

- `fecha` está guardada como texto aunque contiene fechas
  (`tipo_declarado_distinto`);
- casi cada fila trae una fecha distinta (`alta_cardinalidad`).

[`sql_perfil()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve una fila por columna y métrica, con la consulta que la produjo,
su estado y su motivo.

Cuando no se pidió la muestra, los accesores no inventan lo que no hay:

- [`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  devuelve una tabla vacía con un aviso;
- [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  declara el bloque como `no_solicitado`, que no es un fallo.

``` r

aviso <- NULL
sin_filas <- withCallingHandlers(
  hallazgos(agregados),
  warning = function(w) {
    aviso <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  }
)
nrow(sin_filas)
#> [1] 0
writeLines(strwrap(aviso, 88))
#> Este perfil DBI no solicitó la muestra, así que no hay hallazgos por fila. El resumen
#> por columna sigue disponible en columnas(); el detalle, en cobertura().
cobertura(agregados)[, c("bloque", "elemento", "estado")]
#>           bloque elemento        estado
#> 1 perfil_muestra   ventas no_solicitado
```

## 📅 Fechas centinela, períodos y actualidad

Una fecha puede estar ahí para decir otra cosa: `1900-01-01` por «no se
sabe», `9999-12-31` por «sigue vigente».
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
sospecha por omisión de 4 (1900-01-01, 9999-12-31, 1970-01-01 y
1899-12-30), y sólo si se repiten y quedan separadas del resto de las
fechas. Tres argumentos dicen más, y valen igual en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
en
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):

- `centinelas_fecha` declara las fechas centinela de la base, que se
  acusan como `error` desde la primera aparición;
- `periodos` declara qué par de columnas es el inicio y el fin de un
  período y, con `fin_abierto`, qué fecha de fin quiere decir «vigente».
  Nunca se adivinan por el nombre;
- `resumen_fechas = TRUE` agrega las cuentas de fechas centinela y
  futuras de cada columna de fecha, y su actualidad: el último dato y
  cuántos días hace.

El ejemplo es una tabla de contratos. Los vigentes terminan en
`9999-12-31`, a 2 les falta el fin y uno termina antes de empezar. En
`alta` hay 3 veces `1900-01-01`, una vez `2000-01-01`, que en esta base
también quiere decir «sin dato», y una fecha futura. La fecha de la
corrida se fija para que el ejemplo dé siempre lo mismo.

``` r

dias <- as.Date("2025-01-01") + (1:60 * 11) %% 500
contratos <- data.frame(id_contrato = 1:60, alta = dias, desde = dias, hasta = dias + 90)
contratos$hasta[c(3, 9, 15, 21)] <- as.Date("9999-12-31")
contratos$hasta[c(4, 10)] <- NA
contratos$hasta[5] <- contratos$desde[5] - 10
contratos$alta[c(7, 14, 28)] <- as.Date("1900-01-01")
contratos$alta[33] <- as.Date("2000-01-01")
contratos$alta[40] <- as.Date("2027-06-01")

periodo <- list(inicio = "desde", fin = "hasta", fin_abierto = "9999-12-31")
en_memoria <- perfilar(
  contratos, fecha = "2026-06-30", centinelas_fecha = "2000-01-01",
  periodos = periodo, resumen_fechas = TRUE
)
campos_fecha <- c(
  "columna", "n_fechas_centinela", "n_fechas_centinela_declaradas",
  "n_fechas_fin_abierto", "n_fechas_futuras", "lectura_fechas_futuras",
  "ultimo_dato_fecha"
)
columnas(en_memoria)[-1, campos_fecha]
#>   columna n_fechas_centinela n_fechas_centinela_declaradas n_fechas_fin_abierto
#> 2    alta                  4                             1                    0
#> 3   desde                  0                             0                    0
#> 4   hasta                  4                             0                    4
#>   n_fechas_futuras lectura_fechas_futuras ultimo_dato_fecha
#> 2                1                   fila        2026-05-11
#> 3                0                   <NA>        2026-05-11
#> 4                8              admitidas        2026-06-26
```

En `alta`, las 4 fechas centinela son las 3 incorporadas y la declarada;
la fecha futura es un valor suelto, y `lectura_fechas_futuras = "fila"`
dice que se acusa con su fila. En `hasta`, las 4 `9999-12-31` son fin
abierto, no defectos. En una columna cuyo nombre mira hacia adelante,
como `hasta` o un vencimiento, las fechas futuras se cuentan igual, pero
pueden ser legítimas y su lectura es `admitidas`.

`general$actualidad` dice, por columna, el último dato que no es futuro
ni una centinela declarada, y cuántos días antes de la `fecha` de la
corrida cae. No hay una fecha ganadora entre columnas ni un puntaje.
`general$periodos` reparte las filas de cada período declarado en
categorías que no se pisan:

``` r

en_memoria$general$actualidad$columnas[
  , c("columna", "ultimo_dato_fecha", "antiguedad_dias", "n_evaluadas")
]
#>   columna ultimo_dato_fecha antiguedad_dias n_evaluadas
#> 1    alta        2026-05-11              50          58
#> 2   desde        2026-05-11              50          60
#> 3   hasta        2026-06-26               4          50
particion <- en_memoria$general$periodos
particion[, c(
  "n", "n_cerrados_validos", "n_fin_antes_inicio", "n_fin_abierto_nulo",
  "n_fin_abierto_centinela"
)]
#>    n n_cerrados_validos n_fin_antes_inicio n_fin_abierto_nulo n_fin_abierto_centinela
#> 1 60                 53                  1                  2                       4
writeLines(strwrap(particion$aviso, 88))
#> El fin abierto mezcla dos convenciones: fines ausentes y fines con la fecha declarada
#> en `fin_abierto`.
```

De las 60 filas, 53 son períodos cerrados válidos, 1 termina antes de
empezar, 2 no tienen fin y 4 terminan en la fecha de vigencia. El aviso
señala que el fin abierto se escribe de 2 maneras. Una fecha de fin que
suele ser centinela y no se declaró se cuenta aparte, con un aviso de
posible fin abierto: no pasa por un período cerrado.

[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
recibe los mismos argumentos y cuenta lo mismo sobre la tabla completa,
en SQLite y DuckDB, contra literales y con la misma regla que la
memoria. Las cuentas van a `resumen_tabla$columnas`, la actualidad a
`resumen_tabla$meta$actualidad` y los períodos a
`resumen_tabla$meta$periodos`. SQLite no tiene tipo fecha: con
`extended_types = TRUE` al conectar, RSQLite guarda las fechas como
fechas ([Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html#diferencias-con-r)
tiene el detalle).

``` r

con_fechas <- DBI::dbConnect(RSQLite::SQLite(), ":memory:", extended_types = TRUE)
DBI::dbWriteTable(con_fechas, "contratos", contratos)
en_motor <- perfilar_dbi(
  con_fechas, "contratos", fecha = "2026-06-30", centinelas_fecha = "2000-01-01",
  periodos = periodo, resumen_fechas = TRUE
)
identical(
  en_motor$resumen_tabla$columnas[, campos_fecha],
  columnas(en_memoria)[, campos_fecha]
)
#> [1] TRUE
en_motor$resumen_tabla$meta$periodos[, c("n", "n_cerrados_validos", "estado")]
#>    n n_cerrados_validos    estado
#> 1 60                 53 calculado
```

Lo que el SQL no puede contar como lo lee R se declara. Son 3 casos: una
columna de SQLite que guarda las fechas como texto, instantes a menos de
un segundo de una centinela lejana con resolución de microsegundos y una
columna de texto que la muestra lee como fecha. La columna `fecha` de
`ventas` es del último caso:

``` r

texto <- perfilar_dbi(
  conexion, "ventas", muestra = 200, orden_muestra = "id_venta",
  fecha = "2026-06-30", resumen_fechas = TRUE
)
sql_fechas <- texto$resumen_tabla$sql
sql_fechas <- unique(
  sql_fechas[sql_fechas$etapa == "fechas", c("columna", "estado", "motivo")]
)
sql_fechas$estado
#> [1] "no_disponible"
writeLines(strwrap(sql_fechas$motivo, 88))
#> La columna `fecha` es texto en el motor y la muestra la lee como fecha: sus formatos
#> los decide la columna entera. Con `estrategia_no_sql = "bloques"` se cuenta en R.
por_bloques <- perfilar_dbi(
  conexion, "ventas", muestra = 200, orden_muestra = "id_venta",
  fecha = "2026-06-30", resumen_fechas = TRUE, estrategia_no_sql = "bloques"
)
por_bloques$resumen_tabla$meta$actualidad$columnas[
  , c("columna", "ultimo_dato_fecha", "antiguedad_dias", "n_evaluadas", "estado")
]
#>   columna ultimo_dato_fecha antiguedad_dias n_evaluadas    estado
#> 1   fecha        2026-06-29               1        1000 calculado
```

Con `estrategia_no_sql = "no_medible"`, el valor por omisión, esas
cuentas quedan `no_disponible` en `resumen_tabla$sql` con su motivo, y
nunca como cero. Con `"bloques"`, `lupa` lee la columna en R por
bloques, con el mismo controlador, y la cuenta con la misma regla: las
1.000 fechas, no sólo las 200 de la muestra. La partición de un período,
en cambio, necesita que sus 2 columnas sean fechas en el motor: sobre
texto queda `no_disponible` con su motivo, también con `"bloques"`.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
recibe los mismos argumentos y cuenta esas consultas antes de correr.

## ⚠️ Lo que el resumen no garantiza

Los agregados son consultas separadas, y la tabla puede cambiar entre
una y otra. El resumen lo declara:

``` r

completo$resumen_tabla$meta$snapshot
#> [1] FALSE
writeLines(strwrap(completo$resumen_tabla$meta$nota_snapshot, 88))
#> No hubo lectura instantánea: cada agregado se midió en su propio momento y la tabla
#> pudo cambiar entre consultas. La cobertura declara las comparaciones incoherentes entre
#> grupos de consistencia distintos.
```

Si `n_validos` y `n_distintos` salen incoherentes y vienen de consultas
distintas,
[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
agrega una fila `alcance_distinto` con las 2 sentencias. Es evidencia de
que la tabla cambió durante la corrida, no una acusación contra el
motor.

## Antes de conectar a otro motor

El paquete del motor y la biblioteca del sistema que va debajo los
instala quien usa `lupa`. El error que aparece cuando falta alguno rara
vez dice cuál es.
[`requisitos_motor()`](https://sebollin.github.io/lupa/reference/requisitos_motor.md)
lo dice antes de conectar:

``` r

requisitos_motor()            # el catálogo entero
requisitos_motor("mariadb")   # qué necesita MariaDB y cómo conseguirlo
```

Por cada motor devuelve:

- el paquete de R;
- la biblioteca del sistema, con su nombre en Debian y en Fedora;
- la forma de instalarla sin permisos de administrador, cuando existe;
- el dialecto esperado;
- el estado de prueba del motor.

## Compilar el plan antes de correr

Contar las consultas no dice si el motor las acepta. Con
`compilar = TRUE`, el plan le pide al motor que compile cada sentencia
sin ejecutarla y sin traer filas, y deja una fila por sentencia en
`attr(plan, "compilacion")`:

``` r

plan <- plan_perfilado_dbi(conexion, "ventas", compilar = TRUE)
compilacion <- attr(plan, "compilacion")
table(compilacion$estado)
#> 
#> candidata_rechazada           compilada       no_verificada 
#>                   1                  11                  11
unique(compilacion$metodo[compilacion$estado == "compilada"])
#> [1] "EXPLAIN QUERY PLAN"
```

`compilada` quiere decir que el motor aceptó la sentencia;
`no_compilada`, que la rechazó, y entonces `motivo` trae su mensaje: una
columna o una función que no existe, un permiso que falta. El error
aparece antes de una corrida larga y no a la mitad. `no_verificada` es
lo que no se pudo saber, porque la forma de la consulta depende de un
resultado anterior o de la lectura de filas, y `candidata_rechazada` es
la respuesta de un sondeo de capacidad (el desvío se prueba como
`STDDEV_SAMP` y como `STDEV`, y SQLite no conoce una), no una falla del
plan. Cómo se compila depende del motor: `EXPLAIN` en SQLite, DuckDB y
PostgreSQL, `PREPARE` en MySQL y MariaDB, y `SET NOEXEC ON` en SQL
Server. No toca la transacción que tengas abierta.

Compilar no garantiza que la consulta corra: un error que depende de los
datos, como un `CAST` de un texto que no es un número, compila y falla
al ejecutar, y algunos motores no comprueban los permisos al compilar.
Tampoco garantiza que no se ejecute nada: las consultas de preparación
del plan se ejecutan como sin compilar, sin leer filas de la tabla.
[`?plan_perfilado_dbi`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
tiene la sección «Compilación» completa.
[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md)
acepta el mismo argumento. Por omisión, `compilar = FALSE`, y nada
cambia.

## ⏱ Para una tabla grande

Antes de perfilar una tabla de millones de filas conviene mirar el plan.
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)
cuenta las consultas y el trabajo que haría
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
con los mismos argumentos, sin leer datos. El artículo [Motores y
escala](https://sebollin.github.io/lupa/articles/motores-y-escala.html)
reúne lo que hace falta para decidir:

- el costo de cada métrica y los lotes de consultas;
- la política de costo;
- los presupuestos de memoria;
- la copia de la muestra que guarda la sesión;
- los dialectos;
- la garantía de la clave primaria según el motor;
- la matriz de motores probados.

``` r

DBI::dbDisconnect(conexion)
```

## Resumen

- [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  devuelve 2 bloques: `resumen_tabla`, calculado en el motor, y
  `perfil_muestra`, calculado en R sobre las filas leídas.
- Por omisión los agregados cubren toda la tabla. El perfil por fila,
  también por omisión, trae todas las filas que entran en los topes de
  lectura.
- `muestra = n` acota la lectura a R; `universo = "muestra_motor"`
  muestrea en el motor; `bloque_muestra = "solo_agregados"` no trae
  filas.
- El muestreo de lo leído está en
  `perfil_muestra$meta$origen_dbi$muestreo`; el del motor, en
  `resumen_tabla$meta$muestreo`, sólo con `muestra_motor`.
- `centinelas_fecha`, `periodos` y `resumen_fechas = TRUE` cuentan
  fechas centinela, futuras, períodos y actualidad.
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  los cuenta en el motor con la misma regla que la memoria; lo que el
  SQL no puede contar igual se declara, o se cuenta en R con
  `estrategia_no_sql = "bloques"`.
- `meta$snapshot = FALSE`: los agregados no son una lectura instantánea.
- `plan_perfilado_dbi(compilar = TRUE)` le pide al motor que compile
  cada consulta del plan sin ejecutarla: un error de sintaxis, de nombre
  o de permisos aparece antes.

## 🧭 Paso siguiente

Una base no es una tabla:
[Colecciones](https://sebollin.github.io/lupa/articles/colecciones.md)
perfila varias tablas a la vez y busca las relaciones entre ellas.

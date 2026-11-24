# 11. Colecciones

Una base de datos no es una tabla: son muchas, a veces repartidas en
varios esquemas. Y la credencial de trabajo no siempre puede leerlas
todas. ¿Cómo se perfilan varias tablas de una vez? ¿Cómo se ve lo que no
se pudo medir? ¿Y cómo se encuentra qué columna de una tabla apunta a
otra?

> **En esta guía:**
>
> - declarar la frontera de la base con
>   [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md);
> - perfilar todas sus tablas con
>   [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md),
>   una fila por tabla;
> - leer en la cobertura lo que no se pudo medir, que nunca cuenta como
>   cero;
> - estimar con
>   [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
>   cuánto cuesta buscar relaciones;
> - buscar claves foráneas candidatas con
>   [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md);
> - seguir una corrida cortada con `reanudar = TRUE`;
> - encontrar las tablas de códigos y declarar, columna por columna, qué
>   códigos quieren decir «sin dato».

## Una base pequeña

El ejemplo es una base de ventas como la de [Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.md),
esta vez con las tablas que la rodean:

- las sucursales;
- los productos;
- una tabla de devoluciones que todavía no tiene filas.

Hay 2 ventas que citan una sucursal, la 9, que no está en `sucursales`.

``` r

library(lupa)

sucursales <- data.frame(
  id_sucursal = 1:4,
  zona = c("Norte", "Sur", "Este", "Oeste"),
  stringsAsFactors = FALSE
)
productos <- data.frame(
  id_producto = 1:6,
  rubro = rep(c("bebidas", "limpieza", "almacén"), each = 2),
  stringsAsFactors = FALSE
)
ventas <- data.frame(
  id_venta = 1:40,
  id_sucursal = c(rep(1:4, 9), 9, 9, 1, 2),
  id_producto = rep(1:6, length.out = 40),
  monto = seq(100, 490, by = 10)
)
ventas$monto[c(5, 17)] <- NA
```

Las tablas se escriben en una base SQLite en memoria. Es sólo la
preparación del ejemplo: `lupa` no escribe en la base.

``` r

conexion <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(conexion, "sucursales", sucursales)
DBI::dbWriteTable(conexion, "productos", productos)
DBI::dbWriteTable(conexion, "ventas", ventas)
DBI::dbWriteTable(conexion, "devoluciones", ventas[0, ])
```

## 🗄️ Declarar la frontera

[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
dice qué tablas forman la base. No consulta nada: sólo declara. Se
incluye a propósito `pagos`, que no existe en esta base, para ver qué
pasa con una tabla que no se puede leer.

``` r

base <- coleccion(
  conexion,
  c("sucursales", "productos", "ventas", "devoluciones", "pagos"),
  nombre = "comercial"
)
base
#> Colección declarada: comercial
#> Motor: SQLiteConnection
#> Tablas declaradas: 5
#> La frontera es declarada: `lupa` no recorre el catálogo.
```

La frontera se declara y nunca se descubre. Recorrer el catálogo del
motor convertiría un error de permisos en un resultado. Una tabla que la
credencial no ve desaparecería sin dejar rastro. Declarada, la tabla que
no se puede leer queda nombrada en la cobertura.

El esquema es parte de la identidad de una tabla. En un motor con
esquemas, la forma recomendada es un `data.frame` con las columnas
`esquema` y `tabla`. Lleva los nombres tal cual, con puntos o espacios.
El texto `"esquema.tabla"` es un atajo que se parte en el punto. La
ayuda de
[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
explica las 3 formas y la columna `catalogo`, para los nombres de 3
partes.

## Perfilar todas las tablas

[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
conserva el resumen de cada tabla, que es acotado. Por omisión descarta
el perfil de la muestra, que es el objeto pesado: con cientos de tablas
no entraría en memoria. Esta colección es chica, así que el perfil de la
muestra se retiene con `conservar_perfiles = TRUE` para mirarlo más
adelante.

``` r

perfil <- perfilar_coleccion(base, conservar_perfiles = TRUE)
perfil
#> Perfil de colección: comercial
#> Tablas: 4 perfiladas de 5 declaradas
#> Sin perfilar: 1 (ver `cobertura_coleccion`)
#> Vacías: 1 (cero filas: nada que medir, no cero faltantes)
#> Sin lectura instantánea: cada tabla trae su `momento`.
```

[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
devuelve una fila por tabla, con el resumen que el motor calculó en SQL
sobre la tabla completa:

``` r

columnas(perfil)[, c(
  "tabla", "n_columnas", "n_filas", "prop_faltantes_maxima",
  "n_columnas_medidas", "universo"
)]
#>          tabla n_columnas n_filas prop_faltantes_maxima n_columnas_medidas       universo
#> 1   sucursales          2       4                  0.00                  2 tabla_completa
#> 2    productos          2       6                  0.00                  2 tabla_completa
#> 3       ventas          4      40                  0.05                  4 tabla_completa
#> 4 devoluciones          4       0                    NA                  0 tabla_completa
```

Cada tabla se perfila con
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
`muestra`, `bloque_muestra` y el resto de los argumentos se pasan a cada
una, y significan lo mismo que en una tabla sola:

- Por omisión `muestra = Inf`: cada tabla se trae entera si entra en los
  topes de lectura. En una base grande conviene `muestra = n` o
  `bloque_muestra = "solo_agregados"`.
- Las declaraciones por columna, como `columnas_codigo`, también valen
  para todas las tablas. La ayuda de
  [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  explica qué pasa con una tabla que no tiene esa columna.

La tabla vacía no aparece como una tabla perfecta. `devoluciones` tiene
0 filas:

- su `prop_faltantes_maxima` es `NA`;
- `n_columnas_medidas` dice que no se conoce la proporción de ausentes
  de ninguna columna.

Donde no hubo medición va `NA`, no `0`.

## ⚠️ Lo que no se pudo medir

[`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
nombra cada tabla que no se perfiló o que no tenía nada que medir, con
su motivo y la forma de resolverlo:

``` r

faltan <- cobertura(perfil)
faltan[, c("tabla", "alcance")]
#>          tabla     alcance
#> 1 devoluciones tabla_vacia
#> 2        pagos       tabla
for (i in seq_len(nrow(faltan))) {
  writeLines(strwrap(paste0(faltan$tabla[i], ": ", faltan$motivo[i]), 88))
}
#> devoluciones: La tabla tiene cero filas: se leyó su estructura pero no hay nada que
#> medir. Las proporciones quedan en NA, no en cero.
#> pagos: No se pudo perfilar la tabla: La tabla solicitada no existe en la conexión DBI,
#> o la credencial no tiene permiso para verla. El motor no distinguió entre las dos, y
#> `dbExistsTable()` tampoco. no such table: pagos
```

`pagos` no se suma como una tabla de 0 filas: queda declarada como no
leída, y el motivo conserva la respuesta del motor. El motor no
distingue entre una tabla que no existe y una que la credencial no puede
ver. `lupa` tampoco inventa la diferencia. En bases institucionales los
permisos parciales son el caso normal, no el borde.

Lo que falta dentro de las tablas que sí se perfilaron va a
`cobertura_metricas`, una fila por columna y métrica no calculada. Es la
cobertura de las métricas SQL de cada tabla, distinta de la de
diagnósticos de `cobertura(perfil)` y de la de un marco
([glosario](https://sebollin.github.io/lupa/articles/leer-un-perfil.html#glosario)).
Contada por tabla y estado:

``` r

metricas <- perfil$cobertura_metricas
table(metricas$tabla, metricas$estado)
#>               
#>                no_aplica sin_valores
#>   devoluciones         0          44
#>   productos            8           0
#>   sucursales           9           0
#>   ventas               2           0
```

- `no_aplica` son los agregados cuantitativos (mínimo, media, desvío, …)
  que no corresponden a una columna de texto como `zona` o `rubro`.
- `sin_valores` son las métricas de `devoluciones`, que no tiene ningún
  valor que agregar.

Perfilar una colección son muchas consultas, y la base puede cambiar
entre ellas. No hay lectura instantánea: cada fila de
[`columnas()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
trae el `momento` en que se midió la tabla, y `perfil$meta$snapshot`
vale `FALSE`.

## Los hallazgos de cada tabla

Los hallazgos por fila son de cada tabla, no de la colección. Con los
perfiles retenidos, cada uno se lee con los accesores de siempre:

``` r

names(perfil$perfiles)
#> [1] "sucursales"   "productos"    "ventas"       "devoluciones"
hallazgos(perfil$perfiles$ventas)[, c(
  "columna", "tipo_hallazgo", "severidad", "n_afectados"
)]
#>       columna         tipo_hallazgo  severidad n_afectados
#> 1    id_venta posible_identificador         ok           1
#> 2 id_sucursal              outliers sospechoso           2
```

En `ventas`, las 2 filas que citan la sucursal 9 aparecen como valores
atípicos de `id_sucursal`. Sin `conservar_perfiles = TRUE`, lo mismo se
obtiene con
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
sobre la tabla que interesa.

## 📋 Un informe de la colección

Con cientos de tablas, el perfil no se lee tabla por tabla.
[`summary()`](https://rdrr.io/r/base/summary.html) devuelve el índice:
una fila por tabla declarada, con su estado y el motivo que registró la
cobertura. `devoluciones` queda `vacia` y `pagos`, `con_error`; ninguna
se cuenta como una tabla sana:

``` r

indice <- summary(perfil)
indice[, c("tabla", "estado", "n_filas", "n_hallazgos_sospechoso")]
#>          tabla    estado n_filas n_hallazgos_sospechoso
#> 1   sucursales perfilada       4                      0
#> 2    productos perfilada       6                      0
#> 3       ventas perfilada      40                      1
#> 4 devoluciones     vacia       0                     NA
#> 5        pagos con_error      NA                     NA
```

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) da las
otras tablas planas, listas para
[`write.csv()`](https://rdrr.io/r/utils/write.table.html).
`parte = "matriz"` ubica cada tabla en las dimensiones y factores del
marco, con el estado que
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
da a su perfil; las celdas no se combinan en ninguna cifra, porque la
jerarquía del marco es taxonómica. `parte = "no_medido"` junta lo que no
se midió con el motivo de cada cobertura, y `parte = "hallazgos"`, los
hallazgos de cada tabla:

``` r

matriz <- as.data.frame(perfil, parte = "matriz")
matriz[, c("tabla", "estado_tabla", "Completitud / Densidad", "Frescura / Actualidad")]
#>          tabla estado_tabla Completitud / Densidad Frescura / Actualidad
#> 1   sucursales    perfilada                 medida             no_aplica
#> 2    productos    perfilada                 medida             no_aplica
#> 3       ventas    perfilada                 medida             no_aplica
#> 4 devoluciones        vacia              no_aplica             no_aplica
#> 5        pagos    con_error                   <NA>                  <NA>
table(as.data.frame(perfil, parte = "no_medido")$origen)
#> 
#>    cobertura_coleccion cobertura_diagnosticos     cobertura_metricas 
#>                      2                      9                     63
```

`reportar(perfil)` publica las mismas tablas en un HTML autocontenido.
La matriz y los hallazgos necesitan los perfiles: sin
`conservar_perfiles = TRUE`, el índice y lo no medido se arman igual, y
el informe dice lo que falta.

## Cuánto cuesta buscar relaciones

Buscar claves foráneas compara columnas de una tabla con columnas de
otra. Una clave foránea tiene dirección: cada par de tablas cuenta 2
veces. Antes de comparar nada,
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
cuenta las comparaciones leyendo sólo los nombres de las columnas:

``` r

costo <- estimar_costo_coleccion(base)
str(costo[c(
  "n_pares_dirigidos", "n_pares_estimados", "n_pares_sin_estimar",
  "n_comparaciones_columnas", "tablas_sin_esquema"
)])
#> List of 5
#>  $ n_pares_dirigidos       : num 20
#>  $ n_pares_estimados       : num 12
#>  $ n_pares_sin_estimar     : num 8
#>  $ n_comparaciones_columnas: num 104
#>  $ tablas_sin_esquema      : chr "pagos"
```

Con 5 tablas son 20 pares dirigidos. Los que involucran a `pagos` no se
cuentan como cero: quedan en `n_pares_sin_estimar`, porque su esquema no
se pudo leer. En una base de 1.000 tablas los pares dirigidos son
999.000. Explorarlos todos no es una opción cara, sino imposible. Por
eso los pares también se declaran.

Se declaran los 2 pares de interés —cada tabla de referencia contra
`ventas`— y, además, las columnas que pueden participar de una relación:

``` r

pares <- data.frame(
  tabla_1 = c("sucursales", "productos"),
  tabla_2 = c("ventas", "ventas")
)
candidatas <- list(
  sucursales = "id_sucursal",
  productos = "id_producto",
  ventas = c("id_sucursal", "id_producto")
)
estimar_costo_coleccion(base, pares = pares)$n_comparaciones_columnas
#> [1] 16
estimar_costo_coleccion(
  base, pares = pares, columnas_candidatas = candidatas
)$n_comparaciones_columnas
#> [1] 4
```

Declarar las columnas candidatas reduce a la vez la lectura y las
comparaciones.

## Buscar claves foráneas candidatas

[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
compara los pares declarados. `orden` hace que la lectura de cada tabla
sea repetible. Sin él, una lectura acotada devuelve un subconjunto
arbitrario, y el resultado lo declara.

``` r

relaciones <- relaciones_coleccion(
  base, pares = pares, columnas_candidatas = candidatas,
  orden = list(sucursales = "id_sucursal", productos = "id_producto",
               ventas = "id_venta")
)
encontradas <- relaciones$relaciones
data.frame(
  columna_1 = paste(encontradas$tabla_1, encontradas$columna_tabla1, sep = "."),
  columna_2 = paste(encontradas$tabla_2, encontradas$columna_tabla2, sep = "."),
  cobertura_1_en_2 = round(encontradas$cobertura_tabla1_en_tabla2, 2),
  cobertura_2_en_1 = round(encontradas$cobertura_tabla2_en_tabla1, 2),
  huerfanas_2 = encontradas$n_filas_huerfanas_tabla2
)
#>                columna_1          columna_2 cobertura_1_en_2 cobertura_2_en_1 huerfanas_2
#> 1 sucursales.id_sucursal ventas.id_sucursal             1.00             0.95           2
#> 2  productos.id_producto ventas.id_sucursal             0.67             0.95           2
#> 3  productos.id_producto ventas.id_producto             1.00             1.00           0
relaciones$meta$estable
#> [1] TRUE
```

Cada fila trae 2 coberturas, una por dirección. `cobertura_2_en_1` es la
proporción de las filas de la segunda tabla cuyo valor aparece en la
primera:

- `sucursales.id_sucursal` cubre el 95 % de `ventas.id_sucursal`. El 5 %
  que falta son las 2 ventas de la sucursal 9 (`huerfanas_2`): filas
  huérfanas que hay que revisar en el sistema de origen. Para ver cuáles
  son, se leen las dos tablas y se llama a
  [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md),
  que nombra las filas.
- `productos.id_producto` contra `ventas.id_producto` cubre todo en las
  2 direcciones.
- `productos.id_producto` contra `ventas.id_sucursal` es una
  coincidencia: los códigos de sucursal son enteros chicos que también
  son códigos de producto. La cobertura en la otra dirección lo delata,
  porque no todos los productos aparecen como sucursal.

Por eso una relación candidata **no es una clave foránea comprobada**:
es un indicio que se confirma contra el diccionario de datos. Además,
cada par se compara sobre una muestra de hasta 10.000 filas por tabla
(`muestra`). Cada relación dice cuántas filas leyó de cada lado y si la
lectura quedó truncada (`muestreado_1`, `muestreado_2`). Con la tabla de
referencia truncada, la cobertura sólo puede subir al leer más: es una
cota inferior.

## ⏯️ Reanudar una corrida cortada

Una base de cientos de tablas puede tardar una noche. Si el proceso
muere en la tabla doscientos, o se cae la conexión y las que siguen
fallan, no hace falta empezar de nuevo. Con `directorio_reanudacion`,
cada tabla que se perfila sin consultas fallidas se guarda en ese
directorio en cuanto termina.

La base del ejemplo está en memoria, y una base en memoria no sobrevive
al proceso: no tiene nada que reanudar. Se copia a un archivo y se
declara la misma colección, con `pagos` incluida:

``` r

archivo <- tempfile(fileext = ".sqlite")
en_archivo <- DBI::dbConnect(RSQLite::SQLite(), archivo)
RSQLite::sqliteCopyDatabase(conexion, en_archivo)
base_archivo <- coleccion(
  en_archivo,
  c("sucursales", "productos", "ventas", "devoluciones", "pagos"),
  nombre = "comercial"
)
directorio <- tempfile("corridas-")
corrida <- perfilar_coleccion(base_archivo, directorio_reanudacion = directorio)
corrida$meta$reanudacion$tablas
#>   identificador    origen        motivo  estado
#> 1    sucursales perfilada corrida_nueva  limpia
#> 2     productos perfilada corrida_nueva  limpia
#> 3        ventas perfilada corrida_nueva  limpia
#> 4  devoluciones perfilada corrida_nueva  limpia
#> 5         pagos perfilada corrida_nueva fallida
```

`pagos` quedó `fallida`, como quedaría una tabla a la que la corrida
llegó con la conexión caída. La misma llamada con `reanudar = TRUE`
sigue esa corrida: las tablas guardadas no vuelven a consultar la base,
y se perfilan de nuevo las que faltan, las que tuvieron alguna consulta
fallida y las que no se pudieron perfilar.

``` r

seguida <- perfilar_coleccion(
  base_archivo, directorio_reanudacion = directorio, reanudar = TRUE
)
#> ℹ Se reanuda la corrida corrida-20261124-191752-2db848fb356f, empezada el 2026-11-24 19:17:52 UTC: se reusan 4 tablas y se vuelven a perfilar 1.
seguida$meta$reanudacion[c("intento", "reanudada")]
#> $intento
#> [1] 2
#> 
#> $reanudada
#> [1] TRUE
seguida$meta$reanudacion$tablas
#>   identificador    origen  motivo  estado
#> 1    sucursales   reusada    <NA>  limpia
#> 2     productos   reusada    <NA>  limpia
#> 3        ventas   reusada    <NA>  limpia
#> 4  devoluciones   reusada    <NA>  limpia
#> 5         pagos perfilada fallida fallida
```

`meta$reanudacion` dice qué corrida se siguió, en qué intento va y, por
tabla, si se reusó o se perfiló y por qué. El objeto es el mismo que el
de una corrida sin cortes, salvo los tiempos y el `momento` de cada
tabla, que en una tabla reusada es el de su medición original. La
corrida reanudada usa también la `fecha` de la original. Para seguir una
corrida tienen que coincidir la base, las tablas declaradas y su orden,
los argumentos, y las versiones de `lupa`, de R y del controlador; el
error nombra lo que difiere. Si la corrida compatible más reciente ya
terminó con todas sus tablas, o no hay ninguna, `reanudar = TRUE` se
niega y no empieza otra por su cuenta.

Reanudar también se niega si la base cambió desde el corte. Las tablas
guardadas la describen como era y las que faltan la describirían como
es: el resultado no sería el de ninguna corrida. Si alguien crea `pagos`
antes de reanudar:

``` r

DBI::dbWriteTable(en_archivo, "pagos", data.frame(id_pago = 1:3, id_venta = 1:3))
rechazo <- tryCatch(
  perfilar_coleccion(
    base_archivo, directorio_reanudacion = directorio, reanudar = TRUE
  ),
  lupa_error_reanudacion = function(e) e
)
rechazo$motivo
#> [1] "base_cambiada"
# La carpeta temporal se abrevia para que el mensaje se lea.
mensaje <- gsub(dirname(archivo), "...", conditionMessage(rechazo), fixed = TRUE)
writeLines(strwrap(mensaje, 88))
#> No se reanuda la corrida corrida-20261124-191752-2db848fb356f: desde su intento
#> anterior (2026-11-24T19:17:53+0000) cambió la base: en `.../file2db864fb425f.sqlite`,
#> el tamaño, de 20480 a 24576 bytes y la fecha de modificación, de 2026-11-24
#> 19:17:52.813 UTC a 2026-11-24 19:17:54.122 UTC. Las tablas guardadas la describen como
#> era y las que faltan la describirían como es ahora: el resultado no sería el de ninguna
#> corrida. Una corrida nueva, con `reanudar = FALSE`, la mide entera como está.
```

La base se reconoce por su archivo, con su tamaño y su fecha de
modificación. Abrir, leer y perfilar la base no cuentan como cambio.
`reanudar = FALSE`, el valor por omisión, empieza siempre una corrida
nueva y mide la base entera como está.

Por ahora se reanuda con SQLite y DuckDB en archivo. Una base en
memoria, un motor de servidor, `proteger_datos_personales = TRUE` o
`incluir_valores = FALSE` se rechazan con un error de clase
`lupa_error_reanudacion` que dice por qué. Un bloqueo impide que dos
procesos sigan la misma corrida. El directorio guarda el perfil completo
de cada tabla, con lo que el perfil publica de los datos: hay que
tratarlo como a los datos, y las corridas no se borran solas. La sección
«Reanudar una corrida cortada» de
[`?perfilar_coleccion`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
explica qué se compara y qué hacer si el proceso murió con la corrida
tomada.

## 🏷️ Tablas de códigos

Muchas columnas guardan un código cuyo significado está en otra tabla.
Que `medio = 9` quiera decir «sin dato» sólo se sabe leyendo
`medios_pago`. El perfil de la columna sola ve un 9 que se aparta del
resto, no un faltante.

El ejemplo suma a la base 2 tablas de códigos y una de cobros que las
usa. Las claves foráneas quedan declaradas en el catálogo:

``` r

ejecutar <- function(sql) invisible(DBI::dbExecute(conexion, sql))
ejecutar("CREATE TABLE medios_pago (codigo INTEGER PRIMARY KEY, etiqueta TEXT)")
ejecutar("INSERT INTO medios_pago VALUES
  (1, 'Efectivo'), (2, 'Tarjeta'), (3, 'Transferencia'), (9, 'Sin dato')")
ejecutar("CREATE TABLE canales (codigo TEXT PRIMARY KEY, descripcion TEXT)")
ejecutar("INSERT INTO canales VALUES
  ('MOS', 'Mostrador'), ('WEB', 'Web'), ('NS', 'No sabe')")
ejecutar("CREATE TABLE cobros (id_cobro INTEGER, id_venta INTEGER,
  medio INTEGER REFERENCES medios_pago(codigo),
  canal TEXT REFERENCES canales(codigo))")
cobros <- data.frame(
  id_cobro = 1:30,
  id_venta = 1:30,
  medio = c(rep(1:3, 8), 9, 9, 9, 9, 1, 2),
  canal = c(rep(c("MOS", "WEB"), 13), "NS", "NS", "MOS", "WEB")
)
invisible(DBI::dbAppendTable(conexion, "cobros", cobros))
```

[`detectar_tablas_codigos()`](https://sebollin.github.io/lupa/reference/detectar_tablas_codigos.md)
recibe la colección y su perfil:

``` r

base_cobros <- coleccion(conexion, c("medios_pago", "canales", "cobros"))
perfil_cobros <- perfilar_coleccion(base_cobros, conservar_perfiles = TRUE)
codigos <- detectar_tablas_codigos(base_cobros, perfil_cobros, max_filas = 10)
codigos
#> Tablas de códigos
#>   Tablas: 3 (2 con a lo sumo 10 filas)
#>   Candidatas: 2; posibles: 0; sin clave: 0
#>   Usos: 2
#>   Propuestas de ausencia: 2 (ninguna se aplica sola: marcar `incluir` y usar ausencias_desde_propuesta())
codigos$tablas[, c("tabla", "columna_codigo", "n_usos", "cobertura_maxima", "estado")]
#>         tabla columna_codigo n_usos cobertura_maxima    estado
#> 1 medios_pago         codigo      1                1 candidata
#> 2     canales         codigo      1                1 candidata
```

Una tabla de códigos es chica —hasta `max_filas` filas, 1.000 por
omisión; acá 10, porque en esta base de juguete todas las tablas son
chicas—, tiene una columna completa y única, el código, y otra tabla la
usa. El uso sale de las claves foráneas que declara el catálogo, como
acá, o de las relaciones que midió
[`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md),
que se pasan en `relaciones`. Si alguna columna la usa con una cobertura
de al menos `umbral_cobertura` (0,9 por omisión), la tabla es
`candidata`; por la forma sola queda `posible`.

Después lee las etiquetas. Cuando una dice que el código significa «sin
dato», «no sabe» o «no corresponde», propone declararlo como ausencia en
cada columna que lo usa:

``` r

propuesta <- codigos$propuesta
propuesta[, c(
  "codigo", "etiqueta", "semantica", "tabla", "columna", "argumento", "n_filas",
  "incluir"
)]
#>   codigo etiqueta   semantica  tabla columna            argumento n_filas incluir
#> 1      9 Sin dato    sin_dato cobros   medio sentinelas_numericos       4   FALSE
#> 2     NS  No sabe no_responde cobros   canal     cadenas_ausencia       2   FALSE
```

Cada propuesta trae su evidencia: la etiqueta y cuántas filas de la
columna usan ese código (`n_filas`), contadas en el motor. `argumento`
dice adónde va: un número, a `sentinelas_numericos` si R lee la columna
como número; un texto, a `cadenas_ausencia` si la lee como texto. La
función no aplica nada ni calcula un puntaje: cada propuesta nace con
`incluir = FALSE`, y quien conoce la base marca las que confirma. El
léxico reconoce esas expresiones en castellano, inglés y portugués;
`Ninguno` u `Otro` son categorías válidas y no se proponen, y
`etiquetas_ausencia` agrega las propias.

[`ausencias_desde_propuesta()`](https://sebollin.github.io/lupa/reference/ausencias_desde_propuesta.md)
convierte las filas marcadas en la declaración. Sin `tabla`, es una
lista por tabla y, dentro de cada una, por columna, que es lo que
reciben `sentinelas_numericos` y `cadenas_ausencia` en
[`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md):

``` r

propuesta$incluir <- TRUE
declaracion <- ausencias_desde_propuesta(propuesta)
str(declaracion)
#> List of 2
#>  $ sentinelas_numericos:List of 1
#>   ..$ cobros:List of 1
#>   .. ..$ medio: num 9
#>  $ cadenas_ausencia    :List of 1
#>   ..$ cobros:List of 1
#>   .. ..$ canal: chr "NS"
declarado <- do.call(
  perfilar_coleccion,
  c(list(base_cobros, conservar_perfiles = TRUE), declaracion)
)
mirar <- function(perfil) {
  h <- hallazgos(perfil$perfiles$cobros)
  h[h$columna %in% c("medio", "canal"), c("columna", "tipo_hallazgo", "n_afectados")]
}
mirar(perfil_cobros)
#>   columna tipo_hallazgo n_afectados
#> 3   medio      outliers           4
mirar(declarado)
#>   columna         tipo_hallazgo n_afectados
#> 3   medio             faltantes           4
#> 4   medio faltantes_disfrazados           4
#> 5   canal faltantes_disfrazados           2
```

Sin la declaración, los 4 nueves de `medio` eran valores atípicos y los
2 `NS` de `canal` no se veían. Declarados, los dos son faltantes
disfrazados.

Con `tabla`, la salida son las listas por columna de esa tabla, para
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md):

``` r

ausencias_desde_propuesta(propuesta, tabla = "cobros")
#> $sentinelas_numericos
#> $sentinelas_numericos$medio
#> [1] 9
#> 
#> 
#> $cadenas_ausencia
#> $cadenas_ausencia$canal
#> [1] "NS"
```

Cada entrada vale para su columna lo que el vector valdría para ella, y
las columnas que la lista no nombra reciben el valor por omisión. Por
eso la lista es más precisa que el vector: `sentinelas_numericos = 9`
contaría también como faltante el 9 de `id_cobro` y el de `id_venta`;
`list(medio = 9)`, sólo el de `medio`. Qué cambia en el perfil cada una
de estas declaraciones está en [El vacío por
diseño](https://sebollin.github.io/lupa/articles/vacio-por-diseno.html#todo-lo-que-se-le-declara-al-paquete).

``` r

DBI::dbDisconnect(conexion)
```

## Resumen

- [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)
  declara qué tablas forman la base; `lupa` no recorre el catálogo.
- [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md)
  devuelve una fila por tabla con su resumen en el motor, y
  [`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md)
  nombra lo que no se pudo leer, nunca como cero.
- Donde no hubo medición, como en una tabla vacía, el resumen dice `NA`.
- [`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
  cuenta las comparaciones antes de hacerlas; declarar pares y columnas
  candidatas las reduce.
- [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md)
  propone claves foráneas candidatas con su cobertura en las 2
  direcciones: un indicio, no una prueba.
- Con `directorio_reanudacion`, una corrida cortada se sigue con
  `reanudar = TRUE` y da lo mismo que una sin cortes. Si la base cambió
  desde el corte, no se reanuda.
- [`detectar_tablas_codigos()`](https://sebollin.github.io/lupa/reference/detectar_tablas_codigos.md)
  propone qué códigos quieren decir «sin dato», sin aplicar nada;
  [`ausencias_desde_propuesta()`](https://sebollin.github.io/lupa/reference/ausencias_desde_propuesta.md)
  convierte lo confirmado en listas por columna para
  `sentinelas_numericos` y `cadenas_ausencia`.

## 🧭 Paso siguiente

En una tabla grande, 2 filas pueden describir lo mismo escrito de 2
maneras. [Escala y duplicados
aproximados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)
muestra cómo buscarlas y cuánto cuesta.

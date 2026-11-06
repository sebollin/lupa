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
>   [`relaciones_coleccion()`](https://sebollin.github.io/lupa/reference/relaciones_coleccion.md).

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
#> o la credencial no tiene permiso para verla. El motor no distinguio entre las dos, y
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
#>   devoluciones         0          36
#>   productos            7           0
#>   sucursales           7           0
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
  cobertura_2_en_1 = round(encontradas$cobertura_tabla2_en_tabla1, 2)
)
#>                columna_1          columna_2 cobertura_1_en_2 cobertura_2_en_1
#> 1 sucursales.id_sucursal ventas.id_sucursal             1.00             0.95
#> 2  productos.id_producto ventas.id_sucursal             0.67             0.95
#> 3  productos.id_producto ventas.id_producto             1.00             1.00
relaciones$meta$estable
#> [1] TRUE
```

Cada fila trae 2 coberturas, una por dirección. `cobertura_2_en_1` es la
proporción de las filas de la segunda tabla cuyo valor aparece en la
primera:

- `sucursales.id_sucursal` cubre el 95 % de `ventas.id_sucursal`. El 5 %
  que falta son las 2 ventas de la sucursal 9: filas huérfanas que hay
  que revisar en el sistema de origen.
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

## 🧭 Paso siguiente

En una tabla grande, 2 filas pueden describir lo mismo escrito de 2
maneras. [Escala y duplicados
aproximados](https://sebollin.github.io/lupa/articles/escala-y-duplicados.md)
muestra cómo buscarlas y cuánto cuesta.

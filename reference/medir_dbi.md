# Medir un modelo de calidad dentro de una base de datos DBI

Evalúa un modelo de calidad directamente en el motor SQL (SQLite,
DuckDB, PostgreSQL actual y 9.3, MySQL, MariaDB y SQL Server verificados
según la política de motores, sus verificaciones de metadatos y sus
guardas) y devuelve un resumen de clase `medicion_resumen` con el mismo
contrato de columnas que `medir(..., detalle = "resumen")`.

## Usage

``` r
medir_dbi(
  modelo,
  conexion,
  tablas = NULL,
  id_medicion = NULL,
  fecha = Sys.time(),
  aplicabilidad = NULL,
  estrategia_no_sql = c("no_medible", "bloques"),
  bloque_filas = NULL,
  tamano_lote = NULL,
  max_valores_in = 500L,
  max_consultas = Inf,
  dialecto = "auto",
  proteger_datos_personales = FALSE,
  columnas_personales = character(),
  validadores_personales = NULL,
  instrumentar = TRUE,
  orden_texto = c("idioma", "bytes"),
  conformidad = NULL
)
```

## Arguments

- modelo:

  Modelo de calidad creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

- conexion:

  Conexión DBI activa o colección creada por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- tablas:

  Mapeo opcional de nombres de entidades del modelo a nombres de tablas
  físicas (vector de texto, lista con nombre o
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html)). Si es `NULL`,
  cada entidad se busca por su nombre (o en
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md)).

- id_medicion:

  Identificador de la corrida. Si es `NULL`, se genera uno.

- fecha:

  Fecha de la corrida (UTC).

- aplicabilidad:

  Lista con nombre de fórmulas de un solo lado por columna, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- estrategia_no_sql:

  Cómo resolver las métricas que no se compilan a SQL: `"no_medible"`
  (por omisión, no lee filas) o `"bloques"` (lee por bloques sólo las
  columnas necesarias para las métricas separables por fila). Una
  columna que el controlador no lee con fidelidad no se lee por bloques:
  un `BIT` de MySQL leído con RMySQL llega como `0` aunque el bit esté
  encendido, y una regla sobre sus valores queda `no_medible` en las dos
  vías. [`medir()`](https://sebollin.github.io/lupa/reference/medir.md)
  sobre una tabla que el usuario leyó con RMySQL no puede saberlo: mide
  esos `0`.

- bloque_filas:

  Cantidad de filas por bloque cuando `estrategia_no_sql = "bloques"`.
  `NULL` elige el tamaño automático.

- tamano_lote:

  Tope opcional de métricas por consulta consolidada. `NULL` usa el
  límite del motor.

- max_valores_in:

  Tope de valores en listas `IN (...)` compiladas a SQL.

- max_consultas:

  Presupuesto máximo de consultas emitidas durante la ejecución,
  incluidas las consultas de esquema.

- dialecto:

  Dialecto SQL: `"auto"` (por omisión) o uno de `"sqlite"`, `"duckdb"`,
  `"postgresql"`, `"mysql"`, `"mariadb"`, `"sqlserver"`.

- proteger_datos_personales:

  Si es `TRUE`, protege los mensajes del motor y no agrega la marca
  `datos_personales_sin_proteger`.

- columnas_personales:

  Columnas declaradas como datos personales, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- validadores_personales:

  Validadores adicionales de datos personales, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- instrumentar:

  Si es `TRUE` (por omisión), conserva las consultas SQL y el tiempo
  transcurrido en `attr(resultado, "ejecucion_dbi")`.

- orden_texto:

  Orden del texto en el rango de `ValoresPosiblesPorComprension` y en
  las comparaciones `<`, `<=`, `>` y `>=` de la fórmula de
  `ReglaIntegridadIntraEntidad`, como en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md). Con
  `"idioma"` (por omisión) esas reglas sobre texto quedan `no_medible`:
  ningún motor reproduce el orden del locale de la sesión de R. Con
  `"bytes"` se traducen a una comparación por los bytes del UTF-8 —el
  orden por punto de código: `"Z"` antes que `"a"`, `"ñ"` después de
  `"z"`— y miden lo mismo que `medir(orden_texto = "bytes")`: en SQLite
  con `COLLATE BINARY`, si la base guarda el texto en UTF-8, y en DuckDB
  con `encode()`. Un texto con bytes que no son UTF-8 válido se ordena
  por sus bytes, como en memoria; y el rango lo deja afuera, como en
  memoria: en DuckDB con `try(decode())` (una versión sin
  [`try()`](https://rdrr.io/r/base/try.html) lo declara `no_medible`) y
  en SQLite con su guarda de siempre para el texto no ASCII, porque
  SQLite no valida UTF-8. En PostgreSQL, MySQL, MariaDB y SQL Server la
  traducción está escrita pero todavía no se comparó con la medición en
  memoria: se declara `no_medible` con ese motivo, y con
  `estrategia_no_sql = "bloques"` la regla se mide en R, por bytes.

- conformidad:

  Tabla devuelta por
  [`conformidad_dbi()`](https://sebollin.github.io/lupa/reference/conformidad_dbi.md)
  con esta misma conexión, modelo y configuración. Una regla que se
  cuenta en el motor deja de contarse en él sólo si la batería la
  encontró `no_conforme`: el motor cuenta y da distinto que R. Con
  `estrategia_no_sql = "bloques"` se mide leyendo sus columnas por
  bloques en R, como lo que no se traduce a SQL; si no, se declara
  `no_medible`. Si la diferencia apareció sólo con `NULL`
  (`condicion = "con_nulos"`), esto se aplica únicamente si la tabla
  real tiene `NULL` en las columnas de la regla. Una `abstencion` no
  quita la regla: con los datos reales decide la guarda de siempre. Si
  la batería es de otra conexión o de otra configuración, o si una regla
  no está en ella, cambió su definición, no se evaluó o su motor no está
  verificado, se avisa y la regla se mide como siempre. Una tabla sin la
  clase y los atributos de
  [`conformidad_dbi()`](https://sebollin.github.io/lupa/reference/conformidad_dbi.md)
  se rechaza. `NULL` conserva la medición habitual.

## Value

Un `data.frame` de clase `medicion_resumen` (sin heredar de `medicion`)
con las columnas del resumen —también las de sus cuentas, ver
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md)— y los
atributos de `medir(detalle = "resumen")` más `ejecucion_dbi`. Cada fila
sale de una sola lectura, y cada lectura de una tabla queda en
`attr(x, "lecturas")`: si dos lecturas de la misma tabla en la corrida
ven distinto número de filas, sus cifras quedan `no_cierran` y
`medir_dbi()` avisa. Ese control ve filas agregadas o quitadas, no un
`UPDATE` que no cambia el conteo; donde el motor ofrece un contador de
modificaciones que lupa lee, se compara también ese contador y el
control se publica como `contador_del_motor`; si no, como
`solo_filas_agregadas_o_quitadas`. El contador que el motor puede
negarle a una credencial sin permisos extra —`performance_schema` en
MySQL, `INNODB_SYS_TABLESTATS` en MariaDB,
`sys.dm_db_index_operational_stats` en SQL Server— se lee en una
sentencia aparte antes y otra después de las consultas de la tabla,
nunca dentro de una que mide: si el motor lo niega, la tabla se mide
igual, sus lecturas quedan `solo_filas_agregadas_o_quitadas` y el motivo
queda en `attr(x, "ejecucion_dbi")$contador_motor`. En SQL Server con
`READ COMMITTED` por bloqueo una sola sentencia tampoco es una
instantánea. La tabla de cada lectura es la que el motor resuelve para
su nombre al leer su esquema, una vez por llamada —si la base es un
archivo, con la ruta del archivo en lugar del nombre con que se
adjuntó—: si durante la medición cambia lo que resuelve un nombre, por
ejemplo se crea una tabla temporal con ese nombre, ese cambio no se ve.
En PostgreSQL, MySQL, MariaDB y SQL Server la tabla lleva su base y su
servidor (una huella, no la dirección): dos servidores, o dos bases del
mismo servidor, con el mismo esquema y nombre son dos tablas. El
servidor sale de lo que la conexión declara (el anfitrión o la carpeta
del socket, el puerto, la base, el nombre del servidor): dos conexiones
al mismo servidor por rutas distintas —el socket y TCP, o dos nombres
del mismo anfitrión— se tratan como tablas distintas y sus lecturas no
se comparan. En MySQL el catálogo no lista las tablas temporales, y la
lectura de una temporal que tapa a una permanente del mismo nombre se
registra con la tabla (y los tipos) de la permanente; en MariaDB, si el
catálogo la lista, esa lectura queda sin tabla resuelta. Dos o más
lecturas de la misma tabla nunca son `una_lectura`.

## Details

Lo que el motor no puede reproducir con la semántica exacta de R se
declara `no_medible` en `attr(resultado, "cobertura_metricas")` —nunca
se aproxima ni se interpreta como cero— o, cuando la métrica es
separable por fila y se indica `estrategia_no_sql = "bloques"`, se mide
en R proyectando sólo sus columnas y leyendo por bloques.

En la medición en memoria, `aplicable` y `aplicabilidad` conservan la
evaluación publicada de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md); el
resumen resuelve cada `aplicabilidad` una sola vez, igual que la
medición completa. En la vía DBI, una fórmula traducible sólo puede
nombrar columnas de la entidad y literales escritos en la propia
fórmula. Un símbolo externo se declara `no_medible` sin consultar ni
forzar su entorno.

La marca `datos_personales_sin_proteger` (cuando
`proteger_datos_personales = FALSE`) se determina por nombre de columna
y por `columnas_personales`, sin leer filas de la base.

## See also

[`plan_medicion_dbi()`](https://sebollin.github.io/lupa/reference/plan_medicion_dbi.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md),
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  res <- medir_dbi(mod, con)
  tablero_calidad(res)
  DBI::dbDisconnect(con)
}
```

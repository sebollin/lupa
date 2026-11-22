# Planificar una medición de calidad en una base de datos DBI

Clasifica cada métrica del modelo según la vía con que se medirá en el
motor (`sql_consolidado`, `sql_dedicado`, `sql_dedicado_referencial`,
`bloques_r`, `distintos_r`, `no_medible`, `no_medible_metodo` o
`abstencion`) y estima las consultas necesarias, sin leer filas de
ninguna tabla. Un referencial del motor suma la consulta de esquema de
su tabla y una validación de su clave por tabla y clave, que comparten
las métricas que lo usan (en `dedicadas`); `distintos_r` cuenta en
`bloques`.

## Usage

``` r
plan_medicion_dbi(
  modelo,
  conexion,
  tablas = NULL,
  aplicabilidad = NULL,
  estrategia_no_sql = c("no_medible", "bloques"),
  bloque_filas = NULL,
  tamano_lote = NULL,
  max_valores_in = 500L,
  max_consultas = Inf,
  dialecto = "auto",
  orden_texto = c("idioma", "bytes"),
  compilar = FALSE
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

- aplicabilidad:

  Lista con nombre de fórmulas de un solo lado por columna, igual que en
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- estrategia_no_sql:

  Cómo resolver las métricas que no se compilan a SQL: `"no_medible"`
  (por omisión, no lee filas) o `"bloques"` (lee por bloques sólo las
  columnas necesarias para las métricas separables por fila).

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

- compilar:

  Si es `TRUE`, le pide al motor que compile, sin ejecutarlas ni traer
  filas, las consultas SQL del plan, y publica una fila por sentencia en
  `attr(plan, "compilacion")`. Ver la sección Compilación.

## Value

Un `data.frame` de clase `plan_medicion_dbi` con una fila por métrica
instanciada (`metrica_instanciada`, `metrica`, `entidad`, `atributos`,
`via`, `motivo`, `columnas_bloque`) y atributos `motor` y `consultas`
(`esquema`, `consolidadas`, `dedicadas`, `bloques`), y, cuando se pide,
`compilacion`.

## Details

Sólo consulta el esquema (`SELECT * ... WHERE 1 = 0` y metadatos del
motor). No recibe `proteger_datos_personales` y hereda la protección de
quien lo llama, igual que
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md).

## Compilación

Con `compilar = TRUE` el plan, además de contar, le pide al motor que
compile cada consulta que construye, sin ejecutarla ni traer filas, y
publica una fila por sentencia en `attr(plan, "compilacion")`, con
`consulta_id`, `etapa`, `sql`, `estado`, `motivo` y `metodo`. `estado`
es uno de:

- `compilada`: el motor la aceptó.

- `no_compilada`: el motor la rechazó, y `motivo` trae su mensaje: una
  columna o una función que no existe, un permiso que falta.

- `candidata_rechazada`: una forma candidata de un sondeo de capacidad
  que el motor no conoce. El plan de perfilado prueba el desvío como
  `STDDEV_SAMP` y como `STDEV` en todo motor, y la corrida usa la que el
  motor acepte. Las candidatas no se compilan: se mandan como las manda
  la corrida, sobre una constante y sin leer filas, y la rechazada queda
  con `metodo` vacío; es la respuesta del sondeo, no una falla del plan.
  Un plan sano no tiene ninguna fila `no_compilada`.

- `no_verificada`: no se pudo saber, porque la forma de la consulta
  depende de un resultado anterior, de una capacidad del controlador o
  de una lectura de filas, porque el motor no tiene una vía de
  compilación, o porque venció el tope de tiempo de PostgreSQL (ver
  abajo).

En `plan_medicion_dbi()` hay una fila por sentencia compilada, así que
su número no tiene por qué ser el de `attr(plan, "consultas")`: la
consulta de la sesión se ejecuta al planificar, como sin compilar, y las
métricas por bloques se calculan en R sobre una sola lectura por
entidad, la fila `bloques`.

`metodo` dice cómo se compiló: `EXPLAIN QUERY PLAN` en SQLite (los
`PRAGMA` de lectura del catálogo se mandan tal cual, `dbSendQuery`),
`EXPLAIN` en DuckDB y PostgreSQL, `PREPARE` y su `DEALLOCATE` en MySQL y
MariaDB —su `EXPLAIN` ejecuta las subconsultas en `FROM`, `JOIN` y
`WITH`; `PREPARE` no ejecuta ninguna y rechaza lo mismo—, con un nombre
único por llamada que no pisa las sentencias preparadas del usuario, y
`SET NOEXEC ON` en SQL Server.

Lo que compilar no garantiza:

- Que la consulta corra. Los errores que dependen de los datos aparecen
  sólo al ejecutar: un `CAST` de un texto que no es un número compila y
  falla en la corrida. Y no todos los motores comprueban los permisos al
  compilar: SQL Server, bajo `SET NOEXEC ON`, no mira ninguno —una
  columna, una función o una tabla negada compilan—, y PostgreSQL 9.3 no
  mira el `EXECUTE` de una función, tampoco dentro de una vista. En esos
  casos la sentencia queda `compilada` y falla en la corrida.

- Que no se ejecute nada. Las consultas de preparación del plan —las del
  esquema, las que llevan `WHERE 1 = 0` o `LIMIT 0`, las pruebas sobre
  literales como `SELECT ... FROM (SELECT 1 AS ...)`— se ejecutan como
  en el plan sin compilar, sin leer filas de la tabla y sin tope de
  tiempo. Y en PostgreSQL el planificador pliega las constantes: una
  función declarada `IMMUTABLE` con argumentos constantes se ejecuta al
  planificar, aunque lea datos, también en esa preparación, como sin
  compilar. Por eso cada sentencia compilada lleva un tope de 2 segundos
  (`SET LOCAL statement_timeout`, o el de la sesión si es menor) dentro
  de una transacción propia que se revierte —o de un punto de guardado,
  dentro de una transacción del usuario—: la que lo supera queda
  `no_verificada` con ese motivo, y la sesión vuelve al tope que tenía.
  `PREPARE` no planificaría, pero en PostgreSQL tampoco comprueba los
  permisos, y `EXPLAIN` sí.

- En SQL Server, que exista una tabla: bajo `SET NOEXEC ON` la
  resolución de nombres de tablas es diferida y una tabla que no existe
  compila.

Compilar nunca toca la transacción del usuario, y si no lo puede
garantizar no compila: falla con `lupa_error_transaccion_dbi` antes de
mandar nada. Así pasa en DuckDB con una transacción abierta —cualquier
error del motor, también el de una sentencia que no compila, la aborta
sin aviso, un
[`DBI::dbCommit()`](https://dbi.r-dbi.org/reference/transactions.html)
posterior pierde lo hecho en ella, y DuckDB no tiene `SAVEPOINT`—, y en
PostgreSQL cuando no se puede saber si hay una abierta (una abierta a
mano y ya abortada). Si la hay se averigua antes de empezar —la marca de
RPostgres, o `now()`, que dentro de una transacción no cambia— y no por
si un `SAVEPOINT` falla. SQLite, MySQL y MariaDB no abortan la
transacción por el error de una sentencia.

La conexión queda como estaba. En SQL Server `NOEXEC` es un estado de la
sesión que, prendido, hace que nada de lo que se mande se ejecute, sin
error: se apaga después de cada sentencia y se comprueba con un
`SELECT 1`. Si no se puede apagar, la llamada falla con
`lupa_error_conexion_dbi` y dice que la conexión quedó inutilizable.
Dentro de una transacción abierta con `XACT_ABORT ON`, una sentencia que
nombra una columna que no existe deshace la transacción aun bajo
`NOEXEC`: `XACT_ABORT` se apaga mientras se compila y se vuelve a
prender.

Una interrupción —el tope de
[`setTimeLimit()`](https://rdrr.io/r/base/setTimeLimit.html), Ctrl-C— no
es una sentencia que no compila: la compilación deja la sesión como
estaba y la vuelve a señalar, sin devolver el plan. En SQL Server, una
que llega en medio de un envío puede dejar al controlador sin poder
apagar `NOEXEC`; entonces la llamada falla con
`lupa_error_conexion_dbi`, dice que la conexión quedó inutilizable y que
la compilación se interrumpió, y la conexión hay que cerrarla.

## See also

[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`plan_perfilado_dbi()`](https://sebollin.github.io/lupa/reference/plan_perfilado_dbi.md)

## Examples

``` r
if (requireNamespace("DBI", quietly = TRUE) &&
    requireNamespace("RSQLite", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "d", data.frame(x = c(1, NA, 3)))
  mod <- modelo(instanciar(especializar(metricas_nucleo()$NoNulo, "NN"), "d", "x"))
  plan_medicion_dbi(mod, con)
  DBI::dbDisconnect(con)
}
```

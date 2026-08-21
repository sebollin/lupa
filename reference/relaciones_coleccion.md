# Buscar claves foráneas candidatas entre pares declarados

Corre
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)
sobre los pares de tablas que se declaren, y devuelve las relaciones
candidatas junto con la cobertura de los pares que no se pudieron
comparar.

## Usage

``` r
relaciones_coleccion(
  coleccion,
  pares,
  muestra = 10000,
  umbral_cobertura = 0.9,
  orden = NULL,
  tope_cache_mb = 512,
  columnas_candidatas = NULL,
  podar = FALSE,
  tope_memoria_mb = 512
)
```

## Arguments

- coleccion:

  Objeto creado por
  [`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md).

- pares:

  Data frame con columnas `tabla_1` y `tabla_2`. Cero filas es una
  declaración válida de «ningún par».

- muestra:

  Filas traídas por tabla para comparar.

- umbral_cobertura:

  Cobertura mínima para informar una relación.

- orden:

  Columnas para `ORDER BY`: un vector de texto que se aplica a todas las
  tablas, o una lista nombrada por identificador de tabla. Sin él la
  lectura no es repetible y el objeto lo declara.

- tope_cache_mb:

  Presupuesto de memoria para la caché de lecturas, en megabytes. Al
  agotarse se sigue leyendo sin cachear y `meta` lo declara.

- columnas_candidatas:

  Lista nombrada por identificador de tabla. Cada vector declara las
  columnas que pueden participar; una tabla no nombrada conserva todas
  sus columnas.

- podar:

  Si se aplican las podas que cambiarían lo informado —tipos
  incompatibles y cardinalidades imposibles—, tal como en
  [`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md).
  `FALSE` por omisión; la poda cierta por rangos disjuntos se aplica
  siempre porque no cambia ninguna fila.

- tope_memoria_mb:

  Presupuesto de memoria para resultados de relaciones, en megabytes. Al
  agotarse, las combinaciones pendientes se declaran en
  `cobertura_podas` y los pares pendientes en `cobertura_pares`.

## Value

Objeto `relaciones_coleccion` con `relaciones`, `cobertura_pares`,
`cobertura_podas` y `meta`.

## Details

**Los pares se declaran, igual que la frontera.** Con mil tablas hay
casi un millón de pares dirigidos, así que explorar todos no es una
opción cara sino una opción imposible.
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md)
permite verlo antes.

**Cada tabla se lee una sola vez.** Los pares repetidos se descartan,
los pares de una tabla consigo misma se rechazan, y las lecturas se
guardan en una caché con presupuesto de memoria declarado: dos pares que
comparten una tabla ya no la leen dos veces. `meta$lecturas` deja el SQL
exacto, la vía usada, las filas leídas y el momento de cada lectura.

**El orden no se supone.** Sin `orden`, una lectura acotada devuelve un
subconjunto arbitrario y el resultado declara `meta$estable = FALSE`.
Declarar columnas de orden hace la lectura repetible.

Cada par se compara sobre una muestra de filas de cada tabla, y el
resultado declara ese alcance: una relación candidata sobre una muestra
**no es una clave foránea comprobada**, es un indicio que hay que
confirmar contra el diccionario de datos.

Las columnas candidatas se podan antes de materializar cada comparación.
Las podas quedan declaradas en `cobertura_podas`, con su motivo y
conteo.

## See also

[`coleccion()`](https://sebollin.github.io/lupa/reference/coleccion.md),
[`estimar_costo_coleccion()`](https://sebollin.github.io/lupa/reference/estimar_costo_coleccion.md),
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)

## Examples

``` r
if (requireNamespace("RSQLite", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "personas", data.frame(id = 1:20, nombre = letters[1:20]))
  DBI::dbWriteTable(con, "visitas", data.frame(persona_id = c(1:15, 1:5)))
  col <- coleccion(con, c("personas", "visitas"), nombre = "padron")
  relaciones_coleccion(
    col, pares = data.frame(tabla_1 = "personas", tabla_2 = "visitas"),
    columnas_candidatas = list(personas = "id", visitas = "persona_id"),
    orden = list(personas = "id", visitas = "persona_id")
  )
  DBI::dbDisconnect(con)
}
```

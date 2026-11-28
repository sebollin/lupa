# Declarar un conjunto de datos referencial

Un referencial representa conocimiento externo mediante una clave y, de
forma opcional, valores asociados a ella. `normalizar` controla la
representación usada para emparejar referenciales; no modifica los datos
guardados. La declaración de completitud es explícita: `RatioCobertura`
exige `completo = TRUE` y un `alcance` explícito; un referencial parcial
sólo puede usarse para correctitud.

## Usage

``` r
referencial(
  datos,
  clave,
  valor = character(),
  completo = FALSE,
  alcance = NULL,
  nombre = NULL,
  normalizar = TRUE
)
```

## Arguments

- datos:

  Tabla de referencia. Se conserva una copia ordinaria de R. O una tabla
  del motor, nombrada con un
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html) o con su
  nombre: el referencial no copia nada y
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  la resuelve en su conexión o su colección como resuelve las entidades.

- clave:

  Columnas que identifican unívocamente cada fila. Puede contener varias
  columnas.

- valor:

  Columnas asociadas que pueden contrastarse en correctitud débil. Es
  opcional.

- completo:

  Si la tabla cubre todo el universo declarado. Es `FALSE` por omisión.

- alcance:

  Descripción obligatoria cuando `completo = TRUE`.

- nombre:

  Nombre legible del referencial. Si se omite, usa el nombre del objeto
  de entrada o `"referencial"`.

- normalizar:

  `TRUE`, `FALSE`, `"amplio"`, un perfil de
  [`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
  o una lista nombrada por columna. `TRUE` es el valor predeterminado.

## Value

Objeto de clase `referencial`: una lista con la tabla de referencia en
`datos`, los nombres de columna `clave` y `valor`, el indicador
`completo` -si la referencia agota el dominio-, el `alcance` declarado o
`NA`, el `nombre`, y el perfil de normalización resuelto en
`normalizar`. Declara la referencia; no compara nada todavía.

## Details

La clave no admite ausentes y debe identificar cada fila de forma única.
`valor` no puede repetir columnas de `clave` y representa atributos que
se contrastan en correctitud semántica débil. El constructor copia la
tabla y no consulta fuentes externas. Una columna puede ser BLOB —la
clase `blob` o una lista de `raw`—: se aparea por sus bytes y sólo con
otro BLOB; contra un texto o un número ninguna fila coincide, aunque el
texto escriba sus bytes, y no tiene proximidad textual.

**Un referencial que vive en el motor.** Con `datos = DBI::Id(...)` o el
nombre de una tabla, el referencial no copia la tabla a R: guarda la
referencia (en `tabla_motor`, sin conexión: se serializa como cualquier
lista) y `datos` queda `NULL`. Que las columnas existan y que la clave
no tenga ausentes y sea única se verifica en cada
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md),
con una consulta y con las mismas frases, porque la tabla puede cambiar
entre la declaración y la medición.
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) no lo
lee: las métricas se abstienen con estado `no_medible` y el motivo.
`nombre`, si se omite, es el de la tabla. Como el nombre se resuelve en
la conexión de cada medición, el mismo referencial medido en otra base
es otra tabla: el sello de la medición (`attr(, "sello_metricas")`)
firma también la ubicación de esa conexión —archivo, anfitrión, puerto y
base, sin usuario ni credenciales—, y unir o comparar corridas de dos
bases avisa que la métrica cambió de definición. Dos bases en memoria no
se distinguen. En
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
las tres métricas de
[`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md)
cuentan lo que contaría
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) con las
dos tablas leídas por `dbReadTable()`; ver allí qué se mide en el motor
y qué se declara.

## See also

[`metricas_referencial()`](https://sebollin.github.io/lupa/reference/metricas_referencial.md),
[`instanciar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`detectar_relaciones()`](https://sebollin.github.io/lupa/reference/detectar_relaciones.md)

## Examples

``` r
padron <- referencial(
  data.frame(codigo = c("01", "02"), departamento = c("Artigas", "Canelones")),
  clave = "codigo", valor = "departamento",
  completo = TRUE, alcance = "departamentos del Uruguay"
)
padron
#> Referencial: referencial
#>   Filas: 2
#>   Clave: codigo
#>   Valores: departamento
#>   Completo: sí
#>   Alcance: departamentos del Uruguay
```

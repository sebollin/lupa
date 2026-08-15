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

  Tabla de referencia. Se conserva una copia ordinaria de R.

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

Un objeto de clase `referencial`.

## Details

La clave no admite ausentes y debe identificar cada fila de forma única.
`valor` no puede repetir columnas de `clave` y representa atributos que
se contrastan en correctitud semántica débil. El constructor copia la
tabla y no consulta fuentes externas.

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

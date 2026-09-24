# Construir un tablero de calidad

Resume una corrida en una fila por métrica y objeto. Las métricas
booleanas usan `ratio` por omisión y las reales usan `promedio`; la
elección queda siempre en la columna `agregacion`. `ratio_umbral` sólo
se aplica cuando se declara también el umbral correspondiente.

## Usage

``` r
tablero_calidad(
  medidas,
  agregaciones = NULL,
  umbrales = NULL,
  marco = NULL,
  cobertura = NULL
)
```

## Arguments

- medidas:

  Objeto creado por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) o
  [`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md).

- agregaciones:

  `NULL`, una agregación para todas las métricas, un vector con nombres
  de métrica instanciada o un data frame con `metrica_instanciada`,
  `agregacion` y, opcionalmente, `umbral`.

- umbrales:

  Vector numérico opcional con nombres de métrica instanciada. Se exige
  para cada `ratio_umbral` que no lo declare dentro de `agregaciones`.

- marco:

  Marco conceptual. Si se omite, usa el asociado a la medición, el marco
  AGESIC cuando corresponde o el conjunto de factores medidos —los tres
  caminos contienen lo que se publica—. Un marco declarado puede no
  contener los pares medidos: no se rechaza, porque informar contra la
  taxonomía propia es un uso previsto, pero lo que queda afuera se
  cuenta en `medidos_fuera_del_marco` y se nombra en
  `pares_fuera_del_marco`.

- cobertura:

  Cobertura opcional creada por
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md).

  **La correspondencia entre esa cobertura y `medidas` es suya, no del
  paquete**, igual que en
  [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md):
  el tablero cruza las dos por dimensión y factor y reescribe el estado
  de un factor cuando la medición lo cubre, sin verificar que las dos
  describan la misma tabla. Con una cobertura de otro análisis, el
  factor va a figurar como medido por esta corrida.

## Value

Data frame S3 `tablero_calidad`. Los atributos `alcance` y `cobertura`
conservan los conteos y el detalle del marco, y `pares_fuera_del_marco`
nombra los pares medidos que ese marco no declara.

## Details

El objeto conserva la cobertura completa del marco: factores medidos,
sin métrica declarada, no aplicables y fuera de alcance.
[`print()`](https://rdrr.io/r/base/print.html) muestra ambos elementos
para que unas pocas filas nunca se lean como cobertura total.

Esa cobertura se cuenta sobre los factores **del marco**, así que una
medida cuyo par dimensión-factor el marco no declara no entra en ninguna
de sus casillas. Eso es legítimo —declarar el marco propio y medir con
las métricas de `lupa` es un flujo previsto, y la cobertura de ese marco
puede satisfacerse por `perfil_mide`—, pero leer «se midieron 0 factores
de este marco» al lado de una fila con valor parece una contradicción
del objeto. Por eso el alcance publica además `medidos_fuera_del_marco`,
que **no** suma con las otras cuatro casillas, y el atributo
`pares_fuera_del_marco` los nombra.

## See also

[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md),
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)

## Examples

``` r
nucleo <- metricas_nucleo()
instancia <- instanciar(especializar(nucleo$NoNulo), "personas", "edad")
medidas <- medir(modelo(instancia), data.frame(edad = c(20, NA, 35)))
tablero_calidad(medidas)
#> 
#> ── Tablero de calidad ──────────────────────────────────────────────────────────
#>       componente   dimension   factor metrica objeto     valor orientacion
#>  componente-0001 Completitud Densidad  NoNulo   edad 0.6666667 conformidad
#>  agregacion umbral universo
#>       ratio     NA   celdas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican
#>              17                1                    11          0
#>  fuera_de_alcance medidos_fuera_del_marco
#>                 5                       0
```

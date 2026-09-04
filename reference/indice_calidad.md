# Calcular un índice de calidad declarado por el usuario

Sin `pesos`, devuelve
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
y nunca un puntaje. Con pesos nombrados por dimensión, transforma las
métricas de defecto como `1 - valor`, excluye las de orientación
`no_aplica` y conserva cada paso.

## Usage

``` r
indice_calidad(medidas, pesos, pesos_internos = NULL, ...)
```

## Arguments

- medidas:

  Medición, tablero o análisis de `lupa`.

- pesos:

  Vector numérico nombrado por dimensión, en `[0, 1]` y con suma uno. Si
  se omite, se devuelve el tablero.

- pesos_internos:

  Vector opcional nombrado por `componente`; es obligatorio para cada
  dimensión con más de una fila incluida.

- ...:

  Argumentos enviados a
  [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
  cuando `medidas` no es ya un tablero o análisis.

## Value

Sin pesos, un `tablero_calidad`. Con pesos, un objeto S3
`indice_calidad` que nunca se imprime como un número aislado.

## Details

Cuando una dimensión contiene varios componentes, `pesos_internos` debe
declarar una ponderación completa que sume uno dentro de esa dimensión.
No existe un promedio interno por omisión. El resultado conserva el
tablero, ambas capas de pesos, las inversiones, las exclusiones, los
universos y la cobertura del marco.

## See also

[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)

## Examples

``` r
nucleo <- metricas_nucleo()
instancias <- list(
  instanciar(especializar(nucleo$NoNulo), "padron", "codigo"),
  instanciar(especializar(nucleo$EntidadDuplicada), "padron")
)
medidas <- medir(modelo(instancias), data.frame(codigo = c("A", "B", "B")))
indice_calidad(medidas)
#> 
#> ── Tablero de calidad ──────────────────────────────────────────────────────────
#>       componente   dimension         factor          metrica  objeto     valor
#>  componente-0001 Completitud       Densidad           NoNulo  codigo 1.0000000
#>  componente-0002    Unicidad No-duplicación EntidadDuplicada (tabla) 0.6666667
#>  orientacion agregacion umbral universo
#>  conformidad      ratio     NA   celdas
#>      defecto      ratio     NA    filas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican
#>              17                2                    10          0
#>  fuera_de_alcance
#>                 5
# Pesos propios de este ejemplo, no del paquete:
indice_calidad(
  medidas,
  pesos = c(Completitud = 0.6, Unicidad = 0.4)
)
#> ── Índice de calidad declarado ─────────────────────────────────────────────────
#> Valor: 0.733333
#> 
#> ── Cobertura del índice ──
#> 
#>  factores_marco factores_en_indice
#>              17                  2
#>                                           factores metricas_no_medidas
#>  Completitud / Densidad; Unicidad / No-duplicación                    
#> ── Dimensiones, pesos y aportes ──
#> 
#>    dimension     valor peso    aporte                combinacion_interna
#>  Completitud 1.0000000  0.6 0.6000000 un componente; sin paso intermedio
#>     Unicidad 0.3333333  0.4 0.1333333 un componente; sin paso intermedio
#> ── Componentes de defecto invertidos ──
#> 
#>       componente dimension         factor          metrica  objeto     valor
#>  componente-0002  Unicidad No-duplicación EntidadDuplicada (tabla) 0.6666667
#>  orientacion agregacion umbral universo transformacion valor_indice
#>      defecto      ratio     NA    filas      1 - valor    0.3333333
#>  peso_interno
#>             1
#> ℹ Dentro de cada dimensión se usa un solo componente o los pesos_internos declarados; entre dimensiones se usan `pesos`.
#> ! Los componentes salen de universos distintos (por ejemplo, celdas, valores con formato reconocible y filas). El índice sólo los combina porque quien lo solicitó declaró los pesos.
```

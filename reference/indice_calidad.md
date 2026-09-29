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

`advertencia_universos` **se calcula de los universos que el propio
objeto publica**, no es un texto fijo. Con un solo universo lo nombra y
dice que las unidades son comparables; con varios dice cuántos son y
nombra, por universo, qué componentes caen en cada uno; sin componentes
no afirma nada sobre universos. Antes era una sola frase idéntica en
toda corrida, que afirmaba «salen de universos distintos» incluso cuando
los tres componentes publicaban `universo = "celdas"` —una afirmación
que el mismo objeto desmentía en su columna `universo`— y que no
nombraba nunca ningún componente, que es justamente lo que prometía
declarar.

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
#>       componente   dimension         factor          metrica
#>  componente-0001 Completitud       Densidad           NoNulo
#>  componente-0002    Unicidad No-duplicación EntidadDuplicada
#>      metrica_instanciada entidad  objeto     valor orientacion agregacion
#>     NoNulo@padron.codigo  padron  codigo 1.0000000 conformidad      ratio
#>  EntidadDuplicada@padron  padron (tabla) 0.6666667     defecto      ratio
#>  umbral universo
#>      NA   celdas
#>      NA    filas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican
#>              17                2                    10          0
#>  fuera_de_alcance medidos_fuera_del_marco
#>                 5                       0
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
#>       componente dimension         factor          metrica
#>  componente-0002  Unicidad No-duplicación EntidadDuplicada
#>      metrica_instanciada entidad  objeto     valor orientacion agregacion
#>  EntidadDuplicada@padron  padron (tabla) 0.6666667     defecto      ratio
#>  umbral universo transformacion valor_indice peso_interno
#>      NA    filas      1 - valor    0.3333333            1
#> ℹ Dentro de cada dimensión se usa un solo componente o los pesos_internos declarados; entre dimensiones se usan `pesos`.
#> ! Los componentes salen de 2 universos distintos, así que el índice combina unidades que no son comparables y sólo lo hace porque quien lo solicitó declaró los pesos. Por universo: celdas: componente-0001; filas: componente-0002.
```

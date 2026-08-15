# Validadores estructurales de Uruguay

`validar_ci_uy()` comprueba la cédula de identidad mediante su dígito
verificador módulo 10. Acepta siete u ocho dígitos y separadores de
puntos, espacios o guion; una cédula de siete dígitos se completa con
cero inicial.

## Usage

``` r
validar_ci_uy(x)

validar_rut_uy(x)
```

## Arguments

- x:

  Vector que se desea validar.

## Value

Vector lógico de la misma longitud que `x`.

## Details

`validar_rut_uy()` comprueba la estructura de doce dígitos del RUT y su
dígito verificador módulo 11. El algoritmo operativo no está publicado
por DGI como especificación normativa abierta; esta implementación se
contrastó con la implementación pública de `python-stdnum` y con
ejemplos públicos. Por eso valida estructura y dígito, no vigencia ni
existencia registral.

## References

Poder Ejecutivo de Uruguay (1978). Decreto 501/978, artículo 2.
<https://www.impo.com.uy/bases/decretos/501-1978/2>

Unidad de Acceso a la Información Pública (2024). Resolución 145/024.
<https://www.gub.uy/unidad-acceso-informacion-publica/institucional/normativa/resolucion-n-145024-sobre-reserva-informacion>

de Jong A. *python-stdnum: Uruguay RUT*.
<https://github.com/arthurdejong/python-stdnum/blob/master/stdnum/uy/rut.py>

## See also

[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md),
[`validar_iso3166()`](https://sebollin.github.io/lupa/reference/validadores_formato.md)

## Examples

``` r
validar_ci_uy(c("1.234.567-2", "1.234.567-3"))
#> [1]  TRUE FALSE
validar_rut_uy(c("21 100 342 0017", "21 030 367 0014"))
#> [1]  TRUE FALSE
```

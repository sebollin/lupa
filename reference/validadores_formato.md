# Validadores internacionales de sintaxis y dígitos de control

Estas funciones son puras y vectorizadas: no consultan la red ni prueban
la existencia de un país, moneda, buzón o entidad emisora. Los códigos
se contrastan con copias locales de las listas vigentes al preparar esta
versión del paquete. Los valores ausentes devuelven `NA`; todo valor
presente que no cumple el contrato devuelve `FALSE`.

## Usage

``` r
validar_iso3166(x, tipo = c("alpha2", "alpha3", "numerico"))

validar_iso4217(x)

validar_correo(x)

validar_luhn(x)

validar_mod97(x)
```

## Arguments

- x:

  Vector que se desea validar.

- tipo:

  Forma de código ISO 3166: dos letras (`"alpha2"`), tres (`"alpha3"`) o
  tres dígitos (`"numerico"`). Los valores numéricos de uno o dos
  dígitos se completan con ceros a la izquierda.

## Value

Vector lógico de la misma longitud que `x`.

## Details

`validar_correo()` comprueba un subconjunto práctico y deliberadamente
conservador de la sintaxis `addr-spec`: parte local de puntos y
caracteres ASCII habituales, seguida por un dominio DNS con al menos un
punto. No admite comentarios, cadenas entre comillas ni literales de
dominio válidos en la gramática completa de RFC 5322, y no prueba
entrega ni existencia.

`validar_luhn()` acepta únicamente dígitos y aplica el algoritmo de
Luhn. `validar_mod97()` acepta letras ASCII y dígitos, transforma las
letras a `A = 10, ..., Z = 35` y exige resto 1 conforme a ISO 7064 MOD
97-10. No reordena caracteres: protocolos como IBAN deben hacer antes su
transformación propia.

## References

International Organization for Standardization. *ISO 3166 Country
Codes*. <https://www.iso.org/iso-3166-country-codes.html>

SIX Group. *ISO 4217 Currency Codes*.
<https://www.six-group.com/en/products-services/financial-information/market-reference-data/data-standards.html>

Resnick P (2008). *Internet Message Format*, RFC 5322.
<https://www.rfc-editor.org/rfc/rfc5322>

Luhn HP (1960). *Computer for Verifying Numbers*, US Patent 2,950,048.
<https://patents.google.com/patent/US2950048A/en>

International Organization for Standardization. *ISO/IEC 7064:2003*.
<https://www.iso.org/standard/31531.html>

## See also

[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md),
[`validar_ci_uy()`](https://sebollin.github.io/lupa/reference/validadores_uy.md),
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)

## Examples

``` r
validar_iso3166(c("UY", "CL", "ZZ"))
#> [1]  TRUE  TRUE FALSE
validar_iso4217(c("UYU", "CLP", "ZZZ"))
#> [1]  TRUE  TRUE FALSE
validar_correo(c("persona@example.org", "sin-arroba"))
#> [1]  TRUE FALSE
validar_luhn(c("79927398713", "79927398714"))
#> [1]  TRUE FALSE
validar_mod97(c("9999123456789012141490", "9999123456789012141491"))
#> [1]  TRUE FALSE
```

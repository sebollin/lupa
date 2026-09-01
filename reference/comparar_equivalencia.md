# Comparar la equivalencia de dos resúmenes de perfiles

Compara por intersección los campos registrados de dos perfiles y
devuelve una fila por cada par de columna y campo. Los campos que no
tienen un eje registrado no se comparan y quedan declarados en
`campos_no_comparables`. Los campos bajo protección tampoco se comparan:
se declaran por columna, campo y lado en `campos_protegidos`.

## Usage

``` r
comparar_equivalencia(anterior, actual, tolerancia)
```

## Arguments

- anterior, actual:

  Un objeto `perfil` de
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
  un objeto `perfil_dbi` de
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
  o directamente un frame `columnas`.

- tolerancia:

  Número escalar no negativo y finito, declarado por quien llama. No
  tiene valor por omisión y se publica en cada fila.

## Value

Un frame de clase `equivalencia_perfiles` con `columna`, `campo`,
`valor_anterior`, `valor_actual`, `diferencia_relativa`, `veredicto`,
`motivo`, `tipo_eje` y `tolerancia`. `veredicto` es un factor ordenado
con niveles `identico < equivalente < materialmente_distinto`. Los
atributos `campos_no_comparables`, `detalle_campos_no_comparables`,
`campos_protegidos` y `resumen` declaran, respectivamente, los campos
omitidos, los motivos estructurales de esos campos, los campos omitidos
por protección y el conteo de cada veredicto. `campos_protegidos` es un
data frame con las columnas `columna`, `campo` y `lado`; este último
toma los valores `anterior` y `actual`.

## Details

El registro fijo asigna tolerancia sólo a `media`, `mediana`, `desvio` y
`longitud_media`. Los conteos, proporciones de conteos y extremos por
selección se comparan en el eje `exacto`; las fechas canónicas en
`fecha` y `moda` y `centinela_valor` en `valor`. Los ejes `exacto`,
`fecha` y `valor` son binarios por construcción.

El comparador devuelve datos, no decisiones: no alimenta hallazgos,
severidades ni puntajes. La tolerancia es del llamador y jamás entra en
una regla del paquete; sólo se aplica al eje flotante finito y queda
publicada para que el llamador decida cómo usarla.

Si una columna es temporal en exactamente uno de los perfiles, los
campos de magnitud numérica se omiten por el cambio de esquema y su
motivo queda en `detalle_campos_no_comparables` como
`tipo_cambiado:temporal_vs_no_temporal`. Así no se comparan duraciones
en segundos contra magnitudes numéricas sin unidad común. Cuando ambos
lados son temporales, `desvio` sí se compara en flotante porque ambas
puertas lo expresan en segundos.

## See also

[`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)

## Examples

``` r
anterior <- data.frame(
  columna = "monto", media = 10, minimo = 1, moda = "a",
  stringsAsFactors = FALSE
)
actual <- data.frame(
  columna = "monto", media = 10.00000000001, minimo = 2, moda = "b",
  stringsAsFactors = FALSE
)
comparar_equivalencia(anterior, actual, tolerancia = 1e-9)
#>   columna  campo valor_anterior valor_actual diferencia_relativa
#> 1   monto  media             10 10.00000....        9.999113e-13
#> 2   monto minimo              1            2                  NA
#> 3   monto   moda              a            b                  NA
#>                veredicto               motivo tipo_eje tolerancia
#> 1            equivalente dentro_de_tolerancia flotante      1e-09
#> 2 materialmente_distinto           eje_exacto   exacto      1e-09
#> 3 materialmente_distinto            eje_valor    valor      1e-09
```

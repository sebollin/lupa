# Comparar la equivalencia de dos resúmenes de perfiles

Compara por intersección los campos registrados de dos perfiles y
devuelve una fila por cada par de columna y campo. Los campos que no
tienen un eje registrado no se comparan y quedan declarados en
`campos_no_comparables`, y lo mismo pasa con un campo registrado que uno
de los dos lados no mide: se declara ahi, con el motivo
`campo_solo_en_anterior` o `campo_solo_en_actual` en
`detalle_campos_no_comparables` y una fila en `cobertura_diagnosticos`.
Los campos bajo protección tampoco se comparan: se declaran por columna,
campo y lado en `campos_protegidos`.

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

  **La tolerancia es mixta, no relativa.** La diferencia se divide por
  el mayor de `1`, `|anterior|` y `|actual|`, que es la misma regla que
  usa la detección de relaciones aritméticas: por debajo de magnitud 1
  el divisor es 1 y la comparación pasa a ser absoluta, porque dividir
  por casi cero convierte el ruido en un cambio del cien por ciento. Eso
  tiene una consecuencia que conviene tener presente: una media que pasa
  de `0,001` a `0,4` queda dentro de una tolerancia de `0,5`. Cuando la
  escala usada fue la unidad y no el valor, el `motivo` de esa fila lo
  dice (`dentro_de_tolerancia_escala_unidad`,
  `fuera_de_tolerancia_escala_unidad`) y la columna publicada se llama
  `diferencia_normalizada`, no «relativa».

## Value

Un frame de clase `equivalencia_perfiles` con `columna`, `campo`,
`valor_anterior`, `valor_actual`, `diferencia_normalizada`, `veredicto`,
`motivo`, `tipo_eje` y `tolerancia`. `veredicto` es un factor ordenado
con niveles `identico < equivalente < materialmente_distinto`. Los
atributos `campos_no_comparables`, `detalle_campos_no_comparables`,
`columnas_no_comparables`, `cobertura_diagnosticos`, `campos_protegidos`
y `resumen` declaran, respectivamente, los campos omitidos, los motivos
estructurales de esos campos, las columnas presentes en un solo lado o
con tipos incompatibles, los diagnósticos que no se pudieron evaluar,
los campos omitidos por protección y el conteo de cada veredicto. Un
campo sin valor medible en ambos lados, o con valor en un solo lado,
queda en la tabla con veredicto `no_comparable` y no suma como acuerdo.
`campos_protegidos` es un data frame con las columnas `columna`, `campo`
y `lado`; este último toma los valores `anterior` y `actual`.

## Details

Los campos del resumen de fechas (`resumen_fechas = TRUE` en
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md) y
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md))
están registrados. Los que se cuentan contra la fecha de la corrida
—`n_fechas_futuras`, `lectura_fechas_futuras` y `ultimo_dato_fecha`—
sólo se comparan entre dos objetos con la misma fecha: con fechas
distintas se declaran con el motivo `fecha_de_corrida_distinta`.

El registro fijo asigna tolerancia sólo a `media`, `mediana`, `desvio` y
`longitud_media`. Los conteos, proporciones de conteos y extremos por
selección se comparan en el eje `exacto`; las fechas canónicas en
`fecha` y `moda` y `centinela_valor` en `valor`. Los ejes `exacto`,
`fecha` y `valor` son binarios por construcción. En una columna
numérica, `moda` y `centinela_valor` se comparan por el número y no por
cómo lo escribió cada sesión: la marca decimal de `options(OutDec)` y la
notación de `scipen` no cuentan como una diferencia.

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

Si un lado es temporal y el otro **no publica su tipo** —el frame de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md)
no trae ningún campo de tipo—, no se afirma un cambio de tipo: los
mismos campos se omiten con el motivo
`tipo_desconocido:temporal_vs_sin_tipo`, porque tampoco se puede
garantizar una unidad común. Una fecha guardada como número en el motor
tiene su `desvio` en días de ese lado y en segundos del otro, y
compararlos publicaría como diferencia del dato una diferencia de
unidad.

Si una columna guarda caracteres en exactamente uno de los perfiles, los
campos calculados sobre la representación —longitudes, variantes unicode
y números escritos como texto— se omiten y su motivo queda en
`detalle_campos_no_comparables` como `tipo_cambiado:texto_vs_no_texto`.
Esos campos cambian con sólo cambiar el almacenamiento, aunque el dato
sea el mismo, de modo que compararlos publicaría el síntoma y callaría
la causa. `factor` cuenta como almacenamiento de caracteres. Este guarda
lee el tipo *declarado* y no el inferido: si el texto contiene números o
fechas, la inferencia borra justo la diferencia que hay que ver. Cuando
el tipo declarado viene de un vocabulario ajeno al de memoria —un tipo
SQL, por ejemplo— no se afirma nada y la comparación sigue como siempre.

Por la misma razón, si una columna lleva zona horaria en exactamente uno
de los perfiles, los campos que sólo ese almacenamiento puede medir se
omiten con el motivo `tipo_cambiado:con_zona_vs_sin_zona`. Del otro lado
no dan un valor distinto: no dan ninguno, porque no hay zona que
comparar contra UTC. Los extremos de fecha **sí** se siguen comparando:
al guardarse como texto la columna pierde la zona y los instantes que
denota son realmente otros, que es lo más importante que hay para
informar en ese caso.

Si uno de los lados es un objeto de
[`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
cada cifra de su resumen SQL lleva un estado y un alcance
(`resumen_tabla$sql`), y una cifra que no se puede comparar con la del
otro lado no se compara: queda en la tabla con veredicto
`no_comparable`. El motivo es `estado_sql:<anterior>_vs_<actual>` si
alguna no tiene valor —el presupuesto de consultas agotado la deja
`no_disponible`— o se midió de otro modo —`calculado` contra
`estimado_motor`—, y `universo_distinto:<anterior>_vs_<actual>` si se
midieron sobre filas distintas: la misma tabla en
`universo = "tabla_completa"` y en `"muestra_motor"` cuenta 3000 filas
en `n` y 400 en `n_validos`, y esa diferencia no es del dato. Un perfil
en memoria cuenta sobre la tabla entera, salvo que sea la muestra de una
base. Dos cifras medidas igual —también dos estimaciones del mismo
motor— se comparan como siempre, salvo que sean de muestras: dos
muestras independientes —dos `muestra_motor` de la misma tabla, o la
muestra de una base contra otra— no describen las mismas filas, y salen
`no_comparable` con el motivo
`universo_distinto:muestras_independientes`, salvo que los dos objetos
declaren la misma muestra (el mismo `checksum` de la materialización, o
la misma consulta de una muestra reproducible). La que la protección
omitió se declara en `campos_protegidos`. Entre perfiles en memoria o
frames `columnas` no hay estado que leer y nada de esto se aplica.

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
#>   columna  campo valor_anterior valor_actual diferencia_normalizada
#> 1   monto  media             10 10.00000....           9.999113e-13
#> 2   monto minimo              1            2                     NA
#> 3   monto   moda              a            b                     NA
#>                veredicto               motivo tipo_eje tolerancia
#> 1            equivalente dentro_de_tolerancia flotante      1e-09
#> 2 materialmente_distinto           eje_exacto   exacto      1e-09
#> 3 materialmente_distinto            eje_valor    valor      1e-09
```

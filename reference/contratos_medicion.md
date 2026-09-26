# Declarar vigencia y escala de medición

`vigencia()` reúne el contrato temporal que las métricas de actualidad y
oportunidad no pueden inferir de los datos: columna de actualización,
fecha de acceso, último cambio conocido, fecha límite, intervalo y
frecuencia esperada. Cada métrica valida los campos que necesita y se
abstiene si faltan: declara en `cobertura_metricas` cuál falta y cómo
declararlo, en lugar de abortar la medición de las demás métricas.

## Usage

``` r
vigencia(
  columna_actualizacion,
  fecha_acceso = Sys.time(),
  fecha_ultimo_cambio = NULL,
  fecha_limite = NULL,
  inicio_intervalo = NULL,
  fin_intervalo = NULL,
  frecuencia_cambio = NULL
)

escala(error, tipo = c("absoluto", "relativo"))
```

## Arguments

- columna_actualizacion:

  Nombre de la columna Date o POSIXt que registra la última
  actualización de cada fila.

- fecha_acceso:

  Momento de acceso usado para estimar actualidad. De la misma clase que
  la columna de actualización: con una columna `Date`,
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) en lugar del
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) por omisión.

- fecha_ultimo_cambio:

  Fecha conocida del último cambio en el mundo real; puede ser escalar o
  tener una entrada por fila.

- fecha_limite:

  Fecha límite escalar o por fila para oportunidad.

- inicio_intervalo, fin_intervalo:

  Extremos del intervalo de vigencia.

- frecuencia_cambio:

  Frecuencia esperada como `difftime` o número de días.

- error:

  Error no negativo escalar, vectorial o función del valor.

- tipo:

  Interpretación `"absoluto"` o `"relativo"` del error.

## Value

`vigencia()` devuelve un objeto `vigencia_datos`; `escala()` devuelve un
objeto `escala_medicion`. Ambos son contratos de configuración y no
examinan datos.

## Details

Las dos puntas de cada comparación tienen que ser de la **misma clase**.
Un `Date` se ancla a la medianoche UTC y un `POSIXct` vale por su
instante, así que mezclarlos puede hacer contar como tarde una
actualización del día del límite, y si el `POSIXct` se leyó sin huso el
resultado depende de la sesión. Cuando una métrica compara clases
distintas lo avisa. Como `fecha_acceso` vale
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) por omisión, con
una columna `Date` conviene declararlo también como `Date`:
`fecha_acceso = Sys.Date()`.

`escala()` declara el error de un instrumento o de otra escala experta.
Con error absoluto, `Escala` calcula `1 - error / abs(valor)` y acota el
resultado a `[0, 1]`; con error relativo calcula `1 - error`. No se
aprende el error de la distribución observada.

## See also

[`metricas_nucleo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)

## Examples

``` r
contrato <- vigencia(
  "actualizado", fecha_limite = as.Date("2026-02-01"),
  frecuencia_cambio = 30, fecha_acceso = as.Date("2026-02-15")
)
instrumento <- escala(error = 0.5, tipo = "absoluto")
```

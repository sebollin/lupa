# Declarar vigencia y escala de medición

`vigencia()` reúne el contrato temporal que las métricas de actualidad y
oportunidad no pueden inferir de los datos: columna de actualización,
fecha de acceso, último cambio conocido, fecha límite, intervalo y
frecuencia esperada. Cada métrica valida los campos que necesita y se
abstiene si faltan.

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

  Momento de acceso usado para estimar actualidad.

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

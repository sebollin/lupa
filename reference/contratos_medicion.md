# Declarar vigencia y escala de medición

`vigencia()` reúne el contrato temporal que las métricas de actualidad y
oportunidad no pueden inferir de los datos: columna de actualización,
fecha de acceso, último cambio conocido, fecha límite, intervalo y
frecuencia esperada. Cada métrica valida los campos que necesita y se
abstiene si faltan.

## Uso

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

## Argumentos

  - columna\_actualizacion:
    
    Nombre de la columna Date o POSIXt que registra la última
    actualización de cada fila.

  - fecha\_acceso:
    
    Momento de acceso usado para estimar actualidad.

  - fecha\_ultimo\_cambio:
    
    Fecha conocida del último cambio en el mundo real; puede ser escalar
    o tener una entrada por fila.

  - fecha\_limite:
    
    Fecha límite escalar o por fila para oportunidad.

  - inicio\_intervalo, fin\_intervalo:
    
    Extremos del intervalo de vigencia.

  - frecuencia\_cambio:
    
    Frecuencia esperada como `difftime` o número de días.

  - error:
    
    Error no negativo escalar, vectorial o función del valor.

  - tipo:
    
    Interpretación `"absoluto"` o `"relativo"` del error.

## Valor

`vigencia()` devuelve un objeto `vigencia_datos`; `escala()` devuelve un
objeto `escala_medicion`. Ambos son contratos de configuración y no
examinan datos.

## Detalles

`escala()` declara el error de un instrumento o de otra escala experta.
Con error absoluto, `Escala` calcula `1 - error / abs(valor)` y acota el
resultado a `[0, 1]`; con error relativo calcula `1 - error`. No se
aprende el error de la distribución observada.

## Ver también

`metricas_nucleo()`, `especializar()`, `cobertura_analisis()`

## Ejemplos

``` r
contrato <- vigencia(
  "actualizado", fecha_limite = as.Date("2026-02-01"),
  frecuencia_cambio = 30, fecha_acceso = as.Date("2026-02-15")
)
instrumento <- escala(error = 0.5, tipo = "absoluto")
```

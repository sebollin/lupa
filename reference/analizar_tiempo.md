# Examinar regularidad y cobertura temporal

Para cada columna temporal propone una frecuencia en días. La confianza
es el mínimo entre la contigüidad de las fechas sobre la grilla
propuesta y la cobertura del período: una coincidencia breve dentro de
una serie muy dispersa no puede producir confianza alta. Ambas
componentes se devuelven para que la propuesta sea auditable. La
propuesta nunca queda confirmada automáticamente. `calendario` usa días
ISO: 1 es lunes y 7 domingo; así una oficina puede declarar `1:5` sin
que la ausencia de fines de semana se interprete como hueco. Las
fechas-hora se llevan a fecha civil en UTC para que el resultado no
dependa de la zona del equipo que ejecuta el análisis.

## Usage

``` r
analizar_tiempo(
  datos,
  perfil = NULL,
  columnas = NULL,
  calendario = 1:7,
  frecuencia_dias = NULL,
  max_huecos = 20L,
  max_columnas = 50L
)
```

## Arguments

- datos:

  Tabla que se desea examinar.

- perfil:

  Perfil opcional de los mismos datos.

- columnas:

  Columnas temporales; `NULL` usa clases e inferencia.

- calendario:

  Días de semana esperados, enteros entre 1 y 7.

- frecuencia_dias:

  Frecuencia entera conocida en días. Si es `NULL`, se propone la moda
  de los intervalos positivos.

- max_huecos:

  Máximo de grupos de huecos devueltos por columna.

- max_columnas:

  Máximo de columnas temporales analizadas.

## Value

Objeto `analisis_temporal` con `resumen`, `dias_semana`, `huecos` y
`propuestas`. El recorte de huecos queda en `resumen`; el de columnas,
en atributos del objeto.

## See also

[`detectar_formatos_fecha()`](https://sebollin.github.io/lupa/reference/detectar_formatos_fecha.md),
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md)

## Examples

``` r
fechas <- as.Date("2026-01-01") + c(0:4, 20:24)
analizar_tiempo(data.frame(fecha = fechas))
#> $resumen
#>   columna n_presentes n_fechas_distintas n_duplicados_temporales fecha_minima
#> 1   fecha          10                 10                       0   2026-01-01
#>   fecha_maxima monotonicidad cobertura_periodo n_fechas_esperadas_ausentes
#> 1   2026-01-25             1               0.4                          15
#>   n_fechas_fuera_calendario n_grupos_huecos huecos_truncados
#> 1                         0               1            FALSE
#> 
#> $dias_semana
#>   columna dia_iso       dia frecuencia proporcion esperado
#> 1   fecha       1     lunes          1        0.1     TRUE
#> 2   fecha       2    martes          0        0.0     TRUE
#> 3   fecha       3 miercoles          1        0.1     TRUE
#> 4   fecha       4    jueves          2        0.2     TRUE
#> 5   fecha       5   viernes          2        0.2     TRUE
#> 6   fecha       6    sabado          2        0.2     TRUE
#> 7   fecha       7   domingo          2        0.2     TRUE
#> 
#> $huecos
#>   columna      desde      hasta n_esperados_ausentes duracion_dias
#> 1   fecha 2026-01-06 2026-01-20                   15            15
#>   frecuencia_dias    calendario
#> 1               1 1,2,3,4,5,6,7
#> 
#> $propuestas
#>   columna frecuencia_dias confianza contiguidad cobertura_periodo    calendario
#> 1   fecha               1       0.4   0.8888889               0.4 1,2,3,4,5,6,7
#>   confirmada                                            evidencia
#> 1      FALSE Moda de intervalos: 1 dias; 9 intervalos observados.
#> 
#> attr(,"class")
#> [1] "analisis_temporal"
#> attr(,"columnas_analizadas")
#> [1] "fecha"
#> attr(,"columnas_omitidas")
#> character(0)
#> attr(,"truncado")
#> [1] FALSE
```

# Llevar estimaciones ya calculadas al contrato de medición

`lupa` **no estima**: eso necesita un diseño muestral, estimación de
varianza y otra disciplina. Lo que sabe hacer es evaluar contra un marco
declarado. Esta función recibe estimaciones calculadas por otra
herramienta —`survey`, el paquete
[`calidad`](https://github.com/inesscc/calidad) del INE de Chile, o
cualquier otra— y las convierte en una medición que
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md)
entiende.

## Usage

``` r
medicion_desde_estimaciones(
  estimaciones,
  entidad,
  fuente,
  atributo = NULL,
  columnas = NULL,
  fecha = Sys.time()
)
```

## Arguments

- estimaciones:

  Data frame con una fila por estimación y una columna por estadístico.
  Los nombres reconocidos son los de
  [`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md);
  se puede renombrar con `columnas`.

- entidad:

  Nombre de la entidad estimada, por ejemplo el tabulado o la población
  de referencia.

- fuente:

  Texto que declara quién calculó las estimaciones. Es obligatorio: sin
  él, el resultado no dice de dónde viene.

- atributo:

  Columna opcional de `estimaciones` que nombra el atributo o la celda
  estimada. Cuando falta, se numeran las filas.

- columnas:

  Vector con nombres para traducir columnas de `estimaciones` a
  estadísticos reconocidos, en la forma `c(cv = "coef_var")`.

- fecha:

  Fecha y hora de la medición.

## Value

Data frame `medicion_calidad` con el contrato de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md), más las
columnas `fuente` y `unidad`.

## Details

Cada estadístico se convierte en **una medida canónica propia**, con su
métrica, su tipo y su orientación, porque los siete tienen unidades y
dominios distintos: un coeficiente de variación y un tamaño de muestra
no se evalúan con la misma regla. Los reconocidos están en
[`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md).

Cada medida declara su procedencia en `fuente`, de modo que nadie lea el
resultado como si `lupa` lo hubiera calculado. Los estadísticos que la
tabla no traiga simplemente no producen medidas: no se rellenan con
ceros ni se estiman.

## See also

[`estadisticos_estimacion()`](https://sebollin.github.io/lupa/reference/estadisticos_estimacion.md),
[`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md),
[`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)

## Examples

``` r
estimaciones <- data.frame(
  celda = c("Montevideo", "Interior"),
  stat = c(0.42, 0.38),
  cv = c(0.08, 0.34),
  n = c(1200L, 90L)
)
medicion_desde_estimaciones(
  estimaciones, entidad = "ech2024", atributo = "celda",
  fuente = "survey 4.4, diseno complejo declarado por el equipo"
)
#>                                                        id_medida
#> 1           estimaciones-20260825T220706-ech2024-Estimacion-0001
#> 2           estimaciones-20260825T220706-ech2024-Estimacion-0002
#> 3 estimaciones-20260825T220706-ech2024-CoeficienteVariacion-0001
#> 4 estimaciones-20260825T220706-ech2024-CoeficienteVariacion-0002
#> 5        estimaciones-20260825T220706-ech2024-TamanoMuestra-0001
#> 6        estimaciones-20260825T220706-ech2024-TamanoMuestra-0002
#>                            id_medicion               fecha              metrica
#> 1 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06           Estimacion
#> 2 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06           Estimacion
#> 3 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06 CoeficienteVariacion
#> 4 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06 CoeficienteVariacion
#> 5 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06        TamanoMuestra
#> 6 estimaciones-20260825T220706-ech2024 2026-08-25 22:07:06        TamanoMuestra
#>     metrica_especifica          metrica_instanciada dimension
#> 1           Estimacion           Estimacion@ech2024 Precision
#> 2           Estimacion           Estimacion@ech2024 Precision
#> 3 CoeficienteVariacion CoeficienteVariacion@ech2024 Precision
#> 4 CoeficienteVariacion CoeficienteVariacion@ech2024 Precision
#> 5        TamanoMuestra        TamanoMuestra@ech2024 Precision
#> 6        TamanoMuestra        TamanoMuestra@ech2024 Precision
#>                 factor orientacion      granularidad tipo_resultado entidad
#> 1           Estimacion   no_aplica conjuntoEntidades           real ech2024
#> 2           Estimacion   no_aplica conjuntoEntidades           real ech2024
#> 3 CoeficienteVariacion     defecto conjuntoEntidades           real ech2024
#> 4 CoeficienteVariacion     defecto conjuntoEntidades           real ech2024
#> 5        TamanoMuestra conformidad conjuntoEntidades         entero ech2024
#> 6        TamanoMuestra conformidad conjuntoEntidades         entero ech2024
#>     atributo fila     objeto_medible resultado agregacion
#> 1 Montevideo    1 ech2024$Montevideo      0.42       <NA>
#> 2   Interior    2   ech2024$Interior      0.38       <NA>
#> 3 Montevideo    1 ech2024$Montevideo      0.08       <NA>
#> 4   Interior    2   ech2024$Interior      0.34       <NA>
#> 5 Montevideo    1 ech2024$Montevideo   1200.00       <NA>
#> 6   Interior    2   ech2024$Interior     90.00       <NA>
#>                    unidad                                              fuente
#> 1 unidad de la estimacion survey 4.4, diseno complejo declarado por el equipo
#> 2 unidad de la estimacion survey 4.4, diseno complejo declarado por el equipo
#> 3              proporcion survey 4.4, diseno complejo declarado por el equipo
#> 4              proporcion survey 4.4, diseno complejo declarado por el equipo
#> 5                   casos survey 4.4, diseno complejo declarado por el equipo
#> 6                   casos survey 4.4, diseno complejo declarado por el equipo
```

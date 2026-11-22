# Semáforo de calidad por tipo de indicador

Clasifica cada celda del tablero —una métrica sobre un objeto— en
niveles, con cortes que declara quien mide: por dimensión del marco y,
si hace falta, por métrica. **Sólo pinta una cifra cuyas cuentas
cierran**: lo que el resumen de
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md) o de
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
declara en `estado_cuentas`. Una cifra que no cierra —lo no evaluado no
son los vacíos que la métrica declara no evaluar, o la tabla cambió
entre dos lecturas de la corrida—, una calculada sobre una parte (un
vocabulario truncado por `max_valores`) y una sin cuentas de su universo
quedan sin color, con su motivo.

## Usage

``` r
semaforo_calidad(
  medidas,
  cortes,
  niveles = c("rojo", "amarillo", "verde"),
  marco = NULL,
  cobertura = NULL
)
```

## Arguments

- medidas:

  Un resumen de medición: `medir(detalle = "resumen")`,
  [`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)
  o la consolidación `rbind(..., prioridad = )`. Una medición completa,
  un agregado o un tablero no traen las cuentas que el semáforo necesita
  y se rechazan con clase `lupa_semaforo_sin_cuentas`.

- cortes:

  Lista con nombres de dimensión o `data.frame`, como arriba. Sin valor
  por omisión.

- niveles:

  Nombres de los niveles, de peor a mejor.

- marco, cobertura:

  Como en
  [`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md).

## Value

Data frame de clase `semaforo_calidad`, una fila por celda del tablero,
con las columnas del tablero más `metrica_especifica`,
`valor_comparado`, `escala_comparada`, `regla`, `desde`, `hasta`,
`nivel`, `motivo_sin_color`, `fraccion_evaluada`, `estado_cuentas`,
`motivo_cuentas`, `diferencia_cuentas`, `control_lecturas` y, si el
resumen es una consolidación, `fuente`. `control_lecturas` dice cuánto
respalda el control de lecturas a la cifra: `una_lectura` (la tabla se
leyó en una sola sentencia, o en memoria), `completo` (varias lecturas
con la instantánea probada por el contador del motor o la huella de los
datos) o `limitado` (varias lecturas y alguna sin instantánea: sólo se
comparó el número de filas, que no ve un `UPDATE` que no lo cambia). Una
celda con color y control `limitado` lo dice también la impresión.
Atributos: `cortes`, `niveles`, `comparacion`, `conteo_niveles`,
`cortes_sin_celdas`, `fuera_del_semaforo` y los del tablero.

## Details

No hay cortes de fábrica ni una cifra que combine dimensiones: no hay
«peor color» de una tabla ni total. `conteo_niveles` es una distribución
por dimensión.

## Cuándo una celda tiene color

Todas a la vez:

- sus cuentas cierran (`estado_cuentas == "cierran"`). Una métrica que
  da una cifra por objeto sin cuentas de su universo —un estadístico,
  una métrica propia de granularidad `entidad`— no se pinta aunque su
  resultado esté en `[0, 1]`: sin universo no hay cómo saber qué
  representa;

- el resultado es finito (un denominador cero queda `sin_denominador`,
  sin abortar);

- es una proporción: la orientación operativa del tablero no es
  `no_aplica` (por tipo, unidad o valores fuera de `[0, 1]`);

- hay un corte para ella.

La métrica evalúa **su** universo: una de validez evalúa los valores
presentes y los vacíos son de la completitud, que tiene su métrica. Por
eso una columna de validez con vacíos tiene color si evaluó todos sus
valores, y `fraccion_evaluada` se calcula sobre ese universo propio.

Las métricas `no_resumible` (días o cambios de desactualización por
celda) no tienen celda: ningún corte en `[0, 1]` las juzga. Quedan en
`fuera_del_semaforo` con su motivo, y el semáforo de las demás no
aborta.

## Los cortes

`cortes` en dos formas:

- una lista con nombres de dimensión, cada elemento un vector de
  `length(niveles) - 1` cortes estrictamente crecientes en `[0, 1]`:
  `list(Completitud = c(0.80, 0.95), Unicidad = c(0.99, 1))`;

- un `data.frame` con `por` (`"dimension"`, `"metrica"`,
  `"metrica_especifica"` o `"metrica_instanciada"`), `nombre` y una
  columna por nivel que abre, con el nombre del nivel (con los niveles
  por omisión, `amarillo` y `verde`).

Manda la regla más específica: instanciada, específica, genérica,
dimensión. Una clave sin celdas en esta medición no es un error —una
política de cortes se reusa entre muchas tablas— y se declara en
`cortes_sin_celdas`; sí lo es una que coincide con un nombre existente
salvo por la caja o la forma Unicode.

La comparación es `valor_comparado >= corte` con la tolerancia de
redondeo de `ratio_umbral` en
[`agregar()`](https://sebollin.github.io/lupa/reference/agregar.md);
`valor_comparado` es `valor` o `1 - valor` en las métricas de defecto,
la convención de
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
y
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md).
Las bandas se publican en la escala de `valor` (`desde`, `hasta`) y la
escala comparada en `escala_comparada`.

## See also

[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md),
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`medir_dbi()`](https://sebollin.github.io/lupa/reference/medir_dbi.md)

## Examples

``` r
nucleo <- metricas_nucleo()
modelo_ejemplo <- modelo(
  instanciar(especializar(nucleo$NoNulo), "personas", "edad"),
  instanciar(especializar(nucleo$ValoresPosiblesPorComprension,
                          minimo = 0, maximo = 120), "personas", "edad")
)
resumen <- medir(modelo_ejemplo,
                 list(personas = data.frame(edad = c(20, NA, 35, 150))),
                 detalle = "resumen")
# Cortes de este ejemplo, no del paquete:
semaforo_calidad(resumen, cortes = list(
  Completitud = c(0.80, 0.95),
  Consistencia = c(0.70, 0.90)
))
#> 
#> ── Semáforo de calidad ─────────────────────────────────────────────────────────
#>     dimension                         metrica_instanciada objeto     valor
#>   Completitud                        NoNulo@personas.edad   edad 0.7500000
#>  Consistencia ValoresPosiblesPorComprension@personas.edad   edad 0.6666667
#>  escala_comparada                  regla nivel motivo_sin_color
#>             valor  dimension:Completitud  rojo             <NA>
#>             valor dimension:Consistencia  rojo             <NA>
#>  control_lecturas
#>       una_lectura
#>       una_lectura
#> 
#> ── Por dimensión ──
#> 
#>     dimension rojo amarillo verde sin_color sin_medir fuera_del_semaforo
#>   Completitud    1        0     0         0         0                  0
#>  Consistencia    1        0     0         0         0                  0
```

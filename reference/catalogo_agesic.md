# Correspondencia con el catálogo de métricas de AGESIC

Devuelve las 49 entradas del catálogo del marco y explicita qué parte
está disponible en `lupa`. Los ratios no se duplican como métricas:
aparecen con estado `"via_agregacion"` y con la llamada que los
materializa.

## Uso

``` r
catalogo_agesic()
```

## Valor

Data frame con una fila por entrada y las columnas `numero`,
`dimension`, `factor`, `metrica_agesic`, `clase_catalogo`, `estado`,
`motivo`, `metrica_lupa`, `implementacion` y `observacion`. `estado` y
`motivo` son factores.

## Detalles

`estado` responde qué disponibilidad tiene cada entrada:

  - `implementada`: existe un motor ejecutable, completo o parcial;

  - `via_agregacion`: se obtiene agregando una métrica base;

  - `pendiente`: es automatizable, pero el motor necesario aún no
    existe;

  - `fuera_de_alcance`: se decidió no implementarla en el alcance
    tabular de esta versión, por ejemplo métricas geográficas o de
    imágenes.

`motivo` separa la causa o el matiz: `semantica_completa`,
`semantica_parcial`, `agregacion`, `requiere_referencial`,
`requiere_configuracion`, `motor_pendiente` o `decision_alcance`. Así
una métrica ejecutable que necesita un padrón externo no se confunde con
una métrica cuyo motor falta. `observacion` explica la situación
concreta de cada una de las 49 entradas y nunca queda vacía.

`Escala` se clasifica como implementada con configuración experta
mediante `escala()`, no como referencial. `DesactualizacionPorFecha`,
`DesactualizacionPorCambios` y las oportunidades de entidad requieren un
contrato `vigencia()`. `ErrorEstandar` sigue la semántica literal de la
tabla 16.5 y devuelve desviación estándar, aunque su nombre pueda
sugerir el error estándar de la media.

La implementación de `ReglaIntegridadInterEntidad` se declara parcial:
calcula cobertura PK/FK, pero todavía no materializa la regla booleana
de inclusión o expresión condicional con granularidad
`conjuntoEntidades` que define el marco. `OportunidadAtributo*`, en
cambio, sigue las tablas 16.29 y 16.30 con resultado booleano. Las
variantes continuas del curso CPAP se conservan como
`GradoOportunidadAtributo*` y no figuran como entradas del catálogo.
`RatioDensidadPonderada` usa `ratio_umbral`, porque su medida base es
real y `ratio` sólo es válido para medidas booleanas.

## Referencias

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6, capítulo 16, Presidencia de la República,
Uruguay.

## Ver también

`metricas_nucleo()`, `metricas_referencial()`, `agregar()`

## Ejemplos

``` r
catalogo <- catalogo_agesic()
subset(catalogo, estado == "via_agregacion")
#>    numero    dimension                   factor                  metrica_agesic
#> 3       3    Exactitud    Correctitud semántica       RatioCorrectitudSemFuerte
#> 4       4    Exactitud    Correctitud semántica        RatioCorrectitudSemDébil
#> 21     21 Consistencia Integridad intra-entidad     RatioIntegridadIntraEntidad
#> 30     30  Completitud                 Densidad                    RatioNoNulos
#> 31     31  Completitud                 Densidad          RatioDensidadPonderada
#> 37     37     Unicidad           No-duplicación          RatioAtributoDuplicado
#> 38     38     Unicidad           No-duplicación RatioConjuntoAtributosDuplicado
#> 39     39     Unicidad           No-duplicación        RatioEntidadesDuplicadas
#>    clase_catalogo         estado     motivo                metrica_lupa
#> 3        agregada via_agregacion agregacion        CorrectitudSemFuerte
#> 4        agregada via_agregacion agregacion         CorrectitudSemDebil
#> 21       agregada via_agregacion agregacion ReglaIntegridadIntraEntidad
#> 30       agregada via_agregacion agregacion                      NoNulo
#> 31       agregada via_agregacion agregacion           DensidadPonderada
#> 37       agregada via_agregacion agregacion           AtributoDuplicado
#> 38       agregada via_agregacion agregacion  ConjuntoAtributosDuplicado
#> 39       agregada via_agregacion agregacion            EntidadDuplicada
#>                                       implementacion
#> 3                    agregar(m, "atributo", "ratio")
#> 4                    agregar(m, "atributo", "ratio")
#> 21                    agregar(m, "entidad", "ratio")
#> 30                   agregar(m, "atributo", "ratio")
#> 31 agregar(m, "entidad", "ratio_umbral", umbral = u)
#> 37                   agregar(m, "atributo", "ratio")
#> 38                    agregar(m, "entidad", "ratio")
#> 39                    agregar(m, "entidad", "ratio")
#>                                                       observacion
#> 3                Se obtiene con Ratio sobre CorrectitudSemFuerte.
#> 4                 Se obtiene con Ratio sobre CorrectitudSemDebil.
#> 21        Se obtiene con Ratio sobre ReglaIntegridadIntraEntidad.
#> 30                             Se obtiene con Ratio sobre NoNulo.
#> 31 Usa RatioUmbral porque DensidadPonderada es real, no booleana.
#> 37                  Se obtiene con Ratio sobre AtributoDuplicado.
#> 38         Se obtiene con Ratio sobre ConjuntoAtributosDuplicado.
#> 39                   Se obtiene con Ratio sobre EntidadDuplicada.
table(catalogo$estado, catalogo$motivo)
#>                   
#>                    semantica_completa semantica_parcial agregacion
#>   implementada                      6                 1          0
#>   via_agregacion                    0                 0          8
#>   pendiente                         0                 0          0
#>   fuera_de_alcance                  0                 0          0
#>                   
#>                    requiere_referencial requiere_configuracion motor_pendiente
#>   implementada                        3                     18               0
#>   via_agregacion                      0                      0               0
#>   pendiente                           0                      0               3
#>   fuera_de_alcance                    0                      0               0
#>                   
#>                    decision_alcance
#>   implementada                    0
#>   via_agregacion                  0
#>   pendiente                       0
#>   fuera_de_alcance               10
```

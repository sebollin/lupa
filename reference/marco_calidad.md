# Declarar una taxonomía de calidad de datos

Un `marco_calidad` enumera las dimensiones y factores contra los que se
interpreta la cobertura de un análisis. No contiene métricas
instanciadas: esa función sigue correspondiendo a `modelo()`. Tampoco es
un catálogo de métricas; `catalogo_agesic()` conserva esa
correspondencia específica.

## Uso

``` r
marco_calidad(nombre, factores)

marco_agesic()

marco_iso25012()
```

## Argumentos

  - nombre:
    
    Nombre del marco.

  - factores:
    
    Data frame con `dimension` y `factor`, o lista con nombres. El data
    frame puede añadir estos campos de contrato:
    
      - `como_resolverlo`: instrucción que muestra
        `cobertura_analisis()` cuando el factor todavía no fue medido;
    
      - `perfil_mide`: lógico que declara si el profiling por sí solo
        aporta una medición suficiente del factor. No ejecuta métricas
        ni se infiere del nombre; su valor predeterminado es `FALSE`;
    
      - `aplicabilidad`: `"siempre"`, `"temporal"` o `"geometria"`. Las
        dos últimas permiten informar `"no_aplica"` cuando el perfil no
        contiene columnas temporales o geometrías, respectivamente;
    
      - `disponibilidad`: `"disponible"` o `"fuera_de_alcance"`. Esta
        última declara una limitación del motor y no puede combinarse
        con `perfil_mide = TRUE`.
    
    Las columnas adicionales se conservan como metadatos y no cambian
    por sí solas la cobertura. Si se usa una lista, `como_resolverlo`
    recibe una instrucción genérica, `perfil_mide = FALSE`,
    `aplicabilidad = "siempre"` y `disponibilidad = "disponible"` para
    todas las filas.

## Valor

`marco_calidad()`, `marco_agesic()` y `marco_iso25012()` devuelven un
objeto S3 `marco_calidad`. `as.data.frame()` devuelve su tabla de
factores.

## Detalles

`marco_agesic()` devuelve la taxonomía incluida de fábrica. Un marco
propio puede construirse con un data frame o con una lista cuyos nombres
son dimensiones y cuyos valores son factores. La forma lista aplica a
todas las filas los valores predeterminados descritos abajo; en
particular, `perfil_mide = FALSE`.

`marco_iso25012()` adapta las dos perspectivas de ISO/IEC 25012:2008 a
la estructura dimensión-factor. Usa tres grupos disjuntos como
dimensiones: características inherentes, características inherentes y
dependientes del sistema, y características dependientes del sistema.
Este agrupamiento es una representación operativa para `lupa`, no afirma
que la norma defina una jerarquía dimensión-factor. Los nombres de las
quince características y su clasificación siguen la norma; las
descripciones son redacción propia. Las quince filas declaran
`perfil_mide = FALSE`: a diferencia de la política incluida para dos
factores de AGESIC, el profiling genérico no demuestra por sí solo que
una característica ISO satisfaga el uso declarado.

## Referencias

[ISO/IEC (2008)](https://www.iso.org/standard/35736.html). *ISO/IEC
25012:2008 Software engineering — Software product Quality Requirements
and Evaluation (SQuaRE) — Data quality model*.
<https://www.iso.org/standard/35736.html>.

## Ver también

`catalogo_agesic()`, `modelo()`, `cobertura_analisis()`

## Ejemplos

``` r
propio <- marco_calidad("Marco operativo", list(
  Trazabilidad = c("Origen documentado", "Linaje reproducible"),
  Pertinencia = "Adecuación al uso"
))
propio
#> 
#> ── Marco operativo ──
#> 
#> Dimensiones: 2
#> Factores: 3
#> Origen: usuario
as.data.frame(propio)
#>      dimension              factor
#> 1 Trazabilidad  Origen documentado
#> 2 Trazabilidad Linaje reproducible
#> 3  Pertinencia   Adecuación al uso
#>                                     como_resolverlo perfil_mide aplicabilidad
#> 1 Declarar e instanciar una métrica de este factor.       FALSE       siempre
#> 2 Declarar e instanciar una métrica de este factor.       FALSE       siempre
#> 3 Declarar e instanciar una métrica de este factor.       FALSE       siempre
#>   disponibilidad
#> 1     disponible
#> 2     disponible
#> 3     disponible
marco_agesic()
#> 
#> ── Marco de calidad de datos de AGESIC ──
#> 
#> Dimensiones: 5
#> Factores: 17
#> Origen: AGESIC 2020, versión 1.6
iso <- marco_iso25012()
table(as.data.frame(iso)$dimension)
#> 
#>             Dependiente del sistema                           Inherente 
#>                                   3                                   5 
#> Inherente y dependiente del sistema 
#>                                   7 
```

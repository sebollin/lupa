# Declarar una taxonomía de calidad de datos

Un `marco_calidad` enumera las dimensiones y factores contra los que se
interpreta la cobertura de un análisis. No contiene métricas
instanciadas: esa función sigue correspondiendo a
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).
Tampoco es un catálogo de métricas;
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
conserva esa correspondencia específica.

## Usage

``` r
marco_calidad(nombre, factores)

marco_agesic()

marco_iso25012()

marco_cepal()
```

## Arguments

- nombre:

  Nombre del marco.

- factores:

  Data frame con `dimension` y `factor`, o lista con nombres. El data
  frame puede añadir estos campos de contrato:

  - `como_resolverlo`: instrucción que muestra
    [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
    cuando el factor todavía no fue medido;

  - `perfil_mide`: lógico que declara si el profiling por sí solo aporta
    una medición suficiente del factor. No ejecuta métricas ni se
    infiere del nombre; su valor predeterminado es `FALSE`;

  - `aplicabilidad`: `"siempre"`, `"temporal"` o `"geometria"`. Las dos
    últimas permiten informar `"no_aplica"` cuando el perfil no contiene
    columnas temporales o geometrías, respectivamente;

  - `disponibilidad`: `"disponible"` o `"fuera_de_alcance"`. Esta última
    declara una limitación del motor y no puede combinarse con
    `perfil_mide = TRUE`.

  Las columnas adicionales se conservan como metadatos y no cambian por
  sí solas la cobertura. Si se usa una lista, `como_resolverlo` recibe
  una instrucción genérica, `perfil_mide = FALSE`,
  `aplicabilidad = "siempre"` y `disponibilidad = "disponible"` para
  todas las filas.

## Value

`marco_calidad()`, `marco_agesic()`, `marco_iso25012()` y
`marco_cepal()` devuelven un objeto S3 `marco_calidad`.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) devuelve
su tabla de factores.

## Details

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

`marco_cepal()` representa los cuatro niveles y los diecinueve
principios del marco nacional de aseguramiento de la calidad de las
Naciones Unidas, adoptados y adaptados para América Latina y el Caribe
por la CEA/CEPAL. La columna `principio` conserva la numeración de la
fuente. Los nombres de los niveles y principios son textuales; las
descripciones y las instrucciones `como_resolverlo` de este paquete son
redacción propia. Los principios 1 a 13 quedan fuera del alcance de una
tabla: se refieren al sistema, al entorno institucional o al proceso
estadístico. Los principios 14 a 19 describen productos estadísticos y
quedan disponibles, aunque ninguno se considera medido por el profiling
genérico.

## References

[ISO/IEC (2008)](https://www.iso.org/standard/35736.html). *ISO/IEC
25012:2008 Software engineering — Software product Quality Requirements
and Evaluation (SQuaRE) — Data quality model*.
<https://www.iso.org/standard/35736.html>.

[Naciones Unidas
(2019)](https://unstats.un.org/unsd/methodology/dataquality/). *Manual
del marco nacional de aseguramiento de calidad en las estadísticas
oficiales*. Estudios en Métodos, serie M, N° 100
(ST/ESA/STAT/SER.M/100), Nueva York.

[Grupo de Trabajo de la Conferencia Estadística de las Américas (CEA),
coordinado por Colombia (DANE) y México (INEGI), Secretaría Técnica:
División de Estadísticas de la CEPAL
(2022)](https://repositorio.cepal.org/handle/11362/47464). *Guía para la
implementación del marco de aseguramiento de la calidad para procesos y
productos estadísticos*. LC/CEA.11/19. Comisión Económica para América
Latina y el Caribe (CEPAL), Naciones Unidas, Santiago.

## See also

[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md),
[`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)

## Examples

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
marco_cepal()
#> 
#> ── Marco de aseguramiento de la calidad estadística de Naciones Unidas, adaptado por CEA/CEPAL ──
#> 
#> Dimensiones: 4
#> Factores: 19
#> Origen: Marco de Naciones Unidas (2019): Manual del marco nacional de
#> aseguramiento de calidad en las estadísticas oficiales, Estudios en Métodos,
#> serie M, N° 100 (ST/ESA/STAT/SER.M/100), Nueva York. Adaptación regional
#> CEA/CEPAL: Grupo de Trabajo de la Conferencia Estadística de las Américas
#> (CEA), coordinado por Colombia (DANE) y México (INEGI), Secretaría Técnica:
#> División de Estadísticas de la CEPAL (2022), Guía para la implementación del
#> marco de aseguramiento de la calidad para procesos y productos estadísticos
#> (LC/CEA.11/19), Comisión Económica para América Latina y el Caribe (CEPAL),
#> Naciones Unidas, Santiago. Fuentes:
#> https://unstats.un.org/unsd/methodology/dataquality/;
#> https://repositorio.cepal.org/handle/11362/47464
```

# lupa: perfilar y medir la calidad de datos con evidencia

`lupa` examina una tabla o una base de datos antes de confiar en ella:
encuentra faltantes disfrazados, el mismo dato escrito de varias formas,
fechas en formatos mezclados, filas y columnas repetidas y estructura
que nadie declaró; dice en qué filas está cada problema y qué controles
no pudo hacer. Cada resultado conserva su alcance y su evidencia, y
ningún diagnóstico modifica los datos que recibe.

## Details

La puerta de entrada es
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).
Sus hallazgos
([`hallazgos()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md))
traen tipo, severidad (`ok < sospechoso < error`), evidencia y
trazabilidad hasta las filas; su cobertura
([`cobertura()`](https://sebollin.github.io/lupa/reference/accesores_perfil.md))
enumera los diagnósticos que no se pudieron evaluar, con su motivo, y
que nunca cuentan como cero. La viñeta "Empezar en diez minutos"
([`vignette("lupa")`](https://sebollin.github.io/lupa/articles/lupa.md))
recorre el primer uso, y "Leer un perfil"
([`vignette("leer-un-perfil")`](https://sebollin.github.io/lupa/articles/leer-un-perfil.md))
explica cada parte.

Desde el perfil, el recorrido sigue estas etapas, cada una con su guía:

1.  explorar columnas y reunir el diagnóstico en un objeto con
    [`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md),
    y compartirlo en un HTML autocontenido con
    [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md);

2.  perfilar dentro de una base con
    [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md),
    o una colección de tablas con
    [`perfilar_coleccion()`](https://sebollin.github.io/lupa/reference/perfilar_coleccion.md);

3.  limpiar una copia con un plan editable:
    [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
    y
    [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md);

4.  convertir lo confirmado en mediciones explícitas y evaluarlas:
    [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md),
    [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md),
    [`medir()`](https://sebollin.github.io/lupa/reference/medir.md) y
    [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md);

5.  comparar entregas y seguir la calidad en el tiempo con
    [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
    y
    [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md).

Un marco de calidad es una taxonomía que se declara con
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
El paquete incluye tres,
[`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
[`marco_cepal()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
y
[`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
y ninguno restringe las declaraciones de quien lo usa: un factor se mide
cuando los datos aportan evidencia o cuando se declaran el requisito y
la métrica.
[`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
informa qué factores quedaron medidos. No hay un puntaje de fábrica:
[`indice_calidad()`](https://sebollin.github.io/lupa/reference/indice_calidad.md)
calcula un índice sólo con los pesos que declara el proyecto.

## References

Batini C, Scannapieco M (2016)
[doi:10.1007/978-3-319-24106-7](https://doi.org/10.1007/978-3-319-24106-7)
. *Data and Information Quality: Dimensions, Principles and Techniques*.
Springer.

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/comunicacion/publicaciones/marco-trabajo-para-gestion-calidad-datos).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6.

[CEA/CEPAL (2022)](https://repositorio.cepal.org/handle/11362/47464).
*Guía para la implementación del marco de aseguramiento de la calidad
para procesos y productos estadísticos*. LC/CEA.11/19. Santiago.

[ISO/IEC (2008)](https://www.iso.org/standard/35736.html). *ISO/IEC
25012:2008 Software engineering — Software product Quality Requirements
and Evaluation (SQuaRE) — Data quality model*.

## See also

[datos_operativos](https://sebollin.github.io/lupa/reference/datos_operativos.md),
[datos_administrativos](https://sebollin.github.io/lupa/reference/datos_administrativos.md)

## Author

**Maintainer**: Sebastián Lucas <sebalucas@gmail.com>
([ORCID](https://orcid.org/0009-0009-9068-0276)) \[copyright holder\]

Authors:

- Sebastián Lucas <sebalucas@gmail.com>
  ([ORCID](https://orcid.org/0009-0009-9068-0276)) \[copyright holder\]

Other contributors:

- Robyn Speer (byte tables, character categories, regular expressions
  and repair functions of the encoding-repair engine, translated from
  ftfy 6.3.1 (Apache-2.0)) \[contributor, copyright holder\]

- Nicholas Tierney (missing-value sentinel values from naniar 1.1.0
  (MIT)) \[contributor, copyright holder\]

## Examples

``` r
perfil <- perfilar(datos_operativos)
subset(hallazgos(perfil), severidad == "error",
       c(columna, tipo_hallazgo, evidencia))
#>           columna         tipo_hallazgo
#> 3  codigo_usuario faltantes_disfrazados
#> 5    fecha_evento faltantes_disfrazados
#> 6    fecha_evento formatos_fecha_mixtos
#> 9           canal faltantes_disfrazados
#> 20           <NA>      filas_duplicadas
#>                                                             evidencia
#> 3                                                             S/D (1)
#> 5                                                            NULL (1)
#> 6  %Y-%m-%d (4); %d/%m/%Y (4); %Y%m%d (1); %Y/%m/%d (1); %d-%m-%Y (1)
#> 9                                               <blanco> (1); S/D (1)
#> 20                         2 filas en grupos duplicados (1 excedente)
cobertura(perfil)[, c("diagnostico", "columna")]
#>              diagnostico      columna
#> 1               outliers  id_registro
#> 2               outliers fecha_evento
#> 3   resumen_cuantitativo fecha_evento
#> 4            patron_raro fecha_evento
#> 5               outliers        monto
#> 6               outliers     id_copia
#> 7 proximidad_vocabulario         zona
#> 8 proximidad_vocabulario     contacto
#> 9 proximidad_vocabulario    id_evento
```

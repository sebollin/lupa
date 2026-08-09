# lupa: examinar, medir y mejorar la calidad de datos

`lupa` implementa un modelo de calidad de datos de uso general:
dimensiones y factores declarables, métricas con granularidad explícita,
agregación tipada y una cadena de evaluación auditable. El paquete nunca
modifica datos como efecto del diagnóstico: cada etapa devuelve objetos
inspeccionables. `marco_agesic()` y `catalogo_agesic()` aportan de
fábrica la implementación trazable del marco uruguayo, y
`marco_iso25012()` ofrece una segunda taxonomía opcional. Ninguna
restringe las declaraciones del usuario.

## Detalles

El punto de entrada es `analizar()`. En una llamada reúne el diagnóstico
descriptivo y su cobertura, sin medir requisitos observados
automáticamente. Sus componentes también se pueden construir por
separado. El recorrido es:

1.  examinar estructura, tipos, patrones, ausencias, distribuciones,
    asociaciones y comportamiento temporal; `cobertura_analisis()`
    explicita qué factores no fueron evaluados;

2.  convertir el diagnóstico en una propuesta editable con
    `proponer_modelo()`;

3.  declarar y ejecutar métricas mediante `modelo()` y `medir()`;

4.  evaluar reglas y perfiles de madurez con `evaluar()`;

5.  planificar y aplicar mejoras auditables mediante
    `planificar_limpieza()` y `aplicar()`;

6.  acumular corridas y detectar deriva con `historico_calidad()`,
    `detectar_deriva_calidad()` y `comparar_perfiles()`;

7.  persistir el recorrido con `guardar_analisis()` y producir un
    archivo HTML autocontenido con `reportar()`.

Las taxonomías se declaran con `marco_calidad()`. Los padrones externos
se declaran con `referencial()`, y los contratos que no se pueden
inferir se expresan con `vigencia()` y `escala()`. La correspondencia
exacta con las 49 entradas de AGESIC se consulta en `catalogo_agesic()`.
No se calcula un índice global: la jerarquía dimensión–factor–métrica es
taxonómica y requiere un contrato adicional para producirlo. Los
formatos se pueden verificar con funciones propias o con los packs de
`validadores_internacionales()` y `validadores_uruguay()`;
`pack_validadores()` permite añadir otro país sin registrar estado
global.

## Referencias

Batini C, Scannapieco M (2016)
[doi:10.1007/978-3-319-24106-7](https://doi.org/10.1007/978-3-319-24106-7)
. *Data and Information Quality: Dimensions, Principles and Techniques*.
Springer.

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6, Presidencia de la República, Uruguay.

[ISO/IEC (2008)](https://www.iso.org/standard/35736.html). *ISO/IEC
25012:2008 Software engineering — Software product Quality Requirements
and Evaluation (SQuaRE) — Data quality model*.

## Ver también

[datos\_operativos](https://sebollin.github.io/lupa/reference/datos_operativos.md),
[datos\_administrativos](https://sebollin.github.io/lupa/reference/datos_administrativos.md)

## Autor-a

**Maintainer**: Sebastián Lucas <sebalucas@gmail.com>

Authors:

  - Sebastián Lucas <sebalucas@gmail.com>

Other contributors:

  - Robyn Speer \[copyright holder\]

  - Nicholas Tierney \[copyright holder\]

  - Di Cook \[copyright holder\]

  - Miles McBain \[copyright holder\]

  - Colin Fay \[copyright holder\]

## Ejemplos

``` r
resultado <- analizar(datos_operativos, analizar_dependencias = FALSE)
subset(resultado$perfil$hallazgos, severidad != "ok")
#>           columna             tipo_hallazgo  severidad
#> 1  codigo_usuario         alta_cardinalidad sospechoso
#> 2  codigo_usuario     faltantes_disfrazados      error
#> 3    fecha_evento         alta_cardinalidad sospechoso
#> 4    fecha_evento     faltantes_disfrazados      error
#> 5    fecha_evento     formatos_fecha_mixtos      error
#> 6    fecha_evento   tipo_declarado_distinto sospechoso
#> 7           canal         alta_cardinalidad sospechoso
#> 8           canal                 faltantes sospechoso
#> 9           canal     faltantes_disfrazados      error
#> 10          canal        espacios_sobrantes sospechoso
#> 11          canal mayusculas_inconsistentes sospechoso
#> 12          monto     faltantes_disfrazados sospechoso
#> 13          monto                  outliers sospechoso
#> 14        sistema                 constante sospechoso
#> 15       contacto         alta_cardinalidad sospechoso
#> 17           <NA>          filas_duplicadas      error
#> 18    id_registro       columnas_duplicadas sospechoso
#>                                                              descripcion
#> 1                         La columna categórica tiene alta cardinalidad.
#> 2    Hay valores que representan ausencia sin estar codificados como NA.
#> 3                         La columna categórica tiene alta cardinalidad.
#> 4    Hay valores que representan ausencia sin estar codificados como NA.
#> 5                     Conviven dos o más formatos de fecha o fecha-hora.
#> 6         El tipo declarado no coincide con el tipo implícito dominante.
#> 7                         La columna categórica tiene alta cardinalidad.
#> 8         La proporción total de faltantes supera el umbral configurado.
#> 9    Hay valores que representan ausencia sin estar codificados como NA.
#> 10                Hay texto con espacios sobrantes al inicio o al final.
#> 11 Conviven valores que sólo se diferencian por mayúsculas y minúsculas.
#> 12   Hay valores que representan ausencia sin estar codificados como NA.
#> 13      Se detectaron valores fuera de los límites de Tukey (1,5 x IQR).
#> 14                        La columna contiene un único valor no ausente.
#> 15                        La columna categórica tiene alta cardinalidad.
#> 17                           La tabla contiene filas duplicadas exactas.
#> 18                               Dos columnas tienen el mismo contenido.
#>                                                             evidencia
#> 1                                    Tasa de valores distintos: 0.846
#> 2                                                             S/D (1)
#> 3                                    Tasa de valores distintos: 0.923
#> 4                                                            NULL (1)
#> 5  %Y-%m-%d (4); %d/%m/%Y (4); %Y%m%d (1); %Y/%m/%d (1); %d-%m-%Y (1)
#> 6                Declarado: texto; inferido: fecha (0.846 compatible)
#> 7                                    Tasa de valores distintos: 0.615
#> 8                 0 ausentes reales y 2 disfrazados (0.154 del total)
#> 9                                               <blanco> (1); S/D (1)
#> 10                                        1 valores; ejemplos: "web "
#> 11                                                       "web"; "Web"
#> 12                                                            -99 (1)
#> 13                                                          4 valores
#> 14                                   Valor: principal; frecuencia: 13
#> 15                                              [evidencia protegida]
#> 17                                                 1 filas duplicadas
#> 18                                             id_registro = id_copia
#>                                                                               sugerencia
#> 1           Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 2  Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 3           Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 4  Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 5               Estandarizar la columna a un único formato antes de convertirla a fecha.
#> 6                  Confirmar el tipo esperado y convertir la columna de forma explícita.
#> 7           Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 8            Revisar la obligatoriedad del campo y el proceso que origina los faltantes.
#> 9  Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 10         Aplicar trimws() después de confirmar que los espacios no son significativos.
#> 11                   Definir y aplicar una convención de capitalización para la columna.
#> 12   Confirmar que los sentinelas numéricos representan ausencia antes de normalizarlos.
#> 13                        Examinar los valores extremos antes de decidir si son errores.
#> 14                Confirmar si la columna aporta información o si corresponde retirarla.
#> 15          Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 17                    Definir una clave y revisar la causa antes de eliminar duplicados.
#> 18                   Confirmar si ambas columnas son necesarias o si existe redundancia.
#>    n_evaluados n_afectados unidad_conteo estado_reparacion trazabilidad
#> 1            1           1       columna              <NA> no_aplic....
#> 2           13           1          fila              <NA> disponib....
#> 3            1           1       columna              <NA> no_aplic....
#> 4           13           1          fila              <NA> disponib....
#> 5            5           5       formato              <NA> no_aplic....
#> 6            1           1       columna              <NA> no_aplic....
#> 7            1           1       columna              <NA> no_aplic....
#> 8           13           2          fila              <NA> disponib....
#> 9           13           2          fila              <NA> disponib....
#> 10          13           1          fila              <NA> disponib....
#> 11          13           2          fila              <NA> disponib....
#> 12          13           1          fila              <NA> disponib....
#> 13          13           4          fila              <NA> disponib....
#> 14          13          13          fila              <NA> disponib....
#> 15           1           1       columna              <NA> no_aplic....
#> 17          13           1          fila              <NA> disponib....
#> 18          10           2       columna              <NA> no_aplic....
```

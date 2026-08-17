# lupa: examinar, medir y mejorar la calidad de datos

`lupa` implementa un modelo de calidad de datos de uso general:
dimensiones y factores declarables, métricas con granularidad explícita,
agregación tipada y una cadena de evaluación auditable. El paquete nunca
modifica datos como efecto del diagnóstico: cada etapa devuelve objetos
inspeccionables.
[`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
y
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md)
aportan de fábrica la implementación trazable del marco uruguayo, y
[`marco_iso25012()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
ofrece una segunda taxonomía opcional. Ninguna restringe las
declaraciones del usuario.

## Details

El punto de entrada es
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md).
En una llamada reúne el diagnóstico descriptivo y su cobertura, sin
medir requisitos observados automáticamente. Sus componentes también se
pueden construir por separado. El recorrido es:

1.  examinar estructura, tipos, patrones, ausencias, distribuciones,
    asociaciones y comportamiento temporal;
    [`cobertura_analisis()`](https://sebollin.github.io/lupa/reference/cobertura_analisis.md)
    explicita qué factores no fueron evaluados;

2.  convertir el diagnóstico en una propuesta editable con
    [`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md);

3.  declarar y ejecutar métricas mediante
    [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
    y [`medir()`](https://sebollin.github.io/lupa/reference/medir.md);

4.  evaluar reglas y perfiles de madurez con
    [`evaluar()`](https://sebollin.github.io/lupa/reference/evaluar.md);

5.  planificar y aplicar mejoras auditables mediante
    [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
    y
    [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md);

6.  acumular corridas y detectar deriva con
    [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md),
    [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
    y
    [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md);

7.  persistir el recorrido con
    [`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
    y producir un archivo HTML autocontenido con
    [`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md).

Las taxonomías se declaran con
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
Los padrones externos se declaran con
[`referencial()`](https://sebollin.github.io/lupa/reference/referencial.md),
y los contratos que no se pueden inferir se expresan con
[`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md)
y
[`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md).
La correspondencia exacta con las 49 entradas de AGESIC se consulta en
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md).
No se calcula un índice global: la jerarquía dimensión–factor–métrica es
taxonómica y requiere un contrato adicional para producirlo. Los
formatos se pueden verificar con funciones propias o con los packs de
[`validadores_internacionales()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
y
[`validadores_uruguay()`](https://sebollin.github.io/lupa/reference/pack_validadores.md);
[`pack_validadores()`](https://sebollin.github.io/lupa/reference/pack_validadores.md)
permite añadir otro país sin registrar estado global.

## References

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

## See also

[datos_operativos](https://sebollin.github.io/lupa/reference/datos_operativos.md),
[datos_administrativos](https://sebollin.github.io/lupa/reference/datos_administrativos.md)

## Author

**Maintainer**: Sebastián Lucas <sebalucas@gmail.com>
([ORCID](https://orcid.org/0009-0009-9068-0276))

Authors:

- Sebastián Lucas <sebalucas@gmail.com>
  ([ORCID](https://orcid.org/0009-0009-9068-0276))

Other contributors:

- Robyn Speer \[copyright holder\]

- Nicholas Tierney \[copyright holder\]

- Di Cook \[copyright holder\]

- Miles McBain \[copyright holder\]

- Colin Fay \[copyright holder\]

## Examples

``` r
resultado <- analizar(datos_operativos, analizar_dependencias = FALSE)
subset(resultado$perfil$hallazgos, severidad != "ok")
#>           columna               tipo_hallazgo  severidad
#> 1  codigo_usuario           alta_cardinalidad sospechoso
#> 2  codigo_usuario       faltantes_disfrazados      error
#> 3    fecha_evento                  casi_clave sospechoso
#> 4    fecha_evento           alta_cardinalidad sospechoso
#> 5    fecha_evento       faltantes_disfrazados      error
#> 6    fecha_evento       formatos_fecha_mixtos      error
#> 7    fecha_evento     tipo_declarado_distinto sospechoso
#> 8           canal           alta_cardinalidad sospechoso
#> 9           canal                   faltantes sospechoso
#> 10          canal       faltantes_disfrazados      error
#> 11          canal          espacios_sobrantes sospechoso
#> 12          canal   mayusculas_inconsistentes sospechoso
#> 13          monto                  casi_clave sospechoso
#> 14          monto       faltantes_disfrazados sospechoso
#> 15          monto                    outliers sospechoso
#> 16        sistema                   constante sospechoso
#> 17       contacto                  casi_clave sospechoso
#> 18       contacto           alta_cardinalidad sospechoso
#> 19      id_evento                  casi_clave sospechoso
#> 23          canal casi_duplicados_vocabulario sospechoso
#> 24           zona casi_duplicados_vocabulario sospechoso
#> 25           <NA>            filas_duplicadas      error
#> 26    id_registro         columnas_duplicadas sospechoso
#>                                                                               descripcion
#> 1                                          La columna categórica tiene alta cardinalidad.
#> 2                     Hay valores que representan ausencia sin estar codificados como NA.
#> 3       La columna es casi una clave, pero concentra sus colisiones en muy pocos valores.
#> 4                                          La columna categórica tiene alta cardinalidad.
#> 5                     Hay valores que representan ausencia sin estar codificados como NA.
#> 6                                      Conviven dos o más formatos de fecha o fecha-hora.
#> 7                          El tipo declarado no coincide con el tipo implícito dominante.
#> 8                                          La columna categórica tiene alta cardinalidad.
#> 9                          La proporción total de faltantes supera el umbral configurado.
#> 10                    Hay valores que representan ausencia sin estar codificados como NA.
#> 11                                 Hay texto con espacios sobrantes al inicio o al final.
#> 12                  Conviven valores que sólo se diferencian por mayúsculas y minúsculas.
#> 13      La columna es casi una clave, pero concentra sus colisiones en muy pocos valores.
#> 14                    Hay valores que representan ausencia sin estar codificados como NA.
#> 15                       Se detectaron valores fuera de los límites de Tukey (1,5 x IQR).
#> 16                                         La columna contiene un único valor no ausente.
#> 17      La columna es casi una clave, pero concentra sus colisiones en muy pocos valores.
#> 18                                         La columna categórica tiene alta cardinalidad.
#> 19      La columna es casi una clave, pero concentra sus colisiones en muy pocos valores.
#> 23 Hay grupos cuya forma normalizada coincide; eso no confirma que sean la misma entidad.
#> 24    Se detectaron valores cercanos; la distancia es heurística y no confirma identidad.
#> 25                                            La tabla contiene filas duplicadas exactas.
#> 26                                                Dos columnas tienen el mismo contenido.
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              evidencia
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Tasa de valores distintos: 0.846
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              S/D (1)
#> 3                                                                                                                                                                                                                                                                   12 valores distintos de 13 (0.923); 1 valores colisionados; 2 filas en colision; 1 duplicados excedentes; concentracion_colisiones=1.000. Colisiones: 2024-01-31 (2). criterio_casi_clave: tasa_distintos>=0.900 y concentracion_colisiones>=0.500. criterio_tipo_casi_clave: tipo=character; valores_fraccionarios_finitos=0; doble_admitido_solo_sin_fraccionarios_finitos=TRUE.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Tasa de valores distintos: 0.923
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             NULL (1)
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   %Y-%m-%d (4); %d/%m/%Y (4); %Y%m%d (1); %Y/%m/%d (1); %d-%m-%Y (1)
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Declarado: texto; inferido: fecha (0.846 compatible)
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Tasa de valores distintos: 0.615
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  0 ausentes reales y 2 disfrazados (0.154 del total)
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               <blanco> (1); S/D (1)
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         1 valores; ejemplos: "web "
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "web"; "Web"
#> 13                                                                                                                                                                                                                                                                           12 valores distintos de 13 (0.923); 1 valores colisionados; 2 filas en colision; 1 duplicados excedentes; concentracion_colisiones=1.000. Colisiones: 1250 (2). criterio_casi_clave: tasa_distintos>=0.900 y concentracion_colisiones>=0.500. criterio_tipo_casi_clave: tipo=double; valores_fraccionarios_finitos=0; doble_admitido_solo_sin_fraccionarios_finitos=TRUE.
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             -99 (1)
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           4 valores
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Valor: principal; frecuencia: 13
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               [evidencia protegida]
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               [evidencia protegida]
#> 19                                                                                                                                                                                                                                                                      12 valores distintos de 13 (0.923); 1 valores colisionados; 2 filas en colision; 1 duplicados excedentes; concentracion_colisiones=1.000. Colisiones: EVT001 (2). criterio_casi_clave: tasa_distintos>=0.900 y concentracion_colisiones>=0.500. criterio_tipo_casi_clave: tipo=character; valores_fraccionarios_finitos=0; doble_admitido_solo_sin_fraccionarios_finitos=TRUE.
#> 23                                [web (5) / Web (1) / web  (1)]; asimetria=5.0; origen=normalizacion; clase_diferencia=normalizacion_exacta; alcance: 6 de 6 valores; 6 pares comparados de 6; truncado=FALSE; unidades normalizadas: 4 de 4; grupos: 1, mostrados: 1; grupo_maximo: 3 (0.500); limite_aplicado=FALSE; valores_excluidos_faltantes_disfrazados=1; motivo_grupos=. pares descartados por secuencia numerica=0; grupo_maximo compatible=3 (0.500).  criterio_edicion_corta: distancia_edicion<=1; largo<=6; participacion_variante<=0.050; asimetria>=10.0; participacion_dominante>=0.500; candidatos=0; descartados_por_frecuencia=0.
#> 24 [Este (2) / Oeste (3)]; asimetria=1.5; origen=distancia; clase_diferencia=token_unico; distancia_minima=0.0667; distancia_maxima=0.0667; alcance: 5 de 5 valores; 10 pares comparados de 10; truncado=FALSE; unidades normalizadas: 5 de 5; grupos: 1, mostrados: 1; grupo_maximo: 2 (0.400); limite_aplicado=FALSE; valores_excluidos_faltantes_disfrazados=0; motivo_grupos=. pares descartados por secuencia numerica=0; grupo_maximo compatible=2 (0.400).  criterio_edicion_corta: distancia_edicion<=1; largo<=6; participacion_variante<=0.050; asimetria>=10.0; participacion_dominante>=0.500; candidatos=0; descartados_por_frecuencia=0.
#> 25                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  1 filas duplicadas
#> 26                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              id_registro = id_copia
#>                                                                                                                             sugerencia
#> 1                                                         Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 2                                                Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 3  Revisar los valores y filas en colision; corregirlos o confirmar que la columna no es una clave antes de usarla como identificador.
#> 4                                                         Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 5                                                Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 6                                                             Estandarizar la columna a un único formato antes de convertirla a fecha.
#> 7                                                                Confirmar el tipo esperado y convertir la columna de forma explícita.
#> 8                                                         Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 9                                                          Revisar la obligatoriedad del campo y el proceso que origina los faltantes.
#> 10                                               Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 11                                                       Aplicar trimws() después de confirmar que los espacios no son significativos.
#> 12                                                                 Definir y aplicar una convención de capitalización para la columna.
#> 13 Revisar los valores y filas en colision; corregirlos o confirmar que la columna no es una clave antes de usarla como identificador.
#> 14                                                 Confirmar que los sentinelas numéricos representan ausencia antes de normalizarlos.
#> 15                                                                      Examinar los valores extremos antes de decidir si son errores.
#> 16                                                              Confirmar si la columna aporta información o si corresponde retirarla.
#> 17 Revisar los valores y filas en colision; corregirlos o confirmar que la columna no es una clave antes de usarla como identificador.
#> 18                                                        Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 19 Revisar los valores y filas en colision; corregirlos o confirmar que la columna no es una clave antes de usarla como identificador.
#> 23                                                 Revisar las variantes y declarar una normalización o regla de remediación editable.
#> 24             Revisar las variantes y declarar una normalización o regla de remediación editable; la distancia no confirma identidad.
#> 25                                                                  Definir una clave y revisar la causa antes de eliminar duplicados.
#> 26                                                                 Confirmar si ambas columnas son necesarias o si existe redundancia.
#>    n_evaluados n_afectados  unidad_conteo estado_reparacion trazabilidad
#> 1            1           1        columna              <NA> no_aplic....
#> 2           13           1           fila              <NA> disponib....
#> 3           13           2           fila              <NA> disponib....
#> 4            1           1        columna              <NA> no_aplic....
#> 5           13           1           fila              <NA> disponib....
#> 6            5           5        formato              <NA> no_aplic....
#> 7            1           1        columna              <NA> no_aplic....
#> 8            1           1        columna              <NA> no_aplic....
#> 9           13           2           fila              <NA> disponib....
#> 10          13           2           fila              <NA> disponib....
#> 11          13           1           fila              <NA> disponib....
#> 12          13           2           fila              <NA> disponib....
#> 13          13           2           fila              <NA> disponib....
#> 14          13           1           fila              <NA> disponib....
#> 15          13           4           fila              <NA> disponib....
#> 16          13          13           fila              <NA> disponib....
#> 17          13           2           fila              <NA> disponib....
#> 18           1           1        columna              <NA> no_aplic....
#> 19          13           2           fila              <NA> disponib....
#> 23           6           3 valor_distinto              <NA> no_dispo....
#> 24           5           2 valor_distinto              <NA> no_dispo....
#> 25          13           1           fila              <NA> disponib....
#> 26          10           2        columna              <NA> no_aplic....
```

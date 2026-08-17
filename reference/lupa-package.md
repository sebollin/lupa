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
#> 3    fecha_evento           alta_cardinalidad sospechoso
#> 4    fecha_evento       faltantes_disfrazados      error
#> 5    fecha_evento       formatos_fecha_mixtos      error
#> 6    fecha_evento     tipo_declarado_distinto sospechoso
#> 7           canal           alta_cardinalidad sospechoso
#> 8           canal                   faltantes sospechoso
#> 9           canal       faltantes_disfrazados      error
#> 10          canal          espacios_sobrantes sospechoso
#> 11          canal   mayusculas_inconsistentes sospechoso
#> 12          monto       faltantes_disfrazados sospechoso
#> 13          monto                    outliers sospechoso
#> 14        sistema                   constante sospechoso
#> 15       contacto           alta_cardinalidad sospechoso
#> 19          canal casi_duplicados_vocabulario sospechoso
#> 20           zona casi_duplicados_vocabulario sospechoso
#> 21           <NA>            filas_duplicadas      error
#> 22    id_registro         columnas_duplicadas sospechoso
#>                                                                               descripcion
#> 1                                          La columna categórica tiene alta cardinalidad.
#> 2                     Hay valores que representan ausencia sin estar codificados como NA.
#> 3                                          La columna categórica tiene alta cardinalidad.
#> 4                     Hay valores que representan ausencia sin estar codificados como NA.
#> 5                                      Conviven dos o más formatos de fecha o fecha-hora.
#> 6                          El tipo declarado no coincide con el tipo implícito dominante.
#> 7                                          La columna categórica tiene alta cardinalidad.
#> 8                          La proporción total de faltantes supera el umbral configurado.
#> 9                     Hay valores que representan ausencia sin estar codificados como NA.
#> 10                                 Hay texto con espacios sobrantes al inicio o al final.
#> 11                  Conviven valores que sólo se diferencian por mayúsculas y minúsculas.
#> 12                    Hay valores que representan ausencia sin estar codificados como NA.
#> 13                       Se detectaron valores fuera de los límites de Tukey (1,5 x IQR).
#> 14                                         La columna contiene un único valor no ausente.
#> 15                                         La columna categórica tiene alta cardinalidad.
#> 19 Hay grupos cuya forma normalizada coincide; eso no confirma que sean la misma entidad.
#> 20    Se detectaron valores cercanos; la distancia es heurística y no confirma identidad.
#> 21                                            La tabla contiene filas duplicadas exactas.
#> 22                                                Dos columnas tienen el mismo contenido.
#>                                                                                                                                                                                                                                                                                                                                                                                                               evidencia
#> 1                                                                                                                                                                                                                                                                                                                                                                                      Tasa de valores distintos: 0.846
#> 2                                                                                                                                                                                                                                                                                                                                                                                                               S/D (1)
#> 3                                                                                                                                                                                                                                                                                                                                                                                      Tasa de valores distintos: 0.923
#> 4                                                                                                                                                                                                                                                                                                                                                                                                              NULL (1)
#> 5                                                                                                                                                                                                                                                                                                                                                    %Y-%m-%d (4); %d/%m/%Y (4); %Y%m%d (1); %Y/%m/%d (1); %d-%m-%Y (1)
#> 6                                                                                                                                                                                                                                                                                                                                                                  Declarado: texto; inferido: fecha (0.846 compatible)
#> 7                                                                                                                                                                                                                                                                                                                                                                                      Tasa de valores distintos: 0.615
#> 8                                                                                                                                                                                                                                                                                                                                                                   0 ausentes reales y 2 disfrazados (0.154 del total)
#> 9                                                                                                                                                                                                                                                                                                                                                                                                 <blanco> (1); S/D (1)
#> 10                                                                                                                                                                                                                                                                                                                                                                                          1 valores; ejemplos: "web "
#> 11                                                                                                                                                                                                                                                                                                                                                                                                         "web"; "Web"
#> 12                                                                                                                                                                                                                                                                                                                                                                                                              -99 (1)
#> 13                                                                                                                                                                                                                                                                                                                                                                                                            4 valores
#> 14                                                                                                                                                                                                                                                                                                                                                                                     Valor: principal; frecuencia: 13
#> 15                                                                                                                                                                                                                                                                                                                                                                                                [evidencia protegida]
#> 19                              [web (5) / Web (1) / web  (1)]; asimetria=5.0; origen=normalizacion; clase_diferencia=normalizacion_exacta; alcance: 7 de 7 valores; 10 pares comparados de 10; truncado=FALSE; unidades normalizadas: 5 de 5; grupos: 1, mostrados: 1; grupo_maximo: 3 (0.429); limite_aplicado=FALSE; motivo_grupos=. pares descartados por secuencia numerica=0; grupo_maximo compatible=3 (0.429). 
#> 20 [Este (2) / Oeste (3)]; asimetria=1.5; origen=distancia; clase_diferencia=token_unico; distancia_minima=0.0667; distancia_maxima=0.0667; alcance: 5 de 5 valores; 10 pares comparados de 10; truncado=FALSE; unidades normalizadas: 5 de 5; grupos: 1, mostrados: 1; grupo_maximo: 2 (0.400); limite_aplicado=FALSE; motivo_grupos=. pares descartados por secuencia numerica=0; grupo_maximo compatible=2 (0.400). 
#> 21                                                                                                                                                                                                                                                                                                                                                                                                   1 filas duplicadas
#> 22                                                                                                                                                                                                                                                                                                                                                                                               id_registro = id_copia
#>                                                                                                                 sugerencia
#> 1                                             Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 2                                    Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 3                                             Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 4                                    Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 5                                                 Estandarizar la columna a un único formato antes de convertirla a fecha.
#> 6                                                    Confirmar el tipo esperado y convertir la columna de forma explícita.
#> 7                                             Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 8                                              Revisar la obligatoriedad del campo y el proceso que origina los faltantes.
#> 9                                    Normalizar estas representaciones a NA conservando su significado si fuera necesario.
#> 10                                           Aplicar trimws() después de confirmar que los espacios no son significativos.
#> 11                                                     Definir y aplicar una convención de capitalización para la columna.
#> 12                                     Confirmar que los sentinelas numéricos representan ausencia antes de normalizarlos.
#> 13                                                          Examinar los valores extremos antes de decidir si son errores.
#> 14                                                  Confirmar si la columna aporta información o si corresponde retirarla.
#> 15                                            Revisar si es texto libre, un identificador o una categoría mal normalizada.
#> 19                                     Revisar las variantes y declarar una normalización o regla de remediación editable.
#> 20 Revisar las variantes y declarar una normalización o regla de remediación editable; la distancia no confirma identidad.
#> 21                                                      Definir una clave y revisar la causa antes de eliminar duplicados.
#> 22                                                     Confirmar si ambas columnas son necesarias o si existe redundancia.
#>    n_evaluados n_afectados  unidad_conteo estado_reparacion trazabilidad
#> 1            1           1        columna              <NA> no_aplic....
#> 2           13           1           fila              <NA> disponib....
#> 3            1           1        columna              <NA> no_aplic....
#> 4           13           1           fila              <NA> disponib....
#> 5            5           5        formato              <NA> no_aplic....
#> 6            1           1        columna              <NA> no_aplic....
#> 7            1           1        columna              <NA> no_aplic....
#> 8           13           2           fila              <NA> disponib....
#> 9           13           2           fila              <NA> disponib....
#> 10          13           1           fila              <NA> disponib....
#> 11          13           2           fila              <NA> disponib....
#> 12          13           1           fila              <NA> disponib....
#> 13          13           4           fila              <NA> disponib....
#> 14          13          13           fila              <NA> disponib....
#> 15           1           1        columna              <NA> no_aplic....
#> 19           7           3 valor_distinto              <NA> no_dispo....
#> 20           5           2 valor_distinto              <NA> no_dispo....
#> 21          13           1           fila              <NA> disponib....
#> 22          10           2        columna              <NA> no_aplic....
```

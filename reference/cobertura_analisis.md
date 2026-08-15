# Informar la cobertura conceptual de un análisis

Devuelve una fila por dimensión y factor del
[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
elegido. Usa
[`marco_agesic()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
por omisión, pero acepta cualquier taxonomía declarada. Distingue lo
efectivamente medido de lo que no fue declarado, lo que no aplica a los
tipos presentes y lo que queda fuera del alcance actual. La tabla evita
que la ausencia de un hallazgo se interprete como evidencia de calidad.

## Usage

``` r
cobertura_analisis(perfil, medicion = NULL, modelo = marco_agesic())
```

## Arguments

- perfil:

  Objeto creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- medicion:

  Objeto opcional creado por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

- modelo:

  Objeto creado por
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md).
  El nombre enfatiza que es el modelo conceptual de referencia, no el
  objeto operativo de
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

## Value

Data frame con `marco`, `dimension`, `factor`, `estado`, `motivo` y
`como_resolverlo`. `marco` identifica explícitamente la taxonomía contra
la que se calculó la tabla. `estado` es un factor con niveles
`"medida"`, `"no_declarada"`, `"no_aplica"` y `"fuera_de_alcance"`.

## Details

En el marco incluido, el profiling automático mide densidad y no
duplicación. Un marco propio puede marcar otros factores mediante la
columna `perfil_mide`. Los demás sólo pasan a `"medida"` cuando
`medicion` contiene una métrica del factor; descubrir un patrón o una
dependencia no los convierte por sí solo en un requisito confirmado.

## See also

[`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md),
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`medir()`](https://sebollin.github.io/lupa/reference/medir.md),
[`vigencia()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md),
[`escala()`](https://sebollin.github.io/lupa/reference/contratos_medicion.md),
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)

## Examples

``` r
perfil <- perfilar(datos_administrativos, analizar_dependencias = FALSE)
cobertura_analisis(perfil)
#>                                  marco    dimension
#> 1  Marco de calidad de datos de AGESIC    Exactitud
#> 2  Marco de calidad de datos de AGESIC    Exactitud
#> 3  Marco de calidad de datos de AGESIC    Exactitud
#> 4  Marco de calidad de datos de AGESIC    Exactitud
#> 5  Marco de calidad de datos de AGESIC    Exactitud
#> 6  Marco de calidad de datos de AGESIC    Exactitud
#> 7  Marco de calidad de datos de AGESIC Consistencia
#> 8  Marco de calidad de datos de AGESIC Consistencia
#> 9  Marco de calidad de datos de AGESIC Consistencia
#> 10 Marco de calidad de datos de AGESIC Consistencia
#> 11 Marco de calidad de datos de AGESIC  Completitud
#> 12 Marco de calidad de datos de AGESIC  Completitud
#> 13 Marco de calidad de datos de AGESIC  Completitud
#> 14 Marco de calidad de datos de AGESIC     Unicidad
#> 15 Marco de calidad de datos de AGESIC     Unicidad
#> 16 Marco de calidad de datos de AGESIC     Frescura
#> 17 Marco de calidad de datos de AGESIC     Frescura
#>                           factor           estado
#> 1          Correctitud semántica     no_declarada
#> 2         Correctitud sintáctica     no_declarada
#> 3                      Precisión     no_declarada
#> 4  Exactitud posicional absoluta        no_aplica
#> 5  Exactitud posicional relativa        no_aplica
#> 6                      Fidelidad fuera_de_alcance
#> 7       Integridad inter-entidad     no_declarada
#> 8       Integridad intra-entidad     no_declarada
#> 9          Integridad de dominio     no_declarada
#> 10       Consistencia topológica        no_aplica
#> 11                     Cobertura     no_declarada
#> 12                      Densidad           medida
#> 13                      Comisión        no_aplica
#> 14                No-duplicación           medida
#> 15              No-contradicción     no_declarada
#> 16                    Actualidad     no_declarada
#> 17                   Oportunidad     no_declarada
#>                                                                             motivo
#> 1     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 2     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 3     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 4                                       No se identificaron columnas de geometría.
#> 5                                       No se identificaron columnas de geometría.
#> 6  Las métricas del factor requieren capacidades no implementadas en esta versión.
#> 7     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 8     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 9     El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 10                                      No se identificaron columnas de geometría.
#> 11    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 12            El perfil contó ausentes reales y disfrazados en todas las columnas.
#> 13                                      No se identificaron columnas de geometría.
#> 14             El perfil examinó duplicación de valores, columnas y filas exactas.
#> 15    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 16    El perfil describe evidencia, pero no recibió un requisito para este factor.
#> 17    El perfil describe evidencia, pero no recibió un requisito para este factor.
#>                                                                 como_resolverlo
#> 1                      Crear referencial() e instanciar metricas_referencial().
#> 2                  Especializar Formato con expresión, diccionario o validador.
#> 3                    Declarar escala() o medir ErrorEstandar sobre el atributo.
#> 4  Requiere un backend o referencial especializado que no integra esta versión.
#> 5  Requiere un backend o referencial especializado que no integra esta versión.
#> 6  Requiere un backend o referencial especializado que no integra esta versión.
#> 7                Instanciar ReglaIntegridadInterEntidad con claves confirmadas.
#> 8               Confirmar una regla y especializar ReglaIntegridadIntraEntidad.
#> 9               Proveer un dominio a ValoresPosiblesPorExtension o Comprension.
#> 10 Requiere un backend o referencial especializado que no integra esta versión.
#> 11                Crear un referencial(completo = TRUE) y medir RatioCobertura.
#> 12                                             Usar NoNulo o DensidadPonderada.
#> 13 Requiere un backend o referencial especializado que no integra esta versión.
#> 14                     Usar las métricas de duplicación o el perfil automático.
#> 15 Requiere un backend o referencial especializado que no integra esta versión.
#> 16           Declarar vigencia() y medir DesactualizacionPorFecha o PorCambios.
#> 17                        Declarar vigencia() y medir una métrica Oportunidad*.
```

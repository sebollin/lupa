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

  **Primer argumento.** Objeto creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
  o
  [`perfilar_dbi()`](https://sebollin.github.io/lupa/reference/perfilar_dbi.md).
  En el segundo caso se analiza `perfil_muestra` cuando existe; un
  perfil por agregados usa el resumen de la tabla y cubre la tabla
  completa. Cuando corresponde, cada motivo indica que la cobertura es
  la de la muestra y no la de la tabla completa.

- medicion:

  **Segundo argumento, opcional.** Objeto creado por
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md), con
  métricas ejecutadas que pueden completar esa cobertura. No es un
  perfil creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

  **La correspondencia entre los dos objetos es suya, no del paquete.**
  La cobertura aparea por dimensión y factor, y no verifica que la
  medición provenga de la tabla que se perfiló: si le pasa una medición
  de otra tabla, el factor va a figurar como medido por esa corrida. El
  paquete no puede comprobarlo porque un perfil no guarda una identidad
  de tabla comparable con la entidad de la medición.

- modelo:

  **Tercer argumento.** Objeto creado por
  [`marco_calidad()`](https://sebollin.github.io/lupa/reference/marco_calidad.md)
  que actúa como referencia conceptual. El nombre enfatiza que no es el
  modelo operativo creado por
  [`modelo()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
  que recibe
  [`medir()`](https://sebollin.github.io/lupa/reference/medir.md).

## Value

Data frame con `marco`, `dimension`, `factor`, `estado`, `motivo` y
`como_resolverlo`. `marco` identifica explícitamente la taxonomía contra
la que se calculó la tabla. `estado` es un factor con niveles
`"medida"`, `"no_declarada"`, `"no_aplica"` y `"fuera_de_alcance"`.

**El `motivo` publica el alcance cuando el perfil resumió sólo parte de
las columnas.** Si ninguna se pudo resumir, el `estado` es
`"no_aplica"`; si se resumieron todas, el motivo lo dice; y si se
resumieron algunas, el `estado` sigue siendo `"medida"` —el factor sí se
midió, en las columnas que se pudieron resumir— y el motivo empieza con
`"Parcial: "` y nombra cuántas de cuántas. Esas filas se encuentran con
`grepl("^Parcial: ", cobertura$motivo)`. El `estado` no distingue el
caso parcial a propósito: es un factor de cuatro niveles documentados
que
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
valida, y el grado —1 de 100 no es 99 de 100— no cabe en un nivel. El
alcance viaja en el motivo, y
[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
lo conserva al cruzar la cobertura con su medición.

## Details

En el marco incluido, el profiling automático mide densidad y no
duplicación. Un marco propio puede marcar otros factores mediante la
columna `perfil_mide`. Los demás sólo pasan a `"medida"` cuando
`medicion` contiene una métrica del factor; descubrir un patrón o una
dependencia no los convierte por sí solo en un requisito confirmado.

Para AGESIC, `como_resolverlo` se deriva de las entradas implementadas o
agregables de
[`catalogo_agesic()`](https://sebollin.github.io/lupa/reference/catalogo_agesic.md),
no de una lista independiente de factores. Por ejemplo,
`No-contradicción` recomienda instanciar `EntidadContradictoria` sobre
un atributo; no requiere una clave y, cuando compara más de un valor
distinto, necesita el paquete opcional `stringdist`.

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
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           como_resolverlo
#> 1                                                                                                                                                                                                                                                                                                                                        Crear `referencial()` con `clave` y `valor`. Aplicar `especializar()` a `metricas_referencial()$CorrectitudSemDebil`; luego `instanciar()` sobre un atributo por cada columna de `clave` y de `valor`, en ese orden, con `referencial`; después `medir()`. Crear `referencial()` con `clave`. Aplicar `especializar()` a `metricas_referencial()$CorrectitudSemFuerte`; luego `instanciar()` sobre un atributo por cada columna de `clave` con `referencial`; después `medir()`.
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Aplicar `especializar()` a `metricas_nucleo()$Formato` con `expresion_regular`, o bien `diccionario`, o bien `validador`; luego `instanciar()` sobre un atributo; después `medir()`.
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Declarar `escala()` con `error`; `tipo` es opcional. Aplicar `especializar()` a `metricas_nucleo()$Escala` con `escala`; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$ErrorEstandar`; luego `instanciar()` sobre un atributo; después `medir()`.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Requiere un backend o referencial especializado que no integra esta versión.
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Requiere un backend o referencial especializado que no integra esta versión.
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Requiere un backend o referencial especializado que no integra esta versión.
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Aplicar `especializar()` a `metricas_nucleo()$ReglaIntegridadInterEntidad`; `muestra` es opcional (por omisión Inf); luego `instanciar()` sobre dos entidades y sus claves; después `medir()`.
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Aplicar `especializar()` a `metricas_nucleo()$ReglaIntegridadIntraEntidad` con `regla`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`.
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Aplicar `especializar()` a `metricas_nucleo()$ValoresPosiblesPorExtension` con `valores`; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$ValoresPosiblesPorComprension` con `predicado`, o bien `minimo`, `maximo` e `inclusivo`; luego `instanciar()` sobre un atributo; después `medir()`.
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Requiere un backend o referencial especializado que no integra esta versión.
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Crear `referencial()` con `clave`, `completo` = TRUE y `alcance`. Aplicar `especializar()` a `metricas_referencial()$RatioCobertura`; luego `instanciar()` sobre una entidad y sus atributos con `referencial`; después `medir()`.
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                            Aplicar `especializar()` a `metricas_nucleo()$NoNulo`; las propiedades `valores_nulos`, `aplicable` y `opcional` son opcionales; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$DensidadPonderada` con `coeficientes`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`.
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Requiere un backend o referencial especializado que no integra esta versión.
#> 14                                                                                                                                                                                                                                                                                                                                                                                         Aplicar `especializar()` a `metricas_nucleo()$AtributoDuplicado`; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$ConjuntoAtributosDuplicado`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$EntidadDuplicada`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`.
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                  Aplicar `especializar()` a `metricas_nucleo()$EntidadContradictoria`; las propiedades `metodo`, `p`, `umbral`, `normalizar`, `max_valores` y `nucleos` son opcionales; luego `instanciar()` sobre un atributo; después `medir()`. Si se comparan varios valores distintos, se necesita stringdist; después, `agregar()` con destino "atributo" y agregación "ratio" resume la medida.
#> 16                                                                                                                                                      Crear `vigencia()` con `fecha_acceso` y `fecha_ultimo_cambio` o bien `fecha_acceso` y `frecuencia_cambio`. Aplicar `especializar()` a `metricas_nucleo()$DesactualizacionPorFecha` con `vigencia`; luego `instanciar()` sobre un atributo; después `medir()`. Crear `vigencia()` con `fecha_acceso` y `frecuencia_cambio`. Aplicar `especializar()` a `metricas_nucleo()$DesactualizacionPorCambios` con `vigencia`; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$DesactualizacionPorFormato` con `expresion_regular`, o bien `validador`; luego `instanciar()` sobre un atributo; después `medir()`.
#> 17 Aplicar `especializar()` a `metricas_nucleo()$OportunidadAtributoPorFecha` con `fecha_limite`; luego `instanciar()` sobre un atributo; después `medir()`. Aplicar `especializar()` a `metricas_nucleo()$OportunidadAtributoPorIntervalo` con `inicio_vigencia` y `fin_vigencia`; luego `instanciar()` sobre un atributo; después `medir()`. Crear `vigencia()` con `fecha_acceso` y `fecha_limite`. Aplicar `especializar()` a `metricas_nucleo()$OportunidadEntPorFecha` con `vigencia`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`. Crear `vigencia()` con `fecha_acceso`, `inicio_intervalo` y `fin_intervalo`. Aplicar `especializar()` a `metricas_nucleo()$OportunidadEntPorIntervalo` con `vigencia`; luego `instanciar()` sobre una entidad y sus atributos; después `medir()`.
```

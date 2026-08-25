# Empezar con lupa

`lupa` acompaña un recorrido: examinar una entrega, decidir qué medir,
medir, evaluar, proponer mejoras y dejar evidencia. Ninguna etapa
modifica los datos por el solo hecho de diagnosticarlos.

## Una entrega tabular

El conjunto incluido es pequeño y completamente sintético. Tiene fechas
mezcladas, sentinelas, duplicados y formatos discordantes.
[`analizar()`](https://sebollin.github.io/lupa/reference/analizar.md) es
la puerta de entrada: por omisión mide las propuestas en estado `lista`,
las agrega y devuelve un tablero. Las métricas las propuso `lupa` y
nadie las confirmó; se puede apagar la medición con
`medir_propuesta = FALSE`.

``` r

library(lupa)
data(datos_operativos)
dim(datos_operativos)
#> [1] 13 10
```

La lista amplia de sentinelas numéricos de `naniar` se solicita de
manera explícita porque valores como `66`, `77` y `88` pueden ser datos
legítimos.

``` r

perfil_sentinelas <- perfilar(
  data.frame(codigo = c(1, 66, 9999)),
  sentinelas_numericos = sentinelas_naniar,
  analizar_dependencias = FALSE
)
perfil_sentinelas$columnas[, c("columna", "n_faltantes_disfrazados")]
#>   columna n_faltantes_disfrazados
#> 1  codigo                       2
```

El marco contra el que se informa la cobertura también es declarable. En
un data frame, `perfil_mide` señala los factores para los que el
profiling aporta evidencia suficiente por sí solo. La forma abreviada
con una lista deja ese campo en `FALSE`: nunca presume que examinar
equivale a medir.

``` r

factores <- data.frame(
  dimension = c("Estructura", "Estructura", "Trazabilidad"),
  factor = c(
    "Ausencias observadas", "Duplicación exacta", "Origen documentado"
  ),
  perfil_mide = c(TRUE, TRUE, FALSE),
  como_resolverlo = c(
    "Revisar ausencias detectadas por el perfil.",
    "Revisar duplicados exactos detectados por el perfil.",
    "Declarar una métrica sobre el sistema de origen."
  )
)
marco_operativo <- marco_calidad("Marco operativo del ejemplo", factores)

analisis <- analizar(
  datos_operativos,
  nombre = "entrega de ejemplo",
  fecha = as.POSIXct("2026-01-15", tz = "UTC"),
  marco = marco_operativo
)
perfil <- analisis$perfil
```

``` r

analisis$tablero
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>       componente    dimension                 factor                     metrica
#>  componente-0001     Unicidad         No-duplicación            EntidadDuplicada
#>  componente-0002  Completitud               Densidad                      NoNulo
#>  componente-0003  Completitud               Densidad                      NoNulo
#>  componente-0004  Completitud               Densidad                      NoNulo
#>  componente-0005  Completitud               Densidad                      NoNulo
#>  componente-0006    Exactitud Correctitud sintáctica                     Formato
#>  componente-0007    Exactitud Correctitud sintáctica                     Formato
#>  componente-0008    Exactitud Correctitud sintáctica                     Formato
#>  componente-0009    Exactitud Correctitud sintáctica                     Formato
#>  componente-0010 Consistencia  Integridad de dominio ValoresPosiblesPorExtension
#>          objeto     valor orientacion agregacion umbral universo
#>         (tabla) 0.1538462     defecto      ratio     NA    filas
#>           canal 1.0000000 conformidad      ratio     NA   celdas
#>  codigo_usuario 1.0000000 conformidad      ratio     NA   celdas
#>    fecha_evento 1.0000000 conformidad      ratio     NA   celdas
#>           monto 1.0000000 conformidad      ratio     NA   celdas
#>        contacto 0.8461538 conformidad      ratio     NA   celdas
#>       id_evento 1.0000000 conformidad      ratio     NA   celdas
#>         sistema 1.0000000 conformidad      ratio     NA   celdas
#>            zona 1.0000000 conformidad      ratio     NA   celdas
#>            zona 1.0000000 conformidad      ratio     NA   celdas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#>               3                0                     3          0                0
```

Los hallazgos son datos. Se pueden filtrar, ordenar o exportar sin
extraer texto de un reporte.

``` r

perfil$hallazgos[, c(
  "columna", "tipo_hallazgo", "severidad", "evidencia"
)]
#>           columna               tipo_hallazgo  severidad
#> 1     id_registro       posible_identificador         ok
#> 2  codigo_usuario           alta_cardinalidad sospechoso
#> 3  codigo_usuario       faltantes_disfrazados      error
#> 4    fecha_evento           alta_cardinalidad sospechoso
#> 5    fecha_evento       faltantes_disfrazados      error
#> 6    fecha_evento       formatos_fecha_mixtos      error
#> 7    fecha_evento     tipo_declarado_distinto sospechoso
#> 8           canal                   faltantes sospechoso
#> 9           canal       faltantes_disfrazados      error
#> 10          canal          espacios_sobrantes sospechoso
#> 11          canal   mayusculas_inconsistentes sospechoso
#> 12          monto       faltantes_disfrazados sospechoso
#> 13          monto                    outliers sospechoso
#> 14        sistema                   constante sospechoso
#> 15       contacto           alta_cardinalidad sospechoso
#> 16       id_copia       posible_identificador         ok
#> 17      id_evento       posible_identificador         ok
#> 18 codigo_usuario casi_duplicados_vocabulario         ok
#> 19   fecha_evento casi_duplicados_vocabulario         ok
#> 20          canal casi_duplicados_vocabulario sospechoso
#> 21           <NA>            filas_duplicadas      error
#> 22    id_registro         columnas_duplicadas sospechoso
#> 23 codigo_usuario       dato_personal_posible         ok
#> 24       contacto       dato_personal_posible         ok
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           evidencia
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           11 valores distintos de 13 (0.846); densidad del tramo de enteros=1.000
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             [evidencia protegida]
#> 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             [evidencia protegida]
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Tasa de valores distintos: 0.923
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NULL (1)
#> 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                %d/%m/%Y (4); %Y-%m-%d (4); %d-%m-%Y (1); %Y/%m/%d (1); %Y%m%d (1)
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Declarado: texto; inferido: fecha (0.846 compatible); convertir dejaria 2 de 13 valores en NA
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               0 ausentes reales y 2 disfrazados (0.154 del total)
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             <blanco> (1); S/D (1)
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      1 valores; ejemplos: "web "
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "web"; "Web"
#> 12                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          -99 (1)
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        4 valores
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Valor: principal; frecuencia: 13
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            [evidencia protegida]
#> 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          11 valores distintos de 13 (0.846); densidad del tramo de enteros=1.000
#> 17                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               12 valores distintos de 13 (0.923)
#> 18                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            [evidencia protegida]
#> 19                                                                                                                                                                                                                                                      No se formaron grupos por distancia: 2 pares cercanos se descartaron por secuencias numericas incompatibles; alcance: 11 de 11 valores; 55 pares comparados de 55; truncado=FALSE; unidades normalizadas: 11 de 11; grupos: 0, mostrados: 0; grupo_maximo: 0 (0.000); limite_aplicado=FALSE; valores_excluidos_faltantes_disfrazados=1; motivo_grupos=. pares descartados por secuencia numerica=2; grupo_maximo compatible=0 (0.000).  criterio_edicion_corta: distancia_edicion<=1; largo<=6; participacion_variante<=0.050; asimetria>=10.0; participacion_dominante>=0.500; candidatos=0; descartados_por_frecuencia=0.
#> 20 [Web (1) / web (5) / web  (1)]; asimetria=5.0; origen=normalizacion; clase_diferencia=normalizacion_exacta; 2 de 2 variantes difieren de la forma dominante solo en mayusculas, acentos o puntuacion (en español el acento puede cambiar la palabra: conviene mirarlas antes de unificar); alcance: 6 de 6 valores; 6 pares comparados de 6; truncado=FALSE; unidades normalizadas: 4 de 4; grupos: 1, mostrados: 1; grupo_maximo: 3 (0.500); limite_aplicado=FALSE; valores_excluidos_faltantes_disfrazados=1; motivo_grupos=. pares descartados por secuencia numerica=0; grupo_maximo compatible=3 (0.500).  criterio_edicion_corta: distancia_edicion<=1; largo<=6; participacion_variante<=0.050; asimetria>=10.0; participacion_dominante>=0.500; candidatos=0; descartados_por_frecuencia=0.; traza: 7 filas mostradas (2 formas variantes, 5 formas dominantes); total=7
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      2 filas en grupos duplicados (1 excedentes)
#> 22                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                id_registro = id_copia; comparadas sobre 13 filas
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Tipo posible: nombre; fundamento: nombre de columna; poder discriminante: medio; proteccion automatica: si
#> 24                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Tipo posible: correo; fundamento: forma de correo dominante; poder discriminante: alto; proteccion automatica: si; proporción compatible: 0.923
```

Los conteos y la traza tienen unidades relacionadas, pero no
necesariamente iguales. `mayusculas_inconsistentes` y
`normalizacion_unicode` cuentan valores distintos
(`unidad_conteo = "valor_distinto"`); su `trazabilidad` sigue enumerando
por fila todas las filas que contienen esos valores, no sólo las filas
defectuosas. `casi_duplicados_vocabulario` usa la misma forma: cuenta
valores variantes y traza las filas que contienen los valores del grupo
seleccionado, incluida la forma dominante. Como es una señal heurística,
esa traza sirve para revisar el grupo y no afirma que todas sus filas
sean errores. Las filas de las formas no dominantes aparecen primero y
las de las formas dominantes después; la evidencia informa cuántas filas
mostradas pertenecen a cada grupo. Para `patron_raro`, la evidencia
conserva como máximo seis nombres, pero la trazabilidad usa todos los
nombres raros hasta su límite separado de 5.000; un recorte de ese
límite aparece en `cobertura_diagnosticos`. Cada hallazgo informa además
la proporción del patrón dominante y cuántas filas quedaron en patrones
no dominantes excluidos por superar `umbral_patron_raro`. Si el
dominante no alcanza `umbral_patron_dominante`, no se emite el hallazgo:
la no medición, la proporción observada y cómo ajustar ese argumento
quedan en `cobertura_diagnosticos`. En `filas_duplicadas`, tanto el
conteo afectado como la traza incluyen todas las filas participantes; la
evidencia informa aparte los excedentes. Un cero es una medición de
cero, mientras que un `NA` declara que el conteo no pudo medirse. Si se
separan el conteo y la traza,
[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md)
conserva el hallazgo y emite una advertencia de clase
`lupa_trazabilidad_incoherente`.

La tabla por columna mantiene todas las proporciones en `[0, 1]`.

``` r

perfil$columnas[, c(
  "columna", "tipo_declarado", "tipo_inferido",
  "prop_faltantes_totales", "n_distintos"
)]
#>           columna tipo_declarado tipo_inferido prop_faltantes_totales n_distintos
#> 1     id_registro          doble         doble             0.00000000          11
#> 2  codigo_usuario          texto         texto             0.07692308          11
#> 3    fecha_evento          texto         fecha             0.07692308          12
#> 4           canal          texto         texto             0.15384615           8
#> 5           monto          doble         doble             0.07692308          12
#> 6            zona          texto         texto             0.00000000           5
#> 7         sistema          texto         texto             0.00000000           1
#> 8        contacto          texto         texto             0.00000000          12
#> 9        id_copia          doble         doble             0.00000000          11
#> 10      id_evento          texto identificador             0.00000000          12
```

El mismo objeto incluye técnicas complementarias con sus límites
declarados: frecuencias acotadas, cuantiles, asociaciones, calendario
observado y una clasificación confirmable de escalas. La cobertura evita
confundir ausencia de hallazgos con calidad demostrada.

Si una columna se clasifica como posible dato personal, los estadísticos
de orden y cuantiles que podrían ser valores de una persona se suprimen
y la tabla lo marca. Las medias y los desvíos se mantienen como síntesis
no ligadas a una fila.

``` r

analisis$distribuciones$cuantiles
#>        columna probabilidad  valor n_analizados muestreado    estado
#> 1  id_registro         0.00      1           13      FALSE calculado
#> 2  id_registro         0.25      3           13      FALSE calculado
#> 3  id_registro         0.50      6           13      FALSE calculado
#> 4  id_registro         0.75      9           13      FALSE calculado
#> 5  id_registro         1.00     11           13      FALSE calculado
#> 6        monto         0.00    -99           13      FALSE calculado
#> 7        monto         0.25   1250           13      FALSE calculado
#> 8        monto         0.50   1490           13      FALSE calculado
#> 9        monto         0.75   1700           13      FALSE calculado
#> 10       monto         1.00 999999           13      FALSE calculado
#> 11    id_copia         0.00      1           13      FALSE calculado
#> 12    id_copia         0.25      3           13      FALSE calculado
#> 13    id_copia         0.50      6           13      FALSE calculado
#> 14    id_copia         0.75      9           13      FALSE calculado
#> 15    id_copia         1.00     11           13      FALSE calculado
analisis$asociaciones
#>     columna_1 columna_2     tipo_1     tipo_2           metodo
#> 1 id_registro  id_copia   numerica   numerica pearson_absoluto
#> 2       canal     monto categorica   numerica             eta2
#> 3       canal      zona categorica categorica         cramer_v
#> 4 id_registro     monto   numerica   numerica pearson_absoluto
#> 5       monto  id_copia   numerica   numerica pearson_absoluto
#> 6       canal  id_copia categorica   numerica             eta2
#> 7 id_registro     canal   numerica categorica             eta2
#> 8 id_registro      zona   numerica categorica             eta2
#> 9        zona  id_copia categorica   numerica             eta2
#>                                                                              supuesto
#> 1 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 2            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 3                                   Las columnas se tratan como categorias sin orden.
#> 4 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 5 Las columnas numericas se tratan como cuantitativas; la escala no queda confirmada.
#> 6            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 7            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 8            La columna numerica se trata como cuantitativa y la otra como categoria.
#> 9            La columna numerica se trata como cuantitativa y la otra como categoria.
#>   asociacion n_pares
#> 1  1.0000000      13
#> 2  0.9999977      13
#> 3  0.8944272      13
#> 4  0.4301569      13
#> 5  0.4301569      13
#> 6  0.3791539      13
#> 7  0.3791539      13
#> 8  0.3440367      13
#> 9  0.3440367      13
analisis$temporal$resumen
#>        columna n_presentes n_fechas_distintas n_duplicados_temporales fecha_minima
#> 1 fecha_evento          11                  9                       2   2024-01-31
#>   fecha_maxima monotonicidad cobertura_periodo n_fechas_esperadas_ausentes
#> 1   2024-10-23           0.9         0.1111111                           8
#>   n_fechas_fuera_calendario n_grupos_huecos huecos_truncados proteccion_temporal
#> 1                         0               1            FALSE                <NA>
analisis$variables[, c(
  "columna", "tipo_almacenamiento", "tipo_implicito", "escala_propuesta",
  "confianza", "confirmada", "n_niveles_ausentes"
)]
#>           columna tipo_almacenamiento tipo_implicito escala_propuesta confianza
#> 1     id_registro               doble          doble         discreta      0.65
#> 2  codigo_usuario               texto          texto          nominal      0.55
#> 3    fecha_evento               texto          fecha         temporal      0.85
#> 4           canal               texto          texto          nominal      0.55
#> 5           monto               doble          doble         discreta      0.65
#> 6            zona               texto          texto          nominal      0.55
#> 7         sistema               texto          texto          nominal      0.55
#> 8        contacto               texto          texto          nominal      0.55
#> 9        id_copia               doble          doble         discreta      0.65
#> 10      id_evento               texto  identificador          nominal      0.90
#>    confirmada n_niveles_ausentes
#> 1       FALSE                  0
#> 2       FALSE                  0
#> 3       FALSE                  0
#> 4       FALSE                  0
#> 5       FALSE                  0
#> 6       FALSE                  0
#> 7       FALSE                  0
#> 8       FALSE                  0
#> 9       FALSE                  0
#> 10      FALSE                  0
analisis$cobertura[, c("marco", "dimension", "factor", "estado")]
#>                         marco    dimension               factor       estado
#> 1 Marco operativo del ejemplo   Estructura Ausencias observadas       medida
#> 2 Marco operativo del ejemplo   Estructura   Duplicación exacta       medida
#> 3 Marco operativo del ejemplo Trazabilidad   Origen documentado no_declarada
perfil$cobertura_diagnosticos
#>              diagnostico   columna
#> 1 proximidad_vocabulario      zona
#> 2 proximidad_vocabulario  contacto
#> 3 proximidad_vocabulario id_evento
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                 motivo
#> 1 1 grupo de valores cercanos no se informo porque su asimetria de frecuencias quedo por debajo de 2: las formas son casi igual de comunes y no se puede distinguir una errata sistematica de dos valores legitimamente parecidos. 1 grupo de formas cercanas no se formo porque sus frecuencias fueron parecidas: el criterio de variante rara no pudo distinguir una forma correcta de otra. Se identificaron 1 pares cercanos con asimetria <= 2.0.
#> 2                                                                                                                                                                                                                                                                                                                                                                    El grupo candidato mayor abarca 0.917 del vocabulario y el diagnostico no aplica.
#> 3                                                                                                                                                                                                                                                                                                                                                                    El grupo candidato mayor abarca 1.000 del vocabulario y el diagnostico no aplica.
#>                                                                                                                                                                                                                                                                                               como_resolverlo
#> 1 Bajar `min_asimetria_vocabulario` si en esta columna interesan las variantes de frecuencia pareja, sabiendo que aumenta el ruido. Activar un detector de formas equifrecuentes con conocimiento del dominio, o revisar los pares manualmente; no se puede elegir una forma correcta solo por su frecuencia.
#> 2                                                                                                                                                                                  Usar una regla de dominio especifica para la columna o ajustar el criterio de agrupacion con conocimiento del vocabulario.
#> 3                                                                                                                                                                                  Usar una regla de dominio especifica para la columna o ajustar el criterio de agrupacion con conocimiento del vocabulario.
#>   dependencia
#> 1        <NA>
#> 2        <NA>
#> 3        <NA>
```

`cobertura_diagnosticos` es la tabla que sostiene la regla central de
`lupa`: un diagnóstico que no pudo evaluarse queda identificado y no se
confunde con la ausencia de hallazgos.

## Del diagnóstico al modelo

[`proponer_modelo()`](https://sebollin.github.io/lupa/reference/proponer_modelo.md)
no ejecuta nada. Devuelve una propuesta editable que explica de dónde
salió cada métrica. Los dominios y patrones aprendidos de una sola
entrega quedan inactivos hasta que alguien confirme que son requisitos.

``` r

propuesta <- analisis$propuesta_modelo
propuesta[, c(
  "metrica", "atributos", "origen", "prioridad", "incluir", "estado"
)]
#>                        metrica      atributos                             origen
#> 1             EntidadDuplicada                         hallazgo:filas_duplicadas
#> 2                       NoNulo          canal                 hallazgo:faltantes
#> 3                       NoNulo codigo_usuario     hallazgo:faltantes_disfrazados
#> 4                       NoNulo   fecha_evento     hallazgo:faltantes_disfrazados
#> 5                       NoNulo          monto     hallazgo:faltantes_disfrazados
#> 6                      Formato       contacto perfil:patron_dominante:a+9+@a+.a+
#> 7                      Formato      id_evento       perfil:patron_dominante:A+9+
#> 8                      Formato        sistema         perfil:patron_dominante:a+
#> 9                      Formato           zona        perfil:patron_dominante:Aa+
#> 10 ValoresPosiblesPorExtension           zona           perfil:dominio_observado
#>    prioridad incluir estado
#> 1       alta    TRUE  lista
#> 2       alta   FALSE  lista
#> 3       alta   FALSE  lista
#> 4       alta   FALSE  lista
#> 5       alta   FALSE  lista
#> 6      media   FALSE  lista
#> 7      media   FALSE  lista
#> 8      media   FALSE  lista
#> 9      media   FALSE  lista
#> 10      baja   FALSE  lista
```

Las filas activas se convierten en un modelo y se miden. En este ejemplo
la métrica estructural activa detecta las filas que participan en
duplicados.

``` r

modelo_calidad <- modelo_desde_propuesta(propuesta)
medidas <- medir(
  modelo_calidad, datos_operativos,
  id_medicion = "ejemplo-001",
  fecha = as.POSIXct("2026-01-15", tz = "UTC")
)
head(medidas[, c("metrica", "fila", "resultado")])
#>            metrica fila resultado
#> 1 EntidadDuplicada    1         1
#> 2 EntidadDuplicada    2         0
#> 3 EntidadDuplicada    3         0
#> 4 EntidadDuplicada    4         0
#> 5 EntidadDuplicada    5         0
#> 6 EntidadDuplicada    6         0
```

La medida booleana por fila puede agregarse a la entidad mediante
`ratio`. Después se evalúa una regla explícita; no aparece un puntaje
global inventado.

``` r

medida_entidad <- agregar(medidas, "entidad", "ratio")
regla <- regla_evaluacion(
  "Duplicación menor al 20 %",
  function(x) x < 0.2
)
evaluacion <- evaluar(
  medida_entidad,
  perfil_evaluacion("Control operativo", regla)
)
evaluacion$perfiles[, c("perfil", "resultado")]
#>              perfil resultado
#> 1 Control operativo         1
```

## Mejorar sin perder trazabilidad

El plan propone acciones y sólo activa las que no dependen del dominio.
Puede editarse como cualquier otro `data.frame`.

``` r

plan <- analisis$plan_limpieza
plan[, c(
  "grupo", "columna", "estrategia", "recomendada", "aplicar", "estado"
)]
#>         grupo        columna                     estrategia recomendada aplicar
#> 1        <NA> codigo_usuario           revisar_cardinalidad       FALSE   FALSE
#> 2        <NA> codigo_usuario  convertir_ausencias_textuales        TRUE    TRUE
#> 3        <NA>   fecha_evento           revisar_cardinalidad       FALSE   FALSE
#> 4        <NA>   fecha_evento  convertir_ausencias_textuales        TRUE    TRUE
#> 5        <NA>   fecha_evento     convertir_fecha_confirmada       FALSE   FALSE
#> 6        <NA>          canal  convertir_ausencias_textuales        TRUE    TRUE
#> 7        <NA>          canal              recortar_espacios        TRUE    TRUE
#> 8  grupo-0011          canal           convertir_minusculas       FALSE   FALSE
#> 9  grupo-0011          canal               convertir_titulo       FALSE   FALSE
#> 10 grupo-0011          canal           convertir_mayusculas       FALSE   FALSE
#> 11 grupo-0011          canal    convertir_segun_diccionario       FALSE   FALSE
#> 12 grupo-0012          monto convertir_sentinelas_numericos       FALSE   FALSE
#> 13 grupo-0013          monto                marcar_outliers        TRUE   FALSE
#> 14 grupo-0013          monto            winsorizar_outliers       FALSE   FALSE
#> 15 grupo-0014        sistema     eliminar_columna_constante       FALSE   FALSE
#> 16       <NA>       contacto           revisar_cardinalidad       FALSE   FALSE
#> 17 grupo-0021           <NA>        marcar_filas_duplicadas        TRUE    TRUE
#> 18 grupo-0021           <NA>    conservar_primera_duplicada       FALSE   FALSE
#> 19 grupo-0021           <NA>         conservar_mas_completa       FALSE   FALSE
#> 20 grupo-0022    id_registro     marcar_columnas_duplicadas        TRUE    TRUE
#> 21 grupo-0022    id_registro     eliminar_columna_duplicada       FALSE   FALSE
#>         estado
#> 1  informativa
#> 2        lista
#> 3  informativa
#> 4        lista
#> 5    bloqueada
#> 6        lista
#> 7        lista
#> 8        lista
#> 9        lista
#> 10       lista
#> 11   bloqueada
#> 12       lista
#> 13       lista
#> 14       lista
#> 15       lista
#> 16 informativa
#> 17       lista
#> 18       lista
#> 19   bloqueada
#> 20       lista
#> 21       lista

resultado <- aplicar(plan, datos_operativos)
resultado$registro[, c("estrategia", "n_cambiadas")]
#>                      estrategia n_cambiadas
#> 1       marcar_filas_duplicadas           2
#> 2 convertir_ausencias_textuales           1
#> 3 convertir_ausencias_textuales           1
#> 4 convertir_ausencias_textuales           2
#> 5             recortar_espacios           1
#> 6    marcar_columnas_duplicadas           1
```

El original permanece intacto. Para verificar el efecto se vuelve a
perfilar el resultado.

``` r

perfil_despues <- perfilar(resultado$datos, nombre = "entrega normalizada")
c(
  antes = nrow(perfil$hallazgos),
  despues = nrow(perfil_despues$hallazgos)
)
#>   antes despues 
#>      24      21
```

## Un archivo para compartir

[`guardar_analisis()`](https://sebollin.github.io/lupa/reference/persistir_analisis.md)
conserva el recorrido entre sesiones. Por omisión excluye la tabla de
entrada y sustituye las reglas funcionales por declaraciones pequeñas,
evitando serializar sus entornos.
[`reportar()`](https://sebollin.github.io/lupa/reference/reportar.md)
acepta el objeto entero y produce un HTML autocontenido. Este ejemplo
crea ambos archivos en el directorio temporal y luego los retira.

``` r

archivo_rds <- tempfile(fileext = ".rds")
guardar_analisis(analisis, archivo_rds)
analisis_recuperado <- leer_analisis(archivo_rds)
is.null(analisis_recuperado$datos)
#> [1] TRUE

archivo <- reportar(
  analisis, medidas, evaluacion, plan,
  archivo = tempfile(fileext = ".html"),
  titulo = "Calidad de la entrega de ejemplo"
)
basename(archivo)
#> [1] "file22e437fbc199.html"
unlink(c(archivo, archivo_rds))
```

Para profundizar en la arquitectura consulte
[`vignette("definir-la-calidad")`](https://sebollin.github.io/lupa/articles/definir-la-calidad.md);
para revisar decisiones de remediación,
[`vignette("limpiar-con-un-plan")`](https://sebollin.github.io/lupa/articles/limpiar-con-un-plan.md).

# Histórico y monitoreo de deriva

Una medición aislada describe una entrega. Varias corridas permiten
saber si la calidad mejora y si cambió la estructura que el modelo
esperaba.

``` r

library(lupa)
```

## Serie de evaluaciones

El identificador y la fecha se fijan al medir. Aquí se evalúa la
completitud de la misma columna en dos meses.

``` r

nucleo <- metricas_nucleo()
instancia <- instanciar(
  especializar(nucleo$NoNulo, nombre_especifico = "NoNuloDato"),
  "entrega", "dato"
)
modelo_calidad <- modelo(instancia)

enero <- medir(
  modelo_calidad, data.frame(dato = c("A", NA, "C", NA)),
  id_medicion = "enero",
  fecha = as.POSIXct("2026-01-31", tz = "UTC")
)
febrero <- medir(
  modelo_calidad, data.frame(dato = c("A", "B", "C", NA)),
  id_medicion = "febrero",
  fecha = as.POSIXct("2026-02-28", tz = "UTC")
)

enero <- agregar(enero, "atributo", "ratio")
febrero <- agregar(febrero, "atributo", "ratio")
regla <- regla_evaluacion("Completitud mayor al 60 %", function(x) x > 0.6)
perfil <- perfil_evaluacion("Operativo", regla)
evaluacion_enero <- evaluar(enero, perfil)
evaluacion_febrero <- evaluar(febrero, perfil)
comparar_evaluaciones(evaluacion_enero, evaluacion_febrero)
#>      perfil id_medicion_anterior fecha_anterior resultado_anterior id_medicion_actual
#> 1 Operativo                enero     2026-01-31                  0            febrero
#>   fecha_actual resultado_actual delta
#> 1   2026-02-28                1     1
```

[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
normaliza las corridas en un `data.frame` plano y versionado. Puede
escribirse como CSV o llevarse a una tabla institucional sin desarmar
listas anidadas.

``` r

historico <- historico_calidad(evaluacion_enero, evaluacion_febrero)
historico[, c("id_medicion", "fecha", "nivel", "resultado")]
#>   id_medicion      fecha             nivel resultado
#> 1       enero 2026-01-31  evaluacion_regla         0
#> 2       enero 2026-01-31 evaluacion_perfil         0
#> 3     febrero 2026-02-28  evaluacion_regla         1
#> 4     febrero 2026-02-28 evaluacion_perfil         1
detectar_deriva_calidad(historico, umbral = 0.05)
#>    nivel    perfil regla identidad_tabla id_medicion_anterior fecha_anterior
#> 1 perfil Operativo  <NA>         entrega                enero     2026-01-31
#>   resultado_anterior id_medicion_actual fecha_actual resultado_actual delta
#> 1                  0            febrero   2026-02-28                1     1
#>   cambio_absoluto significativo direccion severidad   aspecto
#> 1               1          TRUE    mejora        ok resultado
#>                             descripcion evidencia
#> 1 Cambió el resultado de la evaluación.      <NA>
```

[`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
existe para el flujo incremental: amplía una serie ya guardada y no
duplica una corrida idéntica.

``` r

historico_incremental <- acumular_historico(
  historico_calidad(evaluacion_enero), evaluacion_febrero
)
identical(historico, historico_incremental)
#> [1] TRUE
```

La persistencia usa RDS y no agrega dependencias. El archivo no se
sobrescribe sin consentimiento.

``` r

archivo <- tempfile(fileext = ".rds")
guardar_historico(historico, archivo)
recuperado <- leer_historico(archivo)
identical(historico, recuperado)
#> [1] TRUE
unlink(archivo)
```

## Deriva estructural entre entregas

[`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
distingue cambios de esquema y cambios sobre columnas comparables.
Informa columnas nuevas o retiradas, tipos, ausencias, cardinalidad,
rangos, patrones y hallazgos.

``` r

data(datos_administrativos)
entrega_enero <- datos_administrativos
entrega_febrero <- datos_administrativos
entrega_febrero$cedula[1] <- "12345678"
entrega_febrero$nueva_columna <- "nuevo"

perfil_enero <- perfilar(
  entrega_enero,
  fecha = as.POSIXct("2026-01-31", tz = "UTC")
)
perfil_febrero <- perfilar(
  entrega_febrero,
  fecha = as.POSIXct("2026-02-28", tz = "UTC")
)
deriva <- comparar_perfiles(perfil_enero, perfil_febrero)
deriva[, c("columna", "aspecto", "cambio", "severidad")]
#>         columna      aspecto     cambio  severidad
#> 1 nueva_columna      columna  aparecida      error
#> 2        cedula cardinalidad modificado sospechoso
#> 3 nueva_columna     hallazgo  aparecido sospechoso
#> 4          <NA>     hallazgo   resuelto         ok
```

Una comparación contra el mismo perfil produce cero cambios. Las
columnas que sólo existen en un lado se informan como cambios
estructurales; no hacen fallar la comparación de las demás.

``` r

nrow(comparar_perfiles(perfil_enero, perfil_enero))
#> [1] 0
```

## Equivalencia de resúmenes

[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md)
devuelve una fila por columna y campo compartidos. Compara con igualdad
exacta los conteos y usa la tolerancia declarada por quien llama sólo
para los campos flotantes.

``` r

set.seed(20260831)
datos_equivalencia <- data.frame(
  monto = c(10, 20, 30, 40),
  stringsAsFactors = FALSE
)
perfil_equivalencia_anterior <- perfilar(
  datos_equivalencia, muestra = Inf, analizar_dependencias = FALSE
)
perfil_equivalencia_actual <- perfilar(
  datos_equivalencia, muestra = Inf, analizar_dependencias = FALSE
)

fila_monto <- match(
  "monto", perfil_equivalencia_actual$columnas$columna
)
perfil_equivalencia_actual$columnas$media[fila_monto] <-
  perfil_equivalencia_actual$columnas$media[fila_monto] + 0.001
perfil_equivalencia_actual$columnas$n[fila_monto] <-
  perfil_equivalencia_actual$columnas$n[fila_monto] + 1L

tolerancia_llamador <- 0.01
equivalencia <- comparar_equivalencia(
  perfil_equivalencia_anterior, perfil_equivalencia_actual,
  tolerancia = tolerancia_llamador
)
campos_mostrados <- c("n", "n_faltantes", "media")
equivalencia_monto <- equivalencia[
  equivalencia$columna == "monto" & equivalencia$campo %in% campos_mostrados,
  , drop = FALSE
]
equivalencia_monto <- equivalencia_monto[
  match(campos_mostrados, equivalencia_monto$campo),
  c("campo", "valor_anterior", "valor_actual", "veredicto", "motivo",
    "tipo_eje", "tolerancia")
]
equivalencia_monto
#>          campo valor_anterior valor_actual              veredicto               motivo
#> 3            n              4            5 materialmente_distinto           eje_exacto
#> 8  n_faltantes              0            0               identico      igualdad_exacta
#> 33       media             25       25.001            equivalente dentro_de_tolerancia
#>    tipo_eje tolerancia
#> 3    exacto       0.01
#> 8    exacto       0.01
#> 33 flotante       0.01
```

La salida muestra los tres veredictos y el motivo de cada fila; la
tolerancia es del llamador y viaja ecoada en `tolerancia`. En los ejes
exactos, `equivalente` es inalcanzable: el conteo cambiado permanece
`materialmente_distinto` con cualquier tolerancia válida.

``` r

tolerancias <- c(0, tolerancia_llamador, 1e12)
veredictos_conteo <- vapply(tolerancias, function(tolerancia) {
  prueba <- comparar_equivalencia(
    perfil_equivalencia_anterior, perfil_equivalencia_actual,
    tolerancia = tolerancia
  )
  as.character(prueba$veredicto[
    prueba$columna == "monto" & prueba$campo == "n"
  ])
}, character(1))
data.frame(tolerancia = tolerancias, veredicto_n = veredictos_conteo)
#>   tolerancia            veredicto_n
#> 1      0e+00 materialmente_distinto
#> 2      1e-02 materialmente_distinto
#> 3      1e+12 materialmente_distinto
stopifnot(all(veredictos_conteo == "materialmente_distinto"))
```

El invariante es deliberado: devuelve datos, no decisiones.

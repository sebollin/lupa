# Limpiar con un plan auditable

Diagnosticar no autoriza a modificar datos. En `lupa`, el producto de la
etapa de mejora es primero un plan inspeccionable, filtrable y editable.

``` r

library(lupa)
data(datos_administrativos)

perfil <- perfilar(datos_administrativos)
plan <- planificar_limpieza(perfil, datos_administrativos)
plan[, c(
  "id_accion", "grupo", "columna", "estrategia",
  "recomendada", "aplicar", "reversible", "estado"
)]
#>      id_accion      grupo          columna                     estrategia recomendada
#> 1  accion-0001       <NA>           cedula           revisar_cardinalidad       FALSE
#> 2  accion-0002       <NA>           cedula  convertir_ausencias_textuales        TRUE
#> 3  accion-0003       <NA> fecha_nacimiento           revisar_cardinalidad       FALSE
#> 4  accion-0004       <NA> fecha_nacimiento  convertir_ausencias_textuales        TRUE
#> 5  accion-0005       <NA> fecha_nacimiento     convertir_fecha_confirmada       FALSE
#> 6  accion-0006       <NA>             sexo  convertir_ausencias_textuales        TRUE
#> 7  accion-0007 grupo-0010          ingreso convertir_sentinelas_numericos       FALSE
#> 8  accion-0008 grupo-0011          ingreso                marcar_outliers        TRUE
#> 9  accion-0009 grupo-0011          ingreso            winsorizar_outliers       FALSE
#> 10 accion-0010       <NA>     departamento           revisar_cardinalidad       FALSE
#> 11 accion-0011 grupo-0013             pais     eliminar_columna_constante       FALSE
#> 12 accion-0012       <NA>           correo           revisar_cardinalidad       FALSE
#> 13 accion-0013 grupo-0018             <NA>        marcar_filas_duplicadas        TRUE
#> 14 accion-0014 grupo-0018             <NA>    conservar_primera_duplicada       FALSE
#> 15 accion-0015 grupo-0018             <NA>         conservar_mas_completa       FALSE
#> 16 accion-0016 grupo-0019       id_persona     marcar_columnas_duplicadas        TRUE
#> 17 accion-0017 grupo-0019       id_persona     eliminar_columna_duplicada       FALSE
#>    aplicar reversible      estado
#> 1    FALSE         NA informativa
#> 2     TRUE      FALSE       lista
#> 3    FALSE         NA informativa
#> 4     TRUE      FALSE       lista
#> 5    FALSE      FALSE   bloqueada
#> 6     TRUE      FALSE       lista
#> 7    FALSE      FALSE       lista
#> 8    FALSE       TRUE       lista
#> 9    FALSE      FALSE       lista
#> 10   FALSE         NA informativa
#> 11   FALSE      FALSE       lista
#> 12   FALSE         NA informativa
#> 13    TRUE       TRUE       lista
#> 14   FALSE      FALSE       lista
#> 15   FALSE      FALSE   bloqueada
#> 16    TRUE       TRUE       lista
#> 17   FALSE      FALSE       lista
```

## Recomendaciones y decisiones de dominio

Una acción se recomienda sólo si es correcta con independencia del
dominio: por ejemplo, convertir un sentinela textual inequívoco a `NA` o
quitar espacios de borde. Normalizar mayúsculas, convertir un sentinela
numérico o winsorizar un valor extremo exige contexto y queda
desactivado.

La justificación acompaña cada alternativa.

``` r

plan[, c("estrategia", "justificacion")]
#>                        estrategia
#> 1            revisar_cardinalidad
#> 2   convertir_ausencias_textuales
#> 3            revisar_cardinalidad
#> 4   convertir_ausencias_textuales
#> 5      convertir_fecha_confirmada
#> 6   convertir_ausencias_textuales
#> 7  convertir_sentinelas_numericos
#> 8                 marcar_outliers
#> 9             winsorizar_outliers
#> 10           revisar_cardinalidad
#> 11     eliminar_columna_constante
#> 12           revisar_cardinalidad
#> 13        marcar_filas_duplicadas
#> 14    conservar_primera_duplicada
#> 15         conservar_mas_completa
#> 16     marcar_columnas_duplicadas
#> 17     eliminar_columna_duplicada
#>                                                                                                                                                                       justificacion
#> 1                                                                El perfil señala el problema, pero no contiene conocimiento suficiente del dominio para elegir una transformación.
#> 2                                                   Las representaciones textuales del catálogo son marcadores explícitos de ausencia y pueden normalizarse sin inferir el dominio.
#> 3                                                                El perfil señala el problema, pero no contiene conocimiento suficiente del dominio para elegir una transformación.
#> 4                                                   Las representaciones textuales del catálogo son marcadores explícitos de ausencia y pueden normalizarse sin inferir el dominio.
#> 5  La conversión no es ejecutable sobre los datos completos: Hay valores presentes que no responden a los formatos confirmados. Se conserva como acción destructiva no recomendada.
#> 6                                                   Las representaciones textuales del catálogo son marcadores explícitos de ausencia y pueden normalizarse sin inferir el dominio.
#> 7                                                                           Un sentinela numérico también puede ser un valor legítimo; requiere confirmar el diccionario del campo.
#> 8                                                                          Un valor extremo puede ser correcto; la marca conserva el dato para que el dominio decida cómo tratarlo.
#> 9                                                      Sustituye los extremos por los límites de Tukey y altera valores observados; sólo debe elegirse con justificación analítica.
#> 10                                                               El perfil señala el problema, pero no contiene conocimiento suficiente del dominio para elegir una transformación.
#> 11                                                      Eliminarla pierde contexto potencial; dejarla es la recomendación hasta confirmar que no aporta significado administrativo.
#> 12                                                               El perfil señala el problema, pero no contiene conocimiento suficiente del dominio para elegir una transformación.
#> 13                                                             Marcar conserva todas las filas, identifica las repeticiones y asigna un grupo a todos los registros que participan.
#> 14                                                     Conserva la primera aparición exacta y elimina las siguientes; el orden de entrada pasa a determinar qué registro sobrevive.
#> 15                                Requiere configurar una clave: entre duplicados exactos todas las filas tienen la misma completitud y esta opción sería equivalente a la primera.
#> 16                                                                                                   La anotación conserva ambas columnas y registra explícitamente la redundancia.
#> 17                                                                     Eliminar una columna puede romper consumidores que dependan de su nombre aunque el contenido sea redundante.
```

[`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
permite declarar qué diferencias se ignoran al comparar. El perfil
cambia sólo la representación de comparación; nunca modifica los datos.

``` r

normalizacion(acentos = FALSE, puntuacion = TRUE)
#> Perfil de normalizacion de lupa
#>   minusculas = TRUE
#>   espacios = TRUE
#>   acentos = FALSE
#>   comillas = TRUE
#>   puntuacion = TRUE
#>   ligaduras = FALSE
#>   ancho = FALSE
#>   proteger = ñ, ü, g̃
```

## Grupos mutuamente excluyentes

Las alternativas de un mismo hallazgo comparten `grupo`. Como máximo una
puede tener `aplicar = TRUE`. `decision_grupo` distingue un grupo
pendiente de una decisión explícita de no actuar; no se inventa una fila
ficticia de “no hacer nada”.

``` r

grupos <- plan[!is.na(plan$grupo), c(
  "grupo", "estrategia", "recomendada", "aplicar", "decision_grupo"
)]
grupos
#>         grupo                     estrategia recomendada aplicar decision_grupo
#> 7  grupo-0010 convertir_sentinelas_numericos       FALSE   FALSE      pendiente
#> 8  grupo-0011                marcar_outliers        TRUE   FALSE      pendiente
#> 9  grupo-0011            winsorizar_outliers       FALSE   FALSE      pendiente
#> 11 grupo-0013     eliminar_columna_constante       FALSE   FALSE    recomendada
#> 13 grupo-0018        marcar_filas_duplicadas        TRUE    TRUE    recomendada
#> 14 grupo-0018    conservar_primera_duplicada       FALSE   FALSE    recomendada
#> 15 grupo-0018         conservar_mas_completa       FALSE   FALSE    recomendada
#> 16 grupo-0019     marcar_columnas_duplicadas        TRUE    TRUE    recomendada
#> 17 grupo-0019     eliminar_columna_duplicada       FALSE   FALSE    recomendada
```

El modo guiado es una capa opcional. Fuera de una sesión interactiva
devuelve el plan sin bloquear; en una consola recorre sólo decisiones
pendientes o riesgosas.

``` r

identical(guiar_limpieza(plan, datos_administrativos), plan)
#> [1] TRUE
```

## Aplicar conserva evidencia

``` r

copia <- datos_administrativos
resultado <- aplicar(plan, datos_administrativos)
identical(datos_administrativos, copia)
#> [1] TRUE
resultado$registro[, c(
  "estrategia", "n_cambiadas", "n_filas_eliminadas", "n_columnas_eliminadas"
)]
#>                      estrategia n_cambiadas n_filas_eliminadas n_columnas_eliminadas
#> 1       marcar_filas_duplicadas           2                  0                     0
#> 2 convertir_ausencias_textuales           1                  0                     0
#> 3 convertir_ausencias_textuales           1                  0                     0
#> 4 convertir_ausencias_textuales           2                  0                     0
#> 5    marcar_columnas_duplicadas           1                  0                     0
```

Las acciones destructivas nunca son recomendadas. Aunque el usuario las
active en el plan,
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
exige además `permitir_eliminacion = TRUE`; lo retirado se conserva en
`resultado$eliminados` salvo decisión contraria.

## Imputar por una dependencia

Una dependencia funcional exacta permite deducir un valor desde los
propios datos, pero sigue siendo una regularidad observada en una
entrega. Puede representar una regla real o un error sistemático. Por
eso la acción se ofrece con mapa y soporte, pero no queda recomendada ni
activa.

``` r

datos <- data.frame(
  codigo = rep(1:3, each = 4),
  descripcion = rep(c("A", "B", "C"), each = 4),
  stringsAsFactors = FALSE
)
datos$descripcion[c(2, 6)] <- NA
perfil_fd <- perfilar(datos, muestra = Inf)
plan_fd <- planificar_limpieza(perfil_fd, datos)
imputacion <- grep(
  "^imputar_dependencia_funcional", plan_fd$estrategia
)
plan_fd[imputacion, c(
  "estrategia", "recomendada", "aplicar", "justificacion"
)]
#>                              estrategia recomendada aplicar
#> 6 imputar_dependencia_funcional__codigo       FALSE   FALSE
#>                                                                                                                                                                   justificacion
#> 6 La relación codigo -> descripcion es exacta en 10 filas presentes y cada valor usado tiene al menos 2 observaciones de soporte. Debe confirmarse como regla antes de imputar.
```

Después de confirmar la regla, el usuario activa una alternativa y
registra la decisión. La dependencia vuelve a validarse al aplicar el
plan.

``` r

plan_fd$aplicar[imputacion[1]] <- TRUE
plan_fd$decision_grupo[imputacion[1]] <- "elegida"
resultado_fd <- aplicar(plan_fd, datos)
resultado_fd$datos
#>    codigo descripcion .fila_duplicada .grupo_duplicado
#> 1       1           A           FALSE                1
#> 2       1           A           FALSE               NA
#> 3       1           A            TRUE                1
#> 4       1           A            TRUE                1
#> 5       2           B           FALSE                2
#> 6       2           B           FALSE               NA
#> 7       2           B            TRUE                2
#> 8       2           B            TRUE                2
#> 9       3           C           FALSE                3
#> 10      3           C            TRUE                3
#> 11      3           C            TRUE                3
#> 12      3           C            TRUE                3
resultado_fd$registro[, c("estrategia", "n_cambiadas")]
#>                              estrategia n_cambiadas
#> 1               marcar_filas_duplicadas          10
#> 2 imputar_dependencia_funcional__codigo           2
```

El ciclo termina volviendo a perfilar. Así la mejora queda vinculada a
un plan y a un registro, no a una transformación invisible.

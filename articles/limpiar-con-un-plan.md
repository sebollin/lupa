# 6. Limpiar con un plan

El perfil encontró problemas. ¿Cómo corregimos los que se pueden
corregir sin romper nada, decidiendo nosotros lo que depende del dominio
y dejando registro de cada cambio?

> **En esta guía:**
>
> - qué propone
>   [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md),
>   qué recomienda y qué deja para nosotros;
> - cómo elegir entre alternativas excluyentes para un mismo hallazgo;
> - cómo aplicar el plan sobre una copia y comparar los valores antes y
>   después;
> - qué dice el plan de lo que no cubre;
> - cómo imputar un valor desde una dependencia que confirmamos.

Usamos `datos_administrativos`, el otro conjunto de ejemplo del paquete:
13 registros de personas con defectos sembrados, como una cédula `S/D`,
un sexo en blanco, fechas de nacimiento en formatos mezclados, un
ingreso `-99`, una columna constante y dos filas idénticas.

``` r

library(lupa)

datos_administrativos[, c("id_persona", "cedula", "fecha_nacimiento", "sexo", "ingreso")]
#>    id_persona      cedula fecha_nacimiento sexo ingreso
#> 1           1 0.000.001-1       1980-01-31    F   25000
#> 2           2 0.000.002-2       31/01/1981    M   31000
#> 3           3    00000033       1982/02/15    F   28500
#> 4           4 0.000.004-4       16-03-1983    M     -99
#> 5           5       MAL-5       1984.04.17  S/D       0
#> 6           6 0.000.006-6         19850518    F   33000
#> 7           7 0.000.007-7       19/06/1986    M   34500
#> 8           8 0.000.008-8       1987-07-20    F   29900
#> 9           9 0.000.009-9       21/08/1988    M   31500
#> 10         10 0.000.010-0       1989-09-22    F   32000
#> 11         10 0.000.010-0       23/10/1989    M     -50
#> 12          1 0.000.001-1       1980-01-31    F   25000
#> 13         11         S/D             NULL      9999999
```

## Un plan, no un cambio

Diagnosticar no autoriza a modificar datos.
[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
no toca nada: devuelve una tabla de acciones posibles, una por fila, que
se puede filtrar y editar como cualquier `data.frame`.

``` r

perfil <- perfilar(datos_administrativos)
plan <- planificar_limpieza(perfil, datos_administrativos)
plan[, c("id_accion", "columna", "estrategia", "recomendada", "aplicar")]
#>      id_accion          columna                     estrategia recomendada aplicar
#> 1  accion-0001           cedula           revisar_cardinalidad       FALSE   FALSE
#> 2  accion-0002           cedula  convertir_ausencias_textuales        TRUE    TRUE
#> 3  accion-0003 fecha_nacimiento           revisar_cardinalidad       FALSE   FALSE
#> 4  accion-0004 fecha_nacimiento  convertir_ausencias_textuales       FALSE   FALSE
#> 5  accion-0005 fecha_nacimiento     convertir_fecha_confirmada       FALSE   FALSE
#> 6  accion-0006             sexo  convertir_ausencias_textuales        TRUE    TRUE
#> 7  accion-0007          ingreso convertir_sentinelas_numericos       FALSE   FALSE
#> 8  accion-0008     departamento           revisar_cardinalidad       FALSE   FALSE
#> 9  accion-0009             pais     eliminar_columna_constante       FALSE   FALSE
#> 10 accion-0010           correo           revisar_cardinalidad       FALSE   FALSE
#> 11 accion-0011             <NA>        marcar_filas_duplicadas        TRUE    TRUE
#> 12 accion-0012             <NA>    conservar_primera_duplicada       FALSE   FALSE
#> 13 accion-0013             <NA>         conservar_mas_completa       FALSE   FALSE
#> 14 accion-0014       id_persona     marcar_columnas_duplicadas        TRUE    TRUE
#> 15 accion-0015       id_persona     eliminar_columna_duplicada       FALSE   FALSE
```

`aplicar` dice qué se ejecutaría si aplicáramos el plan tal como está:
por omisión, sólo lo recomendado. `estado` separa lo que está listo de
lo que sólo pide revisar (`informativa`) y de lo que no se puede
ejecutar sin una decisión previa (`bloqueada`):

``` r

plan[plan$estado != "lista", c("id_accion", "estrategia", "estado")]
#>      id_accion                 estrategia      estado
#> 1  accion-0001       revisar_cardinalidad informativa
#> 3  accion-0003       revisar_cardinalidad informativa
#> 5  accion-0005 convertir_fecha_confirmada   bloqueada
#> 8  accion-0008       revisar_cardinalidad informativa
#> 10 accion-0010       revisar_cardinalidad informativa
#> 13 accion-0013     conservar_mas_completa   bloqueada
```

## Recomendaciones y decisiones de dominio

Una acción se recomienda sólo si es correcta con independencia del
dominio: por ejemplo, convertir un centinela textual inequívoco a `NA` o
quitar espacios de borde. Normalizar mayúsculas, convertir un centinela
numérico o eliminar una columna constante exige contexto y queda
desactivado. La justificación acompaña cada alternativa:

``` r

justificaciones <- plan[plan$columna %in% c("sexo", "ingreso"), ]
for (i in seq_len(nrow(justificaciones))) {
  cat(justificaciones$id_accion[i], justificaciones$estrategia[i], "\n")
  writeLines(strwrap(justificaciones$justificacion[i], 86, indent = 2, exdent = 2))
}
#> accion-0006 convertir_ausencias_textuales 
#>   Las representaciones textuales del catálogo son marcadores habituales de ausencia.
#>   El cambio no es reversible: confirmar que ninguno de esos textos sea un valor
#>   legítimo de la columna -`NA` es el código de Namibia, `NULL` puede ser un
#>   apellido-.
#> accion-0007 convertir_sentinelas_numericos 
#>   Un sentinela numérico también puede ser un valor legítimo; requiere confirmar el
#>   diccionario del campo.
```

Convertir `S/D` en ausencia es seguro en casi cualquier columna, y aun
así la justificación pide confirmarlo. Que `-99` signifique “sin dato”
en `ingreso` sólo lo sabe quien conoce el sistema, así que la acción
queda propuesta y sin activar.

Si lo que molesta es sólo la comparación (mayúsculas, acentos,
puntuación), no hace falta limpiar.
[`normalizacion()`](https://sebollin.github.io/lupa/reference/normalizacion.md)
declara qué diferencias ignora el perfil al comparar valores; cambia la
representación de comparación y nunca los datos:

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

Las alternativas para un mismo hallazgo comparten `grupo`, y como máximo
una puede tener `aplicar = TRUE`. `decision_grupo` distingue un grupo
pendiente de una decisión explícita de no actuar: no se inventa una fila
ficticia de “no hacer nada”.

``` r

plan[!is.na(plan$grupo), c(
  "grupo", "estrategia", "recomendada", "aplicar", "decision_grupo"
)]
#>         grupo                     estrategia recomendada aplicar decision_grupo
#> 7  grupo-0010 convertir_sentinelas_numericos       FALSE   FALSE      pendiente
#> 9  grupo-0012     eliminar_columna_constante       FALSE   FALSE    recomendada
#> 11 grupo-0017        marcar_filas_duplicadas        TRUE    TRUE    recomendada
#> 12 grupo-0017    conservar_primera_duplicada       FALSE   FALSE    recomendada
#> 13 grupo-0017         conservar_mas_completa       FALSE   FALSE    recomendada
#> 14 grupo-0018     marcar_columnas_duplicadas        TRUE    TRUE    recomendada
#> 15 grupo-0018     eliminar_columna_duplicada       FALSE   FALSE    recomendada
```

En el grupo de las filas duplicadas, por ejemplo, las alternativas son
marcarlas, conservar la primera o conservar la más completa; marcar es
lo recomendado porque no elimina nada. `decision_grupo` vale `pendiente`
donde falta nuestra decisión (el `-99` de `ingreso`) y `recomendada`
donde el plan sigue su propia recomendación, aunque esa recomendación
sea no actuar: la columna constante se deja como está.

El modo guiado,
[`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md),
es una capa opcional. En una consola recorre sólo las decisiones
pendientes o riesgosas, pregunta qué hacer con cada una y devuelve el
plan con nuestras respuestas:

``` r

plan <- guiar_limpieza(plan, datos_administrativos)
```

Al tejer esta guía no hay nadie que conteste, y fuera de una sesión
interactiva
[`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md)
devuelve el plan sin cambios y sin bloquear; por eso seguimos con las
acciones que marcamos a mano.

## Aplicar sobre una copia

[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
ejecuta las acciones con `aplicar = TRUE` sobre una copia. El original
queda intacto, y el registro dice qué hizo cada acción:

``` r

copia <- datos_administrativos
resultado <- aplicar(plan, datos_administrativos)
identical(datos_administrativos, copia)
#> [1] TRUE
resultado$registro[, c("id_accion", "estrategia", "estado", "n_cambiadas")]
#>     id_accion                    estrategia    estado n_cambiadas
#> 1 accion-0011       marcar_filas_duplicadas ejecutada           2
#> 2 accion-0002 convertir_ausencias_textuales ejecutada           1
#> 3 accion-0006 convertir_ausencias_textuales ejecutada           2
#> 4 accion-0014    marcar_columnas_duplicadas ejecutada           1
```

Veamos los valores antes y después en las dos columnas que cambiaron:

``` r

data.frame(
  cedula_antes = datos_administrativos$cedula,
  cedula_despues = resultado$datos$cedula,
  sexo_antes = datos_administrativos$sexo,
  sexo_despues = resultado$datos$sexo
)
#>    cedula_antes cedula_despues sexo_antes sexo_despues
#> 1   0.000.001-1    0.000.001-1          F            F
#> 2   0.000.002-2    0.000.002-2          M            M
#> 3      00000033       00000033          F            F
#> 4   0.000.004-4    0.000.004-4          M            M
#> 5         MAL-5          MAL-5        S/D         <NA>
#> 6   0.000.006-6    0.000.006-6          F            F
#> 7   0.000.007-7    0.000.007-7          M            M
#> 8   0.000.008-8    0.000.008-8          F            F
#> 9   0.000.009-9    0.000.009-9          M            M
#> 10  0.000.010-0    0.000.010-0          F            F
#> 11  0.000.010-0    0.000.010-0          M            M
#> 12  0.000.001-1    0.000.001-1          F            F
#> 13          S/D           <NA>                    <NA>
```

`S/D` y la celda en blanco pasaron a `NA`. `MAL-5` y `00000033` siguen
igual: corregir una cédula exige saber cuál era, y eso no sale de los
datos.

Las marcas no cambian valores: agregan información. Las filas duplicadas
quedan señaladas en dos columnas nuevas, `.fila_duplicada` (la
repetición) y `.grupo_duplicado` (todas las filas del grupo), y las
columnas duplicadas, en un atributo:

``` r

marcadas <- !is.na(resultado$datos$.grupo_duplicado)
resultado$datos[marcadas, c("id_persona", "cedula", ".fila_duplicada", ".grupo_duplicado")]
#>    id_persona      cedula .fila_duplicada .grupo_duplicado
#> 1           1 0.000.001-1           FALSE                1
#> 12          1 0.000.001-1            TRUE                1
attr(resultado$datos, "columnas_duplicadas_marcadas")
#>    columna_1 columna_2
#> 1 id_persona  id_copia
```

El plan se puede editar antes de aplicar: quitar una acción, corregir un
parámetro o cambiar `n_afectadas` no altera la obligación de observar
qué pasó. Si una acción seleccionada no cambia ningún valor, el registro
dice `fallida` con su motivo, incluso cuando el estimado está ausente
(`NA`).

Las acciones destructivas nunca son recomendadas. Aunque las activemos
en el plan,
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
exige además `permitir_eliminacion = TRUE`; lo retirado se conserva en
`resultado$eliminados` salvo decisión contraria. Sin ese consentimiento,
la llamada se niega **antes de tocar nada**: no corre ninguna acción del
plan, tampoco las que no eliminan, y el error dice cuáles son los dos
caminos.

`n_no_reversibles`, en el registro, no se lee por el nombre de la acción
sino por lo que quedó en el resultado: una normalización que funde dos
valores que eran distintos (pasar `ana` y `ANA` a `ANA`) cuenta esas
celdas, porque lo que las separaba no quedó en ningún lado.

## Lo que el plan no cubre lo dice

Que el plan tenga pocas acciones no significa que lo demás esté bien,
así que declara sus huecos en tres atributos: `cobertura_diagnosticos`
(los diagnósticos que el perfil no pudo evaluar),
`hallazgos_sin_accion_por_columna_ambigua` (los hallazgos cuya columna
comparte nombre con otra, así que no hay forma de saber sobre cuál
actuaría la limpieza) y `hallazgos_sin_accion` (los hallazgos que el
perfil midió y que ninguna acción atiende, con el motivo medido de cada
uno). La impresión del plan los anuncia y el informe HTML les da su
propia sección.

``` r

sin_accion <- attr(plan, "hallazgos_sin_accion")
sin_accion[, c("hallazgo", "columna", "severidad")]
#>                  hallazgo          columna  severidad
#> 1 tipo_declarado_distinto fecha_nacimiento sospechoso
#> 2               faltantes             sexo sospechoso
```

## Volver a perfilar

El ciclo termina volviendo a perfilar el resultado y comparándolo con el
perfil de partida.
[`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
dice qué hallazgos se resolvieron y cuáles aparecieron:

``` r

perfil_despues <- perfilar(resultado$datos)
cambios <- comparar_perfiles(perfil, perfil_despues)
cambios[cambios$aspecto == "hallazgo", c("columna", "cambio", "valor_anterior", "valor_actual")]
#>             columna        cambio        valor_anterior valor_actual
#> 10 .grupo_duplicado     aparecido                  <NA>    constante
#> 11 .grupo_duplicado     aparecido                  <NA>    faltantes
#> 12             <NA> no_comparable      filas_duplicadas         <NA>
#> 13           cedula      resuelto faltantes_disfrazados         <NA>
#> 14             sexo      resuelto faltantes_disfrazados         <NA>
```

Los faltantes disfrazados de `cedula` y `sexo` quedaron `resuelto`. Los
hallazgos `aparecido` en `.grupo_duplicado` son la huella de la marca:
esa columna sólo tiene valor en las filas duplicadas, así que el perfil
la encuentra casi vacía y con un único valor. Y `filas_duplicadas` queda
`no_comparable`: la tabla ganó las columnas de marca, y con otro
conjunto de columnas no se puede decidir si las filas repetidas se
resolvieron. La comparación entre entregas se explica en [9. Comparar
entregas y seguir la
calidad](https://sebollin.github.io/lupa/articles/historico-y-deriva.html).

## Imputar por una dependencia

Una dependencia funcional exacta, como las que muestra [4. Estructura no
declarada](https://sebollin.github.io/lupa/articles/estructura-no-declarada.md),
permite deducir un valor desde los propios datos. Pero sigue siendo una
regularidad observada en una entrega: puede representar una regla real o
un error sistemático. Por eso la acción se ofrece con su mapa y su
soporte, pero no queda recomendada ni activa.

En esta tabla de trámites, `codigo` determina `descripcion`, y faltan
algunas descripciones:

``` r

tramites <- data.frame(
  id_tramite = 101:112,
  codigo = rep(1:3, each = 4),
  descripcion = rep(c("Alta", "Baja", "Cambio"), each = 4),
  stringsAsFactors = FALSE
)
tramites$descripcion[c(2, 6)] <- NA

perfil_fd <- perfilar(tramites, muestra = Inf)
plan_fd <- planificar_limpieza(perfil_fd, tramites)
imputacion <- grep("^imputar_dependencia_funcional", plan_fd$estrategia)
plan_fd[imputacion, c("id_accion", "estrategia", "recomendada", "aplicar")]
#>     id_accion                            estrategia recomendada aplicar
#> 3 accion-0003 imputar_dependencia_funcional__codigo       FALSE   FALSE
writeLines(strwrap(plan_fd$justificacion[imputacion], 88))
#> La relación codigo -> descripcion es exacta en 10 filas presentes y cada valor usado
#> tiene al menos 2 observaciones de soporte. Debe confirmarse como regla antes de
#> imputar.
plan_fd$parametros[[imputacion]]$mapa
#>   determinante dependiente
#> 1            1        Alta
#> 2            2        Baja
#> 3            3      Cambio
```

El mapa es la regla observada: cada código con su descripción. Después
de confirmar que es una regla del sistema, activamos la acción y
registramos la decisión. La dependencia vuelve a validarse al aplicar el
plan:

``` r

plan_fd$aplicar[imputacion] <- TRUE
plan_fd$decision_grupo[imputacion] <- "elegida"
resultado_fd <- aplicar(plan_fd, tramites)
resultado_fd$registro[, c("id_accion", "estrategia", "estado", "n_cambiadas")]
#>     id_accion                            estrategia    estado n_cambiadas
#> 1 accion-0003 imputar_dependencia_funcional__codigo ejecutada           2
data.frame(
  codigo = tramites$codigo,
  antes = tramites$descripcion,
  despues = resultado_fd$datos$descripcion
)
#>    codigo  antes despues
#> 1       1   Alta    Alta
#> 2       1   <NA>    Alta
#> 3       1   Alta    Alta
#> 4       1   Alta    Alta
#> 5       2   Baja    Baja
#> 6       2   <NA>    Baja
#> 7       2   Baja    Baja
#> 8       2   Baja    Baja
#> 9       3 Cambio  Cambio
#> 10      3 Cambio  Cambio
#> 11      3 Cambio  Cambio
#> 12      3 Cambio  Cambio
```

Las descripciones que faltaban se completaron desde el mapa
(`n_cambiadas` las cuenta), y nada más cambió.

## Resumen

- [`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  propone acciones y no toca los datos; sólo activa lo que es correcto
  con independencia del dominio.
- Las alternativas para un mismo hallazgo forman un grupo, y como máximo
  una se aplica.
- [`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)
  trabaja sobre una copia, registra cada acción y exige consentimiento
  explícito para eliminar.
- El plan declara lo que no cubre, y volver a perfilar muestra qué se
  resolvió.
- Imputar desde una dependencia exige confirmarla antes como regla.

Si la limpieza es parte de entregas periódicas y la pregunta es qué
cambió entre una y otra, se puede saltar directamente a [9. Comparar
entregas y seguir la
calidad](https://sebollin.github.io/lupa/articles/historico-y-deriva.html).

## Paso siguiente

[7. Del hallazgo a la
métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.html),
en el sitio del paquete: cómo pasar de lo que el perfil encontró a
métricas que se miden y se evalúan contra un umbral.

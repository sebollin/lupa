# Construir y aplicar un plan de limpieza auditable

`planificar_limpieza()` transforma los hallazgos de un objeto `perfil`
en un objeto de datos editable, sin modificar los datos examinados. Cada
fila representa una acción propuesta. Sólo se marcan como recomendadas
las estrategias correctas con independencia del dominio; algunas, como
marcar valores extremos, permanecen inactivas hasta que se decida
actuar. Las decisiones contextuales quedan desactivadas y los formatos
de fecha ambiguos quedan bloqueados.

## Uso

``` r
planificar_limpieza(perfil, datos = NULL, soporte_minimo_dependencia = 2L)

aplicar(plan, datos, permitir_eliminacion = FALSE, conservar_eliminados = TRUE)
```

## Argumentos

  - perfil:
    
    Objeto de clase `perfil` creado por `perfilar()`.

  - datos:
    
    `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el
    plan. El objeto recibido no se modifica.

  - soporte\_minimo\_dependencia:
    
    Cantidad mínima de observaciones concordantes por valor determinante
    para proponer una imputación.

  - plan:
    
    Objeto de clase `plan_limpieza` o data frame con el mismo contrato.
    Puede filtrarse y editarse antes de aplicarlo.

  - permitir\_eliminacion:
    
    Segundo consentimiento obligatorio para ejecutar acciones que
    eliminan filas o columnas.

  - conservar\_eliminados:
    
    Si se conservan en el resultado las filas y columnas retiradas. Es
    `TRUE` de forma predeterminada.

## Valor

`planificar_limpieza()` devuelve un data frame de clase `plan_limpieza`.
`aplicar()` devuelve una lista de clase `resultado_limpieza` con
`datos`, `registro`, `plan_aplicado`, el `plan` sincronizado y
`eliminados`. Si una columna de entrada es un factor, las acciones que
transforman su texto devuelven una columna `character`: no se
reconstruyen los niveles originales, porque una limpieza puede
introducir valores nuevos.

## Detalles

`aplicar()` ejecuta exclusivamente las filas con `aplicar == TRUE`,
sobre una copia de `datos`. Verifica que cada columna siga siendo
identificable y que las conversiones sean completas antes de
sustituirla. Devuelve los datos nuevos junto con un registro de las
acciones y sus parámetros. El mismo registro queda en el atributo
`registro_limpieza` de los datos resultantes.

Las alternativas para un mismo hallazgo comparten `grupo`; las acciones
independientes usan `NA`. Como máximo una alternativa de cada grupo
puede tener `aplicar == TRUE`, invariante que `aplicar()` vuelve a
validar. No se agrega una fila ficticia para "no hacer nada":
`decision_grupo` distingue `pendiente`, `recomendada`, `desactivada`,
`elegida` y `omitida`, mientras `recomendacion_grupo = "no_hacer_nada"`
representa una recomendación explícita de conservar los datos. Esto
permite separar un grupo aún no revisado de una omisión deliberada.

`estado` distingue acciones `lista`, `bloqueada` e `informativa`;
`orden` fija la secuencia reproducible. `n_afectadas` es la estimación
del perfil y el registro informa `n_cambiadas` sobre los datos
recibidos. `reversible` indica si el resultado puede deshacerse sólo con
los datos transformados. La acción de codificación prueba las tablas
congeladas de varias codificaciones y deja en `estado_reparacion` uno de
`reparado`, `reparado_parcialmente` o `no_se_pudo`. Una reparación
parcial no se activa automáticamente: debe revisarse y seleccionarse de
forma explícita. La estrategia nueva se llama `reparar_codificacion`. El
nombre histórico `reparar_codificacion_latin1` se acepta como alias para
planes guardados, aunque ya no limita el motor a latin-1. Si se marca
una acción que no está `lista`, `aplicar()` aborta antes de modificar la
copia y enumera las filas problemáticas; así no deja un conjunto
parcialmente transformado cuando el plan editable contiene una selección
inválida. Las acciones con `destructiva == TRUE` eliminan filas o
columnas, nunca son recomendadas, declaran `reversible == FALSE` y
requieren además `permitir_eliminacion = TRUE`. Por defecto, el
resultado conserva lo retirado en `eliminados`; use
`conservar_eliminados = FALSE` para evitar ese costo de memoria.

Las imputaciones por dependencia funcional se ofrecen desactivadas.
Aunque una dependencia exacta permite deducir un valor sin usar media,
moda o un modelo externo, sigue siendo una regularidad aprendida de una
sola entrega y puede reflejar un error sistemático en vez de una regla
de negocio. El plan conserva el mapa y su soporte para que el usuario la
confirme; sólo entonces se aplica y se vuelve a validar contra los datos
recibidos.

`marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada`
reproduce la semántica de `duplicated()` y marca sólo las apariciones
posteriores; `.grupo_duplicado` identifica a **todas** las filas que
participan en cada grupo de contenido idéntico.

El orden operativo se aparta deliberadamente de la secuencia dimensional
frescura–completitud–exactitud–consistencia–unicidad sugerida por el
marco. Primero marca duplicados sin borrar, luego normaliza ausencias y
texto, y deja los cambios de esquema para el final. Esto evita perder la
evidencia original, permite imputar antes de convertir tipos y mantiene
identificables las columnas durante todo el plan. Las eliminaciones
nunca se activan por defecto, por lo que deduplicar temprano no puede
hacer desaparecer registros.

## Ver también

`perfilar()`, `guiar_limpieza()`, `detectar_dependencias()`

## Ejemplos

``` r
datos <- data.frame(categoria = c(" A", "S/D", "B"))
perfil <- perfilar(datos)
plan <- planificar_limpieza(perfil)
plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
#>   grupo                    estrategia recomendada aplicar
#> 1  <NA>          revisar_cardinalidad       FALSE   FALSE
#> 2  <NA> convertir_ausencias_textuales        TRUE    TRUE
#> 3  <NA>             recortar_espacios        TRUE    TRUE
resultado <- aplicar(plan, datos)
resultado$datos
#>   categoria
#> 1         A
#> 2      <NA>
#> 3         B
```

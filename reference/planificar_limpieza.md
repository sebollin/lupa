# Construir y aplicar un plan de limpieza auditable

`planificar_limpieza()` transforma los hallazgos de un objeto `perfil`
en un objeto de datos editable, sin modificar los datos examinados. Cada
fila representa una acción propuesta. Sólo se marcan como recomendadas
las estrategias correctas con independencia del dominio; algunas, como
marcar valores extremos, permanecen inactivas hasta que se decida
actuar. Las decisiones contextuales quedan desactivadas y los formatos
de fecha ambiguos quedan bloqueados.

## Usage

``` r
planificar_limpieza(perfil, datos = NULL, soporte_minimo_dependencia = 2L)

aplicar(plan, datos, permitir_eliminacion = FALSE, conservar_eliminados = TRUE)
```

## Arguments

- perfil:

  Objeto de clase `perfil` creado por
  [`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md).

- datos:

  `data.frame`, `tibble` o `data.table` sobre el que se ejecuta el plan.
  El objeto recibido no se modifica.

- soporte_minimo_dependencia:

  Cantidad mínima de observaciones concordantes por valor determinante
  para proponer una imputación.

- plan:

  Objeto de clase `plan_limpieza` o data frame con el mismo contrato.
  Puede filtrarse y editarse antes de aplicarlo.

- permitir_eliminacion:

  Segundo consentimiento obligatorio para ejecutar acciones que eliminan
  filas o columnas.

- conservar_eliminados:

  Si se conservan en el resultado las filas y columnas retiradas. Es
  `TRUE` de forma predeterminada.

## Value

`planificar_limpieza()` devuelve un data frame de clase `plan_limpieza`.
`aplicar()` devuelve una lista de clase `resultado_limpieza` con
`datos`, `registro`, `plan_aplicado`, el `plan` sincronizado y
`eliminados`. El `registro` conserva `estado` (`ejecutada` o `fallida`),
`error`, `n_no_reversibles` y la `justificacion` de cada acción
seleccionada, incluso cuando una falla y las siguientes continúan. Si
una acción seleccionada no produce ningún efecto cuando el plan estimaba
alguno, se registra como `fallida` con el motivo y su copia no se
incorpora al resultado. Si una columna de entrada es un factor, las
acciones que transforman su texto devuelven una columna `character`: no
se reconstruyen los niveles originales, porque una limpieza puede
introducir valores nuevos.

## Details

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
del perfil y `unidad_conteo` dice si cuenta filas, columnas o valores
distintos —lo declara la acción cuando cuenta en una unidad propia, y
sólo si no lo hace se hereda del hallazgo—. El registro informa
`n_cambiadas` sobre los datos recibidos.

**`n_afectadas` y `n_cambiadas` pueden no coincidir, y las dos son
ciertas.** La estimación se calcula sobre los datos que se perfilaron;
el registro cuenta lo que pasó al aplicar. Si una acción anterior del
mismo plan ya tocó esa columna, la posterior encuentra menos —o más— de
lo estimado: con `convertir_sentinelas_numericos` (orden 110)
convirtiendo tres `-999` en ausentes, `winsorizar_outliers` (orden 520)
recorta dos valores donde el plan estimaba siete. No es un desvío que
ocultar: `orden`, `n_afectadas` y `n_cambiadas` se publican los tres, y
compararlos es la forma de ver el efecto de la composición. Sólo el caso
extremo —la acción no produce **ningún** efecto donde el plan estimaba
alguno— se registra como `fallida` con su motivo. `reversible` indica si
la conversión conserva la identidad de cada valor. Las conversiones se
comprueban sobre todos los valores de `datos`: las numéricas bloquean
ceros iniciales y colisiones no inyectivas, mientras que fechas,
fechas-hora y lógicos sólo bloquean conversiones no ejecutables o no
inyectivas. Las fechas pueden cambiar a la representación canónica del
tipo sin que eso sea una pérdida. Sin `datos` no se puede hacer la
comprobación y la acción queda bloqueada. Cuando no es reversible se
marca `destructiva`, no se activa por defecto y el registro conserva
`n_no_reversibles` y la justificación de la decisión. La acción de
codificación prueba las tablas congeladas de varias codificaciones y
deja en `estado_reparacion` uno de `reparado`, `reparado_parcialmente` o
`no_se_pudo`. Una reparación parcial no se activa automáticamente: debe
revisarse y seleccionarse de forma explícita. La estrategia se llama
`reparar_codificacion` y no limita el motor a latin-1. Si se marca una
acción que no está `lista`, `aplicar()` aborta antes de modificar la
copia y enumera las filas problemáticas. Una acción que sí está lista
pero falla se registra con su error y no impide aplicar las siguientes:
cada una conserva atomicidad sobre su propia columna o tabla. Las
acciones que efectivamente eliminan filas o columnas requieren además
`permitir_eliminacion = TRUE`; una conversión `destructiva` requiere
selección explícita y deja la pérdida cuantificada. Por defecto, el
resultado conserva lo retirado en `eliminados`; use
`conservar_eliminados = FALSE` para evitar ese costo de memoria.

Los hallazgos `controles_invisibles`, `entidades_html` y
`separadores_en_campo` tienen acciones separadas. La detección de
invisibles informa tanto los caracteres que se pueden normalizar como
los ZWJ/ZWNJ significativos; la normalización actúa sobre un conjunto
más pequeño que la detección. La acción `eliminar_controles_invisibles`
quita controles C0/C1 que no son separadores y los invisibles Unicode de
transporte, y se recomienda por defecto; conserva ZWJ/ZWNJ.
`normalizar_espacios_invisibles` colapsa espacios Unicode (incluido
NBSP) a un espacio ASCII, no se recomienda por defecto y registra la
pérdida de reversibilidad. `decodificar_entidades_html` cubre las
entidades con nombre comunes en español y referencias numéricas válidas,
pero no se activa sola porque un ampersand puede ser contenido legítimo.
`reemplazar_separadores` convierte tabulaciones, saltos de línea,
avances de página y tabulaciones verticales (`\\t`, `\\n`, `\\r`,
`\\r\\n`, `\\f` y `\\v`) en un espacio y también requiere una decisión
explícita. Las tres acciones registran el número de valores cambiados.
Una comparación aproximada con `normalizar = TRUE` usa estas mismas
clases: colapsa espacios y omite basura de transporte, pero conserva
ZWJ/ZWNJ.

Las imputaciones por dependencia funcional se ofrecen desactivadas.
Aunque una dependencia exacta permite deducir un valor sin usar media,
moda o un modelo externo, sigue siendo una regularidad aprendida de una
sola entrega y puede reflejar un error sistemático en vez de una regla
de negocio. El plan conserva el mapa y su soporte para que el usuario la
confirme; sólo entonces se aplica y se vuelve a validar contra los datos
recibidos. Si la protección enmascaró alguna clave del mapa, éste no se
usa como tabla de cruce: la relación se reconstruye sobre los datos
recibidos con el soporte declarado, sin publicar sus valores.

`marcar_filas_duplicadas` añade dos columnas. `.fila_duplicada`
reproduce la semántica de
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) y marca sólo
las apariciones posteriores; `.grupo_duplicado` identifica a **todas**
las filas que participan en cada grupo de contenido idéntico.

El orden operativo se aparta deliberadamente de la secuencia dimensional
frescura–completitud–exactitud–consistencia–unicidad sugerida por el
marco. Primero marca duplicados sin borrar, luego normaliza ausencias y
texto, y deja los cambios de esquema para el final. Esto evita perder la
evidencia original, permite imputar antes de convertir tipos y mantiene
identificables las columnas durante todo el plan. Las eliminaciones
nunca se activan por defecto, por lo que deduplicar temprano no puede
hacer desaparecer registros. `destructiva` también marca una conversión
que pierde representación, aunque no elimine filas o columnas. El
consentimiento `permitir_eliminacion` sólo se exige para las estrategias
que efectivamente retiran filas o columnas; una conversión destructiva
requiere que el usuario la active explícitamente y deja su pérdida
cuantificada en el registro.

## See also

[`perfilar()`](https://sebollin.github.io/lupa/reference/perfilar.md),
[`guiar_limpieza()`](https://sebollin.github.io/lupa/reference/guiar_limpieza.md),
[`detectar_dependencias()`](https://sebollin.github.io/lupa/reference/detectar_dependencias.md)

## Examples

``` r
datos <- data.frame(categoria = c(" A", "S/D", "B"))
perfil <- perfilar(datos)
plan <- planificar_limpieza(perfil, datos)
plan[, c("grupo", "estrategia", "recomendada", "aplicar")]
#>   grupo                    estrategia recomendada aplicar
#> 1  <NA> convertir_ausencias_textuales        TRUE    TRUE
#> 2  <NA>             recortar_espacios        TRUE    TRUE
resultado <- aplicar(plan, datos)
resultado$datos
#>   categoria
#> 1         A
#> 2      <NA>
#> 3         B
```

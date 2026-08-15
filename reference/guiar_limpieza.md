# Revisar decisiones de limpieza paso a paso

Recorre los grupos pendientes y aquellos que contienen alternativas
destructivas. Muestra evidencia calculada sobre `datos`, las estrategias
y sus justificaciones, y devuelve el plan editado sin aplicarlo. En una
sesión no interactiva retorna inmediatamente el plan sin cambios, salvo
que se proporcione un `selector` explícito.

## Usage

``` r
guiar_limpieza(
  plan,
  datos,
  selector = NULL,
  diccionarios = list(),
  max_ejemplos = 5L
)
```

## Arguments

- plan:

  Objeto `plan_limpieza`.

- datos:

  Datos correspondientes al perfil que originó el plan.

- selector:

  Función opcional que recibe una lista con `grupo`, `acciones`,
  `elegibles`, `ejemplos` y `opciones`. Debe devolver la posición, el
  identificador o el nombre de una estrategia, o `0` para no hacer nada.

- diccionarios:

  Lista opcional con nombre de diccionarios por columna.

- max_ejemplos:

  Máximo de ejemplos reales mostrados por grupo.

## Value

El plan editado, sin ejecutar acciones.

## Details

No se representa "no hacer nada" como una acción ficticia. Elegirlo
cambia `decision_grupo` a `"omitida"`; por contraste, un grupo aún no
revisado conserva `"pendiente"`. Cuando conservar los datos es la
recomendación, esa opción también se muestra con la marca
"(Recomendado)" y su justificación. Los diccionarios de capitalización
se suministran como una lista con nombre de vectores con nombre.

## See also

[`planificar_limpieza()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md),
[`aplicar()`](https://sebollin.github.io/lupa/reference/planificar_limpieza.md)

## Examples

``` r
datos <- data.frame(zona = c("Norte", "NORTE", "sur"))
plan <- planificar_limpieza(perfilar(datos), datos)
guiado <- guiar_limpieza(plan, datos)
identical(plan, guiado) # TRUE en una sesión no interactiva
#> [1] TRUE
```

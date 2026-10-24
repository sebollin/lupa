# 9. Comparar entregas y seguir la calidad

Llega la entrega de febrero de una tabla que ya revisamos en enero. ¿Qué
cambió? Y si la medimos todos los meses, ¿la calidad mejora o empeora?

> **En esta guía:**
>
> - comparamos los perfiles de dos entregas con
>   [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md);
> - separamos un cambio real de una diferencia de redondeo con
>   [`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md);
> - comparamos dos evaluaciones con
>   [`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md);
> - reunimos varias corridas en un histórico y buscamos deriva con
>   [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md);
> - guardamos el histórico y lo ampliamos con cada entrega.

## Dos entregas de la misma tabla

La entrega de enero es `datos_operativos`. La de febrero la armamos a
partir de ella con cuatro cambios: dos canales más llegan como `"S/D"`,
un código de usuario también, el monto de la fila 4, que en enero era el
centinela `-99`, llega corregido, y aparece una columna nueva,
`sucursal`.

``` r

library(lupa)

enero <- datos_operativos
febrero <- datos_operativos
febrero$canal[c(2, 7)] <- "S/D"
febrero$codigo_usuario[6] <- "S/D"
febrero$monto[4] <- 1500
febrero$sucursal <- "central"
```

## Qué cambió entre las dos entregas

Perfilamos cada entrega con su fecha y con un mismo nombre, `entrega`,
que es el que va a llevar la tabla en las mediciones de más abajo.
[`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
no exige que las dos tengan las mismas columnas.

``` r

perfil_enero <- perfilar(enero, nombre = "entrega", fecha = "2026-01-31")
perfil_febrero <- perfilar(febrero, nombre = "entrega", fecha = "2026-02-28")
deriva <- comparar_perfiles(perfil_enero, perfil_febrero)
deriva[, c("columna", "aspecto", "cambio", "severidad")]
#>           columna      aspecto     cambio  severidad
#> 1        sucursal      columna  aparecida      error
#> 2  codigo_usuario    faltantes modificado sospechoso
#> 3  codigo_usuario cardinalidad modificado sospechoso
#> 4           canal    faltantes modificado sospechoso
#> 5           canal cardinalidad modificado sospechoso
#> 6           monto    faltantes modificado         ok
#> 7           monto        rango modificado         ok
#> 8  codigo_usuario       patron modificado sospechoso
#> 9  codigo_usuario       patron modificado sospechoso
#> 10          canal       patron modificado sospechoso
#> 11          canal       patron modificado sospechoso
#> 12 codigo_usuario     hallazgo  aparecido sospechoso
#> 13       sucursal     hallazgo  aparecido sospechoso
#> 14          monto     hallazgo   resuelto         ok
```

Cada fila es un cambio, y `aspecto` dice de qué tipo. La columna nueva
es un cambio de esquema (`columna`, `aparecida`). Las demás filas son
cambios sobre columnas que están en las dos entregas: la proporción de
faltantes, la cardinalidad, el rango, los patrones de formato y los
hallazgos que aparecieron o se resolvieron. La columna que sólo existe
de un lado no impide comparar las otras.

`severidad` es un factor ordenado, `ok < sospechoso < error`, así que la
tabla se filtra como cualquier otra. Los cambios menores quedan como
filas `ok` en lugar de desaparecer. Veamos los faltantes que empeoraron:

``` r

peores <- deriva[deriva$severidad >= "sospechoso" & deriva$aspecto == "faltantes", ]
peores[, c("columna", "aspecto", "evidencia")]
#>          columna   aspecto                           evidencia
#> 2 codigo_usuario faltantes Cambio de +0.0769 en escala [0, 1].
#> 4          canal faltantes Cambio de +0.1538 en escala [0, 1].
```

La comparación usa las propiedades medidas de cada columna, no los
valores uno por uno. Un perfil comparado consigo mismo no produce ningún
cambio:

``` r

nrow(comparar_perfiles(perfil_enero, perfil_enero))
#> [1] 0
```

Lo que la protección de datos personales tapa no se compara, y queda
declarado en el atributo `campos_protegidos` de la deriva; el detalle
está en el artículo [Alcance y
cobertura](https://sebollin.github.io/lupa/articles/alcance-y-cobertura.html).

## Un cambio real o una diferencia de redondeo

A veces la pregunta no es qué cambió en los datos, sino si dos procesos
produjeron lo mismo: la tabla que exporta un sistema nuevo frente a la
del sistema que reemplaza, o la misma consulta corrida en dos motores.
Ahí importa separar lo que difiere de verdad de lo que difiere por
redondeo. En este ejemplo, el sistema nuevo guarda un monto con menos
decimales y trae una fila más, vacía.

``` r

sistema_anterior <- data.frame(monto = c(1250, 1600, 1425.0004, 1750, 1810))
sistema_nuevo <- data.frame(monto = c(1250, 1600, 1425, 1750, 1810, NA))
perfil_anterior <- perfilar(
  sistema_anterior, fecha = "2026-01-31", analizar_dependencias = FALSE
)
perfil_nuevo <- perfilar(
  sistema_nuevo, fecha = "2026-01-31", analizar_dependencias = FALSE
)
equivalencia <- comparar_equivalencia(
  perfil_anterior, perfil_nuevo, tolerancia = 0.01
)
table(equivalencia$veredicto)
#> 
#>               identico            equivalente materialmente_distinto 
#>                     41                      2                      7 
#>          no_comparable 
#>                     23
```

[`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md)
devuelve una fila por columna y campo compartidos. Los conteos se
comparan con igualdad exacta; la tolerancia, que declaramos nosotros, se
usa sólo para los campos de punto flotante, como la media y el desvío.
Un campo sin valor medible en los dos lados queda como `no_comparable` y
no cuenta como acuerdo.

``` r

campos <- c("n", "n_faltantes", "maximo", "media", "desvio")
equivalencia[
  match(campos, equivalencia$campo),
  c("campo", "valor_anterior", "valor_actual", "veredicto", "tipo_eje")
]
#>          campo valor_anterior valor_actual              veredicto tipo_eje
#> 3            n              5            6 materialmente_distinto   exacto
#> 8  n_faltantes              0            1 materialmente_distinto   exacto
#> 33      maximo           1810         1810               identico   exacto
#> 34       media     1567.00008         1567            equivalente flotante
#> 36      desvio   231.4518.... 231.4519....            equivalente flotante
```

La fila de más es un cambio real: `n` y `n_faltantes` salen
`materialmente_distinto`. La media y el desvío se movieron por el
redondeo y salen `equivalente`, dentro de la tolerancia. El máximo no
cambió. En los ejes exactos, `equivalente` es inalcanzable: el conteo
cambiado sigue siendo `materialmente_distinto` con cualquier tolerancia.

``` r

tolerancias <- c(0, 0.01, 0.5, 10)
veredicto_n <- vapply(tolerancias, function(tolerancia) {
  comparacion <- comparar_equivalencia(perfil_anterior, perfil_nuevo, tolerancia)
  as.character(comparacion$veredicto[comparacion$campo == "n"])
}, character(1))
data.frame(tolerancia = tolerancias, veredicto_n = veredicto_n)
#>   tolerancia            veredicto_n
#> 1       0.00 materialmente_distinto
#> 2       0.01 materialmente_distinto
#> 3       0.50 materialmente_distinto
#> 4      10.00 materialmente_distinto
```

La tolerancia se publica en cada fila y no entra en ninguna regla del
paquete: el comparador devuelve datos, no decisiones.

## Dos evaluaciones, una diferencia

Para seguir la calidad medida, y no sólo el perfil, repetimos cada mes
la evaluación de [7. Del hallazgo a la
métrica](https://sebollin.github.io/lupa/articles/del-hallazgo-a-la-metrica.md):
la completitud de las cuatro columnas con faltantes disfrazados,
agregada por columna y comparada con un 90 %. El modelo sale de la
propuesta de enero y queda fijo. Si lo recalculáramos con cada entrega,
una diferencia podría venir del modelo y no de los datos.

``` r

propuesta <- proponer_modelo(perfil_enero, enero)
propuesta$incluir <- propuesta$metrica == "NoNulo"
modelo_completitud <- modelo_desde_propuesta(propuesta)
publicable <- perfil_evaluacion(
  "Publicable",
  regla_evaluacion("Completitud de al menos 90 %", function(x) x >= 0.9)
)

evaluar_entrega <- function(datos, id_medicion, fecha) {
  medicion <- medir(
    modelo_completitud, datos, id_medicion = id_medicion, fecha = fecha
  )
  evaluar(agregar(medicion, "atributo", "ratio"), publicable)
}
evaluacion_enero <- evaluar_entrega(enero, "enero", "2026-01-31")
evaluacion_febrero <- evaluar_entrega(febrero, "febrero", "2026-02-28")

cambio <- comparar_evaluaciones(evaluacion_enero, evaluacion_febrero)
cambio[, c("perfil", "resultado_anterior", "resultado_actual", "delta")]
#>       perfil resultado_anterior resultado_actual delta
#> 1 Publicable               0.75              0.5 -0.25
```

En enero tres de las cuatro columnas alcanzaban el 90 %; en febrero,
dos, porque `canal` y `codigo_usuario` sumaron faltantes disfrazados. El
monto corregido no alcanza a compensarlo: ya cumplía. Si dos corridas no
se pudieran comparar, por ejemplo porque cambió el modelo, la columna
`comparacion` diría por qué falta el delta.

## Una serie de corridas

Llega marzo, con los canales y el código de usuario corregidos. Para más
de dos corridas,
[`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
reúne las evaluaciones en un `data.frame` plano y versionado:

``` r

marzo <- febrero
marzo$canal[c(2, 5, 7)] <- c("telefono", "web", "telefono")
marzo$codigo_usuario[6] <- "USR-006"
evaluacion_marzo <- evaluar_entrega(marzo, "marzo", "2026-03-31")

historico <- historico_calidad(
  evaluacion_enero, evaluacion_febrero, evaluacion_marzo
)
historico[, c("id_medicion", "fecha", "nivel", "resultado")]
#>   id_medicion      fecha             nivel resultado
#> 1       enero 2026-01-31  evaluacion_regla      0.75
#> 2       enero 2026-01-31 evaluacion_perfil      0.75
#> 3     febrero 2026-02-28  evaluacion_regla      0.50
#> 4     febrero 2026-02-28 evaluacion_perfil      0.50
#> 5       marzo 2026-03-31  evaluacion_regla      1.00
#> 6       marzo 2026-03-31 evaluacion_perfil      1.00
```

Cada corrida deja una fila por regla y una por perfil. El histórico se
puede escribir como CSV o llevarse a una tabla de base de datos sin
desarmar listas anidadas. La configuración de cada corrida (la tabla, el
marco, los tipos de resultado) viaja en las columnas `identidad_tabla` y
`configuracion_*` de las filas `evaluacion_perfil`. Por eso un histórico
releído de un CSV separa las tablas y corta los cambios de marco igual
que el objeto original.

[`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
compara corridas consecutivas, ordenadas por fecha dentro de cada
perfil, y marca los cambios que superan el umbral en la escala `[0, 1]`:

``` r

saltos <- detectar_deriva_calidad(historico, umbral = 0.05)
saltos[, c("id_medicion_anterior", "id_medicion_actual", "delta", "direccion",
           "severidad")]
#>   id_medicion_anterior id_medicion_actual delta direccion severidad
#> 1                enero            febrero -0.25 deterioro     error
#> 2              febrero              marzo  0.50    mejora        ok
```

La caída de febrero es un deterioro, con el mismo delta que dio
[`comparar_evaluaciones()`](https://sebollin.github.io/lupa/reference/comparar_evaluaciones.md)
y severidad `error`; la recuperación de marzo, una mejora, con severidad
`ok`.

## Guardar y ampliar el histórico

En el uso de todos los meses, el histórico se guarda y se amplía con
cada corrida.
[`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
agrega la nueva y no duplica una corrida idéntica:

``` r

hasta_febrero <- historico_calidad(evaluacion_enero, evaluacion_febrero)
ampliado <- acumular_historico(hasta_febrero, evaluacion_marzo)
identical(ampliado, historico)
#> [1] TRUE
nrow(acumular_historico(historico, evaluacion_marzo)) == nrow(historico)
#> [1] TRUE
```

La persistencia usa RDS y no agrega dependencias.
[`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
no reemplaza un archivo existente salvo que se lo pidamos con
`sobrescribir = TRUE`:

``` r

archivo <- tempfile(fileext = ".rds")
guardar_historico(historico, archivo)
identical(leer_historico(archivo), historico)
#> [1] TRUE
tryCatch(guardar_historico(historico, archivo), error = conditionMessage)
#> [1] "El archivo ya existe; use `sobrescribir = TRUE` para reemplazarlo."
unlink(archivo)
```

## Resumen

- [`comparar_perfiles()`](https://sebollin.github.io/lupa/reference/comparar_perfiles.md)
  compara dos entregas sin exigir las mismas columnas y separa los
  cambios de esquema de los cambios sobre columnas comparables.
- [`comparar_equivalencia()`](https://sebollin.github.io/lupa/reference/comparar_equivalencia.md)
  aplica la tolerancia que declaramos sólo a los campos de punto
  flotante: un conteo distinto es siempre `materialmente_distinto`.
- Para comparar evaluaciones, el modelo y la regla quedan fijos y cada
  corrida lleva su `id_medicion` y su fecha.
- [`historico_calidad()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  reúne las corridas en una tabla plana, y
  [`detectar_deriva_calidad()`](https://sebollin.github.io/lupa/reference/detectar_deriva_calidad.md)
  marca los deterioros y las mejoras entre corridas consecutivas.
- [`acumular_historico()`](https://sebollin.github.io/lupa/reference/historico_calidad.md)
  y
  [`guardar_historico()`](https://sebollin.github.io/lupa/reference/guardar_historico.md)
  mantienen la serie entre sesiones.

## Paso siguiente

[10. Perfilar una
base](https://sebollin.github.io/lupa/articles/perfilar-una-base.md): el
mismo examen dentro de un motor de base de datos, eligiendo cuánto de la
tabla se trae a R.

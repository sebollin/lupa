# Medir y evaluar

Medir exige una taxonomía que dé contexto a cada métrica. Este recorrido
usa un marco mínimo declarado para el ejemplo; la arquitectura, los
marcos incluidos y la cobertura se explican en [Definir la
calidad](https://sebollin.github.io/lupa/articles/definir-la-calidad.md).

``` r

library(lupa)

marco_medicion <- marco_calidad("Marco mínimo de medición", list(
  Trazabilidad = "Origen documentado",
  Completitud = "Densidad",
  Unicidad = "No-duplicación"
))
```

## Tres niveles de métrica

Una métrica genérica declara semántica, granularidad, tipo de resultado
y propiedades. Especializar fija propiedades reutilizables; instanciar
liga esa especialización a objetos concretos.

``` r

nucleo <- metricas_nucleo()
no_nulo <- nucleo$NoNulo
no_nulo_personas <- especializar(
  no_nulo, nombre_especifico = "NoNuloDocumento"
)
documento <- instanciar(no_nulo_personas, "personas", "documento")
documento$declaracion[c("nombre", "granularidad", "tipo_resultado")]
#> $nombre
#> [1] "NoNulo"
#> 
#> $granularidad
#> [1] "instanciaAtributo"
#> 
#> $tipo_resultado
#> [1] "booleano"
```

La separación permite reutilizar la misma definición sobre entregas y
columnas distintas.

### Una métrica propia de punta a punta

El gancho `metodo` recibe una lista con nombre de tablas y la instancia
que se está midiendo. Devuelve una fila por objeto medido con las
columnas `resultado`, `entidad`, `atributo`, `fila` y `objeto`. Este
ejemplo declara una métrica ajena a los catálogos incluidos y muestra
las dos llamadas de la fábrica: la primera especializa y la segunda
instancia.

``` r

metodo_origen <- function(tablas, instancia) {
  x <- tablas[[instancia$entidad]][[instancia$atributos]]
  data.frame(
    resultado = !is.na(x) & nzchar(x),
    entidad = instancia$entidad,
    atributo = instancia$atributos,
    fila = seq_along(x),
    objeto = paste0(instancia$entidad, "$", instancia$atributos,
                    "[", seq_along(x), "]")
  )
}

OrigenDeclarado <- metrica(
  nombre = "OrigenDeclarado",
  semantica = "Indica si cada registro declara su sistema de origen.",
  granularidad = "instanciaAtributo",
  tipo_resultado = "booleano",
  dimension = "Trazabilidad",
  factor = "Origen documentado",
  metodo = metodo_origen
)
origen <- OrigenDeclarado()(entidad = "entrega", atributos = "origen")
datos_origen <- data.frame(origen = c("sistema_a", "", "sistema_b"))
medir(
  modelo(origen, marco = marco_medicion), datos_origen
)[, c("objeto_medible", "resultado")]
#>      objeto_medible resultado
#> 1 entrega$origen[1]         1
#> 2 entrega$origen[2]         0
#> 3 entrega$origen[3]         1
```

Cuando una métrica declara propiedades,
[`propiedades_metrica()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md)
permite consultarlas antes de llamar a
[`especializar()`](https://sebollin.github.io/lupa/reference/modelo_calidad.md).

``` r

propiedades_metrica(nucleo$Formato)
#>           propiedad configurada
#> 1 expresion_regular       FALSE
#> 2       diccionario       FALSE
#> 3         validador       FALSE
```

## Medir y agregar

``` r

datos <- data.frame(documento = c("1", "2", NA, "4"))
medidas <- medir(
  modelo(documento, marco = marco_medicion), datos,
  id_medicion = "enero",
  fecha = as.POSIXct("2026-01-31", tz = "UTC")
)
medidas[, c("objeto_medible", "resultado", "granularidad")]
#>          objeto_medible resultado      granularidad
#> 1 personas$documento[1]         1 instanciaAtributo
#> 2 personas$documento[2]         1 instanciaAtributo
#> 3 personas$documento[3]         0 instanciaAtributo
#> 4 personas$documento[4]         1 instanciaAtributo

agregar(medidas, "atributo", "ratio")[, c(
  "objeto_medible", "resultado", "agregacion"
)]
#>       objeto_medible resultado agregacion
#> 1 personas$documento      0.75      ratio
```

## Tablero, orientación e índice

[`tablero_calidad()`](https://sebollin.github.io/lupa/reference/tablero_calidad.md)
resume la medición y conserva su marco y su cobertura. La columna
`orientacion` dice cómo leer cada proporción: `conformidad` significa
que un valor mayor es mejor, `defecto` que un valor menor es mejor y
`no_aplica` que la métrica no es una proporción combinable. En el
ejemplo que sigue, el `0.50` de `EntidadDuplicada` expresa la mitad de
las entidades duplicadas —cuanto más bajo, mejor— y el `0.75` de
`NoNulo` expresa tres cuartos de valores presentes —cuanto más alto,
mejor—. Los números no se leen igual aunque estén en la misma escala, y
promediarlos sin mirar `orientacion` da un número que no significa nada.

``` r

modelo_tablero <- modelo(list(
  instanciar(especializar(nucleo$NoNulo), "padron", "codigo"),
  instanciar(especializar(nucleo$EntidadDuplicada), "padron")
), marco = marco_medicion)
datos_tablero <- data.frame(codigo = c("A", "B", "B", NA))
medidas_tablero <- medir(
  modelo_tablero, datos_tablero, id_medicion = "tablero-ejemplo"
)
tablero <- tablero_calidad(medidas_tablero, marco = marco_medicion)
tablero[, c(
  "dimension", "factor", "metrica", "valor", "orientacion", "universo"
)]
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>    dimension         factor          metrica valor orientacion universo
#>  Completitud       Densidad           NoNulo  0.75 conformidad   celdas
#>     Unicidad No-duplicación EntidadDuplicada  0.50     defecto    filas
attr(tablero, "alcance")
#>   factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#> 1              3                2                     1          0                0
```

Sin `pesos`, no hay número: `indice_calidad(tablero)` devuelve el
tablero. Los pesos no pertenecen a `lupa`; los declara el usuario según
la decisión del proyecto. El índice conserva además la cobertura que
acompaña al tablero.

``` r

indice_sin_pesos <- indice_calidad(tablero)
indice_sin_pesos
#> 
#> ── Tablero de calidad ────────────────────────────────────────────────────────────────────
#>       componente   dimension         factor          metrica  objeto valor orientacion
#>  componente-0001 Completitud       Densidad           NoNulo  codigo  0.75 conformidad
#>  componente-0002    Unicidad No-duplicación EntidadDuplicada (tabla)  0.50     defecto
#>  agregacion umbral universo
#>       ratio     NA   celdas
#>       ratio     NA    filas
#> 
#> ── Alcance del marco ──
#> 
#>  factores_marco factores_medidos sin_metrica_declarada no_aplican fuera_de_alcance
#>               3                2                     1          0                0

# Pesos elegidos para este ejemplo; no son valores predeterminados de lupa.
pesos_ejemplo <- c(Completitud = 0.6, Unicidad = 0.4)
indice <- indice_calidad(tablero, pesos = pesos_ejemplo)
indice
#> ── Índice de calidad declarado ───────────────────────────────────────────────────────────
#> Valor: 0.65
#> 
#> ── Cobertura del índice ──
#> 
#>  factores_marco factores_en_indice                                          factores
#>               3                  2 Completitud / Densidad; Unicidad / No-duplicación
#>  metricas_no_medidas
#> 
#> ── Dimensiones, pesos y aportes ──
#>    dimension valor peso aporte                combinacion_interna
#>  Completitud  0.75  0.6   0.45 un componente; sin paso intermedio
#>     Unicidad  0.50  0.4   0.20 un componente; sin paso intermedio
#> ── Componentes de defecto invertidos ──
#>       componente dimension         factor          metrica  objeto valor orientacion
#>  componente-0002  Unicidad No-duplicación EntidadDuplicada (tabla)   0.5     defecto
#>  agregacion umbral universo transformacion valor_indice peso_interno
#>       ratio     NA    filas      1 - valor          0.5            1
#> ℹ Dentro de cada dimensión se usa un solo componente o los pesos_internos declarados; entre dimensiones se usan `pesos`.
#> ! Los componentes salen de universos distintos (por ejemplo, celdas, valores con formato reconocible y filas). El índice sólo los combina porque quien lo solicitó declaró los pesos.
indice$cobertura
#>   factores_marco factores_en_indice                                          factores
#> 1              3                  2 Completitud / Densidad; Unicidad / No-duplicación
#>   metricas_no_medidas
#> 1
```

Para las métricas con orientación `defecto`, el índice usa `1 - valor`;
las de `no_aplica` quedan fuera y se conservan como exclusiones. El
índice no promedia por su cuenta dos componentes de una misma dimensión:
exige que el usuario declare `pesos_internos` para esa combinación.

``` r

modelo_doble <- modelo(list(
  instanciar(
    especializar(nucleo$NoNulo), "padron", "codigo",
    nombre_instancia = "no-nulo-codigo"
  ),
  instanciar(
    especializar(nucleo$NoNulo), "padron", "nombre",
    nombre_instancia = "no-nulo-nombre"
  )
), marco = marco_medicion)
medidas_dobles <- medir(
  modelo_doble,
  data.frame(codigo = c("A", "B", NA), nombre = c("x", NA, "z")),
  id_medicion = "interno-ejemplo"
)
tablero_doble <- tablero_calidad(medidas_dobles, marco = marco_medicion)
tryCatch(
  indice_calidad(tablero_doble, pesos = c(Completitud = 1)),
  error = function(e) conditionMessage(e)
)
#> [1] "Las dimensiones con varios componentes requieren `pesos_internos`: Completitud."
```

Una advertencia acompaña al índice cuando sus componentes provienen de
universos distintos, como celdas y filas. La cobertura y la advertencia
no resuelven esa diferencia: el índice sólo los combina porque el
usuario declaró los pesos.

Las cuatro agregaciones implementadas son:

- `ratio`, para resultados booleanos;
- `ratio_umbral`, para resultados reales;
- `promedio`;
- `promedio_ponderado`, con pesos en `[0, 1]` que suman uno por destino.

La función valida el tipo de resultado y la transición. No agrega hacia
factor, dimensión o modelo. Los resultados no acotados que el marco usa
para `ErrorEstandar` y actualidad conservan su unidad y no entran en
estas cuatro agregaciones.

## Evaluar no es volver a medir

Una regla expresa una condición sobre resultados. La evaluación de una
regla es la proporción que la cumple; la evaluación de un perfil es la
media simple de sus reglas. Los perfiles incluidos de AGESIC usan
condiciones estrictas `> 0.5`, `> 0.7` y `> 0.9`; otra familia se
construye con umbrales con nombres.

``` r

medida_atributo <- agregar(medidas, "atributo", "ratio")
madurez <- perfiles_madurez()
evaluar(medida_atributo, madurez$Intermedio)$perfiles
#>   id_medicion      fecha     perfil n_reglas resultado
#> 1       enero 2026-01-31 Intermedio        1         1
names(perfiles_madurez(umbrales = c(Inicial = 0.4, Consolidado = 0.85)))
#> [1] "Inicial"     "Consolidado"
```

El `id_medicion` y la fecha permanecen en las medidas para comparar
corridas y monitorear deriva.

## Referencias

[Batini C, Scannapieco M
(2016)](https://doi.org/10.1007/978-3-319-24106-7). *Data and
Information Quality: Dimensions, Principles and Techniques*. Springer.

[AGESIC
(2020)](https://www.gub.uy/agencia-gobierno-electronico-sociedad-informacion-conocimiento/).
*Marco de trabajo para la Gestión de la Calidad de Datos en Gobierno
Digital*, versión 1.6. Presidencia de la República, Uruguay.
